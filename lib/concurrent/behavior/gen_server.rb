require 'concurrent/executor/single_thread_executor'

module Concurrent

  # 1> c(kitty_gen_server).
  # #=> {ok,kitty_gen_server}
  # 2> rr(kitty_gen_server).
  # #=> [cat]
  # 3> {ok, Pid} = kitty_gen_server:start_link().
  # #=> {ok,<0.253.0>}
  # 4> Pid ! <<"Test handle_info">>.
  # #=> Unexpected message: <<"Test handle_info">>
  # #=> <<"Test handle_info">>
  # 5> Cat = kitty_gen_server:order_cat(Pid, "Cat Stevens", white, "not actually a cat").
  # #=> cat{name = "Cat Stevens",color = white,
  #         description = "not actually a cat"}
  # 6> kitty_gen_server:return_cat(Pid, Cat).
  # #=> ok
  # 7> kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!").
  # #=> cat{name = "Cat Stevens",color = white,
  #         description = "not actually a cat"}
  # 8> kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!").
  # #=> cat{name = "Kitten Mittens",color = black,
  #         description = "look at them little paws!"}
  # 9> kitty_gen_server:return_cat(Pid, Cat).
  # #=> ok      
  # 10> kitty_gen_server:close_shop(Pid).
  # #=> "Cat Stevens" was set free.
  # ok



  # call/cast methods may be called from any thread
  # cast/call generate messages which must be queued up and processed in order
  # which means that an object must exist as a target for the messages
  # this is not inconsistent with Erlang since a Pid is an "object" with state (a mailbox and an ID)
  # messages must be processed in the order they are received and must be processed one at a time
  # in this respect an Erlang processes is similar to a Ruby thread that loops forever
  #
  # so we need to be able to handle two types of messages: 
  # * cast messages return a value (ask)
  # * call messahes do not (tell)
  #
  # tell: return nil and handle async
  # ask: return an ivar and handle async
  # ask!: block on the ivar value, raise an exception on timeout
  #
  # To process a cast or call:
  # * create a message with the state, type, an IVar, etc.
  # * enqueue the message
  # * dequeue the message on the worker thread
  # * call the appropriate subclass
  # * update the internal process state
  # * update the IVar with the result
  # * NOTE: the IVar may be nil (cast)

  # http://www.erlang.org/doc/design_principles/gen_server_concepts.html
  # http://www.erlang.org/doc/man/gen_server.html
  # http://learnyousomeerlang.com/clients-and-servers
  module GenServer
    extend self

    def self.extended(base)
      base.extend(base)
    end

    def start(*args)
      #NOTE: need a way to handle an options hash
      #NOTE: may want to check self for implemented callback methods
      state = self.init(*args)
      Process.new(self, state)
    end
    alias_method :start_link, :start

    private

    Envelope = Struct.new(:message, :ivar, :process)

    class Process

      def initialize(server, state)
        @server = server
        @state = state
        @executor = Concurrent::SingleThreadExecutor.new
      end

      def cast(message)
        message(message, nil)
      end
      alias_method :tell :cast
      alias_method :<<, :cast

      def call(message, ivar = IVar.new)
        message(message, ivar)
      end
      alias_method :ask :call

      def call!(message, ivar = IVar.new)
        call(message, ivar).value!
      end
      alias_method :ask! :call!

      def message(message, ivar = nil)
        @executor.post(Envelope.new(message, ivar, self), &process_envelope)
        return ivar || self
      end

      def shutdown
        # ???
      end

      def kill
        # ???
      end

      private

      def process_envelope(envelope)
        begin
          if envelope.ivar
            result, @state = @server.handle_cast(@state, *args)
          else
            @state = @server.handle_call(@state, *args)
          end
        rescue => ex
          # supress
        ensure
          ivar.complete(ex.nil?, result, ex) if ivar
        end
      end
    end

    public

    def init(*args)
      warn "[#{self}] Warning: undefined callback function init"
      # Result = {ok,State} | {ok,State,Timeout} | {ok,State,hibernate}
      nil
    end

    def handle_call(state, *args)
      warn "[#{self}] Warning: undefined callback function handle_call"
      [nil, state]
    end

    def handle_cast(state, *args)
      warn "[#{self}] Warning: undefined callback function handle_cast"
      state
    end

    def terminate(state, reason)
      warn "[#{self}] Warning: undefined callback function terminate"
      nil
    end
  end
end

module Kitty
  extend GenServer

  Cat = Struct.new(:name, :color, :description)

  def order_cat(name, color, description)
    call([:order, name, color, description])
  end

  def return_cat(cat)
    cast(cat)
  end

  def close_shop
    terminate
  end

  protected

  def init(*args)
    [] # start with an array of zero cats
  end

  def handle_cast(state, *args)
  end

  def handle_call(state, *args)
  end
end
