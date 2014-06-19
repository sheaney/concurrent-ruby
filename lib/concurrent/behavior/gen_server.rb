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
      #NOTE: state needs to be an Atomic reference
      #NOTE: may want to check self for implemented callback methods
      result, state, *rest = self.init(*args)
      if result == :ok
        Process.new(self, state)
      else
        raise StandardError.new(state)
      end
    end
    alias_method :start_link, :start

    private

    class Process

      def initialize(mod, state)
        #NOTE: @state needs to be an Atomic reference
        @mod = mod
        @state = state
      end

      def call(*args)
        #NOTE: must block
        #NOTE: @state needs to be an Atomic reference
        result, *opts = @mod.handle_call(@state, *args)
        case result
        when :reply
          @state = opts[1]
          opts[0]
        when :noreply
          @state = opts[0]
          nil
        when :stop
          @state = opts.pop
          @mod.terminate(opts.first, @state)
          opts[1]
        else
          raise StandardError.new("#{result} is not a valid result")
        end
      end

      def cast(*args)
        #NOTE: must NOT block
        #NOTE: @state needs to be an Atomic reference
        result, *opts = @mod.handle_cast(@state, *args)
        case result
        when :noreply
          @state = opts[0]
          nil
        when :stop
          @state = opts.pop
          @mod.terminate(opts.first, @state)
          opts[1]
        else
          raise StandardError.new("#{result} is not a valid result")
        end
      end
    end

    public
    #protected

    # Result = {ok,State} | {ok,State,Timeout} | {ok,State,hibernate}
    def init(*args)
      warn "[#{self}] Warning: undefined callback function init"
      [:ok, nil] 
    end

    def handle_call(state, *args)
      warn "[#{self}] Warning: undefined callback function handle_call"
      # {reply,Reply,NewState}
      # {reply,Reply,NewState,Timeout}
      # {reply,Reply,NewState,hibernate}
      # {noreply,NewState}
      # {noreply,NewState,Timeout}
      # {noreply,NewState,hibernate}
      # {stop,Reason,Reply,NewState}
      # {stop,Reason,NewState}
      [:noreply, state]
    end

    def handle_cast(state, *args)
      warn "[#{self}] Warning: undefined callback function handle_cast"
      # {noreply,NewState}
      # {noreply,NewState,Timeout}
      # {noreply,NewState,hibernate}
      # {stop,Reason,NewState}
      [:noreply, state]
    end

    def terminate(state, reason)
      warn "[#{self}] Warning: undefined callback function terminate"
      nil
    end
  end

  module Kitty
    extend GenServer

    def handle_call(state, *args)
      puts 'Kitty::handle_call'
      [:noreply, state]
    end
  end
end
