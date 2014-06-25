require 'concurrent/ivar'
require 'concurrent/executor/serialized_execution'
require 'concurrent/executor/single_thread_executor'

module Concurrent

  # http://www.erlang.org/doc/design_principles/gen_server_concepts.html
  # http://www.erlang.org/doc/man/gen_server.html
  # http://learnyousomeerlang.com/clients-and-servers
  module GenServer
    extend self

    def self.extended(base)
      base.extend(base)
    end

    def start(opts = {})
      state = self.init(*opts.fetch(:args, []))
      executor = opts.fetch(:executor, Concurrent::SingleThreadExecutor.new)
      Process.new(self, state, executor)
    end

    private

    Envelope = Struct.new(:message, :ivar, :process)

    class Process

      def initialize(server, state, executor)
        @server = server
        @state = state
        if executor.is_a?(Concurrent::SerialExecutor)
          @executor = executor
        else
          @executor = Concurrent::SerializedExecutionDelegator.new(executor)
        end
      end

      def cast(msg)
        message(msg, nil)
      end
      alias_method :tell, :cast
      alias_method :<<, :cast

      def call(msg, ivar = IVar.new)
        message(msg, ivar)
      end
      alias_method :ask, :call

      def call!(msg, ivar = IVar.new)
        call(msg, ivar).value!
      end
      alias_method :ask!, :call!

      def message(msg, ivar = nil)
        return false unless @executor.running?
        env = Envelope.new(msg, ivar, self)
        @executor.post(Envelope.new(msg, ivar, self)) do |envelope|
          process_envelope(envelope)
        end
        return ivar || self
      end

      [:running?, :shuttingdown?, :shutdown?, :wait_for_termination].each do |method|
        define_method(method) do |*args|
          @executor.send(method, *args)
        end
      end

      [:shutdown, :kill].each do |method|
        define_method(method) do
          return true unless @executor.running?
          result = @executor.send(method)
          @server.terminate(@state, method)
          result
        end
      end

      private

      def process_envelope(envelope)
        begin
          if envelope.ivar
            result, @state = @server.handle_call(@state, *envelope.message)
          else
            @state = @server.handle_cast(@state, *envelope.message)
          end
        rescue => ex
          # supress
        ensure
          envelope.ivar.complete(ex.nil?, result, ex) if envelope.ivar
        end
      end
    end

    protected

    #NOTE: returns the initial `state` object
    def init(*args)
      warn "[#{self}] Warning: undefined callback function init"
      nil
    end

    #NOTE: returns a two-tuple, the result and the new `state` object
    def handle_call(state, *args)
      warn "[#{self}] Warning: undefined callback function handle_call"
      [nil, state]
    end

    #NOTE: returns the new `state` object
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

module CounterServer
  extend Concurrent::GenServer

  def init(initial_value = 0)
    print "Initial Value: #{initial_value}\n"
    initial_value
  end

  def handle_cast(state, *args)
    print "handle_cast: #{state}, #{args}\n"
    state + args.first.to_i
  end

  def handle_call(state, *args)
    print "handle_call: #{state}, #{args}\n"
    value = state + args.first.to_i
    [value, value]
  end

  def terminate(state, reason)
    print "terminate: #{state}, #{reason}\n"
    nil
  end
end

#require 'concurrent'
#counter = CounterServer.start
#counter.cast(1).cast(2).cast(3)
#counter.call(0).value #=> 6
#counter.shutdown
