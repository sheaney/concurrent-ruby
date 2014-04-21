require 'concurrent/remote_actor_proxy'

module Concurrent

  class RemoteActorRef
    include ActorRef

    def initialize(actor, opts = {})
      @proxy = RemoteActorDrbProxy.new(opts)
    end

    def connected?
      @proxy.running?
    end

    def running?
      super && connected?
    end

    alias :ready? :connected?
  end
end
