require 'concurrent/actor'
require 'concurrent/postable'
require 'drb/drb'

module Concurrent

  class RemoteActorProxy

    DEFAULT_HOST = ActorServer::DEFAULT_HOST
    DEFAULT_PORT = ActorServer::DEFAULT_PORT

    def initialize(opts = {})
      @host = opts.fetch(:host, DEFAULT_HOST)
      @port = opts.fetch(:port, DEFAULT_PORT)
    end

    def start
      @server ||= DRbObject.new_with_uri("druby://#{@host}:#{@port}") # TODO - connection pool
    end

    def running?
      ! @server.nil?
    end

    def stop
      @server = nil
    end

    def send(remote_id, *message)
      @server.post(remote_id, *message) if running?
    end
  end
end
