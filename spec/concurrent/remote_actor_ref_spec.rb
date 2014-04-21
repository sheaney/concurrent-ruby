require 'spec_helper'
require_relative 'actor_ref_shared'

describe Concurrent::RemoteActorRef do

  let(:remote_id) { '123' }

  context 'behavior' do
    let(:actor_server) do
      clazz = Class.new(Concurrent::Actor){
        def act(*message)
          actor_shared_test_message_processor(*message)
        end
      }
      server = Concurrent::ActorServer.new('localhost', 9999)
      server.pool(remote_id, clazz)
      server
    end

    let(:actor_client) do
      client = Concurrent::RemoteActorRef.new(remote_id, host: 'localhost', port: 9999)
      client.run!
      client
    end

  #  it_should_behave_like :actor_ref
  end

  let(:server)    { Concurrent::ActorServer.new }
  subject         { Concurrent::RemoteActorRef.new(remote_id) }

  class MyRemoteActor
    include Concurrent::ActorContext

    attr_accessor :last_message
    def act(*message)
      @last_message = message.first
    end
  end
end
