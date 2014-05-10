require 'spec_helper'
require_relative 'actor_ref_shared'

module Concurrent

  describe SimpleActorRef do

    after(:each) do
      subject.shutdown
      sleep(0.1)
    end

    subject do
      create_actor_test_class.spawn
    end

    it_should_behave_like :actor_ref

    context 'construction' do

      it 'supports :args being nil' do
      end

      it 'passes all :args option to the actor constructor' do
      end

      it 'passes the options hash to the ActorRef constructor' do
      end

      it 'calls #on_start on the actor' do
      end
    end

    context 'reset_on_error' do

      it 'creates a new actor on exception when true' do
      end

      it 'does not create a new actor on exception when false' do
      end

      it 'defaults to true' do
      end
    end

    context 'rescue_exception' do

      #after(:each) do
      #  @ref.shutdown if @ref
      #end

      it 'rescues Exception in the actor thread when true' do
      end

      it 'rescues StandardError in the actor thread when false' do
      end

      it 'defaults to false' do
      end
    end

    context '#shutdown' do

      it 'calls #on_shutdown when shutdown' do
      end
    end
  end
end
