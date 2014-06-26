require 'spec_helper'
require 'concurrent/actress'

module Concurrent
  module Actress
    i_know_it_is_experimental!

    class Reference
      def backdoor(&block)
        core.send :schedule_execution do
          core.instance_eval &block
        end
      end
    end

    describe 'Concurrent::Actress' do
      prepend_before do
        do_no_reset!
      end

      def terminate_actors(*actors)
        actors.each do |actor|
          actor.backdoor { terminate! }
          actor.terminated.wait
        end
      end

      class Ping
        include Context

        def initialize(queue)
          @queue = queue
        end

        def on_message(message)
          case message
          when :terminate
            terminate!
          when :child
            AdHoc.spawn(:pong, @queue) { |queue| -> m { queue << m } }
          else
            @queue << message
            message
          end
        end
      end

      # def trace!
      #   set_trace_func proc { |event, file, line, id, binding, classname|
      #     # thread = eval('Thread.current', binding).object_id.to_s(16)
      #     printf "%8s %20s %20s %s %s:%-2d\n", event, id, classname, nil, file, line
      #   }
      #   yield
      # ensure
      #   set_trace_func nil
      # end

      describe 'stress test' do
        1.times do |i|
          it format('run %3d', i) do
            # puts format('run %3d', i)
            Array.new(10).map do
              Thread.new do
                10.times do
                  # trace! do
                  queue = Queue.new
                  actor = Ping.spawn :ping, queue

                  # when spawn returns children are set
                  expect(Concurrent::Actress.root.send(:core).instance_variable_get(:@children)).to include(actor)

                  actor << 'a' << 1
                  expect(queue.pop).to eq 'a'
                  expect(actor.ask(2).value).to eq 2

                  expect(actor.parent).to eq Concurrent::Actress.root
                  expect(Concurrent::Actress.root.path).to eq '/'
                  expect(actor.path).to eq '/ping'
                  child = actor.ask(:child).value
                  expect(child.path).to eq '/ping/pong'
                  queue.clear
                  child.ask(3)
                  expect(queue.pop).to eq 3

                  actor << :terminate
                  expect(actor.ask(:blow_up).wait).to be_rejected
                  terminate_actors actor, child
                end
              end
            end.each(&:join)
          end
        end
      end

      describe 'spawning' do
        describe 'Actress#spawn' do
          behaviour = -> v { -> _ { v } }
          subjects  = { spawn:                 -> { Actress.spawn(AdHoc, :ping, 'arg', &behaviour) },
                        context_spawn:         -> { AdHoc.spawn(:ping, 'arg', &behaviour) },
                        spawn_by_hash:         -> { Actress.spawn(class: AdHoc, name: :ping, args: ['arg'], &behaviour) },
                        context_spawn_by_hash: -> { AdHoc.spawn(name: :ping, args: ['arg'], &behaviour) } }

          subjects.each do |desc, subject_definition|
            describe desc do
              subject &subject_definition
              after { terminate_actors subject }

              describe '#path' do
                subject { super().path }
                it { is_expected.to eq '/ping' }
              end

              describe '#parent' do
                subject { super().parent }
                it { skip('intermittent JRuby deadlock'); is_expected.to eq Actress.root }
              end

              describe '#name' do
                subject { super().name }
                it { is_expected.to eq 'ping' }
              end
              it('executor should be global') { expect(subject.executor).to eq Concurrent.configuration.global_task_pool }

              describe '#reference' do
                subject { super().reference }
                it { is_expected.to eq subject }
              end
              it 'returns arg' do
                expect(subject.ask!(:anything)).to eq 'arg'
              end
            end
          end
        end

        it 'terminates on failed initialization' do
          a = AdHoc.spawn(name: :fail, logger: Concurrent.configuration.no_logger) { raise }
          expect(a.ask(nil).wait.rejected?).to be_truthy
          expect(a.terminated?).to be_truthy
        end

        it 'terminates on failed initialization and raises with spawn!' do
          expect do
            AdHoc.spawn!(name: :fail, logger: Concurrent.configuration.no_logger) { raise 'm' }
          end.to raise_error(StandardError, 'm')
        end

        it 'terminates on failed message processing' do
          a = AdHoc.spawn(name: :fail, logger: Concurrent.configuration.no_logger) { -> _ { raise } }
          expect(a.ask(nil).wait.rejected?).to be_truthy
          expect(a.terminated?).to be_truthy
        end
      end

      describe 'messaging' do
        subject { AdHoc.spawn(:add) { c = 0; -> v { c = c + v } } }
        specify do
          subject.tell(1).tell(1)
          subject << 1 << 1
          expect(subject.ask(0).value!).to eq 4
        end
        after { terminate_actors subject }
      end

      describe 'children' do
        let(:parent) do
          AdHoc.spawn(:parent) do
            -> message do
              if message == :child
                AdHoc.spawn(:child) { -> _ { parent } }
              else
                children
              end
            end
          end
        end

        it 'has children set after a child is created' do
          child = parent.ask!(:child)
          expect(parent.ask!(nil)).to include(child)
          expect(child.ask!(nil)).to eq parent

          terminate_actors parent, child
        end
      end

      describe 'envelope' do
        subject { AdHoc.spawn(:subject) { -> _ { envelope } } }
        specify do
          envelope = subject.ask!('a')
          expect(envelope).to be_a_kind_of Envelope
          expect(envelope.message).to eq 'a'
          expect(envelope.ivar).to be_completed
          expect(envelope.ivar.value).to eq envelope
          expect(envelope.sender).to eq Thread.current
          terminate_actors subject
        end
      end

      describe 'termination' do
        subject do
          AdHoc.spawn(:parent) do
            child = AdHoc.spawn(:child) { -> v { v } }
            -> v do
              if v == :terminate
                terminate!
              else
                child
              end
            end
          end
        end

        it 'terminates with all its children' do
          child = subject.ask! :child
          expect(subject.terminated?).to be_falsey
          subject.ask(:terminate).wait
          expect(subject.terminated?).to be_truthy
          child.terminated.wait
          expect(child.terminated?).to be_truthy

          terminate_actors subject, child
        end
      end

    end
  end
end
