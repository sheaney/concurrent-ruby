require 'spec_helper'

share_examples_for :count_down_latch do

  let(:latch) { described_class.new(3) }
  let(:zero_count_latch) { described_class.new(0) }

  context '#initialize' do

    it 'raises an exception if the initial count is less than zero' do
      expect {
        described_class.new(-1)
      }.to raise_error(ArgumentError)
    end

    it 'raises an exception if the initial count is not an integer' do
      expect {
        described_class.new('foo')
      }.to raise_error(ArgumentError)
    end
  end

  describe '#count' do

    it 'should be the value passed to the constructor' do
      latch.count.should eq 3
    end

    it 'should be decreased after every count down' do
      latch.count_down
      latch.count.should eq 2
    end

    it 'should not go below zero' do
      5.times { latch.count_down }
      latch.count.should eq 0
    end
  end

  describe '#wait' do

    context 'count set to zero' do
      it 'should return true immediately' do
        result = zero_count_latch.wait
        result.should be_true
      end

      it 'should return true immediately with timeout' do
        result = zero_count_latch.wait(5)
        result.should be_true
      end
    end

    context 'non zero count' do

      it 'should block thread until counter is set to zero' do
        3.times do
          Thread.new { sleep(0.1); latch.count_down }
        end

        result = latch.wait
        result.should be_true
        latch.count.should eq 0
      end

      it 'should block until counter is set to zero with timeout' do
        3.times do
          Thread.new { sleep(0.1); latch.count_down }
        end

        result = latch.wait(1)
        result.should be_true
        latch.count.should eq 0

      end

      it 'should block until timeout and return false when counter is not set to zero' do
        result = latch.wait(0.1)
        result.should be_false
        latch.count.should eq 3
      end
    end
  end
end

module Concurrent

  describe MutexCountDownLatch do

    it_should_behave_like :count_down_latch

    context 'spurious wake ups' do

      subject { described_class.new(3) }

      before(:each) do
        def subject.simulate_spurious_wake_up
          @mutex.synchronize do
            @condition.signal
            @condition.broadcast
          end
        end
      end

      it 'should resist to spurious wake ups without timeout' do
        @expected = false
        Thread.new { subject.wait; @expected = true }

        sleep(0.1)
        subject.simulate_spurious_wake_up

        sleep(0.1)
        @expected.should be_false
      end

      it 'should resist to spurious wake ups with timeout' do
        @expected = false
        Thread.new { subject.wait(0.5); @expected = true }

        sleep(0.1)
        subject.simulate_spurious_wake_up

        sleep(0.1)
        @expected.should be_false

        sleep(0.4)
        @expected.should be_true
      end
    end
  end

  if TestHelpers.jruby?

    describe JavaCountDownLatch do

      it_should_behave_like :count_down_latch
    end
  end

  describe CountDownLatch do
    if jruby?
      it 'inherits from JavaCountDownLatch' do
        CountDownLatch.ancestors.should include(JavaCountDownLatch)
      end
    else
      it 'inherits from MutexCountDownLatch' do
        CountDownLatch.ancestors.should include(MutexCountDownLatch)
      end
    end
  end
end
