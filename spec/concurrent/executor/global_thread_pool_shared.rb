require 'spec_helper'

shared_examples :global_thread_pool do

  context '#post' do

    it 'raises an exception if no block is given' do
      expect {
        subject.post
      }.to raise_error(ArgumentError)
    end

    it 'returns true when the block is added to the queue' do
      expect(subject.post{ nil }).to be_truthy
    end

    it 'calls the block with the given arguments' do
      latch = Concurrent::CountDownLatch.new(1)
      expected = nil
      subject.post(1, 2, 3) do |a, b, c|
        expected = [a, b, c]
        latch.count_down
      end
      latch.wait(0.2)
      expect(expected).to eq [1, 2, 3]
    end

    it 'aliases #<<' do
      latch = Concurrent::CountDownLatch.new(1)
      subject << proc { latch.count_down }
      latch.wait(0.2)
      expect(latch.count).to eq 0
    end
  end
end
