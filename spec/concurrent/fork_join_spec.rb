require 'spec_helper'

module Concurrent

  describe 'join' do

    it 'raises an exception when no block given' do
      expect {
        Concurrent::join
      }.to raise_error(ArgumentError)
    end

    it 'executes fork tasks and returns when they\'re finished' do
      n = 0

      Concurrent::join do
        fork { n += 1 }
        fork { n += 1 }
        fork { n += 1 }
      end

      n.should == 3
    end

    it 'returns values in order' do
      Concurrent::join do
        fork { 1 }
        fork { 2 }
        fork { 3 }
      end.should == [1, 2, 3]
    end

    let(:executor) { ImmediateExecutor.new }

    it 'uses the executor given with the :executor option' do
      pending "deadlocks - as if the second fork isn't run"

      executor.should_receive(:post)
      Concurrent::join(executor: executor) do
        fork { }
        fork { }
      end
    end

    it 'uses the global operation pool when :operation is true' do
      Concurrent.configuration.should_receive(:global_operation_pool).and_return(executor)
      Concurrent::join(operation: true) do
        fork { }
        fork { }
      end
    end

    it 'uses the global task pool when :task is true' do
      Concurrent.configuration.should_receive(:global_task_pool).and_return(executor)
      Concurrent::join(task: true) do
        fork { }
        fork { }
      end
    end

    it 'uses the global task pool by default' do
      Concurrent.configuration.should_receive(:global_task_pool).and_return(executor)
      Concurrent::join do
        fork { }
        fork { }
      end
    end

  end

  describe 'flat_join' do

    it 'raises an exception when no block given' do
      expect {
        Concurrent::flat_join
      }.to raise_error(ArgumentError)
    end

    it 'flattens returned arrays' do
      Concurrent::flat_join do
        fork { [1] }
        fork { [2, 3] }
        fork { [4] }
      end.should == [1, 2, 3, 4]
    end

    it 'allows non-arrays' do
      Concurrent::flat_join do
        fork { 1 }
        fork { [2, 3] }
        fork { 4 }
      end.should == [1, 2, 3, 4]
    end

    it 'does not flatten beyond one level' do
      Concurrent::flat_join do
        fork { 1 }
        fork { [2, [3, 4]] }
        fork { 5 }
      end.should == [1, 2, [3, 4], 5]
    end

  end

  describe 'fork_join' do

    it 'executes fork tasks and returns when they\'re finished' do
      n = 0

      Concurrent::fork_join(
        -> { n += 1 },
        -> { n += 1 },
        -> { n += 1 }
      )

      n.should == 3
    end

    it 'returns values in order' do
      Concurrent::fork_join(
        -> { 1 },
        -> { 2 },
        -> { 3 }
      ).should == [1, 2, 3]
    end

  end

  describe 'flat_fork_join' do

    it 'flattens returned arrays' do
      Concurrent::flat_fork_join(
        -> { [1] },
        -> { [2, 3] },
        -> { [4] }
      ).should == [1, 2, 3, 4]
    end

    it 'allows non-arrays' do
      Concurrent::flat_fork_join(
        -> { 1 },
        -> { [2, 3] },
        -> { 4 }
      ).should == [1, 2, 3, 4]
    end

    it 'does not flatten beyond one level' do
      Concurrent::flat_fork_join(
        -> { 1 },
        -> { [2, [3, 4]] },
        -> { 5 }
      ).should == [1, 2, [3, 4], 5]
    end

  end

end
