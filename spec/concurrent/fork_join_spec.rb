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

end
