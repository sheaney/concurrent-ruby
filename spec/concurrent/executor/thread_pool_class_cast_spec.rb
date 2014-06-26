require 'spec_helper'

module Concurrent

  describe SingleThreadExecutor do
    if jruby?
      it 'inherits from JavaSingleThreadExecutor' do
        expect(SingleThreadExecutor.ancestors).to include(JavaSingleThreadExecutor)
      end
    else
      it 'inherits from RubySingleThreadExecutor' do
        expect(SingleThreadExecutor.ancestors).to include(RubySingleThreadExecutor)
      end
    end
  end

  describe ThreadPoolExecutor do
    if jruby?
      it 'inherits from JavaThreadPoolExecutor' do
        expect(ThreadPoolExecutor.ancestors).to include(JavaThreadPoolExecutor)
      end
    else
      it 'inherits from RubyThreadPoolExecutor' do
        expect(ThreadPoolExecutor.ancestors).to include(RubyThreadPoolExecutor)
      end
    end
  end

  describe CachedThreadPool do
    if jruby?
      it 'inherits from JavaCachedThreadPool' do
        expect(CachedThreadPool.ancestors).to include(JavaCachedThreadPool)
      end
    else
      it 'inherits from RubyCachedThreadPool' do
        expect(CachedThreadPool.ancestors).to include(RubyCachedThreadPool)
      end
    end
  end

  describe FixedThreadPool do
    if jruby?
      it 'inherits from JavaFixedThreadPool' do
        expect(FixedThreadPool.ancestors).to include(JavaFixedThreadPool)
      end
    else
      it 'inherits from RubyFixedThreadPool' do
        expect(FixedThreadPool.ancestors).to include(RubyFixedThreadPool)
      end
    end
  end
end
