module Concurrent

  # @!visibility private
  class ForkJoinContext # :nodoc:

    def initialize
      @tasks = []
    end

    def fork(&block)
      raise ArgumentError.new('no block given') unless block_given?
      add_task(block)
    end

    def add_task(task)
      @tasks << task
    end

    def join
      sequential_task = @tasks.first
      parallel_tasks = @tasks.drop(1)

      futures = parallel_tasks.map do |block|
        Future.execute(&block)
      end

      [sequential_task.call] + futures.map do |future|
        future.value
      end
    end

  end

  # Run a sequence of tasks concurrently and in parallel if resources are
  # available, returning a list of their results. The tasks are finished and
  # their results are ready when the call to #join returns. If tasks perform
  # side effects the return values can be safely ignored.
  #
  # Our implementation of fork-join is similar to `spawn` and `sync` in the Cilk
  # language, `async` and `finish` in Habanero Java, and ` RecursiveTask` and
  # `invokeall` in Doug Lea's Java framework. Unlike the latter we do not have
  # a task object, as we tend towards the Ruby approach of tasks as blocks
  # rather than methods on objects.
  #
  # Fork-join is suited for simple divide-and-conquer concurrency or
  # parallelism, where you want to use concurrency for one part of a program
  # that generally sequential. The #join block starts the concurrent tasks and
  # does not return until they have all finished, which you may find is a
  # simpler model rather than dealing with asynchronous values and concurrent
  # objects.
  #
  # Where the Ruby implementation permits we use a work-stealing scheduler that
  # is suited to load balancing a tree of parallel tasks with minimal overhead.
  #
  # Concurrent::join do
  #   fork do
  #     ...first concurrent task...
  #   end
  #
  #   fork do
  #     ...first concurrent task...
  #   end
  #
  #   ...more concurrent tasks...
  # end
  #
  # Concurrent::join do
  #   fork { 1 }
  #   fork { 2 }
  # end #=> [1, 2]
  #
  # @yield A block containing calls to #fork, which each take a block to execute concurrently
  #
  # @return [[Object]] the list of results from the tasks
  #
  # @raise [ArgumentError] if no block is given
  def join(&block)
    raise ArgumentError.new('no block given') unless block_given?
    builder = ForkJoinContext.new
    builder.instance_eval(&block)
    builder.join
  end

  # Equivalent to `Concurrent::join`, but will produces a single array of values
  # from arrays produced by each fork.
  def flat_join(&block)
    # TOOD(CS): more efficient implementation without temporary arrays
    join(&block).flatten(1)
  end

  # Equivalent to `Concurrent::join`, but instead of yielding to a block that
  # creates tasks with the `fork` command, accepts an array of tasks that are
  # procs or lambdas.
  #
  # This alternative syntax may be better when your tasks are simple expressions.
  #
  # Concurrent::fork_join(
  #   -> { 1 }
  #   -> { 2 }
  # ) #=> [1, 2]
  def fork_join(*tasks)
    builder = ForkJoinContext.new

    tasks.each do |task|
      builder.add_task task
    end

    builder.join
  end

  # Equivalent to `Concurrent::fork_join`, but will produces a single array of values
  # from arrays produced by each fork.
  def flat_fork_join(*tasks)
    # TOOD(CS): more efficient implementation without temporary arrays
    fork_join(*tasks).flatten(1)
  end

  module_function :join, :flat_join, :fork_join, :flat_fork_join

end
