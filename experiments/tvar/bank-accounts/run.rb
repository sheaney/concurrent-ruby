require 'monitor'
require 'concurrent'

require_relative 'config'

class UnsynchronizedBank

  def initialize(account_totals)
    @accounts = account_totals.dup
  end

  def statement(accounts)
    accounts.map do |account|
      @accounts[account]
    end
  end

  def transfer(from, to, sum)
    if @accounts[from] < sum
      false
    else
      @accounts[from] -= sum
      @accounts[to] += sum
      true
    end
  end

  def grand_total
    @accounts.inject(0, :+)
  end

end

class CoarseLockBank

  def initialize(account_totals)
    @accounts = account_totals.dup
    @lock = Mutex.new
  end

  def statement(accounts)
    @lock.synchronize do
      accounts.map do |account|
        @accounts[account]
      end
    end
  end

  def transfer(from, to, sum)
    @lock.synchronize do
      if @accounts[from] < sum
        false
      else
        @accounts[from] -= sum
        @accounts[to] += sum
        true
      end
    end
  end

  def grand_total
    @accounts.inject(0, :+)
  end

end

class FineLockBank

  Account = Struct.new(:lock, :value)

  def initialize(account_totals)
    @accounts = account_totals.map do |v|
      Account.new(Monitor.new, v)
    end
  end

  def statement(accounts)
    locks = accounts.map{ |account| @accounts[account].lock }
    ordered_locks = locks.sort{ |a, b| a.object_id <=> b.object_id }

    ordered_locks.each{ |lock| lock.mon_enter }

    begin
      accounts.map do |account|
        @accounts[account]
      end
    ensure
      ordered_locks.each{ |lock| lock.mon_exit }
    end
  end

  def transfer(from, to, sum)
    locks = [@accounts[from].lock, @accounts[to].lock]
    ordered_locks = locks.sort{ |a, b| a.object_id <=> b.object_id }

    ordered_locks[0].synchronize do
      ordered_locks[1].synchronize do
        if @accounts[from].value < sum
          false
        else
          @accounts[from].value -= sum
          @accounts[to].value += sum
          true
        end
      end
    end
  end

  def grand_total
    @accounts.map(&:value).inject(0, :+)
  end

end

class TransactionalBank

  def initialize(account_totals)
    @accounts = account_totals.map do |v|
      Concurrent::TVar.new(v)
    end
  end

  def statement(accounts)
    Concurrent::atomically do
      accounts.map do |account|
        @accounts[account].value
      end
    end
  end

  def transfer(from, to, sum)
    Concurrent::atomically do
      if @accounts[from].value < sum
        false
      else
        @accounts[from].value -= sum
        @accounts[to].value += sum
        true
      end
    end
  end

  def grand_total
    @accounts.map(&:value).inject(0, :+)
  end

end

RANDOM = Random.new(0)

Transfer = Struct.new(:from, :to, :sum)
Statement = Struct.new(:accounts)

ACCOUNT_TOTALS = (0..100_000).map do
  RANDOM.rand(100)
end

GRAND_TOTAL = ACCOUNT_TOTALS.inject(0, :+)

def test(bank_class)
  WRITE_PROPORTIONS.each do |write_proportion|
    transactions = (0..TRANSACTIONS_COUNT).map do
      if Random.rand(100) > write_proportion
        Transfer.new(
          RANDOM.rand(ACCOUNT_TOTALS.size),
          RANDOM.rand(ACCOUNT_TOTALS.size),
          RANDOM.rand(100))
      else
        Statement.new([RANDOM.rand(ACCOUNT_TOTALS.size),
          RANDOM.rand(ACCOUNT_TOTALS.size),
          RANDOM.rand(ACCOUNT_TOTALS.size),
          RANDOM.rand(ACCOUNT_TOTALS.size)])
      end
    end

    THREADS.each do |threads|
      REPS.times do
        3.times do
          ObjectSpace.garbage_collect
        end

        transactions_per_thread = transactions.size / threads

        bank = bank_class.new(ACCOUNT_TOTALS)
        total_before = bank.grand_total

        start_barrier = Concurrent::CountDownLatch.new(threads + 1)
        finish_barrier = Concurrent::CountDownLatch.new(threads)

        (1..threads).each do |n|
          Thread.new do
            start_barrier.count_down
            start_barrier.wait

            transactions[(n*transactions_per_thread)..((n+1)*transactions_per_thread)].each do |transaction|
              if transaction.is_a? Transfer
                bank.transfer(transaction.from, transaction.to, transaction.sum)
              elsif transaction.is_a? Statement
                bank.statement(transaction.accounts)
              end
            end

            finish_barrier.count_down
          end
        end

        start = Time.now
        start_barrier.count_down
        start_barrier.wait
        finish_barrier.wait
        time = Time.new - start

        raise "error" unless bank.grand_total == total_before or bank_class == UnsynchronizedBank

        puts "#{bank_class} #{write_proportion} #{threads} #{time}"
      end
    end
  end
end

test UnsynchronizedBank
test CoarseLockBank
test FineLockBank
test TransactionalBank
