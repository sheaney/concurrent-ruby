SAMPLES = {}

def read_file(ruby, file)
  File.foreach(file) do |line|
    match = line.match(/(UnsynchronizedBank|CoarseLockBank|FineLockBank|TransactionalBank|) (\d) ([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)/)
    
    if match
      implementation = match[1]
      threads = match[2].to_i
      sample = match[3].to_f

      tuple = [ruby, implementation, threads]

      samples = SAMPLES[tuple]

      if samples.nil?
        samples = []
        SAMPLES[tuple] = samples
      end

      samples.push(sample)
    end
  end
end

read_file("jruby", "bank-accounts-jruby.data")
read_file("rbx", "bank-accounts-rbx.data")

def mean(samples)
  samples.inject(&:+) / samples.length
end

SAMPLES.each do |tuple, samples|
  SAMPLES[tuple] = mean(samples.drop(10))
end

File.open("implementation-absolute.data", "w") do |file|
  file.write("Threads UnsynchronizedBank CoarseLockBank FineLockBank TransactionalBank\n")

  (2..8).each do |threads|
    file.write("#{threads} #{SAMPLES[['jruby', 'UnsynchronizedBank', threads]]} " +
      "#{SAMPLES[['jruby', 'CoarseLockBank', threads]]} " +
      "#{SAMPLES[['jruby', 'FineLockBank', threads]]} " +
      "#{SAMPLES[['jruby', 'TransactionalBank', threads]]}\n")
  end
end

File.open("implementation-scalability.data", "w") do |file|
  file.write("Threads UnsynchronizedBank CoarseLockBank FineLockBank TransactionalBank\n")

  unsync_baseline = SAMPLES[["jruby", "UnsynchronizedBank", 2]]
  coarse_baseline = SAMPLES[["jruby", "CoarseLockBank", 2]]
  fine_baseline = SAMPLES[["jruby", "FineLockBank", 2]]
  transactional_baseline = SAMPLES[["jruby", "TransactionalBank", 2]]

  (2..8).each do |threads|
    file.write("#{threads} #{unsync_baseline/SAMPLES[['jruby', 'UnsynchronizedBank', threads]]} " +
      "#{coarse_baseline/SAMPLES[['jruby', 'CoarseLockBank', threads]]} " +
      "#{fine_baseline/SAMPLES[['jruby', 'FineLockBank', threads]]} " +
      "#{transactional_baseline/SAMPLES[['jruby', 'TransactionalBank', threads]]}\n")
  end
end

File.open("ruby-absolute.data", "w") do |file|
  file.write("Threads JRuby Rubinius\n")


  (2..8).each do |threads|
    file.write("#{threads} #{SAMPLES[['jruby', 'TransactionalBank', threads]]} #{SAMPLES[['rbx', 'TransactionalBank', threads]]}\n")
  end
end

File.open("ruby-scalability.data", "w") do |file|
  file.write("Threads JRuby Rubinius\n")

  jruby_baseline = SAMPLES[["jruby", "TransactionalBank", 2]]
  rbx_baseline = SAMPLES[["rbx", "TransactionalBank", 2]]

  (2..8).each do |threads|
    file.write("#{threads} #{jruby_baseline/SAMPLES[['jruby', 'TransactionalBank', threads]]} #{rbx_baseline/SAMPLES[['rbx', 'TransactionalBank', threads]]}\n")
  end
end
