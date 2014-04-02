require 'set'

SAMPLES = {}

THREADS = [2,3,4,5,6,7,8] #Set.new

def read_file(ruby, file)
  File.foreach(file) do |line|
    match = line.match(/(UnsynchronizedBank|CoarseLockBank|FineLockBank|TransactionalBank|) (\d) ([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)/)
    
    if match
      implementation = match[1]
      threads = match[2].to_i
      #THREADS.add(threads)
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

read_file("mri", "mri.data")
read_file("jruby", "jruby.data")
read_file("rbx", "rbx.data")

def mean(samples)
  samples.inject(&:+) / samples.length
end

SAMPLES.each do |tuple, samples|
  SAMPLES[tuple] = mean(samples.drop(10))
end

File.open("implementation-absolute.data", "w") do |file|
  file.write("Threads UnsynchronizedBank CoarseLockBank FineLockBank TransactionalBank\n")

  THREADS.each do |threads|
    file.write("#{threads} #{SAMPLES[['jruby', 'UnsynchronizedBank', threads]]} " +
      "#{SAMPLES[['jruby', 'CoarseLockBank', threads]]} " +
      "#{SAMPLES[['jruby', 'FineLockBank', threads]]} " +
      "#{SAMPLES[['jruby', 'TransactionalBank', threads]]}\n")
  end
end

`gnuplot implementation-absolute.gnuplot`

File.open("implementation-scalability.data", "w") do |file|
  file.write("Threads UnsynchronizedBank CoarseLockBank FineLockBank TransactionalBank\n")

  unsync_baseline = SAMPLES[["jruby", "UnsynchronizedBank", THREADS[0]]]
  coarse_baseline = SAMPLES[["jruby", "CoarseLockBank", THREADS[0]]]
  fine_baseline = SAMPLES[["jruby", "FineLockBank", THREADS[0]]]
  transactional_baseline = SAMPLES[["jruby", "TransactionalBank", THREADS[0]]]

  THREADS.each do |threads|
    file.write("#{threads} #{unsync_baseline/SAMPLES[['jruby', 'UnsynchronizedBank', threads]]} " +
      "#{coarse_baseline/SAMPLES[['jruby', 'CoarseLockBank', threads]]} " +
      "#{fine_baseline/SAMPLES[['jruby', 'FineLockBank', threads]]} " +
      "#{transactional_baseline/SAMPLES[['jruby', 'TransactionalBank', threads]]}\n")
  end
end

`gnuplot implementation-scalability.gnuplot`

File.open("ruby-absolute.data", "w") do |file|
  file.write("Threads MRI JRuby Rubinius\n")

  THREADS.each do |threads|
    file.write("#{threads} #{SAMPLES[['mri', 'TransactionalBank', threads]]} " +
      "#{SAMPLES[['jruby', 'TransactionalBank', threads]]} " +
      "#{SAMPLES[['rbx', 'TransactionalBank', threads]]}\n")
  end
end

`gnuplot ruby-absolute.gnuplot`

File.open("ruby-scalability.data", "w") do |file|
  file.write("Threads MRI JRuby Rubinius\n")

  mri_baseline = SAMPLES[["mri", "TransactionalBank", THREADS[0]]]
  jruby_baseline = SAMPLES[["jruby", "TransactionalBank", THREADS[0]]]
  rbx_baseline = SAMPLES[["rbx", "TransactionalBank", THREADS[0]]]

  THREADS.each do |threads|
    file.write("#{threads} #{mri_baseline/SAMPLES[['mri', 'TransactionalBank', threads]]} " +
      "#{jruby_baseline/SAMPLES[['jruby', 'TransactionalBank', threads]]} " +
      "#{rbx_baseline/SAMPLES[['rbx', 'TransactionalBank', threads]]}\n")
  end
end

`gnuplot ruby-scalability.gnuplot`
