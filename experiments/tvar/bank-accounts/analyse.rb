require 'set'

require_relative 'config'

SAMPLES = {}

SCALABILITY_REFERENCE = 1

SCALABILITY_A = 1
SCALABILITY_B = 2

WORK_PROPORTION = 75

def read_file(ruby, file)
  File.foreach(file) do |line|
    match = line.match(/(UnsynchronizedBank|CoarseLockBank|FineLockBank|TransactionalBank|) (\d+) (\d+) ([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)/)
    
    if match
      implementation = match[1]
      write_proportion = match[2].to_i
      threads = match[3].to_i
      sample = match[4].to_f

      tuple = [ruby, implementation, write_proportion, threads]

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
  SAMPLES[tuple] = mean(samples.drop(WARMUP_REPS))
end

File.open("implementation-write-proportion-scalability.data", "w") do |file|
  file.write("Threads UnsynchronizedBank CoarseLockBank FineLockBank TransactionalBank\n")

  WRITE_PROPORTIONS.each do |write_proportion|
    file.write("#{write_proportion}% #{SAMPLES[['jruby', 'UnsynchronizedBank', write_proportion, SCALABILITY_A]]/SAMPLES[['jruby', 'UnsynchronizedBank', write_proportion, SCALABILITY_B]]} " +
      "#{SAMPLES[['jruby', 'CoarseLockBank', write_proportion, SCALABILITY_A]]/SAMPLES[['jruby', 'CoarseLockBank', write_proportion, SCALABILITY_B]]} " +
      "#{SAMPLES[['jruby', 'FineLockBank', write_proportion, SCALABILITY_A]]/SAMPLES[['jruby', 'FineLockBank', write_proportion, SCALABILITY_B]]} " +
      "#{SAMPLES[['jruby', 'TransactionalBank', write_proportion, SCALABILITY_A]]/SAMPLES[['jruby', 'TransactionalBank', write_proportion, SCALABILITY_B]]}\n")
  end
end

`gnuplot implementation-write-proportion-scalability.gnuplot`

File.open("implementation-absolute.data", "w") do |file|
  file.write("Threads UnsynchronizedBank CoarseLockBank FineLockBank TransactionalBank\n")

  THREADS.each do |threads|
    file.write("#{threads} #{SAMPLES[['jruby', 'UnsynchronizedBank', WORK_PROPORTION, threads]]} " +
      "#{SAMPLES[['jruby', 'CoarseLockBank', WORK_PROPORTION, threads]]} " +
      "#{SAMPLES[['jruby', 'FineLockBank', WORK_PROPORTION, threads]]} " +
      "#{SAMPLES[['jruby', 'TransactionalBank', WORK_PROPORTION, threads]]}\n")
  end
end

`gnuplot implementation-absolute.gnuplot`

File.open("implementation-scalability.data", "w") do |file|
  file.write("Threads UnsynchronizedBank CoarseLockBank FineLockBank TransactionalBank\n")

  unsync_baseline = SAMPLES[["jruby", "UnsynchronizedBank", WORK_PROPORTION, SCALABILITY_REFERENCE]]
  coarse_baseline = SAMPLES[["jruby", "CoarseLockBank", WORK_PROPORTION, SCALABILITY_REFERENCE]]
  fine_baseline = SAMPLES[["jruby", "FineLockBank", WORK_PROPORTION, SCALABILITY_REFERENCE]]
  transactional_baseline = SAMPLES[["jruby", "TransactionalBank", WORK_PROPORTION, SCALABILITY_REFERENCE]]

  THREADS.each do |threads|
    file.write("#{threads} #{unsync_baseline/SAMPLES[['jruby', 'UnsynchronizedBank', WORK_PROPORTION, threads]]} " +
      "#{coarse_baseline/SAMPLES[['jruby', 'CoarseLockBank', WORK_PROPORTION, threads]]} " +
      "#{fine_baseline/SAMPLES[['jruby', 'FineLockBank', WORK_PROPORTION, threads]]} " +
      "#{transactional_baseline/SAMPLES[['jruby', 'TransactionalBank', WORK_PROPORTION, threads]]}\n")
  end
end

`gnuplot implementation-scalability.gnuplot`

File.open("ruby-absolute.data", "w") do |file|
  file.write("Threads MRI JRuby Rubinius\n")

  THREADS.each do |threads|
    file.write("#{threads} #{SAMPLES[['mri', 'TransactionalBank', WORK_PROPORTION, threads]]} " +
      "#{SAMPLES[['jruby', 'TransactionalBank', WORK_PROPORTION, threads]]} " +
      "#{SAMPLES[['rbx', 'TransactionalBank', WORK_PROPORTION, threads]]}\n")
  end
end

`gnuplot ruby-absolute.gnuplot`

File.open("ruby-scalability.data", "w") do |file|
  file.write("Threads MRI JRuby Rubinius\n")

  mri_baseline = SAMPLES[["mri", "TransactionalBank", WORK_PROPORTION, SCALABILITY_REFERENCE]]
  jruby_baseline = SAMPLES[["jruby", "TransactionalBank", WORK_PROPORTION, SCALABILITY_REFERENCE]]
  rbx_baseline = SAMPLES[["rbx", "TransactionalBank", WORK_PROPORTION, SCALABILITY_REFERENCE]]

  THREADS.each do |threads|
    file.write("#{threads} #{mri_baseline/SAMPLES[['mri', 'TransactionalBank', WORK_PROPORTION, threads]]} " +
      "#{jruby_baseline/SAMPLES[['jruby', 'TransactionalBank', WORK_PROPORTION, threads]]} " +
      "#{rbx_baseline/SAMPLES[['rbx', 'TransactionalBank', WORK_PROPORTION, threads]]}\n")
  end
end

`gnuplot ruby-scalability.gnuplot`
