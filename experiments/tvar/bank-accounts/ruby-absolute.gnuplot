set terminal pdf enhanced
set output "ruby-absolute.pdf"

set title "Absolute Time Taken with for TVar Bank Account Implementation for Different Rubies"

set key left

set xlabel "Threads"
set ylabel "Absolute Time Taken (s)"

plot "ruby-absolute.data" using 1:2 title col with lines, \
     "" using 1:3 title col with lines, \
     "" using 1:4 title col with lines
