set terminal pdf enhanced
set output "ruby-scalability.pdf"

set title "Scalability of TVar Bank Account Implementation for Different Rubies with 75% Writes"

set key left

set xlabel "Threads"
set ylabel "Scalability (s/s)"

plot "ruby-scalability.data" using 1:2 title col with lines, \
     "" using 1:3 title col with lines, \
     "" using 1:4 title col with lines
