set terminal pdf enhanced
set output "implementation-scalability.pdf"

set title "Scalability of Different Bank Account Implementations on JRuby"

set key left

set xlabel "Threads"
set ylabel "Scalability (s/s)"

plot "implementation-scalability.data" using 1:2 title col with lines, \
     "" using 1:3 title col with lines, \
     "" using 1:4 title col with lines, \
     "" using 1:5 title col with lines
