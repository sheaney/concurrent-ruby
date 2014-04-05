set terminal pdf enhanced
set output "implementation-absolute.pdf"

set title "Absolute Time Taken with Different Bank Account Implementations on JRuby with 75% Writes"

set key right

set xlabel "Threads"
set ylabel "Absolute Time Taken (s)"

plot "implementation-absolute.data" using 1:2 title col with lines, \
     "" using 1:3 title col with lines, \
     "" using 1:4 title col with lines, \
     "" using 1:5 title col with lines
