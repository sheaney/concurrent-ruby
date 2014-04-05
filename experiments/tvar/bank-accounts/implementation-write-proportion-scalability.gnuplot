set terminal pdf enhanced
set output "implementation-write-proportion-scalability.pdf"

set title "Scalability of Different Bank Account Implementations on JRuby Between 1 and 2 Threads for Different Read-Write Workloads"

set style data histogram
set style histogram cluster gap 1
set style fill solid 1.0

set key right

set xlabel "Proportion of Transactions that Write"
set ylabel "Scalability (s/s)"

plot "implementation-write-proportion-scalability.data" using 2:xtic(1) title col, \
     "" using 3:xtic(1) title col, \
     "" using 4:xtic(1) title col, \
     "" using 5:xtic(1) title col
