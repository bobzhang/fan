llc -tailcallopt aout.bc -o aout.s
gcc -g -lsigsegv -ldl -lm aout.s -o aout
gcc -g -p -lsigsegv -ldl -lm aout.s -o aoutprof
opt -tailcallelim -std-compile-opts <aout.bc >aoutopt.bc
llc -tailcallopt aoutopt.bc -o aoutopt.s
gcc -g -lsigsegv -ldl -lm aoutopt.s -o aoutopt
