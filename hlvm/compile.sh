g++ -pg -pthread -Wall -O3 -c llvm.cpp
g++ -pg -pthread -Wall -O3 -fPIC -shared runtime.cpp -o libruntime.so
#ocamlc -g -dtypes -pp "/home/jdh30/src/deriving-0.1.1/syntax/deriving" -cclib -lstdc++ -cclib -lsigsegv llvm.cma llvm_executionengine.cma llvm_target.cma llvm_scalar_opts.cma llvm_analysis.cma llvm_bitwriter.cma unix.cma llvm.o llvm_stubs.c hlvm.ml test.ml -o hlvm

ocamlc -g -dtypes -cclib -lstdc++ -cclib -lsigsegv llvm.cma llvm_executionengine.cma llvm_target.cma llvm_scalar_opts.cma llvm_analysis.cma llvm_bitwriter.cma unix.cma llvm.o llvm_stubs.c hlvm.ml test.ml -o hlvm

#ocamlc -pp camlp4oof -I +camlp4 dynlink.cma camlp4lib.cma -g -dtypes -cclib -lstdc++ -cclib -lsigsegv llvm.cma llvm_executionengine.cma llvm_target.cma llvm_scalar_opts.cma llvm_analysis.cma llvm_bitwriter.cma unix.cma llvm.o llvm_stubs.c hlvm.cmo toplevel.ml -o toplevel
