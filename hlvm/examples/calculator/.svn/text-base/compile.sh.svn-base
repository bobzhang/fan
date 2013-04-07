HLVM=../../
ln -fs ../../libruntime.so
ocamlc -pp camlp4oof -I ../../ -I +camlp4 dynlink.cma camlp4lib.cma -g -dtypes -cclib -lstdc++ -cclib -lsigsegv llvm.cma llvm_executionengine.cma llvm_target.cma llvm_scalar_opts.cma llvm_analysis.cma llvm_bitwriter.cma unix.cma $HLVM/llvm.o $HLVM/llvm_stubs.c $HLVM/hlvm.cmo calc.ml -o calc
