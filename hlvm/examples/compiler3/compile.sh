HLVM=../../
ln -fs ../../libruntime.so
ocamllex lex.mll
ocamlyacc -v parse.mly
ocamlc -dtypes -c expr.ml
ocamlc -dtypes -c parse.mli
ocamlc -dtypes -c parse.ml
ocamlc -dtypes -c lex.ml
ocamlc -dtypes -I ../../ -I +camlp4 dynlink.cma camlp4lib.cma -g -dtypes -cclib -lstdc++ -cclib -lsigsegv llvm.cma llvm_executionengine.cma llvm_target.cma llvm_scalar_opts.cma llvm_analysis.cma llvm_bitwriter.cma unix.cma $HLVM/llvm.o $HLVM/llvm_stubs.c hlvm.cmo expr.cmo parse.cmo lex.cmo repl.ml -o repl
