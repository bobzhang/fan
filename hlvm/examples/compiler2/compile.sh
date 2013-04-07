HLVM=../../
ln -fs ../../libruntime.so
ocamllex lex.mll
ocamlyacc -v parse.mly
ocamlc -c expr.ml
ocamlc -c parse.mli
ocamlc -c parse.ml
ocamlc -c lex.ml
ocamlc -I ../../ -I +camlp4 dynlink.cma camlp4lib.cma -g -dtypes -cclib -lstdc++ -cclib -lsigsegv llvm.cma llvm_executionengine.cma llvm_target.cma llvm_scalar_opts.cma llvm_analysis.cma llvm_bitwriter.cma unix.cma $HLVM/llvm.o $HLVM/llvm_stubs.c hlvm.cmo expr.cmo parse.cmo lex.cmo repl.ml -o repl
