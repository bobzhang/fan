HLVM=../../
ln -fs ../../libruntime.so
ocamllex lex.mll
ocamlyacc -v parse.mly
ocamlopt -c expr.ml
ocamlopt -c parse.mli
ocamlopt -c parse.ml
ocamlopt -c lex.ml
ocamlopt -I ../../ -I +camlp4 dynlink.cmxa camlp4lib.cmxa -g -dtypes -cclib -lstdc++ -cclib -lsigsegv llvm.cmxa llvm_executionengine.cmxa llvm_target.cmxa llvm_scalar_opts.cmxa llvm_analysis.cmxa llvm_bitwriter.cmxa unix.cmxa $HLVM/llvm.o $HLVM/llvm_stubs.c hlvm.cmx expr.cmx parse.cmx lex.cmx repl.ml -o repl
