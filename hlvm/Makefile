llvm.o: llvm.cpp
	g++ -pg -pthread -Wall -O3 -c llvm.cpp

libruntime.so: runtime.cpp
	g++ -pg -pthread -Wall -O3 -fPIC -shared runtime.cpp -o libruntime.so

hlvm.inferred.mli: hlvm.ml
	ocamlc -i -I /usr/lib/ocaml hlvm.ml > hlvm.inferred.mli

hlvm.cmo: hlvm.ml
	ocamlc -g -dtypes -I /usr/lib/ocaml hlvm.ml -c

test.cmo: test.ml hlvm.ml
	ocamlc -g -dtypes test.ml -c

test2.cmo: test2.ml hlvm.ml pa_hlvm.cmo
	ocamlc -pp "camlp4o pa_hlvm.cmo" -c -dtypes test2.ml -c

pa_hlvm.cmo: hlvm.cmo pa_hlvm.ml
	ocamlc -pp camlp4oof -I +camlp4 dynlink.cma camlp4lib.cma pa_hlvm.ml -c

test1: llvm.o libruntime.so hlvm.cmo test.cmo
	ocamlc -g -dtypes -cclib -lstdc++ -cclib -lsigsegv -I /usr/lib/ocaml llvm.cma llvm_executionengine.cma llvm_target.cma llvm_scalar_opts.cma llvm_analysis.cma llvm_bitwriter.cma unix.cma llvm.o llvm_stubs.c hlvm.cmo test.cmo -o test1

test2: llvm.o libruntime.so hlvm.cmo test2.cmo
	ocamlc -g -dtypes -cclib -lstdc++ -cclib -lsigsegv -I /usr/lib/ocaml llvm.cma llvm_executionengine.cma llvm_target.cma llvm_scalar_opts.cma llvm_analysis.cma llvm_bitwriter.cma unix.cma llvm.o llvm_stubs.c hlvm.cmo test2.cmo -o test2

clean:
	sh clean.sh
