#! /bin/sh 
# -*- Mode:Shell-script -*-

ocamlfind ocamlmktop -custom -o olvm _build/src/FanTop.cma llvm.cma \
llvm_analysis.cma llvm_bitreader.cma llvm_bitwriter.cma llvm_ipo.cma  llvm_executionengine.cma \
llvm_target.cma llvm_scalar_opts.cma -cc g++ -linkpkg
 # _build/llvm/putchard.o _build/llvm/putchard.cma
