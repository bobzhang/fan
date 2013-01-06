#! /bin/sh 
# -*- Mode:Shell-script -*-
set -e
ocamlbuild tmp/Ast.ml tmp/FanAst.ml
cp _build/tmp/Ast.ml _build/tmp/FanAst.ml cold/