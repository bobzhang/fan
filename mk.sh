#! /bin/sh 
# -*- Mode:Shell-script -*-
set -e
FILES=('Ast' 'FanAst' 'Objs' 'AstLoc' 'ParserRevise' 'Ast2pt' 'FanGrammar' 'FanOps' 'FanObjs' 'FanMeta')

OB= ocamlbuild
TMP=tmp
for i in ${FILES[@]}
do
    ocamlbuild $TMP/$i.ml
    echo "$OB $TMP/$i.ml\n"
    cp _build/$TMP/$i.ml cold/
done
# ocamlbuild tmp/Ast.ml tmp/FanAst.ml tmp/ParserRevise.ml tmp/Sig.ml tmp/OCamlInitSyntax.ml tmp/Ast2pt.ml
# cp _build/tmp/Ast.ml _build/tmp/FanAst.ml _build/tmp/ParserRevise.ml _build/tmp/Sig.ml _build/tmp/OCamlInitSyntax.ml _build/tmp/Ast2pt.ml cold/
