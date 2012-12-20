#! /bin/sh 
# -*- Mode:Shell-script -*-
set -e
cd ~/fan/ && ocamlbuild -use-menhir -menhir "menhir --external-tokens Lexer" js/Main.native
