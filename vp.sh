#! /bin/sh 
# -*- Mode:Shell-script -*-
ocamlbuild src/Fan.native && ./re src Fan.native  && ./snapshot && ocamlbuild cold/Fan.byte