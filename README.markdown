README
======

## Install

### Requirements
     OCaml dev (I am not sure it works with ocaml 4.00, probably ok)
     OCamlfind 

### Initial Install
    ocamlbuild cold/fan.byte
     This is mainly for the cold start(compile from the original syntax
     code). Actually all work is done. The following work is to use the
     compile binary to preprocess the revised syntax to verify Fan
     reaches a fix point now.
    mkdir _build/boot
    ln -s _build/cold/fan.byte _build/boot/fan
    hb fan.byte
     If it reaches fix point, then it succeeds. You can also test hb   
   fan.native for fun!

### Develop
     Everytime, you made some nontrival changes(change the grammar,
     lexer), make sure to type hb fan.byte or hb fan.native to
     verify it can be hot-bootstrapped.
     Then type cb to snapshot your changes to cold.(This is for
     distribute purpose) 

## Directory Structure

### src
     The main dev strcuture

### cold
     The mirror of src, for distribute purpose 
