README
======

## Install

### Requirements
     OCaml dev (I am not sure it works with ocaml 4.00, probably ok)
     OCamlfind 

### Initial Install
     just type dist

#### What it mainly did

  * Start. it will make cold/Fan.byte from original syntax This is

        mainly for the cold start(compile from the original syntax
      code). Actually all work is done.

  * Bootstrap. Using the generated binary to preprocess the revised

        syntax to verify Fan reaches a fix point now.
      If it reaches fix point, then it succeeds. You can also test hb    
    Fan.native for fun!

### Develop
     Everytime, you made some nontrival changes(change the grammar,
     lexer), make sure to type hb Fan.byte or hb Fan.native to
     verify it can be hot-bootstrapped.
     Then type cb to snapshot your changes to cold.(This is for
     distribute purpose) 

## Directory Structure

### src
     The main dev strcuture

### cold
     The mirror of src, for distribute purpose 
