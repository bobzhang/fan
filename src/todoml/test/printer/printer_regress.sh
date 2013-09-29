#! /bin/sh 
# -*- Mode:Shell-script -*-
set -e
_build/test/PprintastDriver.native test/printer/*.ml ~/ocaml-svn/*/*.ml 1>log
