#! /bin/sh 
# -*- Mode:Shell-script -*-
set -e
mv src/Camlp4Parsers.ml src/tmp
mv src/FanParsers.ml src/Camlp4Parsers.ml
mv src/tmp src/FanParsers.ml