#!/bin/sh
set -e 
ocamlopt.opt -w -40 diff_filter.p.ml -o filter.out
