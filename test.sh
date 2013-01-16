#! /bin/sh 
# -*- Mode:Shell-script -*-
set -e
FILES=('c' 'a' 'b' 'c' )
for i in ${FILES[@]}
do
  echo $i
done
