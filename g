#! /bin/sh 
# -*- Mode:Shell-script -*-
set -e

git add .
git commit -m '$1'
git push origin master