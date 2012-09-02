#! /bin/sh 
# -*- Mode:Shell-script -*-
set -e

target=$1
OB=ocamlbuild

echo "First round buiding $target"
$OB src/$target # using previous target as preprocessor
mv _build/boot/$target _build/boot/$target.old # store it
mv _build/src/$target _build/boot/$target # now use the new one  as preprocessor
echo "First round building finished and do the copy"

echo "Cleaning for the second round building"
make cleansrc
$OB src/$target
echo "Second round building finished; now do the comparison"

if cmp _build/src/$target _build/boot/$target
then
    echo fixpoint for $target
else
    echo $target is different, you should rebootstrap it by cleaning, building and call this script  
fi