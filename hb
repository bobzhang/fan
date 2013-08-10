#! /bin/sh 
# -*- Mode:Shell-script -*-
set -e

target=$1
OB=ocamlbuild

echo "First round buiding $target"
$OB -quiet src/$target # using previous target as preprocessor

if [ -f _build/boot/$target ]
then mv _build/boot/$target _build/boot/$target.old
fi  # store it

echo "First round building finished and do the copy"
cp _build/src/$target _build/boot/$target

rm -rf _build/boot/fan
ln -s $target _build/boot/fan # now use the new one  as preprocessor




echo "Cleaning for the second round building"
make cleansrc
$OB -quiet src/$target
echo "Second round building finished; now do the comparison"

if cmp _build/src/$target _build/boot/$target
then
    echo fixpoint for $target
    # ocamlbuild -quiet foo.otarget
    
    mv _build/boot/$target _build/boot/$target.old #store as old 
    mv _build/src/$target _build/boot/$target

else
    echo $target is different, you should rebootstrap it by cleaning, building and call this script
    mv _build/boot/$target _build/boot/$target.old #store as old 
    mv _build/src/$target _build/boot/$target
    ./hb $target
fi
