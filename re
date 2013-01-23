#! /bin/sh 
# -*- Mode:Shell-script -*-
set -e
PROGNAME=$(basename $0)
error_exit(){
	echo "${PROGNAME}: ${1:-"Unknown Error"}" 1>&2
	exit 1
}


dir=$1
target=$2
file=_build/$dir/$target

test $target || error_exit "sample: reset hot Fan.byte"
test -f $file || error_exit "$file does not exist"

test -e  _build/boot || mkdir _build/boot
test -e _build/boot/$target && mv _build/boot/$target _build/boot/$target.old
cp $file _build/boot/
rm -f _build/boot/fan
ln -s $target _build/boot/fan