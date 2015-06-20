#!/bin/sh
# echo $1
mv -f $1.ml $1.mli ../cold/
ln -s ../cold/$1.ml 
ln -s ../cold/$1.mli 
