#!/bin/bash

src="$1"
base="$2"
test -n "$src"  || src=data/cab-ner
test -n "$base" || base="${src%.d}"

dummy=""

runcmd() {
  echo "$@" >&2
  test -n "$dummy" || "$@"
}

##-- create
runcmd ./dc-corpus-create.perl -v=3 -dc=cab1g -o=$base.corpus.xml $src

##-- train
runcmd ./dc-mapper-train.perl -r=64 -lzc=Cab -mdf=2 -mf=5 -nonull -mo=weightByCat=0 -noclear -v=3 -o=$base.map.bin $base.corpus.xml

##-- test
runcmd ./testme.perl $base.map.bin -raw   > $base.prof-raw
runcmd ./testme.perl $base.map.bin -noraw > $base.prof-cooked
