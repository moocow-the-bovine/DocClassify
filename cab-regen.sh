#!/bin/bash

src="$1"
base="$2"
test -n "$src"  || src=data/dta-ner
test -n "$base" || base="${src%.d}"

#dummy=""

runcmd() {
  echo "$@" >&2
  test -n "$dummy" || "$@"
}

##-- create
runcmd ./dc-corpus-create.perl -v=3 -dc=cab1g -o=$base.corpus.xml $src

##-- train
for wraw in 0 1 ; do
 echo "=== twRaw=$wraw"
 map="$base.wraw${wraw}.map.bin"
 runcmd ./dc-mapper-train.perl -r=64 -lzc=Cab -mdf=1 -mf=2 -nonull -mo=twRaw=$wraw -mo=weightByCat=0 -noclear -v=3 -o=$map $base.corpus.xml

 ##-- test
 #runcmd ./testme.perl $map -raw    > $base.prof-raw
 #runcmd ./testme.perl $map -cooked > $base.prof-cooked
 runcmd ./testme.perl $map -json > $base.json
done
