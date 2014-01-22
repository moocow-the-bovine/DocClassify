#!/bin/bash

##-- command-line
showhelp() {
  cat <<EOF >&2
Usage: $0 [OPTIONS] SRCDIR

Options:
  -dummy      # just print commands, don't run
  -o OUTBASE  # set output basename (default=\${SRC%.d})
  -no-create  # disable corpus creation
  -no-train   # disable mapper training

EOF
}

while test $# -gt 0 ; do
  case "$1" in
    -h|-help) showhelp; exit 0; ;;
    -d|-dummy) dummy=1; ;;
    -o|-out|-outbase) if test $# -gt 1 ; then shift; base="$1"; fi; ;;
    -no-create|-nocreate|-nc) no_create=1; ;;
    -no-train|-notrain|-nt)  no_train=1; ;;
    *) src="$1" ;;
  esac
  shift
done

if test -z "$src" ; then showhelp; exit 0; fi
test -n "$src"  || src=data/dta-ner
test -n "$base" || base="${src%.d}"

test "$1" = "-dummy" && dummy=y
#dummy=""

runcmd() {
  echo "$@" >&2
  test -n "$dummy" || "$@"
}

##-- create
test -n "$no_create" || runcmd ./dc-corpus-create.perl -v=3 -dc=cab1g -o=$base.corpus.xml $src

##-- train
for wraw in 0 1 ; do
 echo "=== twRaw=$wraw"
 map="$base.wraw${wraw}.map.bin"
 test -n "$no_train" \
 || runcmd ./dc-mapper-train.perl -r=1024 -lzc=Cab -mdf=1 -mf=2 -nonull -mo=twRaw=$wraw -mo=weightByCat=0 -noclear -v=3 -o=$map $base.corpus.xml

 ##-- test
 #runcmd ./testme.perl $map -raw    > $base.prof-raw
 #runcmd ./testme.perl $map -cooked > $base.prof-cooked
 #runcmd ./testme.perl $map -json > $base.json
done
