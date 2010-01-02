#!/bin/sh

#DUMMY='yes'

if test $# -lt 2; then
  echo "Usage: $0 TRAIN_BASE TEST_BASE [INFIX='' [TRAINARGS=none]]"
  exit 1;
fi

train="$1"
shift;
test="$1"
shift;
infix="$1"
shift;

rc=0

if test -f ./dc-mapper-train.perl ; then
  progdir=./
elif test -f ../dc-mapper-train.perl ; then
  progdir=../
else
  progdir=""
fi

runcmd() {
  echo "$@"
  test -n "$DUMMY" || "$@" || rc=1
}

echo "##=================================================================="
echo "## $0: TRAIN"
runcmd ${progdir}dc-mapper-train.perl "$train.corpus.xml" "$@" -o "$train.$infix.map.bin" || exit 1

echo ""
echo "##=================================================================="
echo "## $0: APPLY"
runcmd ${progdir}dc-mapper-apply.perl "$train.$infix.bin" "$test.corpus.xml" -o "$test.$infix.out.xml" || exit 2

echo ""
echo "##=================================================================="
echo "## $0: EVAL"
runcmd ${progdir}dc-mapper-eval.perl "$test.corpus.xml" "$test.$infix.out.xml" -o "$test.$infix.eval.xml" || exit 3

exit 0
