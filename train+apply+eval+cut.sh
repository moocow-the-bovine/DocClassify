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
echo "## $0: RAW: TRAIN"
#runcmd ${progdir}dc-mapper-train.perl "$train.corpus.xml" "$@" -o "$train.$infix.map.bin" || exit 1

echo ""
echo "##=================================================================="
echo "## $0: RAW: APPLY"
#runcmd ${progdir}dc-mapper-apply.perl "$train.$infix.map.bin" "$test.corpus.xml" -o "$test.$infix.out.xml" || exit 2

echo ""
echo "##=================================================================="
echo "## $0: RAW: EVAL"
#runcmd ${progdir}dc-mapper-eval.perl "$test.corpus.xml" "$test.$infix.out.xml" -o "$test.$infix.eval.xml" || exit 3

echo ""
echo "##=================================================================="
echo "## $0: CUTOFF: TRAIN"
runcmd ${progdir}dc-cutoff-train.perl "$test.$infix.eval.xml" "$@" -o "$test.$infix.cut.map.bin" || exit 4

echo ""
echo "##=================================================================="
echo "## $0: CUTOFF: APPLY"
runcmd ${progdir}dc-mapper-apply.perl "$test.$infix.cut.map.bin" "$test.$infix.out.xml" -o "$test.$infix.cut.out.xml" || exit 5

echo ""
echo "##=================================================================="
echo "## $0: CUTOFF: EVAL"
runcmd ${progdir}dc-mapper-eval.perl "$test.corpus.xml" "$test.$infix.cut.out.xml" -o "$test.$infix.cut.eval.xml" || exit 6

exit 0
