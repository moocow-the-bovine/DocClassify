#!/bin/sh

#base=vzdata-safe
base=vzdata-all
##--
#base=vzdata-safe.head
#base=vzdata-all.head1p

##-- test config(s)
mdfs="1"
##--
mfs="1"
#mfs="1 2 4"
#mfs="1 2 4 8 16 24"
#mfs="8 16 24"
#mfs="8"
##--
#dists="u"
dists="c"
##--

for dist in $dists; do
 for mdf in $mdfs; do
  for mf in $mfs; do
   echo "$0[$$]: base=$base; mf=$mf; mdf=$mdf; dist=$dist"
   ./dc-mapper-xcheck.perl -n=10 -seed=0 -r=128 -mf=$mf -mdf=$mdf -od ${base}.n-10.seed-0.r-128.mf-${mf}.mdf-${mdf}.dist-${dist}.xcheck.d ${base}.corpus.xml
  done
 done
done
echo "$0[$$]: done.\n";
