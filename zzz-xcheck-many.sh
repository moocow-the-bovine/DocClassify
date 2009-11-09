#!/bin/sh

#bases=vzdata-safe
#bases=vzdata-all
##--
#bases=vzdata-safe.head
#bases=vzdata-all.head1p
##--
#bases="vzdata-safe.u1 vzdata-safe.ux vzdata-safe vzdata-all"
bases="vzdata-all vzdata-safe vzdata-safe.ux vzdata-safe.u1"

##-- test config(s)
mdfs="1"
##--
mfs="1"
#mfs="2"
#mfs="1 2 4"
#mfs="1 2 4 8 16 24"
#mfs="8 16 24"
#mfs="8"
##--
dists="u"
#dists="c"
##--
#xns="3"
xns="10"

for base in $bases; do
 for dist in $dists; do
  for mdf in $mdfs; do
   for mf in $mfs; do
    for xn in $xns; do
     label="base=$base; mf=$mf; mdf=$mdf; dist=$dist; xn=$xn"
     echo "$0[$$]: BEGIN ($label): `date -R`"
     ./dc-mapper-xcheck.perl -n=10 -xn=$xn -seed=0 -r=128 -mf=$mf -mdf=$mdf -od ${base}.n-10.xn-${xn}.seed-0.r-128.mf-${mf}.mdf-${mdf}.dist-${dist}.xcheck.d ${base}.corpus.xml
     echo "$0[$$]: END ($label): `date -R`"
    done
   done
  done
 done
done
echo "$0[$$]: all done: `date -R`";
