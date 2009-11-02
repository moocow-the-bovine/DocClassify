#!/bin/sh

#base=vzdata-safe
#base=vzdata-all
##--
base=vzdata-safe.head

for mdf in 1 2; do
 for mf in 1 2 4; do
  ./dc-mapper-xcheck.perl -n=10 -seed=0 -r=128 -mf=$mf -mdf=$mdf -od ${base}.n-10.seed-0.r-128.mf-${mf}.mdf-${mdf}.xcheck.d ${base}.corpus.xml
 done
done

