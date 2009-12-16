#!/bin/sh

#base=vzdata-safe.u1
#base=anno-big-2;
base=data/groups_all_v3.2

n=3
seed=0
mc=LSI
mc_wcat=1
lzc=VzSem;
lz_semw=10
r=768;
tw=Hmax;

./dc-mapper-xcheck.perl -v=1 -seed=$seed -n=$n \
  -mc=$mc -mo weightByCat="$mc_wcat" \
  -lzc=$lzc -lzo semLemmaWeight="$lz_semw" \
  -r=$r -tw=$tw \
  $base.corpus.xml \
  -od $base.n-$n.seed-$seed.mc-$mc.wcat-$mc_wcat.lzc-$lzc.semw-$lz_semw.r-$r.tw-$tw.xcheck.d \
  "$@"
