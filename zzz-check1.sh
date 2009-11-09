#!/bin/sh

#base=vzdata-safe.u1
base=vzdata-all
tw=Hmax
r=512
xn=0
mdf=2
tpd=100

./dc-mapper-xcheck.perl -seed=0 -v=1 -xn=0 -n=3 -tw=$tw -tpd=$tpd -mdf=$mdf -r=$r \
  $base.corpus.xml -od $base.r-$r.tw-$tw.xn-$xn.tpd-$tpd.mdf-$mdf.lc-1.xcheck.d
  