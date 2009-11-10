#!/bin/sh

#base=vzdata-safe.u1

base=anno-big-2;
#n=10;
n=3
tw=Hmax;
r=768;
xn=0;
mdf=0;
mf=0;
mtpd=0;
cp=avg;
lzc=vzsep;

./dc-mapper-xcheck.perl -seed=0 -v=1 \
  -cp=$cp -xn=$xn -n=$n -tw=$tw -mtpd=$mtpd -mdf=$mdf -r=$r -lzc=$lzc \
  $base.corpus.xml -od $base.n-$n.r-$r.mdf-$mdf.mf-$mf.xn-$xn.tw-$tw.mtpd-$mtpd.cp-$cp.lzc-$lzc.xcheck.d
