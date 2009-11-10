#!/bin/sh

#bases=vzdata-safe
#bases=vzdata-all
##--
#bases=vzdata-safe.head
#bases=vzdata-all.head1p
##--
#bases="vzdata-safe.u1 vzdata-safe.ux vzdata-safe vzdata-all"
#bases="vzdata-all vzdata-safe vzdata-safe.ux vzdata-safe.u1"
bases="anno-big-size-3 anno-big-size-5 anno-big-size-7 anno-big-size-10"

##-- test config(s)
mdfs="0"
##--
mfs="0"
#mfs="2"
#mfs="1 2 4"
#mfs="1 2 4 8 16 24"
#mfs="8 16 24"
#mfs="8"
##--
dists="u"
#dists="c"
##--
xns="0"
#xns="10"
##--
rs="768"
##--
ns="3"
##--
tws="Hmax"
##--
mtpds="0"
##--
lzcs="vzsep"

for base in $bases; do
 for dist in $dists; do
  for mdf in $mdfs; do
   for mf in $mfs; do
    for xn in $xns; do
     for r in $rs; do
      for n in $ns; do
       for tw in $tws; do
        for mtpd in $mtpds; do
         for lzc in $lzcs; do

label="${base}.n-$n.r-$r.mf-$mf.mdf-$mdf.mtpd-$mtpd.tw-$tw.lzc-$lzc.dist-$dist"
echo "$0[$$]: BEGIN ($label): `date -R`"
./dc-mapper-xcheck.perl -seed=0 -v=2 \
  -n=$n -xn=$xn -r=$r -mf=$mf -mdf=$mdf -mtpd=$mtpd -tw=$tw -dist=$dist -lzc=$lzc \
  -od "${label}.xcheck.d" "${base}.corpus.xml"
echo "$0[$$]: END ($label): `date -R`"

	 done
	done
       done
      done
     done
    done
   done
  done
 done
done
echo "$0[$$]: all done: `date -R`";
