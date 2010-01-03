#!/bin/sh

dbase=2009_12_30_v2a
test=data/test_data_$dbase
train=data/train_data_$dbase

#d_vals="e b u"
d_vals="u"
#r_vals="128 256 512 768"
r_vals="256"
null_vals="0 1"


for rv in $r_vals; do
 for nv in $null_vals; do
  for dv in $d_vals; do
 
if test $nv = "1" ; then
  nullOpt=nullCat
else
  nullOpt=nullCatNope
fi
./train+apply+eval.sh $train $test "r${rv}-null${nv}-d${dv}" -r=$rv -mo ${nullOpt}=1_Sonstiges -mo dist=$dv

  done 
 done
done
