##-- EXPLORE:
## + figure out best strategy for implementing tdm-based DiaCollo
##   (terms@date) slicing operation
## + idea:
##    - we have an (m x n) sparse term x doc matrix tdm with l<<(n*m) nnz values
##      ~ assuming uniform (avg) density, we have:
##        * nm = l/m nnz values per term
##        * nn = 1/n nnz values per doc
##    - we get a set of a<m target terms (probably small, a < 100)
##    - we get a set of b<n target docs (probably large-ish, b > 1000)
##    - we want to extract the nnz (a,b) cells from tdm for svdApply
##    - we *may* have:
##      ~ ptr0 for constant access to tdm[a,*] and binsearch(log nm) for tdm[a,b]
##      ~ ptr1 for constant access to tdm[*,b] and binsearch(log nn) for tdm[a,b]

##-- config 1: dta-dc-xpages-map.64.d/tdm.ccs.*
m0=165673;   ##-- m: nterms
n0=540332;   ##-- n: ndocs
l0=44066100; ##-- l: nnz

##-- set config
m=m0; n=n0; l=l0;

nnz(m,l) = (1.0*l)/(1.0*m); ##-- nnz(m,l) : avg nnz/item
max2(a,b) = a > b ? a : b;
min2(a,b) = a < b ? a : b;
zeroes(x,y) = 0;

##-- plot: general
set xlabel "a";
set ylabel "b";
set zlabel "O";
set xrange [1:100];
set yrange [1:1000];

##-- plot: indexnd (a x b)
o_indexnd(a,b) = a*b*log(l);
splot o_indexnd(x,y);

##-- plot: ptr0, ptr1 (ptr1 wins, since (n < m) --> (nnz(n,l) < nnz(m,l))
o_ptr0(a,b) = a*b*log(nnz(m,l));
o_ptr1(a,b) = b*a*log(nnz(n,l));
splot o_ptr0(x,y), o_ptr1(x,y);

##-- plot: indexnd vs ptr1 : ptr1 grows *much* slower
splot o_indexnd(x,y), o_ptr1(x,y);
set view map; set contour; set pm3d; splot o_indexnd(x,y) - o_ptr1(x,y) w pm3d;

##-- plot: nz-intersect (ptr1&ptr2)
o_intersect(a,b) = a*nnz(m,l) + b*nnz(n,l) + max2(nnz(m,l),nnz(n,l))
splot o_intersect(x,y);

##-- plot: indexnd vs nz-intersect vs ptr1

##-- indexnd vs intersect: indexnd better for large b or a (b>~50, a<~6), (a>~50, b<~17), else intersect wins
set view map; set contour; set pm3d; splot o_indexnd(x,y) - o_intersect(x,y);

##-- ptr1 vs intersect: ptr1 better for min(a,b)<100 (a<~70 or b<~80)
set view map; set contour; set pm3d; splot o_ptr1(x,y) - o_intersect(x,y);

## UPSHOT:
## + safest overall behavior seems to be intersect
## + -intersect: requires most cached data (ptr0 + ptr1 = O(2*L))
## + +intersect:
##    ~ ptr0, ptr1 methods each have good growth behavior, but need extra implementation
##    ~ intersect can be done by leveraging existing code (e.g. CCS::Nd::indexNDi(), PDL::intersection())
## + if we're considering extra implementation and need/want minimal data footprint,
##   we might think about an optimized indexND strategy:
##   - give 2 sorted vectors a(nA), b(nB) and ccs (ix(),vals())
##   - perform a batch-wise indexND by:
##     ~ set lo=0, hi=nnz;
##     ~ for each a in a():
##         set a_lb=lb((a,*),ix[lo:hi])     ##-- remember last hi with hi>a as a_hi
##         set a_ub=ub((a,*),ix[a_lb:a_hi])
##         set b_lb=a_lb;
##         foreach b in b():
##           set b_lb=lb((a,b),ix[b_lb:a_ub])
##           if (ix[b_lb]==b) { append out[a,b]=vals[b_lb]; b_lb++; }
##         end foreach b
##         set lo=a_ub;                     ##-- start next a iter after current a
##       end foreach a
##   - this gets us exactly the values we want with no extra data footprint
##   - leverages sorted param vectors a(),b()
##   - a binsearch ~ O(2*log(l))
##     + but if "remember last a_hi" works,
##       ~ the first one will be s.t. like O(log(l) + log(l/m)) = O(2*log(l) - log(m))
##       ~ each additional a[i] binsearch will need to consider O(nA/m) fewer term entries
##         * that means O(nA/m * m/l) = O(nA/l) fewer nnz cells
##         * so O(binsearch(a[i])) ~ O(log(l - i*nA/l) + log((l - i*nA/l)/m))
##                                 = O(2*log(l-i*nA/l) - log(m))
##     + each binsearch(b[li]) is O(log(l/m))
##     + so we get sum_{i=0}^{nA-1} O(a*(2*log(l-i*nA/l) - log(m) + b*log(l/m)))
##   - big problem here is predicting how many nnz values we'll need to allocate!

## o_smartindex
o_smartindex(a,b) = o_smartindex_1(a,b,a-1);
o_smartindex_1(a,b,i) = i < 0 ? 0 : 2*log(l-i*a/l) - log(m) + b*log(l/m) + o_smartindex_1(a,b,i-1);

##-- smartindex much better than indexnd, as expected
unset view; unset contour; unset pm3d; splot o_indexnd(x,y), o_smartindex(x,y);
set view map; set contour; set pm3d; splot o_indexnd(x,y) - o_smartindex(x,y);

##-- o_smartindex looks pretty much identical with o_ptr0
unset view; unset contour; unset pm3d; splot o_ptr0(x,y), o_smartindex(x,y);
unset view; unset contour; unset pm3d; splot o_ptr1(x,y), o_smartindex(x,y);
#
set view map; set contour; set pm3d; splot o_ptr0(x,y) - o_smartindex(x,y);
