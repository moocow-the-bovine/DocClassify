## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::LSI::KNN.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: document-to-class mapper: "latent semantic indexing" / SVD, KNN variant

package DocClassify::Mapper::LSI::KNN;
use DocClassify::Mapper::LSI;

use PDL;
use PDL::CCS::Nd;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Mapper::LSI);


##==============================================================================
## Constructors etc.

## $map = $CLASS_OR_OBJ->new(%opts)
## %$map, %opts:
##  ##
##  ##==== NEW in Mapper::LSI::KNN
##  #catProfile => $how,              ##-- cat profiling method [ignored]
##  #xcm => $xcm_pdl,                 ##-- not generated
##  knn => $k,                        ##-- number of nearest neighbors to consider (default=10)
##  ##
##  ##==== INHERITED from Mapper::LSI
##  ##-- options
##  svdr => $svdr,                   ##-- number of reduced dimensions (default=256)
##  dist => $distSpec,               ##-- distance spec for MUDL::Cluster::Distance (default='u')
##                                   ##   + 'c'=Pearson, 'u'=Cosine, 'e'=Euclid, ...
##  catProfile => $how,              ##-- cat profiling method ('fold-in','average', 'weighted-average'...): default='average'
##  xn => $xn,                       ##-- number of splits for compile-time cross-check (0 for none; default=3)
##  seed => $seed,                   ##-- random seed for corpus splitting (undef (default) for none)
##  conf_nofp => $conf,              ##-- confidence level for negative-evidence parameter fitting (.95)
##  conf_nofn => $conf,              ##-- confidence level for positive-evidence parameter fitting (.95)
##  ##
##  ##-- data: post-compile()
##  disto => $distObj,               ##-- MUDL::Cluster::Distance object
##  svd => $svd,                     ##-- a MUDL::SVD object
##  xdm => $xdm_pdl,                 ##-- dense PDL ($svdr,$ND) = $svd->apply( $tdm_pdl )
##  xcm => $xcm_pdl,                 ##-- dense PDL ($svdr,$NC) = $svd->apply( $TERM_CAT_MATRIX($NT,$NC) )
##  c1dist_mu => $c1dist_mu,         ##-- dense PDL ($NC) : [$ci] ->    avg { dist($ci,$di) : $di  \in $ci }
##  c1dist_sd => $c1dist_sd,         ##-- dense PDL ($NC) : [$ci] -> stddev { dist($ci,$di) : $di  \in $ci }
##  c0dist_mu => $c0dist_mu,         ##-- dense PDL ($NC) : [$ci] ->    avg { dist($ci,$di) : $di !\in $ci }
##  c0dist_sd => $c0dist_sd,         ##-- dense PDL ($NC) : [$ci] -> stddev { dist($ci,$di) : $di !\in $ci }
##  ##
##  ##==== INHERITED from Mapper::ByLemma
##  ##-- options
##  verbose => $vlevel,              ##-- verbosity level (default=$verbose)
##  lemmatize => \%opts,             ##-- options for $doc->typeSignature->lemmatize()
##  trainExclusive => $bool,         ##-- use each doc to train at most 1 cat? (default=true)
##  minFreq => $f,                   ##-- minimum global frequency f(t) for term-inclusion (default=0)
##  minDocFreq => $ndocs,            ##-- minimum number of docs with f(t,d)>0 for term-inclusion (default=0)
##  smoothf => $f0,                  ##-- global frequency smoothing constant (undef~(NTypes/NTokens); default=1)
##  termWeight => $how,              ##-- term "weighting" method ('uniform', 'entropy'): default='entropy'
##  ##
##  ##-- data: enums
##  lcenum => $globalCatEnum,        ##-- local cat enum, compcat ($NCg=$globalCatEnum->size())
##  gcenum => $localCatEnum,         ##-- global cat enum         ($NC=$catEnum->size())
##  tenum => $termEnum,              ##-- term (lemma) enum       ($NT=$termEnum->size())
##  denum => $docEnum,               ##-- document (label) enum   ($ND=$docEnum->size()=scalar(@docs))
##  ##
##  ##-- data: training
##  gf => \%global_tf,               ##-- global term-frequency hash: ($term=>$freq, ...)
##  df => \%global_df,               ##-- global term-docfrequency hash: ($term=>$ndocs, ...)
##  docs   => \@docs,                ##-- training docs, indexed by local $docid ($ND=$docEnum->size()=scalar(@docs))
##  sigs   => \@sigs,                ##-- training sigs, indexed by local $docid
##  ##
##  ##-- data: post-compile()
##  dcm => $dcm_pdl,                 ##-- doc-cat matrix:  PDL::CCS::Nd ($ND,$NC): [$di,$ci] -> deg($di \in $ci)||0
##  tdm0=> $tdm0_pdl,                ##-- raw term-doc mx: PDL::CCS::Nd ($NT,$ND): [$ti,$di] ->     f($ti,$di)
##  tcm0=> $tcm0_pdl,                ##-- raw term-cat mx: PDL::CCS::Nd ($NT,$NC): [$ti,$ci] ->     f($ti,$ci)
##  tw  => $tw_pdl,                  ##-- term-weight pdl: dense:       ($NT)    : [$ti]     -> w($ti)
##  tdm => $tdm_pdl,                 ##-- term-doc matrix: PDL::CCS::Nd ($NT,$ND): [$ti,$di] -> log(f($ti,$di)+$f0)*w($ti)
##  tcm => $tcm_pdl,                 ##-- term-cat matrix: PDL::CCS::Nd ($NT,$NC): [$ti,$ci] -> log(f($ti,$ci)+$f0)*w($ti)
sub new {
  my $that = shift;
  my $obj =  $that->SUPER::new(
			       ##-- options
			       knn => 10,

			       ##-- user args
			       @_,
			      );
  return $obj;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override appends qw()
sub noShadowKeys {
  return ($_[0]->SUPER::noShadowKeys(@_[1..$#_]),
	 );
}

##==============================================================================
## Methods: API: Training & Compilation
##  + see DocClassify::Mapper::KNNTrain

##==============================================================================
## Methods: API: Classification

## $corpus = $map->mapCorpus($corpus)
##  + Attempt to classify each $doc in $corpus,
##    destructively altering $doc->{cats} to reflect classification results.
##  + Inherited default implementation just calls $map->mapDocument() on each $doc in $corpus.

## $doc = $map->mapDocument($doc)
##  + attempt to classify $doc
##  + destructively alters $doc->{cats} to reflect classification results
sub mapDocument {
  my ($map,$doc) = @_;

  ##-- be verbose
  $map->vlog('trace', "mapDocument(".$doc->label.")") if ($map->{verbose}>=3);

  ##-- sanity check(s)
  $map->logconfess("mapDocument(): no feature-document matrix 'xdm'!") if (!defined($map->{xdm}));

  ##-- get doc pdl
  my $D_tdm = $map->docPdlRaw($doc);
  my $D_N   = $D_tdm->sum;
  $map->logwarn("mapDocument(): null vector for document '$doc->{label}'") if ($map->{verbose} && $D_N==0);

  ##-- compute distance to each centroid
  my $D_xdm = $map->svdApply($D_tdm);
  my $M_xdm = $map->{xdm};
  my $dD_dist = $map->{disto}->clusterDistanceMatrix(data=>$D_xdm,cdata=>$M_xdm)->lclip(0);

  ##-- convert distance to similarity
  my ($d_sim);

  ##-- just invert distance matrix
  #$d_sim = $d_dist->max-$dD_dist;
  #$d_sim  = 2-$dD_dist;
  $d_sim = $dD_dist**-1;
  $d_sim = $d_sim->flat;
  $d_sim->inplace->clip(0,1e38);

  ##-- get k-nearest neigbors
  my $nn_di = zeroes(long,$map->{knn});
  $d_sim->maximum_n_ind($nn_di);

  ##-- compute cat-membership probabilities by boltzmann-weighting
  my $nn_dp  = $d_sim->index($nn_di) / $d_sim->index($nn_di)->sumover;
  my $dcm    = $map->{dcm};
  my $Dcm    = ($dcm->dice_axis(0,$nn_di)**-1 * $nn_dp)->_missing(0)->sumover->decode;
  my $Dcm_w  = which($Dcm);
  my $Dcm_wx = $Dcm->index($Dcm_w);

  ##-- dump similarities to $doc->{cats}
  my ($cname,$cid);
  @{$doc->{cats}} = map {
    $cid   = $Dcm_w->at($_);
    $cname = $map->{lcenum}{id2sym}[$cid];
    {id=>$map->{gcenum}{sym2id}{$cname}, name=>$cname, sim=>$Dcm_wx->at($_)}
  } $Dcm_wx->qsorti->slice("-1:0")->list;
  $doc->{cats}[$_]{deg} = $_+1 foreach (0..$#{$doc->{cats}});

  return $doc;
}

##==============================================================================
## Methods: Misc

## $dxpdl = $map->svdApply($fpdl)
##  + input $fpdl is dense pdl($NT,1):     [$tid,0]     => f($tid,$doc)
##  + output $dxpdl is dense pdl(1,$svdr): [$svd_dim,0] => $svd_val
##  + INHERITED from Mapper::LSI

## $sig = $map->lemmaSignature($doc_or_sig)
##  + wrapper for $doc->termSignature->lemmatize() with options %{$map->{lemmatize}}
##  + inherited from Mapper::ByLemma

## $fpdl = $map->docPdlRaw($doc, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tidl,1]=>f($tid,$doc)
##  + inherited from Mapper::ByLemma

## $fpdl = $map->sigPdlRaw($sig, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tid,1]=>f($tid,$sig)
##  + inherited from Mapper::ByLemma

##==============================================================================
## Methods: API: I/O
##  + see DocClassify::Object


##==============================================================================
## Footer
1;

__END__
