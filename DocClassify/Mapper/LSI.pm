## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::LSI.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document-to-class mapper: "latent semantic indexing" / SVD

package DocClassify::Mapper::LSI;
use DocClassify::Mapper::ByLemma;
use DocClassify::Utils ':all';

use MUDL::Enum;
use MUDL::SVD;
use MUDL::Cluster::Distance;
use MUDL::Cluster::Distance::Builtin;
use MUDL::PDL::Stats;
use MUDL::PDL::Smooth;

use PDL;
use PDL::CCS::Nd;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Mapper::ByLemma);


#our $verbose = 2;
our $verbose = 3;

##==============================================================================
## Constructors etc.

## $map = $CLASS_OR_OBJ->new(%opts)
## %$map, %opts:
##  ##
##  ##==== NEW in Mapper::LSI
##  ##-- options
##  svdr => $svdr,                   ##-- number of reduced dimensions (default=256)
##  catProfile => $how,              ##-- cate profiling method ('fold-in','average', 'weighted-average'...): default='average'
##  xn => $xn,                       ##-- number of splits for compile-time cross-check (0 for none; default=3)
##  seed => $seed,                   ##-- random seed for corpus splitting (undef (default) for none)
##  conf_nofp => $conf,              ##-- confidence level for negative-evidence parameter fitting (.95)
##  conf_nofn => $conf,              ##-- confidence level for positive-evidence parameter fitting (.95)
##  ##
##  ##-- data: post-compile()
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
##  cleanDocs => $bool,              ##-- whether to implicitly clean $doc->{sig} on train, map [default=true]
##  byCat => $bool,                  ##-- compile() tcm instead of tdm0, tdm? (default=0)
##  weightByCat => $bool,            ##-- compile() tw using tcm0 insteadm of tdm0? (default=0)
##  dist => $distSpec,               ##-- distance spec for MUDL::Cluster::Distance (default='u')
##                                   ##   + 'c'=Pearson, 'u'=Cosine, 'e'=Euclid, ...
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
##  disto => $distObj,               ##-- MUDL::Cluster::Distance object
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
			       svdr => 256,
			       catProfile => 'average',
			       termWeight  => 'entropy',
			       xn => 3,
			       seed => undef,
			       conf_nofp=>.95,
			       conf_nofn=>.95,

			       ##-- data: post-compile
			       svd=>undef,
			       xdm=>undef,
			       xcm=>undef,
			       c1dist_mu=>undef,
			       c1dist_sd=>undef,
			       c0dist_mu=>undef,
			       c0dist_sd=>undef,

			       ##-- user args
			       @_,
			      );
  return $obj;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override appends qw(svd xdm xcm c1dist_mu c1dist_sd c0dist_mu c0dist_sd)
sub noShadowKeys {
  return ($_[0]->SUPER::noShadowKeys(@_[1..$#_]),
	  qw(svd xdm xcm),
	  qw(c1dist_mu c1dist_sd c0dist_mu c0dist_sd));
}

##==============================================================================
## Methods: API: Training

## $map = $map->trainCorpus($corpus)
##  + add training data from $corpus
##  + inherited default just calls $map->trainDocument($doc) foreach doc in corpus

## $map = $map->trainDocument($doc)
##  + inherited from Mapper::ByLemma

## $catId = $map->addCat($catName)
## $catId = $map->addCat($catName,$wantId)
##  + inherited from Mapper::ByLemma

##==============================================================================
## Methods: API: Compilation

## $map = $map->compile(%opts)
##  + compile underlying map data
##  + should be called only after all training data have been added
sub compile {
  my ($map,%opts) = @_;

  ##-- inherited compilation
  $map->SUPER::compile(%opts)
    or $map->logconfess("compile() inherited compilation failed: $!");

  ##-- clear expensive perl signature structs
  @{$map->{sigs}} = qw();
  #@{$map->{docs}} = qw();

  ##-- training-internal cross-check
  $map->compileCrossCheck();
  $map->compileFit();

  ##-- local compilation (final)
  $map->compileLocal(%opts, label=>'FINAL', svdShrink=>1, svdCache=>1);
  $map->{svdr} = $map->{svd}{r};

  return $map;
}


## $bool = $map->compiled()
##  + returns true iff $map has been compiled
##  + override checks for $map->{xcm}
sub compiled { return defined($_[0]{xcm}); }

## $map = $map->clearTrainingCache()
##  + clears any cached data from training
##  + after calling this, $map may no longer be able to train
##  + calls inherited Mapper::ByLemma::clearTrainingCache()
##  + additionally clears @$map{qw(tdm0 tcm0)}
sub clearTrainingCache {
  my $map = shift;
  $map->SUPER::clearTrainingCache();
  delete($map->{tdm0});
  delete($map->{tcm0});
  #delete($map->{tdm});    ##-- still useful for debugging
  #delete($map->{tcm});    ##-- still useful for debugging
  delete($map->{dc_dist}); ##-- good for debugging, but huge!
  return $map;
}

##--------------------------------------------------------------
## Methods: Compilation: Utils
##  + see also Mapper::ByLemma

## $map = $map->compileFit()
##  + compiles fitting paramters from $map->{dc_dist}
sub compileFit {
  my $map = shift;

  if (!defined($map->{dc_dist})) {
    $map->vlog('warn', "compileFit(): no {dc_dist} matrix defined; NOT computing fit paramters");
    return $map;
  }

  ##-- vars
  my $lcenum = $map->{lcenum};
  my $NC     = $map->{lcenum}->size;
  #my $gcenum = $map->{gcenum};
  #my $gcids  = pdl(long, [@{$gcenum->{sym2id}}{@{$lcenum->{id2sym}}}]);
  #my $NCg    = $gcids->max+1;
  #my $ND     = $map->{denum}->size;
  my $ND     = $map->{dc_dist}->dim(0); ##-- allow external cross-check loaded from eval data
  my $dcm    = $map->{dcm};
  my $dcm_z  = $dcm->missing->sclr;
  my $d2c    = $dcm->xchg(0,1)->_missing('inf')->minimum_ind->decode;  ##-- [$di] -> $ci_best
  $dcm->missing($dcm_z);
  my $dc_dist = $map->{dc_dist};

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##-- get positive, negative samples
  my $dc_which1 = sequence($ND)->cat($d2c)->xchg(0,1);  ##-- [$di]     -> [$di,$ci] : $di \in $ci
  my $dc_mask   = zeroes(byte,$dc_dist->dims);          ##-- [$di,$ci] -> 1($di \in     $ci)
  $dc_mask->indexND($dc_which1) .= 1;
  my $dc_which0 = whichND(!$dc_mask);                   ##-- [$di,$ci] -> 1($di \not\in $ci)
  my $nc  = $dc_mask->long->sumover->double;             ##-- [$ci] -> |{ $di : $di \in     $ci }|
  my $nnc = $nc->sum - $nc;                              ##-- [$ci] -> |{ $di : $di \not\in $ci }|

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## + from testme.perl test_compile_xcheck() (from test_errors())
  ## $dcdist_mu = pdl(1): global avg    dist($ci,$di)
  ## $dcdist_sd = pdl(1): global stddev dist($ci,$di)
  my $dcdist_mu = $dc_dist->flat->average;
  my $dcdist_sd = (($dc_dist - $dcdist_mu)**2)->flat->average->sqrt;
  my $nc_min = 10;#50; #3; #10; ##-- minimum #/cats to use fit

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## $cdist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \in $ci
  ## $cdist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \in $ci
  my ($cdist_mu,$cdist_sd,$cdist_isgood,$cdist_sd_raw);
  $cdist_mu = $dc_dist->average;
  $cdist_sd = (($dc_dist - $cdist_mu->slice("*1,"))**2)->average->sqrt;
  $cdist_isgood = ($cdist_sd->isfinite)&($cdist_sd>0);
  $cdist_sd_raw = $cdist_sd->pdl;
  ##
  $cdist_sd->where(!$cdist_isgood) .= $cdist_sd->where($cdist_isgood)->minimum/2;

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## $dc1dist: CCS: [$di,$ci] -> dist($ci,$di) : $di \in $ci
  ## $c1dist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \in $ci
  ## $c1dist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \in $ci
  ## $c1dist_mu0, $c1dist_sd0: global fit parameters
  my $dc1dist = PDL::CCS::Nd->newFromWhich($dc_which1,$dc_dist->indexND($dc_which1));
  my $c1dist_mu0 = $dc1dist->_nzvals->average;
  my $c1dist_sd0 = (($dc1dist->_nzvals-$c1dist_mu0)**2)->average->sqrt;
  my $c1dist_mu = $dc1dist->average_nz->decode;
  my $c1dist_sd = (($dc1dist - $c1dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  my $c1dist_mu_raw = $c1dist_mu->pdl;
  my $c1dist_sd_raw = $c1dist_sd->pdl;
  my $c1dist_isgood = (($c1dist_sd_raw->isfinite) & ($nc >= $nc_min));
  #$c1dist_isgood = (($c1dist_sd_raw->isfinite));
  #my ($c1dist_sd_nan0,$c1dist_sd_bad0);
  #($c1dist_sd_nan0 = $c1dist_sd->pdl)->where(!$c1dist_sd->isfinite) .= 0;
  #($c1dist_sd_bad0 = $c1dist_sd->pdl)->where(!$c1dist_isgood) .= 0;

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## $dc0dist: CCS: [$di,$ci] -> dist($ci,$di) : $di \not\in $ci
  ## $c0dist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \not\in $ci
  ## $c0dist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \not\in $ci
  ## $c0dist_mu0, $c0dist_sd0: global fit parameters
  my $dc0dist = PDL::CCS::Nd->newFromWhich($dc_which0,$dc_dist->indexND($dc_which0));
  my $c0dist_mu0 = $dc0dist->_nzvals->average;
  my $c0dist_sd0 = (($dc0dist->_nzvals-$c0dist_mu0)**2)->average->sqrt;
  my $c0dist_mu = $dc0dist->average_nz->decode;
  my $c0dist_sd = (($dc0dist - $c0dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  my $c0dist_mu_raw = $c0dist_mu->pdl;
  my $c0dist_sd_raw = $c0dist_sd->pdl;
  #my $c0dist_isgood = (($c0dist_sd->isfinite) & ($nc >= $nc_min));
  my $c0dist_isgood = ($c0dist_sd_raw->isfinite);
  #my ($c0dist_sd_nan0,$c0dist_sd_bad0);
  #($c0dist_sd_nan0 = $c0dist_sd->pdl)->where(!$c0dist_sd->isfinite) .= 0;
  #($c0dist_sd_bad0 = $c0dist_sd->pdl)->where(!$c0dist_isgood) .= 0;

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##-- hack mu, sd (positives)
  #$c1dist_mu->where(!$c1dist_sd->isfinite) .= 0;
  $c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu->where($c1dist_isgood)->minimum;
  #$c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu->where($c1dist_isgood)->minimum/2;
  #$c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu0;
  ##
  #$c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd0;
  #$c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd->where($c1dist_sd->isfinite)->minimum/2;
  $c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd->where($c1dist_isgood)->minimum/2;

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##-- hack mu, sd (negatives)
  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu->where($c0dist_isgood)->minimum/2;
  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu->where($c0dist_isgood)->maximum*2;
  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu0;
  ##
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->minimum;
  $c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->maximum;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->maximum*2;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->average;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd0;

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##-- heuristically shovel around fit parameters ($d1)
  my ($conf_nofp,$conf_nofn) = map {$_||.95} @$map{qw(conf_nofp conf_nofn)};
  my $cutoff_w1 = ($nc/$nc->max);
  ##
  my $cutoff_nofp = $c0dist_mu - scalar(gausswidth($conf_nofp, $c0dist_mu,$c0dist_sd));
  my $cutoff_nofn = $c1dist_mu + scalar(gausswidth($conf_nofn, $c1dist_mu,$c1dist_sd));
  my $cutoff_avg  = ($cutoff_nofp + $cutoff_nofn)/2;
  my $cutoff_wavg = (1-$cutoff_w1)*$cutoff_nofp + $cutoff_w1*$cutoff_nofn;
  #my $cutoff_wavg = $nnc/$ND*$cutoff_nofp + $nc/$ND*$cutoff_nofn;
  my $cutoff_ok   = ($cutoff_nofn < $cutoff_wavg) & ($cutoff_nofp > $cutoff_wavg);
  my $c1dist_mu_save = $c1dist_mu->pdl;
  my $c1dist_mu_adj = $c1dist_mu->pdl;
  my $c0dist_mu_adj = $c0dist_mu->pdl;
  $c1dist_mu_adj->where(!$cutoff_ok) .= ($cutoff_wavg - scalar(gausswidth($conf_nofn, $c1dist_mu,$c1dist_sd)))->where(!$cutoff_ok);

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## store final fit parameters
  $map->{c1dist_mu} = $c1dist_mu_adj;
  $map->{c1dist_sd} = $c1dist_sd;
  $map->{c0dist_mu} = $c0dist_mu_adj;
  $map->{c0dist_sd} = $c0dist_sd;

  return $map;
}

## $map = $map->loadCrossCheckEval($eval)
##  + populates $map->{dc_dist} from a DocClassify::Eval object
##  + cross-check data should have been created on a subset of the corpus used to train this mapper!
sub loadCrossCheckEval {
  my ($map,$eval) = @_;
  $map->vlog('info',"loadCrossCheckEval(): ".($eval->{label}||'')) if ($map->{verbose});

  ##-- vars
  my $lcenum = $map->{lcenum};
  my $denum = $map->{denum};
  my $ND = $map->{denum}->size;
  my $NC = $lcenum->size;
  my $lc_sym2id = $lcenum->{sym2id};
  my $d_sym2id  = $denum->{sym2id};
  my $lab2docs  = $eval->{lab2docs};

  ##-- get dc_dist
  my $dc_dist = zeroes($ND,$NC)+2; ##-- initialize to a meaningful maximum
  my ($lab,$d12,$dcats2,@dci,@dcdist);
  my $di = 0;
  while (($lab,$d12)=each(%{$eval->{lab2docs}})) {
    if (!defined($di = $d_sym2id->{$lab})) {
      $map->logwarn("loadCrossCheckEval(): no internal ID for doc label '$lab' -- skipping");
      next;
    }
    $dcats2 = $d12->[1]{cats};
    @dci    = grep {defined($lc_sym2id->{$dcats2->[$_]{name}})} (0..$#$dcats2);
    @dcdist = map {$dcats2->[$_]{dist_raw}} @dci;
    $dc_dist->slice("($di),")->index(pdl(long,[@$lc_sym2id{map {$dcats2->[$_]{name}} @dci}])) .= pdl(\@dcdist);
  }

  ##-- cache dc_dist
  $map->{dc_dist} = $dc_dist;
  return $map;
}

## $map = $map->compileCrossCheck()
##  + does cross-checking for training data
##  + caches $map->{dc_dist}: dense pdl [$di,$ci] -> dist($di,$ci)
sub compileCrossCheck {
  my $map = shift;
  my $xcn = $map->{xn};

  if (!$xcn || $xcn<3) {
    $map->vlog('warn', "compileCrossCheck(): cross-validation disabled (xn=".($xcn||0).")");
    return $map;
  }

  ##-- cross-check: generate test corpora
  my $corpus = DocClassify::Corpus->new(docs=>$map->{docs});
  my @subcs  = $corpus->splitN($xcn, seed=>$map->{seed}, exclusive=>$map->{trainExclusive}, label=>"XCHECK.%d/$xcn");

  ##-- cross-check: map subcorpora to document index pdls
  $map->logconfess("compileCrossCheck(): can't handle non-exclusive training mode!") if (!$map->{trainExclusive});
  my $NC     = $map->{lcenum}->size;
  my $ND     = $map->{denum}->size;
  my $d2subc = zeroes(long,$ND);
  foreach (0..$#subcs) {
    $d2subc->index(pdl(long, [map {$_->{id}} @{$subcs[$_]{docs}}])) .= $_;
  }
  my $dcm_z = $map->{dcm}->missing->sclr;
  my $d2c   = $map->{dcm}->xchg(0,1)->_missing('inf')->minimum_ind->decode;  ##-- [$di] -> $ci_best
  $map->{dcm}->missing($dcm_z);

  ##-- cross-check: train & map
  my $dc_dist = zeroes(double,$ND,$NC);       ##-- [$di,$ci] -> dist($di,$ci)
  ##
  my ($xci,$xclabel, $map2, $docids_train,$docids_test);
  my ($od_did,$od_tdm,$od_xdm,$od_cdmat);
  foreach $xci (1..$xcn) {
    ##-- create & compile subset mapper
    $xclabel = "XCHECK ($xci/$xcn)";
    ($docids_test,$docids_train) = which_both($d2subc==($xci-1));
    if ($docids_train->isempty || $docids_test->isempty) {
      $map->logwarn("compileCrossCheck(): [$xclabel]: TRAIN: empty subcorpus: skipping!");
      next;
    }
    my ($ND_train,$ND_test) = ($docids_train->nelem,$docids_test->nelem);
    $map->vlog('info',"compileCrossCheck(): [$xclabel]: TRAIN: ND_train=$ND_train, ND_test=$ND_test") if ($map->{verbose});
    $map2 = $map->docSubset($docids_train);
    $map2->compileLocal(label=>$xclabel);

    ##-- map left-out ("other") documents
    #$map2->{verbose} = 0; 
    $map->vlog('info', "compileCrossCheck(): [$xclabel]: MAP: ND_test=$ND_test") if ($map->{verbose});
    foreach $od_did ($docids_test->list) {
      $map->vlog('trace', "compileCrossCheck(): [$xclabel]: MAP: DOC(".$map->{docs}[$od_did]{label}.")") if ($map->{verbose}>=2);
      $od_tdm = $map->{tdm}->dice_axis(1,$od_did);
      $od_xdm = $map2->svdApply($od_tdm);
      $od_cdmat = $map2->{disto}->clusterDistanceMatrix(data=>$od_xdm,cdata=>$map2->{xcm})->lclip(0);
      $dc_dist->slice("($od_did),") .= $od_cdmat->flat;
    }
  }
  $map->{dc_dist} = $dc_dist; ##-- save doc-cat distance matrix

  return $map;
}

## $map2 = $map->docSubset($docids)
##  + creates and returns a shallow copy of $map using only the subset $docids
##  + $docids : pdl($ND_local): [$ndli] -> $docid_global
sub docSubset {
  my ($map,$docids) = @_;
  my $map2 = $map->shadow(%$map, docids=>$docids);
  $map2->{dcm}  = $map2->{dcm}->dice_axis(0,$docids) if (defined($map2->{dcm}));
  $map2->{tdm0} = $map2->{tdm0}->dice_axis(1,$docids) if (defined($map2->{tdm0}));
  $map2->{tdm}  = $map2->{tdm}->dice_axis(1,$docids) if (defined($map2->{tdm}));
  delete(@$map2{qw(svd xdm xcm tcm0 tcm)});
  return $map2;
}

## $map = $map->compileLocal(%opts)
##  + compiles local map params qw(svd xdm xcm)
##  + %opts: passed to compile_(svd|xdm|xcm), e.g.
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##     svdShrink => $bool,   ##-- whether to auto-shrink svd (default=false)
##     svdCache  => $bool,   ##-- whether to auto-cache $svd->isigmaVt_() (default=true)
sub compileLocal {
  my ($map,%opts) = @_;

  ##-- svd: $map->{svd}
  $map->compile_svd(%opts);

  ##-- matrix: $map->{xdm}: (R=$map->{svdr} x ND) [R x Doc -> X]
  $map->compile_xdm(%opts);

  ##-- matrix: $map->{xcm}: (R=$map->{svdr} x NC=$NC) [R x Cat -> Sv]
  $map->compile_xcm(%opts);

  return $map;
}

## $map = $map->compile_svd(%opts)
##  + compiles $map->{svd} from $map->{tdm} (or $map->{tcm}, if $map->{byCat} is true)
##  + %opts:
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##     svdShrink => $bool,   ##-- whether to auto-shrink svd (default=false)
##     svdCache  => $bool,   ##-- whether to auto-cache $svd->isigmaVt_() (default=true)
sub compile_svd {
  my ($map,%opts) = @_;
  my $label = $map->labelString(%opts);

  $map->vlog('info',"compile_svd() [$label]: SVD (svdr=>$map->{svdr})") if ($map->{verbose});
  my $svd  = $map->{svd} = MUDL::SVD->new(r=>$map->{svdr}, maxiters=>0); #$maxiters=>(2*$map->{svdr})
  $svd->computeccs_nd($map->{byCat} ? $map->{tcm} : $map->{tdm});
  if ($opts{svdShrink}) {
    $svd->shrink();
    $map->vlog('info', "compile_svd() [$label]: SVD: auto-shrunk to r=$svd->{r}") if ($map->{verbose});
    $map->{svdr} = $svd->{r}; ##-- NOT HERE ?!
  }
  if ($opts{svdCache} || !defined($opts{svdCache})) {
    $map->vlog('info', "compile_svd() [$label]: SVD: cache: isigmaVt_()") if ($map->{verbose});
    $map->{svd}->isigmaVt(); ##-- cache $svd->{isigmaVt_} for $svd->apply0()
  }
  return $map;
}

## $map = $map->compile_xdm(%opts)
##  + compiles $map->{xdm}: (R=$map->{svdr} x ND) [R x Doc -> X]
##  + really just a wrapper for $map->{xdm}=$map->{svd}{u}
##  + %opts:
##     label  => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
sub compile_xdm {
  my ($map,%opts) = @_;
  my $label = $map->labelString(%opts);
  #my $ND    = $map->{denum}->size;
  my $ND    = $map->{svd}{u}->dim(1);
  $map->vlog('info', "compile_xdm() [$label]: matrix: xdm: (r=$map->{svdr} x ND=$ND) [R x Doc -> Sv]") if ($map->{verbose});
  $map->{xdm} = $map->{svd}{u};         ##-- we've already computed the bugger (duh!)
  return $map;
}

## $cpMethod = $map->catProfileMethod()
##  + gets $map->{catProfile}, does some sanity checks & canonicalization (caches)
sub catProfileMethod {
  my $map = shift;
  my $catProfile = $map->{catProfile};
  $catProfile = 'average' if (!defined($catProfile));
  if ($catProfile =~ /^fold/) { $catProfile = 'fold-in'; }
  elsif ($catProfile =~ /^a/) { $catProfile = 'average'; }
  elsif ($catProfile =~ /^w/) { $catProfile = 'weighted-average'; }
  else {
    $map->logconfess("catProfileMethod(): unknown category-profiling method option '$catProfile'");
    return undef;
  }
  return $map->{catProfile} = $catProfile;
}

## $map = $map->compile_xcm(%opts)
##  + compiles dense matrix $map->{xcm}: (r=$map->{svdr} x NC=$NC) [R x Cat -> Sv]
##  + calls $map->catProfileMethod()
##  + requires $map->{svd}
##  + may require $map->{tdm0}, $map->{tcm0}, $map->{xdm}
##  + %opts:
##     label  => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
sub compile_xcm {
  my ($map,%opts) = @_;
  my $label = $map->labelString(%opts);

  ##-- vars: common
  my $catProfile = $map->catProfileMethod();
  my $NC = $map->{lcenum}->size;
  #my $ND = $map->{denum}->size;
  my $ND = $map->{dcm}->dim(0);

  ##-- guts
  $map->vlog('info', "compile_xcm() [$label]: matrix: xcm: (r=$map->{svdr} x NC=$NC) [R x Cat -> Sv] : prof=$catProfile")
    if ($map->{verbose});
  if ($catProfile eq 'fold-in') {
    ##-- fold-in
    $map->compile_tcm0() if (!defined($map->{tcm0}));
    $map->compile_tcm()  if (!defined($map->{tcm}));
    $map->{xcm} = $map->{svd}->apply($map->{tcm}->decode);
  }
  elsif ($catProfile eq 'average' || $catProfile eq 'weighted-average') {
    ##-- TODO: use MUDL::Cluster::Method::d2c_*() methods here!
    my $lc_sym2id = $map->{lcenum}{sym2id};
    my $xdm = $map->{xdm};
    my $xcm = zeroes(double, $map->{svdr},$NC);
    my $doc_weight = $catProfile eq 'average' ? ones($ND) : $map->{tdm0}->sumover->decode;
    $doc_weight  /= $doc_weight->sumover;
    my $docids = $map->docIdPdl;
    my ($d_id_local,$d_id_global,$d_x,$c_id,$cat);
    foreach $d_id_local (0..($ND-1)) {
      $d_id_global = $docids->at($d_id_local);
      $d_x  = $doc_weight->index($d_id_local) * $xdm->slice(",$d_id_local"); ##-- [0,$di] -> $x
      foreach $cat (@{$map->{docs}[$d_id_global]{cats}}) {
	$c_id = $lc_sym2id->{$cat->{name}};
	$xcm->slice(",$c_id") += $d_x;
      }
    }
    $map->{xcm} = $xcm;
  }

  return $map;
}

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
  $map->logconfess("mapDocument(): no feature-category matrix 'xcm'!") if (!defined($map->{xcm}));

  ##-- get doc pdl
  my $tdm = $map->docPdlRaw($doc);
  my $tdN = $tdm->sum;
  $map->logwarn("mapDocument(): null vector for document '$doc->{label}'") if ($map->{verbose} && $tdN==0);

  ##-- compute distance to each centroid
  my $xdm   = $map->svdApply($tdm);
  my $cd_dist = $map->{disto}->clusterDistanceMatrix(data=>$xdm,cdata=>$map->{xcm})->lclip(0);

  ##-- convert distance to similarity
  my ($cd_sim);
  if (!defined($map->{c1dist_mu})) {
    ##-- just invert $cdmat
    #$cd_sim = $cd_dist->max-$cd_dist;
    #$cd_sim  = 2-$cd_dist;
    $cd_sim = $cd_dist**-1;
  } else {
    ##-- use fit parameters to estimate similarity
    my $cd_cdf1 = gausscdf($cd_dist, $map->{c1dist_mu}, $map->{c1dist_sd});
    my $cd_cdf0 = gausscdf($cd_dist, $map->{c0dist_mu}, $map->{c0dist_sd});
    $cd_sim = F1( (1-li1($cd_cdf1)), (1-li1($cd_cdf0)), 1e-5);
  }
  $cd_sim->inplace->clip(0,1e38);

  ##-- dump similarities to $doc->{cats}
  my $cname;
  @{$doc->{cats}} = map {
    $cname = $map->{lcenum}{id2sym}[$_];
    {id=>$map->{gcenum}{sym2id}{$cname}, name=>$cname, sim=>$cd_sim->at($_,0), dist_raw=>$cd_dist->at($_,0)}
  } $cd_sim->flat->qsorti->slice("-1:0")->list;
  $doc->{cats}[$_]{deg} = $_+1 foreach (0..$#{$doc->{cats}});

  return $doc;
}

##==============================================================================
## Methods: Misc

## $dxpdl = $map->svdApply($fpdl)
##  + input $fpdl is dense pdl($NT,1):     [$tid,0]     => f($tid,$doc)
##  + output $dxpdl is dense pdl(1,$svdr): [$svd_dim,0] => $svd_val
sub svdApply {
  my ($map,$fpdl) = @_;
  $fpdl = $fpdl->todense if (UNIVERSAL::isa($fpdl,'PDL::CCS::Nd')); ##-- avoid memory explosion in Nd::inner()
  $fpdl = $map->sigPdlRaw($fpdl) if (!UNIVERSAL::isa($fpdl,'PDL')); ##-- $fpdl passed as doc?
  $fpdl = ($fpdl+$map->{smoothf})->log;
  $fpdl = $fpdl->slice(":,*1") if ($fpdl->ndims != 2);              ##-- ... someone passed in a flat term pdl...
  $fpdl *= $map->{tw} if (defined($map->{tw}));                     ##-- apply term-weights if available
  return $map->{svd}->apply0($fpdl);                                ##-- apply SVD
}

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
