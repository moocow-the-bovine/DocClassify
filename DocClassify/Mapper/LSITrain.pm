## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::LSITrain.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: document-to-class mapper: LSI / SVD: training

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
  #$map->compileCrossCheck();
  #$map->compileFit();
  #$map->compileCutoffs();

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
  $map->SUPER::clearTrainingCache() or return undef;
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

## $map = $map->compileCutoffs(%opts)
##  + compiles $map->{cutoff} from $map->{c(0|1)dist_(mu|sd)}, $map->{cut(0|1)p}, $map->{cut1w}
##  + %opts: overrides @$map{cut*}
##  + OBSOLETE
sub compileCutoffs {
  my ($map,%opts) = @_;
  if (!defined($map->{c0dist_mu})) {
    $map->vlog('warn', "compileCutoffs(): no {c0dist_mu} vector defined; NOT computing cutoffs");
    return $map;
  }

  ##-- map options, defaults
  my %defaults = (cutval=>100,cut0p=>.5,cut1p=>.5,cut1w=>.5);
  $map->{$_} = $opts{$_} foreach (grep {/^cut/} keys %opts);
  $map->{$_} = $defaults{$_} foreach (grep {!defined($map->{$_})} keys(%defaults));
  $map->vlog('info', "compileCutoffs(): [v=$map->{cutval},p0=$map->{cut0p},p1=$map->{cut1p},w1=$map->{cut1w}]");

  ##-- vars: common
  my $NC      = $map->{lcenum}->size;
  my $cutval  = $map->{cutval}; ##-- psuedo-weight to add
  my $cut0p   = $map->{cut0p};
  my $cut1p   = $map->{cut1p};
  my $wt1     = $map->{cut1w};
  my $wt0     = 1-$wt1;
  my ($c0dist_mu,$c0dist_sd) = @$map{qw(c0dist_mu c0dist_sd)};
  my ($c1dist_mu,$c1dist_sd) = @$map{qw(c1dist_mu c1dist_sd)};

  ##-- cutoffs
  my $cutoff0 = $c0dist_mu - _gausswidth($cut0p,$c0dist_mu,$c0dist_sd);
  my $cutoff1 = $c1dist_mu + _gausswidth($cut1p,$c1dist_mu,$c1dist_sd);
  my $cutoff  = ($wt0*$cutoff0) + ($wt1*$cutoff1);
  my $nocutid = (defined($map->{cutCat})
		 ? $map->{lcenum}{sym2id}{$map->{cutCat}}
		 : ($map->{nullCat}
		    ? $map->{lcenum}{sym2id}{$map->{nullCat}}
		    : 0));
  $map->{cutCat} = $map->{lcenum}{id2sym}[$nocutid] if (!defined($map->{cutCat}));
  $cutoff->slice("$nocutid") .= 1e38 if (defined($nocutid));; ##-- effectively no cutoff here

  ##-- store cutoffs
  $map->{cutoff} = $cutoff;

  return $map;
}

## $map = $map->compileFit()
##  + compiles fitting paramters from $map->{dc_dist}, ($map->{dc_d2c} or $map->{dcm})
##  + OBSOLETE
sub compileFit {
  my $map = shift;

  if (!defined($map->{dc_dist})) {
    $map->vlog('warn', "compileFit(): no {dc_dist} matrix defined; NOT computing fit paramters");
    return $map;
  }
  $map->vlog('info', "compileFit()");

  ##-- vars
  my $lcenum = $map->{lcenum};
  my $NC     = $map->{lcenum}->size;
  my $ND     = $map->{dc_dist}->dim(0); ##-- allow external cross-check loaded from eval data
  my $d2c    = $map->{dc_d2c};
  if (!defined($d2c)) {
    ##-- no d2c: try to use $dcm
    if ($ND == $map->{denum}->size) {
      my $dcm    = $map->{dcm};
      my $dcm_z  = $dcm->missing->sclr;
      $d2c       = $dcm->xchg(0,1)->_missing('inf')->minimum_ind->decode;  ##-- [$di] -> $ci_best
      $dcm->missing($dcm_z);
    } else {
      ##-- bail out
      $map->vlog('warn',"compileFit(): doc-set size mismatch: {dc_dist}->dim(0) = $ND != {denum}->size = ".$map->{denum}->size.", and no {dc_d2c} defined! -- NOT computing fit paramters");
      return $map;
    }
  }
  my $dc_dist = $map->{dc_dist};

  ##--------------------
  ##-- get positive & negative samples
  my $dc_which1 = sequence($ND)->cat($d2c)->xchg(0,1);  ##-- [$di]     -> [$di,$ci] : $di \in $ci
  if ($map->{nullCat}) {
    ##-- hack positives for null cat
    my $cid_null_src = $map->{lcenum}{sym2id}{'(null)'};
    my $cid_null_dst = $map->{lcenum}{sym2id}{$map->{nullCat}};
    my $which_dst    = which($d2c==$cid_null_dst);
    $dc_which1 = $dc_which1->glue(1,$which_dst->cat(pdl(long,$cid_null_src))->xchg(0,1));
  }
  my $dc_mask1  = zeroes(byte,$dc_dist->dims);          ##-- [$di,$ci] -> 1($di \in     $ci)
  $dc_mask1->indexND($dc_which1) .= 1;
  my $dc_mask = $dc_mask1; ##-- alias
  my $dc_which0 = whichND(!$dc_mask1);                   ##-- [$di,$ci] -> 1($di \not\in $ci)
  my $nc  = $dc_mask1->long->sumover->double;            ##-- [$ci] -> |{ $di : $di \in     $ci }|
  my $nnc = $nc->sum - $nc;                              ##-- [$ci] -> |{ $di : $di \not\in $ci }|

  ##--------------------
  ## doc-cat distance matrix: by boolean membership
  ##   $dc1dist: CCS: [$di,$ci] -> dist($ci,$di) : $di     \in $ci
  ##   $dc0dist: CCS: [$di,$ci] -> dist($ci,$di) : $di \not\in $ci
  my $dc1dist = PDL::CCS::Nd->newFromWhich($dc_which1,$dc_dist->indexND($dc_which1),dims=>pdl(long,[$dc_dist->dims]));
  my $dc0dist = PDL::CCS::Nd->newFromWhich($dc_which0,$dc_dist->indexND($dc_which0),dims=>pdl(long,[$dc_dist->dims]));

  ##--------------------
  ## fit parameters: global
  ##   $dcdist_mu = pdl(1): global avg    dist($ci,$di)
  ##   $dcdist_sd = pdl(1): global stddev dist($ci,$di)
  my $dcdist_mu = $dc_dist->flat->average;
  my $dcdist_sd = (($dc_dist - $dcdist_mu)**2)->flat->average->sqrt;

  ##--------------------
  ## fit parameters: by category
  ##   $cdist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \in $ci
  ##   $cdist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \in $ci
  ##   $cdist_isgood: [$ci] -> isfinite($cddist_sd)
  my $cdist_mu = $dc_dist->average;
  my $cdist_sd = (($dc_dist - $cdist_mu->slice("*1,"))**2)->average->sqrt;
  my $cdist_isgood = ($cdist_sd->isfinite)&($cdist_sd>0);
  my $cdist_sd_raw = $cdist_sd->pdl;
  $cdist_sd = fixvals($cdist_sd, $cdist_isgood, $cdist_sd->where($cdist_isgood)->minimum/2);

  ##--------------------
  ## fit parameters: by boolean membership
  ##   $c1dist_mu0: constant: [] ->    avg d($cat,$doc) : $doc     \in $cat
  ##   $c1dist_sd0: constant: [] -> stddev d($cat,$doc) : $doc     \in $cat
  ##   $c0dist_mu0: constant: [] ->    avg d($cat,$doc) : $doc \not\in $cat
  ##   $c0dist_sd0: constant: [] -> stddev d($cat,$doc) : $doc \not\in $cat
  my $c1dist_mu0 = $dc1dist->_nzvals->average;
  my $c1dist_sd0 = (($dc1dist->_nzvals-$c1dist_mu0)**2)->average->sqrt;
  my $c0dist_mu0 = $dc0dist->_nzvals->average;
  my $c0dist_sd0 = (($dc0dist->_nzvals-$c0dist_mu0)**2)->average->sqrt;
  ##
  my $nc_min = 2;#3; #10; #50; ##-- minimum #/docs to use fit

  ##--------------------
  ## fit parameters: by category & boolean membership: positive
  ##   $c1dist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \in $ci
  ##   $c1dist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \in $ci
  ##   $c1dist_sd_nan0: like $c1dist_sd, but NaN->0
  ##   $c1dist_sd_bad0: like $c1dist_sd, but (!$c1dist_sd_isgood)->0
  my $c1dist_mu = $dc1dist->average_nz->decode;
  my $c1dist_sd = (($dc1dist - $c1dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  my $c1dist_mu_raw = $c1dist_mu->pdl;
  my $c1dist_sd_raw = $c1dist_sd->pdl;
  my $c1dist_isgood = (($c1dist_sd_raw->isfinite) & ($nc >= $nc_min));
  my $c1dist_sd_nan0 = fixvals($c1dist_sd, $c1dist_sd->isfinite, 0);
  my $c1dist_sd_bad0 = fixvals($c1dist_sd, $c1dist_isgood, 0);
  $c1dist_mu = fixvals($c1dist_mu, $c1dist_isgood, $c1dist_mu->where($c1dist_isgood)->minimum);
  $c1dist_sd = fixvals($c1dist_sd, $c1dist_isgood, $c1dist_sd->where($c1dist_isgood)->minimum/2);

  ##--------------------
  ## fit parameters: by category & boolean membership: negative
  ##   $c0dist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \not\in $ci
  ##   $c0dist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \not\in $ci
  ##   $c0dist_sd_nan0: like $c0dist_sd, but NaN->0
  ##   $c0dist_sd_bad0: like $c0dist_sd, but (!$c0dist_sd_isgood)->0
  my $c0dist_mu = $dc0dist->average_nz->decode;
  my $c0dist_sd = (($dc0dist - $c0dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  my $c0dist_mu_raw = $c0dist_mu->pdl;
  my $c0dist_sd_raw = $c0dist_sd->pdl;
  #my $c0dist_isgood = (($c0dist_sd_raw->isfinite) & ($nc >= $nc_min));
  my $c0dist_isgood = $c0dist_sd_raw->isfinite;
  my $c0dist_sd_nan0 = fixvals($c1dist_sd, $c1dist_sd->isfinite, 0);
  my $c0dist_sd_bad0 = fixvals($c1dist_sd, $c1dist_isgood, 0);
  $c0dist_mu = fixvals($c0dist_mu, $c0dist_isgood, $c0dist_mu->where($c0dist_isgood)->minimum);
  $c0dist_sd = fixvals($c0dist_sd, $c0dist_isgood, $c0dist_sd->where($c0dist_isgood)->maximum);


  ##--------------------
  ## store final fit parameters
  $map->{c1dist_mu} = $c1dist_mu;
  $map->{c1dist_sd} = $c1dist_sd;
  $map->{c0dist_mu} = $c0dist_mu;
  $map->{c0dist_sd} = $c0dist_sd;

  return $map;
}

## $map = $map->loadCrossCheckEval($eval)
##  + populates $map->{dc_dist} from a DocClassify::Eval object
##  + cross-check data should have been created on a subset of the corpus used to train this mapper!
##  + OBSOLETE
sub loadCrossCheckEval {
  my ($map,$eval) = @_;
  $map->vlog('info',"loadCrossCheckEval(): ".($eval->{label}||'')) if ($map->{verbose});

  ##-- vars
  my $lcenum = $map->{lcenum};
  my $NC = $lcenum->size;
  my $lc_sym2id = $lcenum->{sym2id};
  my $lab2docs  = $eval->{lab2docs};

  ##-- eval-local $denum
  my $denum = MUDL::Enum->new;
  @{$denum->{id2sym}} = keys %$lab2docs;
  @{$denum->{sym2id}}{@{$denum->{id2sym}}} = (0..$#{$denum->{id2sym}});
  my $d_sym2id = $denum->{sym2id};
  my $NDx = $denum->size;

  ##-- $dc_d2c: pdl($NDx): [$di] -> $ci_wanted
  my $d2c = pdl(long,[@$lc_sym2id{map {$lab2docs->{$_}[0]{cats}[0]{name}} @{$denum->{id2sym}}}]);

  ##-- $dc_dist: pdl($NDx,$NC): [$dix,$ci] -> dist($ci,$di)
  my $dc_dist = zeroes($NDx,$NC)+2; ##-- initialize to a meaningful maximum
  my ($lab,$d12,@dcats2,@gotcat,@dcname,@dci,@dcdist,$di);
  while (($lab,$d12)=each(%{$eval->{lab2docs}})) {
    if (!defined($di = $d_sym2id->{$lab})) {
      $map->logwarn("loadCrossCheckEval(): no ID for doc label '$lab' -- skipping");
      next;
    }
    #@dcats2 = grep {!$gotcat[$_->{id}] && ($gotcat[$_->{id}]=1)} @{$d12->[1]{cats}}; ##-- handle dup cats (e.g. nullCat)
    @dcats2 = @{$d12->[1]{cats}}; ##-- ... or don't
    @dcname = map  {$_->{proto} ? $_->{proto} : $_->{name}} @dcats2;
    @dci    = grep {defined($lc_sym2id->{$dcname[$_]})} (0..$#dcats2);
    @dcdist = map  {$dcats2[$_]{dist_raw}} @dci;
    $dc_dist->slice("($di),")->index(pdl(long,[@$lc_sym2id{@dcname[@dci]}])) .= pdl(\@dcdist);
  }

  ##-- cache values: {dc_dist}, {dc_d2c}
  $map->{dc_dist} = $dc_dist;
  $map->{dc_d2c}  = $d2c;

  return $map;
}

## $map = $map->compileCrossCheck()
##  + does cross-checking for training data
##  + caches $map->{dc_dist}: dense pdl [$di,$ci] -> dist($di,$ci)
##  + OBSOLETE
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

  ##-- epsilon (avoid null vectors)
  #$map->compile_xeps(%opts);

  ##-- vector: $map->{tw_ccs}: ($NT): [$tid] -> weight($term[$tid])
  #$map->compile_tw_ccs(%opts);

  return $map;
}

## $tw_ccs = $map->compile_tw_ccs(%opts)
##  + compiles CCS representation of $map->{tw} to $map->{tw_ccs}
##  + %opts:
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
sub compile_tw_ccs {
  my ($map,%opts) = @_;
  my $label = $map->labelString(%opts);
  $map->vlog('info',"compile_tw_ccs() [$label]: tw_ccs") if ($map->{verbose});
  $map->{tw_ccs} = $map->{tw}->toccs;
  $map->{tw_ccs}->missing(0);
  return $map->{tw_ccs};
}

## $tw_ccs = $map->tw_ccs()
##  + get or compile $map->{tw_ccs}
sub tw_ccs {
  return $_[0]{tw_ccs} if (defined($_[0]{tw_ccs}));
  return $_[0]->compile_tw_ccs();
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
  if    ($catProfile =~ /^fold\-a/) { $catProfile = 'fold-avg'; }
  elsif ($catProfile =~ /^fold/) { $catProfile = 'fold-in'; }
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
  elsif ($catProfile eq 'fold-avg') {
    $map->compile_tcm0() if (!defined($map->{tcm0}));
    $map->compile_tcm()  if (!defined($map->{tcm}));
    my $tcma = $map->{tcm}->decode;
    my $cn   = ($map->{dcm}->decode != 0)->double->sumover;
    $map->{xcm} = $map->{svd}->apply($tcma/$cn->slice("*1,"));
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
	(my $tmp=$xcm->slice(",$c_id")) += $d_x;
      }
    }
    if ($map->{nullCat}) {
      $c_id  = $map->{lcenum}{sym2id}{'(null)'};
      $xcm->slice(",$c_id") .= $map->svdApply(zeroes($map->{tenum}->size,1));
    }
    $map->{xcm} = $xcm;
  }

  return $map;
}



##==============================================================================
## Functions: Utils

## $vals_fixed = fixvals($vals,$isgood_mask,$fixval)
##  + OBSOLETE?
sub fixvals {
  my ($vals,$isgood,$fixval) = @_;
  $isgood = (($vals->isfinite)&($vals>0)) if (!defined($isgood) || $isgood->isnull);
  $fixval = $vals->where($isgood)->average if (!defined($fixval));
  my $fixed = $vals->pdl;
  $fixed->where(!$isgood) .= $fixval;
  return $fixed;
}


##==============================================================================
## Footer
1;

__END__
