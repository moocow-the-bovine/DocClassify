## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::Cutoff.pm
## Author: Bryan Jurish <jurish@.uni-potsdam.de>
## Descript: document classifier: document-to-class pseudo-mapper: cutoffs

package DocClassify::Mapper::Cutoff;
use DocClassify::Utils ':all';

use MUDL::Enum;
use MUDL::PDL::Stats;
use MUDL::PDL::Smooth;

use PDL;
use PDL::CCS::Nd;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Mapper);

#our $verbose = 2;
our $verbose = 3;

##==============================================================================
## Constructors etc.

## $map = $CLASS_OR_OBJ->new(%opts)
## %$map, %opts:
##  ##
##  ##-- options: generl
##  verbose => $vlevel,              ##-- verbosity level (default=$verbose)
##  eval    => $eval,                ##-- DocClassify::Eval training source (REQURIED; see also trainEval() method)
##  ##
##  ##-- options: cutoff
##  cut0p => $p,                     ##-- confidence level for negative-sample cutoff fitting (0.5)
##  cut1p => $p,                     ##-- confidence level for positive-sample cutoff fitting (0.5)
##  cut1w => $w,                     ##-- positive weight (0<=$w<=1) for cutoff fitting (0.5)
##  cutval => $val,                  ##-- constant to add if cutoff is exceeded (default=100)
##  cutCat => $catName,              ##-- name of cutoff sink cat (default: cat with id=0 in $lcenum)
##  ##
##  ##-- data: enums
##  lcenum => $globalCatEnum,        ##-- local cat enum, compcat ($NC=$catEnum->size())
##  gcenum => $localCatEnum,         ##-- global cat enum         ($NCg=$globalCatEnum->size())
##  denum  => $docEnum,              ##-- [temp] local doc enum   ($ND=$docEnum->size())
##  ##
##  ##-- data: post-compile()
##  cutCatId => $id,                 ##-- local id of {cutCat}
##  dc_dist => $dc_dist,             ##-- [temp] dense PDL ($ND,$NC) : [$di,$ci] -> dist($ci,$di)
##  d2c     => $d2c,                 ##-- dense PDL ($ND) : [$di] -> $ci : $di \in $ci
##  c1dist_mu => $c1dist_mu,         ##-- dense PDL ($NC) : [$ci] ->    avg { dist($ci,$di) : $di  \in $ci }
##  c1dist_sd => $c1dist_sd,         ##-- dense PDL ($NC) : [$ci] -> stddev { dist($ci,$di) : $di  \in $ci }
##  c0dist_mu => $c0dist_mu,         ##-- dense PDL ($NC) : [$ci] ->    avg { dist($ci,$di) : $di !\in $ci }
##  c0dist_sd => $c0dist_sd,         ##-- dense PDL ($NC) : [$ci] -> stddev { dist($ci,$di) : $di !\in $ci }
##  cutoff    => $cutoff,            ##-- dense PDL ($NC) : [$ci] ->    max { dist($ci,$di) : $di  \in $ci } : heuristic
sub new {
  my $that = shift;
  my $obj =  $that->SUPER::new(
			       ##-- options
			       verbose => $verbose,
			       eval => undef,
			       cut0p => 0.5,
			       cut1p => 0.5,
			       cut1w => 0.5,
			       cutval => 100,
			       cutCat => undef, ##-- default computed in compileEnums()

			       ##-- data: enums
			       lcenum => MUDL::Enum->new,
			       gcenum => MUDL::Enum->new,
			       denum => MUDL::Enum->new,

			       ##-- data: post-compile
			       cutCatId =>undef,
			       c1dist_mu=>undef,
			       c1dist_sd=>undef,
			       c0dist_mu=>undef,
			       c0dist_sd=>undef,
			       cutoff=>undef,

			       ##-- user args
			       @_,
			      );
  return $obj;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override appends qw(eval denum dc_dist dc_d2c d2c c1dist_mu c1dist_sd c0dist_mu c0dist_sd cutoff)
sub noShadowKeys {
  return ($_[0]->SUPER::noShadowKeys(@_[1..$#_]),
	  qw(eval denum),
	  qw(dc_dist dc_d2c d2c c1dist_mu c1dist_sd c0dist_mu c0dist_sd cutoff));
}

##==============================================================================
## Methods: API: Training

## $map = $map->trainCorpus($corpus)
##  + add training data from $corpus
##  + inherited default just calls $map->trainDocument($doc) foreach doc in corpus

## $map = $map->trainDocument($doc)
## $map = $map->trainDocument($doc)
##  + add training data from $doc
##  + inherited default just dies

##==============================================================================
## Methods: NEW: Training

## $map = $map->trainEval($eval)
##  + trains cutoffs from DocClassify::Eval object $eval
##  + adds $eval to $map->{eval} (destructive alters $map->{eval} if defined, otherwise sets $map->{eval}=$eval)
sub trainEval {
  my ($map,$eval) = @_;
  if (!defined($map->{eval})) {
    $map->{eval} = $eval;
  } else {
    $map->{eval}->addEval($eval);
  }
  return $map;
}

##==============================================================================
## Methods: API: Compilation

## $map = $map->compile(%opts)
##  + compile underlying cutoff data from $map->{eval}
##  + should be called only after all training data have been added with trainEval()
sub compile {
  my ($map,%opts) = @_;

  ##-- report
  $map->vlog('info', "compile()") if ($map->{verbose});
  if (!defined($map->{eval})) {
    $map->logconfess("compile(): no {eval} key defined -- you must call trainEval()!");
    return undef;
  }

  ##--------------------
  ## vars
  my $eval = $map->{eval};
  my $lab2docs = $eval->{lab2docs};

  ##--------------------
  ## compile: enums
  $map->vlog('info', "compile(): enums") if ($map->{verbose});
  ##
  my $denum  = $map->{denum} = MUDL::Enum->new;
  @{$denum->{id2sym}} = keys %$lab2docs;
  @{$denum->{sym2id}}{@{$denum->{id2sym}}} = (0..$#{$denum->{id2sym}});
  my $d_sym2id = $denum->{sym2id};
  my $ND = $denum->size;
  ##
  my $gcenum = $map->{gcenum} = MUDL::Enum->new;
  my ($d12,$cat,$cid);
  foreach $d12 (values(%$lab2docs)) {
    foreach $cat (map {@{$_->{cats}}} @$d12) {
      $cid = ($cat->{id}||0);
      $cid = $1 if ($cat->{name} =~ /^(\d+)_/);
      $gcenum->addIndexedSymbol($cat->{name}, $cid);
    }
  }
  my $lcenum = $map->{lcenum} = $gcenum->clone->compact;
  my $lc_sym2id = $lcenum->{sym2id};
  my $NC = $lcenum->size;

  ##--------------------
  ## compile $d2c
  $map->vlog('info', "compile(): d2c (ND=$ND) [Doc -> Cat]") if ($map->{verbose});
  my $d2c = pdl([@$lc_sym2id{map {$lab2docs->{$_}[0]{cats}[0]{name}} @{$denum->{id2sym}}}]); ##-- [$di] -> $ci_wanted
  $map->{d2c} = $d2c;

  ##--------------------
  ## ... other vars
  my $gcids  = pdl(long, [@{$gcenum->{sym2id}}{@{$lcenum->{id2sym}}}]);             ##-- [$lci] -> $gci
  my $lcids  = zeroes(long,$gcids->max+1); $lcids->index($gcids) .= $gcids->xvals;  ##-- [$gci] -> $lci
  my $NCg    = $gcids->max+1;

  ##--------------------
  ## $dc_dist (re-worked from Mapper::LSI::loadCrossCheckEval()
  $map->vlog('info', "compile(): dc_dist (ND=$ND x NC=$NC) [Doc x Cat -> Dist]") if ($map->{verbose});
  my $dc_dist = $map->{dc_dist} = zeroes($ND,$NC)+2; ##-- initialize to a meaningful max (here=2)
  my ($lab,@dcats2,@gotcat,@dci,@dcdist,$di); #$d12
  while (($lab,$d12)=each(%$lab2docs)) {
    if (!defined($di = $d_sym2id->{$lab})) {
      warn("$0: no internal ID for doc label '$lab' -- skipping");
      next;
    }
    @gotcat = qw();
    @dcats2 = grep {!$gotcat[$_->{id}] && ($gotcat[$_->{id}]=1)} @{$d12->[1]{cats}}; ##-- handle dup cats (e.g. nullCat)
    @dci    = grep {defined($lc_sym2id->{$dcats2[$_]{name}})} (0..$#dcats2);
    @dcdist = map {$dcats2[$_]{dist_raw}} @dci;
    $dc_dist->slice("($di),")->index(pdl(long,[@$lc_sym2id{map {$dcats2[$_]{name}} @dci}])) .= pdl(\@dcdist);
  }

  ##--------------------
  ##-- get positive & negative evidence
  $map->vlog('info', "compile(): fit parameters") if ($map->{verbose});
  my $dc_which1 = sequence($ND)->cat($d2c)->xchg(0,1);  ##-- [$di]     -> [$di,$ci] : $di \in $ci
  my $dc_mask1  = zeroes(byte,$dc_dist->dims);          ##-- [$di,$ci] -> 1($di \in     $ci)
  $dc_mask1->indexND($dc_which1) .= 1;
  my $dc_mask   = $dc_mask1; ##-- alias
  my $dc_which0 = whichND(!$dc_mask1);                   ##-- [$di,$ci] -> 1($di \not\in $ci)
  my $nc  = $dc_mask1->long->sumover->double;            ##-- [$ci] -> |{ $di : $di \in     $ci }|
  my $nnc = $nc->sum - $nc;                              ##-- [$ci] -> |{ $di : $di \not\in $ci }|

  ##--------------------
  ## doc-cat distance matrix: by boolean membership
  ##   $dc1dist: CCS: [$di,$ci] -> dist($ci,$di) : $di     \in $ci
  ##   $dc0dist: CCS: [$di,$ci] -> dist($ci,$di) : $di \not\in $ci
  my $dc1dist = PDL::CCS::Nd->newFromWhich($dc_which1,$dc_dist->indexND($dc_which1));
  my $dc0dist = PDL::CCS::Nd->newFromWhich($dc_which0,$dc_dist->indexND($dc_which0));

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

  ##--------------------
  ## compile cutoff vector: params
  my %defaults = (cutval=>100,cut0p=>.5,cut1p=>.5,cut1w=>.5);
  $map->{$_} = $opts{$_} foreach (grep {/^cut/} keys %opts);
  $map->{$_} = $defaults{$_} foreach (grep {!defined($map->{$_})} keys(%defaults));
  $map->vlog('info', "compile(): cutoff (NC=$NC) [Cat -> maxDist]") if ($map->{verbose});
  $map->vlog('info', "compile(): cutoff: params=(v=$map->{cutval},p0=$map->{cut0p},p1=$map->{cut1p},w1=$map->{cut1w}]")
    if ($map->{verbose});

  ##--------------------
  ## compile cutoff vector: guts
  my $cutoff0 = $c0dist_mu - _gausswidth($map->{cut0p},$c0dist_mu,$c0dist_sd);
  my $cutoff1 = $c1dist_mu + _gausswidth($map->{cut1p},$c1dist_mu,$c1dist_sd);
  my $cutoff  = ((1-$map->{cut1w})*$cutoff0) + ($map->{cut1w}*$cutoff1);
  my $nocutid = $map->{cutCatId}
    = (defined($map->{cutCat})
       ? $map->{lcenum}{sym2id}{$map->{cutCat}}
       : (defined($map->{nullCat})
	  ? $map->{lcenum}{sym2id}{$map->{nullCat}}
	  : 0));
  $map->{cutCat} = $map->{lcenum}{id2sym}[$nocutid] if (!defined($map->{cutCat}));
  $cutoff->slice("$nocutid") .= 1e38 if (defined($nocutid));; ##-- effectively no cutoff here

  ##-- store cutoff vector
  $map->{cutoff} = $cutoff;

  ##--------------------
  ## eval: report accuracies
  if ($map->{verbose}) {
    my $acc_raw = ($dc_dist->xchg(0,1)->minimum_ind==$d2c)->nnz->double / $ND;
    ##
    my $dc_dist_new = $dc_dist->pdl;
    my $cut_mask    = ($dc_dist_new > $cutoff->slice("*1,"));
    $dc_dist_new->where($cut_mask) += $map->{cutval};
    my $acc_new = ($dc_dist_new->xchg(0,1)->minimum_ind==$d2c)->nnz->double / $ND;
    ##
    $map->vlog('info', "compile(): accuracy[-cut] = $acc_raw");
    $map->vlog('info', "compile(): accuracy[+cut] = $acc_new");
  }

  return $map;
}

## $bool = $map->compiled()
##  + returns true iff $map has been compiled
##  + override checks for $map->{cutoff}
sub compiled { return defined($_[0]{cutoff}); }

## $map = $map->clearTrainingCache()
##  + clears any cached data from training
##  + after calling this, $map may no longer be able to train
##  + calls inherited Mapper::ByLemma::clearTrainingCache()
##  + additionally clears @$map{qw(denum dc_dist eval)}
sub clearTrainingCache {
  my $map = shift;
  $map->SUPER::clearTrainingCache();
  delete($map->{eval});
  delete($map->{dc_dist});
  $map->{denum}->clear() if (defined($map->{denum}));
  return $map;
}

##==============================================================================
## Functions: Utils

## $vals_fixed = fixvals($vals,$isgood_mask,$fixval)
sub fixvals {
  my ($vals,$isgood,$fixval) = @_;
  $isgood = (($vals->isfinite)&($vals>0)) if (!defined($isgood) || $isgood->isnull);
  $fixval = $vals->where($isgood)->average if (!defined($fixval));
  my $fixed = $vals->pdl;
  $fixed->where(!$isgood) .= $fixval;
  return $fixed;
}

##==============================================================================
## Methods: API: Classification

## $corpus = $map->mapCorpus($corpus)
##  + Attempt to classify each $doc in $corpus,
##    destructively altering $doc->{cats} to reflect classification results.
##  + inherited default just calls $map->mapDocument() on each $doc in $corpus.

## $doc = $map->mapDocument($doc)
##  + attempt to classify $doc
##  + destructively alters $doc->{cats} to reflect classification results
sub mapDocument {
  my ($map,$doc) = @_;

  ##-- be verbose?
  $map->vlog('trace', "mapDocument(".$doc->label.")") if ($map->{verbose}>=3);

  ##-- sanity check(s)
  if (!defined($map->{cutoff})) {
    $map->logwarn("mapDocument(): no {cutoff} vector defined, not activating") if ($map->{verbose});
    return $doc;
  }

  ##-- preparation
  my $lcenum  = $map->{lcenum};
  my $cutoff  = $map->{cutoff};
  $map->{cutCatId} ||= 0;

  ##-- distribution stuff
  my ($mu0,$sd0,$mu1,$sd1) = @$map{qw(c0dist_mu c0dist_sd c1dist_mu c1dist_sd)};
  my $NC       = $mu0->nelem;
  my $dist_ids = pdl(long, [@{$lcenum->{sym2id}}{map {$_->{name}} @{$doc->{cats}}}]);
  my $dist_raw = zeroes($NC);
  $dist_raw->index($dist_ids) .= pdl(double,[map {$_->{dist_raw}} @{$doc->{cats}}]);
  ##
  my $p1       = 1-_gausscdf($dist_raw,$mu1,$sd1);
  #my $p1e      = $p1/$NC;
  #my $p0       =   _gausscdf($dist_raw,$mu1,$sd1);
  #my $p0e      = $p0/$NC;
  #my $p0avg    = $p0e->sumover;

  ##-- apply cutoffs
  my ($cid);
  foreach (@{$doc->{cats}}) {
    $_->{cut} = 0;
    $_->{deg} = 0;
    next if (!defined($cid=$lcenum->{sym2id}{$_->{name}}));

    ##-- mark "confidence" value
    #$_->{confp} = ($p0avg + $p1e->at($cid) - $p0e->at($cid))->sclr;
    ##--
    $_->{confp} = $p1->at($cid);
    ##
    ##-- ... and quantize it too
    $_->{confi} = int($_->{confp}*10);

    next if ($cid==$map->{cutCatId} || $_->{dist_raw} <= $cutoff->at($cid)); ##-- no cutoff
    #$_->{dist_raw} += $map->{cutval};
    $_->{cut}       = 1;
  }

  ##-- re-sort && re-number cats
  $doc->cats();
  $doc->{cats}[$_]{deg} = $_+1 foreach (0..$#{$doc->{cats}});

  return $doc;
}


##==============================================================================
## Methods: API: I/O
##  + see DocClassify::Object


##==============================================================================
## Footer
1;

__END__
