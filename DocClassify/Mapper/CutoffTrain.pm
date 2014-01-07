## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::CutoffTrain.pm
## Author: Bryan Jurish <jurish@.uni-potsdam.de>
## Descript: document classifier: document-to-class pseudo-mapper: cutoffs: training

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
  $map->vlog('info', "trainEval()") if ($map->{verbose});

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
      if (!defined($gcenum->{id2sym}[$cid])) {
	$gcenum->addIndexedSymbol($cat->{name}, $cid);
      } elsif ($gcenum->{id2sym}[$cid] ne $cat->{name}) {
	$gcenum->addSymbol($cat->{name});
      }
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
  $map->info("compile(): cutCatId=$nocutid ; cutCat='$map->{cutCat}'");

  ##-- store cutoff vector
  $map->{cutoff} = $cutoff;

  ##-- optimize (maybe)
  $map->optimize();

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
  $map->info("clearTrainingCache()") if ($map->{verbose});
  $map->SUPER::clearTrainingCache();
  delete($map->{eval});
  delete($map->{dc_dist});
  $map->{denum}->clear() if (defined($map->{denum}));
  return $map;
}

##==============================================================================
## Methods: Optimization

## $map = $map->optimize(%opts)
##  + %opts: clobbers %$map
##  + performs $map->{optimize} random optimization iterations on $map->{cutoff}
##  + salient %$map, %opts:
##     optimize => $n,      ##-- number of optimization iterations
##     fitness => $func,    ##-- fitness function: one of 'F', 'avg', 'min', 'acc'; default='F'
##     seed => $seed,       ##-- random seed
sub optimize {
  my ($map,%opts) = @_;
  @$map{keys %opts} = values %opts;
  $map->{optimize} ||= 0;
  $map->{fitness} = 'F' if (!defined($map->{fitness}));
  my $fitf = $map->{fitness};
  $map->vlog('info',
	     "optimize("
	     ."n=$map->{optimize}"
	     .", seed=".(defined($map->{seed}) ? $map->{seed} : 'undef')
	     .", fitness=$fitf"
	     .")"
	    ) if ($map->{verbose});
  srand($map->{seed}) if (defined($map->{seed}));

  ##-- common vars
  my $dc_dist = $map->{dc_dist};

  ##-- init: current state
  my $cut     = $map->{cutoff};        ##-- current best cutoff
  my $fit     = $map->fitness($cut);   ##-- current best fitness
  my $cchist  = $map->{cc_hist};       ##-- current best histogram (if cached by fitness())
  ##
  my $cut_raw = $cut->zeroes + 1e38;      ##-- dummy cutoff vector for raw fitness
  my $fit_raw = $map->fitness($cut_raw);  ##-- raw fitness
  my $cchist_raw = $map->{cc_hist};       ##-- current best histogram (if cached by fitness())
  ##
  my $i       = 0;  ##-- optmization iteration (generation)
  my $ilen    = length($map->{optimize});
  $map->info("optimize(): fitness:$fitf\[RAW]=$fit_raw");
  $map->info("optimize(): fitness:$fitf\[USR]=$fit");
  ##
  ($cut,$fit,$cchist)=($cut_raw,$fit_raw,$cchist_raw) if ($fit_raw > $fit);
  $map->info("optimize(): fitness:$fitf\[INITIAL]=$fit");
  for ( ; $i < $map->{optimize}; $i++) {
    #print STDERR ".";     ##-- debug

    ##-- mutate
    my $cuti = $map->mutate($cut);
    my $fiti = $map->fitness($cuti);
    if ($fiti > $fit) {
      $map->debug("optimize(): ", sprintf("fitness:$fitf\[i=%${ilen}d]=%.5f; DIFF=%.5e", $i, $fiti, $fiti-$fit));
      $cut = $cuti;
      $fit = $fiti;
    }
  }

  ##-- verbose cutoff dump
  if ($map->{verbose} > 1) {
    my ($c,$ci);
    foreach $c (sort DocClassify::Utils::catcmp keys %{$map->{lcenum}{sym2id}}) {
      $ci = $map->{lcenum}{sym2id}{$c};
      $map->debug(sprintf("optimize(): cutoff[%2d] = %.4e (%s)", $ci, $cut->at($ci), $c));
    }
  }

  ##-- dump final fitness
  $fit = $map->fitness($cut); ##-- re-cache fitness data just in case
  $map->info("optimize(): FINAL: fitness:$fitf=$fit");

  ##-- update & return
  $map->{cutoff} = $cut;
  return $map;
}

##----------------------------------------------------------------------
## Methods: Optimization: Mutation

## $cuti = $map->mutate($cut)
##  + randomly mutates $cut
BEGIN {
  #*mutate = \&mutate_bounds;   ##-- better for *fitness=\&fitness_acc
  #*mutate = \&mutate_select;   ##-- better for *fitness=\&fitness_nz_avg_F
  *mutate = \&mutate_norm;     ##-- good for both, but especially for *fitness=\&fitness_nz_avg_F
}

## $cuti = $map->mutate_norm($cut)
##  + randomly mutates $cut: generates gaussians from $map->{c0dist_(mu|sd)}
sub mutate_norm {
  my ($map,$cut) = @_;
  my $cats = $map->mutate_which();
  my $mu = $map->{c0dist_mu}->index($cats);
  my $sd = $map->{c0dist_sd}->index($cats);
  ##
  ##-- increase stddev to improve range of search (format: "seed=0, seed=1, seed=2 | random, random, random")
  #my $sdx = 1;    ##-- 0.425866, 0.425507, 0.425828, | 0.423594, 0.423780, 0.423748, 
  #my $sdx = 2;    ##-- 0.439277, 0.438787, 0.437915, | 0.435887, 0.436245, 0.437118, 
  #my $sdx = 2.5;  ##-- 0.442736, 0.443208, 0.443644, | 0.442460, 0.438160, 0.441279, 
  my $sdx = 3;    ##-- 0.445141, 0.444375, 0.444212, | 0.444899, 0.441770, 0.441459, 
  #my $sdx = 4;    ##-- 0.434286, 0.434467, 0.428762, | 0.432759, 0.431710, 0.436401, 
  #my $sdx = 8;    ##-- 0.431044, 0.429787, 0.429907, | 0.429186, 0.429112, 0.436093, 
  ##
  my $cuti = $cut->pdl;
  $cuti->index($cats) .= grandom($cats->nelem) * $sd * $sdx + $mu;
  return $cuti;
}

## $cuti = $map->mutate_select($cut)
##  + randomly mutates $cut: selects values from $map->{dc_dist}
sub mutate_select {
  my ($map,$cut) = @_;
  my $mutate_which = $map->mutate_which();

  ##-- random mutation
  my $cuti = $cut->pdl;
  my $ND = $map->{dc_dist}->dim(0);
  $cuti->index($mutate_which) .= $map->{dc_dist}->index2d((random($mutate_which->nelem)*$ND)->long,$mutate_which);
  return $cuti;
}


## $cuti = $map->mutate($cut)
##  + randomly mutates $cut; generates values in a range
sub mutate_bounds {
  my ($map,$cut) = @_;
  my $mutate_which = $map->mutate_which();

  ##-- get mutation bounds
  my $mutate_bounds = $map->{mutate_bounds};
  if (!defined($mutate_bounds)) {
    $mutate_bounds = $map->{mutate_bounds} = [];
    $mutate_bounds->[0] = $map->{dc_dist}->minimum;
    $mutate_bounds->[1] = $map->{dc_dist}->maximum;
  }
  my $mutate_min = $mutate_bounds->[0]->index($mutate_which);
  my $mutate_rng = $mutate_bounds->[1]->index($mutate_which) - $mutate_min;

  ##-- random mutation
  my $cuti = $cut->pdl;
  $cuti->index($mutate_which) .= ($mutate_which->random * $mutate_rng) + $mutate_min;
  return $cuti;
}

## $cids = $map->mutate_which()
##  + select categories for mutation
sub mutate_which {
  my $map = shift;
  my $NC  = $map->{dc_dist}->dim(1);
  my $pmutate     = $map->{pmutate} || 1.0/$NC;
  my $mutate_mask = random($NC) < $pmutate;
  $mutate_mask->slice($map->{cutCatId}) .= 0; ##-- never mutate the cutoff cat itself
  if (!$mutate_mask->any) {
    my $mutate_mask_nz = $mutate_mask->where($mutate_mask->xvals != $map->{cutCatId});
    $mutate_mask_nz->index(int(rand($mutate_mask_nz->nelem))) .= 1; ##-- just select a single random cat
  }
  return $mutate_mask->which;
}

##----------------------------------------------------------------------
## Methods: Optimization: Fitness

## $fitness = $map->fitness($cutoff)
##  + fitness function
##  + selects based on $map->{fitness}
#BEGIN {
#  #*fitness = \&fitness_acc;
#  *fitness = \&fitness_nz_avg_F;
#  #*fitness = \&fitness_nz_avg_a;
#}
sub fitness {
  my $map = shift;
  my $func = lc($map->{fitness} || 'F');
  if    ($func eq 'f')   { return $map->fitness_nz_avg_F(@_); }
  elsif ($func eq 'avg') { return $map->fitness_nz_avg_a(@_); }
  elsif ($func eq 'min') { return $map->fitness_nz_avg_min(@_); }
  elsif ($func eq 'acc') { return $map->fitness_acc(@_); }
  else {
    $map->vlog('warn', "fitness(): unknown fitness function '$func' - using F");
    return $map->fitness_nz_avg_F(@_);
  }
}

## $fitness = $map->fitness_acc($cutoff)
##  + fitness function, global accuracy
sub fitness_acc {
  my ($map,$cutoff) = @_;
  my $dc_dist_fit = $map->{dc_dist}->pdl;
  $dc_dist_fit->where($dc_dist_fit > $cutoff->slice("*1,")) += $map->{cutval};
  my $d2c_fit = $dc_dist_fit->xchg(0,1)->minimum_ind;
  my $ntp = ($d2c_fit==$map->{d2c})->nnz->double->sclr;
  my $ND  = $d2c_fit->nelem;
  return $ntp / $ND;
}

## $fitness = $map->fitness_nz_avg_F($cutoff)
##  + fitness function, non-null average F
sub fitness_nz_avg_F {
  my ($map,$cutoff) = @_;
  my $dc_dist_fit = $map->{dc_dist}->pdl;
  $dc_dist_fit->where($dc_dist_fit > $cutoff->slice("*1,")) += $map->{cutval};
  my $d2c_fit = $dc_dist_fit->xchg(0,1)->minimum_ind;
  ##
  ##-- $cc_hist : [$c_wanted,$c_got] -> $ndocs
  my $cc_hist_dims = zeroes(long,2) + $dc_dist_fit->dim(1);
  my $cc_hist_wnd  = $map->{d2c}->long->cat($d2c_fit)->xchg(0,1);
  my $cc_hist = PDL::CCS::Nd->newFromWhich($cc_hist_wnd, $d2c_fit->ones->long, dims=>$cc_hist_dims)->dummy(0,1)->sumover->decode;
  $map->{cc_hist} = $cc_hist; ##-- cache histogram
  ##
  my $tp = $cc_hist->diagonal(0,1)->double;        ##-- [$c] -> $tp
  my $n1 = $cc_hist->xchg(0,1)->sumover->double;   ##-- [$c] -> $nwanted = $tp + $fn
  my $n2 = $cc_hist->sumover->double;              ##-- [$c] -> $ngot    = $tp + $fp
  ##
  my $pr = $tp / $n2;
  my $rc = $tp / $n1;
  #my $F  = 2.0 / ($pr**-1 + $rc**-1);
  ##
  #my $fp = $n2-$tp;
  #my $fn = $n1-$tp;
  #dbg_cchist($map,$cc_hist); ##-- DEBUG
  ##
  my $nz_mask = defined($map->{nz_mask}) ? $map->{nz_mask} : ($map->{nz_mask}=(($tp->xvals!=$map->{cutCatId}) & ($n1>0) & ($n2>0)));
  return
    #$pr->where($nz_mask)->avg
    #$rc->where($nz_mask)->avg
    #$F->where($nz_mask)->avg
    (2.0 / ($pr->where($nz_mask)->avg**-1 + $rc->where($nz_mask)->avg**-1))
    ;
}

## $fitness = $map->fitness_nz_avg_a($cutoff)
##  + fitness function, non-null cat-wise average (pr+rc)/2 average
sub fitness_nz_avg_a {
  my ($map,$cutoff) = @_;
  my $dc_dist_fit = $map->{dc_dist}->pdl;
  $dc_dist_fit->where($dc_dist_fit > $cutoff->slice("*1,")) += $map->{cutval};
  my $d2c_fit = $dc_dist_fit->xchg(0,1)->minimum_ind;
  ##
  ##-- $cc_hist : [$c_wanted,$c_got] -> $ndocs
  my $cc_hist_dims = zeroes(long,2) + $dc_dist_fit->dim(1);
  my $cc_hist_wnd  = $map->{d2c}->long->cat($d2c_fit)->xchg(0,1);
  my $cc_hist = PDL::CCS::Nd->newFromWhich($cc_hist_wnd, $d2c_fit->ones->long, dims=>$cc_hist_dims)->dummy(0,1)->sumover->decode;
  $map->{cc_hist} = $cc_hist; ##-- cache histogram
  ##
  my $tp = $cc_hist->diagonal(0,1)->double;        ##-- [$c] -> $tp
  my $n1 = $cc_hist->xchg(0,1)->sumover->double;   ##-- [$c] -> $nwanted = $tp + $fn
  my $n2 = $cc_hist->sumover->double;              ##-- [$c] -> $ngot    = $tp + $fp
  ##
  my $pr = $tp / $n2;
  my $rc = $tp / $n1;
  ##
  my $nz_mask = defined($map->{nz_mask}) ? $map->{nz_mask} : ($map->{nz_mask}=(($tp->xvals!=$map->{cutCatId}) & ($n1>0) & ($n2>0)));
  return
    #$pr->where($nz_mask)->avg
    #$rc->where($nz_mask)->avg
    #$F->where($nz_mask)->avg
    #(2.0 / ($pr->where($nz_mask)->avg**-1 + $rc->where($nz_mask)->avg**-1))
    (($pr+$rc)/2.0)->where($nz_mask)->avg;
    ;
}

## $fitness = $map->fitness_nz_avg_min($cutoff)
##  + fitness function, non-null average min{pr,rc}
sub fitness_nz_avg_min {
  my ($map,$cutoff) = @_;
  my $dc_dist_fit = $map->{dc_dist}->pdl;
  $dc_dist_fit->where($dc_dist_fit > $cutoff->slice("*1,")) += $map->{cutval};
  my $d2c_fit = $dc_dist_fit->xchg(0,1)->minimum_ind;
  ##
  ##-- $cc_hist : [$c_wanted,$c_got] -> $ndocs
  my $cc_hist_dims = zeroes(long,2) + $dc_dist_fit->dim(1);
  my $cc_hist_wnd  = $map->{d2c}->long->cat($d2c_fit)->xchg(0,1);
  my $cc_hist = PDL::CCS::Nd->newFromWhich($cc_hist_wnd, $d2c_fit->ones->long, dims=>$cc_hist_dims)->dummy(0,1)->sumover->decode;
  $map->{cc_hist} = $cc_hist; ##-- cache histogram
  ##
  my $tp = $cc_hist->diagonal(0,1)->double;        ##-- [$c] -> $tp
  my $n1 = $cc_hist->xchg(0,1)->sumover->double;   ##-- [$c] -> $nwanted = $tp + $fn
  my $n2 = $cc_hist->sumover->double;              ##-- [$c] -> $ngot    = $tp + $fp
  ##
  my $pr = $tp / $n2;
  my $rc = $tp / $n1;
  ##
  my $nz_mask = defined($map->{nz_mask}) ? $map->{nz_mask} : ($map->{nz_mask}=(($tp->xvals!=$map->{cutCatId}) & ($n1>0) & ($n2>0)));
  return
    $pr->cat($rc)->xchg(0,1)->minimum->where($nz_mask)->avg;
}


sub dbg_cchist {
  my ($map,$cc_hist) = @_;
  $cc_hist = $map->{cc_hist} if (!defined($cc_hist));
  ##
  my $tp = $cc_hist->diagonal(0,1)->double;        ##-- [$c] -> $tp
  my $n1 = $cc_hist->xchg(0,1)->sumover->double;   ##-- [$c] -> $nwanted = $tp + $fn
  my $n2 = $cc_hist->sumover->double;              ##-- [$c] -> $ngot    = $tp + $fp
  ##
  my $fp = $n2-$tp;
  my $fn = $n1-$tp;
  ##
  my $pr = $tp / $n2;
  my $rc = $tp / $n1;
  my $F  = 2.0 / ($pr**-1 + $rc**-1);
  ##
  my $vals = {tp=>$tp,fp=>$fp,fn=>$fn,pr=>$pr,rc=>$rc,F=>$F};
  ##
  my ($c,$ci,$ch);
  my $llen = 36;
  foreach $c (sort DocClassify::Utils::catcmp keys %{$map->{lcenum}{sym2id}}) {
    $ci = $map->{lcenum}{sym2id}{$c};
    print STDERR
      (sprintf("CUT: %-${llen}s : ", $c),
       join(' ', map {sprintf("$_=%4d",($vals->{$_}->at($ci)||0))} qw(tp fp fn)),
       ' : ',
       join(' ', map {sprintf("$_=%6.2f",(100*$vals->{$_}->at($ci)||0))} qw(pr rc F)),
       "\n",
      );
  }
}

##==============================================================================
## Methods: Dump

## $bool = $cutoff->dumpData($dump_basename, %opts)
sub dumpData {
  my ($map,$outbase,%opts) = @_;
  $outbase = '-' if (!defined($outbase));
  my ($fh);
  $map->vlog('info', "dumpData(outbase=$outbase)");

  ##-- dump: single-cat data
  $map->vlog('info', "dumpData(): $outbase.bycat.dat");
  $fh = $map->dumpFh($outbase,'.bycat.dat');
  my ($mu0,$sd0, $mu1,$sd1, $cut) = @$map{qw(c0dist_mu c0dist_sd c1dist_mu c1dist_sd cutoff)};
  #my $cutp0 = ($cut-$mu0)/$sd0;
  #my $cutp1 = ($cut-$mu1)/$sd1;
  my $NC = $cut->nelem;
  ##
  my $cchist = defined($map->{cc_hist}) ? $map->{cc_hist} : zeroes($NC,$NC);
  my $n1     = $cchist->xchg(0,1)->sumover;
  my $n2     = $cchist->sumover;
  my $tp     = $cchist->diagonal(0,1);
  my $fp     = $n2 - $tp;
  my $fn     = $n1 - $tp;
  ##
  $fh->print("#1:ID_LOCAL 2:ID_GUESS  3:MU0 4:SD0  5:MU1 6:SD1  7:CUTVAL  8:TP 9:FP 10:FN  11:CATNAME\n");
  my ($c,$ci,$cig,$cstr);
  foreach $ci (0..$#{$map->{lcenum}{id2sym}}) {
    $c = $map->{lcenum}{id2sym}[$ci];
    ($cstr = $c) =~ s/\s/_/g;
    $cig = $c =~ /^(\d+)/ ? $1 : -1;
    ##
    $fh->print(join(" ", $ci,$cig, (map {$_->at($ci)} ($mu0,$sd0, $mu1,$sd1, $cut, $tp,$fp,$fn)), $cstr), "\n");
  }
  $fh->close;

  ##-- dump: histogram
  $map->vlog('info', "dumpData(): $outbase.cchist.dat");
  $fh = $map->dumpFh($outbase,'.cchist.dat');
  $fh->print("#1:C1_WANTED  2:C2_GOT  3:N12  4:N1  5:N2\n");
  my ($c0);
  foreach $c0 (0..($NC-1)) {
    $fh->print((map { join("\t", $c0,$_,$cchist->at($c0,$_),$n1->at($c0),$n2->at($_))."\n" } (0..($NC-1))), "\n")
  }
  $fh->close;


  return $map;
}

## $outfh = $cutoff->dumpFh($dump_basename, $suffix)
sub dumpFh {
  my ($map,$outbase,$suffix) = @_;
  my $outfile = !defined($outbase) || $outbase eq '-' ? '-' : $outbase.$suffix;
  my $fh = IO::File->new(">$outfile") or $map->logdie("dumpFh(): could not open '$outfile' for write: $!");
  $fh->print("# ", __PACKAGE__, "->dumpFh(${outbase}${suffix})\n");
  return $fh;
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
## Footer
1;

__END__
