## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::LSI.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document-to-class mapper: "latent semantic indexing" / SVD

package DocClassify::Mapper::LSI;
use DocClassify::Object;
use DocClassify::Mapper;
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
##  svdr => $svdr,                   ##-- number of reduced dimensions (default=128)
##  dist => $distSpec,               ##-- distance spec for MUDL::Cluster::Distance (default='u')
##                                   ##   + 'c'=Pearson, 'u'=Cosine, 'e'=Euclid, ...
##  catProfile => $how,              ##-- cate profiling method ('fold-in','average', 'weighted-average'...): default='average'
##  xn => $xn,                       ##-- number of splits for compile-time cross-check (0 for none; default=3)
##  seed => $seed,                   ##-- random seed for corpus splitting (undef (default) for none)
##  ##
##  ##-- data: post-compile()
##  disto => $distObj,               ##-- MUDL::Cluster::Distance object
##  svd => $svd,                     ##-- a MUDL::SVD object
##  xdm => $xdm_pdl,                 ##-- dense PDL ($svdr,$ND) = $svd->apply( $tdm_pdl )
##  xcm => $xcm_pdl,                 ##-- dense PDL ($svdr,$NC) = $svd->apply( $TERM_CAT_MATRIX($NT,$NC) )
##  cdist_mu => $cdist_mu,           ##-- dense PDL ($NC) : [$ci] ->    avg { dist($ci,$di) : $di \in $ci }
##  cdist_sd => $cdist_sd,           ##-- dense PDL ($NC) : [$ci] -> stddev { dist($ci,$di) : $di \in $ci }
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
			       svdr => 128,
			       dist => 'u',
			       catProfile => 'average',
			       termWeight  => 'entropy',
			       xn => 3,
			       seed => undef,

			       ##-- data: post-compile
			       svd=>undef,
			       xdm=>undef,
			       xcm=>undef,
			       disto=>undef,
			       cdist_mu=>undef,
			       cdist_sd=>undef,

			       ##-- user args
			       @_,
			      );
  return $obj;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override appends qw(svd xdm xcm disto cdist_mu cdist_sd)
sub noShadowKeys {
  return ($_[0]->SUPER::noShadowKeys(@_[1..$#_]), qw(svd xdm xcm disto cdist_mu cdist_sd));
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

## $map = $map->compile()
##  + compile underlying map data
##  + should be called only after all training data have been added
sub compile {
  my $map = shift;

  ##-- inherited compilation
  $map->SUPER::compile() or confess(ref($map)."::compile() inherited compilation failed: $!");

  ##-- clear expensive perl signature structs
  @{$map->{sigs}} = qw();
  #@{$map->{docs}} = qw();

  ##-- training-internal cross-check
  $map->compileCrossCheck();

  ##-- local compilation (final)
  $map->compileLocal(label=>'FINAL', svdShrink=>1);
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
  return $map;
}

##--------------------------------------------------------------
## Methods: Compilation: Utils
##  + see also Mapper::ByLemma

## $map = $map->compileCrossCheck()
##  + does cross-checking and parameter extraction for training data
sub compileCrossCheck {
  my $map = shift;
  my $xcn = $map->{xn};

  if (!$xcn || $xcn<3) {
    warn(ref($map)."::compileCrossCheck(): cross-validation disabled (xn=".($xcn||0).")");
    return $map;
  }

  ##-- cross-check: generate test corpora
  my $corpus = DocClassify::Corpus->new(docs=>$map->{docs});
  my @subcs  = $corpus->splitN($xcn, seed=>$map->{seed}, exclusive=>$map->{trainExclusive}, label=>"XCHECK.%d/$xcn");

  ##-- cross-check: map subcorpora to document index pdls
  confess(ref($map)."::compileCrossCheck(): can't handle non-exclusive training mode!") if (!$map->{trainExclusive});
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
  my ($xci,$xclabel, $map2, $docids_local,$docids_other);
  my ($od_did,$od_tdm,$od_xdm,$od_cdmat);
  foreach $xci (1..$xcn) {
    ##-- create & compile subset mapper
    $xclabel = "XCHECK ($xci/$xcn)";
    print STDERR ref($map)."::compileCrossCheck(): [$xclabel]: TRAIN\n" if ($map->{verbose});
    ($docids_local,$docids_other) = which_both($d2subc==($xci-1));
    if ($docids_local->isempty) {
      warn(ref($map)."::compileCrossCheck(): [$xclabel]: TRAIN: empty subcorpus: skipping!\n");
      next;
    }
    $map2   = $map->docSubset($docids_local);
    $map2->compileLocal(label=>$xclabel);

    ##-- map left-out ("other") documents
    #$map2->{verbose} = 0;
    print STDERR ref($map)."::compileCrossCheck(): [$xclabel]: MAP\n" if ($map->{verbose});
    foreach $od_did ($docids_other->list) {
      print STDERR ref($map)."::compileCrossCheck(): [$xclabel]: MAP: DOC(".$map->{docs}[$od_did]{label}.")\n"
	if ($map->{verbose}>=2);
      $od_tdm = $map->{tdm}->dice_axis(1,$od_did);
      $od_xdm = $map2->svdApply($od_tdm);
      $od_cdmat = $map2->{disto}->clusterDistanceMatrix(data=>$od_xdm,cdata=>$map2->{xcm});
      $dc_dist->slice("($od_did),") .= $od_cdmat->flat;
    }
  }

  ##-- DEBUG
  $map->{dc_dist} = $dc_dist;

  ##-- get positive evidence
  my $dc_which1 = sequence($ND)->cat($d2c)->xchg(0,1);  ##-- [$di]     -> [$di,$ci] : $di \in $ci
  my $dc_mask   = $dc_dist->zeroes->byte;               ##-- [$di,$ci] -> 1($di \in     $ci)
  $dc_mask->indexND($dc_which1) .= 1;
  my $dc_which0 = whichND(!$dc_mask);                   ##-- [$di,$ci] -> 1($di \not\in $ci)
  my $d_dist1   = $dc_dist->indexND($dc_which1);        ##-- [$di]     ->     dist($di,$ci) : $di \in $ci
  my $d_dist0   =                                       ##-- [$di]     -> avg dist($di,$ci) : $di \not\in $ci
    PDL::CCS::Nd->newFromWhich($dc_which0,$dc_dist->indexND($dc_which0))->xchg(0,1)->average_nz->decode;

  return $map;
}

sub argh {
  no strict;
  ##-- get normal fit parameters
  print STDERR ref($map)."::compileCrossCheck(): FIT\n" if ($map->{verbose});
  my $dcdist_w = sequence($ND)->cat($d2c)->xchg(0,1); ##-- [$di,$ci] -> dist($ci,$di) : $di \in $ci
  my $dcdist_v = $d_dist;
  my $dcdist   = PDL::CCS::Nd->newFromWhich($dcdist_w,$dcdist_v,missing=>0);
  ##
  ## $cdist_mu: dense: [$ci] ->    avg { dist($ci,$di) : $di \in $ci }
  ## $cdist_sd: dense: [$ci] -> stddev { dist($ci,$di) : $di \in $ci }
  my $cdist_mu = $dcdist->average_nz->decode;
  my $cdist_sd = (($dcdist - $cdist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  my $sd_eps   = 1e-30;
  #$cdist_sd->where( (!$cdist_sd->isfinite) | ($cdist_sd==0) ) .= $sd_eps; ##-- ARGH!
  if ($map->{verbose}) {
    print STDERR
      (ref($map)."compileCrossCheck(): c_mu = $cdist_mu\n",
       ref($map)."compileCrossCheck(): c_sd = $cdist_sd\n",
      );
  }

  ##-- store fit parameters
  $map->{dcdist}   = $dcdist;   ##-- DEBUG
  $map->{cdist_mu} = $cdist_mu;
  $map->{cdist_sd} = $cdist_sd;

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
##  + compiles local map params qw(svd xdm xcm disto)
##  + %opts: passed to compile_(svd|xdm|xcm|disto), e.g.
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##     svdShrink => $bool,   ##-- whether to auto-shrink svd (default=false)
sub compileLocal {
  my ($map,%opts) = @_;

  ##-- svd: $map->{svd}
  $map->compile_svd(%opts);

  ##-- matrix: $map->{xdm}: (R=$map->{svdr} x ND) [R x Doc -> X]
  $map->compile_xdm(%opts);

  ##-- matrix: $map->{xcm}: (R=$map->{svdr} x NC=$NC) [R x Cat -> Sv]
  $map->compile_xcm(%opts);

  ##-- create distance object
  $map->compile_disto(%opts);

  return $map;
}

## $labstr = $map->labelString(%opts)
##  + gets symbolic label for verbose messages
##  + %opts:
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
sub labelString {
  my ($map,%opts) = @_;
  return $opts{label} || $map->{label} || '';
}

## $map = $map->compile_svd(%opts)
##  + compiles $map->{svd} from $map->{tdm}
##  + %opts:
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##     svdShrink => $bool,   ##-- whether to auto-shrink svd (default=false)
sub compile_svd {
  my ($map,%opts) = @_;
  my $label = $map->labelString(%opts);

  print STDERR ref($map)."::compile_svd() [$label]: SVD (svdr=>$map->{svdr})\n" if ($map->{verbose});
  my $svd  = $map->{svd} = MUDL::SVD->new(r=>$map->{svdr});
  $svd->computeccs_nd($map->{tdm});
  if ($opts{svdShrink}) {
    $svd->shrink();
    print STDERR ref($map)."::compile_svd() [$label]: SVD: auto-shrunk to r=$svd->{r}\n" if ($map->{verbose});
    $map->{svdr} = $svd->{r};
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
  print STDERR ref($map)."::compile_xdm() [$label]: matrix: xdm: (r=$map->{svdr} x ND=$ND) [R x Doc -> Sv]\n"
    if ($map->{verbose});
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
    confess(ref($map)."::catProfileMethod(): unknown category-profiling method option '$catProfile'");
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
  print STDERR ref($map)."::compile_xcm() [$label]: matrix: xcm: (r=$map->{svdr} x NC=$NC) [R x Cat -> Sv] : prof=$catProfile\n";
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

## $map = $map->compile_disto(%opts)
##  + compiles distance object $map->{disto} from symbolic spec $map->{dist}
##  + %opts:
##     label  => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
sub compile_disto {
  my ($map,%opts) = @_;
  my $label = $map->labelString(%opts);
  print STDERR ref($map)."::compile_disto() [$label]: disto: dist=$map->{dist}\n" if ($map->{verbose});
  $map->{disto} = MUDL::Cluster::Distance->new(class=>$map->{dist});
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
  print STDERR ref($map)."::mapDocument(".$doc->label.")\n" if ($map->{verbose}>=3);

  ##-- sanity check(s)
  confess(ref($map)."::mapDocument(): no feature-category matrix 'xcm'!") if (!defined($map->{xcm}));

  ##-- get doc pdl
  my $tdm = $map->docPdlRaw($doc);
  my $tdN = $tdm->sum;
  warn(ref($map)."::mapDocument(): null vector for document '$doc->{label}'") if ($map->{verbose} && $tdN==0);

  ##-- compute distance to each centroid
  my $xdm   = $map->svdApply($tdm);
  my $cdmat = $map->{disto}->clusterDistanceMatrix(data=>$xdm,cdata=>$map->{xcm});
  my $cdmat_cdf = defined($map->{cdist_mu}) ? gausscdf($cdmat, $map->{cdist_mu}, $map->{cdist_sd}) : $cdmat;

  ##-- dump distances to $doc->{cats}
  my $cname;
  @{$doc->{cats}} = map {
    $cname = $map->{lcenum}{id2sym}[$_];
    {id=>$map->{gcenum}{sym2id}{$cname}, name=>$cname, dist=>$cdmat_cdf->at($_,0), dist_raw=>$cdmat->at($_,0)}
  } $cdmat_cdf->flat->qsorti->list;
  #$doc->{cats}[0]{deg} = 1;
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
