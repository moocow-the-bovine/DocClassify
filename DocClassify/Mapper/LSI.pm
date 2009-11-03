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
##  ##
##  ##-- data: post-compile()
##  disto => $distObj,               ##-- MUDL::Cluster::Distance object
##  svd => $svd,                     ##-- a MUDL::SVD object
##  xdm => $xdm_pdl,                 ##-- dense PDL ($svdr,$ND) = $svd->apply( $tdm_pdl )
##  xcm => $xcm_pdl,                 ##-- dense PDL ($svdr,$NC) = $svd->apply( $TERM_CAT_MATRIX($NT,$NC) )
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

			       ##-- data: post-compile
			       svd=>undef,
			       xdm=>undef,
			       xcm=>undef,
			       disto=>undef,

			       ##-- user args
			       @_,
			      );
  return $obj;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override appends qw(svd xdm xcm disto)
sub noShadowKeys {
  return ($_[0]->SUPER::noShadowKeys(@_[1..$#_]), qw(svd xdm xcm disto));
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

  ##-- create & compute $map->{svd}
  print STDERR ref($map)."::compile(): SVD (svdr=>$map->{svdr})\n" if ($map->{verbose});
  my $svd  = $map->{svd} = MUDL::SVD->new(r=>$map->{svdr});
  $svd->computeccs_nd($map->{tdm});
  $svd->shrink();
  print STDERR ref($map)."::compile(): SVD: auto-shrunk to r=$svd->{r}\n" if ($map->{verbose});
  $map->{svdr} = $svd->{r};

  ##-- create $map->{xdm} ~= $map->{svd}->apply($map->{tdm})
  my ($xdm);
  my ($NC,$ND) = map {$map->{$_}->size} qw(lcenum denum);
  print STDERR ref($map)."::compile(): matrix: xdm: (r=$map->{svdr} x ND=$ND) [R x Doc -> Sv]\n" if ($map->{verbose});
  $xdm = $map->{xdm} = $svd->{u};         ##-- we've already computed the bugger (duh!)

  ##-- get cat-profiling method
  my $catProfile = $map->{catProfile};
  $catProfile = 'average' if (!defined($catProfile));
  if ($catProfile =~ /^fold/) { $catProfile = 'fold-in'; }
  elsif ($catProfile =~ /^a/) { $catProfile = 'average'; }
  elsif ($catProfile =~ /^w/) { $catProfile = 'weighted-average'; }
  else {
    confess(ref($map)."::compile(): unknown category-profiling method '$catProfile'");
    return undef;
  }

  ##-- create $map->{xcm} ~= $map->{svd}->apply($map->{tcm})
  print STDERR ref($map)."::compile(): matrix: xcm: (r=$map->{svdr} x NC=$NC) [R x Cat -> Sv] : prof=$catProfile\n"
    if ($map->{verbose});

  if ($catProfile eq 'fold-in') {
    $map->{xcm} = $svd->apply($map->{tcm}->decode);
  }
  elsif ($catProfile eq 'average' || $catProfile eq 'weighted-average') {
    ##-- TODO: use MUDL::Cluster::Method::d2c_*() methods here!
    my $lc_sym2id = $map->{lcenum}{sym2id};
    my $xcm = $map->{xcm} = zeroes(double, $map->{svdr},$NC);
    my $doc_weight = $catProfile eq 'average' ? ones($ND) : $map->{tdm0}->sumover->decode;
    $doc_weight  /= $doc_weight->sumover;
    my ($doc,$d_id,$d_x,$c_id,$cat);
    foreach $doc (@{$map->{docs}}) {
      $d_id = $doc->{id};
      $d_x  = $doc_weight->index($d_id) * $xdm->slice(",$d_id");       ##-- [0,$di] -> $x
      foreach $cat (@{$doc->{cats}}) {
	$c_id = $lc_sym2id->{$cat->{name}};
	$xcm->slice(",$c_id") += $d_x;
      }
    }
  }

  ##-- create distance object
  print STDERR ref($map)."::compile(): disto: dist=$map->{dist}\n" if ($map->{verbose});
  $map->{disto} = MUDL::Cluster::Distance->new(class=>$map->{dist});

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

  ##-- dump distances to $doc->{cats}
  my $cname;
  @{$doc->{cats}} = map {
    $cname = $map->{lcenum}{id2sym}[$_];
    {id=>$map->{gcenum}{sym2id}{$cname}, name=>$cname, dist=>$cdmat->at($_,0)} #deg=>99
  } $cdmat->flat->qsorti->list;
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
  $fpdl = $map->sigPdlRaw($fpdl) if (!UNIVERSAL::isa($fpdl,'PDL')); ##-- $fpdl passed as doc?
  $fpdl = $fpdl->todense if (UNIVERSAL::isa($fpdl,'PDL::CCS::Nd')); ##-- avoid memory explosion in Nd::inner()
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
