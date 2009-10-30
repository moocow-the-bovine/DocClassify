## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::LSI.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document-to-class mapper: "latent semantic indexing" / SVD

package DocClassify::Mapper::LSI;
use DocClassify::Object;
use DocClassify::Mapper;
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

our @ISA = qw(DocClassify::Mapper);

our $verbose = 2;

##==============================================================================
## Constructors etc.

## $map = $CLASS_OR_OBJ->new(%opts)
## %$map, %opts:
##  ##-- options
##  lemmatize => \%opts,             ##-- options for $doc->typeSignature->lemmatize()
##  #relemmatize => $bool,            ##-- whether to implicitly re-lemmatize documents (defualt=false)
##  trainExclusive => $bool,         ##-- use each doc to train at most 1 cat? (default=true)
##  minFreq => $f,                   ##-- minimum global term-frequency (default=1)
##  smoothf => $f0,                  ##-- global frequency smoothing constant (undef~(NTypes/NTokens); default=1)
##  unkTerm => $str,                 ##-- unknown term string (default='__UNKNOWN__')
##  svdr => $svdr,                   ##-- number of reduced dimensions (default=64)
##  dist => $distSpec,               ##-- distance spec for MUDL::Cluster::Distance (default='u')
##                                   ##   + 'c'=Pearson, 'u'=Cosine, 'e'=Euclid, ...
##  catProfile => $how,              ##-- cate profiling method ('fold-in','average', 'weighted-average'...): default='average'
##  termWeight => $how,              ##-- term "weighting" method ('uniform', 'entropy'): default='uniform'
##  ##
##  ##-- data: enums
##  cenum => $catEnum,               ##-- category enum         ($NC=$catEnum->size())
##  tenum => $termEnum,              ##-- term enum             ($NT=$termEnum->size())
##  denum => $docEnum,               ##-- document (label) enum ($ND=$docEnum->size()=scalar(@docs))
##  ##
##  ##-- data: training
##  gf => \%global_tf,               ##-- global term-frequency hash
##  #c2sigs => \%cat2sigs,            ##-- maps category names to term-signatures
##  docs   => \@docs,                ##-- training docs, indexed by local $docid ($ND=$docEnum->size()=scalar(@docs))
##  sigs   => \@sigs,                ##-- training sigs, indexed by local $docid
##  ##
##  ##-- data: post-compile()
##  distf => $distObj,               ##-- MUDL::Cluster::Distance object
##  tw  => $tw_pdl,                  ##-- term-weight pdl: dense:       ($NT)    : [$ti]     -> w($ti)
##  tdm => $tdm_pdl,                 ##-- term-doc matrix: PDL::CCS::Nd ($NT,$ND): [$ti,$di] -> log(f($ti,$di)+$f0)*w($ti)
##  #tcm => $tcm_pdl,                ##-- term-cat matrix: PDL::CCS::Nd ($NT,$NC): [$ti,$ci] -> log(f($ti,$ci)+$f0)*w($ti)
##  dcm => $dcm_pdl,                 ##-- doc-cat matrix:  byte PDL     ($ND,$NC): [$di,$ci] -> deg($di \in $ci) || 0
##  svd => $svd,                     ##-- a MUDL::SVD object
##  xdm => $xdm_pdl,                 ##-- dense PDL ($svdr,$ND) = $svd->apply( $tdm_pdl )
##  xcm => $xcm_pdl,                 ##-- dense PDL ($svdr,$NC) = $svd->apply( $TERM_CAT_MATRIX($NT,$NC) )
sub new {
  my $that = shift;
  my $obj =  $that->SUPER::new(
			       ##-- options
			       lemmatize => {},
			       #relemmatize => 1,
			       trainExclusive => 1,
			       minFreq =>0,
			       smoothf =>1,
			       unkTerm => '__UNKNOWN__',
			       svdr => 64,
			       dist => 'u',
			       catProfile => 'average',
			       termWeight  => 'uniform',

			       ##-- data: enums
			       cenum => MUDL::Enum->new,
			       tenum => MUDL::Enum->new,
			       denum => MUDL::Enum->new,

			       ##-- data: training
			       gf => {},
			       #c2sigs => {},
			       docs => [],
			       sigs => [],

			       ##-- data: post-compile
			       tdm=>undef,
			       #tcm=>undef,
			       dcm=>undef,
			       svd=>undef,
			       xdm=>undef,
			       xcm=>undef,
			       distf=>undef,

			       ##-- user args
			       @_,
			      );
  return $obj;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(gf c2sigs denum docs dcm tdm tcm svd xdm xcm distf)
sub noShadowKeys {
  return qw(gf c2sigs denum docs sigs dcm tdm tcm svd xdm xcm tw distf);
}

##==============================================================================
## Methods: API: Training

## $map = $map->trainCorpus($corpus)
##  + add training data from $corpus
##  + inherited default just calls $map->trainDocument($doc) foreach doc in corpus

## $map = $map->trainDocument($doc)
##  + add training data from $doc
##  + calls $map->lemmaSignature($doc)
sub trainDocument {
  my ($map,$doc) = @_;
  print STDERR ref($map)."::trainDocument(".$doc->label.")\n" if ($verbose >= 2);
  my $sig = $map->lemmaSignature($doc);

  ##-- add sig frequency data to global hash
  $map->{gf}{$_} += $sig->{lf}{$_} foreach (keys(%{$sig->{lf}}));

  ##-- add sig to local category enum
  my $cats = $map->{trainExclusive} ? [$doc->cats->[0]] : $doc->cats;
  my ($cat);
  foreach $cat (@$cats) {
    next if (!defined($map->addCat(@$cat{qw(name id)})));
    #push(@{$map->{c2sigs}{$cat->{name}}}, $sig);
  }

  ##-- add shallow copy of doc, overriding $id
  $doc = $doc->shadow( %$doc, cats=>$cats, id=>$map->{denum}->addSymbol($doc->label) )->clearCache;
  $map->{docs}[$doc->{id}] = $doc;

  ##-- save reference to sig (for matrix construction)
  $map->{sigs}[$doc->{id}] = $sig;

  ##-- that's it for here...
  return $doc;
}

## $catId = $map->addCat($catName)
## $catId = $map->addCat($catName,$wantId)
##  + requests addition of a category $catName (with id $wantId) in $map->{cenum}
sub addCat {
  my ($map,$name,$id) = @_;
  return undef if (!defined($name));
  if (defined($id) && !defined($map->{cenum}{id2sym}[$id])) {
    ##-- accept the requested id
    return $map->{cenum}->addIndexedSymbol($name,$id);
  }
  ##-- maybe assign a new id
  return $map->{cenum}->addSymbol($name);
}

##==============================================================================
## Methods: API: Compilation

## $map = $map->compile()
##  + compile underlying map data
##  + should be called only after all training data have been added
sub compile {
  my $map = shift;

  ##-- trim global frequency hash
  print STDERR ref($map)."::compile(): trim(minFreq=>$map->{minFreq})\n" if ($verbose);
  delete(@{$map->{gf}}{ grep {$map->{gf}{$_} < $map->{minFreq}} keys(%{$map->{gf}}) }) if ($map->{minFreq}>0);

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

  ##-- get doc-weighting method
  my $termWeight = $map->{termWeight};
  $termWeight = 'uniform' if (!defined($termWeight));
  if ($termWeight =~ /^u/ || $termWeight =~ /^no/ || $termWeight =~ /^id/) { $termWeight='uniform'; }
  elsif ($termWeight =~ /^ent/ || $termWeight eq 'H') { $termWeight='entropy'; }
  else {
    confess(ref($map)."::compile(): unknown doc-weighting method '$termWeight'");
    return undef;
  }

  ##-- enums: $tenum: terms
  print STDERR ref($map)."::compile(): enum: TERMS\n" if ($verbose);
  my $tenum = $map->{tenum};
  $tenum->clear();
  @{$tenum->{id2sym}} = ($map->{unkTerm}, keys(%{$map->{gf}}));
  @{$tenum->{sym2id}}{@{$tenum->{id2sym}}} = (0..$#{$tenum->{id2sym}});
  my $NT = $tenum->size;

  ##-- enums: $cenum: categories (renumber)
  print STDERR ref($map)."::compile(): enum: CATS\n" if ($verbose);
  my $cenum = $map->{cenum};
  $cenum->compact(); ##-- renumber local categories (no missing rows!)
  #$cenum->clear();
  #$cenum->addSymbol($_) foreach (map {$_->{name}} map {@{$_->{cats}}} @{$map->{docs}});
  my $NC = $cenum->size;

  ##-- enums: $denum: documents (labels)
  print STDERR ref($map)."::compile(): enum: DOCS\n" if ($verbose);
  my $denum = $map->{denum};
  my $ND = $denum->size;

  ##-- matrix: doc-cat degree: $dcm: ($ND,$NC) -> deg($di \in $ci) || 0
  print STDERR ref($map)."::compile(): matrix: dcm: (ND=$ND x NC=$NC) [Doc x Cat -> Deg]\n" if ($verbose);
  my $dcm = $map->{dcm} = zeroes(byte, $ND,$NC);
  my ($doc,$cat);
  foreach $doc (@{$map->{docs}}) {
    foreach $cat (@{$doc->{cats}}) {
      $cat->{id} = $cenum->{sym2id}{$cat->{name}}; ##-- re-assign category IDs !
      $dcm->slice("($doc->{id}),($cat->{id})") .= ($cat->{deg} > 255 ? 255 : $cat->{deg});
    }
  }

  ##-- matrix: term-doc frequency: $tdm : [$tid,$did] => f($term[$tid],$doc[$did])
  print STDERR ref($map)."::compile(): matrix: tdm: (NT=$NT x ND=$ND) [Term x Doc -> Freq]\n" if ($verbose);
  my ($tdm_w,$tdm_nz) = (null,null);
  my ($d_name,$d_id);
  my ($c_name,$c_id,$c_deg);
  my ($sig, $sig_wt,$sig_w,$sig_nz);
  foreach $doc (grep {defined($_)} @{$map->{docs}}) {
    $d_id = $doc->{id};
    $sig = $map->{sigs}[$d_id];

    $sig_wt = pdl(long, grep {defined($_)} @{$tenum->{sym2id}}{keys(%{$sig->{lf}})}); ##-- [$nzi]   -> $ti : f($di,$ti) defined
    $sig_w  = $sig_wt->slice("*1,")->glue(0, zeroes(long,1,1)+$d_id);                 ##-- [*,$nzi] -> [$ti,$di]
    $sig_nz = pdl(double, @{$sig->{lf}}{@{$tenum->{id2sym}}[$sig_wt->list]});         ##-- [$nzi]   -> f($di,$ti)

    $tdm_w  = $tdm_w->glue(1,$sig_w);
    $tdm_nz = $tdm_nz->append($sig_nz);

    #delete($sig->{lf});	##-- frequency data all used up
    #$sig->unlemmatize();       ##-- lemma data all used up
  }
  my $tdm_dims = pdl(long,$tenum->size,$denum->size);
  my $tdm = PDL::CCS::Nd->newFromWhich($tdm_w,$tdm_nz,dims=>$tdm_dims,missing=>0); #->dummy(0,1)->sumover;

  ##-- smooth & log-transform term-doc matrix
  $map->{smoothf} = $NT/$tdm->sum if (!$map->{smoothf});
  print STDERR ref($map)."::compile(): smooth(smoothf=$map->{smoothf})\n" if ($verbose);
  my $tdm0 = $tdm; ##-- save for folding-in, term-weighting, etc.
  $tdm = $map->{tdm} = ($tdm+$map->{smoothf})->inplace->log;

  ##-- term-weighting
  print STDERR ref($map)."::compile(): termWeight($termWeight)\n" if ($verbose);
  my ($tw);
  if ($termWeight eq 'uniform') {
    $tw = ones($NT);                                ##-- identity weighting
  }
  elsif ($termWeight eq 'entropy') {
    ##-- weight terms by doc entropy (see e.g. Berry(1995); Nakov, Popova, & Mateev (2001))
    my $t_f     = $tdm0->xchg(0,1)->sumover;        ##-- ccs: [$ti] -> f($ti)
    my $td_pdgt = ($tdm0 / $t_f)->_missing(0);      ##-- ccs: [$ti,$di] -> p($di|$ti)
    $tw         = $td_pdgt->log->_missing(0);       ##-- ccs: [$ti,$ti] -> ln p($di|$ti)
    $tw        /= log(2);                           ##                  -> log p($di|$ti)
    $tw        *= $td_pdgt;                         ##                  -> p($di|$ti) * log p($di|$ti)
    $tw         = $tw->xchg(0,1)->sumover;          ##-- pdl: [$ti] -> -H(Doc|$ti)
    $tw        /= log($ND)/log(2);                  ##-- pdl: [$ti] -> -H(Doc|$ti)/H(Doc)
    $tw        += 1;
  }
  $tdm = $map->{tdm} = $tdm*$tw;
  $map->{tw}  = $tw->decode; ##-- store term weights

  ##-- clear expensive perl signature structs
  @{$map->{sigs}} = qw();
  #@{$map->{docs}} = qw();

  ##-- create $map->{tcm} ($NT,$NC) : total (cat,word) frequency (for catProfile='fold-in')
  my ($tcm);
  if ($catProfile eq 'fold-in') {
    print STDERR ref($map)."::compile(): tcm: (NT=$NT x NC=$NC) [Term x Cat -> Freq]\n" if ($verbose);
    my ($tcm_w,$tcm_nz) = (null,null);
    my ($d_tf,$d_wt,$d_w,$d_nz, $c_w);
    foreach $doc (@{$map->{docs}}) {
      $d_id = $doc->{id};
      $d_tf = $tdm0->dice_axis(1,pdl(long,$d_id));
      $d_wt = $d_tf->_whichND->slice("(0),");
      $d_nz = $d_tf->_nzvals;
      foreach $cat (@{$doc->{cats}}) {
	$c_id = $cat->{id};
	$c_w  = $d_wt->slice("*1,")->glue(0, zeroes(long,1,1)+$c_id); ##-- [*,$nzi] -> [$ti,$ci]
	##
	$tcm_w  = $tcm_w->glue(1,$c_w);
	$tcm_nz = $tcm_nz->append($d_nz);
      }
    }
    my $tcm_dims = pdl(long,$NT,$NC);
    $tcm = PDL::CCS::Nd->newFromWhich($tcm_w,$tcm_nz,dims=>$tcm_dims,missing=>0)->dummy(0,1)->sumover;
    $tcm = $map->{tcm} = $tcm = ($tcm+$map->{smoothf})->inplace->log  * $tw;
  }

  ##-- create & compute $map->{svd}
  print STDERR ref($map)."::compile(): SVD (svdr=>$map->{svdr})\n" if ($verbose);
  my $svd  = $map->{svd} = MUDL::SVD->new(r=>$map->{svdr});
  $svd->computeccs_nd($tdm);
  ##
  ##-- auto-shrink SVD
  $svd->shrink();
  print STDERR ref($map)."::compile(): SVD: auto-shrunk to r=$svd->{r}\n" if ($verbose);
  $map->{svdr} = $svd->{r};


  ##-- create $map->{xdm} ~= $map->{svd}->apply($map->{tdm})
  ##   ::: EXPECT MEMORY CRASH HERE (decoded PDL (NT=50K x ND=113) @ 43MB) :::
  print STDERR ref($map)."::compile(): matrix: xdm: (r=$map->{svdr} x ND=$ND) [R x Doc -> Sv]\n" if ($verbose);
  my $xdm = $map->{xdm} = $svd->apply($tdm->decode);

  ##-- create $map->{xcm} ~= $map->{svd}->apply($map->{tcm})
  print STDERR ref($map)."::compile(): matrix: xcm: (r=$map->{svdr} x NC=$NC) [R x Cat -> Sv] : prof=$catProfile\n" if ($verbose);
  if ($catProfile eq 'fold-in') {
    $map->{xcm} = $svd->apply($tcm->decode);
  }
  elsif ($catProfile eq 'average' || $catProfile eq 'weighted-average') {
    my $xcm = $map->{xcm} = zeroes(double, $map->{svdr},$NC);
    my ($d_x);
    my $doc_weight = $catProfile eq 'average' ? ones($ND) : $tdm0->sumover->decode;
    $doc_weight  /= $doc_weight->sumover;
    foreach $doc (@{$map->{docs}}) {
      $d_id = $doc->{id};
      $d_x  = $doc_weight->index($d_id) * $xdm->slice(",$d_id");       ##-- [0,$di] -> $x
      foreach $cat (@{$doc->{cats}}) {
	$c_id = $cat->{id};
	$xcm->slice(",$c_id") += $d_x;
      }
    }
  }

  ##-- create distance object
  $map->{disto} = MUDL::Cluster::Distance->new(class=>$map->{dist});

  return $map;
}


## $bool = $map->compiled()
##  + returns true iff $map has been compiled
##  + override checks for $map->{xcf}
sub compiled { return defined($_[0]{xcf}); }

## $map = $map->clearTrainingCache()
##  + clears any cached data from training
##  + after calling this, $map may no longer be able to train
##  + override clears training data @$map{qw(gf c2sigs)}
sub clearTrainingCache {
  my $map = shift;
  %{$map->{gf}} = qw();
  #%{$map->{c2sigs}} = qw();
  #@{$map->{sigs}} = qw();
  #@{$map->{docs}} = qw(); ##-- still needed for category mapping?
  #delete($map->{tdm});    ##-- still useful for debugging
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
  print STDERR ref($map)."::mapDocument(".$doc->label.")\n" if ($verbose>=2);

  ##-- sanity check(s)
  confess(ref($map)."::mapDocument(): no feature-category matrix 'xcm'!") if (!defined($map->{xcm}));

  ##-- get doc pdl
  my $tdm = $map->docPdlRaw($doc);
  my $tdN = $tdm->sum;
  warn(ref($map)."::mapDocument(): null vector for document '$doc->{label}'") if ($verbose && $tdN==0);

  ##-- compute distance to each centroid
  my $xdm   = $map->svdApply($tdm);
  my $cdmat = $map->{disto}->clusterDistanceMatrix(data=>$xdm,cdata=>$map->{xcm});

  ##-- dump distances to $doc->{cats}
  @{$doc->{cats}} = map {
    {id=>$_,name=>$map->{cenum}{id2sym}[$_],dist=>$cdmat->at($_,0)} #deg=>99
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
  return $map->{svd}->apply($fpdl);
}

## $sig = $map->lemmaSignature($doc_or_sig)
##  + wrapper for $doc->termSignature->lemmatize() with options %{$map->{lemmatize}}
sub lemmaSignature {
  my ($map,$sig) = @_;
  $sig = $sig->typeSignature if (!UNIVERSAL::isa($sig,'DocClassify::Signature'));
  $sig = $sig->lemmatize( %{$map->{lemmatize}} ) if (!$sig->lemmatized);
  return $sig;
}

## $fpdl = $map->docPdlRaw($doc, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tidl,1]=>f($tid,$doc)
sub docPdlRaw {
  return $_[0]->sigPdlRaw(@_[1..$#_]);
}

## $fpdl = $map->sigPdlRaw($sig, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tid,1]=>f($tid,$sig)
sub sigPdlRaw {
  my ($map,$sig, $as_ccs) = @_;
  $sig = $map->lemmaSignature($sig); ##-- ensure lemmatized signature
  my $tenum = $map->{tenum};
  my $dtf_wt = pdl(long,   grep{defined($_)} @{$tenum->{sym2id}}{keys(%{$sig->{lf}})});
  my $dtf_nz = pdl(double, @{$sig->{lf}}{@{$tenum->{id2sym}}[$dtf_wt->list]});
  if ($as_ccs) {
    ##-- ccs mode
    return PDL::CCS::Nd->newFromWhich($dtf_wt->slice("*1,"),$dtf_nz,dims=>pdl(long,[$tenum->size]),missing=>0)->dummy(1,1);
  }
  ##-- dense mode
  my $dtf = zeroes(double,$tenum->size);
  $dtf->index($dtf_wt) .= $dtf_nz;
  return $dtf->slice(",*1");
}

##==============================================================================
## Methods: API: I/O
##  + see DocClassify::Object


##==============================================================================
## Footer
1;

__END__
