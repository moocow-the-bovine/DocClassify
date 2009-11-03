## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::ByLemma.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document-to-class mapper: by lemma

package DocClassify::Mapper::ByLemma;
use DocClassify::Object;
use DocClassify::Mapper;
use DocClassify::Utils ':all';

use MUDL::Enum;
#use MUDL::SVD;
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


#our $verbose = 2;
our $verbose = 3;

##==============================================================================
## Constructors etc.

## $map = $CLASS_OR_OBJ->new(%opts)
## %$map, %opts:
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
##  dcm => $dcm_pdl,                 ##-- doc-cat matrix:  byte PDL     ($ND,$NC): [$di,$ci] -> deg($di \in $ci) || 0
##  tdm0=> $tdm0_pdl,                ##-- raw term-doc mx: PDL::CCS::Nd ($NT,$ND): [$ti,$di] ->     f($ti,$di)
##  tcm0=> $tcm0_pdl,                ##-- raw term-cat mx: PDL::CCS::Nd ($NT,$NC): [$ti,$ci] ->     f($ti,$ci)
##  tw  => $tw_pdl,                  ##-- term-weight pdl: dense:       ($NT)    : [$ti]     -> w($ti)
##  tdm => $tdm_pdl,                 ##-- term-doc matrix: PDL::CCS::Nd ($NT,$ND): [$ti,$di] -> log(f($ti,$di)+$f0)*w($ti)
##  tcm => $tcm_pdl,                 ##-- term-cat matrix: PDL::CCS::Nd ($NT,$NC): [$ti,$ci] -> log(f($ti,$ci)+$f0)*w($ti)
sub new {
  my $that = shift;
  my $obj =  $that->SUPER::new(
			       ##-- options
			       verbose=>$verbose,
			       lemmatize => {},
			       trainExclusive => 1,
			       minFreq =>0,
			       minDocFreq =>0,
			       smoothf =>1,
			       termWeight  => 'entropy',

			       ##-- data: enums
			       lcenum => MUDL::Enum->new,
			       gcenum => MUDL::Enum->new,
			       tenum => MUDL::Enum->new,
			       denum => MUDL::Enum->new,

			       ##-- data: training
			       gf => {},
			       df => {},
			       docs => [],
			       sigs => [],

			       ##-- data: post-compile
			       dcm=>undef,
			       tw=>undef,
			       tdm0=>undef,
			       tcm0=>undef,
			       tdm=>undef,
			       tcm=>undef,

			       ##-- user args
			       @_,
			      );
  return $obj;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(gf df lcenum denum tenum docs sigs dcm tw tdm0 tcm0 tdm tcm)
sub noShadowKeys {
  return qw(gf df clenum denum tenum docs sigs dcm tw tdm0 tcm0 tdm tcm);
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
  print STDERR ref($map)."::trainDocument(".$doc->label.")\n" if ($map->{verbose} >= 3);
  my $sig = $map->lemmaSignature($doc);

  ##-- add sig frequency data to global hash(es)
  my ($gf,$df) = @$map{qw(gf df)};
  my ($t,$f);
  while (($t,$f)=each(%{$sig->{lf}})) {
    $gf->{$t} += $f;
    $df->{$t}++;
  }

  ##-- add sig to local category enum
  my $cats = $map->{trainExclusive} ? [$doc->cats->[0]] : $doc->cats;
  my ($cat);
  foreach $cat (@$cats) {
    next if (!defined($map->addCat(@$cat{qw(name id)})));
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
  if (defined($id) && !defined($map->{gcenum}{id2sym}[$id])) {
    ##-- accept the requested id
    return $map->{gcenum}->addIndexedSymbol($name,$id);
  }
  ##-- maybe assign a new id
  return $map->{gcenum}->addSymbol($name);
}

##==============================================================================
## Methods: API: Compilation

## $map = $map->compile()
##  + compile underlying map data
##  + should be called only after all training data have been added
sub compile {
  my $map = shift;

  ##-- frequency-trimming
  $map->compileTrim();

  ##-- enums
  $map->compileCatEnum();
  $map->compileDocEnum();
  $map->compileTermEnum();
  #my ($lcenum,$gcenum,$denum,$tenum) = @$map{qw(lcenum gcenum denum tenum)};
  #my ($NC,$NCg,$ND,$NT) = map {$_->size} @$map{qw(lcenum gcenum denum tenum)};

  ##-- matrix: doc-cat-degree: $dcm: ($ND,$NC) -> deg($di \in $ci) || 0
  $map->compile_dcm();

  ##-- matrix: term-doc frequency: $tdm0 : [$tid,$did] => f($term[$tid],$doc[$did])
  $map->compile_tdm0();

  ##-- smooth & log-transform term-doc matrix
  $map->compile_tdm_log();

  ##-- compile & apply term weights
  $map->compile_tw();
  $map->{tdm} = $map->{tdm} * $map->{tw};

  ##-- clear expensive perl signature structs (we've outgrown them)
  #@{$map->{sigs}} = qw();
  #@{$map->{docs}} = qw();

  ##-- matrix: $map->{tcm0}: ($NT,$NC) : CCS::Nd: [Term x Cat -> Freq]
  $map->compile_tcm0();

  ##-- matrix: $map->{tcm}: ($NT,$NC) : CCS::Nd: [Term x Cat -> WeightedLogFreq]
  $map->{tcm} = ($map->{tcm0}+$map->{smoothf})->inplace->log*$map->{tw};

  return $map;
}

## $bool = $map->compiled()
##  + returns true iff $map has been compiled
##  + override checks for $map->{tcm}
sub compiled { return defined($_[0]{tcm}); }

## $map = $map->clearTrainingCache()
##  + clears any cached data from training
##  + after calling this, $map may no longer be able to train
##  + override clears training data @$map{qw(gf df sigs)} #c2sigs
sub clearTrainingCache {
  my $map = shift;
  %{$map->{gf}} = qw();
  %{$map->{df}} = qw();
  @{$map->{sigs}} = qw();
  #@{$map->{docs}} = qw(); ##-- still needed for category mapping?
  delete($map->{tdm0});   ##-- still useful for debugging
  delete($map->{tcm0});   ##-- still useful for debugging
  return $map;
}

##--------------------------------------------------------------
## Methods: Compilation: Utils

##----------------------------------------------
## $map = $map->compileTrim()
##  + trims @$map{qw(gf df)} by specified freqs
sub compileTrim {
  my $map = shift;

  ##-- trim by global term frequency
  print STDERR ref($map)."::compileTrim(): by global term freqency: minFreq=$map->{minFreq}\n" if ($map->{verbose});
  delete(@{$map->{gf}}{ grep {$map->{gf}{$_} < $map->{minFreq}} keys(%{$map->{gf}}) }) if ($map->{minFreq}>0);

  ##-- trim by doc "frequency"
  print STDERR ref($map)."::compileTrim(): by document term frequency: minDocFreq=$map->{minDocFreq}\n" if ($map->{verbose});
  delete(@{$map->{gf}}{ grep {$map->{df}{$_} < $map->{minDocFreq}} keys(%{$map->{gf}}) }) if ($map->{minDocFreq}>0);
  delete(@{$map->{df}}{ grep {!exists($map->{gf}{$_})} keys(%{$map->{df}}) });

  return $map;
}

##----------------------------------------------
## $map = $map->compileTermEnum()
##  + compiles $map->{tenum} from keys(%{$map->{gf}})
sub compileTermEnum {
  my $map = shift;
  print STDERR ref($map)."::compileTermEnum()\n" if ($map->{verbose});
  my $tenum = $map->{tenum};
  $tenum->clear();
  #@{$tenum->{id2sym}} = ($map->{unkTerm}, keys(%{$map->{gf}}));
  @{$tenum->{id2sym}} = (keys(%{$map->{gf}}));
  @{$tenum->{sym2id}}{@{$tenum->{id2sym}}} = (0..$#{$tenum->{id2sym}});
  return $map;
}

##----------------------------------------------
## $map = $map->compileCatEnum()
##  + compiles $map->{lcenum} from $map->{gcenum}
sub compileCatEnum {
  my $map = shift;
  print STDERR ref($map)."::compileCatEnum()\n" if ($map->{verbose});
  $map->{lcenum} = $map->{gcenum}->clone;
  $map->{lcenum}->compact; ##-- renumber local categories (no missing rows!)
  return $map;
}

##----------------------------------------------
## $map = $map->compileDocEnum()
##  + compile $map->{denum} (dummy placeholder; should already have happened in trainDocument())
sub compileDocEnum {
  my $map = shift;
  #print STDERR ref($map)."::compileDocEnum()\n" if ($map->{verbose});
  # should already have happened in $map->trainDocument()
  return $map;
}

##----------------------------------------------
## $map = $map->compile_dcm()
##  + compiles matrix $map->{dcm}: ($ND,$NC) -> deg($di \in $ci) || 0
sub compile_dcm {
  my $map = shift;
  my $lcenum = $map->{lcenum};
  my ($ND,$NC) = map {$_->size} @$map{qw(denum lcenum)};

  print STDERR ref($map)."::compile_dcm(): matrix: dcm: (ND=$ND x NC=$NC) [Doc x Cat -> Deg]\n" if ($map->{verbose});
  my $dcm = $map->{dcm} = zeroes(byte, $ND,$NC);
  my ($doc,$cat);
  foreach $doc (@{$map->{docs}}) {
    foreach $cat (@{$doc->{cats}}) {
      $cat->{id} = $lcenum->{sym2id}{$cat->{name}}; ##-- re-assign category IDs !
      $dcm->slice("($doc->{id}),($cat->{id})") .= ($cat->{deg} > 255 ? 255 : $cat->{deg});
    }
  }
  return $map;
}

##----------------------------------------------
## $map = $map->compile_tdm0()
##  + compiles raw term-doc frequency matrix: $map->{tdm_raw} : [$tid,$did] => f($term[$tid],$doc[$did])
BEGIN { *compile_tdm0 = \&compile_tdm0_v1; }
sub compile_tdm0_v1 {
  my $map = shift;

  ##-- vars
  my ($denum,$tenum) = @$map{qw(denum tenum)};
  my $NT = $tenum->size;
  my $ND = $denum->size;

  print STDERR ref($map)."::compile_tdm0(): matrix: tdm0: (NT=$NT x ND=$ND) [Term x Doc -> Freq]\n" if ($map->{verbose});
  my ($tdm_w,$tdm_nz) = (null,null);
  my ($d_name,$d_id);
  my ($c_name,$c_id,$c_deg);
  my ($doc,$sig, $sig_wt,$sig_w,$sig_nz);
  foreach $doc (grep {defined($_)} @{$map->{docs}}) {
    $d_id = $doc->{id};
    $sig = $map->{sigs}[$d_id];

    $sig_wt = pdl(long, [grep {defined($_)} @{$tenum->{sym2id}}{keys(%{$sig->{lf}})}]); ##-- [$nzi]   -> $ti : f($di,$ti) defined
    next if ($sig_wt->isempty); ##-- sanity check
    $sig_w  = $sig_wt->slice("*1,")->glue(0, zeroes(long,1,1)+$d_id);                   ##-- [*,$nzi] -> [$ti,$di]
    $sig_nz = pdl(double, [@{$sig->{lf}}{@{$tenum->{id2sym}}[$sig_wt->list]}]);         ##-- [$nzi]   -> f($di,$ti)

    $tdm_w  = $tdm_w->glue(1,$sig_w);
    $tdm_nz = $tdm_nz->append($sig_nz);

    #delete($sig->{lf});	##-- frequency data all used up
    #$sig->unlemmatize();       ##-- lemma data all used up
  }
  my $tdm_dims = pdl(long,$tenum->size,$denum->size);
  my $tdm0 = $map->{tdm0} = PDL::CCS::Nd->newFromWhich($tdm_w,$tdm_nz,dims=>$tdm_dims,missing=>0); #->dummy(0,1)->sumover;
  return $map;
}

##----------------------------------------------
## $map = $map->compile_tdm_log()
##  + smooths & logs raw term-doc frequency matrix: $map->{tdm} : [$tid,$did] => log(f($term[$tid],$doc[$did])+1)
##  + computes from $map->{tdm0}
sub compile_tdm_log {
  my $map = shift;

  ##-- smooth & log-transform term-doc matrix
  $map->{smoothf} = $map->{tenum}->size/$map->{tdm0}->sum if (!$map->{smoothf});
  print STDERR ref($map)."::compile_tdm_log(): smooth(smoothf=$map->{smoothf})\n" if ($map->{verbose});
  $map->{tdm} = ($map->{tdm0}+$map->{smoothf})->inplace->log;

  return $map;
}

##----------------------------------------------
## $twMethod = $map->termWeightMethod()
##  + gets $map->{termWeight}, does some sanity checks & canonicalization
sub termWeightMethod {
  my $map = shift;
  my $termWeight = $map->{termWeight};
  $termWeight = 'uniform' if (!defined($termWeight));

  if ($termWeight =~ /^u/ || $termWeight =~ /^no/ || $termWeight =~ /^id/) {
    $termWeight='uniform';
  }
  elsif ($termWeight =~ /^ent/ || $termWeight eq 'H') {
    $termWeight='entropy';
  }
  else {
    confess(ref($map)."::compile(): unknown term-weighting method '$termWeight'");
  }

  return $map->{termWeight}=$termWeight;
}

##----------------------------------------------
## $map = $map->compile_tw()
##  + compiles term-weight vector $map->{tw}: ($NT): [$tid] -> weight($term[$tid])
##  + does NOT apply weights to $map->{tdm} -- do that yourself!
sub compile_tw {
  my $map = shift;

  ##-- vars
  my $termWeight = $map->termWeightMethod;
  my $NT = $map->{tenum}->size;
  my $ND = $map->{denum}->size;

  ##-- guts
  print STDERR ref($map)."::compile_tw(): vector: tw: ($NT): [Term -> Weight]\n" if ($map->{verbose});
  my ($tw);
  if ($termWeight eq 'uniform') {
    $tw = ones($NT);                                ##-- identity weighting
  }
  elsif ($termWeight eq 'entropy') {
    ##-- weight terms by doc entropy (see e.g. Berry(1995); Nakov, Popova, & Mateev (2001))
    my $tdm0    = $map->{tdm0};                     ##-- ccs: [$ti,$di] -> f($ti,$di)
    my $t_f     = $tdm0->xchg(0,1)->sumover;        ##-- ccs: [$ti] -> f($ti)
    my $td_pdgt = ($tdm0 / $t_f)->_missing(0);      ##-- ccs: [$ti,$di] -> p($di|$ti)
    $tw         = $td_pdgt->log->_missing(0);       ##-- ccs: [$ti,$ti] -> ln p($di|$ti)
    $tw        /= log(2);                           ##                  -> log p($di|$ti)
    $tw        *= $td_pdgt;                         ##                  -> p($di|$ti) * log p($di|$ti)
    $tw         = $tw->xchg(0,1)->sumover;          ##-- pdl: [$ti] -> -H(Doc|$ti)
    $tw        /= log($ND)/log(2);                  ##-- pdl: [$ti] -> -H(Doc|$ti)/H(Doc) ##-- assumed uniform!
    $tw        += 1;
  }
  #$map->{tdm} = $map->{tdm}*$tw;
  $map->{tw} = $tw->todense;

  ##-- sanity check(s)
  if (!all($map->{tw}->isfinite)) {
    confess(ref($map)."::compile_tw(): infinite values in term-weight vector: something has gone horribly wrong!");
  }

  return $map;
}

## $map = $map->compile_tcm0()
##  + compiles matrix $map->{tcm0}: CCS::Nd: ($NT x $NC) [Term x Cat -> Freq] from $map->{tdm0}
BEGIN { *compile_tcm0 = \&compile_tcm0_v1; }
sub compile_tcm0_v1 {
  my $map = shift;

  ##-- vars
  my ($NT,$NC) = map {$map->{$_}->size} qw(tenum lcenum);
  my $lc_sym2id = $map->{lcenum}{sym2id};

  ##-- guts
  print STDERR ref($map)."::compile(): tcm: (NT=$NT x NC=$NC) [Term x Cat -> Freq]\n" if ($map->{verbose});
  my ($doc,$d_id, $cat,$c_id);
  my ($tcm_w,$tcm_nz) = (null,null);
  my ($d_tf,$d_wt,$d_w,$d_nz, $c_w);
  foreach $doc (@{$map->{docs}}) {
    $d_id = $doc->{id};
    $d_tf = $map->{tdm0}->dice_axis(1,pdl(long,$d_id));
    $d_wt = $d_tf->_whichND->slice("(0),");
    $d_nz = $d_tf->_nzvals;
    foreach $cat (@{$doc->{cats}}) {
      $c_id = $lc_sym2id->{$cat->{name}};
      $c_w  = $d_wt->slice("*1,")->glue(0, zeroes(long,1,1)+$c_id); ##-- [*,$nzi] -> [$ti,$ci]
      ##
      $tcm_w  = $tcm_w->glue(1,$c_w);
      $tcm_nz = $tcm_nz->append($d_nz);
    }
  }
  my $tcm_dims = pdl(long,$NT,$NC);
  my $tcm0 = $map->{tcm0} = PDL::CCS::Nd->newFromWhich($tcm_w,$tcm_nz,dims=>$tcm_dims,missing=>0)->dummy(0,1)->sumover;

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
##  + no implementation here

##==============================================================================
## Methods: Misc


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
