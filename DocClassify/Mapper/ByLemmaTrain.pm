## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::ByLemmaTrain.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document-to-class mapper: by lemma: training

package DocClassify::Mapper::ByLemma;
use DocClassify::Mapper;
use DocClassify::Lemmatizer;
use DocClassify::Utils ':all';

use MUDL::Enum;
use MUDL::Cluster::Distance;
use MUDL::Cluster::Distance::Builtin;

use PDL;
use PDL::CCS::Nd;
use PDL::VectorValued;

use IO::File;
use Carp;
use strict;

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
  $map->vlog('trace',"trainDocument(".$doc->label.")") if ($map->{verbose} >= 3);
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

  ##-- cleanup document (avoid memory gobbling)
  if ($map->{cleanDocs}) {
    $doc->clearCache();
  }

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

## $map = $map->compile(%opts)
##  + compile underlying map data
##  + should be called only after all training data have been added
##  + %opts:
##     byCat => $bool,         ##-- compile tcm0, tcm instead of tdm0, tdm? (default=$map->{byCat})
##     weightByCat => $bool,   ##-- use tcm0 to compute term weights? (default=$map->{weightByCat})
##     _compile_tcm0 => $bool, ##-- force compilation of $map->{tcm0}
##     _compile_tcm  => $bool, ##-- force compilation of $map->{tcm}
sub compile {
  my ($map,%opts) = @_;

  ##-- option defaults
  $opts{byCat}       = $map->{byCat} if (!exists($opts{byCat}));
  $opts{weightByCat} = $map->{weightByCat} if (!exists($opts{weightByCat}));
  $opts{_compile_tcm0} = $map->{_compile_tcm0} if (!exists($opts{_compile_tcm0}));
  $opts{_compile_tcm} = $map->{_compile_tcm} if (!exists($opts{_compile_tcm}));

  ##-- report lemmatizer class
  $map->vlog('info', "compile(): lemmatizer class: ", ref($map->lemmatizer));

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

  ##-- smooth & log-transform term-(doc|cat) matrix, compile & apply term weights
  if ($opts{byCat}) {
    ##-- matrix: $map->{tcm0}: ($NT,$NC) : CCS::Nd: [Term x Cat -> Freq]
    $map->compile_tcm0();
    $map->compile_tcm_log();
    $map->compile_tw($map->{tcm0});
    $map->{tcm} = $map->{tcm} * $map->{tw};
  } else {
    ##-- smooth & log-transform term-doc matrix
    $map->compile_tdm_log();

    ##-- compile & apply term weights
    if ($opts{weightByCat}) {
      $map->compile_tcm0();
      $map->compile_tw($map->{tcm0});
    } else {
      $map->compile_tw();
    }
    $map->{tdm} = $map->{tdm} * $map->{tw};
  }

  ##-- clear expensive perl signature structs (we've outgrown them)
  #@{$map->{sigs}} = qw();
  #@{$map->{docs}} = qw();

  ##-- matrix: $map->{tcm0}: ($NT,$NC) : CCS::Nd: [Term x Cat -> Freq]
  $map->compile_tcm0()
    if (!defined($map->{tcm0}) && ($opts{_compile_tcm0} || ref($map) eq __PACKAGE__));

  ##-- matrix: $map->{tcm}: ($NT,$NC) : CCS::Nd: [Term x Cat -> WeightedLogFreq]
  $map->compile_tcm()
    if (!defined($map->{tcm})  && ($opts{_compile_tcm}  || ref($map) eq __PACKAGE__));

  ##-- clear training cache (done by dc-mapper-train.perl)
  #$map->clearTrainingCache();

  ##-- compile distance object
  $map->compile_disto(%opts);

  return $map;
}

## $bool = $map->compiled()
##  + returns true iff $map has been compiled
##  + override checks for $map->{tcm}
sub compiled { return defined($_[0]{tcm}); }

## $map = $map->clearTrainingCache()
##  + clears any cached data from training
##  + after calling this, $map may no longer be able to train
##  + override clears training data @$map{qw(gf df sigs docs tdm0 tcm0 doc_wt)} #c2sigs
sub clearTrainingCache {
  my $map = shift;
  $map->SUPER::clearTrainingCache() or return undef;
  %{$map->{gf}} = qw();
  %{$map->{df}} = qw();
  @{$map->{sigs}} = qw();
  @{$map->{docs}} = qw(); ##-- still needed for category mapping?
  delete($map->{tw0});    ##-- useful for debugging, but recoverable as ($tw-$Raw) / $wCooked
  delete($map->{tdm0});   ##-- useful for debugging, but recoverable as ($tdm/$tw)->exp - $smoothf
  delete($map->{tcm0});   ##-- useful for debugging, but recoverable as ($tcm/$tw)->exp - $smoothf
  delete($map->{doc_wt});
  return $map;
}

##--------------------------------------------------------------
## Methods: Compilation: Utils

##----------------------------------------------
## $labstr = $map->labelString(%opts)
##  + gets symbolic label for verbose messages
##  + %opts:
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
sub labelString {
  my ($map,%opts) = @_;
  return $opts{label} || $map->{label} || '';
}

##----------------------------------------------
## $map = $map->compile_disto(%opts)
##  + compiles distance object $map->{disto} from symbolic spec $map->{dist}
##  + %opts:
##     label  => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
sub compile_disto {
  my ($map,%opts) = @_;
  my $label = $map->labelString(%opts);
  $map->vlog('info', "compile_disto() [$label]: disto: dist=$map->{dist}") if ($map->{verbose});
  $map->{disto} = MUDL::Cluster::Distance->new(class=>$map->{dist});
  return $map;
}

##----------------------------------------------
## $map = $map->compileTrim()
##  + trims @$map{qw(gf df)} by specified freqs
sub compileTrim {
  my $map = shift;

  ##-- trim by max terms per doc
  $map->vlog('info',"compileTrim(): by #/terms per doc: maxTermsPerDoc=$map->{maxTermsPerDoc}") if ($map->{verbose});
  if ($map->{maxTermsPerDoc} && $map->{maxTermsPerDoc}>0) {
    my $maxtpd = $map->{maxTermsPerDoc};
    my $gf  = $map->{gf};
    %$gf = qw();
    my ($sig,$tf,@tk,$ti);
    foreach $sig (@{$map->{sigs}}) {
      $tf = pdl([values(%{$sig->{lf}})]);
      @tk = keys(%{$sig->{lf}});
      $ti = $tf->nelem <= $maxtpd ? $tf->xvals : $tf->qsorti->slice("-1:-$maxtpd");
      $gf->{$tk[$_]} += $tf->at($_) foreach ($ti->list);
    }
  }

  ##-- trim by global term frequency
  $map->vlog('info', "compileTrim(): by global term freqency: minFreq=$map->{minFreq}") if ($map->{verbose});
  delete(@{$map->{gf}}{ grep {$map->{gf}{$_} < $map->{minFreq}} keys(%{$map->{gf}}) }) if ($map->{minFreq}>0);

  ##-- trim by doc "frequency"
  $map->vlog('info', "compileTrim(): by document term frequency: minDocFreq=$map->{minDocFreq}") if ($map->{verbose});
  delete(@{$map->{gf}}{ grep {$map->{df}{$_} < $map->{minDocFreq}} keys(%{$map->{gf}}) }) if ($map->{minDocFreq}>0);
  delete(@{$map->{df}}{ grep {!exists($map->{gf}{$_})} keys(%{$map->{df}}) });

  return $map;
}

##----------------------------------------------
## $map = $map->compileTermEnum()
##  + compiles $map->{tenum} from keys(%{$map->{gf}})
sub compileTermEnum {
  my $map = shift;
  $map->vlog('info', "compileTermEnum()") if ($map->{verbose});
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
  $map->vlog('info', "compileCatEnum(): nullCat='".($map->{nullCat} || '(none)')."'")
    if ($map->{verbose});

  if ($map->{nullCat}) {
    if ($map->{nullCat} eq '(auto)') {
      ##-- check for & expand auto null-cat
      $map->{nullCat} = (grep {defined($_)} @{$map->{gcenum}{id2sym}})[0];
      $map->vlog('info', "compileCatEnum(): nullCat -> '$map->{nullCat}'") if ($map->{verbose});
    }
    if (!defined($map->{gcenum}{id2sym}[0])) {
      ##-- set id(nullCat) to zero if possible
      $map->{gcenum}->addIndexedSymbol('(null)',0);
    } else {
      $map->{gcenum}->addSymbol('(null)');
    }
  }
  $map->{lcenum} = $map->{gcenum}->clone;
  $map->{lcenum}->compact; ##-- renumber local categories (no missing rows!)
  return $map;
}

##----------------------------------------------
## $map = $map->compileDocEnum()
##  + compile $map->{denum} (dummy placeholder; should already have happened in trainDocument())
sub compileDocEnum {
  my $map = shift;
  #$map->vlog('info',"compileDocEnum()") if ($map->{verbose});
  # ... this should already have happened in $map->trainDocument()
  return $map;
}

##----------------------------------------------
## $map = $map->compile_dcm()
##  + compiles matrix $map->{dcm}: ($ND,$NC) -> deg($di \in $ci) || 0
sub compile_dcm {
  my $map = shift;
  my $lcenum = $map->{lcenum};
  my ($ND,$NC) = map {$_->size} @$map{qw(denum lcenum)};

  $map->vlog('info', "compile_dcm(): matrix: dcm: (ND=$ND x NC=$NC) [Doc x Cat -> Deg]") if ($map->{verbose});
  my $dcm = zeroes(double, $ND,$NC); #+'inf';
  my ($doc,$cat);
  foreach $doc (@{$map->{docs}}) {
    foreach $cat (@{$doc->{cats}}) {
      $cat->{id} = $lcenum->{sym2id}{$cat->{name}}; ##-- re-assign category IDs !
      (my $tmp=$dcm->slice("($doc->{id}),($cat->{id})")) .= $cat->{deg};
    }
  }
  $map->{dcm} = PDL::CCS::Nd->newFromDense($dcm);
  #(...,'inf')->badmissing->nanmissing;
  return $map;
}

##----------------------------------------------
## $map = $map->compile_tdm0()
##  + compiles raw term-doc frequency matrix: $map->{tdm0} : [$tid,$did] => f($term[$tid],$doc[$did])
##  + caches $map->{doc_wt}
sub compile_tdm0 {
  my $map = shift;

  ##-- vars
  my ($denum,$tenum) = @$map{qw(denum tenum)};
  my $NT = $tenum->size;
  my $ND = $denum->size;

  $map->vlog('info', "compile_tdm0(): matrix: tdm0: (NT=$NT x ND=$ND) [Term x Doc -> Freq]") if ($map->{verbose});

  ##-- step 1: @$doc_wt: doc-wise term ids
  $map->vlog('info',"compile_tdm0(): matrix: tdm0: doc_wt [Doc -> Terms]") if ($map->{verbose});
  my $tenum_sym2id = $tenum->{sym2id};
  my $tenum_id2sym = $tenum->{id2sym};
  my $doc_wt = $map->{doc_wt} = []; ##-- [$docid] => pdl($nnz_doc) : [$nzi_doc] -> $ti : f($doc,$ti) defined
  my ($lf);
  @$doc_wt = map {
    $lf=$_->{lf};
    pdl(long, [ grep {defined($_)} @$tenum_sym2id{grep {$lf->{$_}>0} keys(%$lf)} ])
  } @{$map->{sigs}};

  ##-- step 2: count doc-term nnz
  $map->vlog('info',"compile_tdm0(): matrix: tdm0: Nnz") if ($map->{verbose});
  my $doc_wt_n  = pdl(long, [map {$_->nelem} @$doc_wt]);
  my $nnz       = $doc_wt_n->sum;                        ##-- sclr: nnz($d,$t)
  my $doc_wt_i1 = $doc_wt_n->cumusumover-1;              ##-- [$di] -> sum_{$dj<=$di} nnz($dj)
  my $doc_wt_i0 = ($doc_wt_n                             ##-- [$di] -> sum_{$dj< $di} nnz($di)
		   ->append(0)->rotate(1)->slice("0:-2")->cumusumover);

  ##-- step 3: create CCS::Nd tdm0
  $map->vlog('info',"compile_tdm0(): matrix: tdm0: PDL::CCS::Nd") if ($map->{verbose});
  my $sigs   = $map->{sigs};
  my $tdm0_w = zeroes(long,2,$nnz);
  my $tdm0_v = zeroes(double,$nnz);
  my ($slice1);
  foreach ($doc_wt_n->which->list) {
    $slice1 = $doc_wt_i0->at($_).":".$doc_wt_i1->at($_);
    (my $tmp=$tdm0_w->slice("(0),$slice1")) .= $doc_wt->[$_];
    ($tmp=$tdm0_w->slice("(1),$slice1")) .= $_;
    ($tmp=$tdm0_v->slice("$slice1"))     .= pdl([ @{$sigs->[$_]{lf}}{@$tenum_id2sym[$doc_wt->[$_]->list]} ]);
  }
  my $tdm0_dims = pdl(long,$NT,$ND);
  my $tdm0 = $map->{tdm0} = PDL::CCS::Nd->newFromWhich($tdm0_w,$tdm0_v,dims=>$tdm0_dims,missing=>0);
  #$tdm0 = $map->{tdm0} = $tdm0->dummy(0,1)->sumover->recode; ##-- avoid dangling zeroes

  return $map;
}

##----------------------------------------------
## $docIdPdl = $map->docIdPdl()
##  + returns size of local doc subset (docids) or sequence($map->{denum}->size)
##  + returned pdl is ($NDocsLocal) : [$docid_local] -> $docid_global
sub docIdPdl {
  my $map = shift;
  return $map->{docids} if (defined($map->{docids}));
  return sequence(long,$map->{denum}->size);
}

##----------------------------------------------
## $map = $map->compile_tcm0()
##  + compiles matrix $map->{tcm0}: CCS::Nd: ($NT x $NC) [Term x Cat -> Freq] from $map->{tdm0}
##  + requires cached $map->{tdm0}, $map->{dcm}, $map->{doc_wt}
sub compile_tcm0 {
  my $map = shift;

  ##-- vars
  my ($denum,$tenum,$lcenum) = @$map{qw(denum tenum lcenum)};
  my $docids = $map->docIdPdl;
  my $NT = $tenum->size;
  my $ND = $docids->nelem;
  my $NC = $lcenum->size;

  $map->vlog('info', "compile_tcm0(): matrix: tcm0: (NT=$NT x NC=$NC) [Term x Cat -> Freq]") if ($map->{verbose});

  ##-- step 0: get sparse boolean dcmb [Doc x Cat -> Bool]
  my $dcm    = $map->{dcm};
  my $dcmb   = $dcm->clone;
  $dcmb->_nzvals->slice("0:-1") .= 1;
  my $doc_nc = $dcmb->xchg(0,1)->sumover->decode; ##-- [$di_local] -> $ncats
  ###
  #my $dcm_wd = $dcmb->_whichND->slice("(0),");
  #my $dcm_wc = $dcmb->_whichND->slice("(1),");

  ##-- step 1: count doc-term nnz
  $map->vlog('info', "compile_tcm0(): matrix: tcm0: Nnz") if ($map->{verbose});
  my $doc_wt    = $map->{doc_wt};
  my $doc_wt_n  = pdl(long, [map {$_->nelem} @$doc_wt])->index($docids);
  my $doc_cat_wt_n = $doc_wt_n * $doc_nc;
  my $nnz       = $doc_cat_wt_n->sum;                        ##-- sclr: nnz($d,$t)

  ##-- step 3: create CCS::Nd
  $map->vlog('info', "compile_tcm0(): matrix: tcm0: PDL::CCS::Nd") if ($map->{verbose});
  my $docs = $map->{docs};
  my $lcenum_sym2id = $map->{lcenum}{sym2id};
  my $tdm0 = $map->{tdm0};
  ##
  my $tcm0_w = zeroes(long,2,$nnz);
  my $tcm0_v = zeroes(double,$nnz);
  my ($di_local,$di_global,$tdm0d,$ci,$n,$slice1);
  my $nzi = 0;
  foreach $di_local (0..($ND-1)) {
    $di_global = $docids->at($di_local);
    $n = $doc_wt_n->at($di_local);
    next if (!$n);
    $tdm0d = $tdm0->dice_axis(1,$di_local);
    foreach $ci (@$lcenum_sym2id{map {$_->{name}} @{$docs->[$di_global]{cats}}}) {
      $slice1 = $nzi.':'.($nzi+$n-1);
      $tcm0_w->slice("(0),$slice1") .= $tdm0d->_whichND->slice("(0),");
      $tcm0_w->slice("(1),$slice1") .= $ci;
      $tcm0_v->slice("$slice1")     .= $tdm0d->_nzvals;
      $nzi += $n;
    }
  }
  my $tcm0_dims = pdl(long,$NT,$NC);
  my $tcm0 = PDL::CCS::Nd->newFromWhich($tcm0_w,$tcm0_v,dims=>$tcm0_dims,missing=>0)->dummy(0,1)->sumover;
  $map->{tcm0} = $tcm0;

  return $map;
}

##----------------------------------------------
## $map = $map->compile_tcm()
##  + compiles matrix $map->{tcm}: CCS::Nd: ($NT x $NC) [Term x Cat -> Freq] from $map->{tcm0}
sub compile_tcm {
  my $map = shift;

  my ($NT,$NC) = $map->{tcm0}->dims;
  $map->vlog('info', "compile_tcm(): matrix: tcm: (NT=$NT x NC=$NC) [Term x Cat -> WeightedLogFreq]") if ($map->{verbose});
  $map->{tcm} = $map->logwm($map->{tcm0});

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
  $map->vlog('info', "compile_tdm_log(): smooth(smoothf=$map->{smoothf})") if ($map->{verbose});
  $map->{tdm} = ($map->{tdm0}+$map->{smoothf})->inplace->log;

  return $map;
}

##----------------------------------------------
## $map = $map->compile_tcm_log()
##  + smooths & logs raw term-cat frequency matrix: $map->{tcm} : [$tid,$did] => log(f($term[$tid],$cat[$lcid])+1)
##  + computes from $map->{tcm0}
sub compile_tcm_log {
  my $map = shift;

  ##-- smooth & log-transform term-doc matrix
  $map->{smoothf} = $map->{lcenum}->size/$map->{tcm0}->sum if (!$map->{smoothf});
  $map->vlog('info', "compile_tcm_log(): smooth(smoothf=$map->{smoothf})") if ($map->{verbose});
  $map->{tcm} = ($map->{tcm0}+$map->{smoothf})->inplace->log;

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
  elsif ($termWeight =~ /^entropy.q/ || $termWeight eq 'Hq') {
    $termWeight='entropy-quotient';
  }
  elsif ($termWeight =~ /^cond/ || $termWeight eq 'Hc') {
    $termWeight='conditional-entropy';
  }
  elsif ($termWeight =~ /^max/ || $termWeight =~ /^H/ || $termWeight eq 'entropy') {
    $termWeight='max-entropy-quotient';
  }
  else {
    $map->logconfess("compile(): unknown term-weighting method '$termWeight'");
  }

  return $map->{termWeight}=$termWeight;
}

##----------------------------------------------
## $map = $map->compile_tw()
## $map = $map->compile_tw($tdm0)
##  + compiles term-weight vector $map->{tw}: ($NT): [$tid] -> weight($term[$tid])
##  + does NOT apply weights to $map->{tdm} -- do that yourself!
##  + default $tdm0 = $map->{tdm0}
##    - to use doc-cat matrix, call $map->compile_tw($map->{tcm0})
sub compile_tw {
  my ($map,$tdm0) = @_;
  $tdm0 = $map->{tdm0} if (!defined($tdm0));

  ##-- vars
  my $termWeight = $map->termWeightMethod;
  #my $NT = $map->{tenum}->size;
  #my $ND = $map->{denum}->size;
  my ($NT,$ND) = $tdm0->dims;
  my $wRaw    = $map->{twRaw} = $map->{twRaw} // 0;
  my $wCooked = $map->{twCooked} = $map->{twCooked} // 1;

  ##-- guts
  if ($map->{verbose}) {
    $map->vlog('info',"compile_tw(): vector: tw: (NT=$NT) [Term -> Weight]");
    $map->vlog('info',"compile_tw(): tw=$termWeight, weightByCat=".($map->{weightByCat} ? 1 : 0).", wRaw=$wRaw, wCooked=$wCooked");
  }

  my $t_f = $tdm0->xchg(0,1)->sumover;              ##-- ccs: [$ti] -> f($ti)
  my ($tw);
  if ($termWeight eq 'uniform') {
    $tw = ones($NT);                                ##-- identity weighting
  }
  elsif ($termWeight eq 'max-entropy-quotient') {
    ##-- weight terms by doc max-entropy (see e.g. Berry(1995); Nakov, Popova, & Mateev (2001))
    #my $tdm0   = $map->{tdm0};                     ##-- ccs: [$ti,$di] -> f($ti,$di)
    my $t_f     = $tdm0->xchg(0,1)->sumover;        ##-- ccs: [$ti] -> f($ti)
    my $td_pdgt = ($tdm0 / $t_f)->_missing(0);      ##-- ccs: [$ti,$di] -> p($di|$ti)
    $tw         = $td_pdgt->log->_missing(0);       ##-- ccs: [$ti,$di] -> ln p($di|$ti)
    $tw        /= log(2);                           ##                  -> log p($di|$ti)
    $tw        *= $td_pdgt;                         ##                  -> p($di|$ti) * log p($di|$ti)
    $tw         = $tw->xchg(0,1)->sumover;          ##-- pdl: [$ti] ->  -H(Doc|T=$ti)
    $tw        /= log($ND)/log(2);                  ##-- pdl: [$ti] ->  -H(Doc|T=$ti)/Hmax(Doc) ##-- uniform!
    $tw        += 1;                                ##-- pdl: [$ti] -> 1-H(Doc|T=$ti)/Hmax(Doc)
  }
  elsif ($termWeight eq 'entropy-quotient') {
    ##-- weight terms by relative doc entropy: tw(t) = 1 - (H(Doc|t) / H(Doc))
    #my $tdm0    = $map->{tdm0};                     ##-- ccs: [$ti,$di] -> f($ti,$di)
    my $t_f     = $tdm0->xchg(0,1)->sumover;        ##-- ccs: [$ti] -> f($ti)
    my $td_pdgt = ($tdm0 / $t_f)->_missing(0);      ##-- ccs: [$ti,$di] -> p($di|$ti)
    my $td_pdt  = ($tdm0 / $t_f->sum)->_missing(0); ##-- ccs: [$ti,$di] -> p($di,$ti)
    ##
    my $d_f     = $tdm0->sumover->todense;          ##-- dense: [$di] -> f($di);
    my $d_p     = $d_f / $d_f->sum;                 ##-- dense: [$di] -> p($di)
    $d_p        = li1($d_p,$d_p->where($d_p)->minimum/2);            #-> p~($di)
    my $d_h     = log($d_p);                        ##-- dense: [$di] ->            ln p($di)
    $d_h       /= log(2);                           ##                ->           log p($di)
    $d_h       *= $d_p;                             ##                ->  p($di) * log p($di)
    my $d_H     = -($d_h->sum);                     ##-- sclr:  []    -> H(Doc)
    ##
    $tw         = $td_pdgt->log->_missing(0);       ##-- ccs: [$ti,$di] -> ln p($di|$ti)
    $tw        /= log(2);                           ##                  -> log p($di|$ti)
    #$tw        *= $td_pdt;                          ##                  -> p($di,$ti) * log p($di|$ti) ~h($di|$ti)
    $tw        *= $td_pdgt;                         ##                  -> p($di|$ti) * log p($di|$ti) ~h($di|T=$di)
    $tw         = $tw->xchg(0,1)->sumover;          ##-- pdl: [$ti] -> -H(Doc|T=$ti)
    $tw        /= $d_H;                             ##              -> -H(Doc|T=$ti)/H(Doc)
    $tw        += 1;                                ##              -> 1 - H(Doc|T=$ti)/H(Doc)
  }
  elsif ($termWeight eq 'conditional-entropy') {
    ##-- weight terms by conditional doc|term subdistribution entropy: tw(t) = H(Doc|t)
    #my $tdm0    = $map->{tdm0};                     ##-- ccs: [$ti,$di] -> f($ti,$di)
    my $t_f     = $tdm0->xchg(0,1)->sumover;        ##-- ccs: [$ti] -> f($ti)
    my $td_pdgt = ($tdm0 / $t_f)->_missing(0);      ##-- ccs: [$ti,$di] -> p($di|$ti)
    ##
    $tw         = $td_pdgt->log->_missing(0);       ##-- ccs: [$ti,$di] -> ln p($di|$ti)
    $tw        *= $td_pdgt;                         ##                  -> p($di|$ti) * ln p($di|$ti)
    $tw         = $tw->xchg(0,1)->sumover->todense; ##-- pdl: [$ti] -> -H_e(Doc|$ti)
    $tw        *= -1;                               ##              ->  H_e(Doc|$ti)
    $tw        /= log(2);                           ##              ->    H(Doc|$ti)
  }
  else {
    $map->logconfess("compile_tw(): unknown term-weighting method '$termWeight'");
  }

  ##-- "pure" weights
  $map->{tw0} = $tw->todense;

  ##-- adjusted weights
  $map->{tw}  = $map->{tw0}*$wCooked;
  $map->{tw} += $wRaw;

  ##-- sanity check(s)
  if (!all($map->{tw}->isfinite)) {
    $map->logconfess("compile_tw(): infinite values in term-weight vector: something has gone horribly wrong!");
  }
  if (any($map->{tw}<0)) {
    $map->logconfess("compile_tw(): negative values in term-weight vector: something has gone horribly wrong!");
  }
  if (any($map->{tw}==0)) {
    $map->logwarn("compile_tw(): zero values in term-weight vector: something looks fishy!");
  }

  return $map;
}

##----------------------------------------------
## $tXm = $map->logwm($tXm0)
##  + compiles log-transformed, term-weighted matrix $tXm ($NT,$NX) from raw frequency matrix $tXm0 ($NT,$NX)
##  + $tXm0 may be either a dense PDL or a PDL::CCS::Nd
sub logwm {
  my ($map,$txm0) = @_;
  $txm0 = $txm0->dummy(1,1) if ($txm0->ndims==0); ##-- someone passed in a flat term matrix
  return ($txm0+$map->{smoothf})->inplace->log->inplace->mult($map->{tw},0);
}

##==============================================================================
## Methods: DEBUG: Recovery

##----------------------------------------------
## $tdm0 = $map->get_tdm0()
##  + computes & re-caches $map->{tcm0}: CCS::Nd: ($NT x $NC) [Term x Cat -> Freq] from $map->{tdm0}
##  + requires $map->{tdm}, $map->{tw}
sub get_tdm0 {
  my $map = shift;
  return $map->{tdm0} if (defined($map->{tdm0}));
  return $map->{tdm0} = ($map->{tdm} / $map->{tw})->_missing(0)->exp - $map->{smoothf};
}

##----------------------------------------------
## $doc_wt = $map->get_doc_wt()
##  + compites & re-caches $map->{doc_wt} from $map->{tdm0}
sub get_doc_wt {
  my $map = shift;
  return $map->{doc_wt} if (defined($map->{doc_wt}) && @{$map->{doc_wt}});
  my $tdm0 = $map->get_tdm0();
  my ($ptr,$pi2nzi) = $tdm0->getptr(1);
  my $nzimin = $ptr->slice("0:-2");
  my $nzimax = $ptr->slice("1:-1")-1;
  #my $ptrlen = ccs_pointerlen($ptr);
  #my $which  = $tdm0->_whichND->dice_axis(1,$pi2nzi);
  my $nzvals = $tdm0->_nzvals->index($pi2nzi);
  $map->{doc_wt} = [
		    map {
		      ($nzimin->at($_) > $nzimax->at($_)
		       ? null->long
		       : $nzvals->slice($nzimin->at($_).":".$nzimax->at($_)))
		    } $nzimin->xvals->list
		   ];
  return $map->{doc_wt};
}

##----------------------------------------------
## $tcm = $map->get_tcm0()
##  + (re-)computes & caches $map->{tcm0} from $map->{tcm} or $map->{tdm0}
sub get_tcm0 {
  my $map = shift;
  return $map->{tcm0} if (defined($map->{tcm0}));
  if (defined($map->{tcm})) {
    $map->{tcm0} = ($map->{tcm} / $map->{tw})->_missing(0)->exp - $map->{smoothf};
  } else {
    ##-- create CCS::Nd
    #$map->vlog('info', "get_tcm0(): matrix: tcm0: PDL::CCS::Nd") if ($map->{verbose});

    ##-- step 0: get sparse boolean dcmb [Doc x Cat -> Bool]
    my $dcm    = $map->{dcm};
    my $dcmb   = $dcm->clone;
    $dcmb->_nzvals->slice("0:-1") .= 1;
    my $doc_nc = $dcmb->xchg(0,1)->sumover->decode; ##-- [$di_local] -> $ncats

    ##-- step 1: count doc-term nnz [HACKED]
    my $tdm0 = $map->get_tdm0();
    my ($d_ptr,$d_pi2nzi) = $tdm0->getptr(1);
    my $d_nzimin = $d_ptr->slice("0:-2");
    my $d_nzimax = $d_ptr->slice("1:-1")-1;
    my $d_nzinull = ($d_nzimin > $d_nzimax);
    my $d_ptrlen = $d_nzimax - $d_nzimin + 1;
    my $tdm0_which  = $tdm0->_whichND->dice_axis(1,$d_pi2nzi);
    my $tdm0_nzvals = $tdm0->_nzvals->index($d_pi2nzi);
    my $nnz = ($d_ptrlen * $doc_nc)->sum;

    ###~~~ OLD
    my $tcm0_w = zeroes(long,2,$nnz);
    my $tcm0_v = zeroes(double,$nnz);

    my ($di,$tdm0d,$ci,$n,$slice1,$slice2);
    my $nzi = 0;
    foreach $di ($d_nzimin->xvals->where(!$d_nzinull)->list) {
      $n = $d_ptrlen->at($di);
      $slice2 = $d_nzimin->at($di).":".$d_nzimax->at($di);
      foreach $ci (
		   #$dcmb->slice("($di)")->which->list
		   $dcmb->dice_axis(0,$di)->_whichND->slice("(1),")->list
		  )
	{
	  $slice1 = $nzi.':'.($nzi+$n-1);
	  $tcm0_w->slice("(0),$slice1") .= $tdm0_which->slice("(0),$slice2");
	  $tcm0_w->slice("(1),$slice1") .= $ci;
	  $tcm0_v->slice("$slice1")     .= $tdm0_nzvals->slice("$slice2");
	  $nzi += $n;
	}
    }
    my $tcm0_dims = pdl(long,$map->{tenum}->size,$map->{lcenum}->size);
    my $tcm0 = PDL::CCS::Nd->newFromWhich($tcm0_w,$tcm0_v,dims=>$tcm0_dims,missing=>0)->dummy(0,1)->sumover;
    $map->{tcm0} = $tcm0;
  }
  return $map->{tcm0};
}

##----------------------------------------------
## $tcm = $map->get_tcm()
##  + computes $map->{tcm}: CCS::Nd: 
##  + requires $map->{tdm0}
sub get_tcm {
  my $map = shift;
  return $map->{tcm} if (defined($map->{tcm}));
  my $tcm0 = $map->get_tcm0();
  return $map->{tcm} = $map->logwm($map->{tcm0});
}

##==============================================================================
## Footer
1;

__END__
