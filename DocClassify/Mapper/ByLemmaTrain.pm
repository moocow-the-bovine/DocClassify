## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::ByLemmaTrain.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: document-to-class mapper: by lemma: training

package DocClassify::Mapper::ByLemma;
use DocClassify::Mapper;
use DocClassify::Lemmatizer;
use DocClassify::Utils ':all';

use MUDL::Enum;
use MUDL::Cluster::Distance;
use MUDL::Cluster::Distance::Builtin;

use PDL;
use PDL::IO::FastRaw; ##-- for mapfraw()
use PDL::CCS::Nd;
use PDL::VectorValued;

use IO::File;
use File::Basename qw(basename dirname);
use Carp;
use strict;

##==============================================================================
## Methods: API: Training

## $map = $map->trainInit()
##  + override honors "saveMem" attribute by tying @$map{qw(docs sigs gf df)} if not already tied
sub trainInit {
  my $map = shift;
  if ($map->{saveMem}) {
    tied(@{$map->{docs}})
      or $map->{docs}=tmparray("dc_docs_XXXXXXXX", UNLINK=>$map->{clearCache})
      or $map->logconfess("new(): failed to tie temporary {docs} array for 'saveMem' option: $!");

    tied(@{$map->{sigs}})
      or $map->{sigs}=tmparray("dc_sigs_XXXXXXXX", UNLINK=>$map->{clearCache})
      or $map->logconfess("new(): failed to tie temporary {sigs} array for 'saveMem' option: $!");

    tied(%{$map->{gf}})
      or $map->{gf}=tmphash("dc_gf_XXXXXXXX", UNLINK=>$map->{clearCache})
      or $map->logconfess("new(): failed to tie temporary {gf} hash for 'saveMem' option: $!");

    tied(%{$map->{df}})
      or $map->{df}=tmphash("dc_df_XXXXXXXX", UNLINK=>$map->{clearCache})
      or $map->logconfess("new(): failed to tie temporary {df} hash for 'saveMem' option: $!");
  }
  return $map->SUPER::trainInit(@_);
}

## $map = $map->trainCorpus($corpus)
##  + add training data from $corpus
##  + override calls trainInit()
##  + inherited default just calls $map->trainDocument($doc) foreach doc in corpus
sub trainCorpus {
  my $map = shift;
  $map->trainInit();
  return $map->SUPER::trainCorpus(@_);
}

## $map = $map->trainDocument($doc)
##  + add training data from $doc
##  + calls $map->lemmaSignature($doc)
sub trainDocument {
  my ($map,$doc) = @_;
  if ($map->{verbose} >= 2 && dirname($doc->label) ne ($map->{trainDocumentDir_}//'')) {
    $map->{trainDocumentDir_} = dirname($doc->label);
    $map->vlog('trace',"trainDocument(DIR=$map->{trainDocumentDir_})");
  }
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
##     _compile_types => $bool, ##-- force call $map->compile_types()
sub compile {
  my ($map,%opts) = @_;

  ##-- option defaults
  $opts{byCat}       = $map->{byCat} if (!exists($opts{byCat}));
  $opts{weightByCat} = $map->{weightByCat} if (!exists($opts{weightByCat}));
  $opts{_compile_tcm0} = $map->{_compile_tcm0} if (!exists($opts{_compile_tcm0}));
  $opts{_compile_tcm} = $map->{_compile_tcm} if (!exists($opts{_compile_tcm}));

  ##-- report lemmatizer class
  $map->vlog('info', "compile(): lemmatizer class: ", ref($map->lemmatizer)) if ($map->{verbose});

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
  $map->compile_tdm(%opts);

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

  ##-- convert to target index+value types
  $map->compile_types(%opts) if ($opts{_compile_types} || ref($map) eq __PACKAGE__);

  return $map;
}

## $bool = $map->compiled()
##  + returns true iff $map has been compiled
##  + override checks for $map->{tcm}
sub compiled { return defined($_[0]{tcm}); }

## $map = $map->recompile()
##  + re-compile underlying map data
##  + may be called after map options have changed
##  + not supported by all mappers
sub recompile {
  my ($map,%opts) = @_;

  ##-- inherited re-compilation
  $map->SUPER::recompile(%opts)
    or $map->logconfess("recompile() inherited re-compilation failed: $!");

  ##-- recover local training data: $map->{sigs}
  if (!@{$map->{sigs}//[]}) {
    $map->vlog('info', "recompile(): lemma signatures") if ($map->{verbose});
    my $tdm0   = $map->get_tdm0();
    my $tdm0_w = $tdm0->_whichND;
    my $tdm0_v = $tdm0->_whichVals;
    my $t2sym  = $map->{tenum}{id2sym};
    my $sigs   = ($map->{sigs} //= []);
    @$sigs     = map {DocClassify::Signature->new(lf=>{})} @{$map->{docs}};
    my $nzimax = $tdm0_v->nelem-1;
    my ($di,$ti);
    foreach (0..$nzimax) {
      ($ti,$di) = $tdm0_w->slice(",($_)")->list;
      $sigs->[$di]{lf}{$t2sym->[$ti]} = $tdm0_v->at($_);
    }
    my $dNl = $tdm0->sumover->todense;
    foreach (0..$#$sigs) {
      $sigs->[$_]{Nl} = $dNl->at($_);
    }
  }

  ##-- compile()
  return $map->compile(%opts);
}


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
  delete($map->{tf0});
  delete($map->{tdf0});
  delete($map->{doc_wt});
  delete($map->{trainDocumentDir_});
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

  ##-- trim: common variables
  my ($gf,$df, $mingf,$mindf) = @$map{qw(gf df minFreq minDocFreq)};

  ##-- trim %$gf by global term frequency
  $map->vlog('info', "compileTrim(): by global term freqency: minFreq=$map->{minFreq}") if ($map->{verbose});
  if (($mingf=$map->{minFreq}) > 0) {
    if (UNIVERSAL::isa(tied(%$gf),'DB_File')) {
      ##-- trim: by global term frequency: saveMem/DB_File
      $map->vlog('info', "compileTrim(): by global term freqency: DB_File") if ($map->{verbose});
      my $tied = tied(%$gf);
      my ($key,$val,$status);
      for ($status=$tied->seq($key,$val,DB_File::R_FIRST()); $status==0; $status=$tied->seq($key,$val,DB_File::R_NEXT())) {
	$tied->del($key, DB_File::R_CURSOR()) if ($val < $mingf);
      }
    } else {
      ##-- trim: by global term frequency: default/HASH
      delete(@$gf{ grep {$gf->{$_} < $mingf} keys(%$gf) });
    }
  }

  ##-- trim %$gf by doc "frequency"
  $map->vlog('info', "compileTrim(): by document term frequency: minDocFreq=$map->{minDocFreq}") if ($map->{verbose});
  if (($mindf=$map->{minDocFreq}) > 0) {
    if (UNIVERSAL::isa(tied(%$gf),'DB_File')) {
      ##-- trim: by doc-frequency: saveMem/DB_File
      $map->vlog('info', "compileTrim(): by document term freqency: DB_File") if ($map->{verbose});
      my $tied = tied(%$gf);
      my ($key,$val,$status);
      for ($status=$tied->seq($key,$val,DB_File::R_FIRST()); $status==0; $status=$tied->seq($key,$val,DB_File::R_NEXT())) {
	$tied->del($key, DB_File::R_CURSOR()) if ($df->{$key} < $mindf);
      }
    }
    else {
      ##-- trim: by doc-frequency: default/HASH: gf
      delete(@$gf{ grep {$df->{$_} < $mindf} keys(%$gf) });
    }
  }

  ##-- trim %$df: intersect with %$gf
  if ($mingf > 0 || $mindf > 0) {
    $map->vlog('info', "compileTrim(): intersect global- and doc-frequency") if ($map->{verbose});
    if (UNIVERSAL::isa(tied(%$df),'DB_File')) {
      ##-- trim: intersect: saveMem/DB_File
      $map->vlog('info', "compileTrim(): intersect: DB_File") if ($map->{verbose});
      my $tied = tied(%$df);
      my ($key,$val,$status);
      for ($status=$tied->seq($key,$val,DB_File::R_FIRST()); $status==0; $status=$tied->seq($key,$val,DB_File::R_NEXT())) {
	$tied->del($key, DB_File::R_CURSOR()) if (!exists($gf->{$key}));
      }
    } else {
      ##-- trim: intersect: defaulkt/HASH
      delete(@$df{ grep {!exists  $gf->{$_}} keys(%$df) });
    }
  }

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
  @{$tenum->{id2sym}} = keys %{$map->{gf}};
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

  my $ndc = 0;
  $ndc += scalar(@{$_->{cats}}) foreach (@{$map->{docs}});

  my $dc_which = zeroes($map->itype,2,$ndc);
  my $dc_vals  = zeroes($map->vtype,$ndc);
  my $nzi      = 0;
  my $docs     = $map->{docs};
  my $dtied    = tied @$docs;
  my ($doc,$cat,$catid,$updated,$tmp);
  foreach $doc (@$docs) {
    $updated = 0;                               ##-- true if we've updated any cat-ids (for saveMem)
    foreach $cat (@{$doc->{cats}}) {
      $catid = $lcenum->{sym2id}{$cat->{name}}; ##-- re-compute category IDs !
      if ($catid != $cat->{id}) {
	$cat->{id} = $catid;
	$updated   = 1;
      }
      $dc_which->set(0,$nzi, $doc->{id});
      $dc_which->set(1,$nzi, $cat->{id});
      $dc_vals->set($nzi,    $cat->{deg});
      ++$nzi;
    }
    $docs->[$doc->{id}] = $doc if ($updated && $dtied); ##-- update tied doc
  }
  $map->{dcm} = PDL::CCS::Nd->newFromWhich($dc_which,$dc_vals);
  #(...,'inf')->badmissing->nanmissing;
  return $map;
}

##----------------------------------------------
## $map = $map->compile_tdm0()
##  + compiles raw term-doc frequency matrix: $map->{tdm0} : [$tid,$did] => f($term[$tid],$doc[$did])
##  + new implementation does NOT cache or use $map->{doc_wt}
sub compile_tdm0 {
  my $map = shift;

  ##-- vars
  my ($denum,$tenum) = @$map{qw(denum tenum)};
  my $NT = $tenum->size;
  my $ND = $denum->size;

  $map->vlog('info', "compile_tdm0(): matrix: tdm0: (NT=$NT x ND=$ND) [Term x Doc -> Freq]") if ($map->{verbose});

  ##-- step 1: create tempfiles $tdm0_w_fh (~whichND), $tdm0_val_fh (~nzvals)
  my $tenum_sym2id = $tenum->{sym2id};
  my $tenum_id2sym = $tenum->{id2sym};
  my $pack_wnd = $PDL::Types::pack[ $map->_itype->enum ];
  my $pack_val = $PDL::Types::pack[ $map->_vtype->enum ];
  my ($tdm0_wnd_fh,$tdm0_wnd_file) = tmpfh('dc_tdm0_wnd_XXXXXX', UNLINK=>1, SUFFIX=>'.pdl');
  my ($tdm0_val_fh,$tdm0_val_file) = tmpfh('dc_tdm0_val_XXXXXX', UNLINK=>1, SUFFIX=>'.pdl');
  $map->vlog('info',"compile_tdm0(): matrix: tdm0: tempfiles ($tdm0_wnd_file, $tdm0_val_file)") if ($map->{verbose});
  my $di = 0;
  my ($lf,%tif,$ti,$tf);
  foreach (@{$map->{sigs}}) {
    $lf  = $_->{lf};
    %tif = map {defined($ti=$tenum_sym2id->{$_}) && ($tf=$lf->{$_})>0 ? ($ti=>$tf) : qw()} keys(%$lf);
    $tdm0_wnd_fh->print(pack($pack_wnd, map {($_,$di)} keys %tif));
    $tdm0_val_fh->print(pack($pack_val, values %tif));
    ++$di;
  }
  undef $lf;
  %tif = qw();
  my $nnz = $tdm0_val_fh->tell / length(pack($pack_val,0));

  ##-- step 2: mmap temp files to PDLs
  $map->vlog('info',"compile_tdm0(): matrix: tdm0: mmap") if ($map->{verbose});
  $tdm0_wnd_fh->close() or $map->logconfess("compile_tdm0(): close failed for tempfile $tdm0_wnd_file: $!");
  $tdm0_val_fh->close() or $map->logconfess("compile_tdm0(): close failed for tempfile $tdm0_val_file: $!");
  my $tdm0_wnd = mapfraw($tdm0_wnd_file, {Creat=>0,Trunc=>0,ReadOnly=>1,Dims=>[2,$nnz],Datatype=>$map->_itype});
  my $tdm0_val = mapfraw($tdm0_val_file, {Creat=>0,Trunc=>0,ReadOnly=>1,Dims=>[$nnz+1],Datatype=>$map->_vtype});
  $map->logconfess("compile_tdm0(): mmap failed for $tdm0_wnd_file") if (!defined($tdm0_wnd));
  $map->logconfess("compile_tdm0(): mmap failed for $tdm0_val_file") if (!defined($tdm0_val));

  ##-- step 3: ccs
  $map->vlog('info',"compile_tdm0(): matrix: tdm0: PDL::CCS::Nd [Nnz=$nnz]") if ($map->{verbose});
  my $tdm0 = $map->{tdm0} = PDL::CCS::Nd->newFromWhich($tdm0_wnd,$tdm0_val,dims=>pdl($map->_dtype,$NT,$ND),missing=>0);

  ##-- step 4: cleanup
  undef $tdm0_wnd;
  undef $tdm0_val;
  !-e $tdm0_wnd_file or CORE::unlink($tdm0_wnd_file) or $map->logwarn("compile_tdm0(): failed to unlink tempfile $tdm0_wnd_file: $!");
  !-e $tdm0_val_file or CORE::unlink($tdm0_val_file) or $map->logwarn("compile_tdm0(): failed to unlink tempfile $tdm0_val_file: $!");

  return $map;
}

##----------------------------------------------
## $map = $map->compile_tdm(%opts)
##  + compiles $map->{tdm} from $map->{tdm0} and $map->{tw}
##  + calls $map->compile_tw()
##  + may call $map->compile_tcm0() etc.
##  + %opts: byCat, weightByCat (see $map->compile())
sub compile_tdm {
  my ($map,%opts) = @_;

  ##-- option defaults
  $opts{byCat}       = $map->{byCat} if (!exists($opts{byCat}));
  $opts{weightByCat} = $map->{weightByCat} if (!exists($opts{weightByCat}));

  my $NT = $map->{tenum}->size;
  my $ND = $map->{denum}->size;
  $map->vlog('info', "compile_tdm(): matrix: tdm: (NT=$NT x ND=$ND) [Term x Doc -> WeightedVal]") if ($map->{verbose});

  ##-- smooth & log-transform term-(doc|cat) matrix, compile & apply term weights
  my ($tmp);
  if ($opts{byCat}) {
    ##-- matrix: $map->{tcm0}: ($NT,$NC) : CCS::Nd: [Term x Cat -> Freq]
    $map->compile_tcm0();
    $map->compile_tcm_log();
    $map->compile_tw($map->{tcm0});
    #$map->{tcm} = $map->{tcm} * $map->{tw};
    ($tmp=$map->{tcm}->_nzvals) *= $map->{tw}->index($map->{tcm}->_whichND->slice("(0),")); ##-- faster and memory-friendlier
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
    #$map->{tdm} = $map->{tdm} * $map->{tw};
    ($tmp=$map->{tdm}->_nzvals) *= $map->{tw}->index($map->{tdm}->_whichND->slice("(0),"));  ##-- faster and memory-friendlier
  }

  return $map;
}

##----------------------------------------------
## $map = $map->compile_tcm0()
##  + compiles matrix $map->{tcm0}: CCS::Nd: ($NT x $NC) [Term x Cat -> Freq] from $map->{tdm0}
##  + requires cached $map->{tdm0}, $map->{dcm}
sub compile_tcm0 {
  my $map = shift;

  ##-- vars
  my ($denum,$tenum,$lcenum) = @$map{qw(denum tenum lcenum)};
  my $NT = $tenum->size;
  my $ND = $denum->size;
  my $NC = $lcenum->size;
  my ($tmp); ##-- for annoying 'Can't return a temporary from lvalue subroutine at ...' workarounds

  $map->vlog('info', "compile_tcm0(): matrix: tcm0: (NT=$NT x NC=$NC) [Term x Cat -> Freq]") if ($map->{verbose});

  ##-- step 0: get sparse boolean dcmb [Doc x Cat -> Bool]
  my $dcm    = $map->{dcm};
  my $dcmb   = $dcm->shadow(which=>$dcm->_whichND, vals=>ones(ushort,$dcm->_vals->dims))->_missing(0);
  my $doc_nc = $dcmb->xchg(0,1)->sumover; ##-- ccs: [$di_local] -> $ncats

  ##-- check for determistic (doc->cat) mappings
  if ($doc_nc->max == 1) {
    ##-- optimize for determistic (doc->cat) mapping
    $map->vlog('info', "compile_tcm0(): deterministic (Doc->Cat) mapping detected") if ($map->{verbose});

    ##-- save some memory
    undef $dcmb;
    undef $doc_nc;

    my ($tmp);
    my $tdm0 = $map->get_tdm0();
    my $d2c  = zeroes($map->itype, $dcm->dim(0)); ##-- pdl (ND) : [$di] => $ci_local
    ($tmp=$d2c->index($dcm->_whichND->slice("(0),"))) .= $dcm->_whichND->slice("(1),");
    my $which0 = $tdm0->_whichND->pdl;
    my $vals0  = $tdm0->_vals;
    ($tmp=$which0->slice("(1),")) .= $d2c->index($which0->slice("(1),"));
    my $which  = $which0->uniqvec;
    my $nzvals = zeroes($tdm0->type, $which->dim(1));
    $tdm0->_nzvals->indadd( $which0->vsearchvec($which), $nzvals );
    my $tcm0   = PDL::CCS::Nd->newFromWhich($which,$nzvals->append($tdm0->missing),steal=>1);
    $map->{tcm0} = $tcm0;
  }
  else {
    ##-- non-deterministic (doc->cat) mapping: do it the old (hard and slow) way
    $map->vlog('info', "compile_tcm0(): non-deterministic (Doc->Cat) mapping detected") if ($map->{verbose});

    ##-- step 1: count doc-term nnz
    $map->vlog('info', "compile_tcm0(): matrix: tcm0: Nnz") if ($map->{verbose});
    my $tdm0        = $map->get_tdm0;
    my $doc_nnz     = $tdm0->nnz->decode;
    my $nnz         = ($doc_nnz * $doc_nc)->sum;                        ##-- sclr: nnz($c,$t)

    ##-- step 3: create CCS::Nd
    $map->vlog('info', "compile_tcm0(): matrix: tcm0: PDL::CCS::Nd") if ($map->{verbose});
    my $tcm0_w = zeroes($map->_itype, 2,$nnz);
    my $tcm0_v = zeroes($map->_vtype,   $nnz);

    my ($ci,$cdis,$di,$n,$tdm0d,$slice1);
    my $nzi = 0;
    foreach $ci (0..($NC-1)) {
      $cdis = $dcmb->dice_axis(1,$ci)->_whichND->slice("(0),");
      foreach $di ($cdis->list) {
	next if (!($n = $doc_nnz->at($di)));
	$tdm0d  = $tdm0->dice_axis(1,$di);
	$slice1 = $nzi.':'.($nzi+$n-1);
	($tmp   = $tcm0_w->slice("(0),$slice1")) .= $tdm0d->_whichND->slice("(0),");
	($tmp   = $tcm0_w->slice("(1),$slice1")) .= $ci;
	($tmp   = $tcm0_v->slice("$slice1"))     .= $tdm0d->_nzvals;
	$nzi += $n;
      }
    }
    my $tcm0 = PDL::CCS::Nd->newFromWhich($tcm0_w,$tcm0_v,dims=>pdl($map->_dtype,$NT,$NC),missing=>0)->dummy(0,1)->sumover;
    $map->{tcm0} = $tcm0;
  }

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

  #my $t_f = $tdm0->xchg(0,1)->sumover;              ##-- ccs: [$ti] -> f($ti)  :: CLOBBERED BELOW!
  my ($tw,$tmp);
  if ($termWeight eq 'uniform') {
    $tw = ones($NT);                                ##-- identity weighting
  }
  elsif ($termWeight eq 'max-entropy-quotient') {
    ##-- weight terms by doc max-entropy (see e.g. Berry(1995); Nakov, Popova, & Mateev (2001))
    #my $tdm0   = $map->{tdm0};                     ##-- ccs: [$ti,$di] -> f($ti,$di)
    #my $t_f     = $tdm0->xchg(0,1)->sumover;        ##-- ccs: [$ti] -> f($ti)
    my $t_f     = $tdm0->xchg(0,1)->sumover->decode; ##-- pdl: [$ti] -> f($ti) : faste
    my $td_pdgt = $tdm0->shadow(                     ##-- ccs: [$ti,$di] -> p($di|$ti)
				which=>$tdm0->_whichND,
				vals=>$tdm0->_vals->pdl
			       );
    ($tmp=$td_pdgt->_nzvals) /= $t_f->index($tdm0->_whichND->slice("(0),"));
    $tw         = $td_pdgt->log->_missing(0);       ##-- ccs: [$ti,$di] -> ln p($di|$ti)
    my $twvals  = $tw->_vals;
    $twvals    /= log(2);                           ##                  -> log p($di|$ti)
    #$tw       *= $td_pdgt;                         ##                  -> p($di|$ti) * log p($di|$ti) [ccs]
    $twvals    *= $td_pdgt->_vals;                  ##                  -> p($di|$ti) * log p($di|$ti) [dense]
    $tw         = $tw->xchg(0,1)->sumover->todense; ##-- pdl: [$ti] ->  -H(Doc|T=$ti)
    $tw        /= log($ND)/log(2);                  ##-- pdl: [$ti] ->  -H(Doc|T=$ti)/Hmax(Doc)
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
    $tw         = $tw->xchg(0,1)->sumover->todense; ##-- pdl: [$ti] -> -H(Doc|T=$ti)
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

##----------------------------------------------
## $map = $map->compile_types(%opts)
##  + converts compiled piddles to target types @$map{qw(vtype itype))} [downgrade only]
##  + %opts:
##     label  => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##     upgrade => $bool,  ##-- allow upgrading types? (default=false)
##     vtype => $vtype,   ##-- target value-type (default=$map->vtype)
##     itype => $itype,   ##-- target index-type (default=$map->itype)
##     refs => \@refs,    ##-- target pdl-refs to convert (default: all PDLs in values %$map)
sub compile_types {
  my ($map,%opts) = @_;
  my $label = $map->labelString(%opts);
  my $itype = $opts{itype} // $map->itype;
  my $vtype = $opts{vtype} // $map->vtype;
  my $upgrade = $opts{upgrade} // 0;
  $map->vlog('info', "compile_types() [$label]: vtype=$vtype ; itype=$itype ; upgrade=$upgrade");

  ##-- convert: piddles
  my $refs = $opts{refs} // $map->object_pdlrefs();
  foreach (@$refs) {
    if (UNIVERSAL::isa($$_,'PDL::CCS::Nd')) {
      $map->convert_pdl(\($$_->_whichND), $itype,$upgrade);
      $map->convert_pdl(\($$_->_vals),    $vtype,$upgrade);
    } else {
      $map->convert_pdl($_, $vtype,$upgrade);
    }
  }

  return $map;
}

## \@refs = $map->object_pdlrefs()
## \@refs = $map->object_pdlrefs(\%obj)
sub object_pdlrefs {
  my ($map,$obj) = @_;
  $obj //= $map;
  return [
	  map  { \$obj->{$_} }
	  grep { ref($obj->{$_}) && (UNIVERSAL::isa($obj->{$_},'PDL') || UNIVERSAL::isa($obj->{$_},'PDL::CCS::Nd')) }
	  keys %$obj
	 ]
}

## \$pdl = $map->convert_pdl(\$pdl, $dst_type, $allow_upgrade=0)
##  + converts \$pdl to target type $dst_type (downgrade only unless $allow_upgrade is true)
sub convert_pdl {
  my ($map,$pdlr,$type,$upgrade) = @_;
  $$pdlr = $$pdlr->convert($type) if ($upgrade ? ($$pdlr->type != $type) : ($$pdlr->type > $type));
  return $pdlr;
}

##==============================================================================
## Methods: DEBUG: Recovery

##----------------------------------------------
## $tdm0 = $map->get_tdm0()
##  + computes & re-caches $map->{tcm0}: CCS::Nd: ($NT x $ND) [Term x Doc -> Freq] from $map->{tdm0}
##  + requires $map->{tdm}, $map->{tw}
sub get_tdm0 {
  my $map = shift;
  return $map->{tdm0} if (defined($map->{tdm0}));

  ##-- correct but expensive
  #return $map->{tdm0} = ($map->{tdm} / $map->{tw})->_missing(0)->exp - $map->{smoothf};

  ##-- faster & memory-friendlier (shares whichND with $tdm)
  my $tdm   = $map->{tdm};
  my $tdm0  = $tdm->shadow(which=>$tdm->_whichND, vals=>$tdm->_vals->pdl, missing=>$map->{smoothf});
  my $nzv0  = $tdm0->_nzvals;
  $nzv0    /= $map->{tw}->index($tdm0->_whichND->slice("(0),"));
  $nzv0->inplace->exp();
  $nzv0   -= $map->{smoothf};
  return $map->{tdm0} = $tdm0;
}

##----------------------------------------------
## $tf0 = $map->get_tf0()
##  + computes & (re-)caches tf0: dense: ($NT) [Term -> Freq] from $map->{tdm0}
sub get_tf0 {
  my $map = shift;
  return $map->{tf0} if (defined($map->{tf0}));
  return $map->{tf0} = $map->get_tdm0()->xchg(0,1)->sumover->todense;
}

##----------------------------------------------
## $tdf0 = $map->get_tdf0()
##  + computes & (re-)caches tdn0: CCS::Nd: ($NT x $NC) [Term -> NDocs] from $map->{tdm0}
##  + requires $map->{tdm}, $map->{tw}
sub get_tdf0 {
  my $map = shift;
  return $map->{tdf0} if (defined($map->{tdf0}));
  my $tdn = $map->get_tdm0()->pdl();
  (my $tmp = $tdn->_nzvals) .= ($tdn->_nzvals >= 1);
  return $map->{tdf0} = $tdn->xchg(0,1)->sumover->todense->convert($map->_ftype);
}

##----------------------------------------------
## $tw0 = $map->get_tw0()
##  + computes & re-caches $map->{tw0} from @$map{qw(tw twRaw twCooked)}
sub get_tw0 {
  my $map = shift;
  return $map->{tw0} if (defined($map->{tw0}));
  return $map->{tw0} = $map->{tw} if (($map->{twRaw}//0)==0 && ($map->{twCooked}//1)==1);
  return $map->{tw0} = ($map->{tw} - ($map->{twRaw}//0)) / ($map->{twCooked}//1);
}

##----------------------------------------------
## $doc_wt = $map->get_doc_wt()
##  + computes & re-caches $map->{doc_wt} from $map->{tdm0}
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
		       ? null->convert($map->_itype)
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
    ##-- correct but expensive
    #$map->{tcm0} = ($map->{tcm} / $map->{tw})->_missing(0)->exp - $map->{smoothf};

    ##-- faster & memory-friendlier (shares whichND with $tcm)
    my $tcm = $map->{tcm};
    my $tcm0 = $tcm->shadow(which=>$tcm->_whichND, vals=>$tcm->_vals->pdl, missing=>$map->{smoothf});
    my $nzv0 = $tcm0->_nzvals;
    $nzv0   /= $map->{tw}->index($tcm0->_whichND->slice("(0),"));
    $nzv0->inplace->exp();
    $nzv0   -= $map->{smoothf};
    $map->{tcm0} = $tcm0;
  }
  else {
    ##-- create CCS::Nd
    #$map->vlog('info', "get_tcm0(): matrix: tcm0: PDL::CCS::Nd") if ($map->{verbose});

    ##-- step 0: get sparse boolean dcmb [Doc x Cat -> Bool]
    my $dcm    = $map->{dcm};
    my $dcmb   = $dcm->shadow(which=>$dcm->_whichND, vals=>ones(ushort,$dcm->_vals->dims))->_missing(0);
    my $doc_nc = $dcmb->xchg(0,1)->sumover->decode; ##-- [$di_local] -> $ncats

    ##-- check for determistic (doc->cat) mappings
    if ($doc_nc->max == 1) {
      ##-- optimize for determistic (doc->cat) mapping
      $map->vlog('info', "get_tcm0(): deterministic (Doc->Cat) mapping detected") if ($map->{verbose});

      ##-- save some memory
      undef $dcmb;
      undef $doc_nc;

      my ($tmp);
      my $tdm0 = $map->get_tdm0();
      my $d2c  = zeroes($map->itype, $dcm->dim(0)); ##-- pdl (ND) : [$di] => $ci_local
      ($tmp=$d2c->index($dcm->_whichND->slice("(0),"))) .= $dcm->_whichND->slice("(1),");
      my $which0 = $tdm0->_whichND->pdl;
      my $vals0  = $tdm0->_vals;
      ($tmp=$which0->slice("(1),")) .= $d2c->index($which0->slice("(1),"));
      my $which  = $which0->uniqvec;
      my $nzvals = zeroes($tdm0->type, $which->dim(1));
      $tdm0->_nzvals->indadd( $which0->vsearchvec($which), $nzvals );
      my $tcm0   = PDL::CCS::Nd->newFromWhich($which,$nzvals->append($tdm0->missing),steal=>1);
      $map->{tcm0} = $tcm0;
    }
    else {
      ##-- non-deterministic (doc->cat) mapping: do it the old (hard and slow) way
      $map->vlog('info', "get_tcm0(): non-deterministic (Doc->Cat) mapping detected") if ($map->{verbose});

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
      my $tcm0_w = zeroes($map->_itype, 2,$nnz);
      my $tcm0_v = zeroes($map->_vtype,   $nnz);

      my ($di,$tdm0d,$ci,$n,$slice1,$slice2);
      my $nzi = 0;
      foreach $di ($d_nzimin->xvals->where(!$d_nzinull)->list) {
	$n = $d_ptrlen->at($di);
	$slice2 = $d_nzimin->at($di).":".$d_nzimax->at($di);
	foreach $ci (
		     #$dcmb->slice("($di)")->which->list
		     $dcmb->dice_axis(0,$di)->_whichND->slice("(1),")->list
		    ) {
	  $slice1 = $nzi.':'.($nzi+$n-1);
	  $tcm0_w->slice("(0),$slice1") .= $tdm0_which->slice("(0),$slice2");
	  $tcm0_w->slice("(1),$slice1") .= $ci;
	  $tcm0_v->slice("$slice1")     .= $tdm0_nzvals->slice("$slice2");
	  $nzi += $n;
	}
      }
      my $tcm0_dims = pdl($map->_dtype,$map->{tenum}->size,$map->{lcenum}->size);
      my $tcm0 = PDL::CCS::Nd->newFromWhich($tcm0_w,$tcm0_v,dims=>$tcm0_dims,missing=>0)->dummy(0,1)->sumover;
      $map->{tcm0} = $tcm0;
    }
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
