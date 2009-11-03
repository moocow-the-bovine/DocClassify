## -*- Mode: CPerl -*-
## File: DocClassify::Eval.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: evaluator (test vs. mapped)


package DocClassify::Eval;
use DocClassify::Object;
use DocClassify::Utils ':all';
use DocClassify::Document;

use PDL;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Object Exporter);

our @EXPORT = qw();
our %EXPORT_TAGS =
  (
   utils =>[qw(frac precision recall prF)],
  );
our @EXPORT_OK = map {@$_} values(%EXPORT_TAGS);
$EXPORT_TAGS{all} = [@EXPORT_OK];


##==============================================================================
## Constructors etc.

## $eval = $CLASS_OR_OBJ->new(%opts)
## %$eval, %opts:
##  ##
##  ##-- user options
##  label  => $label,      ##-- optional label (root attribute; default='')
##  label1 => $label1,     ##-- label for "wanted" corpus (gold-standard; default="wanted")
##  label2 => $label2,     ##-- label for "got" corpus (mapper output; default="got")
##  ##
##  ##-- low-level data
##  lab2docs => \%lab2docs, ##-- $docLabel => [$doc1,$doc2]
##  cat2eval => \%c2e,      ##-- ($catName => \%catEval, ''=>\%globalEval)
##                          ##   where:
##                          ##       %catEval = ( $which=>$n )
##                          ##   for    $which =~ /^pr|rc|F|((tp|fp|fn)_(docs|bytes))$/, and:
##                          ##   global $which =~ /^((pr|rc|F)(_avg?))|((tp|fp|fn)_(docs|bytes)))$/
##  Ndocs => $Ndocs,        ##-- total number of docs
##  Nbytes => $Nbytes,      ##-- total number of bytes
##  #...
sub new {
  my $that = shift;
  my $eval = $that->SUPER::new(
			       ##-- source options
			       label=>'',
			       label1 => undef,
			       label2 => undef,

			       ##-- low-level data
			       lab2docs => {},
			       cat2eval => {},
			       Ndocs => 0,
			       Nbytes => 0,

			       ##-- user options
			       @_,
			      );

  return $eval;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(label1 label2 lab2docs cat2eval Ndocs Nbytes)
sub noShadowKeys {
  return qw(lab2docs cat2eval Ndocs Nbytes);
}

## $eval = $eval->clear()
##  + clears all evaluation data, including label1, label2
sub clear {
  my $eval = shift;
  %{$eval->{lab2docs}} = qw();
  %{$eval->{cat2eval}} = qw();
  $eval->{Ndocs} = 0;
  $eval->{Nbytes} = 0;
  $eval->{label1} = undef;
  $eval->{label2} = undef;
  return $eval;
}

##==============================================================================
## Methods: Corpus Comparison

## $eval = $eval->compare($corpus1,$corpus2,%opts)
##  + $corpus1: "wanted" corpus (gold-standard)
##  + $corpus2: "got" corpus (mapper output)
##  + %opts: overrides %$eval
sub compare {
  my ($eval,$corpus1,$corpus2,%opts) = @_;
  @$eval{keys(%opts)} = values(%opts);

  ##-- labels
  $eval->{label1} = $corpus1->{label} if (!defined($eval->{label1}));
  $eval->{label2} = $corpus2->{label} if (!defined($eval->{label2}));
  my ($clab1,$clab2) = @$eval{qw(label1 label2)};

  ##-- populate $l2doc = $eval->{lab2docs} = { $docLabel=>[$doc1,$doc2], ... } : shallow copy docs
  my $l2doc = $eval->{lab2docs};
  $l2doc->{$_->label}[0] = $_->shadow(%$_)->clearCache foreach (@{$corpus1->{docs}});
  $l2doc->{$_->label}[1] = $_->shadow(%$_)->clearCache foreach (@{$corpus2->{docs}});

  ##-- %$l2doc: sanity checks
  my ($doc1,$doc2);
  foreach (values(%$l2doc)) {
    ($doc1,$doc2) = @$_;
    if (defined($doc1) && !defined($doc2)) {
      warn(ref($eval)."::eval(): document label '".$doc1->label."' not defined for corpus2='$clab2' -- ignoring");
      delete($l2doc->{$doc1->label});
    }
    elsif (!defined($doc1) && defined($doc2)) {
      warn(ref($eval)."::eval(): document label '".$doc2->label."' not defined for corpus1='$clab1' -- ignoring");
      delete($l2doc->{$doc2->label});
    }
  }

  ##-- populate $c2e = $eval->{cat2eval}: tp,fp,fn: compare docs by label
  my ($docs, $cats1,$cats2, $cat1,$cat2);
  foreach $docs (values(%$l2doc)) {
    ($doc1,$doc2) = @$docs;
    ($cats1,$cats2) = (scalar($doc1->cats),scalar($doc2->cats));
    ($cat1,$cat2) = ($cats1->[0],$cats2->[0]);                     ##-- exclusive membership only!
    if ($cat1->{name} eq $cat2->{name}) {
      $eval->true_positive($cat1->{name},$doc1);
      $cat1->{evalClass} = 'tp1';
      $cat2->{evalClass} = 'tp2';
    } else {
      $eval->false_negative($cat1->{name},$doc1);
      $eval->false_positive($cat2->{name},$doc1);
      $cat1->{evalClass} = 'fn';
      $cat2->{evalClass} = 'fp';
    }
  }

  return $eval;
}

##==============================================================================
## Methods: Cross-Validation

## $eval1 = $eval1->addEval($eval2)
##  + adds evaluation data from $eval2 to $eval1
sub addEval {
  my ($eval1,$eval2) = @_;
  $eval1->uncompile() if ($eval1->compiled);

  ##-- add: lab2docs
  my $l2d = $eval1->{lab2docs};
  my ($lab,$docs);
  while (($lab,$docs)=each(%{$eval2->{lab2docs}})) {
    if (exists($l2d->{$lab})) {
      confess(ref($eval1)."::addEval(): label '$lab' already exists!");
      return undef;
    }
    $l2d->{$lab} = [@$docs];
  }

  ##-- add: c2e
  my $c2e = $eval1->{cat2eval};
  my ($cat,$chash1,$chash2);
  my @addKeys = map {("${_}_docs","${_}_bytes")} qw(tp fp fn);
  while (($cat,$chash2)=each(%{$eval2->{cat2eval}})) {
    $chash1 = $c2e->{$cat};
    $chash1 = $c2e->{$cat} = {} if (!defined($chash1));
    $chash1->{$_} += ($chash2->{$_}||0) foreach (@addKeys);
  }

  ##-- add: Ndocs, Nbytes
  $eval1->{Ndocs}  += $eval2->{Ndocs};
  $eval1->{Nbytes} += $eval2->{Nbytes};

  return $eval1;
}

##==============================================================================
## Methods: Compilation

## $eval = $eval->compile()
##  + (re-)compiles $eval->{cat2eval}{$catName} pr,rc,F values, also $eval->{cat2eval}{''} doc-wise and byte-wise average values
sub compile {
  my $eval = shift;
  ##
  my $c2e   = $eval->{cat2eval};
  my $Ncats = scalar(keys(%$c2e))-1;
  my $eg    = $c2e->{''};
  $eg       = $c2e->{''} = {} if (!defined($eg));
  delete(@$eg{map {($_."_docs",$_."_bytes",$_."_docs_avg",$_."_bytes_avg")} qw(pr rc F)});
  my ($c);
  foreach (grep {$_ ne ''} keys(%$c2e)) {
    $c = $c2e->{$_};
    ##
    $eg->{tp_docs_avg} += 1/$Ncats * ($c->{tp_docs}||0);
    $eg->{fp_docs_avg} += 1/$Ncats * ($c->{fp_docs}||0);
    $eg->{fn_docs_avg} += 1/$Ncats * ($c->{fn_docs}||0);
    $eg->{pr_docs_avg} += 1/$Ncats * precision($c,'docs');
    $eg->{rc_docs_avg} += 1/$Ncats * recall($c,'docs');
    ##
    $eg->{tp_bytes_avg} += 1/$Ncats * ($c->{tp_bytes}||0);
    $eg->{fp_bytes_avg} += 1/$Ncats * ($c->{fp_bytes}||0);
    $eg->{fn_bytes_avg} += 1/$Ncats * ($c->{fn_bytes}||0);
    $eg->{pr_bytes_avg} += 1/$Ncats * precision($c,'bytes');
    $eg->{rc_bytes_avg} += 1/$Ncats * recall($c,'bytes');
    ##
    prF($c,$_) foreach (qw(docs bytes)); ##-- ensure pr,rc,F computed
  }
  $eg->{F_docs_avg}  = F($eg,'docs_avg');
  $eg->{F_bytes_avg} = F($eg,'bytes_avg');
  prF($eg,$_) foreach (qw(docs bytes)); ##-- ensure pr,rc,F computed

  return $eval;
}

## $bool = $eval->compiled()
##  + returns true iff pr,rc,F have been compiled
##  + really just checks for $eval->{cat2eval}{''}{F_docs}
sub compiled {
  return defined($_[0]{cat2eval}{''}{F_docs});
}

## $eval = $eval->uncompile()
##  + un-compiles $eval->{cat2eval}{$catName} pr,rc,F values
##  + un-compiles $eval->{cat2eval}{''} doc-wise and byte-wise average values
sub uncompile {
  my $eval = shift;
  my @delkeys = map {("${_}_docs", "${_}_bytes", "${_}_docs_avg", "${_}_bytes_avg")} qw(pr rc F);
  delete(@$_{@delkeys}) foreach (values(%{$eval->{cat2eval}}));
  return $eval;
}

##==============================================================================
## Methods: Utilities

## $eval = $eval->true_positive($catName,$doc1)
sub true_positive {
  my ($eval,$cat,$doc) = @_;
  $eval->{cat2eval}{$cat}{tp_docs}++;
  $eval->{cat2eval}{$cat}{tp_bytes} += $doc->sizeBytes;
  $eval->{cat2eval}{''}{tp_docs}++;
  $eval->{cat2eval}{''}{tp_bytes} += $doc->sizeBytes;
  $eval->{Ndocs}++;
  $eval->{Nbytes} += $doc->sizeBytes;
  return $eval;
}

## $eval = $eval->false_negative($catName,$doc1)
sub false_negative {
  my ($eval,$cat,$doc) = @_;
  $eval->{cat2eval}{$cat}{fn_docs}++;
  $eval->{cat2eval}{$cat}{fn_bytes} += $doc->sizeBytes;
  $eval->{cat2eval}{''}{fn_docs}++;
  $eval->{cat2eval}{''}{fn_bytes} += $doc->sizeBytes;
  $eval->{Ndocs}++;
  $eval->{Nbytes} += $doc->sizeBytes;
  return $eval;
}

## $eval = $eval->false_positive($catName,$doc2)
sub false_positive {
  my ($eval,$cat,$doc) = @_;
  $eval->{cat2eval}{$cat}{fp_docs}++;
  $eval->{cat2eval}{$cat}{fp_bytes} += $doc->sizeBytes;
  $eval->{cat2eval}{''}{fp_docs}++;
  $eval->{cat2eval}{''}{fp_bytes} += $doc->sizeBytes;
  #$eval->{Ndocs}++;                      ##-- counted by false_negative()
  #$eval->{Nbytes} += $doc->sizeBytes;    ##-- counted by false_negative()
  return $eval;
}

## $frac = PACKAGE::frac($num,$denom)
##  + treats 0/0 == 1
sub frac {
  my ($num,$denom) = @_;
  #return $denom!=0 ? ($num/$denom) : ($num==0 ? 1 : 0);
  return $denom!=0 ? ($num/$denom) : 0;
}

## $pr = PACKAGE::precision(\%catEvalHash,$units)
sub precision {
  my ($hash,$unit) = @_;
  $unit = 'docs' if (!defined($unit));
  return $hash->{"pr_${unit}"} if (defined($hash->{"pr_${unit}"}));
  my $tp = $hash->{"tp_${unit}"} || 0;
  my $fp = $hash->{"fp_${unit}"} || 0;
  return $hash->{"pr_${unit}"} = frac($tp, ($tp+$fp));
}

## $rc = PACKAGE::recall(\%catEvalHash,$units)
sub recall {
  my ($hash,$unit) = @_;
  $unit = 'docs' if (!defined($unit));
  return $hash->{"rc_${unit}"} if (defined($hash->{"rc_${unit}"}));
  my $tp = $hash->{"tp_${unit}"} || 0;
  my $fn = $hash->{"fn_${unit}"} || 0;
  return $hash->{"rc_${unit}"} = frac($tp, ($tp+$fn));
}

## $F = PACKAGE::F(\%catEvalHash,$units)
sub F {
  my ($hash,$unit) = @_;
  $unit = 'docs' if (!defined($unit));
  return $hash->{"F_${unit}"} if (defined($hash->{"F_${unit}"}));
  my ($pr,$rc) = (precision(@_),recall(@_));
  return $hash->{"F_${unit}"} = frac(2.0, ($pr**-1 + $rc**-1));
}

## ($pr,$rc,$F) = PACKAGE::prF(\%catEvalHash,$unit)
sub prF {
  return (precision(@_),recall(@_),F(@_));
}

## $str = PACKAGE::reportStr($label,\%catEvalHash,$unit, %opts)
##  + %opts:
##     llen=>$fmtLen, ##-- default=24 #48
##     plen=>$fmtLen, ##-- default='4.1'
##     ilen=>$fmtLen, ##-- default='4',
sub reportStr {
  my ($lab,$hash,$unit,%opts) = @_;
  my ($llen,$plen,$ilen) = @opts{qw(llen plen ilen)};
  #$llen ||= 48;
  $llen ||= 24;
  $plen ||= '5.1';
  $ilen ||= 4;
  prF($hash,$unit);
  return (join('  ',
	       sprintf("%-${llen}s:", $lab),
	       #(map {sprintf("$_=%${ilen}d", ($hash->{$_.'_'.$unit}||0))} qw(tp fp fn)),
	       (map {"$_=".sistr(($hash->{$_.'_'.$unit}||0),'d',$ilen,' ')} qw(tp fp fn)),
	       ':',
	       (map {sprintf("$_=%${plen}f", 100*($hash->{$_.'_'.$unit}||0))} qw(pr rc F))
	      )."\n");
}


##==============================================================================
## Methods: I/O

##--------------------------------------------------------------
## Methods: I/O: generic

## $mode_hash_or_name = $CLASS_OR_OBJ->defaultIoMode()
##  + returns default I/O mode for object
##  + override returns 'xml'
sub defaultIoMode { return 'xml'; }

##--------------------------------------------------------------
## Methods: I/O: Ascii (output only)

## $eval = $eval->saveTextFile($file_or_fh,%opts)
sub saveTextFile {
  my ($eval,$file,%opts) = @_;
  $eval->compile if (!$eval->compiled);

  my $fh = ref($file) ? $file : IO::File->new(">$file");
  confess(ref($eval)."::saveTextFile: open failed for '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8') if ($fh->can('binmode'));

  ##-- brief report
  my $ge = $eval->{cat2eval}{''};
  my %ropts = (llen=>20);
  $fh->print(ref($eval).": ".($eval->{label}||'(no label)').": Summary\n",
	     reportStr(" : Docs  : Total", $ge, 'docs', %ropts),
	     reportStr(" : Docs  : Average", $ge, 'docs_avg', %ropts),
	     reportStr(" : Bytes : Total", $ge, 'bytes', %ropts),
	     reportStr(" : Bytes : Average", $ge, 'bytes_avg', %ropts),
	    );
  return $fh;
}


##--------------------------------------------------------------
## Methods: I/O: XML: save

## $xdoc = $eval->saveXmlDoc(%opts)
##  + creates xml document from $eval
##  + %opts:
##     saveDocs => $bool,  ##-- whether to save raw document list (default=true)
sub saveXmlDoc {
  my ($eval,%opts) = @_;
  $eval->compile() if (!$eval->compiled); ##-- ensure compiled

  my $xdoc = XML::LibXML::Document->new('1.0','UTF-8');
  $xdoc->setDocumentElement($xdoc->createElement('eval'));
  my $root = $xdoc->documentElement;

  ##-- root attributes
  $root->setAttribute('label',  ($eval->{label}||ref($eval)));

  ##-- evaluated corpora
  my $head = $root->addNewChild(undef,'head');
  my $corpus_root = $head->addNewChild(undef,'corpora');
  $corpus_root->setAttribute('Ndocs',  ($eval->{Ndocs}||0));
  $corpus_root->setAttribute('Nbytes', ($eval->{Nbytes}||0));
  ##
  my $corpus_node = $corpus_root->addNewChild(undef,'corpus');
  $corpus_node->setAttribute('n',1);
  $corpus_node->setAttribute('type','wanted');
  $corpus_node->setAttribute('label',($eval->{label1}||''));
  ##
  $corpus_node = $corpus_root->addNewChild(undef,'corpus');
  $corpus_node->setAttribute('n',2);
  $corpus_node->setAttribute('type','got');
  $corpus_node->setAttribute('label',($eval->{label2}||''));

  ##-- data root
  my $data_root = $root->addNewChild(undef,'data');
  my $c2e = $eval->{cat2eval};

  ##-- global average eval data
  my $ae_root = $data_root->addNewChild(undef,'average');
  my $aed_node = $ae_root->addNewChild(undef,'by-n-docs');
  my $aeb_node = $ae_root->addNewChild(undef,'by-n-bytes');
  $aed_node->setAttribute($_, ($c2e->{''}{$_."_docs_avg"}||0)) foreach (qw(pr rc F));
  $aeb_node->setAttribute($_, ($c2e->{''}{$_."_bytes_avg"}||0)) foreach (qw(pr rc F));

  ##-- global total eval data
  my $te_root = $data_root->addNewChild(undef,'total');
  my $ted_node = $te_root->addNewChild(undef,'by-n-docs');
  my $teb_node = $te_root->addNewChild(undef,'by-n-bytes');
  $ted_node->setAttribute($_, ($c2e->{''}{$_."_docs"}||0)) foreach (qw(tp fp fn pr rc F));
  $teb_node->setAttribute($_, ($c2e->{''}{$_."_bytes"}||0)) foreach (qw(tp fp fn pr rc F));

  ##-- category eval data
  my $ce_root = $data_root->addNewChild(undef,'by-category');
  my $ced_node = $ce_root->addNewChild(undef,'by-n-docs');
  my $ceb_node = $ce_root->addNewChild(undef,'by-n-bytes');
  ##
  my ($c_name,$c_hash,$c_node);
  foreach $c_name (grep {$_ ne ''} sort(keys(%$c2e))) {
    $c_hash = $c2e->{$c_name};
    ##
    $c_node = $ced_node->addNewChild(undef,'cat');
    $c_node->setAttribute('name',$c_name);
    $c_node->setAttribute($_, ($c_hash->{$_."_docs"}||0)) foreach (qw(tp fp fn pr rc F));
    ##
    $c_node = $ceb_node->addNewChild(undef,'cat');
    $c_node->setAttribute('name',$c_name);
    $c_node->setAttribute($_, ($c_hash->{$_."_bytes"}||0)) foreach (qw(tp fp fn pr rc F));
  }

  ##-- document-pair list
  if ($opts{saveDocs} || !defined($opts{saveDocs})) {
    my $docs_node = $data_root->addNewChild(undef,'by-document');
    my ($d_label,$doc1,$doc2, $d_node, $cats,$cat, $cats_node1,$cats_node2);
    foreach $d_label (sort keys %{$eval->{lab2docs}}) {
      ($doc1,$doc2) = @{$eval->{lab2docs}{$d_label}};
      $d_node = $docs_node->addNewChild(undef,'doc');
      $d_node->setAttribute('label',$doc1->label) if (defined($doc1->{file}) && $doc1->label ne $doc1->{file});
      $d_node->setAttribute('file',$doc1->{file}) if (defined($doc1->{file}));
      $d_node->setAttribute('bytes',$doc1->sizeBytes);
      #$d_node->setAttribute('sigFile',$doc->{sigFile}) if (defined($doc->{sigFile}));

      $cats_node1 = $d_node->addNewChild(undef,'cats');
      $cats_node1->setAttribute('n',1);
      $cats_node1->setAttribute('type','wanted');
      foreach $cat (@{$doc1->cats}) {
	$c_node = $cats_node1->addNewChild(undef,'cat');
	$c_node->setAttribute($_,(defined($cat->{$_}) ? $cat->{$_} : '')) foreach (sort(keys(%$cat)));
      }
      ##
      $cats_node2 = $d_node->addNewChild(undef,'cats');
      $cats_node2->setAttribute('n',2);
      $cats_node2->setAttribute('type','got');
      foreach $cat (@{$doc2->cats}) {
	$c_node = $cats_node2->addNewChild(undef,'cat');
	$c_node->setAttribute($_,(defined($cat->{$_}) ? $cat->{$_} : '')) foreach (sort(keys(%$cat)));
      }
    }
  }
  ##-- done
  return $xdoc;
}

## $bool = $eval->saveXmlFile($filename_or_fh,%opts)
##  + save to XML file
##  + %opts:
##     format=>$level,  ##-- default=1
sub saveXmlFile {
  my ($eval,$file,%opts) = @_;
  my $xdoc = $eval->saveXmlDoc(%opts);
  my $fmt = defined($opts{format}) ? $opts{format} : 1;
  my $rc = ref($file) ? $xdoc->toFH($file,$fmt) : $xdoc->toFile($file,$fmt);
  return $rc;
}

## $str = $eval->saveXmlString(%opts)
##  + save to XML string
##  + %opts:
##     format=>$level,  ##-- default=1
sub saveXmlString {
  my ($eval,%opts) = @_;
  my $xdoc = $eval->saveXmlDoc(%opts);
  return $xdoc->toString(defined($opts{format}) ? $opts{format} : 1);
}

##--------------------------------------------------------------
## Methods: I/O: XML: load

## $eval = $CLASS_OR_OBJECT->loadXmlDoc($xdoc,%opts)
##  + (re-)loads corpus data from $xdoc
sub loadXmlDoc {
  my ($that,$xdoc,%opts) = @_;
  my $eval = ref($that) ? $that->clear : $that->new(%opts);

  ##-- load: label
  my $root = $xdoc->documentElement;
  $eval->{label} = $root->getAttribute('label');

  ##-- load: header data
  my $head=$root->findnodes('head[1]')->[0];
  $eval->{$_} = $head->findnodes('corpora[1]/@'.$_)->[0]->value foreach (qw(Ndocs Nbytes));
  ##
  $eval->{label1} = $head->findnodes('corpora[1]/corpus[1]/@label')->[0]->value;
  $eval->{label2} = $head->findnodes('corpora[1]/corpus[2]/@label')->[0]->value;

  ##-- load: data
  my $data_root = $root->findnodes('data[1]')->[0];
  my $c2e = $eval->{cat2eval};

  ##-- data: eval: global average
  my $ae_root = $data_root->findnodes('average[1]')->[0];
  $c2e->{''}{$_."_docs_avg"} = $ae_root->findnodes("by-n-docs[1]/\@$_")->[0]->value foreach (qw(pr rc F));
  $c2e->{''}{$_."_bytes_avg"} = $ae_root->findnodes("by-n-bytes[1]/\@$_")->[0]->value foreach (qw(pr rc F));

  ##-- data: eval: global total
  my $te_root = $data_root->findnodes('total[1]')->[0];
  $c2e->{''}{$_."_docs"} = $te_root->findnodes("by-n-docs[1]/\@$_")->[0]->value foreach (qw(tp fp fn pr rc F));
  $c2e->{''}{$_."_bytes"} = $te_root->findnodes("by-n-bytes[1]/\@$_")->[0]->value foreach (qw(tp fp fn pr rc F));

  ##-- data: eval: by category
  my ($ce_unit,$ce_node, $c_node,$c_name);
  foreach $ce_unit (qw(docs bytes)) {
    $ce_node = $data_root->findnodes('by-category[1]/by-n-'.$ce_unit.'[1]')->[0];
    foreach $c_node (@{$ce_node->findnodes('cat')}) {
      $c_name = $c_node->getAttribute('name');
      $c2e->{$c_name}{$_."_".$ce_unit} = $c_node->getAttribute($_) foreach (qw(tp fp fn pr rc F));
    }
  }

  ##-- data: eval: documents
  my $lab2docs = $eval->{lab2docs};
  my $docs_node = $data_root->findnodes('by-document[1]')->[0];
  if (defined($docs_node)) {
    my ($d_node,%d_attrs, $docs, $d_i,$doc,$dc_node,$cat);
    foreach $d_node (@{$docs_node->findnodes('doc')}) {
      %d_attrs = map {($_->name=>$_->value)} $d_node->attributes;
      $docs    = [map {DocClassify::Document->new(%d_attrs)} qw(1 2)];
      $lab2docs->{$docs->[0]->label} = $docs;
      ##
      ##-- parse categories
      foreach $d_i (0..1) {
	$doc = $docs->[$d_i];
	foreach $dc_node (@{$d_node->findnodes('cats[@n="'.($d_i+1).'"][1]/cat')}) {
	  $cat = { map {($_->name=>$_->value)} $dc_node->attributes };
	  push(@{$doc->{cats}},$cat);
	}
	#$doc->cats(); ##-- should already be sorted in eval file
      }
    }
  }

  return $eval;
}

## $bool = $CLASS_OR_OBJ->loadXmlFile($filename_or_fh,%opts)
##  + load from XML file
sub loadXmlFile {
  my ($that,$file,%opts) = @_;
  my $parser = libxmlParser();
  my $xdoc = ref($file) ? $parser->parse_fh($file) : $parser->parse_file($file)
    or confess((ref($that)||$that)."::loadXmlFile(): could not parse '$file': $!");
  return $that->loadXmlDoc($xdoc,%opts);
}

## $eval = $CLASS_OR_OBJECT->loadXmlString($str,%opts)
## $eval = $CLASS_OR_OBJECT->loadXmlString(\$str,%opts)
##  + load from XML string
sub loadXmlString {
  my ($that,$str,%opts) = @_;
  my $parser = libxmlParser();
  my $xdoc = $parser->parse_string(ref($str) ? $$str : $str)
    or confess((ref($that)||$that)."::loadXmlFile(): could not parse string: $!");
  return $that->loadXmlDoc($xdoc);
}



##==============================================================================
## Footer
1;

__END__
