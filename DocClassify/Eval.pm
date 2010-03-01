## -*- Mode: CPerl -*-
## File: DocClassify::Eval.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: evaluator (test vs. mapped)


package DocClassify::Eval;
use DocClassify::Object;
use DocClassify::Logger;
use DocClassify::Utils ':all';
use DocClassify::Document;

use PDL;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Object DocClassify::Logger Exporter);

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
##  cat2eval => \%c2e,      ##-- ($catName => \%catEval)
##                          ##   where:
##                          ##       %catEval = ( $which=>$n, class=>$class )
##                          ##   for $which =~ /^pr|rc|F|(tp|fp|fn)$/,
##                          ##   and $class one of ('null', 'safe', or 'unsafe')
##  geval => \%g2e,         ##-- ($globalEvalMode => \%globalEval)
##                          ##   where $globalEvalMode is of the form "${gclass}.${ghow}",
##                          ##   for $gclass in:
##                          ##      'all'      : all parsed cats
##                          ##      'nz'       : all non-"null" cats (see 'nullCat' option)
##                          ##      'nz_safe'  : all non-null cats with (tp+fn) >  ($pSafe*$Ndocs)
##                          ##      'nz_unsafe': all non-null cats with (tp+fn) <= ($pSafe*$Ndocs)
##                          ##   and ${ghow} in ('total','avg')
##  Ndocs => $Ndocs,        ##-- total number of docs
##  psafe => $frac,         ##-- minimum relative frequency of "safe" cats (default=0.025=2.5%)
##  nullCat => $catName,    ##-- name of 'null' cat; set to a non-cat for none
##                          ##   + default='(auto)': cat w/ largest (tp+fn)
##  errors => \%errs,       ##-- {"${catName1}\t${catName2}" => \%c12errors,
##                          ##   where:
##                                 $c12errors = {ndocs=>$ndocs,fdocs=>$fdocs}
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
			       geval => {},
			       Ndocs => 0,
			       Nbytes => 0,
			       errors => {},
			       psafe => 0.025,
			       nullCat => '(auto)',

			       ##-- user options
			       @_,
			      );
  $eval->{nullCat0} = $eval->{nullCat} if (!$eval->{nullCat0}); ##-- save requested nullCat

  return $eval;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(label1 label2 lab2docs cat2eval Ndocs Nbytes errors)
sub noShadowKeys {
  return qw(lab2docs cat2eval Ndocs Nbytes errors);
}

## $eval = $eval->clear()
##  + clears all evaluation data, including label1, label2
sub clear {
  my $eval = shift;
  %{$eval->{lab2docs}} = qw();
  %{$eval->{cat2eval}} = qw();
  %{$eval->{geval}} = qw();
  $eval->{Ndocs} = 0;
  $eval->{label1} = undef;
  $eval->{label2} = undef;
  %{$eval->{errors}} = qw();
  $eval->{nullCat} = $eval->{nullCat0};
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
  ##   + also populates $eval->{errors}{ndocs}
  ##   + global eval $eval->{geval} is NOT populated
  my ($docs, $cats1,$cats2, $cat1,$cat2);
  my ($ename);
  foreach $docs (values(%$l2doc)) {
    ($doc1,$doc2) = @$docs;
    ($cats1,$cats2) = (scalar($doc1->cats),scalar($doc2->cats));
    ($cat1,$cat2) = ($cats1->[0],$cats2->[0]);          ##-- exclusive membership only!
    if ($cat1->{name} eq $cat2->{name}) {
      $eval->{cat2eval}{$cat1->{name}}{tp}++;      ##-- true positive for $cat1,$cat2
      $cat1->{evalClass} = 'tp1';
      $cat2->{evalClass} = 'tp2';
    } else {
      $eval->{cat2eval}{$cat1->{name}}{fn}++;      ##-- false negative for $cat1
      $eval->{cat2eval}{$cat2->{name}}{fp}++;      ##-- false positive for $cat2
      $cat1->{evalClass} = 'fn';
      $cat2->{evalClass} = 'fp';
      ##
      $ename = $cat1->{name}."\t".$cat2->{name};
      $eval->{errors}{$ename}{ndocs}++;
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
  my @addKeys = qw(tp fp fn);
  while (($cat,$chash2)=each(%{$eval2->{cat2eval}})) {
    $chash1 = $c2e->{$cat};
    $chash1 = $c2e->{$cat} = {} if (!defined($chash1));
    $chash1->{$_} += ($chash2->{$_}||0) foreach (@addKeys);
  }

  ##-- add: errors
  my ($ekey,$err);
  while (($ekey,$err)=each(%{$eval2->{errors}})) {
    $eval1->{errors}{$ekey}{ndocs} += ($err->{ndocs}||0);
  }

  return $eval1;
}

##==============================================================================
## Methods: Compilation

## $eval = $eval->compile()
##  + (re-)compiles $eval->{Ndocs} from $eval->{cat2eval} (tp+fn)
##  + (re-)compiles $eval->{nullCat} if it was given as '(auto)'
##  + (re-)compiles $eval->{cat2eval}{$catName} pr,rc,F values
##  + (re-)compiles $eval->{geval} global evaluation modes
##  + (re-)compiles $eval->{errors}{qw(cat1 cat2 fdocs)}: cat-wise errors (fractional)
sub compile {
  my $eval = shift;

  ##-- variables
  my $c2e  = $eval->{cat2eval};
  my ($cname,$c);

  ##-- compile cat-wise pr,rc,F and $eval->{Ndocs}
  my $Ndocs = 0;
  foreach (values %$c2e) {
    $_->{tp} ||= 0;
    $_->{fp} ||= 0;
    $_->{fn} ||= 0;
    prF($_);
    $Ndocs += $_->{tp} + $_->{fn};
  }
  $eval->{Ndocs} = $Ndocs;

  ##-- compile $eval->{nullCat} if requested (i.e. '(auto)')
  if (defined($eval->{nullCat}) && $eval->{nullCat} eq '(auto)') {
    my $nNull = -1;
    while (($cname,$c)=each(%$c2e)) {
      if ( ($c->{tp}+$c->{fn}) > $nNull ) {
	$eval->{nullCat} = $cname;
	$nNull = ($c->{tp}+$c->{fn});
      }
    }
  }

  ##-- compile cat classes
  while (($cname,$c)=each(%$c2e)) {
    my $nsafe = ($eval->{psafe}||0) * $Ndocs;
    if (defined($eval->{nullCat}) &&$cname eq $eval->{nullCat}) {
      $c->{class} = 'null';
    } elsif ($c->{tp}+$c->{fn} > $nsafe) {
      $c->{class} = 'safe';
    } else {
      $c->{class} = 'unsafe';
    }
  }

  ##-- compile errors: get total number of errors
  my $nerrs = 0;
  $nerrs += ($_->{ndocs}||0) foreach (values(%{$eval->{errors}}));

  ##-- compile errors: cat1,cat2,fdocs
  my ($ekey,$e);
  while (($ekey,$e) = each(%{$eval->{errors}})) {
    @$e{qw(cat1 cat2)} = split(/\t/,$ekey,2);
    $e->{fdocs} = ($e->{ndocs}||0) / $nerrs;
  }

  ##-- compile errors: nested
  my $errs1 = $eval->{errors1} = {};  ##-- $catName1 => @errors with wanted=$catName1
  my $errs2 = $eval->{errors2} = {};  ##-- $catName2 => @errors with got=$catName2
  foreach $e (values %{$eval->{errors}}) {
    push(@{$errs1->{$e->{cat1}}},$e);
    push(@{$errs2->{$e->{cat2}}},$e);
  }

  ##-- compile globals
  $eval->compileGlobals('all');
  $eval->compileGlobals('nz',denyClassRe=>qr/^null$/);
  $eval->compileGlobals('nz_safe',allowClassRe=>qr/^safe$/);
  $eval->compileGlobals('nz_safe1',allowClassRe=>qr/^safe$/, error1ClassRe=>qr/^(?:null|safe)$/);
  $eval->compileGlobals('nz_unsafe',allowClassRe=>qr/^unsafe$/);

  ##-- return
  return $eval;
}

## $eval = $eval->compileGlobals($gclass, %opts)
##  + compiles $eval->{geval}{"$gclass.total","$gclass.avg"} from $eval->{cat2eval}
##  + called by compile()
##  + %opts:
##     allowClassRe => $regex,       ##-- only allow cats $c with $c->{class} matching $regex
##     denyClassRe  => $regex,       ##-- deny cats $c with $c->{class} matching $regex
##     error1ClassRe => $regex,      ##-- only allow errors $e with $cat2eval->{$e->{cat1}}{class} matching $regex
sub compileGlobals {
  my ($eval,$gclass,%opts) = @_;
  my $allowRe = $opts{allowClassRe};
  my $denyRe  = $opts{denyClassRe};
  my $e1re    = $opts{error1ClassRe};

  ##-- compile: get allowed cats
  my %cats = qw();
  my ($name,$c);
  while (($name,$c)=each(%{$eval->{cat2eval}})) {
    next if (defined($allowRe) && $c->{class} !~ $allowRe);
    next if (defined($denyRe) && $c->{class} =~ $denyRe);
    $cats{$name}=$c;
  }
  my @cats = values(%cats);

  ##-- compile: total mode
  my $etotal = $eval->{geval}{"$gclass.total"} = {};
  my ($tp,$fp,$fn, $e);
  foreach $c (@cats) {
    ($tp,$fp,$fn) = @$c{qw(tp fp fn)};
    if (defined($e1re)) {
      ##-- TODO!
      ;
    }
    $etotal->{tp} += $tp;
    $etotal->{fp} += $fp;
    $etotal->{fn} += $fn;
  }
  prF($etotal);

  ##-- compile: average mode
  my $eavg = $eval->{geval}{"$gclass.avg"} = {};
  my $ncats = scalar(@cats);
  foreach $c (@cats) {
    $eavg->{tp} += 1/$ncats * ($c->{tp}||0);
    $eavg->{fp} += 1/$ncats * ($c->{fp}||0);
    $eavg->{fn} += 1/$ncats * ($c->{fn}||0);
    $eavg->{pr} += 1/$ncats * precision($c);
    $eavg->{rc} += 1/$ncats * recall($c);
  }
  prF($eavg);

  return $eval;
}


## $bool = $eval->compiled()
##  + returns true iff pr,rc,F have been compiled
##  + really just checks for non-empty $eval->{geval}
sub compiled {
  return scalar(%{$_[0]{geval}});
}

## $eval = $eval->uncompile()
##  + deletes $eval->{cat2eval}{$catName} pr,rc,F values
##  + clears $eval->{geval}
##  + re-sets $eval->{nullCat}
sub uncompile {
  my $eval = shift;
  my @delkeys = qw(pr rc F);
  delete(@$_{@delkeys}) foreach (values(%{$eval->{cat2eval}}));
  %{$eval->{geval}} = qw();
  $eval->{nullCat} = $eval->{nullCat0};
  return $eval;
}

##==============================================================================
## Methods: Utilities

## $frac = PACKAGE::frac($num,$denom)
##  + treats 0/0 == 0
sub frac {
  my ($num,$denom) = @_;
  return $denom!=0 ? ($num/$denom) : ($num==0 ? 1 : 0);
  #return $denom!=0 ? ($num/$denom) : 0;
}

## $pr = PACKAGE::precision(\%catEvalHash,$units)
sub precision {
  my $hash = shift;
  return $hash->{pr} if (defined($hash->{pr}));
  my $tp = $hash->{tp} || 0;
  my $fp = $hash->{fp} || 0;
  return $hash->{pr} = frac($tp, ($tp+$fp));
}

## $rc = PACKAGE::recall(\%catEvalHash,$units)
sub recall {
  my $hash = shift;
  return $hash->{"rc"} if (defined($hash->{"rc"}));
  my $tp = $hash->{tp} || 0;
  my $fn = $hash->{fn} || 0;
  return $hash->{"rc"} = frac($tp, ($tp+$fn));
}

## $F = PACKAGE::F(\%catEvalHash,$units)
sub F {
  my $hash = shift;
  return $hash->{"F"} if (defined($hash->{"F"}));
  my ($pr,$rc) = (precision($hash),recall($hash));
  return $hash->{"F"} = frac(2.0, ($pr**-1 + $rc**-1));
}

## ($pr,$rc,$F) = PACKAGE::prF(\%catEvalHash,$unit)
sub prF {
  return (precision(@_),recall(@_),F(@_));
}

## $str = PACKAGE::reportStr($label,\%catEvalHash, %opts)
##  + %opts:
##     llen=>$fmtLen, ##-- default=24 #48
##     plen=>$fmtLen, ##-- default='4.1'
##     ilen=>$fmtLen, ##-- default='4',
sub reportStr {
  my ($lab,$hash,%opts) = @_;
  my ($llen,$plen,$ilen) = @opts{qw(llen plen ilen)};
  #$llen ||= 48;
  $llen ||= 24;
  $plen ||= '5.1';
  $ilen ||= 4;
  prF($hash);
  return (join('  ',
	       sprintf("%-${llen}s:", $lab),
	       #(map {sprintf("$_=%${ilen}d", ($hash->{$_}||0))} qw(tp fp fn)),
	       (map {"$_=".sistr(($hash->{$_}||0),'d',$ilen,' ')} qw(tp fp fn)),
	       ':',
	       (map {sprintf("$_=%${plen}f", 100*($hash->{$_}||0))} qw(pr rc F))
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
##  + %opts:
##     nErrors => $n,  ##-- number of errors to save (default=10)
sub saveTextFile {
  my ($eval,$file,%opts) = @_;
  $eval->compile if (!$eval->compiled);

  my $fh = ref($file) ? $file : IO::File->new(">$file");
  confess(ref($eval)."::saveTextFile: open failed for '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8') if ($fh->can('binmode'));

  ##-- brief report
  $fh->print(ref($eval).": ".($eval->{label}||'(no label)').": Summary\n");
  my %ropts = (llen=>20);
  my ($gmode);
  foreach $gmode (sort(keys(%{$eval->{geval}}))) {
    $fh->print(reportStr(" : $gmode", $eval->{geval}{$gmode}, %ropts));
  }

  ##-- error report
  $opts{nErrors} = 10 if (!defined($opts{nErrors}));
  if ($opts{nErrors} > 0) {
    my @errs = sort {$b->{ndocs} <=> $a->{ndocs}} values(%{$eval->{errors}});
    my $nerrs = ($opts{nErrors} < @errs ? $opts{nErrors} : @errs);
    my $sfmt  = "%-32s";
    my $dfmt  = "%6d";
    my $ffmt  = "%6.2f";
    $fh->print(" + Top $nerrs error types (WANTED -> GOT = NDOCS (% ERRS))\n",
	       (map {
		 sprintf("   ~ $sfmt -> $sfmt = $dfmt ($ffmt%%)\n", @$_{qw(cat1 cat2 ndocs)}, 100*$_->{fdocs})
	       } (@errs[0..($nerrs-1)])),
	      );
  }

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
  my $g2e = $eval->{geval};

  ##-- global data
  my $g_root =$data_root->addNewChild(undef,'global');
  my ($gmode,$ge,$gnode);
  foreach $gmode (sort keys %$g2e) {
    $ge = $g2e->{$gmode};
    $gnode = $g_root->addNewChild(undef,'mode');
    $gnode->setAttribute('name', $gmode);
    $gnode->setAttribute($_,$ge->{$_}) foreach (qw(tp fp fn pr rc F));
  }

  ##-- category eval data
  my $ce_root = $data_root->addNewChild(undef,'by-category');
  my $ced_node = $ce_root->addNewChild(undef,'by-n-docs');
  ##
  my ($c_name,$c_hash,$c_node);
  foreach $c_name (grep {$_ ne ''} sort(keys(%$c2e))) {
    $c_hash = $c2e->{$c_name};
    ##
    $c_node = $ced_node->addNewChild(undef,'cat');
    $c_node->setAttribute('name',$c_name);
    $c_node->setAttribute($_, ($c_hash->{$_}||0)) foreach (qw(tp fp fn pr rc F));
  }

  ##-- error data
  my $errs_node = $data_root->addNewChild(undef,'errors');
  my ($err,$e_node);
  foreach $err (sort {$b->{ndocs} <=> $a->{ndocs}} values(%{$eval->{errors}})) {
    $e_node = $errs_node->addNewChild(undef,'error');
    $e_node->setAttribute('cat1',$err->{cat1});
    $e_node->setAttribute('cat2',$err->{cat2});
    $e_node->setAttribute('ndocs',$err->{ndocs});
    $e_node->setAttribute('fdocs',$err->{fdocs});
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

## $xpvalue = xpvalue($node_or_doc,$xpath)
sub xpvalue {
  my ($node,$xp) = @_;
  my $val = scalar($node->findnodes($xp));
  return undef if (!defined($val));
  $val = $val->[0] if (UNIVERSAL::isa($val,'ARRAY'));
  $val = $val->value if (defined($val) && UNIVERSAL::can($val,'value'));
  return $val;
}

## $eval = $CLASS_OR_OBJECT->loadXmlDoc($xdoc,%opts)
##  + (re-)loads corpus data from $xdoc
sub loadXmlDoc {
  my ($that,$xdoc,%opts) = @_;
  my $eval = ref($that) ? $that->clear : $that->new(%opts);

  ##-- load: label
  my $root = $xdoc->documentElement;
  $eval->{label} = $root->getAttribute('label');

  ##-- load: header data
  my $head=xpvalue($root,'head[1]');
  $eval->{Ndocs} = xpvalue($head,'corpora[1]/@Ndocs');
  ##
  $eval->{label1} = xpvalue($head,'corpora[1]/corpus[1]/@label');
  $eval->{label2} = xpvalue($head,'corpora[1]/corpus[2]/@label');

  ##-- load: data
  my $data_root = xpvalue($root,'data[1]');
  my $c2e = $eval->{cat2eval};

  ##-- data: eval: global
  my $g_root = xpvalue($data_root,'global[1]');
  my $g2e = $eval->{geval};
  my ($g_node,$g_name);
  foreach $g_node (@{$data_root->findnodes('./mode')}) {
    $g_name = xpvalue($g_node,'./@name');
    $g2e->{$g_name}{$_} = ($g_node->getAttribute($_)||0) foreach (qw(tp fp fn pr rc F));
  }

  ##-- data: eval: by category
  my ($ce_unit,$ce_node, $c_node,$c_name);
  $ce_unit = 'docs';
  $ce_node = xpvalue($data_root,'by-category[1]/by-n-docs[1]');
  foreach $c_node (@{$ce_node->findnodes('cat')}) {
    $c_name = $c_node->getAttribute('name');
    #next if ($c_name eq ''); ##-- IGNORE
    $c2e->{$c_name}{$_} = $c_node->getAttribute($_) foreach (qw(tp fp fn pr rc F));
  }

  ##-- data: eval: errors
  my $errs_root = xpvalue($data_root,'errors[1]');
  if (defined($errs_root)) {
    my ($e_node,$err);
    foreach $e_node (@{$errs_root->findnodes('error')}) {
      $err = {};
      $err->{$_} = $e_node->getAttribute($_) foreach (qw(cat1 cat2 ndocs fdocs));
      $eval->{errors}{$err->{cat1}."\t".$err->{cat2}} = $err;
    }
  }

  ##-- data: eval: documents
  my $lab2docs = $eval->{lab2docs};
  my $docs_node = xpvalue($data_root,'by-document[1]');
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
	  if (0 && defined($cat->{proto}) && ($cat->{proto} ne $cat->{name})) {
	    ##-- handle pseudo-cats (e.g. "(null)") literally
	    $cat->{target} = $cat->{name};
	    $cat->{name} = $cat->{proto};
	    $cat->{id} = 0;
	  }
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
