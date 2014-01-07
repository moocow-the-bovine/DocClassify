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
##  verbose => $level,     ##-- verbosity level (0..4), default=2
##  ##
##  ##-- low-level data
##  lab2docs => \%lab2docs, ##-- $docLabel => [$doc1,$doc2]
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
##  pairs => \%pairs,       ##-- {"${catName1}\t${catName2}" => \%pairInfo,
##                          ##   + where \%pairInfo = {ndocs=>$ndocs, cat1=>$c1,cat2=>$cat2,...}
##  cat2info => \%cat2info, ##-- $catName=>{class=>$class,tp=>$tp_total,fp=>$fp_total,fn=>$fn_total,...}
##                          ##   + \%catInfo=>(class=>$class,...)
##                          ##   + $class is one of ('null', 'safe', or 'unsafe')
##  #...
sub new {
  my $that = shift;
  my $eval = $that->SUPER::new(
			       ##-- source options
			       label=>'',
			       label1 => undef,
			       label2 => undef,
                               verbose => 2,

			       ##-- low-level data
			       lab2docs => {},
			       geval => {},
			       Ndocs => 0,
			       Nbytes => 0,
			       pairs => {},
			       cat2info => {},
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
##  + override returns qw(label1 label2 lab2docs Ndocs Nbytes pairs cat2info)
sub noShadowKeys {
  return qw(lab2docs Ndocs Nbytes pairs cat2eval cat2info);
}

## $eval = $eval->clear()
##  + clears all evaluation data, including label1, label2
sub clear {
  my $eval = shift;
  %{$eval->{lab2docs}} = qw();
  %{$eval->{cat2info}} = qw();
  %{$eval->{geval}} = qw();
  $eval->{Ndocs} = 0;
  $eval->{label1} = undef;
  $eval->{label2} = undef;
  %{$eval->{pairs}} = qw();
  $eval->{nullCat} = $eval->{nullCat0};
  return $eval;
}

##==============================================================================
## Methods: Corpus Comparison

## $eval = $eval->compare($corpus1,$corpus2,%opts)
##  + $corpus1: "wanted" corpus (gold-standard)
##  + $corpus2: "got" corpus (mapper output)
##  + %opts: overrides %$eval
##  + updates $eval->{lab2docs}, $eval->{pairs}
sub compare {
  my ($eval,$corpus1,$corpus2,%opts) = @_;
  @$eval{keys(%opts)} = values(%opts);
  $eval->debug("compare(", ($corpus1->{label}||'nolabel1'), ", ", ($corpus2->{label}||'nolabel2'), ")");

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

  ##-- update $eval->{pairs}{"$cat1\t$cat2"}{ndocs}: compare docs by label
  ##   + global eval $eval->{geval}, $eval->{cat2info}, etc. are NOT populated
  my $pairs = $eval->{pairs};
  my ($docs, $cats1,$cats2, $cat1,$cat2);
  my ($pname);
  foreach $docs (values(%$l2doc)) {
    ($doc1,$doc2) = @$docs;
    ($cats1,$cats2) = (scalar($doc1->cats),scalar($doc2->cats));
    ($cat1,$cat2) = ($cats1->[0],$cats2->[0]);     ##-- exclusive membership only!
    $pname = $cat1->{name}."\t".$cat2->{name};
    $pairs->{$pname}{ndocs}++;
  }

  return $eval;
}

##==============================================================================
## Methods: Cross-Validation

## $eval1 = $eval1->addEval($eval2)
##  + adds evaluation data {lab2docs}, {pairs}{*}{ndocs} from $eval2 to $eval1
sub addEval {
  my ($eval1,$eval2) = @_;
  $eval1->uncompile() if ($eval1->compiled);
  $eval1->debug("addEval()");

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

  ##-- add: pairs (ndocs)
  my $pairs1 = $eval1->{pairs};
  my ($pkey,$pval);
  while (($pkey,$pval)=each(%{$eval2->{pairs}})) {
    $pairs1->{$pkey}{ndocs} += ($pval->{ndocs}||0);
  }

  return $eval1;
}

##==============================================================================
## Methods: Compilation

## $eval = $eval->compile()
##  + (re-)compiles eval data from $eval->{pairs}{"$c1\t$c2"}}{ndocs}
##    - $eval->{Ndocs}
##    - $eval->{nullCat} if it was given as '(auto)'
##    - $eval->{geval} global evaluation modes
##    - $eval->{cat2info} basic cat info
##    - #$eval->{errors}{qw(cat1 cat2 fdocs)}: cat-wise errors (fractional)
sub compile {
  my $eval = shift;
  $eval->debug("compile()");

  ##-- variables
  my $pairs = $eval->{pairs};
  my ($pkey,$pval);
  my ($cname,$c);

  ##-- compile Ndocs, cat1, cat2
  my $Ndocs = 0;
  while (($pkey,$pval)=each(%$pairs)) {
    @$pval{qw(cat1 cat2)} = split(/\t/,$pkey,2);
    $pval->{ndocs} ||= 0;
    $Ndocs += $pval->{ndocs};
  }
  $eval->{Ndocs} = $Ndocs;
  $eval->logwarn("compile(): WARNING: Ndocs=0") if ($Ndocs==0);

  ##-- compile cat2info: tp,fp,fn
  my $c2i  = $eval->{cat2info};
  while (($pkey,$pval)=each(%$pairs)) {
    if ($pval->{cat1} eq $pval->{cat2}) {
      $c2i->{$pval->{cat1}}{tp} += $pval->{ndocs};
    } else {
      $c2i->{$pval->{cat1}}{fn} += $pval->{ndocs};
      $c2i->{$pval->{cat2}}{fp} += $pval->{ndocs};
    }
  }

  ##-- compile cat2info: pr,rc,F
  foreach (values %$c2i) {
    $_->{tp} ||= 0;
    $_->{fp} ||= 0;
    $_->{fn} ||= 0;
    prF($_); ##-- superseded by {geval}{'all.avg'}{bycat}, but compiled here just in case
  }

  ##-- compile $eval->{nullCat} if requested (i.e. '(auto)')
  $eval->debug("compile(): nullCat='".($eval->{nullCat}||'(none)')."'") if ($eval->{verbose}>=3);
  if (defined($eval->{nullCat}) && $eval->{nullCat} eq '(auto)') {
    my $nNull = -1;
    while (($cname,$c)=each(%$c2i)) {
      if ( ($c->{tp}+$c->{fn}) > $nNull ) {
	$eval->{nullCat} = $cname;
	$nNull = ($c->{tp}+$c->{fn});
      }
    }
    $eval->debug("compile(): nullCat->'".($eval->{nullCat}||'(none)')."' (n=$nNull ~ ".sprintf("%.1f%%",100*$nNull/$Ndocs).")")
      if ($eval->{verbose}>=3);
  }

  ##-- compile: cat classes
  my $nsafe = ($eval->{psafe}||0) * $Ndocs;
  while (($cname,$c)=each(%$c2i)) {
    if (defined($eval->{nullCat}) &&$cname eq $eval->{nullCat}) {
      $c->{class} = 'null';
    } elsif ($c->{tp}+$c->{fn} > $nsafe) {
      $c->{class} = 'safe';
    } else {
      $c->{class} = 'unsafe';
    }
  }

  ##-- compile: pair classes
  while (($pkey,$pval)=each(%$pairs)) {
    $pval->{class1} = $c2i->{$pval->{cat1}}{class};
    $pval->{class2} = $c2i->{$pval->{cat2}}{class};
    $pval->{class} = "$pval->{class1}-$pval->{class2}";
  }

  ##-- compile globals
  $eval->compileGlobals('all');
  $eval->compileGlobals('nz',           denyAvgRe=>qr/^null$/);
  $eval->compileGlobals('nz_picky',     denyPairRe=>qr/^null-/); #qr/^null-|-null$/
  $eval->compileGlobals('safe',         allowAvgRe=>qr/^safe$/);
  $eval->compileGlobals('safe_picky',   allowPairRe=>qr/^safe-safe$/, allowAvgRe=>qr/^safe$/);
  $eval->compileGlobals('unsafe',       allowAvgRe=>qr/^unsafe$/);
  $eval->compileGlobals('unsafe_picky', allowPairRe=>qr/^unsafe-unsafe$/, allowAvgRe=>qr/^unsafe$/);

  ##-- compile errors
  $eval->compileErrors();

  ##-- return
  return $eval;
}

BEGIN {
  #*compileErrors = \&compileErrors_total;
  *compileErrors = \&compileErrors_nz_avg_F;
}

## $eval = $eval->compileErrors_nz_avg_F()
##  + compiles @$p{qw(fdocs ferrs)} for each pair $p in $eval->{pairs}
##  + this version compiles $p->{ferrs} as fraction of 'avg.total' errors
##  + requires $eval->{geval}{'all.avg'}{bycat}{qw(tp fp fn pr rc F)}
sub compileErrors_nz_avg_F {
  my $eval = shift;

  ##-- get global precision, recall weights for 'nz.avg.F'
  my $geval = $eval->{geval}{'nz.avg'};
  my $bycat = $geval->{bycat};
  my $gw_pr = Fw_pr(@$geval{qw(pr rc)});
  my $gw_rc = Fw_rc(@$geval{qw(pr rc)});

  ##-- get class-wise fraction of total nz.avg.F errors to $c->{ferrs_pr}, $c->{ferrs_rc}
  my $ncats = scalar grep {$_->{class} ne 'null'} values %$bycat;
  my ($c);
  foreach $c (values %$bycat) {
    if ($c->{class} eq 'null') {
      $c->{ferrs_pr} = $c->{ferrs_rc} = 0; ##-- no direct error contribution
    } else {
      $c->{ferrs_pr} = $gw_pr * $c->{fp}/($geval->{fp}*$ncats);
      $c->{ferrs_rc} = $gw_rc * $c->{fn}/($geval->{fn}*$ncats);
    }
  }

  ##-- now get fraction of class-wise errors to $p->{ferrs1}, $p->{ferrs2}, $p->{ferrs}
  my $ndocs = $eval->{Ndocs};
  my ($pval,$c1,$c2);
  foreach $pval (values %{$eval->{pairs}}) {
    ($c1,$c2) = @$pval{qw(cat1 cat2)};
    if ($c1 eq $c2) {
      $pval->{ferrs1}=$pval->{ferrs2}=0;
    } else {
      $pval->{ferrs1} = ($pval->{ndocs}||0) / $bycat->{$c1}{fn};
      $pval->{ferrs2} = ($pval->{ndocs}||0) / $bycat->{$c2}{fp};
    }
    $pval->{ferrs} = $pval->{ferrs1}*$bycat->{$c1}{ferrs_rc} + $pval->{ferrs2}*$bycat->{$c2}{ferrs_pr};
    ##
    ##-- also compile basic 'fdocs'
    $pval->{fdocs} = ($pval->{ndocs}||0) / $ndocs;
  }

  return $eval;
}

## $eval = $eval->compileErrors_total()
##  + compiles @$p{qw(fdocs ferrs)} for each pair $p in $eval->{pairs}
##  + this version compiles $p->{ferrs} as fraction of 'avg.total' errors
sub compileErrors_total {
  my $eval = shift;

  ##-- errors: get total number of errors
  my $nerrs = 0;
  $nerrs += ($_->{ndocs}||0) foreach (grep {$_->{cat1} ne $_->{cat2}} values %{$eval->{pairs}});
  my $ndocs = $eval->{Ndocs};
  $eval->{nerrs} = $nerrs;

  ##-- pairs: fdocs, ferrs
  my ($pval);
  foreach $pval (values %{$eval->{pairs}}) {
    $pval->{fdocs} = ($pval->{ndocs}||0) / $ndocs;
    $pval->{ferrs} = ($pval->{cat1} eq $pval->{cat2} ? 0 : (($pval->{ndocs}||0)/$nerrs));
  }

  return $eval;
}


## $eval = $eval->compileGlobals($gclass, %opts)
##  + compiles $eval->{geval}{"$gclass.total","$gclass.avg"} from $eval->{cat2eval}
##  + called by compile()
##  + %opts:
##     allowPairRe  => $regex,  ##-- only  count pairs $p with $p->{pclass} =~ $regex
##     denyPairRe   => $regex,  ##-- don't count pairs $p with $p->{pclass} =~ $regex
##     allowAvgRe   => $regex,  ##-- only  average classes $c with $cname =~ $regex
##     denyAvgRe    => $regex,  ##-- don't average classes $c with $cname =~ $regex
sub compileGlobals {
  my ($eval,$gclass,%opts) = @_;
  ##
  my $allowPairRe = $opts{allowPairRe} || qr/./;
  my $denyPairRe  = $opts{denyPairRe}  || qr/^$/;
  ##
  my $allowAvgRe = $opts{allowAvgRe} || qr/./;
  my $denyAvgRe  = $opts{denyAvgRe}  || qr/^$/;

  ##-- compile: CLASS.total: {bycat}
  my $etotal = $eval->{geval}{"$gclass.total"} = { bycat=>{} };
  my ($pkey,$pval,$cat1,$cat2);
  while (($pkey,$pval)=each(%{$eval->{pairs}})) {
    ($cat1,$cat2) = @$pval{qw(cat1 cat2)};
    next if ($pval->{class} !~ $allowPairRe || $pval->{class} =~ $denyPairRe);
    if ($cat1 eq $cat2) {
      $etotal->{bycat}{$cat1}{tp} += $pval->{ndocs};
    } else {
      $etotal->{bycat}{$cat1}{fn} += $pval->{ndocs};
      $etotal->{bycat}{$cat2}{fp} += $pval->{ndocs};
    }
  }

  ##-- compile: CLASS.total: tp,fp,fn, pr,rc,F
  my @cats = qw();
  my ($cname,$c);
  while (($cname,$c)=each(%{$etotal->{bycat}})) {
    $c->{class} = $eval->{cat2info}{$cname}{class} || '';
    next if ($c->{class} !~ $allowAvgRe || $c->{class} =~ $denyAvgRe);
    prF($c);
    $etotal->{tp} += $c->{tp};
    $etotal->{fp} += $c->{fp};
    $etotal->{fn} += $c->{fn};
    push(@cats,$c);
  }
  prF($etotal);

  ##-- compile: CLASS.avg
  my $eavg = $eval->{geval}{"$gclass.avg"} = { bycat=>$etotal->{bycat} };
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
##  + really just checks for non-empty $eval->{cat2info}
sub compiled {
  return scalar(%{$_[0]{cat2info}});
}

## $eval = $eval->uncompile()
##  + clears $eval->{cat2eval}
##  + clears $eval->{geval}
##  + re-sets $eval->{nullCat}
sub uncompile {
  my $eval = shift;
  %{$eval->{cat2eval}} = qw();
  %{$eval->{geval}} = qw();
  $eval->{nullCat} = $eval->{nullCat0};
  return $eval;
}

sub dbg_bycat {
  my ($eval,$bycat) = @_;
  my ($c);
  my $llen = 36;
  foreach $c (sort DocClassify::Utils::catcmp keys %$bycat) {
    print STDERR
      (sprintf("EVAL: %-${llen}s : ", $c),
       join(' ', map {sprintf("$_=%4d",($bycat->{$c}{$_}||0))} qw(tp fp fn)),
       ' : ',
       join(' ', map {sprintf("$_=%6.2f",(100*$bycat->{$c}{$_}||0))} qw(pr rc F)),
       "\n",
      );
  }
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

## $pr = PACKAGE::precision(\%catEvalHash)
sub precision {
  my $hash = shift;
  return $hash->{pr} if (defined($hash->{pr}));
  my $tp = $hash->{tp} = $hash->{tp} || 0;
  my $fp = $hash->{fp} = $hash->{fp} || 0;
  return $hash->{pr} = frac($tp, ($tp+$fp));
}

## $rc = PACKAGE::recall(\%catEvalHash)
sub recall {
  my $hash = shift;
  return $hash->{"rc"} if (defined($hash->{"rc"}));
  my $tp = $hash->{tp} = $hash->{tp} || 0;
  my $fn = $hash->{fn} = $hash->{fn} || 0;
  return $hash->{"rc"} = frac($tp, ($tp+$fn));
}

## $avg = pravg(\%catEvalHash)
sub pravg {
  my $hash = shift;
  return $hash->{'a'} if (defined($hash->{'a'}));
  return $hash->{'a'} = (precision($hash)+recall($hash))/2.0;
}

## $F = PACKAGE::_F($pr,$rc)
sub _F {
  return frac(2.0, ($_[0]**-1 + $_[1]**-1));
}

## $F = PACKAGE::F(\%catEvalHash)
sub F {
  my $hash = shift;
  return $hash->{"F"} if (defined($hash->{"F"}));
  my ($pr,$rc) = (precision($hash),recall($hash));
  return $hash->{"F"} = _F($pr,$rc);
}

## ($pr,$rc,$F,$pravg) = PACKAGE::prF(\%catEvalHash)
##  + sets {qw(pr rc F a)} from {qw(tp fp fn)}
sub prF {
  return (precision(@_),recall(@_),F(@_),pravg(@_));
}

## $weight_pr = PACKAGE::Fw_pr($pr,$rc)
##  + relative contribution of precision errors to F errors
sub Fw_pr {
  my ($pr,$rc) = @_;
  return _F(1-$pr,1) / (_F(1-$pr,1)+_F(1,1-$rc));
}
## $weight_rc = PACKAGE::Fw_rc($pr,$rc)
##  + relative contribution of recall errors to F errors
sub Fw_rc {
  my ($pr,$rc) = @_;
  return _F(1,1-$rc) / (_F(1-$pr,1)+_F(1,1-$rc));
}


## $str = PACKAGE::reportStr($label,\%catEvalHash, %opts)
##  + %opts:
##     llen=>$fmtLen, ##-- default=24 #48
##     plen=>$fmtLen, ##-- default='4.1'
##     ilen=>$fmtLen, ##-- default='4',
##     counts => $bool, ##-- print counts? (default=0)
sub reportStr {
  my ($lab,$hash,%opts) = @_;
  my ($llen,$plen,$ilen,$counts) = @opts{qw(llen plen ilen counts)};
  #$llen ||= 48;
  $llen ||= 24;
  $plen ||= '5.1';
  $ilen ||= 4;
  prF($hash);
  return (join('  ',
	       sprintf("%-${llen}s:", $lab),
	       ##(map {sprintf("$_=%${ilen}d", ($hash->{$_}||0))} qw(tp fp fn)),
	       ($counts ? ((map {sprintf("$_=%${ilen}d", $hash->{$_})} qw(tp fp fn)), ':') : qw()),
	       ##
	       (map {sprintf("$_=%${plen}f", 100*($hash->{$_}||0))} qw(pr rc F a))
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
##     nErrors => $n,    ##-- number of errors to save (default=10)
##     counts  => $bool, ##-- dump counts? (default=0)
##     cats    => $bool, ##-- dump cat-wise summaries (default=0)
sub saveTextFile {
  my ($eval,$file,%opts) = @_;
  $eval->compile if (!$eval->compiled);

  my $fh = ref($file) ? $file : IO::File->new(">$file");
  confess(ref($eval)."::saveTextFile: open failed for '$file': $!") if (!defined($fh));
  #$fh->binmode(':utf8') if ($fh->can('binmode'));

  ##-- brief report
  my $label = $eval->{label} || '(no label)';
  $fh->print(ref($eval).": $label: Summary\n");
  my %ropts = (%opts,llen=>22);
  my ($gmode);
  foreach $gmode (sort(keys(%{$eval->{geval}}))) {
    $fh->print(reportStr(" : $gmode", $eval->{geval}{$gmode}, %ropts));
  }

  ##-- category summary
  if ($opts{cats}) {
    $fh->print(" + $label: summary by category:\n");
    my $bycat = $eval->{geval}{'all.avg'}{bycat};
    my $catlen = 36;
    my $classlen = 6;
    my ($c);
    foreach $c (sort DocClassify::Utils::catcmp keys %$bycat) {
      $fh->print(join('  :  ',
		      sprintf("   > %-${catlen}s", $c),
		      sprintf("%${classlen}s", $eval->{cat2info}{$c}{class}),
		      ($opts{counts} ? join(' ', map {sprintf("$_=%4d",($bycat->{$c}{$_}||0))} qw(tp fp fn)) : qw()),
		      join(' ', map {sprintf("$_=%6.2f",(100*$bycat->{$c}{$_}||0))} qw(pr rc F a)),
		     ),
		 "\n",
		);
    }
  }

  ##-- error report
  $opts{nErrors} = 10 if (!defined($opts{nErrors}));
  if ($opts{nErrors} != 0) {
    #my @errs = sort {$b->{ndocs} <=> $a->{ndocs}} grep {$_->{cat1} ne $_->{cat2}} values(%{$eval->{pairs}});
    my @errs = sort {$b->{ferrs} <=> $a->{ferrs}} values(%{$eval->{pairs}});
    my $nerrs = ($opts{nErrors} > 0 && $opts{nErrors} < @errs ? $opts{nErrors} : @errs);
    my $sfmt  = "%-36s";
    my $dfmt  = "%6d";
    my $ffmt  = "%6.2f";
    $fh->print(" + $label: top $nerrs error types (WANTED -> GOT = NDOCS (% NZ_AVG_F_ERRS))\n",
	       (map {
		 sprintf("   ~ $sfmt -> $sfmt = $dfmt ($ffmt%%)\n", @$_{qw(cat1 cat2 ndocs)}, 100*$_->{ferrs})
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
  my $c2i = $eval->{cat2info};
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
  my $ce_root  = $data_root->addNewChild(undef,'by-category');
  my ($ceg_node,$ge_bycat, $c_name,$c_hash,$c_node);
  foreach $gmode (sort keys %$g2e) {
    $ge_bycat = $g2e->{$gmode}{bycat};
    next if (!defined($ge_bycat));
    $ceg_node = $ce_root->addNewChild(undef,'mode');
    $ceg_node->setAttribute('name',$gmode);
    foreach $c_name (sort keys %$ge_bycat) {
      $c_hash = $ge_bycat->{$c_name};
      $c_node = $ceg_node->addNewChild(undef,'cat');
      $c_node->setAttribute('name',$c_name);
      $c_node->setAttribute($_, ($c_hash->{$_}||0)) foreach (qw(class tp fp fn pr rc F));
    }
  }

  ##-- pair data
  my $pairs_node = $data_root->addNewChild(undef,'pairs');
  my ($pair,$p_node);
  foreach $pair (sort {$b->{ndocs} <=> $a->{ndocs}} values(%{$eval->{pairs}})) {
    $p_node = $pairs_node->addNewChild(undef,'pair');
    $p_node->setAttribute($_,$pair->{$_}) foreach (qw(cat1 cat2 class1 class2 ndocs fdocs ferrs));
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
##  + %opts:
##     nocompile=>$bool,  ##-- if true, suppresses implicit compile()
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

  ##-- load: data: pairs
  my ($p_node,$pkey,$pval);
  foreach $p_node (@{$root->findnodes('data[1]/pairs[1]/pair')}) {
    $pval = { (map {($_=>$p_node->getAttribute($_))} qw(cat1 cat2 ndocs)) };
    $pkey = $pval->{cat1}."\t".$pval->{cat2};
    $eval->{pairs}{$pkey} = $pval;
  }

  ##-- data: eval: documents (if available)
  my $lab2docs = $eval->{lab2docs};
  my $docs_node = xpvalue($root,'data[1]/by-document[1]');
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

  ##-- implicit compile
  $eval->compile() if (!$opts{nocompile});

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
