## -*- Mode: CPerl -*-
## File: DocClassify::Signature.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: signature (term frequencies)


package DocClassify::Signature;
use DocClassify::Object;
use DocClassify::Utils ':all';
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Object);

## $CAT_DEG_MAX
##  + maximum category-membership degree
our $CAT_DEG_MAX = 3;

## $CAT_DEG_DEFAULT
##  + default membership degree if unspecified (for loadCsvFile())
our $CAT_DEG_DEFAULT = 1;

##==============================================================================
## Constructors etc.

## $sig = $CLASS_OR_OBJ->new(%opts)
## %$sig, %opts:
##  ##-- base data
##  tf   => \%term2freq,  ##-- raw frequency hash
##  N    => $totalFreq,   ##-- total frequency
##  ##
##  ##-- category information data
##  cat2deg => \%cat2deg, ##-- maps category names to membership degree ($deg>=1, 1 is best)
##  cat2id  => \%cat2id,  ##-- maps category names to loaded IDs (or undef)
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   tf=>{},
			   N=>0,
			   cat2deg=>{},
			   cat2id=>{},
			   @_,
			  );
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(tf N cat2deg)
sub noShadowKeys {
  return qw(tf N cat2deg);
}

##==============================================================================
## Methods: Agglomeration

## $sig = $sig->addSig($sig2)
##  adds frequency data from $sig2
sub addSig {
  my ($sig1,$sig2) = @_;

  ##-- add: frequencies
  $sig1->{N} += $sig2->{N};
  my ($t,$f);
  while (($t,$f)=each(%{$sig2->{tf}})) {
    $sig1->{tf}{$t} += $f;
  }

  ##-- add: category membership data (choose minimum degree: "best")
  my ($c,$d);
  while (($c,$d)=each(%{$sig2->{cat2deg}})) {
    $sig1->{cat2deg}{$c} = min2( ($sig2->{cat2deg}{$c}||$CAT_DEG_MAX), $d );
  }

  ##-- add: category id assignment
  foreach (grep {!defined($sig1->{cat2id}{$_})} keys(%{$sig2->{cat2id}})) {
    $sig1->{cat2id}{$_} = $sig2->{cat2id}{$_};
  }

  ##-- return
  return $sig1;
}

##==============================================================================
## Methods: I/O


##--------------------------------------------------------------
## Methods: I/O: CSV

## $sig = $sig->loadCsvFile($file_or_fh,%opts)
##  + alias: loadTextFile()
##  + $file_or_fh is assumed to be utf-8 encoded
BEGIN { *loadTextFile = \&loadCsvFile; }
sub loadCsvFile {
  my ($sig,$file,%opts) = @_;

  my $fh = ref($file) ? $file : IO::File->new("<$file");
  confess(ref($sig)."::loadCsvFile(): open failed for file '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8');

  ##-- parse rest of file (TAB-separated: (TERM FREQ ...) or tab-less ("<"DEG">"? CAT_ID? CAT_NAME) or comment /^%%/)
  my $tf = $sig->{tf};
  my $N  = $sig->{N};
  my ($line,$cat,$cid,$deg, $term,$freq,$rest);
  while (defined($line=<$fh>)) {
    chomp;
    next if ($line =~ /^\s*$/ || $line =~ /^%%/);

    ##-- category declarations
    if ($line =~ m/^(?:\<(\d+)\>)?(?:\s*(\d+))?\s*(.*)$/) {
      ($deg,$cid,$cat) = ($1,$2,$3);
      $deg = $CAT_DEG_DEFAULT if (!defined($deg));
      $sig->{cat2deg}{$cat}=min2( $cat, $CAT_DEG_MAX );
      $sig->{cat2id}{$cat} = $cid if (defined($cid) && !defined($sig->{cat2id}{$cat}));
      next;
    }

    ##-- TERM FREQ ...
    ($term,$freq,$rest) = split(/\t/,$line,3);
    $tf->{$term} += $freq;
    $N += $freq;
  }
  $fh->close if (!ref($file));
  $sig->{N} = $N;

  return $sig;
}

## $sig = $sig->saveCsvFile($file_or_fh,%opts)
##  + alias: saveTextFile()
##  + $file_or_fh will be be utf-8 encoded
##  + %$opts:
##     dumpCats => $bool,  ##-- dump category data? (default=true)
##     dumpFreqs => $bool, ##-- dump term-frequency data? (default=true)
BEGIN { *saveTextFile = \&saveCsvFile; }
sub saveCsvFile {
  my ($sig,$file,%opts) = @_;

  my $fh = ref($file) ? $file : IO::File->new(">$file");
  confess(ref($sig)."::saveCsvFile(): open failed for file '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8');

  ##-- dump category data
  $opts{dumpCats} = $sig->{dumpCats} if (!defined($opts{dumpCats}));
  if (!defined($opts{dumpCats}) || $opts{dumpCats}) {
    my ($cat);
    $fh->print(
	       map {
		 ("<".($sig->{cat2deg}{$_}||$CAT_DEG_DEFAULT)."> "
		  .(defined($sig->{cat2id}{$_}) ? "$sig->{cat2id}{$_} " : '')
		  . $_ . "\n")
	       } sort {$sig->{cat2deg}{$a}<=>$sig->{cat2deg}{$b}} keys(%{$sig->{cat2deg}})
	      );
  }

  ##-- dump frequency data
  $opts{dumpFreqs} = $sig->{dumpFreqs} if (!defined($opts{dumpFreqs}));
  if (!defined($opts{dumpFreqs}) || $opts{dumpFreqs}) {
    my ($t);
    my $tf = $sig->{tf};
    $fh->print( map {"$_\t$tf->{$_}\n"} sort {$tf->{$b}<=>$tf->{$a}} keys(%$tf) );
  }

  ##-- cleanup & return
  $fh->close() if (!ref($file));
  return $sig;
}



##==============================================================================
## Footer
1;

__END__
