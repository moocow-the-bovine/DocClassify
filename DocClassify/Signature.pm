## -*- Mode: CPerl -*-
## File: DocClassify::Signature.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: signature (raw term-type frequencies)

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
##  ##-- frequency data
##  tf   => \%type2freq,  ##-- raw type-frequency hash (keys are TAB-separated "${ATTR}=${VAL}")
##  lf   => \%lemma2freq, ##-- raw lemma-frequency hash (keys are lemmata; see lemmatize() method)
##  N    => $totalFreq,   ##-- total frequency
##  Nl   => $lemmaFreq,   ##-- total lemma frequency (set by lemmatize())
##  ##
##  ##-- category information data
##  cat2deg => \%cat2deg, ##-- maps category names to membership degree ($deg>=1, 1 is best)
##  cat2id  => \%cat2id,  ##-- maps category names to loaded IDs (or undef)
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   ##-- raw frequency data
			   tf=>{},
			   N=>0,

			   ##-- lemmatized frequency data
			   lf=>undef,
			   Nl=>undef,

			   ##-- category data
			   cat2deg=>{},
			   cat2id=>{},

			   ##-- user args
			   @_,
			  );
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(tf N cat2deg lf Nl)
sub noShadowKeys {
  return qw(tf N cat2deg lf Nl);
}

## $sig = $sig->clear()
##  + clears object, preparing it for re-use
sub clear {
  my $sig = shift;
  %{$sig->{tf}} = qw();
  $sig->{N} = 0;
  %{$sig->{cat2deg}} = qw();
  %{$sig->{cat2id}} = qw();
  delete(@$sig{qw(lf N)});
  return $sig;
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

## $sig = $sig->addCat( {name=>$cat, id=>$catid, deg=>$degree} )
sub addCat {
  my ($sig,$cat) = @_;
  $sig->{cat2id}{$cat->{name}}  = $cat->{id}                    if (!defined($sig->{cat2id}{$cat->{name}}));
  $sig->{cat2deg}{$cat->{name}} = $cat->{deg}||$CAT_DEG_DEFAULT if (!defined($sig->{cat2deg}{$cat->{name}}));
  return $sig;
}

##==============================================================================
## Methods: Pruning

## $bool = $sig->lemmatized()
##  + check if sig has been lemmatized
##  + default just checks for $sig->{lf}
sub lemmatized {
  return defined($_[0]{lf});
}

## $sig = $sig->unlemmatize()
##  + clears lemma-frequency hash
##  + default just deletes @$sig{qw(lf Nl)}
sub unlemmatize {
  delete(@{$_[0]}{qw(lf Nl)});
  return $_[0];
}

## $sig = $sig->lemmatize(%opts)
##  + wrapper $opts{lemmatizer}->lemmatize($sig) rsp. $opts{lzClass}->new(%opts)->lemmatize($sig) 
##  + does nothing if $sig->lemmatized() returns true
##  + %opts:
##     lemmatizer => $lz,         ##-- lemmatizer object to use
##     lzClass    => $lzClass,    ##-- lemmatizer class
##     $lzOptKey  => $lzOptVal,   ##-- other options are passed to $lemmatizerClass->new(), if classed
sub lemmatize {
  my ($sig,%opts) = @_;
  return $sig if ($sig->lemmatized);

  my $lz = $opts{lemmatizer};
  if (!UNIVERSAL::isa($lz,'DocClassify::Lemmatizer')) {
    my $lzClass = ($lz || $opts{lzClass} || 'default');
    $lz = DocClassify::Lemmatizer->new(%opts,class=>$lzClass);
  }
  confess(ref($sig)."::lemmatize(): could not create lemmatizer!") if (!defined($lz));

  return $lz->lemmatize($sig);
}



##==============================================================================
## Methods: I/O


##--------------------------------------------------------------
## Methods: I/O: CSV

## $sig = $sig->loadCsvFile($file_or_fh,%opts)
##  + OBSOLETE alias: loadTextFile()
##  + $file_or_fh is assumed to be utf-8 encoded
##  + %opts:
##     lemmatized => $bool, ##-- are we loading a lemma-map (true) or a type-map (false)? (default=true)
#BEGIN { *loadTextFile = \&loadCsvFile; }
sub loadCsvFile {
  my ($sig,$file,%opts) = @_;
  my $lemmatized = defined($opts{lemmatized}) ? $opts{lemmatized} : 1;

  my $fh = ref($file) ? $file : IO::File->new("<$file");
  confess(ref($sig)."::loadCsvFile(): open failed for file '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8') if ($fh->can('binmode'));

  ##-- parse rest of file (TAB-separated: (TERM FREQ ...) or tab-less ("<"DEG">"? CAT_ID? CAT_NAME) or comment /^%%/)
  my ($xf,$xN);
  if ($lemmatized) {
    $sig->{lf} = {} if (!defined($xf=$sig->{lf}));
    $sig->{Nl} = 0;
    $xf = $sig->{lf};
    $xN = \$sig->{Nl};
  } else {
    $xf = $sig->{tf};
    $xN = \$sig->{N};
  }
  my ($line,$cat,$cid,$deg, $term,$freq,$rest, @fields);
  while (defined($line=<$fh>)) {
    chomp($line);
    next if ($line =~ /^\s*$/ || $line =~ /^%%/);

    ##-- category declarations
    if ($line !~ m/\t/) {
      if ($line =~ m/^(?:\<(\d+)\>)?(?:\s*(\d+))?\s*(.*)$/) {
	($deg,$cid,$cat) = ($1,$2,$3);
	$deg = $CAT_DEG_DEFAULT if (!defined($deg));
	$sig->{cat2deg}{$cat} = min2( $deg, $CAT_DEG_MAX );
	$sig->{cat2id}{$cat}  = $cid if (defined($cid) && !defined($sig->{cat2id}{$cat}));
      }
      next; ##-- just skip anything else without a TAB
    }

    if ($lemmatized) {
      ##-- ATTR1=VAL1 "\t" ATTR2=VAL2 "\t" ... "\t" FREQ "\n"
      ($term,$freq,$rest) = split(/\t/,$line,3);
    } else {
      ##-- LEMMA "\t" FREQ "\t" ...
      @fields = split(/\t/,$line);
      $freq   = pop(@fields);
      $term   = join("\t",@fields);
    }
    $xf->{$term} += $freq;
    $$xN += $freq;
  }
  $fh->close if (!ref($file));

  return $sig;
}

## $sig = $sig->saveCsvFile($file_or_fh,%opts)
##  + OBSOLETE alias: saveTextFile()
##  + $file_or_fh will be be utf-8 encoded
##  + %$opts:
##     dumpCats => $bool,   ##-- dump category data? (default=true)
##     dumpFreqs => $bool,  ##-- dump term-frequency data? (default=true)
##     lemmatized => $bool, ##-- dump a lemma-map (true) or a type-map (false)? (default=true)
#BEGIN { *saveTextFile = \&saveCsvFile; }
sub saveCsvFile {
  my ($sig,$file,%opts) = @_;
  my $lemmatized = defined($opts{lemmatized}) ? $opts{lemmatized} : 1;

  my $fh = ref($file) ? $file : IO::File->new(">$file");
  confess(ref($sig)."::saveCsvFile(): open failed for file '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8') if ($fh->can('binmode'));

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
    my $xf = $lemmatized ? $sig->lemmatize->{lf} : $sig->{tf};
    $fh->print( map {"$_\t$xf->{$_}\n"} sort {$xf->{$b}<=>$xf->{$a}} keys(%$xf) );
  }

  ##-- cleanup & return
  $fh->close() if (!ref($file));
  return $sig;
}


##--------------------------------------------------------------
## Methods: I/O: TT-1grams (see also Lingua::TT::Ngrams)

## $sig = $sig->load1gFile($file_or_fh,%opts)
##  + $file_or_fh is assumed to be utf-8 encoded
BEGIN { *loadTextFile = \&load1gFile; }
sub load1gFile {
  my ($sig,$file,%opts) = @_;

  my $fh = ref($file) ? $file : IO::File->new("<$file");
  confess(ref($sig)."::load1gFile(): open failed for file '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8') if ($fh->can('binmode'));

  ##-- parse rest of file (TAB-separated: (TERM FREQ ...) or tab-less ("<"DEG">"? CAT_ID? CAT_NAME) or comment /^%%/)
  my ($deg,$cid,$cat);
  my ($f,$item);
  my ($xf,$Nr) = ($sig->{tf}, \$sig->{N});
  while (defined($_=<$fh>)) {
    chomp;
    if (/^%%/ || /^\s*$/) {
      if (/^%%\$dc.cat(?:\.([0-9\.\-\+eE]+))?=(?:([0-9]+)_)?(.*)$/) {
	##-- parse category
	($deg,$cid,$cat) = (($1//1), ($2||$CAT_DEG_DEFAULT), ($2//'unknown'));
	$sig->{cat2deg}{$cat} = min2( $deg, $CAT_DEG_MAX );
	$sig->{cat2id}{$cat}  = $cid if (defined($cid) && !defined($sig->{cat2id}{$cat}));
      }
      elsif (/^%%\$dc.terms$/) {
	($xf,$Nr) = ($sig->{tf}, \$sig->{N});
      }
      elsif (/^%%\$dc.lemma(?:ta|s)$/) {
	$sig->{lf} //= {};
	($xf,$Nr) = ($sig->{lf}, \$sig->{Nl});
      }
      next;
    }

    ##-- parse csv lines
    ($f,$item) = split(/\t/, $_, 2);
    $f //= 1;
    $xf->{$item} += $f;
    $$Nr         += $f;
  }
  $fh->close if (!ref($file));

  ##-- cleanup
  delete @$sig{qw(tf N)}  if (!$sig->{N});
  delete @$sig{qw(lf Nl)} if (!$sig->{Nl});

  return $sig;
}

## $sig = $sig->save1gFile($file_or_fh,%opts)
##  + alias: saveTextFile()
##  + $file_or_fh will be be utf-8 encoded
BEGIN { *saveTextFile = \&save1gFile; }
sub save1gFile {
  my ($sig,$file,%opts) = @_;

  my $fh = ref($file) ? $file : IO::File->new(">$file");
  confess(ref($sig)."::save1gFile(): open failed for file '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8') if ($fh->can('binmode'));

  ##-- dump category data
  foreach (sort {$sig->{cat2deg}{$a}<=>$sig->{cat2deg}{$b}} keys(%{$sig->{cat2deg}})) {
    $fh->print("%%\$dc.cat.".($sig->{cat2deg}{$_}||$CAT_DEG_DEFAULT)."=".$sig->{cat2id}{$_}."_".$_."\n");
  }

  ##-- dump frequency data: terms  if ($sig->{N} && $sig->{tf}) {
  if ($sig->{N} && $sig->{tf}) {
    my $xf = $sig->{tf};
    $fh->print("%%\$dc.terms\n",
	       map {"$xf->{$_}\t$_\n"} sort {$xf->{$b}<=>$xf->{$a}} keys(%$xf)
	      );
  }

  ##-- dump frequency data: lemmata
  if ($sig->{Nl} && $sig->{lf}) {
    my $xf = $sig->{lf};
    $fh->print("%%\$dc.lemmata\n",
	       map {"$xf->{$_}\t$_\n"} sort {$xf->{$b}<=>$xf->{$a}} keys(%$xf)
	      );
  }

  ##-- cleanup & return
  $fh->close() if (!ref($file));
  return $sig;
}


##==============================================================================
## Footer
1;

__END__
