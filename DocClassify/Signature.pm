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

## $LEMMATIZE_TEXT_ATTR
##  + default text attribute for lemmatize()
our $LEMMATIZE_TEXT_ATTR = 'norm';

## $LEMMATIZE_TEXT_REGEX
##  + default text regex for lemmatize()
our $LEMMATIZE_TEXT_REGEX = undef;

## $LEMMATIZE_POS_REGEX
##  + default pos regex for lemmatize()
#our $LEMMATIZE_POS_REGEX = qr/^N/;
#our $LEMMATIZE_POS_REGEX = qr/^(?:N|TRUNC|VV|ADJ|ITJ)/;
our $LEMMATIZE_POS_REGEX= qr/./;

## $LEMMATIZE_LEMMA_ATTR
##  + default lemma attribute for lemmatize()
our $LEMMATIZE_LEMMA_ATTR = 'lemma';

## $LEMMATIZE_LEMMA_TOLOWER
##  + default lemmatize to lower-case?
our $LEMMATIZE_LEMMA_TOLOWER = 1;

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
## Methods: Pruning

## $bool = $sig->lemmatized()
##  + check if sig has been lemmatized
##  + default just checks for $sig->{lf}
sub lemmatized { return defined($_[0]{lf}); }

## $sig = $sig->unlemmatize()
##  + clears lemma-frequency hash
##  + default just deletes @$sig{qw(lf Nl)}
sub unlemmatize {
  delete(@{$_[0]}{qw(lf Nl)});
  return $_[0];
}

## $sig = $sig->lemmatize(%opts)
##  + creates lemma-frequency hash $sig->{lf}, $sig->{Nl} from $sig->{tf}
##  + does nothing if $sig->lemmatized() returns true
##  + %opts:
##     textAttr => $attr,        ##-- text attribute (default=$LEMMATIZE_TEXT_ATTR)
##     textRegex => $re,         ##-- regex for wanted text (default=$LEMMATIZE_TEXT_REGEX)
##     textStop => \%stopText,   ##-- unwanted if exists($stopText{$text}); default=undef (none)
##     posRegex => $re,          ##-- regex for wanted PoS  (default=$LEMMATIZE_POS_REGEX)
##     lemmaAttr => $attr,       ##-- lemma attribute (default=$LEMMATIZE_LEMMA_ATTR)
##     lemmaToLower => $bool,    ##-- force lemmata to lower-case? (default=$LEMMATIZE_LEMMA_TOLOWER)
sub lemmatize {
  my ($sig,%opts) = @_;
  return $sig if ($sig->lemmatized);

  ##-- defaults
  my $textAttr  = defined($opts{textAttr}) ? $opts{textAttr} : $LEMMATIZE_TEXT_ATTR;
  my $textRegex = defined($opts{textRegex}) ? $opts{textRegex} : $LEMMATIZE_TEXT_REGEX;
  my $textStop  = $opts{textStop};
  my $posAttr   = 'pos';
  my $posRegex  = defined($opts{posRegex}) ? $opts{posRegex} : $LEMMATIZE_POS_REGEX;
  my $lemmaAttr = defined($opts{lemmaAttr}) ? $opts{lemmaAttr} : $LEMMATIZE_LEMMA_ATTR;
  my $lemma2lc  = defined($opts{lemmaToLower}) ? $opts{lemmaToLower} : $LEMMATIZE_LEMMA_TOLOWER;

  ##-- pre-compile regexes
  $textRegex = qr/$textRegex/ if (defined($textRegex) && !UNIVERSAL::isa($textRegex,'Regexp'));
  $posRegex  = qr/$posRegex/  if (defined($posRegex) && !UNIVERSAL::isa($posRegex,'Regexp'));

  ##-- lemmatize
  my $tf = $sig->{tf};
  my $lf = $sig->{lf} = {};
  my $Nlref = \$sig->{Nl};
  $$Nlref = 0;

  my ($y,$f, %ya);
  while (($y,$f)=each(%$tf)) {
    %ya = (map {split(/=/,$_,2)} split(/\t/,$y));
    next if (defined($posRegex)  && $ya{$posAttr}  !~ $posRegex);
    next if (defined($textStop)  && exists($textStop->{$ya{$textAttr}}));
    next if (defined($textRegex) && $ya{$textAttr} !~ $textRegex);
    $ya{$lemmaAttr} = lc($ya{$lemmaAttr}) if ($lemma2lc);
    $lf->{$ya{$lemmaAttr}} += $f;
    $$Nlref += $f;
  }

  return $sig;
}


##==============================================================================
## Methods: I/O


##--------------------------------------------------------------
## Methods: I/O: CSV

## $sig = $sig->loadCsvFile($file_or_fh,%opts)
##  + alias: loadTextFile()
##  + $file_or_fh is assumed to be utf-8 encoded
##  + %opts:
##     lemmatized => $bool, ##-- are we loading a lemma-map (true) or a type-map (false)? (default=true)
BEGIN { *loadTextFile = \&loadCsvFile; }
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
##  + alias: saveTextFile()
##  + $file_or_fh will be be utf-8 encoded
##  + %$opts:
##     dumpCats => $bool,   ##-- dump category data? (default=true)
##     dumpFreqs => $bool,  ##-- dump term-frequency data? (default=true)
##     lemmatized => $bool, ##-- dump a lemma-map (true) or a type-map (false)? (default=true)
BEGIN { *saveTextFile = \&saveCsvFile; }
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



##==============================================================================
## Footer
1;

__END__
