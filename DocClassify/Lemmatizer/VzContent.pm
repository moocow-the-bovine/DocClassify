## -*- Mode: CPerl -*-
## File: DocClassify::Lemmatizer::VzContent.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: lemmatizer: from Vz-signatures to content lemmata

package DocClassify::Lemmatizer::VzContent;
use DocClassify::Object;
use DocClassify::Utils ':all';
use DocClassify::Lemmatizer;
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Lemmatizer);

## $POS_REGEX
##  + default pos regex for lemmatize()
##  + don't use qr// here, since Storable doesn't like pre-compiled Regexps
#our $POS_REGEX = qr/^N/;
#our $POS_REGEX = qr/^(?:N|TRUNC|VV|ADJ|ITJ)/;
#our $POS_REGEX = qr/^(?:N|TRUNC|VV|ADJ|ITJ|FM)/;
#our $POS_REGEX = qr/^(?:N|TRUNC|VV|ADJ|ITJ|FM|XY)/;
#our $POS_REGEX= qr/./;
##--
our $POS_REGEX = q/^(?:N|TRUNC|VV|ADJ|ITJ|FM|XY)/;

## $TEXT_REGEX_GOOD
##  + default "good" text regex for lemmatize()
##  + don't use qr// here, since Storable doesn't like pre-compiled Regexps
##  + default requires at least 1 ASCII alphanumeric character
our $TEXT_REGEX_GOOD = q/[a-zA-Z0-9]/;

## $TEXT_REGEX_BAD
##  + default "bad" text regex for lemmatize()
##  + don't use qr// here, since Storable doesn't like pre-compiled Regexps
#our $TEXT_REGEX_BAD = qr/(?:^[^[:alpha:][:digit:]\_\-]*$)|(?:\#)/;
our $TEXT_REGEX_BAD = q/(?:^[^[:alpha:][:digit:]\_\-]*$)|(?:\#)/;

##==============================================================================
## Constructors etc.

## $lz = $CLASS_OR_OBJ->new(%opts)
## %$lz, %opts:
##  ##---- NEW for DocClassify::Lemmatizer::VzContent
##  textAttr => $attr,      ##-- text attribute (default='norm')
##  textRegexGood => $re,   ##-- regex matching "good" text types (default=$TEXT_REGEX_GOOD)
##  textRegexBad  => $re,   ##-- regex matching "bad" text types  (default=$TEXT_REGEX_BAD)
##  textStop => \%stopText, ##-- pseudo-hash of unwanted text types; default=undef (none)
##  posRegex => $re,        ##-- regex matching "good" pos tags (default=$POS_REGEX)
##  lemmaAttr => $attr,     ##-- lemma attribute (default='lemma')
##  lemmaToLower => $bool,  ##-- whether to canonicalize lemmata to lower-case (default=1)
##  ##---- INHERITED from DocClassify::Lemmatizer
##  ##-- subclass selection:
##  class => $class,        ##-- package name or alias or 'DocClassify::Lemmatizer' suffix

sub new {
  my $that = shift;
  return $that->SUPER::new(
			   ##-- defaults: Lemmatizer::VzContent
			   textAttr => 'norm',
			   textRegexGood => $TEXT_REGEX_GOOD,
			   textRegexBad  => $TEXT_REGEX_BAD,
			   textStop => undef,
			   posRegex => $POS_REGEX,
			   lemmaAttr => 'lemma',
			   lemmaToLower => 1,

			   ##-- user args
			   @_
			  );
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + inherited default returns qw()
#sub noShadowKeys { return qw(); }

##==============================================================================
## Methods: API: Signature Lemmatization

## $sig = $lz->lemmatize($sig)
##  + lemmatizes the raw DocClassify::Signature $sig by (re-)populating
##    $sig->{lf}, $sig->{Nl} from $sig->{tf}
##  + does nothing if $sig->lemmatized() returns true
sub lemmatize {
  my ($lz,$sig) = @_;
  return $sig if ($sig->lemmatized);

  ##-- defaults
  my $textAttr  = $lz->{textAttr};
  my $textRegexGood = $lz->{textRegexGood};
  my $textRegexBad = $lz->{textRegexBad};
  my $textStop  = $lz->{textStop};
  my $posAttr   = 'pos';
  my $posRegex  = $lz->{posRegex};
  my $lemmaAttr = $lz->{lemmaAttr};
  my $lemma2lc  = $lz->{lemmaToLower};

  ##-- pre-compile regexes
  $textRegexGood = qr/$textRegexGood/ if (defined($textRegexGood) && !UNIVERSAL::isa($textRegexGood,'Regexp'));
  $textRegexBad = qr/$textRegexBad/ if (defined($textRegexBad) && !UNIVERSAL::isa($textRegexBad,'Regexp'));
  $posRegex  = qr/$posRegex/  if (defined($posRegex) && !UNIVERSAL::isa($posRegex,'Regexp'));

  ##-- lemmatize: vars
  my $tf = $sig->{tf};
  my $lf = $sig->{lf} = {};
  my $Nlref = \$sig->{Nl};
  $$Nlref = 0;

  ##-- lemmatize: loop
  my ($y,$f, %ya);
  while (($y,$f)=each(%$tf)) {
    %ya = (map {split(/=/,$_,2)} split(/\t/,$y));
    next if (defined($posRegex)  && $ya{$posAttr}  !~ $posRegex);
    next if (defined($textStop)  && exists($textStop->{$ya{$textAttr}}));
    next if (defined($textRegexGood) && $ya{$textAttr} !~ $textRegexGood);
    next if (defined($textRegexBad)  && $ya{$textAttr} =~ $textRegexBad);
    $lf->{$lemma2lc ? lc($ya{$lemmaAttr}) : $ya{$lemmaAttr}} += $f;
    $$Nlref += $f;
  }

  return $sig;
}



##==============================================================================
## Footer
1;

__END__
