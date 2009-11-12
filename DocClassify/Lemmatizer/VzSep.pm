## -*- Mode: CPerl -*-
## File: DocClassify::Lemmatizer::VzSep.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: lemmatizer: from Vz-signatures to content lemmata, using separators

package DocClassify::Lemmatizer::VzSep;

use DocClassify::Object;
use DocClassify::Utils ':all';
use DocClassify::Lemmatizer;
use DocClassify::Lemmatizer::VzContent;
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Lemmatizer::VzContent);

##==============================================================================
## Constructors etc.

## $lz = $CLASS_OR_OBJ->new(%opts)
## %$lz, %opts:
##  ##---- NEW for DocClassify::Lemmatizer::VzSep
##  sepLemmaAttr => $attr,  ##-- separated-lemma attribute (default='sep_lemma')
##  ##---- INHERITED from DocClassify::Lemmatizer::VzContent
##  textAttr => $attr,      ##-- text attribute (default='norm')
##  textRegex => $re,       ##-- regex matching "good" text types (default=undef (none))
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
			   ##-- new
			   sepLemmaAttr => 'sep_lemma',

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
  my $textRegex = $lz->{textRegex};
  my $textStop  = $lz->{textStop};
  my $posAttr   = 'pos';
  my $posRegex  = $lz->{posRegex};
  my $lemmaAttr = $lz->{lemmaAttr};
  my $sLemmaAttr= $lz->{sepLemmaAttr};
  my $lemma2lc  = $lz->{lemmaToLower};

  ##-- pre-compile regexes
  $textRegex = qr/$textRegex/ if (defined($textRegex) && !UNIVERSAL::isa($textRegex,'Regexp'));
  $posRegex  = qr/$posRegex/  if (defined($posRegex) && !UNIVERSAL::isa($posRegex,'Regexp'));

  ##-- lemmatize: vars
  my $tf = $sig->{tf};
  my $lf = $sig->{lf} = {};
  my $Nlref = \$sig->{Nl};
  $$Nlref = 0;

  ##-- lemmatize: loop
  my ($y,$f, %ya,$lemma,$lemmas);
  while (($y,$f)=each(%$tf)) {
    %ya = (map {split(/=/,$_,2)} split(/\t/,$y));
    next if (defined($posRegex)  && $ya{$posAttr}  !~ $posRegex);
    next if (defined($textStop)  && exists($textStop->{$ya{$textAttr}}));
    next if (defined($textRegex) && $ya{$textAttr} !~ $textRegex);
    next if (!defined($lemma = $ya{$lemmaAttr}));
    $lemma = lc($ya{$lemmaAttr}) if ($lemma2lc);

    ##-- add raw lemma
    $lf->{$lemma} += $f;
    $$Nlref += $f;

    ##-- add separated raw lemma (on punct, '_' space)
    foreach ($lemma =~ m/[^\_\s[:punct:]]+/g) {
      $lf->{$_} += $f;
      $$Nlref += $f;
    }

    ##-- use tagh-separated lemma components
    next if (!defined($lemmas = $ya{$sLemmaAttr}));
    $lemmas =~ s/[\|\=]//g; ##-- e.g. Band/N#er|fahr/V~ung -> Band/N#erfahr/V~ung, aus=stell/V~ung -> ausstell/V~ung
    $lemmas =~ s/(?:\-)|(?:\/\w+)|(?:[\~\\]\w+)|(?:[\|\=\+])|(?:\#)/ /g;
    $lemmas = lc($lemmas) if ($lemma2lc);
    foreach ($lemmas =~ m/\S+/g) {
      $lf->{$_} += $f;
      $$Nlref += $f;
    }
  }

  return $sig;
}



##==============================================================================
## Footer
1;

__END__
