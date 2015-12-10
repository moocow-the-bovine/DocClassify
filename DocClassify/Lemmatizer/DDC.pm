## -*- Mode: CPerl ; coding: utf-8 -*-
## File: DocClassify::Lemmatizer::DDC.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: lemmatizer:
##  + from vz-like cab-signatures to content lemmata

package DocClassify::Lemmatizer::DDC;

use DocClassify::Object;
use DocClassify::Utils ':all';
use DocClassify::Lemmatizer;
use DocClassify::Lemmatizer::Cab;
use IO::File;
use Carp;
use strict;
use utf8;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Lemmatizer::Cab);
BEGIN {
  *isa = \&UNIVERSAL::isa;
}
##==============================================================================
## Constructors etc.

## $lz = $CLASS_OR_OBJ->new(%opts)
## %$lz, %opts:
##  ##---- NEW for DocClassify::Lemmatizer::Cab
##  #(none)
##
##  ##---- INHERITED from DocClassify::Lemmatizer::VzContent
##  textAttr => $attr,      ##-- text attribute (OVERRIDE default='norm')
##  textRegexGood => $re,   ##-- regex matching "good" text types (OVERRIDE default=$TEXT_REGEX_GOOD)
##  textRegexBad  => $re,   ##-- regex matching "bad" text types  (OVERRIDE default=$TEXT_REGEX_BAD)
##  textStop => \%stopText, ##-- pseudo-hash of unwanted text types; default=undef (none)
##  posAttr  => $attr,      ##-- pos attribute (OVERRIDE default='pos')
##  posRegex => $re,        ##-- regex matching "good" pos tags (OVERRIDE default=$POS_REGEX)
##  lemmaAttr => $attr,     ##-- lemma attribute (OVERRIDE default='lemma')
##  lemmaToLower => $bool,  ##-- whether to canonicalize lemmata to lower-case (OVERRIDE default=0)
##  ##---- INHERITED from DocClassify::Lemmatizer
##  ##-- subclass selection:
##  class => $class,        ##-- package name or alias or 'DocClassify::Lemmatizer' suffix

sub new {
  my $that = shift;
  return $that->SUPER::new(
			   ##-- overrides
			   textAttr      => 'text',
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
##  + INHERITED from Lemmatizer::VzContent


##==============================================================================
## Footer
1;

__END__
