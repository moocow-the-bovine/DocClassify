## -*- Mode: CPerl ; coding: utf-8 -*-
## File: DocClassify::Lemmatizer::Cab.pm
## Author: Bryan Jurish <jurish@bbaw.de>
## Descript: document classifier: lemmatizer:
##  + from vz-like cab-signatures to content lemmata

package DocClassify::Lemmatizer::Cab;

use DocClassify::Object;
use DocClassify::Utils ':all';
use DocClassify::Lemmatizer;
use DocClassify::Lemmatizer::VzContent;
use IO::File;
use Carp;
use strict;
use utf8;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Lemmatizer::VzContent);

## $POS_REGEX
##  + default pos regex for lemmatize()
##  + don't use qr// here, since Storable doesn't like pre-compiled Regexps
our $POS_REGEX       = q/^NN$/; #q/^(?:N|TRUNC|VV|ADJ)/; #ITJ|FM|XY

## $TEXT_REGEX_GOOD
##  + default "good" text regex for lemmatize()
##  + don't use qr// here, since Storable doesn't like pre-compiled Regexps
##  + default requires at least 1 alphabetic character
our $TEXT_REGEX_GOOD = q/[[:alpha:]]/;

## $TEXT_REGEX_BAD
##  + default "bad" text regex for lemmatize()
##  + don't use qr// here, since Storable doesn't like pre-compiled Regexps
our $TEXT_REGEX_BAD = q/[\.]/;

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
			   textAttr      => 'norm',
			   textRegexGood => $TEXT_REGEX_GOOD,
			   textRegexBad  => $TEXT_REGEX_BAD,
			   posAttr       => 'pos',
			   posRegex      => $POS_REGEX,
			   lemmaAttr     => 'lemma',
			   lemmaToLower  => 0,

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
