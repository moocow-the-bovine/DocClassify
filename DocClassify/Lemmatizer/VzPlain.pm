## -*- Mode: CPerl -*-
## File: DocClassify::Lemmatizer::VzPlain.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: lemmatizer: from Vz-signatures to plaintext lemmata

package DocClassify::Lemmatizer::VzPlain;
use DocClassify::Lemmatizer::VzContent;
use DocClassify::Object;
use DocClassify::Utils ':all';
use DocClassify::Lemmatizer;
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
##  ##---- NEW for DocClassify::Lemmatizer::VzContent
##  textAttr => $attr,      ##-- text attribute (default='norm')
##  textRegex => $re,       ##-- regex matching "good" text types (default=undef (none))
##  textStop => \%stopText, ##-- pseudo-hash of unwanted text types; default=undef (none)
##  posRegex => $re,        ##-- regex matching "good" pos tags (default=undef (none)): OVERRIDE
##  lemmaAttr => $attr,     ##-- lemma attribute (default='plain'): OVERRIDE
##  lemmaToLower => $bool,  ##-- whether to canonicalize lemmata to lower-case (default=1)
##  ##---- INHERITED from DocClassify::Lemmatizer
##  ##-- subclass selection:
##  class => $class,        ##-- package name or alias or 'DocClassify::Lemmatizer' suffix

sub new {
  my $that = shift;
  return $that->SUPER::new(
			   ##-- defaults: Lemmatizer::VzContent
			   textAttr => 'plain',
			   textRegex => undef,
			   textStop => undef,
			   posRegex => undef,
			   lemmaAttr => 'plain',
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
##  + INHERITED from Lemmatizer::VzContent


##==============================================================================
## Footer
1;

__END__
