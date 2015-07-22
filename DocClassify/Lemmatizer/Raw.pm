## -*- Mode: CPerl -*-
## File: DocClassify::Lemmatizer::Raw.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: lemmatizer: raw (just copy %$tf, $N)

package DocClassify::Lemmatizer::Raw;
use DocClassify::Object;
use DocClassify::Utils ':all';
use DocClassify::Lemmatizer;
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Lemmatizer);

##==============================================================================
## Constructors etc.

## $lz = $CLASS_OR_OBJ->new(%opts)
## %$lz, %opts:
##  ##---- INHERITED from DocClassify::Lemmatizer
##  ##-- subclass selection:
##  class => $class,        ##-- package name or alias or 'DocClassify::Lemmatizer' suffix

sub new {
  my $that = shift;
  return $that->SUPER::new(
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

  ##-- ensure N is defined
  if (!$sig->{N}) {
    my $n = 0;
    $n += $_ foreach (values %{$sig->{tf}});
    $sig->{N} = $n;
  }

  ##-- just copy raw text counts
  @$sig{qw(lf Nl)} = @$sig{qw(tf N)};

  return $sig;
}



##==============================================================================
## Footer
1;

__END__
