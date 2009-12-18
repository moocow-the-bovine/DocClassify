## -*- Mode: CPerl -*-
## File: DocClassify::Lemmatizer.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: lemmatizer (abstract base class)

package DocClassify::Lemmatizer;
use DocClassify::Object;
use DocClassify::Logger;
use DocClassify::Signature;
use DocClassify::Utils ':all';
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Object DocClassify::Logger);

## %ALIAS
##  + subclass aliases
our (%ALIAS);
BEGIN {
  %ALIAS =
    (
     ##
     VZ => 'VzContent',
     vzcontent => 'VzContent',
     vzContent => 'VzContent',
     ##
     plain => 'VzPlain',
     vzPlain => 'VzPlain',
     ##
     vzsep => 'VzSep',
     vzSep => 'VzSep',
     ##
     vzsem => 'VzSem',
     vzSem => 'VzSem',
     ##
     #Default => 'VzContent',
     #Default => 'VzSep',
     Default => 'VzSem',
    );
  @ALIAS{map {lc($_)} keys(%ALIAS)} = values(%ALIAS);
}


##==============================================================================
## Constructors etc.

## $lz = $CLASS_OR_OBJ->new(%opts)
## %$lz, %opts:
##  ##---- new for DocClassify::Lemmatizer
##  ##-- subclass selection:
##  class => $class,       ##-- package name or alias or 'DocClassify::Lemmatizer' suffix
sub new {
  my ($that,%opts) = @_;
  if (defined($opts{class})) {
    ##-- subclass selection
    my $class = $opts{class};
    $class=$ALIAS{$class} while (defined($ALIAS{$class}));
    delete($opts{class});
    $class = __PACKAGE__ . "::$class" if (!UNIVERSAL::isa($class,__PACKAGE__));
    eval "use $class;" if (!UNIVERSAL::can($class,'new'));
    return $class->new(%opts);
  }
  ##-- ... otherwise just pass through to MUDL::Object
  return $that->DocClassify::Object::new(
					 ##-- local options
					 #(none)

					 ##-- user args
					 %opts,
					);
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + inherited default returns qw()
#sub noShadowKeys { return qw(tf N cat2deg lf Nl); }

##==============================================================================
## Methods: API: Signature Lemmatization

## $sig = $lz->lemmatize($sig)
##  + lemmatizes the raw DocClassify::Signature $sig by (re-)populating
##    $sig->{lf}, $sig->{Nl} from $sig->{tf}
##  + does nothing if $sig->lemmatized() returns true
##  + child classes should override this method!
sub lemmatize {
  my ($lz,$sig) = @_;
  confess(ref($lz)."::lemmatize(): abstract API method called!");
  return $sig;
}

##==============================================================================
## Footer
1;

__END__
