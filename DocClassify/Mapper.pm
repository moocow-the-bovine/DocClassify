## -*- Mode: CPerl -*-
## File: DocClassify::Mapper.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document-to-class mapper: abstract API

package DocClassify::Mapper;
use DocClassify::Object;
use DocClassify::Logger;
use DocClassify::Utils ':all';
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Object DocClassify::Logger);

## %ALIAS
##  + subclass aliases
our %ALIAS =
  (
   raw=>'ByLemma',
   bylemma=>'ByLemma',
   lemma=>'ByLemma',
   ##
   lsi=>'LSI',
   svd=>'LSI',
   SVD=>'LSI',
   ##
   'lsi-bycat'=>'LSI::ByCat',
   'bycat' => 'LSI::ByCat',
  );

##==============================================================================
## Constructors etc.

## $map = $CLASS_OR_OBJ->new(%opts)
## %$map, %opts:
##  ##-- subclass selection
##  class => $class,       ##-- package name or alias or 'DocClassify::Mapper' suffix
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
  return $that->DocClassify::Object::new(%opts);
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw()
sub noShadowKeys {
  return qw();
}

##==============================================================================
## Methods: API: Training

## $map = $map->trainCorpus($corpus)
##  + add training data from $corpus
##  + default just calls $map->trainDocument($doc) foreach doc in corpus
sub trainCorpus {
  my ($map,$corpus) = @_;
  $map->trainDocument($_) foreach (@{$corpus->{docs}});
  return $map;
}

## $map = $map->trainDocument($doc)
##  + add training data from $doc
##  + REQUIRED
sub trainDocument {
  my ($map,$doc) = @_;
  $map->logconfess("trainDocument(): not implemented!");
  return $map;
}

##==============================================================================
## Methods: API: Compilation

## $map = $map->compile()
##  + compile underlying map data
##  + should be called only after all training data have been added
##  + default just returns $map
sub compile { return $_[0]; }

## $bool = $map->compiled()
##  + returns true iff $map has been compiled
##  + default always returns false
sub compiled { return 0; }

## $map = $map->clearTrainingCache()
##  + clears any cached data from training
##  + after calling this, $map may no longer be able to train
##  + default implementation does nothing
sub clearTrainingCache { return $_[0]; }

##==============================================================================
## Methods: API: Classification

## $corpus = $map->mapCorpus($corpus)
##  + Attempt to classify each $doc in $corpus,
##    destructively altering $doc->{cats} to reflect classification results.
##  + Default implementation just calls $map->mapDocument() on each $doc in $corpus.
sub mapCorpus {
  my ($map,$corpus) = @_;
  $map->mapDocument($_) foreach (@{$corpus->{docs}});
  return $corpus;
}

## $doc = $map->mapDocument($doc)
##  + attempt to classify $doc
##  + destructively alters $doc->{cats} to reflect classification results
##  + REQUIRED
sub mapDocument {
  my ($map,$doc) = @_;
  $map->logconfess("mapDocument(): not implemented!");
  return $doc;
}

##==============================================================================
## Methods: API: I/O
##  + see DocClassify::Object


##==============================================================================
## Footer
1;

__END__
