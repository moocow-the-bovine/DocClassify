## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::Cutoff.pm
## Author: Bryan Jurish <jurish@.uni-potsdam.de>
## Descript: document classifier: document-to-class pseudo-mapper: cutoffs

package DocClassify::Mapper::Cutoff;
use DocClassify::Utils ':all';

use MUDL::Enum;
use MUDL::PDL::Stats;
use MUDL::PDL::Smooth;

use PDL;
use PDL::CCS::Nd;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Mapper);

#our $verbose = 2;
our $verbose = 3;

##==============================================================================
## Constructors etc.

## $map = $CLASS_OR_OBJ->new(%opts)
##  + defaults in constructor are obfuscated; see "real" program defaults in
##     DocClassify::Mapper::Train (and DocClassify::Program defaults)
## %$map, %opts:
##  ##
##  ##-- options: generl
##  verbose => $vlevel,              ##-- verbosity level (default=$verbose)
##  eval    => $eval,                ##-- DocClassify::Eval training source (REQURIED; see also trainEval() method)
##  ##
##  ##-- options: cutoff
##  cut0p => $p,                     ##-- confidence level for negative-sample cutoff fitting (0.5)
##  cut1p => $p,                     ##-- confidence level for positive-sample cutoff fitting (0.5)
##  cut1w => $w,                     ##-- positive weight (0<=$w<=1) for cutoff fitting (0.5)
##  cutval => $val,                  ##-- constant to add if cutoff is exceeded (default=100)
##  cutCat => $catName,              ##-- name of cutoff sink cat (default: cat with id=0 in $lcenum)
##  ##
##  ##-- data: enums
##  lcenum => $globalCatEnum,        ##-- local cat enum, compcat ($NC=$catEnum->size())
##  gcenum => $localCatEnum,         ##-- global cat enum         ($NCg=$globalCatEnum->size())
##  denum  => $docEnum,              ##-- [temp] local doc enum   ($ND=$docEnum->size())
##  ##
##  ##-- data: post-compile()
##  cutCatId => $id,                 ##-- local id of {cutCat}
##  dc_dist => $dc_dist,             ##-- [temp] dense PDL ($ND,$NC) : [$di,$ci] -> dist($ci,$di)
##  d2c     => $d2c,                 ##-- dense PDL ($ND) : [$di] -> $ci : $di \in $ci
##  c1dist_mu => $c1dist_mu,         ##-- dense PDL ($NC) : [$ci] ->    avg { dist($ci,$di) : $di  \in $ci }
##  c1dist_sd => $c1dist_sd,         ##-- dense PDL ($NC) : [$ci] -> stddev { dist($ci,$di) : $di  \in $ci }
##  c0dist_mu => $c0dist_mu,         ##-- dense PDL ($NC) : [$ci] ->    avg { dist($ci,$di) : $di !\in $ci }
##  c0dist_sd => $c0dist_sd,         ##-- dense PDL ($NC) : [$ci] -> stddev { dist($ci,$di) : $di !\in $ci }
##  cutoff    => $cutoff,            ##-- dense PDL ($NC) : [$ci] ->    max { dist($ci,$di) : $di  \in $ci } : heuristic
sub new {
  my $that = shift;
  my $obj =  $that->SUPER::new(
			       ##-- options
			       verbose => $verbose,
			       eval => undef,
			       cut0p => 0.5,
			       cut1p => 0.5,
			       cut1w => 0.5,
			       cutval => 100,
			       cutCat => undef, ##-- default computed in compileEnums()

			       ##-- data: enums
			       lcenum => MUDL::Enum->new,
			       gcenum => MUDL::Enum->new,
			       denum => MUDL::Enum->new,

			       ##-- data: post-compile
			       cutCatId =>undef,
			       c1dist_mu=>undef,
			       c1dist_sd=>undef,
			       c0dist_mu=>undef,
			       c0dist_sd=>undef,
			       cutoff=>undef,

			       ##-- user args
			       @_,
			      );
  return $obj;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override appends qw(eval denum dc_dist dc_d2c d2c c1dist_mu c1dist_sd c0dist_mu c0dist_sd cutoff)
sub noShadowKeys {
  return ($_[0]->SUPER::noShadowKeys(@_[1..$#_]),
	  qw(eval denum),
	  qw(dc_dist dc_d2c d2c c1dist_mu c1dist_sd c0dist_mu c0dist_sd cutoff));
}

##==============================================================================
## Methods: API: Training

## $map = $map->trainCorpus($corpus)
##  + add training data from $corpus
##  + inherited default just calls $map->trainDocument($doc) foreach doc in corpus

## $map = $map->trainDocument($doc)
## $map = $map->trainDocument($doc)
##  + add training data from $doc
##  + inherited default just dies

##==============================================================================
## Methods: NEW: Training
##  + see DocClassify::Mapper::CutoffTrain


##==============================================================================
## Methods: API: Classification

## $corpus = $map->mapCorpus($corpus)
##  + Attempt to classify each $doc in $corpus,
##    destructively altering $doc->{cats} to reflect classification results.
##  + inherited default just calls $map->mapDocument() on each $doc in $corpus.

## $doc = $map->mapDocument($doc)
##  + attempt to classify $doc
##  + destructively alters $doc->{cats} to reflect classification results
sub mapDocument {
  my ($map,$doc) = @_;

  ##-- be verbose?
  $map->vlog('trace', "mapDocument(".$doc->label.")") if ($map->{verbose}>=3);

  ##-- sanity check(s)
  if (!defined($map->{cutoff})) {
    $map->logwarn("mapDocument(): no {cutoff} vector defined, not activating") if ($map->{verbose});
    return $doc;
  }

  ##-- preparation
  my $lcenum  = $map->{lcenum};
  my $cutoff  = $map->{cutoff};
  $map->{cutCatId} ||= 0;

  ##-- distribution stuff
  my ($mu0,$sd0,$mu1,$sd1) = @$map{qw(c0dist_mu c0dist_sd c1dist_mu c1dist_sd)};
  my $NC       = $mu0->nelem;
  my $dist_ids = pdl(long, [@{$lcenum->{sym2id}}{map {$_->{name}} @{$doc->{cats}}}]);
  my $dist_raw = zeroes($NC);
  $dist_raw->index($dist_ids) .= pdl(double,[map {$_->{dist_raw}} @{$doc->{cats}}]);
  ##
  my $p1       = 1-_gausscdf($dist_raw,$mu1,$sd1);
  #my $p1e      = $p1/$NC;
  #my $p0       =   _gausscdf($dist_raw,$mu1,$sd1);
  #my $p0e      = $p0/$NC;
  #my $p0avg    = $p0e->sumover;

  ##-- apply cutoffs
  my ($cid);
  foreach (@{$doc->{cats}}) {
    $_->{cut} = 0;
    $_->{deg} = 0;
    next if (!defined($cid=$lcenum->{sym2id}{$_->{name}}));

    ##-- mark "confidence" value
    #$_->{confp} = ($p0avg + $p1e->at($cid) - $p0e->at($cid))->sclr;
    ##--
    $_->{confp} = $p1->at($cid);
    ##
    ##-- ... and quantize it too
    $_->{confi} = int($_->{confp}*10);

    next if ($cid==$map->{cutCatId} || $_->{dist_raw} <= $cutoff->at($cid)); ##-- no cutoff
    #$_->{dist_raw} += $map->{cutval};
    $_->{cut}       = 1;
  }

  ##-- re-sort && re-number cats
  $doc->cats();
  $doc->{cats}[$_]{deg} = $_+1 foreach (0..$#{$doc->{cats}});

  return $doc;
}


##==============================================================================
## Methods: API: I/O
##  + see DocClassify::Object


##==============================================================================
## Footer
1;

__END__
