## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::LSI::ByCatTrain.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document-to-class mapper: LSI / SVD, by category: training

package DocClassify::Mapper::LSI::ByCat;
use DocClassify::Mapper::LSI;

use PDL;
use PDL::CCS::Nd;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Methods: API: Training

## $map = $map->trainCorpus($corpus)
##  + add training data from $corpus
##  + inherited default just calls $map->trainDocument($doc) foreach doc in corpus

## $map = $map->trainDocument($doc)
##  + inherited from Mapper::ByLemma

## $catId = $map->addCat($catName)
## $catId = $map->addCat($catName,$wantId)
##  + inherited from Mapper::ByLemma

##==============================================================================
## Methods: API: Compilation

## $map = $map->compile()
##  + compile underlying map data
##  + should be called only after all training data have been added
sub compile {
  my ($map,%opts) = @_;

  ##-- options
  @opts{qw(byCat weightByCat)} = (1,1);

  ##-- inherited compilation (calls $map->compileLocal())
  $map->SUPER::compile(%opts) or $map->logconfess("compile() inherited compilation failed: $!");

  return $map;
}


## $bool = $map->compiled()
##  + returns true iff $map has been compiled
##  + override checks for $map->{xcm}
sub compiled { return defined($_[0]{xcm}); }

## $map = $map->clearTrainingCache()
##  + clears any cached data from training
##  + after calling this, $map may no longer be able to train
##  + calls inherited Mapper::LSI::clearTrainingCache()
##  + additionally clears @$map{qw()}
sub clearTrainingCache {
  my $map = shift;
  $map->SUPER::clearTrainingCache();
  return $map;
}

##--------------------------------------------------------------
## Methods: Compilation: Utils
##  + see also Mapper::LSI, Mapper::ByLemma

## $map = $map->compileLocal(%opts)
##  + compiles local map params qw(svd xdm xcm disto)
##  + %opts: passed to compile_(svd|xdm|disto), e.g.
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##     svdShrink => $bool,   ##-- whether to auto-shrink svd (default=false)
##     svdCache  => $bool,   ##-- whether to auto-cache $svd->isigmaVt_() (default=true)
sub compileLocal {
  my ($map,%opts) = @_;

  ##-- svd: $map->{svd}
  $map->compile_svd(%opts,byCat=>1);

  ##-- matrix: $map->{xdm}: (R=$map->{svdr} x ND) [R x Doc -> X] : NOT HERE
  #$map->compile_xdm(%opts);

  ##-- matrix: $map->{xcm}: (R=$map->{svdr} x NC=$NC) [R x Cat -> Sv]
  $map->compile_xcm(%opts);

  ##-- create distance object
  $map->compile_disto(%opts);

  return $map;
}

## $map = $map->compile_xcm(%opts)
##  + compiles dense matrix $map->{xcm}: (r=$map->{svdr} x NC=$NC) [R x Cat -> Sv]
##  + calls $map->catProfileMethod()
##  + requires $map->{svd}
##  + requires $map->{svd}
##  + %opts:
##     label  => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
sub compile_xcm {
  my ($map,%opts) = @_;
  my $label = $map->labelString(%opts);
  my $NC    = $map->{svd}{u}->dim(1);
  $map->vlog('info', "compile_xcm() [$label]: matrix: xcm: (r=$map->{svdr} x NC=$NC) [R x Cat -> Sv]") if ($map->{verbose});
  $map->{xcm} = $map->{svd}{u};         ##-- we've already computed the bugger (duh!)
  return $map;
}


## $labstr = $map->labelString(%opts)
##  + gets symbolic label for verbose messages
##  + %opts:
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##  + INHERITED from Mapper::LSI

## $map = $map->compile_svd(%opts)
##  + compiles $map->{svd} from $map->{tdm} (or $map->{tcm}, if $map->{byCat} is true)
##  + %opts:
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##     svdShrink => $bool,   ##-- whether to auto-shrink svd (default=false)
##     svdCache  => $bool,   ##-- whether to auto-cache $svd->isigmaVt_() (default=true)
##  + INHERITED from Mapper::LSI

## $map = $map->compile_xdm(%opts)
##  + compiles $map->{xdm}: (R=$map->{svdr} x ND) [R x Doc -> X]
##  + really just a wrapper for $map->{xdm}=$map->{svd}{u}
##  + %opts:
##     label  => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##  + INHERITED from Mapper::LSI

## $cpMethod = $map->catProfileMethod()
##  + gets $map->{catProfile}, does some sanity checks & canonicalization (caches)
##  + INHERITED from Mapper::LSI, should never get called here

## $map = $map->compile_disto(%opts)
##  + compiles distance object $map->{disto} from symbolic spec $map->{dist}
##  + %opts:
##     label  => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##  + INHERITED from Mapper::LSI


##==============================================================================
## Footer
1;

__END__
