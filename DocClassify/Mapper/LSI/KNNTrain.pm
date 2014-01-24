## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::LSI::KNNTrain.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: document-to-class mapper: LSI / SVD, KNN variant: training

package DocClassify::Mapper::LSI::KNN;
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
  my $map = shift;

  ##-- inherited compilation
  $map->DocClassify::Mapper::ByLemma::compile()
    or confess(ref($map)."::compile() inherited compilation failed: $!");

  ##-- clear expensive perl signature structs
  @{$map->{sigs}} = qw();
  #@{$map->{docs}} = qw();

  ##-- local compilation (final)
  $map->compileLocal(label=>'FINAL', svdShrink=>1, svdCache=>1);
  $map->{svdr} = $map->{svd}{r};

  return $map;
}


## $bool = $map->compiled()
##  + returns true iff $map has been compiled
##  + override checks for $map->{xdm}
sub compiled { return defined($_[0]{xdm}); }

## $map = $map->clearTrainingCache()
##  + clears any cached data from training
##  + after calling this, $map may no longer be able to train
##  + calls inherited Mapper::ByLemma::clearTrainingCache()
##  + additionally clears @$map{qw(tdm0 tcm0)}
sub clearTrainingCache {
  my $map = shift;
  $map->SUPER::clearTrainingCache();
  return $map;
}

##--------------------------------------------------------------
## Methods: Compilation: Utils
##  + see also Mapper::ByLemma


## $map = $map->compileLocal(%opts)
##  + compiles local map params qw(svd xdm xcm disto)
##  + %opts: passed to compile_(svd|xdm|disto), e.g.
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##     svdShrink => $bool,   ##-- whether to auto-shrink svd (default=false)
##     svdCache  => $bool,   ##-- whether to auto-cache $svd->isigmaVt_() (default=true)
sub compileLocal {
  my ($map,%opts) = @_;

  ##-- svd: $map->{svd}
  $map->compile_svd(%opts);

  ##-- matrix: $map->{xdm}: (R=$map->{svdr} x ND) [R x Doc -> X]
  $map->compile_xdm(%opts);

  ##-- create distance object
  $map->compile_disto(%opts);

  return $map;
}

## $labstr = $map->labelString(%opts)
##  + gets symbolic label for verbose messages
##  + %opts:
##     label     => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##  + INHERITED from Mapper::LSI

## $map = $map->compile_svd(%opts)
##  + compiles $map->{svd} from $map->{tdm}
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

## $map = $map->compile_xcm(%opts)
##  + compiles dense matrix $map->{xcm}: (r=$map->{svdr} x NC=$NC) [R x Cat -> Sv]
##  + calls $map->catProfileMethod()
##  + requires $map->{svd}
##  + may require $map->{tdm0}, $map->{tcm0}, $map->{xdm}
##  + %opts:
##     label  => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##  + INHERITED from Mapper::LSI, should never get called here

## $map = $map->compile_disto(%opts)
##  + compiles distance object $map->{disto} from symbolic spec $map->{dist}
##  + %opts:
##     label  => $label,  ##-- symbolic label (for verbose messages; default=$map->{label})
##  + INHERITED from Mapper::LSI


##==============================================================================
## Methods: Misc

## $dxpdl = $map->svdApply($fpdl)
##  + input $fpdl is dense pdl($NT,1):     [$tid,0]     => f($tid,$doc)
##  + output $dxpdl is dense pdl(1,$svdr): [$svd_dim,0] => $svd_val
##  + INHERITED from Mapper::LSI

## $sig = $map->lemmaSignature($doc_or_sig)
##  + wrapper for $doc->termSignature->lemmatize() with options %{$map->{lemmatize}}
##  + inherited from Mapper::ByLemma

## $fpdl = $map->docPdlRaw($doc, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tidl,1]=>f($tid,$doc)
##  + inherited from Mapper::ByLemma

## $fpdl = $map->sigPdlRaw($sig, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tid,1]=>f($tid,$sig)
##  + inherited from Mapper::ByLemma

##==============================================================================
## Methods: API: I/O
##  + see DocClassify::Object


##==============================================================================
## Footer
1;

__END__
