## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::Train.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: training: top-level include

package DocClassify::Mapper::Train;
use DocClassify::Mapper::ByLemmaTrain;
use DocClassify::Mapper::LSITrain;
use DocClassify::Mapper::LSI::KNNTrain;
use DocClassify::Mapper::LSI::ByCatTrain;
use DocClassify::Mapper::CutoffTrain;

##==============================================================================
## Program Defaults
use DocClassify::Program qw(:all);
BEGIN {
  my %mapNew =
    (
     class=>'LSI',	         ##-- mapper class
     svdr => 256,                ##-- svd dimensions (see DocClassify::Mapper::LSI defaults)
     maxTermsPerDoc=>0,          ##-- maximum #/terms per doc
     minFreq =>0,                ##-- minimum global term-frequency f(t) for term-inclusion
     minDocFreq =>0,             ##-- minimum #/docs with f(t,d)>0 for term-inclusion
     smoothf =>1+1e-3,           ##-- smoothing frequency (undef for NTypes/NTokens)
     catProfile => 'average',    ##-- how to do category profiling
     termWeight => 'entropy',    ##-- how to do term weighting
     weightByCat => 1,           ##-- do term weighting by category?
     dist => 'u',                ##-- PDL::Cluster distance function
     mapccs => 1,                ##-- use sparse PDLs for mapping (faster)
    );
  @{$opts{mapNew}}{keys %mapNew} = values %mapNew;

  my %cutNew =
    (
     cut0p => 0.5,               ##-- confidence level for negative-sample cutoff fitting (0.5)
     cut1p => 0.5,               ##-- confidence level for positive-sample cutoff fitting (0.5)
     cut1w => 0.65,              ##-- positive weight (0<=$w<=1) for cutoff fitting (0.65)
     cutval => 100,              ##-- constant to add if cutoff is exceeded (default=100)
     cutCat => undef,            ##-- name of cutoff sink cat (default: cat with id=0 in $lcenum)
     optimize => 100,            ##-- number of cutoff optimization iterations (default=100: fast but not exhaustive (fitness grows logaritmically with n iters))
    );
  @{$opts{cutoffNew}}{keys %cutNew} = values %cutNew;
}

##==============================================================================
## Footer
1;

__END__
