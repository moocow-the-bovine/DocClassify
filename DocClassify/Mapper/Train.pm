## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::Train.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: training: top-level include

package DocClassify::Mapper::Train;
use DocClassify::Mapper::ByLemmaTrain;
use DocClassify::Mapper::LSITrain;
use DocClassify::Mapper::LSI::KNNTrain;
use DocClassify::Mapper::LSI::ByCatTrain;
use DocClassify::Mapper::CutoffTrain;

##==============================================================================
## Footer
1;

__END__
