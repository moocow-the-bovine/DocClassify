## -*- Mode: CPerl -*-
## File: DocClassify::Temp.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: temporary files

package DocClassify::Temp;
use DocClassify::Logger;
use strict;

##======================================================================
## Gloabls
our @ISA = qw(DocClassify::Logger);

##======================================================================
## DocClassify::Temp API

## $tied = TIEHASH($classname, $filename, %opts)
## $tied = TIEARRAY($classname, $filename, %opts)
##  + should honor 'UNLINK' option in %opts to auto-unlink $filename on object destruction

## undef = $obj->cleanup()
##  + unlink underlying file(s) (only if created with 'UNLINK' option)
##  + call this from DESTROY()
sub cleanup {
  ;
}

1; ##-- be happy

__END__
