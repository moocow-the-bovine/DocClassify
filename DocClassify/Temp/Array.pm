## -*- Mode: CPerl -*-
## File: DocClassify::Temp::Array.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: temporary arrays

package DocClassify::Temp::Array;
use DocClassify::Temp;
use Tie::File::Indexed::JSON;
use strict;

##======================================================================
## Globals
our @ISA = qw(Tie::File::Indexed::JSON DocClassify::Temp);

##======================================================================
## DocClassify::Temp API

## $tied = TIEARRAY($classname, $filename, %opts)
##  + honors 'UNLINK' option in %opts to auto-unlink $filename on object destruction
sub TIEARRAY {
  my ($that,$file,%opts) = @_;
  return $that->SUPER::TIEARRAY($file,
				#%opts,
				mode=>'rw',
				temp=>(!exists($opts{UNLINK}) || $opts{UNLINK}),
			       );
}

## undef = $obj->cleanup()
##  + unlink temp files (only if created with 'UNLINK' option)
sub cleanup {
  $_[0]->unlink() if ($_[0]{temp});
}


1; ##-- be happy

__END__
