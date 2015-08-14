## -*- Mode: CPerl -*-
## File: DocClassify::Temp::Array.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: temporary arrays

package DocClassify::Temp::Hash;
use DocClassify::Temp;
use DB_File;
use Fcntl qw(:DEFAULT);
use strict;

##======================================================================
## Globals
our @ISA = qw(DB_File DocClassify::Temp);

## %TMPFILES : ("$obj" => $filename, ...)
##   + stores temporary filenames by stringified object; used by cleanup()
our %TMPFILES = qw();

##======================================================================
## Overrides

## $tied = TIEHASH($classname, $file, %opts)
##  + %opts:
##    (
##     ##-- DB_File options
##     flags => $flags,   ##-- DB_File flags; default=O_RDWR|O_CREAT|O_TRUNC
##     mode => $mode,     ##-- DB_File mode; default=(0666 & ~umask)
##     type => $type,     ##-- DB_File type; default=$DB_File::DB_BTREE
##     ##
##     ##-- local options
##     UNLINK => $bool,   ##-- if unspecified or true, file will be unlinked on DESTROY
##    )
sub TIEHASH {
  my ($that,$file,%opts) = @_;
  my $flags = $opts{flags} // (O_RDWR|O_CREAT|O_TRUNC);
  my $mode  = $opts{mode}  // (0666 & ~umask);
  my $type  = $opts{type}  // $DB_File::DB_BTREE;
  my $tied = DB_File->TIEHASH($file, $flags, $mode, $type)
    or $that->logconfess("TIEHASH(): failed to tie '$file' via DB_File: $!");
  bless($tied, ref($that)||$that);

  ##-- track temporary files
  if (!exists($opts{UNLINK}) || $opts{UNLINK}) {
    $TMPFILES{"$tied"} = $file;
  }

  ##-- setup key filters for UTF-8 keys
  $tied->filter_fetch_key(sub { utf8::decode($_) if (defined($_)); });
  $tied->filter_store_key(sub { utf8::encode($_) if (defined($_)); });

  return $tied;
}

## undef = $tied->DESTROY()
##  + destructor calls cleanup()
sub DESTROY {
  my $tied = shift;

  ##-- inherited destructor
  $tied->DB_File::DESTROY();

  ##-- unlink temps
  $tied->cleanup();
}

##======================================================================
## DocClassify::Temp API

## undef = $obj->cleanup()
##  + unlink temp files (only if 'temp' attribute is set)
sub cleanup {
  my $obj = shift;
  return if (!exists($TMPFILES{"$obj"}));
  CORE::unlink($TMPFILES{"$obj"});
  delete $TMPFILES{"$obj"};
}

##======================================================================
## Global destruction
END {
  CORE::unlink($_) foreach (values %TMPFILES);
}


1; ##-- be happy

__END__
