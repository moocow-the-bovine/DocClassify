## -*- Mode: CPerl -*-
## File: DocClassify::Signature.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: language identification utilities: generic objects


package DocClassify::Object;

##-- NO
#use PDL;
#use PDL::IO::Storable;
#use Storable;
##
##-- NO
#use Storable;
#use PDL;
#use PDL::IO::Storable;
##
##-- YES
use PDL;
use Storable;
use PDL::IO::Storable;

use DocClassify::Utils ':all';
use IO::File;
use Carp;
use strict;

##==============================================================================
## Constructors etc.

## $obj = $CLASS_OR_OBJ->new(%opts)
##  + returns new object as hash-ref
sub new {
  my $that = shift;
  return bless({ @_ }, ref($that)||$that);
}

## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + default just returns empty list
sub noShadowKeys {
  return qw();
}

## $obj2 = $obj->shadow(%opts)
##  + new object identical to $obj, overriding %opts
sub shadow {
  my $obj = shift;
  my %oopts = %$obj;
  delete(@oopts{$obj->noShadowKeys});
  return ref($obj)->new(%oopts,@_);
}

## $obj2 = $obj->clone()
##  + uses Storable::dclone
sub clone {
  return Storable::dclone($_[0]);
}

##==============================================================================
## Methods: I/O

##--------------------------------------------------------------
## Methods: I/O: Binary: save

## $bool = $obj->saveBinFile($filename_or_fh)
sub saveBinFile {
  my ($obj,$file) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  confess(ref($obj)."::saveBinFile(): open failed for '$file': $!") if (!defined($fh));
  my $rc = Storable::store_fd($obj,$fh);
  $fh->close() if (!ref($file));
  return $rc;
}

## $str = $obj->saveBinString()
sub saveBinString {
  my $obj = shift;
  my ($fh,$ref) = stringfh('>');
  $obj->saveBinFile($fh);
  $fh->close();
  return $$ref;
}

##--------------------------------------------------------------
## Methods: I/O: Binary: load

## $bool = $CLASS_OR_OBJECT->loadBinFile($filename_or_fh)
sub loadBinFile {
  my ($obj,$file) = @_;
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  confess(ref($obj)."::loadBinFile(): open failed for '$file': $!") if (!defined($fh));
  my $robj = Storable::retrieve_fd($fh);
  $fh->close() if (!ref($file));
  if (ref($obj) && $obj->isa('HASH') && $robj->isa('HASH')) {
    %$obj = %$robj;
    return $obj;
  }
  return $robj;
}

## $str = $CLASS_OR_OBJECT->loadBinString($str)
sub loadBinString {
  my $obj = shift;
  my ($fh,$ref) = stringfh('<',\$_[0]);
  my $rc = $obj->loadBinFile($fh);
  $fh->close();
  return $rc;
}

##--------------------------------------------------------------
## Methods: I/O: Text: save

## $bool = $obj->saveTextFile($filename_or_fh)
##  + dummy method
sub saveTextFile {
  my $obj = shift;
  confess(ref($obj)."saveTextFile(): not implemented");
}

## $str = $obj->saveTextString()
BEGIN { *asString = \&saveTextString; }
sub saveTextString {
  my $obj = shift;
  my ($fh,$ref) = stringfh('>');
  $obj->saveTextFile($fh);
  $fh->close();
  return $$ref;
}

##--------------------------------------------------------------
## Methods: I/O: Text: load

## $bool = $CLASS_OR_OBJECT->loadTextFile($filename_or_fh)
##  + dummy method
sub loadTextFile {
  my $obj = shift;
  confess(ref($obj)."loadTextFile(): not implemented");
}

## $str = $CLASS_OR_OBJECT->loadTextString($str)
BEGIN { *fromString = \&loadTextString; }
sub loadTextString {
  my $obj = shift;
  my ($fh,$ref) = stringfh('<',\$_[0]);
  my $rc = $obj->loadTextFile($fh);
  $fh->close();
  return $rc;
}

##==============================================================================
## Methods: ...

##==============================================================================
## Footer
1;

__END__
##========================================================================
## POD DOCUMENTATION, auto-generated by podextract.perl, edited
=pod

=cut

##========================================================================
## NAME
=pod

=head1 NAME

DocClassify::Object - language guesser: generic objects

=cut

##========================================================================
## SYNOPSIS
=pod

=head1 SYNOPSIS

 ##========================================================================
 ## PRELIMINARIES
 
 use DocClassify::Object;
 
 ##========================================================================
 ## Constructors etc.
 
 $obj = $CLASS_OR_OBJ->new(%opts);
 @noShadowKeys = $obj->noShadowKeys();
 $obj2 = $obj->shadow(%opts);
 $obj2 = $obj->clone();
 
 ##========================================================================
 ## Methods: I/O
 
 $bool = $obj->saveBinFile($filename_or_fh);
 $str = $obj->saveBinString();
 $bool = $CLASS_OR_OBJECT->loadBinFile($filename_or_fh);
 $str = $CLASS_OR_OBJECT->loadBinString($str);
 $bool = $obj->saveTextFile($filename_or_fh);
 $str = $obj->saveTextString();
 $bool = $CLASS_OR_OBJECT->loadTextFile($filename_or_fh);
 $str = $CLASS_OR_OBJECT->loadTextString($str);
 

=cut

##========================================================================
## DESCRIPTION
=pod

=head1 DESCRIPTION

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Object: Constructors etc.
=pod

=head2 Constructors etc.

=over 4

=item new

 $obj = $CLASS_OR_OBJ->new(%opts);

Returns new object as hash-ref.


=item noShadowKeys

 @noShadowKeys = $obj->noShadowKeys();


Returns list of keys not to be passed to $CLASS-E<gt>new() on shadow().
Default just returns empty list



=item shadow

 $obj2 = $obj->shadow(%opts);

Returns new object identical to $obj, overriding %opts.

=item clone

 $obj2 = $obj->clone();

Object cloning utility using Storable::dclone.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Object: Methods: I/O
=pod

=head2 Methods: I/O

=over 4

=item saveBinFile

 $bool = $obj->saveBinFile($filename_or_fh);

Save object to a binary file or handle using Storable module.

=item saveBinString

 $str = $obj->saveBinString();

Save object to a binary string.

=item loadBinFile

 $bool = $CLASS_OR_OBJECT->loadBinFile($filename_or_fh);

Load object from a binary file or handle.

=item loadBinString

 $str = $CLASS_OR_OBJECT->loadBinString($str);

Load object form a binary string.

=item saveTextFile

 $bool = $obj->saveTextFile($filename_or_fh);

Save object to a text file or handle.
Not supported by all subclasses.

Dummy method, override to make available for a new subclass.

=item saveTextString

 $str = $obj->saveTextString();

Save object to a text string.
Not supported by all subclasses.

=item loadTextFile

 $bool = $CLASS_OR_OBJECT->loadTextFile($filename_or_fh);

Load object from a text file or handle.
Not supported by all subclasses.

Dummy method, override to make available for a new subclass.

=item loadTextString

 $str = $CLASS_OR_OBJECT->loadTextString($str);

Load object from text string.

=back

=cut

##========================================================================
## END POD DOCUMENTATION, auto-generated by podextract.perl
=pod



=cut

=cut

##======================================================================
## See Also
=pod

=head1 SEE ALSO

DocClassify(3pm)

=cut

##======================================================================
## Footer
=pod

=head1 AUTHOR

Bryan Jurish E<lt>jurish@uni-potsdam.deE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Bryan Jurish

This package is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut


=cut