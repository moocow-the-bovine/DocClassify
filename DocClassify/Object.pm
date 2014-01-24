## -*- Mode: CPerl -*-
## File: DocClassify::Signature.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: generic objects

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

##-- storable v2.18, v2.21 can't handle qr//-style Regexp regexes
#use Regexp::Copy;     ##-- occasional segfaults with Regexp::Copy-0.06, Storable-2.21, perl-5.10.0 on SuSE x86-64
#use Regexp::Storable; ##-- there's a bug in this; hack is in DocClassify::Utils

use DocClassify::Logger;
use DocClassify::Utils ':all';  ##-- load this AFTER Regexp::Copy, Regexp::Storable (and don't store compiled regexes!)
use Data::Dumper;
use IO::File;
use Carp;
use strict;

our @ISA = qw(DocClassify::Logger);

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
## Methods: I/O: Generic

## @IO_MODES
##  + array of I/O modes:
##     ( {name=>$modeName,re=>$filename_re,method=>$methodInfix, ...}, ... )
our (@IO_MODES);
BEGIN {
  @IO_MODES =
    (
     {name=>'bin',re=>qr/\.(?:bin|sto|frz)$/i,method=>'Bin'},
     {name=>'text',re=>qr/\.(?:txt)$/i,method=>'Text'},
     {name=>'csv',re=>qr/\.(?:csv)$/i,method=>'Csv'},
     {name=>'xml',re=>qr/\.(?:xml)$/i,method=>'Xml'},
     {name=>'perl',re=>qr/\.(?:perl|pl|plm)$/i,method=>'Perl'},
    );
}

## %IO_ALIAS
##  + hash-ref of I/O mode aliases:
##     ( $modeName=>\%modeHash, ..., 'DEFAULT'=>\%defaultHash )
our (%IO_ALIAS);
BEGIN {
  %IO_ALIAS =
    (
     (map {($_->{name}=>$_)} @IO_MODES),
     'txt'=>'text',
     'native'=>'text',
     'DEFAULT' => 'bin',
    );
  foreach (keys(%IO_ALIAS)) {
    $IO_ALIAS{ucfirst($_)} = $IO_ALIAS{uc($_)} = $IO_ALIAS{lc($_)} = $IO_ALIAS{$_};
  }
}

## $mode_hash_or_name = $CLASS_OR_OBJ->defaultIoMode()
##  + returns default I/O mode for object
##  + default implementation just returns 'DEFAULT'
sub defaultIoMode { return 'DEFAULT'; }

## \%mode = $CLASS_OR_OBJ->guessFileMode($filename,%opts)
##  + guesses I/O mode name from $filename
sub guessFileMode {
  my ($obj,$file,%opts) = @_;
  if (defined($file) && !defined($opts{mode})) {
    ##-- guess mode from filename
    foreach (@IO_MODES) {
      return $_ if ($file =~ m/$_->{re}/);
    }
  }
  ##-- use default mode: get class default
  my $mode = ($opts{mode} || $obj->defaultIoMode);
  $mode=$IO_ALIAS{$mode} while (!ref($mode) && exists($IO_ALIAS{$mode}));
  return ref($mode) ? $mode : $IO_MODES[0];
}

## $bool = $obj->saveFile($filename_or_fh,%opts)
##  + %opts:
##     mode=>$mode,  ##-- I/O mode; otherwise guessed from $filename_or_fh
##     #others passed to $obj->saveModeFile()
BEGIN { *save = \&saveFile; }
sub saveFile {
  my ($obj,$file,%opts) = @_;
  my $mode = $obj->guessFileMode($file,%opts);
  my $method = "save".($mode->{method} || ucfirst($mode->{name}))."File";
  my $sub = $obj->can($method);
  $obj->logconfess("saveFile(): no method for output mode '$mode->{name}'") if (!$sub);
  $obj->vlog('info', "saveFile($file) [mode=$mode->{name}]") if ($opts{verboseIO});
  return $sub->($obj,$file,%opts);
}

## $obj = $CLASS_OR_OBJ->loadFile($filename_or_fh,%opts)
##  + %opts:
##     mode=>$mode,  ##-- I/O mode; otherwise guessed from $filename_or_fh
##     #others passed to $obj->loadModeFile()
BEGIN { *load = \&loadFile; }
sub loadFile {
  my ($that,$file,%opts) = @_;
  my $mode = $that->guessFileMode($file,%opts);
  my $method = "load".($mode->{method} || ucfirst($mode->{name}))."File";
  my $sub = $that->can($method);
  $that->logconfess("loadFile(): no method for input mode '$mode->{name}'") if (!$sub);
  $that->vlog('info',"loadFile($file) [mode=$mode->{name}]") if ($opts{verboseIO});
  return $sub->($that,$file,%opts);
}

## $str = $obj->saveString(%opts)
##  + %opts:
##     mode=>$mode,  ##-- I/O mode; otherwise guessed from $filename_or_fh
##     #others passed to $obj->saveModeFile()
BEGIN { *asString = \&saveString; }
sub saveString {
  my ($obj,%opts) = @_;
  my $mode = $obj->guessFileMode(undef,%opts);
  my ($fh,$ref) = stringfh('>');
  $obj->saveFile($fh,%opts,mode=>$mode);
  $fh->close();
  return $$ref;
}

## $obj = $CLASS_OR_OBJ->loadString($str,%opts)
##  + %opts:
##     mode=>$mode,  ##-- I/O mode; otherwise guessed from $filename_or_fh
##     #others passed to $obj->loadModeFile()
BEGIN { *fromString = \&loadString; }
sub loadString {
  my ($that,$str,%opts) = @_;
  my $mode = $that->guessFileMode(undef,%opts);
  my ($fh,$ref) = stringfh('<',\$str);
  my $obj = $that->loadFile($fh,%opts,mode=>$mode);
  $fh->close();
  return $obj;
}

##==============================================================
## Methods: I/O: Binary

##--------------------------------------------------------------
## Methods: I/O: Binary: save

## $bool = $obj->saveBinFile($filename_or_fh,%opts)
##  + %opts:
##     netorder => $bool,  ##-- store in network order? (default=0)
sub saveBinFile {
  my ($obj,$file,%opts) = @_;
  #$obj->vlog('info', "saveBinFile($file) [netorder=".($opts{netorder} ? 1 : 0),"]");
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  $obj->logconfess("saveBinFile(): open failed for '$file': $!") if (!defined($fh));
  my $rc = $opts{netorder} ? Storable::nstore_fd($obj,$fh) : Storable::store_fd($obj,$fh);
  $fh->close() if (!ref($file));
  return $rc;
}

## $str = $obj->saveBinString()
sub saveBinString {
  my $obj = shift;
  my ($fh,$ref) = stringfh('>');
  $obj->saveBinFile($fh,@_);
  $fh->close();
  return $$ref;
}

##--------------------------------------------------------------
## Methods: I/O: Binary: load

## $bool = $CLASS_OR_OBJECT->loadBinFile($filename_or_fh)
sub loadBinFile {
  my ($obj,$file) = @_;
  #$obj->vlog('info', "loadBinFile($file)");
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  $obj->logconfess("loadBinFile(): open failed for '$file': $!") if (!defined($fh));
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

##==============================================================
## Methods: I/O: Text

##--------------------------------------------------------------
## Methods: I/O: Text: save

## $bool = $obj->saveTextFile($filename_or_fh)
##  + dummy method
sub saveTextFile {
  my $obj = shift;
  $obj->logconfess("saveTextFile(): not implemented");
}

## $str = $obj->saveTextString()
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
  $obj->logconfess("loadTextFile(): not implemented");
}

## $str = $CLASS_OR_OBJECT->loadTextString($str)
sub loadTextString {
  my $obj = shift;
  my ($fh,$ref) = stringfh('<',\$_[0]);
  my $rc = $obj->loadTextFile($fh);
  $fh->close();
  return $rc;
}

##==============================================================
## Methods: I/O: Perl

## @keys = $class_or_obj->noSaveKeys()
##  + returns list of keys not to be saved for perl-mode I/O
##  + default just returns empty list
sub noSaveKeys { return qw(); }

## $saveRef = $obj->savePerlRef()
##  + return reference to be saved
##  + default implementation assumes $obj is HASH-ref
sub savePerlRef {
  my $obj = shift;
  my %noSave = map {($_=>undef)} $obj->noSaveKeys;
  return {
	  map { ($_=>(ref($obj->{$_}) && UNIVERSAL::can($obj->{$_},'savePerlRef') ? $obj->{$_}->savePerlRef : $obj->{$_})) }
	  grep {
	    (!exists($noSave{$_})
	     && (!ref($obj->{$_})
		 || !UNIVERSAL::isa($obj->{$_},'CODE')
		 || !UNIVERSAL::isa($obj->{$_},'GLOB')
		 || !UNIVERSAL::isa($obj->{$_},'IO::Handle')
		)
	    )}
	  keys(%$obj)
	 };
}

## $loadedObj = $CLASS_OR_OBJ->loadPerlRef($ref)
##  + default implementation just clobbers $CLASS_OR_OBJ with $ref and blesses
sub loadPerlRef {
  my ($that,$ref) = @_;
  $that = ref($ref) if (UNIVERSAL::isa($ref,$that)); ##-- "virtual load": return subclass for superclass method
  #my $obj = ref($that) ? $that : $that->new();
  #$obj = bless(unifyClobber($obj,$ref,undef),ref($obj));
  ##--
  my $obj = bless($ref,ref($that)||$that);
  if (UNIVERSAL::isa($that,'HASH') && UNIVERSAL::isa($obj,'HASH')) {
    %$that = %$obj; ##-- hack in case someone does "$obj->load()" and expects $obj to be destructively altered...
    return $that;
  } elsif (UNIVERSAL::isa($that,'ARRAY') && UNIVERSAL::isa($obj,'ARRAY')) {
    @$that = @$obj; ##-- ... analagous hack for array refs
    return $that;
  } elsif (UNIVERSAL::isa($that,'SCALAR') && UNIVERSAL::isa($obj,'SCALAR')) {
    $$that = $$obj; ##-- ... analagous hack for scalar refs
    return $that;
  }
  return $obj;
}

##----------------------------------------------------
## Methods: Persistence: Perl: File (delegate to string)

## $rc = $obj->savePerlFile($filename_or_fh, @args)
##  + calls "$obj->savePerlString(@args)"
sub savePerlFile {
  my ($obj,$file) = (shift,shift);
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  $obj->logconfess("savePerlFile(): open failed for '$file': $!") if (!$fh);
  $fh->print("## Perl code auto-generated by ", __PACKAGE__, "::savePerlFile()\n",
	     "## EDIT AT YOUR OWN RISK\n",
	     $obj->savePerlString(@_));
  $fh->close() if (!ref($file));
  return 1;
}

## $obj = $CLASS_OR_OBJ->loadPerlFile($filename_or_fh, %args)
##  + calls $CLASS_OR_OBJ->loadPerlString(var=>undef,src=>$filename_or_fh, %args)
sub loadPerlFile {
  my ($that,$file,%args) = @_;
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  $that->logconfess("loadPerlFile(): open failed for '$file': $!") if (!$fh);
  local $/=undef;
  my $str = <$fh>;
  $fh->close() if (!ref($file));
  return $that->loadPerlString($str, var=>undef, src=>$file, %args);
}

##----------------------------------------------------
## Methods: Persistence: Perl: String (perl code)

## $str = $obj->savePerlString(%args)
##  + save $obj as perl code
##  + %args:
##      var => $perl_var_name
sub savePerlString {
  my ($obj,%args) = @_;
  my $var = $args{var} ? $args{var} : '$obj';
  my $ref = $obj->savePerlRef();
  my $dumper = Data::Dumper->new([$ref],[$var]);
  $dumper->Indent(1)->Purity(1)->Terse(0)->Sortkeys(1);
  return $dumper->Dump;
}

## $obj = $CLASS_OR_OBJ->loadPerlString($str,%args)
##  + %args:
##     var=>$perl_var_name, ##-- default='$index'                 ; local var: $VAR
##     src=>$src_name,      ##-- default=(substr($str,0,42).'...'); local var: $SRC
##     %more_obj_args,      ##-- literally inserted into $obj
##  + load from perl code string
sub loadPerlString {
  my ($that,$str,%args) = @_;
  my $var = $args{var} ? $args{var} : '$obj';
  my $src = (defined($args{src})
	     ? $args{src}
	     : (length($str) <= 42
		? $str
		: (substr($str,0,42).'...')));
  my $VAR = $var;
  my $SRC = (defined($args{src}) ? $args{src} : '/dev/null');
  #my $TOP = $DTA::CAB::Unify::TOP;
  delete(@args{qw(var src)});

  my $loaded = eval("no strict; $str; $var");
  $that->logconfess("loadString(): eval() failed for '$src': $@")
    if ($@ || !defined($loaded)); #|| $!

  return $that->loadPerlRef($loaded);
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009,2010 by Bryan Jurish

This package is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut


=cut
