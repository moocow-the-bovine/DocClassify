## -*- Mode: CPerl -*-
## File: DocClassify::Object.pm
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
use PDL::IO::FastRaw;
use PDL::IO::Misc;
use PDL::IO::FITS;
use PDL::Types;

##-- storable v2.18, v2.21 can't handle qr//-style Regexp regexes
#use Regexp::Copy;     ##-- occasional segfaults with Regexp::Copy-0.06, Storable-2.21, perl-5.10.0 on SuSE x86-64
#use Regexp::Storable; ##-- there's a bug in this; hack is in DocClassify::Utils

use DocClassify::Logger;
use DocClassify::Utils ':all';  ##-- load this AFTER Regexp::Copy, Regexp::Storable (and don't store compiled regexes!)
use Data::Dumper;
use IO::File;
use File::Path;			##-- for directory I/O e.g. make_path() etc.
use JSON;
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
     {name=>'textdir',re=>qr/\.t(?:e?xt)?\.?d$/, method=>'TextDir'},
     {name=>'textfitsdir',re=>qr/\.t(?:e?xt)?\.?f(?:its)?\.?d$/, method=>'TextDir', opts=>{pdlio=>'fits'}},
     {name=>'fitsdir',re=>qr/\.f(?:its)?\.?d$/, method=>'Dir', opts=>{pdlio=>'fits'}},
     {name=>'dir',re=>qr/\.d$/, method=>'Dir'},
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
     'directory'=>'dir',
     (map {($_=>'textdir')} qw(import export txtdir td)),
     (map {($_=>'fitsdir')} qw(fits fitsd fd)),
     (map {($_=>'textfitsdir')} qw(textfits tfitsdir tfdir tfd)),
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
      return $_ if (defined($_->{re}) && $file =~ m/$_->{re}/);
    }
    ##-- try 'dir' mode for directories
    if (defined($file) && -d $file) {
      my $mode = (grep {$_->{name} eq 'dir'} @IO_MODES)[0];
      return $mode if ($mode);
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
  my $method = "save".($mode->{method} || ucfirst($mode->{name}));
  my $sub = $obj->can($method) || $obj->can("${method}File");
  $obj->logconfess("saveFile(): no method for output mode '$mode->{name}'") if (!$sub);
  $obj->vlog('info', "saveFile($file) [mode=$mode->{name}]") if ($opts{verboseIO});
  return $sub->($obj,$file,%{$mode->{opts}//{}},%opts);
}

## $obj = $CLASS_OR_OBJ->loadFile($filename_or_fh,%opts)
##  + %opts:
##     mode=>$mode,  ##-- I/O mode; otherwise guessed from $filename_or_fh
##     #others passed to $obj->loadModeFile()
BEGIN { *load = \&loadFile; }
sub loadFile {
  my ($that,$file,%opts) = @_;
  my $mode = $that->guessFileMode($file,%opts);
  my $method = "load".($mode->{method} || ucfirst($mode->{name}));
  my $sub = $that->can($method) || $that->can("${method}File");
  $that->logconfess("loadFile(): no method for input mode '$mode->{name}'") if (!$sub);
  $that->vlog('info',"loadFile($file) [mode=$mode->{name}]") if ($opts{verboseIO});
  return $sub->($that,$file,%{$mode->{opts}//{}},%opts);
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

##==============================================================
## Methods: I/O: Directory

##--------------------------------------------------------------
## Methods: I/O: Directory: save

## @keys = $obj->dirHeaderKeys()
##  + keys for header save
##  + default returns keys of all non-referencesin %$obj
sub dirHeaderKeys {
  my $obj = shift;
  return grep {!ref($obj->{$_})} keys %$obj;
}

## \%data = $obj->dirHeaderData()
##  + local data to save in $dir/header.json for saveDir()
##  + default returns hash of $obj->dirHeaderKeys() and special '__CLASS__' key
sub dirHeaderData {
  my $obj = shift;
  return { __CLASS__=>(ref($obj)||$obj), (map {($_=>$obj->{$_})} grep {exists($obj->{$_})} $obj->dirHeaderKeys) };
}

## $bool = $obj->saveDirHeader($dirname_or_filename)
##  + save "$dir/header.json"
sub saveDirHeader {
  my ($obj,$dir,%opts) = @_;
  my $file = (-d $dir ? "$dir/header.json" : $dir);
  $obj->trace("saveDirHeader(): $file") if ($opts{verboseIO});
  return $obj->writeJsonFile($obj->dirHeaderData(), $file);
}

## $bool = $obj->saveDir($dirname,%opts)
##  + abstract method loads header and calls saveDirData()
sub saveDir {
  my ($obj,$dir,%opts) = @_;
  $dir =~ s{/$}{};
  $obj->trace("saveDir($dir)") if ($opts{verboseIO});
  -d $dir || File::Path::make_path($dir)
      or $obj->logconfess("saveDir(): failed to create directory $dir/: $!");

  ##-- save: header
  $obj->saveDirHeader($dir,%opts)
    or $obj->logconfess("saveDir(): failed to save header to $dir/: $!");

  ##-- save: data
  return $obj->saveDirData($dir,%opts);
}

## $bool = $obj->saveDirData($dirname)
##  + dummy method
sub saveDirData {
  my ($obj,$dir,%opts) = @_;
  $obj->logconfess("saveDirData(): not implemented");
}

##--------------------------------------------------------------
## Methods: I/O: Directory: load

## $obj = $CLASS_OR_OBJECT->loadDirHeader($dirname_or_filename,%opts)
##  + loads "$dir/header.json"
sub loadDirHeader {
  my ($that,$dir,%opts) = @_;
  my $file = -d $dir ? "$dir/header.json" : $dir;
  $that->trace("loadDirHeader($file)") if ($opts{verboseIO});
  my $hdr = $that->readJsonFile($file)
    or $that->logconfess("loadDirHeader(): failed to load $file: $!");
  my $class = $hdr->{__CLASS__} || $hdr->{class} || ref($that) || $that || __PACKAGE__;
  $class    = undef if ($class =~ /^(?:HASH|ARRAY|SCALAR)$/);
  delete $hdr->{__CLASS__};
  if (UNIVERSAL::isa($that,'HASH')) {
    @$that{keys %$hdr} = values %$hdr;
    bless($that,$class) if ($class && UNIVERSAL::isa($class,ref($that)));
    return $that;
  }
  bless($hdr,$class) if ($class);
  return $hdr;
}

## $bool = $CLASS_OR_OBJECT->loadDir($dirname,%opts)
##  + abstract method loads header and calls loadDirData()
sub loadDir {
  my ($that,$dir,%opts) = @_;
  $dir =~ s{/$}{};

  $that->trace("loadDir($dir)") if ($opts{verboseIO});
  $that->logconfess("loadDir(): no such directory $dir") if (!-d $dir);

  ##-- load: header
  my $obj = $that->loadDirHeader($dir,@_)
    or $that->logconfess("loadDir(): failed to load header from $dir/: $!");

  ##-- load: data
  return $obj->loadDirData($dir,@_);
}

## $obj = $obj->loadDirData($dirname,%opts)
##  + dummy method
sub loadDirData {
  my ($obj,$dir,%opts) = @_;
  $obj->logconfess("loadDirData(): not implemented");
}

##==============================================================
## Methods: I/O: textdir

##--------------------------------------------------------------
## Methods: I/O: textdir: save

## $bool = $obj->saveTextDir($dirname)
##  + abstract method loads header and calls saveTextDirData()
sub saveTextDir {
  my ($obj,$dir,%opts) = @_;
  $dir =~ s{/$}{};
  $obj->trace("saveTextDir($dir)") if ($opts{verboseIO});
  -d $dir || File::Path::make_path($dir)
      or $obj->logconfess("saveTextDir(): failed to create directory $dir/: $!");

  ##-- save: header
  $obj->saveDirHeader($dir,%opts)
    or $obj->logconfess("saveTextDir(): failed to save header to $dir/: $!");

  ##-- save: data
  return $obj->saveTextDirData($dir,%opts);
}

## $bool = $obj->saveTextDirData($dirname)
##  + dummy method
sub saveTextDirData {
  my ($obj,$dir,%opts) = @_;
  $obj->logconfess("saveTextDirData(): not implemented");
}

##--------------------------------------------------------------
## Methods: I/O: textdir: load

## $bool = $CLASS_OR_OBJECT->loadTextDir($dirname,%opts)
##  + abstract method loads header and calls loadTextDirData()
sub loadTextDir {
  my ($that,$dir,%opts) = @_;
  $dir =~ s{/$}{};
  $that->trace("loadTextDir($dir)") if ($opts{verboseIO});
  $that->logconfess("loadTextDir(): no such directory $dir") if (!-d $dir);

  ##-- load: header
  my $obj = $that->loadDirHeader($dir,%opts)
    or $that->logconfess("loadTextDir(): failed to load header from $dir/: $!");

  ##-- load: data
  return $obj->loadTextDirData($dir,%opts);
}

## $obj = $obj->loadTextDirData($dirname,%opts)
##  + dummy method
sub loadTextDirData {
  my ($obj,$dir,%opts) = @_;
  $obj->logconfess("loadTextDirData(): not implemented");
}

##==============================================================================
## Methods: I/O: JSON utilities

## $data = $obj->TO_JSON()
##  + wrapper for JSON module
sub TO_JSON {
  return { %{$_[0]} };
}

## $bool = $CLASS_OR_OJBECT->writeJsonFile($data,$filename_or_fh,%opts)
sub writeJsonFile {
  my ($that,$data,$file,%opts) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  $that->logconfess("writeJsonDataFile(): open failed for '$file': $!") if (!defined($fh));
  binmode($fh,':raw');
  $fh->print(to_json($data, {utf8=>1, allow_nonref=>1, allow_unknown=>1, allow_blessed=>1, convert_blessed=>1, pretty=>1, canonical=>1, %opts}));
  if (!ref($file)) {
    $fh->close() or $that->logconfess("writeJsonDataFile(): failed to close $file: $!");
  }
  return 1;
}

## $data = $CLASS_OR_OJBECT->readJsonFile($filename_or_fh,%opts)
sub readJsonFile {
  my ($that,$file,%opts) = @_;
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  $that->logconfess("readJsonFile(): open failed for $file: $!") if (!defined($fh));
  my $buf;
  {
    local $/ = undef;
    $buf = <$fh>;
  }
  $fh->close() if (!ref($file));
  return from_json($buf, {utf8=>!utf8::is_utf8($buf), relaxed=>1, allow_nonref=>1, %opts});
}

##==============================================================================
## Methods: I/O: PDL utilities: binary

## $bool = $CLASS_OR_OBJECT->writePdlFile($pdl, $filename, %opts)
##  + %opts:
##     verboseIO => $bool,
##     pdlio     => $how,   ##-- one of qw(raw fits); default='fits'
sub writePdlFile {
  my ($that,$pdl,$file,%opts) = @_;
  $opts{pdlio} //= 'raw';
  if (defined($pdl)) {
    $that->trace("writePdlFile($file) [pdlio=$opts{pdlio}]") if ($opts{verboseIO});
    local $,='';
    if ($opts{pdlio} eq 'fits') {
      ##-- write: fits
      if (!$pdl->can('wfits') && UNIVERSAL::isa($pdl,'PDL::CCS::Nd')) {
	##-- write: fits: ccs (hack)
	PDL::CCS::IO::FastRaw::_ccsio_write_header({magic=>(ref($pdl)." $PDL::CCS::Nd::VERSION"),
						    pdims=>$pdl->pdims, vdims=>$pdl->vdims, flags=>$pdl->flags},
						   "$file.hdr")
	    or $that->logconfess("writePdlFile(): failed to save header to $file.hdr: $!");
	my $ix = $pdl->_whichND;
	$ix = $ix->long if ($ix->type->ioname eq 'indx'); ##-- hack: treat 'indx' as 'long' for PDL::IO::FITS in PDL v2.006_90
	$ix->wfits("$file.ix.fits")
	    or $that->logconfess("writePdlFile(): failed to save indices to $file.ix.fits: $!");
	$pdl->_vals->wfits("$file.nz.fits")
	    or $that->logconfess("writePdlFile(): failed to save values to $file.ix.fits: $!");
      } else {
	##-- write: fits: pdl
	$pdl->wfits("$file.fits")
	  or $that->logconfess("writePdlFile(): wfits() failed for '$file': $!");
      }
    } else {
      ##-- write: raw
      $pdl->writefraw($file)
	or $that->logconfess("writePdlFile(): writefraw() failed for '$file': $!");
    }
  }
  else {
    $that->trace("writePdlFile($file): unlink") if ($opts{verboseIO});
    foreach (grep {-e "file$_"} ('','.hdr','.ix','.ix.hdr','.nz','.nz.hdr','.fits')) {
      unlink("file$_") or $that->logconfess(__PACKAGE__, "::writePdlFile(): failed to unlink '$file$_': $!");
    }
  }
  return 1;
}

## $pdl = $CLASS_OR_OBJECT->readPdlFile($filename,%opts)
##  + %opts:
##     class=>$class,
##     mmap =>$bool,
##     verboseIO=>$bool,
##     pdlio =>$how, ##-- one of qw(raw fits); default='raw'
sub readPdlFile {
  my ($that,$file,%opts) = @_;
  $opts{mmap} //= 0;
  $opts{pdlio} //= 'raw';
  $that->trace("readPdlFile($file) [pdlio=$opts{pdlio},mmap=$opts{mmap}]") if ($opts{verboseIO});
  my $class = $opts{class} // 'PDL';
  my $pdl;
  if ($opts{pdlio} eq 'fits') {
    ##-- read: fits
    $that->logwarn("readPdlFile(): mmap not supported for FITS file $file") if ($opts{mmap});
    if (!$class->can('rfits') && UNIVERSAL::isa($class,'PDL::CCS::Nd')) {
      ##-- read: fits: ccs
      return undef if (!-e "$file.hdr");
      my $hdr = PDL::CCS::IO::FastRaw::_ccsio_read_header("$file.hdr")
	or $that->logconfess("readPdlFile(): failed to read header from $file.hdr: $!");
      defined(my $ix = PDL->rfits("$file.ix.fits"))
	or $that->logconfess("readPdlFile(): failed for $file.ix.fits: $!");
      defined(my $nz = PDL->rfits("$file.nz.fits"))
	or $that->logconfess("readPdlFile(): failed for $file.nz.fits: $!");
      $pdl = $class->newFromWhich($ix,$nz, pdims=>$hdr->{pdims}, vdims=>$hdr->{vdims}, sorted=>1, steal=>1);
    }
    else {
      ##-- read: fits: pdl
      return undef if (!-e "$file.fits");
      defined($pdl = $class->rfits("$file.fits"))
	or $that->logconfess("readPdlFile(): rfits() failed for '$file' via class '$class': $!");
    }
  } else {
    ##-- read or map: raw
    return undef if (!-e "$file.hdr");
    my $ro   = (!$opts{mmap} || (exists($opts{ReadOnly}) ? $opts{ReadOnly} : (!-w "$file.hdr"))) || 0;
    local $, = '';
    defined($pdl = $opts{mmap} ? $class->mapfraw($file,{ReadOnly=>$ro}) : $class->readfraw($file))
      or $that->logconfess("readPdlFile(): failed to ".($opts{mmap} ? 'mmap' : 'read')." pdl file '$file' via class '$class' (readonly=$ro)");
  }
  return $pdl;
}

## $pdl = $CLASS_OR_OBJECT->mmapPdlFile($filename,%opts)
sub mmapPdlFile {
  return $_[0]->readPdlFile($_[1],$_[2],1,@_[3..$#_]);
}

##==============================================================================
## Methods: I/O: PDL utilities: text

## $bool = $CLASS_OR_OBJECT->writePdlTextFile($pdl, $filename,%opts)
##  + %opts:
##     verboseIO=>$bool,
##     pdlio =>$how, ##-- one of qw(txt fits); default='txt'
sub writePdlTextFile {
  my ($that,$pdl,$file,%opts) = @_;
  return $that->writePdlFile($pdl,$file,%opts) if (($opts{pdlio}//'') eq 'fits');
  $that->trace("writePdlTextFile($file)") if ($opts{verboseIO});
  if (defined($pdl)) {
    local $,='';
    open(my $fh, ">$file")
      or $that->logconfess("writePdlTextFile(): open failed for '$file': $!");
    binmode($fh,':raw');
    $fh->print(join(' ', ref($pdl), $pdl->type->ioname, $pdl->dims), "\n");
    if (UNIVERSAL::isa($pdl,'PDL::CCS::Nd')) {
      ##-- write: ccs
      $fh->print($pdl->flags,"\n");
      $fh->close();
      $pdl->make_physically_indexed;
      return ($that->writePdlTextFile($pdl->_whichND, "$file.ix")
	      && $that->writePdlTextFile($pdl->_vals, "$file.nz"));
    }
    ##-- write: pdl
    $pdl->flat->wcols($fh);
    $fh->close()
      or $that->logconfess("writePdlTextFile(): failed to close '$file': $!")
  }
  else {
    foreach (grep {-e "file$_"} ('','.ix','.nz')) {
      unlink("file$_") or $that->logconfess(__PACKAGE__, "::writePdlFile(): failed to unlink '$file$_': $!");
    }
  }
  return 1;
}

## $pdl = $CLASS_OR_OBJECT->readTextPdlFile($filename,%opts)
##  + %opts:
##     class=>$class,
##     verboseIO=>$booll
##     pdlio =>$how, ##-- one of qw(txt fits); default='txt'
sub readPdlTextFile {
  my ($that,$file,%opts) = @_;
  return $that->readPdlFile($file,%opts) if (($opts{pdlio}//'') eq 'fits');
  $that->trace("readPdlTextFile($file)") if ($opts{verboseIO});
  return undef if (!-e $file);
  local $, = '';
  open(my $fh, "<$file")
    or $that->logconfess("readPdlTextFile(): open failed for '$file': $!");
  binmode($fh,':raw');
  my $hdr  = <$fh>;
  chomp($hdr);
  my ($hclass,$type,@dims) = split(' ',$hdr);
  my $class = $hclass // $opts{class} // 'PDL';
  if ($class eq 'PDL::CCS::Nd') {
    ##-- read: ccs
    my $flags = <$fh>;
    $fh->close();
    chomp($flags);
    my $ix = $that->readPdlTextFile("$file.ix",%opts,class=>'PDL');
    my $nz = $that->readPdlTextFile("$file.nz",%opts,class=>'PDL');
    my $ccs = PDL::CCS::Nd->newFromWhich($ix,$nz,pdims=>\@dims,flags=>$flags,sorted=>1,steal=>1);
    return $ccs;
  }
  else {
    ##-- read: dense pdl
    my $pdl = rcols($fh, {TYPES=>[PDL->can($type)->()], CHUNKSIZE=>8192});
    $fh->close();
    $pdl->reshape(@dims);
    return $pdl;
  }
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

Copyright (C) 2009-2015 by Bryan Jurish

This package is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut


=cut
