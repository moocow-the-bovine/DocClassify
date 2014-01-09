## -*- Mode: CPerl -*-
## File: DocClassify::Document.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: document (generic)

package DocClassify::Document;
use DocClassify::Object;
use DocClassify::Logger;
use DocClassify::Utils ':all';
use DocClassify::Signature;
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Object DocClassify::Logger);

## $DOC_ID
##  + used for auto-generating doc-ids
##  + currently useless (just a dumb counter, no collision checking, etc.)
our $DOC_ID = 0;

## %ALIAS
##  + subclass aliases
our (%ALIAS);
BEGIN {
  %ALIAS =
    (
     ##
     'VZ'    => 'VzXml',
     'VzXML' => 'VzXml',
     ##
     'cab' => 'CabXml',
     'CabXML'  => 'CabXml',
     'cab-xml' => 'CabXml',
     't-xml'   => 'CabXml',
     'txml'    => 'CabXml',
     'twxml'   => 'CabXml',
     ##
     'Default' => 'VzXml',
    );
  @ALIAS{map {lc($_)} keys(%ALIAS)} = values(%ALIAS);
}

##==============================================================================
## Constructors etc.

## $doc = $CLASS_OR_OBJ->new(%opts)
## %$doc, %opts:
##  ##-- source options
##  file => $srcFile,     ##-- load from file (sets label=$srcFile)
##  string => $srcString, ##-- load from string, (sets label='string')
##  label => $srcLabel,   ##-- override initial document label
##  id => $id,            ##-- unique id (string or integer; default = ++$DOC_ID)
##  ##
##  ##-- other data
##  cats => \@catList,    ##-- [ {id=>$catId,deg=>$catDeg,name=>$catName}, ... ] : see cats()
##  sig => $termSignature,##-- cached signature
##  sigFile => $sigFile,  ##-- cached signature file
##  raw => $rawText,      ##-- cached raw text
##  rawFile => $rawFile,  ##-- cached raw text file
##  ##
##  ##-- subclass data
##  #xdoc => $xmlDoc,      ##-- XML::LibXML::Document object (default: none)
sub new {
  my ($that,%opts) = @_;

  ##-- default subclass
  $opts{class} = 'Default' if ($that eq __PACKAGE__ && !exists($opts{class}));

  my ($doc);
  if (defined($opts{class})) {
    ##-- subclass selection
    my $class = $opts{class};
    $class=$ALIAS{$class} while (defined($ALIAS{$class}));
    delete($opts{class});
    $class = __PACKAGE__ . "::$class" if (!UNIVERSAL::isa($class,__PACKAGE__));
    eval "use $class;" if (!UNIVERSAL::can($class,'new'));
    $doc = $class->new(%opts);
  }
  else {
    ##-- ... otherwise just pass through to DocClassify::Object
    $doc = $that->DocClassify::Object::new(
					   ##-- source options
					   label=>undef,
					   id   =>undef,
					   xdoc =>undef,
					   %opts,
					  );
  }

  ##-- ensure label, id are set
  $doc->label() if (!defined($doc->{label}));
  $doc->id() if (!defined($doc->{id}));

  return $doc;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(xdoc id cats sig raw sigFile rawFile)
sub noShadowKeys {
  return qw(id cats sig raw sigFile rawFile);
}

## $doc = $doc->clearCache()
##  + clears cached keys qw(xdoc raw sig)
sub clearCache {
  delete(@{$_[0]}{qw(raw sig)});
  return $_[0];
}

##==============================================================================
## Methods: labelling

## $label = $doc->label()
## $label = $doc->label($label)
##  + get label, setting it if not already defined
sub label {
  my $doc = shift;
  return $doc->{label}=$_[0] if (defined($_[0]));
  return $doc->{label} if (defined($doc->{label}));
  return $doc->{label} = $doc->{file} if (defined($doc->{file}));
  #return $doc->{label} = $doc->{string} if (defined($doc->{string}));
  return $doc->{label} = 'string' if (defined($doc->{string}));
  return $doc->{label};
}

## $id = $doc->id()
## $id = $doc->id($id)
##  + get id, setting if not already defined
sub id {
  my $doc = shift;
  return $doc->{id} = $_[0] if (defined($_[0]));
  return $doc->{id} if (defined($doc->{id}));
  return $doc->{id} = ++$DOC_ID;
}

##==============================================================================
## Methods: Parsing

##--------------------------------------------------------------
## Methods: Parsing: Header Data

## $size = $doc->sizeBytes()
##  + returns size of document in raw XML bytes (usually fast)
sub sizeBytes {
  my $doc = shift;
  if (defined($doc->{str})) {
    use bytes;
    return ref($doc->{str}) ? length(${$doc->{str}}) : length($doc->{str});
  } elsif (defined($doc->{file}) && -e $doc->{file}) {
    return -s $doc->{file};
  } elsif (defined($doc->{xdoc})) {
    use bytes;
    return length($doc->{xdoc}->toString(1));
  }
  return undef;
}

## $nChars = $doc->sizeText()
##  + calls $doc->rawText
sub sizeText {
  return length(${$_[0]->rawText});
}

## $nTermTokens = $doc->sizeTerms()
##  + calls $doc->typeSignature()
sub sizeTerms {
  return $_[0]->typeSignature->{N};
}

## $nTokens = $doc->sizeTokens()
##  + calls $doc->typeSignature()
sub sizeTokens {
  my $doc = shift;
  return $doc->typeSignature->{N};
}

## @cats = $doc->cats()
## \@cats = $doc->cats()
##  + Returns array (ref) of category membership data:
##    @cats = ( {id=>$catId,deg=>$catDeg,name=>$catName}, ... )
##  + just returns $doc->{cats} if defined, otherwise calls $doc->getCats()
sub cats {
  my $doc = shift;
  $doc->{cats} = $doc->getCats()//[] if (!defined($doc->{cats}));

  ##-- sanitize & sort
  $_->{deg} = 1 foreach (grep {!defined($_->{deg})} @{$doc->{cats}});
  @{$doc->{cats}} =
    sort {($a->{deg}<=>$b->{deg}
	   || ($a->{cut}||0) <=> ($b->{cut}||0)
	   || ($b->{sim}||0) <=> ($a->{sim}||0)
	   || ($a->{dist}||0) <=> ($b->{dist}||0)
	   || ($a->{dist_raw}||0) <=> ($b->{dist_raw}||0)
	   || $a->{id}<=>$b->{id}
	   || $a->{name} cmp $b->{name})
	 } @{$doc->{cats}};

  return wantarray ? @{$doc->{cats}} : $doc->{cats};
}

## \@cats = $doc->getCats()
##  + Returns array-ref of category membership data:
##    @cats = ( {id=>$catId,deg=>$catDeg,name=>$catName}, ... )
##  + REQUIRED by subclasses
sub getCats {
  my $doc = shift;
  confess(ref($doc)."::getCats(): abstract API method called!");
  return [];
}

##--------------------------------------------------------------
## Methods: Parsing: Type-Frequency Signature (NEW)

## $sig = $doc->typeSignature(%opts)
##  + %opts: (none yet)
##  + template checks for $doc->{sig}, $doc->{sigFile}, otherwise calls $doc->getTypeSignature(%opts)
sub typeSignature {
  my $doc = shift;
  return $doc->{sig} if (defined($doc->{sig}));            ##-- check for cached object
  return DocClassify::Signature->loadFile($doc->{sigFile}) ##-- check for cached file
    if ($doc->{sigFile} && -r $doc->{sigFile});

  return $doc->getTypeSignature(@_);
}

## $sig = $doc->getTypeSignature(%opts)
##  + %opts: (none yet)
##  + (re-)generate document signature; guts for typeSignature() method
##  + REQUIRED by subclasses
sub getTypeSignature {
  my $doc = shift;
  confess(ref($doc)."::getTypeSignature(): abstract API method called!");
}

## $bool = $doc->saveSignature($sigFile,%saveopts)
##  + saves document type signature to a (new) file
##  + caches signature filename $doc->{sigFile}=$sigFile
##  + caches document signature $doc->{sig}
sub saveSignature {
  my ($doc,$sigFile,%opts) = @_;
  my $sig = $doc->typeSignature();
  $doc->{sigFile} = $sigFile;
  return $sig->saveFile($sigFile,%opts);
}


##--------------------------------------------------------------
## Methods: Parsing: Raw Text

## \$str = $doc->rawText(%opts)
## + %opts:
##     str    => \$str,    ##-- write to \$str
##     xpaths => \@xpaths, ##-- override @XML_RAW_XPATHS (VzXml only)
## + sets $str=$rawDocText and returns \$str
## + default checks for raw data in $doc->{raw} or existing file $doc->{rawFile}, otherwise calls $doc->getRawText()
sub rawText {
  my ($doc,%opts) = @_;

  ##-- defaults
  my ($ref);
  if (defined($doc->{raw})) {
    ##-- cached raw text data
    $ref = \$doc->{raw};
  }
  elsif (defined($doc->{rawFile}) && -r $doc->{rawFile}) {
    ##-- cached raw text file
    local $/ = undef;
    open(my $fh,"<:utf8",$doc->{rawFile})
      or die(ref($doc)."::rawText(): open failed for $doc->{rawFile}: $!");
    my $buf = <$fh>;
    close $fh;
    $ref = \$buf;
  }
  else {
    $ref = $doc->getRawText(%opts);
  }

  ##-- set & return
  if (defined($opts{str})) {
    ${$opts{str}} = $$ref;
    return $opts{str};
  }
  return $ref;
}

## \$str = $doc->getRawText(%opts)
## + %opts:
##     xpaths => \@xpaths, ##-- override @XML_RAW_XPATHS (VzXml only)
## + guts for rawText() method
## + REQUIRED for subclasses
sub getRawText {
  my ($doc,%opts) = @_;
  confess(ref($doc)."::getRawText(): abstract API method called!");
}


##==============================================================================
## Methods: I/O

##--------------------------------------------------------------
## Methods: I/O: XML: generic

## $mode_hash_or_name = $CLASS_OR_OBJ->defaultIoMode()
##  + returns default I/O mode for object
##  + override returns 'bin'
sub defaultIoMode { return 'bin'; }

##--------------------------------------------------------------
## Methods: I/O: XML
## (nothing here)

##--------------------------------------------------------------
## Methods: I/O: Binary

## ($serialized,$ref1,...) = STORABLE_freeze($obj,$is_cloning)
sub STORABLE_freeze {
  my ($obj,$is_cloning) = @_;
  my $fobj = {%$obj,xdoc=>undef};
  return ('', $fobj, ($is_cloning ? \$obj->{xdoc} : qw()));
}


## undef = STORABLE_thaw($obj,$is_cloning,$serialized,$ref1,...)
sub STORABLE_thaw {
  my ($obj,$is_cloning,$ser,$fobj,$xdocr) = @_;
  if (!UNIVERSAL::isa($fobj,'HASH')) {
    %$obj = %{ref($obj)->new(label=>$ser)}; ##-- bug in 1st implementation: workaround here returns empty doc for debugging purposes
  } else {
    %$obj = (%$fobj, ($is_cloning ? (xdoc=>$$xdocr) : qw()));
  }
  return;
}



##==============================================================================
## Footer
1;

__END__
