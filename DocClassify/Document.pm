## -*- Mode: CPerl -*-
## File: DocClassify::Document.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document (raw xml data)


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

## @XML_CAT_XPATHS
##  + XPaths for categorization
our @XML_CAT_XPATHS =
  (
   '/*/head/classification/cat',
   '/*/head/classification/vat',
  );

## @XML_RAW_XPATHS
##  + XPaths for raw text extraction
our @XML_RAW_XPATHS =
  (
   '/*/head/title',
   '/*/head/descrition',
   '/*/head/description',
   '/*/body/thread/title',
   '/*/body/thread/posts/post/plain',
  );

## $DOC_ID
##  + used for auto-generating doc-ids
##  + currently useless (just a dumb counter, no collision checking, etc.)
our $DOC_ID = 0;

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
##  xdoc => $xmlDoc,      ##-- XML::LibXML::Document object (default: none)
##  cats => \@catList,    ##-- [ {id=>$catId,deg=>$catDeg,name=>$catName}, ... ] : see cats()
##  sig => $termSignature,##-- cached signature
##  sigFile => $sigFile,  ##-- cached signature file
##  raw => $rawText,      ##-- cached raw text
##  rawFile => $rawFile,  ##-- cached raw text file
sub new {
  my $that = shift;
  my $doc = $that->SUPER::new(
			      ##-- source options
			      label=>undef,
			      xdoc =>undef,
			      id   =>undef,
			      @_,
			     );

  ##-- ensure label, id are set
  $doc->label() if (!defined($doc->{label}));
  $doc->id() if (!defined($doc->{id}));

  return $doc;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(xdoc id cats sig raw sigFile rawFile)
sub noShadowKeys {
  return qw(xdoc id cats sig raw sigFile rawFile);
}

## $doc = $doc->clearCache()
##  + clears cached keys qw(xdoc raw sig)
sub clearCache {
  delete(@{$_[0]}{qw(xdoc raw sig)});
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
  } elsif (defined($doc->{file})) {
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
##  + just returns $doc->{cats} if defined, otherwise parses xml doc
sub cats {
  my $doc = shift;
  if (!defined($doc->{cats})) {
    ##-- parse category data
    $doc->{cats} = [];
    my $xdoc = $doc->xmlDoc();
    my ($c_node,$c_id,$c_name,$c_deg);
    foreach $c_node (@{$xdoc->findnodes(join('|',@XML_CAT_XPATHS))}) {
      ($c_id,$c_name,$c_deg) = map {$c_node->getAttribute($_)} qw(id name degree);
      push(@{$doc->{cats}}, {id=>$c_id,deg=>$c_deg,name=>$c_name});
    }
  }
  ##-- sanitize & sort
  $_->{deg} = 1 foreach (grep {!defined($_->{deg})} @{$doc->{cats}});
  @{$doc->{cats}} =
    sort {($a->{deg}<=>$b->{deg}
	   || ($b->{sim}||0) <=> ($a->{sim}||0)
	   || ($a->{dist}||0) <=> ($b->{dist}||0)
	   || ($a->{dist_raw}||0) <=> ($b->{dist_raw}||0)
	   || $a->{id}<=>$b->{id}
	   || $a->{name} cmp $b->{name})
	 } @{$doc->{cats}};

  return wantarray ? @{$doc->{cats}} : $doc->{cats};
}

##--------------------------------------------------------------
## Methods: Parsing: Type-Frequency Signature (NEW)

## $sig = $doc->typeSignature(%opts)
##  + %opts: (none yet)
sub typeSignature {
  my ($doc,%opts) = @_;
  return $doc->{sig} if (defined($doc->{sig}));            ##-- check for cached object
  return DocClassify::Signature->loadFile($doc->{sigFile}) ##-- check for cached file
    if ($doc->{sigFile} && -r ($doc->{sigFile}));

  ##-- common vars
  my $sig = DocClassify::Signature->new();

  ##-- category data
  my ($cat);
  foreach $cat ( @{$doc->cats} ) {
    $sig->{cat2id}{$cat->{name}}  = $cat->{id} if (!defined($sig->{cat2id}{$cat->{name}}));
    $sig->{cat2deg}{$cat->{name}} = $cat->{deg} if (!defined($sig->{cat2deg}{$cat->{name}}));
  }

  ##-- get type-frequency signature (using XSL)
  my $xdoc = $doc->xmlDoc();
  my $yf_stylesheet = $doc->typeFrequencyStylesheet();
  my $xxdoc = $yf_stylesheet->transform($xdoc);
  my $xxstr = $yf_stylesheet->output_string($xxdoc);

  my $tf = $sig->{tf};
  my $N  = $sig->{N};
  foreach ($xxstr =~ /^(?!%%).+$/mg) {
    $tf->{$_}++;
    $N++;
  }
  $sig->{N}=$N;

  return $doc->{sig}=$sig;
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

## $stylesheet = $CLASS_OR_OBJ->typeFrequencyStylesheet()
sub typeFrequencyStylesheet {
  our ($YF_STYLESHEET,$YF_STYLESTR);
  return $YF_STYLESHEET if (defined($YF_STYLESHEET));
  return $YF_STYLESHEET = xsl_stylesheet(string=>$YF_STYLESTR);
}

## $YF_STYLESHEET
##  + XSL stylesheet for type-frequency counting (xml -> utf-8 text)
our ($YF_STYLESHEET);

## $YF_STYLESTR
##  + XSL stylesheet string for type-frequency counting (xml -> utf-8 text)
our $YF_STYLESTR = q(<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- parameters: (none) -->

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- options: (none) -->

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: root: recurse -->
  <xsl:template match="/">
   <xsl:apply-templates select="./*"/>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: words: output TT-like format -->
  <xsl:template match="w">
    <xsl:apply-templates select="@*"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: word-attribute: ATTR1=VAL1 "\t" ATTR2=VAL2 ... -->
  <xsl:template match="w/@*">
    <xsl:if test="position()>1">
      <xsl:text>&#09;</xsl:text>
    </xsl:if>
    <xsl:value-of select="name()"/>
    <xsl:text>=</xsl:text>
    <xsl:value-of select="normalize-space(.)"/>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: cooked: output comment -->
  <xsl:template match="//cooked">
    <xsl:text>&#10;</xsl:text>
    <xsl:text>%% COOKED type=</xsl:text>
    <xsl:value-of select="./@type"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates select="./*"/>
  </xsl:template>


  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: sentence: output comment -->
  <xsl:template match="s">
    <xsl:text>&#10;</xsl:text>
    <!-- <xsl:text>%% SENTENCE&#10;</xsl:text> -->
    <xsl:apply-templates select="./*"/>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- Templates: default: ignore -->
  <xsl:template match="*|@*|text()|comment()|processing-instruction()" priority="-1">
    <xsl:apply-templates select="./*"/>
  </xsl:template>

</xsl:stylesheet>
);

##--------------------------------------------------------------
## Methods: Parsing: Raw Text

## \$str = $doc->rawText(%opts)
## + %opts:
##     str=>\$str,
##     xpaths => \@xpaths, ##-- override @XML_RAW_XPATHS
sub rawText {
  my ($doc,%opts) = @_;

  ##-- defaults
  my $ref = \$doc->{raw};
  if (!defined($$ref)) {
    ##-- compute raw text
    %opts = (%$doc,%opts);
    $opts{xpaths} = \@XML_RAW_XPATHS if (!defined($opts{xpaths}));
    my $xdoc = $doc->xmlDoc();
    $$ref = join("\n\n", map {$_->textContent} @{$xdoc->findnodes(join('|',@{$opts{xpaths}}))})."\n";
  }

  ##-- return
  if (defined($opts{str})) {
    ${$opts{str}} = $$ref;
    return $opts{str};
  }
  return $ref;
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

## $xdoc = $doc->xmlDoc(%opts)
##  + gets cached xml document or load from source
sub xmlDoc {
  return $_[0]{xdoc} if (defined($_[0]{xdoc}));
  return $_[0]->loadXmlDoc(@_[1..$#_])
}


## $xdoc = $doc->loadXmlDoc(%opts)
##  + (re-)loads $doc->{xdoc} from specified source
##  + %opts clobbers %$doc
##  + implicitly re-sets $doc->{label} from %opts or source
sub loadXmlDoc {
  my ($doc,%opts) = shift;
  delete($doc->{label});
  @$doc{keys(%opts)} = values(%opts);
  $doc->label() if (!defined($doc->{label})); ##-- re-set label

  my $parser = libxmlParser();
  if (defined($doc->{doc})) {
    $doc->{xdoc} = $doc->{doc};
    delete($doc->{doc});
  }
  elsif (defined($doc->{string})) {
    $doc->{xdoc} = $parser->parse_string(ref($doc->{string}) ? ${$doc->{string}} : $doc->{string});
  }
  elsif (defined($doc->{file})) {
    $doc->{xdoc} = ref($doc->{file}) ? $parser->parse_fh($doc->{file}) : $parser->parse_file($doc->{file});
  }
  else {
    confess(ref($doc)."::loadXmlDoc(): no XML document source to load from!");
  }
  confess(ref($doc)."::loadXmlDoc(): ".$doc->label.": could not parse source document: $!") if (!$doc->{xdoc});

  return $doc->{xdoc};
}

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
