## -*- Mode: CPerl -*-
## File: DocClassify::Document::VzXml.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: document (VZ-XML)

package DocClassify::Document::VzXml;
use DocClassify::Document;
use DocClassify::Utils ':all';
use DocClassify::Signature;
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Document);

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

##==============================================================================
## Constructors etc.

## $doc = $CLASS_OR_OBJ->new(%opts)
## %$doc, %opts:
##  ##-- NEW in DocClassify::Document::VzXml
##  xdoc => $xmlDoc,      ##-- XML::LibXML::Document object (default: none)
##  ##
##  ##-- INHERITED from DocClassify::Document: source options
##  file => $srcFile,     ##-- load from file (sets label=$srcFile)
##  string => $srcString, ##-- load from string, (sets label='string')
##  label => $srcLabel,   ##-- override initial document label
##  id => $id,            ##-- unique id (string or integer; default = ++$DOC_ID)
##  ##
##  ##-- INHERITED from DocClassify::Document: other data
##  xdoc => $xmlDoc,      ##-- XML::LibXML::Document object (default: none)
##  cats => \@catList,    ##-- [ {id=>$catId,deg=>$catDeg,name=>$catName}, ... ] : see cats()
##  sig => $termSignature,##-- cached signature
##  sigFile => $sigFile,  ##-- cached signature file
##  raw => $rawText,      ##-- cached raw text
##  rawFile => $rawFile,  ##-- cached raw text file
sub new {
  my $that = shift;
  return $that->SUPER::new(@_);
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(xdoc id cats sig raw sigFile rawFile)
sub noShadowKeys {
  my $that = shift;
  return ($that->SUPER::noShadowKeys(), qw(xdoc));
}

## $doc = $doc->clearCache()
##  + clears cached keys qw(xdoc raw sig)
sub clearCache {
  my $doc = shift;
  $doc->SUPER::clearCache();
  delete @{$doc}{qw(xdoc)};
  return $doc;
}

##==============================================================================
## Methods: labelling

## $label = $doc->label()
## $label = $doc->label($label)
##  + get label, setting it if not already defined
##  + INHERITED from DocClassify::Document

## $id = $doc->id()
## $id = $doc->id($id)
##  + get id, setting if not already defined
##  + INHERITED from DocClassify::Document

##==============================================================================
## Methods: Parsing

##--------------------------------------------------------------
## Methods: Parsing: Header Data

## \@cats = $doc->getCats()
##  + Returns array-ref of category membership data:
##    @cats = ( {id=>$catId,deg=>$catDeg,name=>$catName}, ... )
##  + REQUIRED by subclasses
sub getCats {
  my $doc = shift;

  ##-- parse category data
  my $cats = [];
  my $xdoc = $doc->xmlDoc();
  my ($c_node,$c_id,$c_name,$c_deg);
  foreach $c_node (@{$xdoc->findnodes(join('|',@XML_CAT_XPATHS))}) {
    ($c_id,$c_name,$c_deg) = map {$c_node->getAttribute($_)} qw(id name degree);
    push(@$cats, {id=>$c_id,deg=>$c_deg,name=>$c_name});
  }

  return $cats;
}

##--------------------------------------------------------------
## Methods: Parsing: Type-Frequency Signature (NEW)

## $sig = $doc->getTypeSignature(%opts)
##  + %opts: (none yet)
##  + (re-)generate document signature; guts for typeSignature() method
##  + REQUIRED by subclasses
sub getTypeSignature {
  my ($doc,%opts) = @_;
  my $sig = DocClassify::Signature->new();

  ##-- get category data
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

## \$str = $doc->getRawText(%opts)
## + %opts:
##     xpaths => \@xpaths, ##-- override @XML_RAW_XPATHS (VzXml only)
## + guts for rawText() method
## + REQUIRED for subclasses
sub rawText {
  my ($doc,%opts) = @_;

  ##-- compute raw text
  %opts = (%$doc,%opts);
  $opts{xpaths} = \@XML_RAW_XPATHS if (!defined($opts{xpaths}));
  my $xdoc = $doc->xmlDoc();
  my $buf = join("\n\n", map {$_->textContent} @{$xdoc->findnodes(join('|',@{$opts{xpaths}}))})."\n";

  return \$buf;
}



##==============================================================================
## Methods: I/O

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

##==============================================================================
## Footer
1;

__END__
