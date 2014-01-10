## -*- Mode: CPerl -*-
## File: DocClassify::Document::CabCsv1g.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description: document classifier: document (DTA::CAB CSV via tt-1grams.perl)
##  + document may contain special comments:
##     %%$dc.cat=CATNAME
##     %%$dc.cat=CATID_CATNAME
##     %%$dc.cat.DEGREE=CATNAME
##     %%$dc.cat.DEGREE=CATID_CATNAME

package DocClassify::Document::CabCsv1g;
use DocClassify::Document;
use DocClassify::Utils ':all';
use DocClassify::Signature;
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Document);

## $CSV_ATTR_NAMES
##  + attribute names for loadCsvFile(), space or attribute-separated
our $CSV_ATTR_NAMES = q(text xlit norm pos lemma);

##==============================================================================
## Constructors etc.

## $doc = $CLASS_OR_OBJ->new(%opts)
## %$doc, %opts:
##  ##-- NEW in DocClassify::Document::CabCsv1g
##  csvAttrNames => $names, ##-- space or comma-separatated attribute names (default=$CSV_ATTR_NAMES)
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
  return $that->SUPER::new(
			   csvAttrNames => $CSV_ATTR_NAMES,
			   @_
			  );
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
## Methods: Parsing: Local

## $doc = $doc->parseSource(%opts)
##  + %opts: overrides %$doc (e.g. file=>FILE)
##  + parses source directly into $doc->{sig}, $doc->{cats}
sub parseSource {
  my ($doc,%opts) = @_;
  @$doc{keys %opts} = values %opts;

  ##-- open source
  my ($fh);
  if (ref($doc->{file})) {
    $fh = $doc->{file};
  } elsif ($doc->{file}) {
    open($fh,"<:utf8",$doc->{file})
      or confess(ref($doc)."::parseSource(): open failed for file $doc->{file}: $!");
  } elsif (defined($doc->{string})) {
    open($fh,"<:utf8",\$doc->{string})
      or confess(ref($doc)."::parseSource(): open failed for string buffer: $!");
  } else {
    confess(ref($doc)."::parseSource(): no source specified!");
  }

  ##-- get attribute names
  my @names = grep {($_//'') ne ''} split(/[\s\,]+/, $doc->{csvAttrNames}//$CSV_ATTR_NAMES);

  ##-- chug doc, building sig term frequencies
  my $cats = $doc->{cats} = [];
  my $sig  = $doc->{sig}  = DocClassify::Signature->new();
  my $tf   = $sig->{tf};
  my $N    = 0;
  my ($f,@vals);
  while (defined($_=<$fh>)) {
    chomp;
    if (/^%%\$dc\.cat(?:\.([0-9\.\-\+eE]+))?=(?:([0-9]+)_)?(.*)$/) {
      ##-- parse category
      push(@$cats, {id=>($1//1), deg=>($2//1), name=>($2//'unknown')});
    }
    elsif (/^%%/ || /^\s*$/) {
      next;
    }

    ##-- parse csv lines
    ($f,@vals) = split(/\t/, $_, scalar(@names)+1);
    $f //= 1;
    $tf->{join("\t", map {"$names[$_]=$vals[$_]"} (0..$#vals))} += $f;
    $N += $f;
  }
  $sig->{N} = $N;

  ##-- ensure sig cats
  $sig->addCat($_) foreach (@{$doc->cats});

  ##-- cleanup
  close($fh) if (!ref($doc->{file}));
  return $doc;
}

##--------------------------------------------------------------
## Methods: Parsing: Header Data

## \@cats = $doc->getCats(%opts)
##  + Returns array-ref of category membership data:
##    @cats = ( {id=>$catId,deg=>$catDeg,name=>$catName}, ... )
##  + REQUIRED by subclasses
sub getCats {
  my $doc = shift;
  $doc->parseSource(@_);

  ##-- insert a dummy category if none were defined
  if (!$doc->{cats} || !@{$doc->{cats}}) {
    my $cat = {id=>1, deg=>1, name=>'dummy'};
    push(@{$doc->{cats}}, $cat);
    $doc->{sig}->addCat($cat);
  }

  return $doc->{cats};
}

##--------------------------------------------------------------
## Methods: Parsing: Type-Frequency Signature (NEW)

## $sig = $doc->getTypeSignature(%opts)
##  + %opts: (none yet)
##  + (re-)generate document signature; guts for typeSignature() method
##  + REQUIRED by subclasses
sub getTypeSignature {
  my ($doc,%opts) = @_;
  $doc->parseSource(%opts);
  return $doc->{sig};
}

##--------------------------------------------------------------
## Methods: Parsing: Raw Text

## \$str = $doc->getRawText(%opts)
## + %opts:
##     xpaths => \@xpaths, ##-- override @XML_RAW_XPATHS (VzXml only)
## + guts for rawText() method
## + REQUIRED for subclasses
sub rawText {
  my ($doc,%opts) = @_;
  return slurpFile($doc->{file},undef,':utf8') if (defined($doc->{file}));
  return \$doc->{string};
  confess(ref($doc)."::rawText(): no source specified!");
}

##==============================================================================
## Footer
1;

__END__
