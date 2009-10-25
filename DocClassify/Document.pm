## -*- Mode: CPerl -*-
## File: DocClassify::Document.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document (raw xml data)


package DocClassify::Document;
use DocClassify::Object;
use DocClassify::Utils ':all';
use DocClassify::Signature;
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Object);

## @XML_CAT_XPATHS
##  + XPaths for categorization
our @XML_CAT_XPATHS =
  (
   '/*/head/classification/cat',
   '/*/head/classification/vat',
  );

## @XML_TERM_XPATHS
##  + XPaths for term signatures
our @XML_TERM_XPATHS =
  (
   #'//cooked/s/w[@stop!="1"]',
   '//w[@stop!="1"]',
  );

## $XML_POS_REGEX
##  + regex matching PoS of terms to select
our $XML_POS_REGEX = qr/^N/;

## $XML_TEXT_REGEX
##  + regex matching text of terms to select
our $XML_TEXT_REGEX = qr/./;

## $XML_TEXT_ATTR
##  + plain (normalized) text attribute
our $XML_TEXT_ATTR = 'norm';

## $XML_LEMMA_ATTR
##  + lemma attribute
our $XML_LEMMA_ATTR = 'lemma';


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
##  raw => $rawText,      ##-- cached raw text
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
##  + override returns qw(xdoc id cats sig raw)
sub noShadowKeys {
  return qw(xdoc id cats sig raw);
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
##  + calls $doc->termSignature()
sub sizeTerms {
  return $_[0]->termSignature->{N};
}

## $nTokens = $doc->sizeTokens()
##  + slow
sub sizeTokens {
  my $doc = shift;
  return scalar(@{$doc->xmlDoc->findnodes(join('|',@XML_TERM_XPATHS))});
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
    sort {$a->{deg}<=>$b->{deg} || $a->{id}<=>$b->{id} || $a->{name} cmp $b->{name}} @{$doc->{cats}};

  return wantarray ? @{$doc->{cats}} : $doc->{cats};
}

##--------------------------------------------------------------
## Methods: Parsing: Term-Frequency Signature

## \%tfhash = $doc->termSignature(%opts)
##  + %opts:
##     textStopList => \%stopword2undef, ##-- stop-list
##     textAttr => $a,                   ##-- default=$XML_TEXT_ATTR
##     textRegex => $re,                 ##-- default=$XML_TEXT_REGEX
##     posRegex => $re,                  ##-- default=$XML_POS_REGEX
##     lemmaAttr => $a,                  ##-- default=$XML_LEMMA_ATTR
sub termSignature {
  my ($doc,%opts) = @_;
  return $doc->{sig} if (defined($doc->{sig})); ##-- check cache
  %opts = (%$doc,%opts);

  ##-- defaults
  $opts{textAttr} = $XML_TEXT_ATTR if (!defined($opts{textAttr}));
  $opts{textRegex} = $XML_TEXT_REGEX if (!defined($opts{textRegex}));
  $opts{posRegex}  = $XML_POS_REGEX if (!defined($opts{posRegex}));
  $opts{lemmaAttr} = $XML_LEMMA_ATTR if (!defined($opts{lemmaAttr}));

  ##-- common vars
  my $sig = DocClassify::Signature->new();

  ##-- category data
  my ($cat);
  foreach $cat ( @{$doc->cats} ) {
    $sig->{cat2id}{$cat->{name}}  = $cat->{id} if (!defined($sig->{cat2id}{$cat->{name}}));
    $sig->{cat2deg}{$cat->{name}} = $cat->{deg} if (!defined($sig->{cat2deg}{$cat->{name}}));
  }

  ##-- term frequency hash
  my $xdoc = $doc->xmlDoc();
  my $tf = $sig->{tf};
  my $N  = $sig->{N};
  my ($t_node,$t_text,$t_pos,$t_lemma);
  foreach $t_node (@{$xdoc->findnodes(join('|',@XML_TERM_XPATHS))}) {
    $t_text = $t_node->getAttribute($opts{textAttr});
    next if (defined($opts{textStopList}) && exists($opts{textStopList}{$t_text}));
    next if (defined($opts{textRegex}) && $t_text !~ $opts{textRegex});

    $t_pos = $t_node->getAttribute('pos');
    next if (defined($opts{posRegex}) && $t_pos !~ $opts{posRegex});

    $t_lemma = $t_node->getAttribute($opts{lemmaAttr});
    $tf->{$t_lemma}++;
    $N++;
  }
  $sig->{N}=$N;

  return $doc->{sig}=$sig;
}

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
