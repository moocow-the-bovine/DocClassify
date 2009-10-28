## -*- Mode: CPerl -*-
## File: DocClassify::Corpus.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: corpus (set of documents)


package DocClassify::Corpus;
use DocClassify::Object;
use DocClassify::Utils ':all';
use DocClassify::Document;

use PDL;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Object);

##==============================================================================
## Constructors etc.

## $corpus = $CLASS_OR_OBJ->new(%opts)
## %$corpus, %opts:
##  ##
##  ##-- base data
##  label => $label,      ##-- optional label (root attribute; default='')
##  docs => \@docs,       ##-- array of member documents
##  #cenum => $cenum,      ##-- category enum
##  #...
sub new {
  my $that = shift;
  my $corpus = $that->SUPER::new(
				 ##-- source options
				 docs =>[],
				 label=>'',
				 @_,
				);

  return $corpus;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(docs)
sub noShadowKeys {
  return qw(docs);
}

## $corpus = $corpus->clear()
##  + clears corpus data
sub clear {
  my $corpus = shift;
  @{$corpus->{docs}} = qw();
  $corpus->{label} = '';
  return $corpus;
}

##==============================================================================
## Methods: Basic

## $corpus = $corpus->addDocuments(@docs)
BEGIN { *addDocument = *addDocs = *addDoc = \&addDocuments; }
sub addDocuments {
  my $corpus = shift;
  push(@{$corpus->{docs}},@_);
  return $corpus;
}

## $corpus = $corpus->addCorpus($corpus2)
sub addCorpus {
  my $corpus = shift;
  push(@{$corpus->{docs}}, @{$_->{docs}}) foreach (@_);
  return $corpus;
}


##==============================================================================
## Methods: Splitting

## (@corpora) = $corpus->splitN($n,%opts)
##  + $N: number of (uniformly sized) sub-corpora into which to split $corpus
##  + %opts:
##      seed => $randomSeed,  ##-- for reproducability
##      exclusive => $bool,   ##-- passed to docsByCat()
##      label => $fmt,        ##-- sprintf-format for new labels (passed $i, default="$corpus->{label}.%0.2d")
sub splitN {
  my ($corpus,$N,%opts) = @_;

  ##-- create sub-corpora
  $opts{label} = "$corpus->{label}.%0.2d" if (!defined($opts{label}));
  my @corpora = map {$corpus->shadow(label=>sprintf($opts{label},$_))} (1..$N);

  ##-- proceed by category: build cat2doc hash
  my $cat2doc = $corpus->docsByCat(%opts);
  my ($cat,$cdocs,@ckeys, $scsizes,$sci, $doc);
  while (($cat,$cdocs)=each(%$cat2doc)) {
    ##-- randomize doc-list
    srand($opts{seed}) if (defined($opts{seed})); ##-- random seed (because hash order may change)
    @ckeys = map {rand} @$cdocs;
    @$cdocs = @$cdocs[sort {$ckeys[$a]<=>$ckeys[$b]} (0..$#$cdocs)];

    ##-- assign category documents to sub-corpora
    $scsizes = zeroes(long,$N);
    foreach $doc (@$cdocs) {
      $sci = $scsizes->minimum_ind->sclr;
      push(@{$corpora[$sci]{docs}},$doc);
      $scsizes->slice("($sci)") += $doc->sizeBytes;
    }
  }

  #@corpora = @corpora[random($N)->qsorti->list]; ##-- extra random permutation step
  return @corpora;
}


## \%cat2doc = $corpus->docsByCat(%opts)
##  + returns map ($catName=>\@docs)
##  + %opts:
##     exclusive => $bool, ##-- assign each doc to at most 1 cat (default=1)
BEGIN { *cat2doc = \&docsByCat; }
sub docsByCat {
  my ($corpus,%opts) = @_;
  my $excl = defined($opts{exclusive}) ? $opts{exclusive} : 1;

  my $c2d = {};
  my ($doc,$dcats);
  foreach $doc (@{$corpus->{docs}}) {
    $dcats = $doc->cats();
    next if (!$dcats || !@$dcats); ##-- no category data available
    if ($excl) {
      push(@{$c2d->{$dcats->[0]{name}}},$doc);
    } else {
      push(@{$c2d->{$_->{name}}},$doc) foreach (@$dcats);
    }
  }

  return $c2d;
}

##==============================================================================
## Methods: I/O

##--------------------------------------------------------------
## Methods: I/O: XML: generic

## $mode_hash_or_name = $CLASS_OR_OBJ->defaultIoMode()
##  + returns default I/O mode for object
##  + override returns 'xml'
sub defaultIoMode { return 'xml'; }

##--------------------------------------------------------------
## Methods: I/O: XML: save

## $xdoc = $corpus->saveXmlDoc(%opts)
##  + creates xml document from $corpus
##  + %opts:
##     saveCats => $bool,  ##-- whether to save category data (default=true)
sub saveXmlDoc {
  my ($corpus,%opts) = @_;
  my $xdoc = XML::LibXML::Document->new('1.0','UTF-8');
  $xdoc->setDocumentElement($xdoc->createElement('corpus'));
  my $root = $xdoc->documentElement;
  $root->setAttribute('label',($corpus->{label}||''));

  ##-- raw document list
  my $docs_node = $root->addNewChild(undef,'docs');
  my ($doc,$d_node, $cats,$cat,$c_node);
  foreach $doc (@{$corpus->{docs}}) {
    $d_node = $docs_node->addNewChild(undef,'doc');
    $d_node->setAttribute('id',$doc->id);
    $d_node->setAttribute('label',$doc->label) if (defined($doc->{file}) && $doc->label ne $doc->{file});
    $d_node->setAttribute('file',$doc->{file}) if (defined($doc->{file}));
    $d_node->setAttribute('bytes',$doc->sizeBytes); ##-- save XML document size in bytes (e.g. for splitN())
    $d_node->setAttribute('sigFile',$doc->{sigFile}) if (defined($doc->{sigFile}));
    if ($opts{saveCats} || !defined($opts{saveCats})) {
      foreach $cat (@{$doc->cats}) {
	$c_node = $d_node->addNewChild(undef,'cat');
	$c_node->setAttribute($_,(defined($cat->{$_}) ? $cat->{$_} : '')) foreach (sort(keys(%$cat)));
      }
    }
  }

  ##-- done
  return $xdoc;
}

## $bool = $corpus->saveXmlFile($filename_or_fh,%opts)
##  + save to XML file
##  + %opts:
##     format=>$level,  ##-- default=1
sub saveXmlFile {
  my ($corpus,$file,%opts) = @_;
  my $xdoc = $corpus->saveXmlDoc(%opts);
  my $fmt = defined($opts{format}) ? $opts{format} : 1;
  my $rc = ref($file) ? $xdoc->toFH($file,$fmt) : $xdoc->toFile($file,$fmt);
  return $rc;
}

## $str = $corpus->saveXmlString(%opts)
##  + save to XML string
##  + %opts:
##     format=>$level,  ##-- default=1
sub saveXmlString {
  my ($corpus,%opts) = @_;
  my $xdoc = $corpus->saveXmlDoc(%opts);
  return $xdoc->toString(defined($opts{format}) ? $opts{format} : 1);
}

##--------------------------------------------------------------
## Methods: I/O: XML: load

## $corpus = $CLASS_OR_OBJECT->loadXmlDoc($xdoc,%opts)
##  + (re-)loads corpus data from $xdoc
sub loadXmlDoc {
  my ($that,$xdoc,%opts) = @_;
  my $corpus = ref($that) ? $that->clear : $that->new(%opts);

  ##-- load: label
  my $root = $xdoc->documentElement;
  $corpus->{label} = $root->getAttribute('label');

  ##-- load: documents
  my ($d_node,$doc, $c_node,$cat);
  foreach $d_node (@{$root->findnodes('./docs/doc')}) {
    push(@{$corpus->{docs}}, $doc=DocClassify::Document->new());
    $doc->label($d_node->getAttribute('label'));
    $doc->id($d_node->getAttribute('id'));
    $doc->{file} = $d_node->getAttribute('file');
    $doc->{sigFile} = $d_node->getAttribute('sigFile');
    $doc->label();
    $doc->id();
    foreach $c_node (@{$d_node->findnodes('./cat')}) {
      $cat = {};
      $cat->{$_->name} = $_->value foreach ($c_node->attributes);
      push(@{$doc->{cats}},$cat);
    }
  }

  return $corpus;
}

## $bool = $CLASS_OR_OBJ->loadXmlFile($filename_or_fh,%opts)
##  + load from XML file
sub loadXmlFile {
  my ($that,$file,%opts) = @_;
  my $parser = libxmlParser();
  my $xdoc = ref($file) ? $parser->parse_fh($file) : $parser->parse_file($file)
    or confess((ref($that)||$that)."::loadXmlFile(): could not parse '$file': $!");
  return $that->loadXmlDoc($xdoc,%opts);
}

## $corpus = $CLASS_OR_OBJECT->loadXmlString($str,%opts)
## $corpus = $CLASS_OR_OBJECT->loadXmlString(\$str,%opts)
##  + load from XML string
sub loadXmlString {
  my ($that,$str,%opts) = @_;
  my $parser = libxmlParser();
  my $xdoc = $parser->parse_string(ref($str) ? $$str : $str)
    or confess((ref($that)||$that)."::loadXmlFile(): could not parse string: $!");
  return $that->loadXmlDoc($xdoc);
}



##==============================================================================
## Footer
1;

__END__
