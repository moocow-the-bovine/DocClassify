## -*- Mode: CPerl -*-
## File: DocClassify::Document.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document


package DocClassify::Document;
use DocClassify::Object;
use DocClassify::Utils ':all';
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Object);

## @XML_CAT_XPATHS
## for xml input
our @XML_CAT_XPATHS =
  (
   '//head/classification/cat',
   '//head/classification/vat',
  );

## @XML_TERM_XPATHS
## for xml input
our @XML_TERM_XPATHS =
  (
   '//cooked//w[@stop!="1"]',
  );

## @XML_RAW_XPATHS
##  for raw output
our @XML_RAW_XPATHS =
  (
   '//head/title',
   '//head/descrition',
   '//head/description',
   '//body/thread/title',
   '//body/thread/posts/post/plain',
  );

##==============================================================================
## Constructors etc.

## $obj = $CLASS_OR_OBJ->new(%opts)
## %$obj, %opts:
##  ##-- base data
##  tf   => \%term2freq,  ##-- raw frequency hash
##  name => $srcName,     ##-- source name (default=undef)
##  N    => $totalFreq,   ##-- total frequency
##  ##
##  ##-- optional data
##  cat2deg => \%cat2deg, ##-- $categoryName => $degree (lower degree is better "fit")
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   tf=>{},
			   N=>0,
			   name=>undef,
			   cat2deg=>{},
			   @_,
			  );
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(tf N cat2seg)
sub noShadowKeys {
  return qw(tf N cat2deg);
}

##==============================================================================
## Methods: Agglomeration

## $doc = $doc->addDoc($doc2)
##  adds frequency and category data from $doc2
sub addDoc {
  my ($doc1,$doc2) = @_;

  ##-- add: frequencies
  $doc1->{N} += $doc2->{N};
  my ($t,$f);
  while (($t,$f)=each(%{$doc2->{tf}})) {

    $doc1->{tf}{$t} += $f;
  }

  ##-- add: category data
  my ($c,$d);
  while (($c,$d)=each(%{$doc2->{cat2deg}})) {
    $doc1->{cat2deg}{$c} = min2( ($doc1->{cat2deg}{$c}||9999), $d );
  }

  ##-- return
  return $doc1;
}

##==============================================================================
## Methods: I/O


##==============================================================================
## Methods: I/O: CSV

## $doc = $doc->loadCsvFile($file_or_fh,%opts)
## %opts, %$doc keys:
sub loadCsvFile {
  my ($doc,$file,%opts) = @_;

  ##-- set document name
  $doc->{name} = $file;

  my $fh = ref($file) ? $file : IO::File->new("<$file");
  confess(ref($doc)."::loadCsvFile(): open failed for file '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8');

  ##-- parse rest of file (TAB-separated: (TERM FREQ ...) or tab-less (CAT_ID? DEG? CAT_NAME) or comment /^%%/)
  my $tf = $doc->{tf};
  my ($line,$cat,$deg, $term,$freq,$rest);
  while (defined($line=<$fh>)) {
    chomp;
    next if ($line =~ /^\s*$/ || $line =~ /^%%/);

    if ($line !~ /\t/) {
      ##-- category declaration: CLASS_ID? DEG? CLASS_NAME
      $line =~ s/^\d+\s*//;
      if ($line =~ /^(\d+)\s*(.*)$/) {
	($deg,$cat) = ($1,$2);
      } else {
	($deg,$cat) = (1,$line);
      }
      $doc->{cat2deg}{$cat}=$deg;
      next;
    }

    ##-- TERM FREQ ...
    ($term,$freq,$rest) = split(/\t/,$line,3);
    $tf->{$term} += $freq;
    $N += $freq;
  }
  $fh->close if (!ref($file));
}

##==============================================================================
## Methods: I/O: XML

## $doc = $doc->loadXmlFile($file_or_fh,%opts)
## %opts, %$doc keys:
##  posRegex => $regex,           ##-- default: /^N/
##  textStopList => \%text2undef, ##-- text stop-list, default: empty
##  textRegex => $regex,          ##-- text regex, default: none (keep all)
##  textAttr  => $attr,           ##-- default: 'norm'
##  lemmaAttr => $attr,           ##-- default: 'lemma'
sub loadXmlFile {
  my ($doc,$file,%opts) = @_;

  ##-- defaults
  $opts{posRegex} = qr/^N/ if (!defined($opts{posRegex}));
  $opts{lemmaAttr} = 'lemma' if (!defined($opts{lemmaAttr}));
  $opts{textAttr} = 'norm' if (!defined($opts{textAttr}));

  ##-- doc name
  $doc->{name} = $file;
  my $parser = libxmlParser();
  my $doc = ref($file) ? $parser->parse_fh($file) : $parser->parse_file($file);
  confess(ref($doc)."::loadXmlFile(): parse failed for file '$file': $!") if (!$doc);

  ##-- category data
  my ($c_node,$c_name,$c_deg);
  foreach $c_node (@{$doc->findnodes(join('|',@XML_CAT_XPATHS))}) {
    ($c_name,$c_deg) = map {$c_node->getAttribute($_)} qw(id name degree);
    $doc->{cat2seg}{$c_name} = $c_deg;
  }

  ##-- term frequency
  my $tf = $doc->{tf};
  my $N  = $doc->{N};
  my ($t_node,$t_text,$t_lemma);
  foreach $t_node (@{$doc->findnodes(join('|',@XML_TERM_XPATHS))}) {
    $t_text = $t_node->getAttribute($opts{textAttr});
    next if (defined($opts{textStopList}) && exists($opts{textStopList}{$t_text}));
    next if (defined($opts{textRegex}) && $t_text !~ $opts{textRegex});

    $t_pos = $t_node->getAttribute('pos');
    next if (defined($opts{posRegex}) && $t_pos !~ $opts{posRegex});

    $t_lemma = $t_node->getAttribute($opts{lemmaAttr});
    $tf->{$t_lemma}++;
    $N++;
  }
  $doc->{N}=$N;
  $fh->close() if (!ref($file));

  return $doc;
}



##==============================================================================
## Footer
1;

__END__
