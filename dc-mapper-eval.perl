#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;

#use PDL;
#use PDL::Ngrams;

use Getopt::Long qw(:config no_ignore_case);
use Encode qw(encode decode);
use File::Basename qw(basename);
use Pod::Usage;

#use strict;
BEGIN { select(STDERR); $|=1; select(STDOUT); }

##------------------------------------------------------------------------------
## Constants & Globals
##------------------------------------------------------------------------------
our $prog = basename($0);
our $verbose = 2;
our ($help);

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;

our %corpusopts = qw();
our %loadopts_corpus = ( mode=>undef, );

our $outfile = '-';

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- Mapping Options (none)

	   ##-- I/O
	   'corpus-input-mode|input-mode|cim|im=s' => \$loadopts_corpus{mode},
	   #'format-xml|format|fx|f!' => sub { $saveopts_corpus{format}=$_[1] ? 1 : 0; },
	   'output-file|outfile|out|of|o=s'=> \$outfile,
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);
pod2usage({-exitval=>0, -verbose=>0, -msg=>'You must specify at least the WANTED corpus!'}) if (!@ARGV);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

##-- subs: counting
sub true_positive {
  my ($cat,$doc) = @_;
  $c2e{$cat}{tp_docs}++;
  $c2e{$cat}{tp_bytes} += $doc->sizeBytes;
  $c2e{''}{tp_docs}++;
  $c2e{''}{tp_bytes} += $doc->sizeBytes;
  $Ndocs++;
  $Nbytes += $doc->sizeBytes;
}
sub false_negative {
  my ($cat,$doc) = @_;
  $c2e{$cat}{fn_docs}++;
  $c2e{$cat}{fn_bytes} += $doc->sizeBytes;
  $c2e{''}{fn_docs}++;
  $c2e{''}{fn_bytes} += $doc->sizeBytes;
  $Ndocs++;
  $Nbytes += $doc->sizeBytes;
}
sub false_positive {
  my ($cat,$doc) = @_;
  $c2e{$cat}{fp_docs}++;
  $c2e{$cat}{fp_bytes} += $doc->sizeBytes;
  $c2e{''}{fp_docs}++;
  $c2e{''}{fp_bytes} += $doc->sizeBytes;
  #$Ndocs++;
  #$Nbytes += $doc->sizeBytes;
}

## $frac = frac($num,$denom)
sub frac {
  my ($num,$denom) = @_;
  return $denom==0 ? 0 : ($num/$denom);
}

## $pr = precision(\%hash,$units)
sub precision {
  my ($hash,$unit) = @_;
  $unit = 'docs' if (!defined($unit));
  my $tp = $hash->{"tp_${unit}"} || 0;
  my $fp = $hash->{"fp_${unit}"} || 0;
  return frac($tp, ($tp+$fp));
}

## $rc = recall(\%hash,$units)
sub recall {
  my ($hash,$unit) = @_;
  $unit = 'docs' if (!defined($unit));
  my $tp = $hash->{"tp_${unit}"} || 0;
  my $fn = $hash->{"fn_${unit}"} || 0;
  return frac($tp, ($tp+$fn));
}

## $F = F(\%hash,$units)
sub F {
  my ($hash,$unit) = @_;
  my ($pr,$rc) = (precision(@_),recall(@_));
  return frac(2.0, ($pr**-1 + $rc**-1));
}

## ($pr,$rc,$F) = prF(\%hash,$unit)
sub prF {
  return (precision(@_),recall(@_),F(@_));
}

## $str = report($label,\%hash,$unit)
our $llen = 58;
our $flen = '6.2';
sub report {
  my ($lab,$hash,$unit) = @_;
  return sprintf("%-${llen}s: pr=%${flen}f  rc=%${flen}f  F=%${flen}f\n", $lab,prF($hash,$unit));
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- load input corpora
push(@ARGV,'-') if (!@ARGV);
our ($cfile1,$cfile2) = @ARGV;
our $corpus1 = DocClassify::Corpus->new(%corpusopts)->loadFile($cfile1,%loadopts_corpus)
  or die("$0: Corpus->loadFile() failed for '$cfile1': $!");
our $corpus2 = DocClassify::Corpus->new(%corpusopts)->loadFile($cfile2,%loadopts_corpus)
  or die("$0: Corpus->loadFile() failed for '$cfile2': $!");

## %l2doc = $label=>[$doc1,$doc2]
our %l2doc = qw();
$l2doc->{$_->label}[0] = $_ foreach (@{$cfile1->{docs}});
$l2doc->{$_->label}[1] = $_ foreach (@{$cfile2->{docs}});
my ($doc1,$doc2);
foreach (values(%l2doc)) {
  ($doc1,$doc2) = @$_;
  if (defined($doc1) && !defined($doc2)) {
    warn("$0: document label '".$doc1->label."' not defined for '$cfile2' -- ignoring");
    delete($l2doc{$doc1->label});
  }
  elsif (!defined($doc1) && defined($doc2)) {
    warn("$0: document label '".$doc2->label."' not defined for '$cfile1' -- ignoring");
    delete($l2doc{$doc2->label});
  }
}


## %c2e: ($catName => { (tp|fp|fn)_(docs|bytes)=>$n }, ''=>{(tp|fp|fn)_(docs|bytes)=>$n}, )
our %c2e = qw();
our $Ndocs = 0;
our $Nbytes = 0;

##-- compare docs by label
my ($docs, $cats1,$cats2);
foreach $docs (values(%l2doc)) {
  ($doc1,$doc2) = @$docs;
  ($cats1,$cats2) = ($doc1->cats,$doc2->cats);
  ($cat1,$cat2) = ($cats1->[0],$cats2->[0]);
  if ($cat1 eq $cat2) {
    true_positive($cat1,$doc1);
  }
  else {
    false_negative($cat1,$doc1);
    false_positive($cat2,$doc1);
  }

}

##-- report: precision, recall, F: by category
print
  (
   (map { report($_,$c2e{$_},'docs') } sort(keys(%c2e))),
   report('GLOBAL', $c2e{''}, 'docs'),
   "\n",
   ##--
   (map { report($_,$c2e{$_},'bytes') } sort(keys(%c2e))),
   report('GLOBAL', $c2e{''}, 'bytes'),
  );

=pod

=head1 NAME

dc-mapper-eval.perl - evaluate Mapper results

=head1 SYNOPSIS

 dc-mapper-eval.perl [OPTIONS] CORPUS_WANTED CORPUS_GOT

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -input-mode MODE       # I/O mode for input corpora (default=guess)
  -output-mode MODE      # I/O mode for output corpus (default=guess)
  -output-file FILE      # set output file (default=-)

=cut

##------------------------------------------------------------------------------
## Options and Arguments
##------------------------------------------------------------------------------
=pod

=head1 OPTIONS AND ARGUMENTS

Not yet written.

=cut

##------------------------------------------------------------------------------
## Description
##------------------------------------------------------------------------------
=pod

=head1 DESCRIPTION

Not yet written.

=cut

##------------------------------------------------------------------------------
## See Also
##------------------------------------------------------------------------------
=pod

=head1 SEE ALSO

...

=cut

##------------------------------------------------------------------------------
## Footer
##------------------------------------------------------------------------------
=pod

=head1 AUTHOR

Bryan Jurish E<lt>jurish@uni-potsdam.deE<gt>

=cut
