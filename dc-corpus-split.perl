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
our ($help,$verbose);

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;

our $outfile1 = '-';
our $outfile2 = '-';

our $frac1=undef;
our $frac2=undef;

our ($label1,$label2);

our $seed=undef;

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- Misc
	   'srand|seed|s=i' => \$seed,
	   'fraction1|frac1|f1=f' => \$frac1,
	   'fraction2|frac2|f2=f' => \$frac2,
	   'label1|l1=s' => \$label1,
	   'label2|l2=s' => \$label2,
	   'output-file1|outfile1|out1|of1|o1=s'=> \$outfile1,
	   'output-file2|outfile2|out2|of2|o2=s'=> \$outfile2,
	  );

pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- sanity checks
if (!defined($frac1) && !defined($frac2)) {
  $frac1 = 0.9;
  $frac2 = 0.1;
} elsif (!defined($frac1)) {
  $frac1 = 1.0-$frac2;
} elsif (!defined($frac2)) {
  $frac2 = 1.0-$frac1;
}
die("$0: cannot handle negative fractions!") if ($frac1 < 0 || $frac2 < 0);

##-- re-normalize (in case of integer fractions)
if ($frac1+$frac2 != 1.0) {
  $frac1 = $frac1/($frac1+$frac2);
  $frac2 = $frac2/($frac1+$frac2);
}

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);

my $cfile = shift(@ARGV);
our $corpus = DocClassify::Corpus->loadXmlFile($cfile)
  or die("$0: load failed for XML corpus file '$cfile': $!");

our ($c1,$c2) = $corpus->splitCorpus($frac1,seed=>$seed,label1=>$label1,label2=>$label2);

$c1->saveXmlFile($outfile1) or die("$0: saveXmlFile($outfile1) failed: $!");
$c2->saveXmlFile($outfile2) or die("$0: saveXmlFile($outfile2) failed: $!");

=pod

=head1 NAME

dc-corpus-split.perl - split an XML corpus into training and test sets

=head1 SYNOPSIS

 dc-corpus-split.perl [OPTIONS] [CORPUS=-]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -seed SEED             # specify random seed (default=none)
  -frac1 FRAC1           # fraction of corpus to OUTFILE1 (default=0.9)
  -frac2 FRAC2           # fraction of corpus to OUTFILE2 (default=1-FRAC1)
  -out1 OUTFILE1         # output file 1 (e.g. training set)
  -out2 OUTFILE2         # output file 2 (e.g. test set)

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
