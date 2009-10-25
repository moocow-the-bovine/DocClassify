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
our ($help);
our $verbose = 2;

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
our $format   = 1;

our $outfmt = '-';
our $seed=undef;
our $labfmt=undef;

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- Misc
	   'srand|seed|s=i' => \$seed,
	   'label-fmt|labfmt|lf|l=s' => \$labfmt,
	   'n-corpora|nc|n=i' => \$n_corpora,
	   'output-fmt|output|outfile|outfmt|out|of|o=s'=> \$outfmt,
	   'format-xml|format|fx|f!' => \$format,
	  );

pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- sanity check(s)
$outfmt .= '.%0.2d' if ($outfmt ne '-' && $outfmt !~ m/%(?:\d*)(?:\.?)(?:\d*)d/);

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);

my $cfile = shift(@ARGV);
print STDERR "$0: loadXmlFile($cfile)\n" if ($verbose);
our $corpus = DocClassify::Corpus->loadXmlFile($cfile)
  or die("$0: load failed for XML corpus file '$cfile': $!");

print STDERR "$0: splitN(n=>$n_corpora)\n" if ($verbose);
our @subc = $corpus->splitN($n_corpora,seed=>$seed,label=>$labfmt);

foreach $i (0..$#subc) {
  my $outfile = sprintf($outfmt,$i+1);
  print STDERR "$0: saveXmlFile($outfile)\n" if ($verbose);
  $subc[$i]->saveXmlFile($outfile,format=>$format);
}

=pod

=head1 NAME

dc-corpus-split.perl - split an XML corpus into training and test sets

=head1 SYNOPSIS

 dc-corpus-split.perl [OPTIONS] [CORPUS=-]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -n N                   # number of output corpora
  -seed SEED             # specify random seed (default=none)
  -label LABFMT          # printf() format for output labels (default=none)
  -output OUTFMT         # printf() format for output files (default=STDOUT='-')

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
