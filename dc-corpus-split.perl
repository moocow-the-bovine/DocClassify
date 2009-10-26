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
#our $format   = 1;

our $outfmt = '-';
our $seed=undef;
our $labfmt=undef;
our $n_corpora = 2;

our %loadopts = ( mode=>undef, );
our %saveopts = ( mode=>undef, format=>1, );


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
	   'input-mode|im=s' => \$loadopts{mode},
	   'output-mode|om=s' => \$saveopts{mode},
	   'format-xml|format|fx|f!' => \$saveopts{format},
	  );

pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- sanity check(s)
$saveopts{mode} = DocClassify::Corpus->guessFileMode($outfmt,%saveopts) if (!defined($saveopts{mode}));
if ($outfmt ne '-' && $outfmt !~ m/%(?:\d*)(?:\.?)(?:\d*)d/) {
  $outfmt =~ s/\.([^\.]*)$/%d.$1/;
}

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);

my $cfile = shift(@ARGV);
print STDERR "$0: loadFile($cfile)\n" if ($verbose);
our $corpus = DocClassify::Corpus->loadFile($cfile,%loadopts)
  or die("$0: load failed for corpus file '$cfile': $!");

print STDERR "$0: splitN(n=>$n_corpora)\n" if ($verbose);
our @subc = $corpus->splitN($n_corpora,seed=>$seed,label=>$labfmt);

foreach $i (0..$#subc) {
  my $outfile = sprintf($outfmt,$i);
  print STDERR "$0: saveFile($outfile)\n" if ($verbose);
  $subc[$i]->saveFile($outfile,%saveopts);
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
  -input-mode MODE       # input mode (default=guess)
  -output-mode MODE      # output mode (default=guess)

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
