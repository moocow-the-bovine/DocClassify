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

our %evalopts = qw();
our %saveopts = qw();

our $outfile = '-';

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- I/O
	   #'recursive|recurse|r!' => \$fcopts{recursive},
	   'output-file|outfile|out|of|o=s'=> \$outfile,
	   #'output-suffix|os=s' => \$fcopts{outputFileSuffix},
	   #'format|f=1' => \$format
	  );
#$verbose=$fcopts{verbose};


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- out file
our $outfh = IO::File->new(">$outfile")
  or die("$0: open failed for output file '$outfile': $!");

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
foreach my $infile (@ARGV) {
  my $eval = DocClassify::Eval->new(%evalopts)->loadFile($infile)
    or die("$0: Eval->loadFile() failed for '$infile': $!");
  $eval->{label} = $infile;
  $eval->saveTextFile($outfh,%saveopts);
  $outfh->print("\n");
}

$outfh->close();


=pod

=head1 NAME

dc-eval-info.perl - get human-readable output from DocClassify::Eval files

=head1 SYNOPSIS

 dc-eval-info.perl [OPTIONS] [EVAL_FILE(s)...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -output-file FILE      # all output to a single file

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
