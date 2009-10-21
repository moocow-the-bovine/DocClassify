#!/usr/bin/perl -w

use lib qw(.);
use MUDL;

use PDL;
use PDL::Ngrams;

use Getopt::Long qw(:config no_ignore_case);
use Encode qw(encode decode);
use File::Basename qw(basename);
use Pod::Usage;

#use strict;

##------------------------------------------------------------------------------
## Constants & Globals
##------------------------------------------------------------------------------
our $prog = basename($0);
our ($help,$verbose);

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;
our $outfile  = '-';


##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,

	   ##-- I/O
	   #'input-encoding|ie=s'             => \$inputEncoding,
	   #'output-encoding|oe=s'            => \$outputEncoding,
	   'output|o=s'=>\$outfile,
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
open(OUT,">$outfile") or die("$0: open failed for '$outfile': $!");

foreach $f (@ARGV) {
  #print STDERR "$progname: parsing file '$f'...";

  do_stuff();

  #print STDERR " done.\n";
}

close(OUT);

=pod

=head1 NAME

template.perl - undocumented program

=head1 SYNOPSIS

 template.perl [OPTIONS] [FILE...]

 General Options:
  -help                  # this help message

 I/O Options:
  -output FILE           # specify output file (default='-' (STDOUT))

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
