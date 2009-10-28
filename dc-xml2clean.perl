#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Utils ':io';

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

our %fcopts = (
	       verbose=>2,
	       recursive=>1,
	       inputFileMatch=>qr/\.xml$/,
	       inputFileTrim=>qr/\.[^\.]*$/,
	       outputFile=>undef,
	       outputFileSuffix=>'.clean.xml',
	      );

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$fcopts{verbose},

	   ##-- I/O
	   'recursive|recurse|r!' => \$fcopts{recursive},
	   'output-file|outfile|out|of|o=s'=> \$fcopts{outputFile},
	   'output-suffix|os=s' => \$fcopts{outputFileSuffix},
	   #'format|f=1' => \$format
	  );
$verbose=$fcopts{verbose};


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

our ($fc);
sub cb_xml2clean {
  my ($xmlfile) = @_;
  my $xmlbufr = slurpFile($xmlfile);
  $$xmlbufr =~ s/(\"\S*)\&(\S*\")/$1&amp;$2/g;
  $$xmlbufr =~ s/(\"\S*)\"(\S*\")/$1&quot;$2/g;
  $$xmlbufr =~ s/(\"\S*)\'(\S*\")/$1&apos;$2/g;
  $$xmlbufr =~ s/(\"\S*)\<(\S*\")/$1&lt;$2/g;
  $$xmlbufr =~ s/(\"\S*)\>(\S*\")/$1&gt;$2/g;
  ##
  my ($outfile,$outfh) = $fc->in2out($xmlfile);
  $outfh->print($$xmlbufr);
  $outfh->close() if (!defined($fc->{outputFile}));
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
$fc = DocClassify::FileChurner->new( %fcopts, fileCallback=>\&cb_xml2clean );
$fc->churn(@ARGV);


=pod

=head1 NAME

dc-xml2clean.perl - convert "dirty" pseudo-xml docs to "clean" well-formed XMl

=head1 SYNOPSIS

 dc-xml2clean.perl [OPTIONS] [INPUT(s)...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -output-file FILE      # all output to a single file
  -output-suffix SUFFIX  # one outfile per infile, suffix SUFFIX (default=.clean.xml)

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
