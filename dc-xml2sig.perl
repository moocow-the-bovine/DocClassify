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

our %fcopts = (
	       verbose=>2,
	       recursive=>1,
	       inputFileMatch=>qr/\.xml$/,
	       inputFileTrim=>qr/\.[^\.]*$/,
	       outputFile=>undef,
	       outputFileSuffix=>'.sig.bin',
	      );

our %loadopts =
  (
   mode=>undef,
  );

our %saveopts =
  (
   mode=>undef,
   lemmatized=>0,
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
	   'output-mode|om=s' => \$saveopts{mode},
	  );
$verbose=$fcopts{verbose};

pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

our ($fc);
sub cb_xml2sig {
  my ($infile) = @_;
  my $doc = DocClassify::Document->new(file=>$infile)
    or die("$0: Document->new() failed for '$infile': $!");
  my ($outfile,$outfh) = $fc->in2out($infile);
  $outfh->binmode(':utf8');
  my $sig = $doc->typeSignature()
    or die("$0: typeSignature() failed for '$infile': $!");
  $sig->saveFile($outfh,%saveopts)
    or die("$0: Signature::saveFile() failed for '$outfile': $!");
  $outfh->close() if (!defined($fc->{outputFile}));
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
$fc = DocClassify::FileChurner->new( %fcopts, fileCallback=>\&cb_xml2sig );
$fc->churn(@ARGV);


=pod

=head1 NAME

dc-xml2sig.perl - convert DocClassify xml docs to binary term-frequency signature files

=head1 SYNOPSIS

 dc-xml2sig.perl [OPTIONS] [INPUT(s)...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -output-file FILE      # all output to a single file
  -output-suffix SUFFIX  # one outfile per infile, suffix SUFFIX (default=.sig.bin)
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
