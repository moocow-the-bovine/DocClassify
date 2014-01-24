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
	       inputFileMatch=>qr/(?<!\.head)\.xml$/,
	       inputFileTrim=>qr/\.[^\.]*$/,
	       outputFile=>undef,
	       outputFileSuffix=>'.head.xml',
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
our $nfiles = 0;
sub cb_xml2head {
  my ($xmlfile) = @_;
  my $doc = DocClassify::Document->new(file=>$xmlfile);
  my $xdoc = $doc->xmlDoc;
  my ($outfile,$outfh) = $fc->in2out($xmlfile);
  my $root = $xdoc->documentElement;
  $root->removeChild($_) foreach (@{$root->findnodes('*[name()!="head"]')});
  if (!defined($fc->{outputFile})) {
    $outfh->close() ;
    $xdoc->toFile($outfile, 1);
  } else {
    $xdoc->toFH($outfh, 1);
  }
  ++$nfiles;
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
$fc = DocClassify::FileChurner->new( %fcopts, fileCallback=>\&cb_xml2head );
$fc->churn(@ARGV);

print STDERR "$0: processed $nfiles files.\n" if ($verbose);

=pod

=head1 NAME

dc-xml2head.perl - extract header data from DocClassify xml files

=head1 SYNOPSIS

 dc-xml2head.perl [OPTIONS] [INPUT(s)...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -output-file FILE      # all output to a single file
  -output-suffix SUFFIX  # one outfile per infile, suffix SUFFIX (default=.head.xml)

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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=cut
