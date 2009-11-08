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

our %loadopts_map = ( mode=>undef, );
our %loadopts_corpus = ( mode=>undef, );
our %saveopts_corpus = ( mode=>undef, format=>1, saveCats=>1,saveSigs=>0 );

our $outfile = '-';

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- Mapping Options (none)

	   ##-- I/O
	   'map-input-mode|mim=s' => \$loadopts_map{mode},
	   'corpus-input-mode|input-mode|cim|im=s' => \$loadopts_corpus{mode},
	   'corpus-output-mode|output-mode|com|om=s' => \$saveopts_corpus{mode},
	   'format-xml|format|fx|f!' => sub { $saveopts_corpus{format}=$_[1] ? 1 : 0; },
	   'output-file|outfile|out|of|o=s'=> \$outfile,
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);
pod2usage({-exitval=>0, -verbose=>0, -msg=>'No Mapper file specified!'}) if (!@ARGV);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- vars
our $mapfile = shift(@ARGV);
print STDERR "$prog: Mapper->load($mapfile)\n" if ($verbose);
our $mapper = DocClassify::Mapper->loadFile($mapfile, %loadopts_map )
  or die("$0: Mapper->load() failed for '$mapfile': $!");
$mapper->{verbose} = $verbose;

##-- load input corpora
push(@ARGV,'-') if (!@ARGV);
our ($corpus);
foreach (@ARGV) {
  print STDERR "$prog: mapCorpus($_)\n" if ($verbose);
  my $c2 = DocClassify::Corpus->new(%corpusopts)->loadFile($_,%loadopts_corpus)
    or die("$0: Corpus::loadFile() failed for '$_': $!");
  $c2 = $mapper->mapCorpus($c2)
    or die("$0: Mapper::mapCorpus() failed for '$_': $!");
  if (!defined($corpus)) { $corpus=$c2; }
  else { $corpus->addCorpus($c2); }
}

print STDERR "$prog: Corpus::saveFile($outfile)\n" if ($verbose);
$corpus->saveFile($outfile, %saveopts_corpus);

=pod

=head1 NAME

dc-mapper-apply.perl - apply DocClassify::Mapper subclass object to a corpus

=head1 SYNOPSIS

 dc-mapper-apply.perl [OPTIONS] MAPFILE [CORPUS...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -map-input-mode MODE   # I/O mode for input mapfile (default=guess)
  -input-mode MODE       # I/O mode for input corpora (default=guess)
  -output-mode MODE      # I/O mode for output corpus (default=guess)
  -output-file FILE      # set corpus output file (default=-)

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
