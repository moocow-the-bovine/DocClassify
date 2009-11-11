#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Program ':all';

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

our $verbose = setVerbose(2);
%opts = (%opts,
	 corpusSave => { optsSave('corpus'), format=>1, saveCats=>1,saveSigs=>0 },
	 outputFile => '-',
	);

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   dcOptions(),
	  );
$verbose = $opts{verbose};
our $outfile = $opts{outputFile};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});
pod2usage({-exitval=>0, -verbose=>0, -msg=>'No Mapper file specified!'}) if (!@ARGV);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- vars
our $logger = 'DocClassify::Program';
our $mapfile = shift(@ARGV);
our $mapper = DocClassify::Mapper->loadFile($mapfile, optsLoad('map') )
  or die("$0: Mapper->load() failed for '$mapfile': $!");
$mapper->{verbose} = $verbose;

##-- load input corpora
push(@ARGV,'-') if (!@ARGV);
our ($corpus);
foreach (@ARGV) {
  $logger->info("Mapper->mapCorpus($_)") if ($verbose);
  my $c2 = DocClassify::Corpus->new(optsNew('corpus'))->loadFile($_,optsLoad('corpus'))
    or die("$0: Corpus::loadFile() failed for '$_': $!");
  $c2 = $mapper->mapCorpus($c2)
    or die("$0: Mapper::mapCorpus() failed for '$_': $!");
  if (!defined($corpus)) { $corpus=$c2; }
  else { $corpus->addCorpus($c2); }
}

$corpus->saveFile($outfile, optsSave('corpus'));

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
