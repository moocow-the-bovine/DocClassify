#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Program ':all';
use DocClassify::Utils ':all';

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
our $doProfile = 1;
%opts = (%opts,
	 corpusSave => { optsSave('corpus'), format=>1, saveCats=>1,saveSigs=>0 },
	 outputFile => '-',
	);

##-- hack: set only local overrides with '-map-option OPT=VALUE'
our $mapUser = {};
my %dcOpts = dcOptions();
$_ = $mapUser foreach (grep {$_ eq $opts{mapNew}} values %dcOpts);

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(%dcOpts,
	   'stats|st!' => \$doProfile,
	  );
$verbose = $opts{verbose};
our $outfile = $opts{outputFile};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});
pod2usage({-exitval=>1, -verbose=>0, -msg=>'No Mapper file specified!'}) if (!@ARGV);


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
@$mapper{keys %$mapUser} = values %$mapUser; ##-- user overrides

##-- profiling
our $ndocs = 0;
profile_start() if ($doProfile);

##-- load input corpora
push(@ARGV,'-') if (!@ARGV);
our ($corpus);
foreach (@ARGV) {
  my $c2 = DocClassify::Corpus->new(optsNew('corpus'))->loadFile($_,optsLoad('corpus'))
    or die("$0: Corpus::loadFile() failed for '$_': $!");
  $mapper->info("mapCorpus($_)") if ($verbose);
  $c2 = $mapper->mapCorpus($c2)
    or die("$0: Mapper::mapCorpus() failed for '$_': $!");
  if (!defined($corpus)) { $corpus=$c2; }
  else { $corpus->addCorpus($c2); }
  $ndocs += scalar(@{$c2->{docs}});
}

profile_stop() if ($doProfile);
$corpus->saveFile($outfile, optsSave('corpus'));

##-- report profiling info
if ($doProfile && $ndocs>0) {
  print STDERR "$prog: ", profile_string($ndocs), "\n";
}

=pod

=head1 NAME

dc-mapper-apply.perl - apply DocClassify::Mapper subclass object to a corpus

=head1 SYNOPSIS

 dc-mapper-apply.perl [OPTIONS] MAPFILE [CORPUS...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -map-option OPT=VALUE  # override stored mapper option
  -map-input-mode MODE   # I/O mode for input mapfile (default=guess)
  -input-mode MODE       # I/O mode for input corpora (default=guess)
  -output-mode MODE      # I/O mode for output corpus (default=guess)
  -output-file FILE      # set corpus output file (default=-)
  -profile, -noprofile   # do/don't report profiling info (default=do)

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
