#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Mapper::Train;
use DocClassify::Program ':all';
use DocClassify::Utils ':all';

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
	 outputFile => '-',
	);

##-- hack: set only local overrides with '-map-option OPT=VALUE'
our $mapUser = {};
my %dcOpts = dcOptions();
$_ = $mapUser foreach (grep {$_ eq $opts{mapNew}} values %dcOpts);

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(%dcOpts
	  );
$verbose = $opts{verbose};
our $outfile = $opts{outputFile};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});
pod2usage({-exitval=>1, -verbose=>0, -msg=>'No Mapper file specified!'}) if (!@ARGV);


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- logger
DocClassify::Logger->ensureLog;

##-- vars
our $logger = 'DocClassify::Program';
our $mapfile = shift(@ARGV);
our $mapper = DocClassify::Mapper->loadFile($mapfile, optsLoad('map') )
  or die("$0: Mapper->load() failed for '$mapfile': $!");
$mapper->{verbose} = $verbose;
@$mapper{keys %$mapUser} = values %$mapUser; ##-- user overrides
our %mapopts = optsNew('map');

$mapper->clearTrainingCache() if ($mapper->{clearCache} ||= $mapopts{clearCache});
$mapper->saveFile($outfile, optsSave('map'));

=pod

=head1 NAME

dc-mapper-tweak.perl - tweak DocClassify::Mapper options

=head1 SYNOPSIS

 dc-mapper-tweak.perl [OPTIONS] MAPFILE

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -map-option OPT=VALUE  # override stored mapper option
  -map-input-mode MODE   # I/O mode for input mapfile (default=guess)
  -[no]clear-cache       # do/don't clear training cache (default=do)
  -output-file FILE      # set mapper output file (default=-)

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
