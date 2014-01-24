#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Mapper::Train;
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
our $verbose = setVerboseOptions(2);

%opts = (%opts,
	 evalLoad=>{optsLoad('eval'), verboseIO=>1},
	);
$opts{outputFile} = undef;

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- common options
	   dcOptions(),
	  );

$verbose = $opts{verbose};
our $outfile = $opts{outputFile};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});
#pod2usage({-exitval=>1, -verbose=>0, -msg=>'No Mapper file specified!'}) if (!@ARGV);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- vars: logger
our $logger = 'DocClassify::Program';

##-- vars: cutoff pseudo-mapper
push(@ARGV,'-') if (!@ARGV);
our $mapfile = shift(@ARGV);
our $map = DocClassify::Mapper::Cutoff->new(optsNew('cutoff'))->loadFile($mapfile,optsLoad('cutoff'))
  or die("$0: Cutoff->loadFile() failed for '$mapfile': $!");

##-- save
$outfile = $mapfile if (!defined($outfile));
$map->dumpData($outfile, optsSave('cutoff'));

=pod

=head1 NAME

dc-cutoff-dat.perl - dump data underlying a DocClassify::Mapper::Cutoff pseudo-mapper

=head1 SYNOPSIS

 dc-cutoff-dat.perl [OPTIONS] [CUTOFF_FILE]

 General Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level

 I/O Options:
  -output-file FILE      # set output basename (default=CUTOFF_FILE)

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
