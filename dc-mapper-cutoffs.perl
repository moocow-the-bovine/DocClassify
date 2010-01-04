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
our $verbose = setVerboseOptions(2);

%opts = (%opts,

	 #corpusLoad=>{optsLoad('corpus'),verboseIO=>0},
	 #corpusSave=>{optsSave('corpus'),verboseIO=>0},
	);

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- common options
	   dcOptions(),
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

##-- vars: logger
our $logger = 'DocClassify::Program';

##-- vars: mapper
our $mapfile = shift(@ARGV);
our %mapopts = optsLoad('map');
our $map = DocClassify::Mapper->loadFile($mapfile,optsLoad('map'))
  or die("$0: Mapper->load() failed for '$mapfile': $!");

##-- map: override cutoff options
our %cutopts = optsNew('map');
delete(@cutopts{grep {$_ !~ /^cut/} keys(%cutopts)});
@$map{keys %cutopts} = values %cutopts;

##-- map: override verbosity options
$map->{verbose} = $verbose;

##-- load input eval file
push(@ARGV,'-') if (!@ARGV);
our $efile = shift(@ARGV);
our $eval = DocClassify::Eval->new(optsNew('eval'))->loadFile($efile, optsLoad('eval'))
  or die("$0: Eval->loadFile() failed for '$efile': $!");

##-- compile cutoffs
$map->loadCrossCheckEval($eval)
  or die("$0: Mapper->loadCrossCheckEval() failed: $!");
$map->compileFit()
  or die("$0: Mapper->compileFit() failed: $!");
$map->compileCutoffs()
  or die("$0: Mapper->compileCutoffs() failed: $!");

##-- save
$map->saveFile($outfile, optsSave('map'));

=pod

=head1 NAME

dc-mapper-cutoffs.perl - train DocClassify::Mapper::LSI cutoffs from an eval file

=head1 SYNOPSIS

 dc-mapper-cutoffs.perl [OPTIONS] [CORPUS...]

 General Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level

 Mapper Options:
  -mapper-option OPT=VAL # set generic (class-specific) mapper option

 I/O Options:
  -output-mode MODE      # I/O mode for output mapper (default=guess)
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
