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

	 evalLoad=>{optsLoad('eval'), verboseIO=>1},
	 cutoffSave=>{optsSave('cutoff'), verboseIO=>1},
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
our %cutopts = optsLoad('map');
our $map = DocClassify::Mapper::Cutoff->new(optsNew('cutoff'))
  or die("$0: Cutoff->new() failed: $!");

##-- map: override verbosity options
$map->{verbose} = $verbose;

##-- load input eval file(s)
$logger->info("train()") if ($verbose);
push(@ARGV,'-') if (!@ARGV);
foreach my $efile (@ARGV) {
  my $eval = DocClassify::Eval->new(optsNew('eval'))->loadFile($efile, optsLoad('eval'))
    or die("$0: Eval->loadFile() failed for '$efile': $!");
  $map->trainEval($eval);
}

##-- compile cutoffs
$logger->info("compile()") if ($verbose);
$map->compile();

$logger->info("clearTrainingCache()") if ($verbose);
$map->clearTrainingCache() if ($opts{mapNew}{clearCache});

##-- save
$map->saveFile($outfile, optsSave('cutoff'));

=pod

=head1 NAME

dc-cutoff-train.perl - train DocClassify::Mapper::Cutoff pseudo-mapper from eval file(s)

=head1 SYNOPSIS

 dc-cutoff-train.perl [OPTIONS] [CORPUS...]

 General Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level

 Cutoff Options:
  -cut0p   P             # confidence level for negative-sample (0.5)
  -cut1p   P             # confidence level for positive-sample (0.5)
  -cut1w   W             # weight (0<=W<=1) for positive-point (0.65)
  -cut-val ADD           # constant to add if cutoff is exceeded (100)
  -cut-cat CAT           # name of cutoff sink cat (default: cat with id=0 in $lcenum)

 I/O Options:
  -output-mode MODE      # I/O mode for output pseudo-mapper (default=guess)
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
