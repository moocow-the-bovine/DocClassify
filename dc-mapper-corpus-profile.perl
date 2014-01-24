#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Mapper::Train;
use DocClassify::Program ':all';

use PDL;
#use PDL::Ngrams;

use File::Path qw(make_path);
use File::Basename qw(basename dirname);
use Getopt::Long qw(:config no_ignore_case);
use Encode qw(encode decode);
use File::Basename qw(basename);
use Pod::Usage;

use strict;
BEGIN { select(STDERR); $|=1; select(STDOUT); }

##------------------------------------------------------------------------------
## Constants & Globals
##------------------------------------------------------------------------------
our $prog = basename($0);

our $verbose = setVerbose(2);
%opts = (%opts,
	 #corpusSave => { optsSave('corpus'), format=>1, saveCats=>1,saveSigs=>0 },
	 outputFile => undef,
	);

##-- hack: set only local overrides with '-map-option OPT=VALUE'
our $mapUser = {};
my %dcOpts = dcOptions();
$_ = $mapUser foreach (grep {$_ eq $opts{mapNew}} values %dcOpts);

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   %dcOpts,
	  );
$verbose = $opts{verbose};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});
pod2usage({-exitval=>0, -verbose=>0, -msg=>'No Mapper file specified!'}) if (!@ARGV);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- logger
DocClassify::Logger->ensureLog;

##-- vars
our $mapfile = shift(@ARGV);
our $outdir = $opts{outputFile} || "$mapfile.d";
our $logger = 'DocClassify::Program';
our $map = DocClassify::Mapper->loadFile($mapfile, optsLoad('map') )
  or die("$0: Mapper->load() failed for '$mapfile': $!");
@$map{keys %$mapUser} = values %$mapUser; ##-- user overrides a la dc-mapper-apply.perl
our %mapopts = optsNew('map');
$map->{verbose} = $verbose;

##-- output directory
-d "$outdir"
  or File::Path::make_path($outdir)
  or $logger->logconfess("mkpath(): failed to create output directory $outdir: $!");

##-- get data matrices
my $tdm  = $map->{tdm};
my $tw   = $map->{tw};
my $tdm0 = $map->get_tdm0();
my $tf0  = $map->get_tf0();
my $tdf0 = $map->get_tdf0();
my $tw0  = $map->get_tw0();
my $denum = $map->{denum};
my $tenum = $map->{tenum};

my ($dname);
foreach my $dname (sort keys %{$denum->{sym2id}}) {
  my $dbase = File::Basename::basename($dname);
  my $dfile = "$outdir/$dbase.dat";
  $logger->info("writing $dfile");

  my $outfh = IO::File->new(">$dfile")
    or die("$0: open failed for output file '$dfile': $!");
  binmode($outfh,':utf8');
  $outfh->print("## ".ref($map)," profile for $dname\n",
		"## Format (TAB-separated): TERM  WEIGHTED_LOG_FREQ  RAW_FREQ  TERM_WEIGHT  TERM_NDOCS\n",
	       );

  my $di    = $denum->{sym2id}{$dname};
  my $d_tf  = $tdm->dice_axis(1,$di)->decode->flat;
  my $d_tf0 = $tdm0->dice_axis(1,$di)->decode->flat;
  my $d_tfi = $d_tf->qsorti->slice("-1:0");
  $d_tfi    = $d_tfi->where(($d_tf0->index($d_tfi) >= $map->{minFreq}) & ($tdf0->index($d_tfi) >= $map->{minDocFreq}));
  my ($ti,$term, $termWF, $termRF, $termW, $termDF);
  foreach $ti ($d_tfi->list) {
    $term   = $tenum->{id2sym}[$ti];
    $termWF = $d_tf->at($ti);
    $termRF = $d_tf0->at($ti);
    $termW  = $tw0->at($ti);
    $termDF = $tdf0->at($ti);

    $outfh->print(join("\t", $term, $termWF, $termRF, $termW, $termDF)."\n");
  }
  $outfh->close();
}

$logger->info("exiting normally.");

__END__

=pod

=head1 NAME

dc-mapper-corpus-profile.perl - get a document-level corpus profile for DocClassify::Mapper

=head1 SYNOPSIS

 dc-mapper-corpus-profile.perl [OPTIONS] MAPFILE

 Options:
  -help                   # this help message
  -verbose LEVEL          # verbosity level
  -map-option OPT=VALUE   # override stored mapper option
  -min-freq FREQ          # override stored minimum term frequency
  -min-doc-frequency FREQ # override stored minimum doc frequency
  -output DIRECTORY       # set output directory (default=MAPFILE.d)

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
