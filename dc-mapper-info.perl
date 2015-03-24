#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Mapper::Train;
use DocClassify::Program ':all';

use PDL;
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
	 #corpusSave => { optsSave('corpus'), format=>1, saveCats=>1,saveSigs=>0 },
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

## $str = banner(@cmts)
BEGIN { *banner = \&banner1; }
sub banner1 {
  my @cmts = @_;
  return (''
	  ."##" . ("=" x 72) . "\n"
	  .join('', map {"## $_\n"} @cmts)
	  ."##" . ("=" x 72) . "\n"
	 );
}

sub banner2 {
  my @cmts = @_;
  return (''
	  ."##" . ("-" x 64) . "\n"
	  .join('', map {"## $_\n"} @cmts)
	  #."##" . ("=" x 64) . "\n"
	 );
}


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

my ($tmp); ##-- avoid annoying 'Can't return a temporary from lvalue subroutine' messages

##-- vars
our $logger = 'DocClassify::Program';
our $mapfile = shift(@ARGV);
our $map = DocClassify::Mapper->loadFile($mapfile, optsLoad('map') )
  or die("$0: Mapper->load() failed for '$mapfile': $!");
$map->{verbose} = $verbose;

##-- outfile
our $outfh = IO::File->new(">$outfile")
  or die("$0: open failed for output file '$outfile': $!");
$outfh->binmode(':encoding(utf8)');


##-- common vars
my ($tenum,$denum,$lcenum,$gcenum) = @$map{qw(tenum denum lcenum gcenum)};
my $gcids  = pdl(long, [@{$gcenum->{sym2id}}{@{$lcenum->{id2sym}}}]);             ##-- [$lci] -> $gci
my $lcids  = zeroes(long,$gcids->max+1); ($tmp=$lcids->index($gcids)) .= $gcids->xvals;  ##-- [$gci] -> $lci
my $NCg    = $gcids->max+1;

##--------------------------------------------------------------
## Dump: Header
$outfh->print("## -*- Mode: Shell-Script; encoding: utf-8; -*-\n\n");

##--------------------------------------------------------------
## Dump: Cats: Ids
my $catlen = 0; foreach (@{$lcenum->{id2sym}}) { $catlen=length($_) if (length($_)>$catlen); }
my $idlen  = length(scalar(@{$gcenum->{id2sym}}));
$idlen = 4 if ($idlen < 4);
my ($cname);
my @lcids   = (0..$#{$lcenum->{id2sym}});
my @cnames  = @{$lcenum->{id2sym}};
my @gcids   = @{$gcenum->{sym2id}}{@cnames};
$outfh->print(#"\n",
	      banner("Category IDs"),
	      ("#ID_G\t(ID_L)\tNAME\n"),
	      map {sprintf("%d\t(%d)\t%s\n", $gcids[$_], $lcids[$_], $cnames[$_])} (0..$#cnames)
	     );

##--------------------------------------------------------------
## Dump: Cats: Size: #/Docs
my $dcm = $map->{dcm};
my $ND  = $dcm->dim(0);
my $lc_ndocs = $dcm->nnz->decode->double;
my $gc_ndocs = zeroes($NCg);
$gc_ndocs->index($gcids) .= $lc_ndocs;

my $dlen = 6;
$dlen = length($ND) if ($dlen < length($ND));
my $pctlen = 10;
my $pctfmt = "%${pctlen}.".($pctlen-4)."f";
sub _pct {
  my ($num,$denom) = @_;
  $num ||= 0;
  $denom ||= 0;
  return 'nan' if (!$denom);
  return 100.0*$num/$denom;
  #return ($num*1.0)/$denom;
}
my ($id,$ndocs);
$outfh->print("\n",
	      banner("Category Size (#/Docs)"),
	      sprintf("%-${dlen}s  %${pctlen}s%%  %-${idlen}s  %s\n", "#NDOCS", "PCTDOCS", "ID_G", "NAME"),
	      map {
		$id = $gcids[$_];
		$ndocs = $gc_ndocs->at($id);
		sprintf("%-${dlen}d  ${pctfmt}%%  %-${idlen}d  %s\n", $ndocs, _pct($ndocs,$ND), $id, $cnames[$_])
	      } (0..$#cnames)
	     );

##--------------------------------------------------------------
## Dump: Cats: Size: Total Term-Freq
my $tdm  = $map->{tdm};
my $tcm0 = $map->get_tcm0();
#my $tcm  = $map->get_tcm();

my $lc_tf = $tcm0->sumover->decode->rint;
my $gc_tf = zeroes($NCg);
$gc_tf->index($gcids) .= $lc_tf;
my $Ntf = $gc_tf->sum;

$dlen = length($Ntf) if (length($Ntf) > $dlen);
my ($tf);
$outfh->print("\n",
	      banner("Category Size (Total Term Frequency)"),
	      sprintf("%-${dlen}s  %${pctlen}s%%  %-${idlen}s  %s\n", "#NTOKS", "PCTTOKS", "ID_G", "NAME"),
	      map {
		$id = $gcids[$_];
		$tf = $gc_tf->at($id);
		sprintf("%-${dlen}d  ${pctfmt}%%  %-${idlen}d  %s\n", $tf, _pct($tf,$Ntf), $id, $cnames[$_])
	      } (0..$#cnames)
	     );

##--------------------------------------------------------------
## Dump: Cats: Size: total non-SEM term-freq
my $t_sem_mask = pdl(long, [map {/^SEM/ ? 1 : 0} @{$tenum->{id2sym}}]);
my $tcm0_sem0  = $tcm0->dice_axis(0,which(!$t_sem_mask));

my $lc_tf_sem0 = $tcm0_sem0->sumover->decode->rint;
my $gc_tf_sem0 = zeroes($NCg);
$gc_tf_sem0->index($gcids) .= $lc_tf_sem0;
my $Ntf_sem0 = $gc_tf_sem0->sum;

$outfh->print("\n",
	      banner("Category Size (non-SEM Term Frequency)"),
	      sprintf("%-${dlen}s  %${pctlen}s%%  %-${idlen}s  %s\n", "#NSEM0", "PCTSEM0", "ID_G", "NAME"),
	      map {
		$id = $gcids[$_];
		$tf = $gc_tf_sem0->at($id);
		sprintf("%-${dlen}d  ${pctfmt}%%  %-${idlen}d  %s\n", $tf, _pct($tf,$Ntf_sem0), $id, $cnames[$_])
	      } (0..$#cnames)
	     );

##--------------------------------------------------------------
## Dump: Cats: Size: total SEM term-freq
my $tcm0_sem1 = $tcm0->dice_axis(0,$t_sem_mask->which);

my $gc_tf_sem1 = zeroes($NCg);
my $Ntf_sem1   = 0;
if ($t_sem_mask->any) {
  my $lc_tf_sem1 = $tcm0_sem1->sumover->decode->rint;
  (my $tmp=$gc_tf_sem1->index($gcids)) .= $lc_tf_sem1;
  $Ntf_sem1 = $gc_tf_sem1->sum;
}

$outfh->print("\n",
	      banner("Category Size (SEM Term Frequency)"),
	      sprintf("%-${dlen}s  %${pctlen}s%%  %-${idlen}s  %s\n", "#NSEM1", "PCTSEM1", "ID_G", "NAME"),
	      map {
		$id = $gcids[$_];
		$tf = $gc_tf_sem1->at($id);
		sprintf("%-${dlen}d  ${pctfmt}%%  %-${idlen}d  %s\n", $tf, _pct($tf,$Ntf_sem1), $id, $cnames[$_])
	      } (0..$#cnames)
	     );

##--------------------------------------------------------------
## Dump: Cats: Size: best terms
my $tcm = $map->get_tcm();
my $tw  = $map->{tw};
my $ntpc  = 50;
my @ctp = qw();
foreach (0..$#cnames) {
  $lci = $lcids[$_];
  $tf  = $tcm->dice_axis(1,$lci)->decode->flat;
  $tfi = $tf->qsorti->slice("-1:0");
  $ctp[$_] = pdl($tfi->slice("0:".($ntpc-1)));
}

my $tlen = 32;
$outfh->print("\n",
	      banner("Best Category Terms (weighted, nTermsPerCat=$ntpc)"),
	      #sprintf("%-${idlen}s  %-${catlen}s  %s\n", "ID_G", "NAME", "TERM(WF:[W,F])..."),
	      map {
		$gcid = $gcids[$_];
		$lcid = $lcids[$_];
		$tp = $ctp[$_];
		("\n",
		 banner2("BestTerms[$gcid]: CAT=$cnames[$_]"),
		 map {
		   $tid=$_;
		   sprintf("%-${catlen}s\tv=%.4f\tw=%.4f\tf=%d\n",
			   $tenum->{id2sym}[$tid],
			   $tcm->at($tid,$lcid),
			   $tw->at($tid),
			   $tcm0->at($tid,$lcid))
		 } $tp->list)
	      } (0..$#cnames)
	     );


##-- cleanup
$outfh->close();



=pod

=head1 NAME

dc-mapper-info.perl - get some potentially useful information on a DocClassify::Mapper

=head1 SYNOPSIS

 dc-mapper-info.perl [OPTIONS] MAPFILE

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -map-input-mode MODE   # I/O mode for input mapfile (default=guess)
  -output-file FILE      # set output file (default=-)

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
