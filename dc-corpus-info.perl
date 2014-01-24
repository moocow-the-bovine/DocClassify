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
	 outputFile => '-',
	 cats=>1,
	);

our $want_ndocs = 1;
our $want_bytes = 0;
our $want_cats = 1; ##-- $opts{cats}

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(dcOptions(),

	   ##-- Misc
	   'ndocs|docs|d!' => \$want_ndocs,
	   'sizes|bytes|s|b!' => \$want_bytes,
	   'c!' => \$opts{cats},
	  );
$verbose=$opts{verbose};
$want_cats=$opts{cats};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

sub catcmp {
  my ($ai,$bi,$as,$bs) = (0,0,$a,$b);
  ($ai,$as) = ($1,$2) if ($a=~/^(\d+)(.*)$/);
  ($bi,$bs) = ($1,$2) if ($b=~/^(\d+)(.*)$/);
  return ( $ai <=> $bi || $as cmp $bs);
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

our $outfile = $opts{outputFile};
our $outfh = IO::File->new(">$outfile")
  or die("$0: open failed for output file '$outfile': $!");
binmode($outfh,':utf8');


##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
foreach my $cfile (@ARGV) {
  our $corpus = DocClassify::Corpus->loadFile($cfile)
    or die("$0: load failed for corpus file '$cfile': $!");

  my $c2doc = $corpus->docsByCat();
  my $ndocs = scalar(@{$corpus->{docs}});
  my %c2ndocs = (map {$_=>scalar(@{$c2doc->{$_}})} keys(%$c2doc));
  my $ncats = scalar(keys(%c2ndocs));

  my $nbytes = 0;
  $nbytes += $_->sizeBytes foreach (@{$corpus->{docs}});
  ##
  my %c2nbytes = (map {$_=>0} keys(%$c2doc));
  foreach my $c (keys(%c2nbytes)) {
    $c2nbytes{$c} += $_->sizeBytes foreach (@{$c2doc->{$c}});
  }

  my $llen = '-60';
  my $ilen = '10';
  my $flen = '6.2';
  $outfh->print(
		"Corpus File: $cfile\n",
		sprintf("%${llen}s: %${ilen}d\n", "#/Categories", $ncats),
		##
		($want_ndocs
		 ? (sprintf("%${llen}s: %${ilen}d (%${flen}f %%)\n", "#/Docs (TOTAL)", $ndocs, 100),
		    ($want_cats
		     ? (
			map {
			  sprintf("%${llen}s: %${ilen}d (%${flen}f %%)\n", "> #/Docs (CAT '$_')",
				  $c2ndocs{$_}, 100*$c2ndocs{$_}/$ndocs)
			} sort catcmp keys(%c2ndocs)
		       )
		     : qw()))
		 : qw()),
		##
		($want_bytes
		 ? (sprintf("%${llen}s: %${ilen}d (%${flen}f %%)\n", "#/Bytes (TOTAL)", $nbytes, 100),
		    ($want_cats
		     ? (
			map {
			  sprintf("%${llen}s: %${ilen}d (%${flen}f %%)\n", "> #/Bytes (CAT '$_')",
				  $c2nbytes{$_}, 100*$c2nbytes{$_}/$nbytes)
			} sort catcmp keys(%c2nbytes)
		       )
		     : qw()))
		 : qw()),
	       );
}

=pod

=head1 NAME

dc-corpus-info.perl - get some basic information from xml corpus files

=head1 SYNOPSIS

 dc-corpus-info.perl [OPTIONS] [CORPUS...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -out OUTFILE           # output file (default=STDOUT)

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
