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
our ($help,$verbose);

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;

our $outfile = '-';
our $want_ndocs = 1;
our $want_bytes = 1;
our $want_cats = 1;

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- Misc
	   'ndocs|docs|d!' => \$want_ndocs,
	   'sizes|bytes|s|b!' => \$want_bytes,
	   'cats|c!' => \$want_cats,
	   'output-file|outfile|out|of|o=s'=> \$outfile,
	  );

pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

our $outfh = IO::File->new(">$outfile")
  or die("$0: open failed for output file '$outfile': $!");


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
		sprintf("%${llen}s: %${ilen}d (%${flen}f %%)\n", "#/Docs (TOTAL)", $ndocs, 100),
		($want_cats && $want_ndocs
		 ? (
		    map {
		      sprintf("%${llen}s: %${ilen}d (%${flen}f %%)\n", "> #/Docs (CAT '$_')",
			      $c2ndocs{$_}, 100*$c2ndocs{$_}/$ndocs)
		    } sort(keys(%c2ndocs)))
		 : qw()),
		sprintf("%${llen}s: %${ilen}d (%${flen}f %%)\n", "#/Bytes (TOTAL)", $nbytes, 100),
		($want_cats && $want_bytes
		 ? (
		    map {
		      sprintf("%${llen}s: %${ilen}d (%${flen}f %%)\n", "> #/Bytes (CAT '$_')",
			      $c2nbytes{$_}, 100*$c2nbytes{$_}/$nbytes)
		    } sort(keys(%c2nbytes))
		   )
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

Bryan Jurish E<lt>jurish@uni-potsdam.deE<gt>

=cut
