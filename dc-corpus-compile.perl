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

our $verbose = setVerboseOptions(1);
%opts = (%opts,
	 corpusSave => { %{$opts{corpusSave}}, saveCats=>1, saveSigs=>1 },
	 sigLoad => { %{$opts{sigLoad}}, mode=>undef, lemmatized=>0, verboseIO=>0 },
	 sigSave => { %{$opts{sigSave}}, mode=>undef, lemmatized=>0, verboseIO=>0 },
	 outputFile => '-',
	 sigSuffix => '.sig.bin',
	);

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(dcOptions(),

	   ##-- I/O
	   'signature-mode|sigmode|sm=s' => \$opts{sigSave}{mode},
	   'signature-suffix|sig-suffix|ss=s' => \$opts{sigSuffix}, ##-- per-document signature suffix
	  );
$verbose = $opts{verbose};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- vars
$corpus = undef;

##-- load input corpora
push(@ARGV,'-') if (!@ARGV);
foreach (@ARGV) {
  my $c2 = DocClassify::Corpus->new( newOpts('corpus') )->loadFile($_, optsLoad('corpus'))
    or die("$0: Corpus::loadFile() failed for '$_': $!");
  if (!$corpus) { $corpus=$c2; next; }
  $corpus->addCorpus($c2);
}

##-- compile
my ($doc);
foreach $doc (@{$corpus->{docs}}) {
  DocClassify::Program->vlog('trace', "DOC: ", $doc->label) if ($verbose);

  ##-- compile: categories
  if ($opts{corpusSave}{saveCats}) { $doc->cats; }

  ##-- compile: signature
  if ($opts{corpusSave}{saveSigs}) {
    my $sigFile = $doc->{file}.$opts{sigSuffix};
    $doc->saveSignature($sigFile, optsSave('sig'));
    delete(@$doc{qw(sig xdoc)}); ##-- clear cache, in case of binary save
  }
}

#print STDERR "$prog: saveFile($outfile)\n" if ($verbose);
$corpus->saveFile($opts{outputFile}, optsSave('corpus'));

=pod

=head1 NAME

dc-corpus-compile.perl - pre-compile per-document information (categories, signature, ...)

=head1 SYNOPSIS

 dc-corpus-compile.perl [OPTIONS] [CORPUS...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -label LABEL           # set global corpus label
  -cats , -nocats        # do/don't pre-compile categories (default=do)
  -sigs , -nosigs        # do/don't pre-compile signatures (default=do)
  -input-mode MODE       # I/O mode for input corpora (default=guess)
  -output-mode MODE      # I/O mode for output corpus (default=guess)
  -signature-mode MODE   # I/O mode for signature files (default=guess)
  -signature-suffix SUFF # file suffix for generated signature files (default='.sig.bin')
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
