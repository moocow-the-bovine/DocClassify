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
our $verbose = 2;
our ($help);

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;

our %corpusOpts = ( label=>'', );

our %loadopts = ( mode=>undef, );
our %saveopts = ( mode=>undef, format=>1, saveCats=>1, saveSigs=>1, );

our %sigsaveopts = ( mode=>undef, lemmatized=>0 );
our $sigSuffix = '.sig.bin';
our $outfile = '-';

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- Misc
	   'label|l=s' => \$corpusOpts{label},
	   'categories|cats|cat!' => \$saveopts{saveCats}, ##-- whether to compile cats
	   'signatures|sigs|sig!' => \$saveopts{saveSigs}, ##-- whether to compile signatures

	   ##-- I/O
	   'output-file|outfile|out|of|o=s'=> \$outfile,
	   'format-xml|format|fx|f!' => sub { $saveopts{format}=$_[1] ? 1 : 0; },
	   'input-mode|im=s' => \$loadopts{mode},
	   'output-mode|om=s' => \$saveopts{mode},
	   'signature-mode|sigmode|sm=s' => \$sigsaveopts{mode},
	   'signature-suffix|sig-suffix|ss=s' => \$sigSuffix, ##-- per-document signature suffix
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


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
  my $c2 = DocClassify::Corpus->new(%corpusOpts)->loadFile($_,%loadopts)
    or die("$0: Corpus::loadFile() failed for '$_': $!");
  if (!$corpus) { $corpus=$c2; next; }
  $corpus->addCorpus($c2);
}

##-- compile
my ($doc);
foreach $doc (@{$corpus->{docs}}) {
  print STDERR "DOC: ", $doc->label, "\n" if ($verbose);

  ##-- compile: categories
  if ($saveopts{saveCats}) { $doc->cats; }

  ##-- compile: signature
  if ($saveopts{saveSigs}) {
    my $sigFile = $doc->{file}.$sigSuffix;
    $doc->saveSignature($sigFile, %sigsaveopts);
    delete(@$doc{qw(sig xdoc)}); ##-- clear cache, in case of binary save
  }
}

print STDERR "$prog: saveFile($outfile)\n" if ($verbose);
$corpus->saveFile($outfile, %saveopts);

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
