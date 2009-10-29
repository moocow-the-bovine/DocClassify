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
our $catListFile = undef;
our $outfile = '-';

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- Misc
	   'label|l=s' => \$corpusOpts{label},
	   'category-list|cat-list|cats|cat-file|cf|c=s' => \$catListFile,

	   ##-- I/O
	   'output-file|outfile|out|of|o=s'=> \$outfile,
	   'format-xml|format|fx|f!' => sub { $saveopts{format}=$_[1] ? 1 : 0; },
	   'input-mode|im=s' => \$loadopts{mode},
	   'output-mode|om=s' => \$saveopts{mode},
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

our %goodCats = qw();
sub loadCatList {
  my ($catfile)=shift;
  open(CATS,"<$catfile") or die("$0: open failed for '$catfile': $!");
  my ($cat,$rest);
  while (defined($_=<CATS>)) {
    chomp;
    next if (/^\#/ || /^\%%/ || /^\s*$/); ##-- ignore comments & blank lines
    ($cat,$rest) = split(/\t/,$_,2);
    $goodCats{$cat} = undef;
  }
  close(CATS);
}


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- vars
our $corpus = undef;

##-- load good-cat list file
pod2usage({-exitval=>1, -verbose=>0, -msg=>'No category list file specified!'})
  if (!defined($catListFile));
loadCatList($catListFile);

##-- load input corpora
push(@ARGV,'-') if (!@ARGV);
foreach (@ARGV) {
  my $c2 = DocClassify::Corpus->new(%corpusOpts)->loadFile($_,%loadopts)
    or die("$0: Corpus::loadFile() failed for '$_': $!");
  if (!$corpus) { $corpus=$c2; next; }
  $corpus->addCorpus($c2);
}

##-- prune
@{$corpus->{docs}} = grep {exists($goodCats{$_->cats->[0]{name}})} @{$corpus->{docs}};

print STDERR "$prog: saveFile($outfile)\n" if ($verbose);
$corpus->saveFile($outfile, %saveopts);

=pod

=head1 NAME

dc-corpus-select.perl - select sub-corpus by document category

=head1 SYNOPSIS

 dc-corpus-select.perl [OPTIONS] [CORPUS...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -label LABEL           # set output corpus label
  -cat-file FILE         # file containing list of "good" categories
  -input-mode MODE       # I/O mode for input corpora (default=guess)
  -output-mode MODE      # I/O mode for output corpus (default=guess)
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
