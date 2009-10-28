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

our %corpusopts = qw();

our %mapopts = (
		class=>'LSI',    ##-- mapper class
		label=>undef,    ##-- default label
		lemmatize=>{},   ##-- see $DocClassify::Signature::LEMMA_XYZ variables for defaults
		svdr => 64,      ##-- svd dimensions
		minFreq =>0,     ##-- minimum frequency
		smoothf =>1,     ##-- smoothing frequency (undef for NTypes/NTokens)
		trainExclusive=>1, ##-- exclusive-mode training?
		catProfile => 'average',   ##-- how to do category profiling
		termWeight => 'entropy',   ##-- how to do term weighting
	       ),

our %loadopts_corpus = ( mode=>undef, );
our %saveopts_map    = ( mode=>undef, format=>1, );

our $compileMap = 1; ##-- whether to compile map before saving
our $outfile = '-';

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- Map Options
	   'mapper-class|mapclass|class|mapc|mc=s' => \$mapopts{class},
	   'label|l=s' => \$mapopts{label},
	   'lemmatize-option|lemma-option|lemma|L=s%' => $mapopts{lemmatize},
	   'min-frequency|min-freq|mf=f' => \$mapopts{minFreq},
	   'smooth-frequency|smooth-freq|smoothf|sf=f' => \$mapopts{smoothf},
	   'svd-dims|svd-r|svdr|r=i' =>\$mapopts{svdr},
	   'exclusive|x!' => \$mapopts{trainExclusive},
	   'cat-profile|catProfile|profile|cp=s' => \$mapopts{catProfile},
	   'term-weight|termWeight|tw|w=s'       => \$mapopts{termWeight},
	   'mapper-option|mo=s' => \%mapopts,
	   'compile|c!' => \$compileMap,

	   ##-- I/O
	   'corpus-input-mode|input-mode|cim|im=s' => \$loadopts_corpus{mode},
	   'map-output-mode|output-mode|om=s' => \$saveopts_map{mode},
	   'format-xml|format|fx|f!' => sub { $saveopts_map{format}=$_[1] ? 1 : 0; },
	   'output-file|outfile|out|of|o=s'=> \$outfile,
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- vars
our $mapper = DocClassify::Mapper->new( %mapopts )
  or die("$0: Mapper::new(class=>'$mapopts{class}') failed: $!");

##-- load input corpora
push(@ARGV,'-') if (!@ARGV);
foreach (@ARGV) {
  print STDERR "$prog: loadCorpus($_)\n" if ($verbose);
  my $corpus = DocClassify::Corpus->new(%corpusopts)->loadFile($_,%loadopts_corpus)
    or die("$0: Corpus::loadFile() failed for '$_': $!");
  $mapper->trainCorpus($corpus)
    or die("$0: Mapper::trainCorpus() failed for '$_': $!");
}

if ($compileMap) {
  print STDERR "$prog: compile()\n" if ($verbose);
  $mapper->compile()
    or die("$0: Mapper::compile() failed, class=$mapopts{class}: $!");
  $mapper->clearTrainingCache();
}

print STDERR "$prog: saveFile($outfile)\n" if ($verbose);
$mapper->saveFile($outfile, %saveopts_map);

=pod

=head1 NAME

dc-mapper-train.perl - train DocClassify::Mapper subclass object

=head1 SYNOPSIS

 dc-mapper-train.perl [OPTIONS] [CORPUS...]

 General Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level

 Mapper Options:
  -mapper-class CLASS    # set mapper class (default='LSI')
  -label LABEL           # set global mapper label
  -lemma OPT=VALUE       # set lemmatization option
  -min-freq FREQ         # set minimum global lemma frequency (default=0)
  -smooth-freq FREQ      # set global smoothing frequency (default=1)
  -svd-dims DIMS         # set max SVD dimensions (default=64)
  -cat-profile CP_HOW    # one of 'fold-in', 'average', 'weighted-average' (default='average')
  -term-weight TW_HOW    # one of 'uniform', 'entropy' (default='entropy')
  -exclusive , -nox      # do/don't use only best category for each doc (default=do)
  -compile   , -noc      # do/don't compile mapper after training (default=do)
  -mapper-option OPT=VAL # set generic (class-specific) mapper option

 I/O Options:
  -input-mode MODE       # I/O mode for input corpora (default=guess)
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
