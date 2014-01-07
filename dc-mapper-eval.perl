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
our $verbose = 2;
our ($help);

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;

our %corpusopts = qw();
our %evalopts = qw();

our %loadopts_corpus = ( mode=>undef, );
our %saveopts_eval = ( mode=>undef, format=>1, saveDocs=>1 );
our %saveopts_eval_txt = ( nErrors=>10, counts=>0 );
#$opts{log}{level} = 'DEBUG';

our $outfile = '-';

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- Mapping Options (none)

	   ##-- I/O
	   'corpus-input-mode|input-mode|cim|im=s' => \$loadopts_corpus{mode},
	   'eval-output-mode|output-mode|eom|om=s' => \$saveopts_eval{mode},
	   'save-documents|save-docs|docs|d!' => \$saveopts_eval{saveDocs},
	   'format-xml|format|fx|f!' => sub { $saveopts_eval{format}=$_[1] ? 1 : 0; },
	   'n-errors|nerrors|nerrs|ne=i' => \$saveopts_eval_txt{nErrors},
	   'counts|count|cnt!' => \$saveopts_eval_txt{counts},
	   'verbose-io|vio!' => sub {$_->{verboseIO}=$_[1] foreach (\%loadopts_corpus,\%saveopts_eval,\%saveopts_eval);},
	   'output-file|outfile|out|of|o=s'=> \$outfile,

	   ##-- logging
	   dcLogOptions,
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);
pod2usage({-exitval=>0, -verbose=>0, -msg=>'You must specify at least the WANTED corpus!'})
  if (!@ARGV);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- load input corpora
push(@ARGV,'-') if (!@ARGV);
our ($cfile1,$cfile2) = @ARGV;
our $corpus1 = DocClassify::Corpus->new(%corpusopts)->loadFile($cfile1,%loadopts_corpus)
  or die("$0: Corpus->loadFile() failed for '$cfile1': $!");
our $corpus2 = DocClassify::Corpus->new(%corpusopts)->loadFile($cfile2,%loadopts_corpus)
  or die("$0: Corpus->loadFile() failed for '$cfile2': $!");

##-- label input corpora
$corpus1->{label} ||= $cfile1;
$corpus2->{label} ||= $cfile2;

##-- evaluate
our $eval = DocClassify::Eval->new(%evalopts)
  or die("$0: Eval->new() failed: $!");
$eval->compare($corpus1, $corpus2)
  or die("$0: Eval->compare() failed: $!");
$eval->compile()
  or die("$0: Eval->compile() failed: $!");

##-- report: just save
$eval->{label} = $outfile if (!$eval->{label});
$eval->saveFile($outfile, %saveopts_eval)
  or die("$0: Eval->saveFile() failed for '$outfile': $!");

##-- brief report
if ($verbose) {
  #binmode(STDERR,":utf8");
  $eval->saveTextFile(\*STDERR, %saveopts_eval_txt);
}

=pod

=head1 NAME

dc-mapper-eval.perl - evaluate Mapper results

=head1 SYNOPSIS

 dc-mapper-eval.perl [OPTIONS] CORPUS_WANTED CORPUS_GOT

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -docs , -nodocs        # do/don't save full document list (default=do)
  -nerrors NERRS         # summary top NERRS errors to stderr (default=10)
  -input-mode MODE       # I/O mode for input corpora (default=guess)
  -output-mode MODE      # I/O mode for output eval data (default=guess)
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

Bryan Jurish E<lt>jurish@uni-potsdam.deE<gt>

=cut
