#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Eval;

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
our ($help);
our $verbose = 2;

our $verbose_io=1;

our %evalopts = qw();
our %loadopts_eval = (verboseIO=>$verbose_io, nocompile=>1);
our %saveopts_eval = ( mode=>undef, format=>1, saveDocs=>1, verboseIO=>$verbose_io );

our $outfile = '-';

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- I/O
	   'output-file|outfile|out|of|o=s'=> \$outfile,
	   'eval-output-mode|output-mode|eom|om=s' => \$saveopts_eval{mode},
	   'save-documents|save-docs|docs|d!' => \$saveopts_eval{saveDocs},
	   'format-xml|format|fx|f!' => sub { $saveopts_eval{format}=$_[1] ? 1 : 0; },
	  );
#$verbose=$fcopts{verbose};


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- global eval
our $eval = DocClassify::Eval->new(%evalopts)
  or die("$0: Eval->new() failed: $!");

##-- load evals
push(@ARGV,'-') if (!@ARGV);
foreach my $infile (@ARGV) {
  my $evali = DocClassify::Eval->new(%evalopts)->loadFile($infile,%loadopts_eval)
    or die("$0: Eval->loadFile() failed for '$infile': $!");
  $eval->addEval($evali)
    or die("$0: addEval() failed for '$infile': $!");
}

##-- save
$eval->{label} = $outfile if (!$eval->{label});
$eval->compile(); ##-- ensure compiled
$eval->saveFile($outfile,%saveopts_eval)
  or die("$0: Eval->saveFile() failed for '$outfile': $!");


=pod

=head1 NAME

dc-eval-collect.perl - collect multiple DocClassify::Eval files

=head1 SYNOPSIS

 dc-eval-collect.perl [OPTIONS] [EVAL_FILE(s)...]

 Options:
  -help                  # this help message
  -verbose=LEVEL         # verbosity level
  -output-file=FILE      # all output to a single file
  -output-mode=MODE      # set eval output mode (default=guess)
  -docs   , -nodocs      # do/don't save document data (default=do)
  -format , -noformat    # do/don't format xml output (default=do)

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
