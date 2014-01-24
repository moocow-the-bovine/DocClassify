#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Eval;
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

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;

$opts{corpusSave}{saveCats} = 0;
%{$opts{evalNew}}  = qw();
%{$opts{evalSave}} = ( nErrors=>10, saveDocs=>1, counts=>0, cats=>0 );
$opts{verbosse} = 2;
$opts{log}{level} = 'WARN';

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(
	   ##-- default options
	   dcOptions,

	   ##-- local options
	   'n-errors|nerrors|nerrs|ne=i' => \$opts{evalSave}{nErrors},
	   'counts|count|cnt!' => \$opts{evalSave}{counts},
	  );

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});

##-- copy options ('-cats' option)
$opts{evalSave}{cats} = $opts{corpusSave}{saveCats};


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- out file
our $outfile = $opts{outputFile};
our $outfh = IO::File->new(">$outfile")
  or die("$0: open failed for output file '$outfile': $!");

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
foreach (0..$#ARGV) {
  my $infile = $ARGV[$_];
  my $eval = DocClassify::Eval->new(optsNew('eval'))->loadFile($infile,optsLoad('eval'))
    or die("$0: Eval->loadFile() failed for '$infile': $!");
  $eval->{label} = $infile;
  $eval->saveTextFile($outfh,optsSave('eval'));
  $outfh->print("\n") if ($_ < $#ARGV);
}

$outfh->close();


=pod

=head1 NAME

dc-eval-info.perl - get human-readable output from DocClassify::Eval files

=head1 SYNOPSIS

 dc-eval-info.perl [OPTIONS] [EVAL_FILE(s)...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -n-errors NERRS        # report top NERRS error types (default=10)
  -output-file FILE      # all output to a single file

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
