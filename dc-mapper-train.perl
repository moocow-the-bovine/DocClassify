#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Mapper::Train;
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

	 #corpusLoad=>{optsLoad('corpus'),verboseIO=>0},
	 #corpusSave=>{optsSave('corpus'),verboseIO=>0},
	);

our $compileMap = 1; ##-- whether to compile map before saving

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- common options
	   dcOptions(),

	   ##-- local options
	   'compile|c!' => \$compileMap,
	  );
$verbose = $opts{corpusLoad}{verboseIO} = $opts{mapSave}{verboseIO} = $opts{verbose};
our $outfile = $opts{outputFile};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- logger
DocClassify::Logger->ensureLog;

##-- vars
our %mapopts = optsNew('map');
our $mapper = DocClassify::Mapper->new( %mapopts )
  or die("$0: Mapper::new(class=>'$mapopts{class}') failed: $!");
our $logger = 'DocClassify::Program';

##-- load input corpora
push(@ARGV,'-') if (!@ARGV);
foreach (@ARGV) {
  $logger->info("loadCorpus($_)") if ($verbose);
  my $corpus = DocClassify::Corpus->new(optsNew('corpus'))->loadFile($_,optsLoad('corpus'))
    or die("$0: Corpus::loadFile() failed for '$_': $!");

  $logger->info("trainCorpus()") if ($verbose);
  $mapper->trainCorpus($corpus)
    or die("$0: Mapper::trainCorpus() failed for '$_': $!");
}

if ($compileMap) {
  $logger->info("compile()") if ($verbose);
  $mapper->compile()
    or die("$0: Mapper::compile() failed, class=$mapopts{class}: $!");

  $logger->info("clearTrainingCache()") if ($verbose);
  $mapper->clearTrainingCache() if ($mapopts{clearCache});
}

$mapper->saveFile($outfile, optsSave('map'));

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
  -null-cat              # set target cat for null prototype (default='(auto)' -> cat with min id)
  -no-null-cat           # disable null cat prototype (alias for -null-cat='')
  -lz-class LZ_CLASS     # set lemmatizer subclass (default='default')
  -lz-option OPT=VALUE   # set lemmatizer option OPT to VAL (default=none)
  -max-tpd NTERMS        # set maximum #/terms per doc (default=0 [no limit])
  -min-freq FREQ         # set minimum global lemma frequency (default=0)
  -min-docs NDOCS        # set minimum "document frequency" (num docs) (default=0)
  -smooth-freq FREQ      # set global smoothing frequency (default=1)
  -svd-dims DIMS         # set max SVD dimensions (default=512)
  -cat-profile CP_HOW    # one of 'fold-in', 'average', 'weighted-average' (default='average')
  -term-weight TW_HOW    # one of 'uniform', 'entropy' (default='entropy')
  -xcheck-n XN           # set number of cross-check splits for param-fitting (default=0) [SLOW AND GOOFY!]
  -exclusive , -nox      # do/don't use only best category for each doc (default=do)
  -compile   , -noc      # do/don't compile mapper after training (default=do)
  -clear     , -noclear  # do/don't clear training cache before saving (default=do)
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=cut
