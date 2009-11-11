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

%opts = (%opts,

	 #corpusLoad=>{optsLoad('corpus'),verboseIO=>0},
	 #corpusSave=>{optsSave('corpus'),verboseIO=>0},
	);

our $verbose = setVerbose(2);
our $outdir       = 'xcheck.d';  ##-- output dir
our $n_subcorpora = 10;

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(
	   ##-- common options
	   dcOptions(),

	   ##-- Local: I/O
	   'output-directory|output-dir|outdir|dir|od|d=s'=> \$outdir,
	  );
$verbose = $opts{verbose};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
##MAIN
##------------------------------------------------------------------------------

##--------------------------------------------------------------
## Initialize: Hacks
our $logger = 'DocClassify::Program';

##--------------------------------------------------------------
## Initialize: output directory
if (!-d $outdir) {
  system('mkdir','-p',$outdir)==0
    or die("$0: mkdir -p '$outdir' failed: $!");
}

##--------------------------------------------------------------
## Initialize: mapper (master)
our %mapopts = optsNew('map');
our $mapper0 = DocClassify::Mapper->new( %mapopts )
  or die("$0: Mapper::new(class=>'$mapopts{class}') failed: $!");

##--------------------------------------------------------------
## Initialize: master corpus
push(@ARGV,'-') if (!@ARGV);
our $cfile0  = shift(@ARGV);
$logger->info("loadMasterCorpus($cfile0)") if ($verbose);
our $corpus0 = DocClassify::Corpus->new(optsNew('corpus'))->loadFile($cfile0,optsLoad('corpus'))
    or die("$0: Corpus->loadFile() failed for master corpus '$cfile0': $!");
$corpus0->{label} ||= $cfile0;
our $label0 = $corpus0->{label};

##--------------------------------------------------------------
## Pre-compile & cache document signatures
if ($mapper0->can('lemmaSignature')) {
  $logger->info("lemmaSignatures()") if ($verbose);
  foreach (@{$corpus0->{docs}}) {
    $logger->info("lemmaSignature(".$_->label.")") if ($verbose >= 3);
    $_->{sig} = $mapper0->lemmaSignature($_);
  }
}

##--------------------------------------------------------------
## Split master corpus
$logger->info("splitN(n=>$n_subcorpora)") if ($verbose);
our @subcorpora = $corpus0->splitN($n_subcorpora, %{$opts{split}}, label=>"$label0.%d");
our ($i);
foreach $i (0..$#subcorpora) {
  $logger->info("save($outdir/split.$i.xml)") if ($verbose >= 2);
  $subcorpora[$i]->{spliti} = $i;
  $subcorpora[$i]->saveFile("$outdir/split.$i.xml");
}

##--------------------------------------------------------------
## Train - Test - Eval loop
$logger->info("LOOP: TRAIN - MAP - EVAL") if ($verbose);
our $eval0 = DocClassify::Eval->new(optsNew('eval'));
foreach $i (0..$#subcorpora) {
  $logger->info("LOOP (i=$i): CORPORA") if ($verbose);
  my $test = $subcorpora[$i]->shadow( %{$subcorpora[$i]}, label=>"TEST($i,$label0)" );
  my $train = $test->shadow( label=>"TRAIN($i,$label0)" );
  $train->addCorpus($_) foreach (@subcorpora[grep {$_ != $i} (0..$#subcorpora)]);

  ##-- save corpora
  $test->saveFile("$outdir/test.$i.xml");
  $train->saveFile("$outdir/train.$i.xml");

  ##-- train mapper
  $logger->info("LOOP (i=$i): TRAIN") if ($verbose);
  my $mapper = $mapper0->clone  or die("$0: Mapper->clone() failed for i=$i: $!");
  $mapper->trainCorpus($train)  or die("$0: Mapper->train() failed for i=$i: $!");
  $mapper->compile() or die("$0: Mapper->compile() failed for i=$i: $!");
  $logger->info("LOOP (i=$i): CLEAR_CACHE") if ($verbose);
  $mapper->clearTrainingCache();

  ##-- apply mapper to test corpus
  $logger->info("LOOP (i=$i): MAP") if ($verbose);
  my $test_mapped = $test->clone;
  $test_mapped->{label} = "MAPPED($i,$label0)";
  $mapper->mapCorpus($test_mapped);
  $test_mapped->saveFile("$outdir/test.$i.mapped.xml");

  ##-- evaluate
  $logger->info("LOOP (i=$i): EVAL") if ($verbose);
  my $eval = $eval0->shadow()->compare($test,$test_mapped)->compile();
  $eval->{label} = "EVAL($i,$label0)";
  $eval->saveFile("$outdir/eval.$i.xml", optsSave('eval'))
    or die("$0: Eval->saveFile($outdir/eval.$i.xml) failed: $!");
  $eval->saveTextFile(\*STDERR) if ($verbose);

  ##-- evaluate: add to global
  $eval0->addEval($eval);
}

##-- evaluate: total
$logger->info("FINAL: EVAL ($outdir/eval.all.xml)") if ($verbose);
@$eval0{qw(label label1 label2)} = ("EVAL($label0)","TEST(i,$label0)","MAPPED(i,$label0)");
$eval0->compile();
$eval0->saveFile("$outdir/eval.all.xml", optsSave('eval'))
  or die("$0: Eval->saveFile($outdir/eval.all.xml) failed: $!");
$eval0->saveFile("$outdir/eval.summary.xml", optsSave('eval'), saveDocs=>0)
  or die("$0: Eval->saveFile($outdir/eval.summary.xml) failed: $!");

$eval0->saveTextFile(\*STDERR,verboseIO=>0) if ($verbose);

=pod

=head1 NAME

dc-mapper-xcheck.perl - cross-validation split, train, map & evaluate in one swell foop

=head1 SYNOPSIS

 dc-mapper-xcheck.perl [OPTIONS] [CORPUS...]

 General Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level

 Splitting Options:
  -seed SEED             # set random seed
  -exclusive , -nox      # do/don't split exclusively (also effects mapper)

 Mapper Options:
  -mapper-class CLASS    # set mapper class (default='LSI')
  -label LABEL           # set global mapper label
  -lz-class LZ_CLASS     # set lemmatizer subclass (default='default')
  -lz-option OPT=VALUE   # set lemmatizer option OPT to VAL (default=none)
  -max-tpd NTERMS        # set maximum #/terms per doc (default=0 [no limit])
  -min-freq FREQ         # set minimum global lemma frequency (default=0)
  -min-docs NDOCS        # set minimum "document frequency" (num docs) (default=0)
  -smooth-freq FREQ      # set global smoothing frequency (default=1)
  -svd-dims DIMS         # set max SVD dimensions (default=256)
  -cat-profile CP_HOW    # one of 'fold-in', 'average', 'weighted-average' (default='average')
  -term-weight TW_HOW    # one of 'uniform', 'entropy' (default='entropy')
  -exclusive , -nox      # do/don't use only best category for each doc (default=do)
  -compile   , -noc      # do/don't compile mapper after training (default=do)
  -txn N                 # number of training-internal cross-check iterations (default=3)
  -mapper-option OPT=VAL # set generic (class-specific) mapper option

 I/O Options:
  -input-imode MODE      # I/O mode for input corpora (default=guess)
  -corpus-omode MODE     # I/O mode for output corpora (default=guess)
  -eval-omode MODE       # I/O mode for eval results (default=guess)
  -output-dir DIR        # directory for output files (default='xcheck.d')

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
