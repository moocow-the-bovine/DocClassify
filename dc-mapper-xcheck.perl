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

our %corpusopts = (label=>undef);

our %splitopts = (
		  seed => undef,
		  exclusive => 1,
		  label => undef,
		 );

our %mapopts = (
		class=>'LSI',    ##-- mapper class
		label=>undef,    ##-- default label
		lemmatize=>{},   ##-- see $DocClassify::Signature::LEMMA_XYZ variables for defaults
		svdr => 256,     ##-- svd dimensions
		maxTermsPerDoc=>0, ##-- maximum #/terms per doc
		minFreq =>0,     ##-- minimum frequency
		minDocFreq =>0,  ##-- minimum #/docs with f(t,d)>0 for term-inclusion
		smoothf =>1,     ##-- smoothing frequency (undef for NTypes/NTokens)
		trainExclusive=>1, ##-- exclusive-mode training?
		catProfile => 'average',   ##-- how to do category profiling
		termWeight => 'entropy',   ##-- how to do term weighting
		xn => 3, ##-- training-internal cross-checking iterations
	       ),

our %evalopts = qw();

our %loadopts_corpus = ( mode=>undef, );
our %saveopts_corpus = ( mode=>undef, format=>1, );
our %saveopts_eval   = ( mode=>undef, format=>1, saveDocs=>1 );

our $outdir       = 'xcheck.d';  ##-- output dir
our $n_subcorpora = 10;

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- splitting options
	   'seed|s=i' => \$splitopts{seed},

	   ##-- Map Options
	   'n-subcorpora|n=i' => \$n_subcorpora,
	   'mapper-class|mapclass|class|mapc|mc=s' => \$mapopts{class},
	   'label|l=s' => sub { $evalopts{label}=$mapopts{label}=$corpusopts{label}=$_[1]; },
	   'lemmatize-option|lemma-option|lemma|L=s%' => $mapopts{lemmatize},
	   'max-terms-per-doc|max-tpd|maxtpd|mtpd|tpd=f' => \$mapopts{maxTermsPerDoc},
	   'min-frequency|min-freq|mf=f' => \$mapopts{minFreq},
	   'min-doc-frequency|min-docs|mdf|md=f' => \$mapopts{minDocFreq},
	   'smooth-frequency|smooth-freq|smoothf|sf=f' => \$mapopts{smoothf},
	   'svd-dims|svd-r|svdr|r=i' =>\$mapopts{svdr},
	   'exclusive|x!' => sub { $splitopts{exclusive}=$mapopts{trainExclusive}=$_[1]; },
	   'cat-profile|catProfile|profile|cp=s' => \$mapopts{catProfile},
	   'term-weight|termWeight|tw|w=s'       => \$mapopts{termWeight},
	   'train-xcheck-n|txn|xn=i' => \$mapopts{xn},
	   'mapper-option|mo=s' => \%mapopts,

	   ##-- I/O
	   'corpus-input-mode|corpus-imode|input-mode|cim|im=s' => \$loadopts_corpus{mode},
	   'corpus-output-mode|corpus-omode|com=s' => \$saveopts_corpus{mode},
	   'eval-output-mode|output-mode|eom=s'    => \$saveopts_eval{mode},
	   'output-directory|output-dir|outdir|dir|od|o|d=s'=> \$outdir,
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------


##------------------------------------------------------------------------------
##MAIN
##------------------------------------------------------------------------------

##--------------------------------------------------------------
## Initialize: Hacks
#$DocClassify::Mapper::LSI::verbose = 2 if ($DocClassify::Mapper::LSI::verbose > 2);
$mapopts{verbose} = $verbose;

##--------------------------------------------------------------
## Initialize: output directory
if (!-d $outdir) {
  system('mkdir','-p',$outdir)==0
    or die("$0: mkdir -p '$outdir' failed: $!");
}

##--------------------------------------------------------------
## Initialize: mapper (master)
our $mapper0 = DocClassify::Mapper->new( %mapopts )
  or die("$0: Mapper::new(class=>'$mapopts{class}') failed: $!");

##--------------------------------------------------------------
## Initialize: master corpus
push(@ARGV,'-') if (!@ARGV);
our $cfile0  = shift(@ARGV);
print STDERR "$prog: loadMasterCorpus($cfile0)\n" if ($verbose);
our $corpus0 = DocClassify::Corpus->new(%corpusopts)->loadFile($cfile0,%loadopts_corpus)
    or die("$0: Corpus->loadFile() failed for master corpus '$cfile0': $!");
$corpus0->{label} ||= $cfile0;
our $label0 = $corpus0->{label};

##--------------------------------------------------------------
## Pre-compile & cache document signatures
if ($mapper0->can('lemmaSignature')) {
  print STDERR "$prog: lemmaSignatures()\n" if ($verbose);
  foreach (@{$corpus0->{docs}}) {
    print STDERR "$prog: lemmaSignature(".$_->label.")\n" if ($verbose >= 2);
    $_->{sig} = $mapper0->lemmaSignature($_);
  }
}

##--------------------------------------------------------------
## Split master corpus
print STDERR "$prog: splitN(n=>$n_subcorpora)\n" if ($verbose);
our @subcorpora = $corpus0->splitN($n_subcorpora, %splitopts, label=>"$label0.%d");
our ($i);
foreach $i (0..$#subcorpora) {
  print STDERR "$prog: save($outdir/split.$i.xml)\n" if ($verbose >= 2);
  $subcorpora[$i]->{spliti} = $i;
  $subcorpora[$i]->saveFile("$outdir/split.$i.xml");
}

##--------------------------------------------------------------
## Train - Test - Eval loop
print STDERR "$prog: LOOP: TRAIN - MAP - EVAL\n" if ($verbose);
our $eval0 = DocClassify::Eval->new(%evalopts);
foreach $i (0..$#subcorpora) {
  print STDERR "$prog: LOOP (i=$i): CORPORA\n" if ($verbose);
  my $test = $subcorpora[$i]->shadow( %{$subcorpora[$i]}, label=>"TEST($i,$label0)" );
  my $train = $test->shadow( label=>"TRAIN($i,$label0)" );
  $train->addCorpus($_) foreach (@subcorpora[grep {$_ != $i} (0..$#subcorpora)]);

  ##-- save corpora
  $test->saveFile("$outdir/test.$i.xml");
  $train->saveFile("$outdir/train.$i.xml");

  ##-- train mapper
  print STDERR "$prog: LOOP (i=$i): TRAIN\n" if ($verbose);
  my $mapper = $mapper0->clone  or die("$0: Mapper->clone() failed for i=$i: $!");
  $mapper->trainCorpus($train)  or die("$0: Mapper->train() failed for i=$i: $!");
  $mapper->compile() or die("$0: Mapper->compile() failed for i=$i: $!");
  print STDERR "$prog: LOOP (i=$i): CLEAR_CACHE\n" if ($verbose);
  $mapper->clearTrainingCache();

  ##-- apply mapper to test corpus
  print STDERR "$prog: LOOP (i=$i): MAP\n" if ($verbose);
  my $test_mapped = $test->clone;
  $test_mapped->{label} = "MAPPED($i,$label0)";
  $mapper->mapCorpus($test_mapped);
  $test_mapped->saveFile("$outdir/test.$i.mapped.xml");

  ##-- evaluate
  print STDERR "$prog: LOOP (i=$i): EVAL\n" if ($verbose);
  my $eval = $eval0->shadow()->compare($test,$test_mapped)->compile();
  $eval->{label} = "EVAL($i,$label0)";
  $eval->saveFile("$outdir/eval.$i.xml", %saveopts_eval)
    or die("$0: Eval->saveFile($outdir/eval.$i.xml) failed: $!");
  $eval->saveTextFile(\*STDERR) if ($verbose);

  ##-- evaluate: add to global
  $eval0->addEval($eval);
}

##-- evaluate: total
print STDERR "$prog: FINAL: EVAL ($outdir/eval.all.xml)\n" if ($verbose);
@$eval0{qw(label label1 label2)} = ("EVAL($label0)","TEST(i,$label0)","MAPPED(i,$label0)");
$eval0->compile();
$eval0->saveFile("$outdir/eval.all.xml", %saveopts_eval)
  or die("$0: Eval->saveFile($outdir/eval.all.xml) failed: $!");
$eval0->saveFile("$outdir/eval.summary.xml", %saveopts_eval, saveDocs=>0)
  or die("$0: Eval->saveFile($outdir/eval.summary.xml) failed: $!");

$eval0->saveTextFile(\*STDERR) if ($verbose);

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
  -lemma OPT=VALUE       # set lemmatization option
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
