## -*- Mode: CPerl -*-
## File: DocClassify::Program.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: program utilities

package DocClassify::Program;
use DocClassify::Logger;
use DocClassify::Utils ':all';
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Logger Exporter);

our %EXPORT_TAGS =
  (
   opts => [qw(%opts dcOptions dcLogOptions setVerboseOptions setVerbose),
	    qw(newOpts loadOpts saveOpts),
	    qw(optsNew optsLoad optsSave),
	   ],
  );
our @EXPORT_OK = map {@$_} values(%EXPORT_TAGS);
$EXPORT_TAGS{all} = [@EXPORT_OK];
our @EXPORT = qw();

##==============================================================================
## Utilities: Getopt::Long

## %opts
##  + generic options hash
##  + cf. overrides in DocClassify::Mapper::Train.pm
##  + structure:
##    (
##     $globalKey     => $globalVal,       ##-- global options (e.g. 'verbose')
##     load           => \%globalLoadOpts, ##-- generic options for CLASS->load()
##     save           => \%globalSaveOpts, ##-- generic options for CLASS->save()
##     log            => \%globalLogOpts,  ##-- generic logging options for DocClassify::Logger->logInit()
##     "${class}New"  => \%classOptsNew,   ##-- generic options for $class->new()
##     "${class}Load" => \%classOptsLoad,  ##-- generic options for $class->load()
##     "${class}Save" => \%classOptsSave,  ##-- generic options for $class->save()
##    )
##  + $globalKey is e.g.
##     outputFile => $file,
##     outputDir => $dir,
##  + "$class" is a class alias, e.g.:
##     'fc'        ##-- FileChurner
##     'corpus'    ##-- Corpus
##     'doc'       ##-- Document
##     'map'       ##-- Mapper
our %opts =
  (
   ##-- General Options
   verbose => 1,
   help    => 0,

   ##-- I/O options
   outputFile => '-',
   outputDir  => undef,
   load => { mode=>undef, verboseIO=>1 },
   save => { mode=>undef, format=>1, verboseIO=>1 },
   log =>  \%DocClassify::Logger::defaultLogOpts,

   ##-- FileChurner Options
   fcNew => { verbose=>1, recursive=>1 },
   fcSave => {},
   fcLoad => {},

   ##-- Corpus Options
   corpusNew => { label=>'' },
   corpusSave => {},
   corpusLoad => {},

   ##-- Document Options
   docNew => { class=>'default' },

   ##-- Signature Options
   sigNew => {},
   sigSave => {verboseIO=>0},
   sigLoad => {verboseIO=>0},

   ##-- Mapper Options (cf. overrides in DocClassify::Mapper::Train.pm)
   mapNew => {
	      class=>'LSI::KNN',      ##-- mapper class
	      verbose=>1,                 ##-- verbosity level
	      label=>undef,	          ##-- default label
	      lzClass => 'default',       ##-- default lemmatizer class; see DocClassify::Lemmatizer::new()
	      lzOpts=>{},                 ##-- lemmatizer options; see DocClassify::Lemmatizer::LZ_CLASS for detatils
	      maxTermsPerDoc=>0,      ##-- maximum #/terms per doc (0:no max)
	      minFreq =>10,           ##-- minimum global term-frequency f(t) for term-inclusion
	      minDocFreq =>4,         ##-- minimum #/docs with f(t,d)>0 for term-inclusion
	      smoothf =>1,            ##-- smoothing frequency (undef for NTypes/NTokens)
	      trainExclusive=>1,	  ##-- exclusive-mode training?
	      catProfile => 'fold-in',##-- how to do category profiling
	      termWeight => 'uniform',##-- how to do term weighting
	      twRaw => 0,             ##-- tw raw log-freq coefficient
	      twCooked => 1,          ##-- tw estimated weight coefficient
	      dist => 'e',            ##-- PDL::Cluster distance function
	      xn => 0,                    ##-- number of splits for parameter-fitting cross-check
	      seed =>0,    		  ##-- random seed for x-check
	      nullCat => '(auto)',        ##-- null-prototype target category; false for none

	      ##-- program-local options
	      clearCache => 1,
	     },
   mapSave => { mode=>undef },
   mapLoad => { mode=>undef },

   ##-- Cutoff Options (cf. overrides in DocClassify::Mapper::Train.pm)
   cutoffNew => {
		 cut0p => 0.5,               ##-- confidence level for negative-sample cutoff fitting (0.5)
		 cut1p => 0.5,               ##-- confidence level for positive-sample cutoff fitting (0.5)
		 cut1w => 0.5,               ##-- positive weight (0<=$w<=1) for cutoff fitting (0.65)
		 cutval => 100,                    ##-- constant to add if cutoff is exceeded (default=100)
		 cutCat => undef,                  ##-- name of cutoff sink cat (default: cat with id=0 in $lcenum)
		 optimize => 0,                    ##-- number of cutoff optimization iterations (default=none)
                 fitness => 'F',                   ##-- fitness function for cutoff optimization
		},
   cutoffSave => {mode=>undef},
   cutoffLoad => {mode=>undef},

   ##-- Corpus-Split Options
   split => {
	     seed => 0,
	     exclusive => 1,
	     label => undef,
	    },

   ##-- Eval Options
   evalNew => {},
   evalLoad => {},
   evalSave => {format=>1, saveDocs=>1},
  );

##----------------------------------------------------------------------
## Utilities: Getopt::Long: verbosity

## $level = setVerboseOptions($level)
## $level = setVerboseOptions($level,\%opts)
##  + sets all 'verbose' options in \%opts to $level
BEGIN { *setVerbose = \&setVerboseOptions; }
sub setVerboseOptions {
  my ($level,$opts) = @_;
  $opts = \%opts if (!$opts);
  $opts->{verbose}=$level;
  $_->{verbose}=$level foreach (grep {UNIVERSAL::isa($_,'HASH') && exists($_->{verbose})} values(%$opts));
  return $level;
}

##----------------------------------------------------------------------
## Utilities: Getopt::Long: all options

sub dcOptions {
  (
   ##-- General Options
   'help|h' => \$opts{help},
   'verbose|v=i' => sub { setVerboseOptions($_[1]); },

   ##-- I/O Options
   #'output-dir|outdir|odir|od|d=s'=> \$opts{outputDir},
   'output-file|outfile|out|of|o=s'=> \$opts{outputFile},
   'input-mode|im=s' => \$opts{load}{mode},
   'input-option|io=s%' => \$opts{load},
   'output-mode|om=s' => \$opts{save}{mode},
   'output-option|oo=s%' => $opts{save},
   'format-xml|format|fx|f!' => sub { $opts{save}{format}=$_[1] ? 1 : 0; },
   'verbose-io|verboseio|vio!' => sub { $opts{load}{verboseIO}=$opts{save}{verboseIO}=$_[1]; },
   'mmap!' => sub { $opts{load}{mmap}=$_[1]; },

   ##-- FileChurner options
   'recursive|recurse|R' => \$opts{fcNew}{recursive},

   ##-- Labelling Options
   'label|l=s' => \$opts{label},

   ##-- Corpus Options
   'corpus-label|clabel|cl=s' => \$opts{corpusNew}{label},
   'categories|cats|cat!' => \$opts{corpusSave}{saveCats},
   'signatures|sigs|sig!' => \$opts{corpusSave}{saveSigs},
   'corpus-input-mode|cim=s' => \$opts{corpusLoad}{mode},
   'corpus-output-mode|com=s' => \$opts{corpusSave}{mode},

   ##-- Document Options
   'document-class|doc-class|dclass|dc=s' => \$opts{docNew}{class},

   ##-- Signature Options
   #'signature-mode|sig-mode|sigmode|sm=s' => \$opts{sigSave}{mode},
   #'signature-suffix|sig-suffix|sugsuffix|ss=s' => \$opts{sigSuffix},

   ##-- Mapper Options
   'mapper-label|maplabel|ml=s' => \$opts{mapNew}{label},
   'mapper-class|mapclass|class|mapc|mc=s' => \$opts{mapNew}{class},
   'lemmatizer-class|lemma-class|lz-class|lzc|lc=s' => \$opts{mapNew}{lzClass},
   'lemmatizer-option|lemma-option|lz-option|lzo|lo=s' => $opts{mapNew}{lzOpts},
   'max-terms-per-doc|max-tpd|maxtpd|mtpd|tpd=f' => \$opts{mapNew}{maxTermsPerDoc},
   'min-frequency|min-freq|mf=f' => \$opts{mapNew}{minFreq},
   'min-doc-frequency|min-docs|mdf|md=f' => \$opts{mapNew}{minDocFreq},
   'smooth-frequency|smooth-freq|smoothf|sf=f' => \$opts{mapNew}{smoothf},
   'svd-dims|svd-r|svdr|r=i' =>\$opts{mapNew}{svdr},
   'exclusive|x!' => \$opts{mapNew}{trainExclusive},
   'cat-profile|catProfile|profile|cp=s' => \$opts{mapNew}{catProfile},
   'term-weight|termWeight|tw|w=s'       => \$opts{mapNew}{termWeight},
   'term-weight-raw|termWeightRaw|twRaw|twraw|wraw|tw0=s' => \$opts{mapNew}{twRaw},
   'term-weight-cooked|termWeightCooked|twCooked|twcooked|wcooked|tw1=s' => \$opts{mapNew}{twCooked},
   'cross-check-n|xcheck-n|txn|xn=i' => \$opts{mapNew}{xn},
   #'random-seed|seed|rs=i' => \$opts{mapNew}{seed},
   'clear-training-cache|clear-cache|clear!' => \$opts{mapNew}{clearCache},
   'mapper-option|map-option|mapopt|mo=s%' => $opts{mapNew},
   'null-cat|nullcat|null|nc=s' => \$opts{mapNew}{nullCat},
   'no-null-cat|no-nullcat|nonullcat|nonull' => sub { $opts{mapNew}{nullCat}=undef; },
   #'compile|c!' => \$compileMap,
   'mapper-input-mode|map-input-mode|mim=s' => \$opts{mapLoad}{mode},
   'mapper-output-mode|map-output-mode|mom=s' => \$opts{mapSave}{mode},

   ##-- cutoff options
   'cut-negative-p|cnp|cut0p|c0p|c0=f' => \$opts{cutoffNew}{cut0p},
   'cut-positive-p|cpp|cut1p|c1p|c1=f' => \$opts{cutoffNew}{cut1p},
   'cut-positive-weight|cpw|cut1w|c1w|cw=f' => \$opts{cutoffNew}{cut1w},
   'cut-value|cut-add|cut-val|cutval|cv|ca=f' => \$opts{cutoffNew}{cutval},
   'cut-sink-cat|cut-cat|cutcat|ccat|cc=s' => \$opts{cutoffNew}{cutCat},
   'cut-optimize|optimize|co=i' => \$opts{cutoffNew}{optimize},
   'cut-fitness|fitness|cf=s' => \$opts{cutoffNew}{fitness},

   ##-- Corpus-Split Options
   'random-seed|seed|rs=i' => sub { $opts{mapNew}{seed}=$opts{split}{seed}=$_[1]; },
   'random|no-random-seed|noseed' => sub { $opts{mapNew}{seed}=$opts{split}{seed}=undef; },
   'split-label|sl=s' => \$opts{split}{label},

   ##-- Eval Options
   'eval-output-mode|eom=s'   => \$opts{evalSave}{mode},
   'eval-save-docs|edocs|ed!' => \$opts{evalSave}{saveDocs},

   ##-- Logging Options
   dcLogOptions(),
  );
}

sub dcLogOptions {
  return
    (##-- Logging Options
     'log-level|loglevel|ll|L=s'  => sub { $opts{log}{level}=uc($_[1]); },
     'log-config|logconfig|log4perl-config|l4p-config|l4p=s' => \$opts{log}{l4pfile},
     'log-watch|logwatch|watch|lw=i' => \$opts{log}{watch},
     'nolog-watch|nologwatch|nowatch|nolw|now' => sub { $opts{log}{watch}=undef; },
     'log-stderr|stderr|le!' => \$opts{log}{stderr},
     'log-date|logdate|ld!' => \$opts{log}{logdate},
     'log-time|logtime|lt!' => sub { $opts{log}{logtime}=$_[1]; }, #$opts{log}{logdate}=0 if (!$_[1]);
     'log-file|lf=s' => \$opts{log}{file},
     'nolog-file|nolf' => sub { $opts{log}{file}=undef; },
     'log-rotate|rotate|lr!' => \$opts{log}{rotate},
     'log-syslog|syslog|ls!' => \$opts{log}{syslog},
    );
}

## %opts = optsNew($classKey)
BEGIN { *newOpts = \&optsNew; }
sub optsNew {
  my $class = shift;
  return ($class && $opts{"${class}New"} ? (%{$opts{"${class}New"}}) : qw());
}

## %opts = loadOpts($classKey)
BEGIN { *loadOpts = \&optsLoad; }
sub optsLoad {
  my $class = shift;
  return (%{$opts{load}}, ($class && $opts{"${class}Load"} ? (%{$opts{"${class}Load"}}) : qw()));
}

## %opts = saveOpts($classKey)
BEGIN { *saveOpts = \&optsSave; }
sub optsSave {
  my $class = shift;
  return (%{$opts{save}}, ($class && $opts{"${class}Save"} ? (%{$opts{"${class}Save"}}) : qw()));
}



##==============================================================================
## Utilities

#(nothing else here)


##==============================================================================
## Footer
1;

__END__
