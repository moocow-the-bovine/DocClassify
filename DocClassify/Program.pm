## -*- Mode: CPerl -*-
## File: DocClassify::Program.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
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
   opts => [qw(%opts dcOptions setVerboseOptions setVerbose),
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
##  + structure:

##    (
##     $globalKey     => $globalVal,       ##-- global options (e.g. 'verbose')
##     load           => \%globalLoadOpts, ##-- generic options for CLASS->load()
##     save           => \%globalSaveOpts, ##-- generic options for CLASS->save()
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

   ##-- FileChurner Options
   fcNew => { verbose=>1, recursive=>1 },
   fcSave => {},
   fcLoad => {},

   ##-- Corpus Options
   corpusNew => { label=>'' },
   corpusSave => {},
   corpusLoad => {},

   ##-- Signature Options
   sigNew => {},
   sigSave => {verboseIO=>0},
   sigLoad => {verboseIO=>0},

   ##-- Mapper Options
   mapNew => {
	      class=>'LSI',	          ##-- mapper class
	      verbose=>1,                 ##-- verbosity level
	      label=>undef,	          ##-- default label
	      lzClass => 'default',       ##-- default lemmatizer class; see DocClassify::Lemmatizer::new()
	      lzOpts=>{},                 ##-- lemmatizer options; see DocClassify::Lemmatizer::LZ_CLASS for detatils
	      svdr => 256,                ##-- svd dimensions (see DocClassify::Mapper::LSI defaults)
	      maxTermsPerDoc=>0,          ##-- maximum #/terms per doc
	      minFreq =>0,                ##-- minimum global term-frequency f(t) for term-inclusion
	      minDocFreq =>0,             ##-- minimum #/docs with f(t,d)>0 for term-inclusion
	      smoothf =>1+1e-5,           ##-- smoothing frequency (undef for NTypes/NTokens)
	      trainExclusive=>1,	  ##-- exclusive-mode training?
	      catProfile => 'average',    ##-- how to do category profiling
	      termWeight => 'entropy',    ##-- how to do term weighting
	      xn => 0,                    ##-- number of splits for parameter-fitting cross-check
	      seed =>0,    		  ##-- random seed for x-check

	      ##-- program-local options
	      clearCache => 1,
	     },
   mapSave => { mode=>undef },
   mapLoad => { mode=>undef },

   ##-- Cutoff Options
   cutoffNew => {
		 cut0p => 0.5,                     ##-- confidence level for negative-sample cutoff fitting (0.5)
		 cut1p => 0.5,                     ##-- confidence level for positive-sample cutoff fitting (0.5)
		 cut1w => 0.5,                     ##-- positive weight (0<=$w<=1) for cutoff fitting (0.5)
		 cutval => 100,                    ##-- constant to add if cutoff is exceeded (default=100)
		 cutCat => undef,                  ##-- name of cutoff sink cat (default: cat with id=0 in $lcenum)
		},
   cutoffSave => {mode=>undef},
   cutoffLoad => {mode=>undef},

   ##-- Corpus-Split Options
   split => {
	     seed => undef,
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
   'output-mode|om=s' => \$opts{save}{mode},
   'format-xml|format|fx|f!' => sub { $opts{save}{format}=$_[1] ? 1 : 0; },

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
   'cross-check-n|xcheck-n|txn|xn=i' => \$opts{mapNew}{xn},
   #'random-seed|seed|rs=i' => \$opts{mapNew}{seed},
   'clear-training-cache|clear-cache|clear!' => \$opts{mapNew}{clearCache},
   'mapper-option|map-option|mapopt|mo=s%' => $opts{mapNew},
   #'compile|c!' => \$compileMap,
   'mapper-input-mode|mim=s' => \$opts{mapLoad}{mode},
   'mapper-output-mode|mom=s' => \$opts{mapSave}{mode},

   ##-- Corpus-Split Options
   'random-seed|seed|rs=i' => sub { $opts{mapNew}{seed}=$opts{split}{seed}=$_[1]; },
   'split-label|sl=s' => \$opts{split}{label},

   ##-- Eval Options
   'eval-output-mode|eom=s'   => \$opts{evalSave}{mode},
   'eval-save-docs|edocs|ed!' => \$opts{evalSave}{saveDocs},
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
