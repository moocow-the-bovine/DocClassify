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
   opts => [qw(%opts)],
   specs => [qw(generalOptions fcOptions corpusOptions ioOptions)],
   utils => [qw(setVerboseOptions)],
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

   ##-- Mapper Options
   mapNew => {
	      verbose => 1,
	     },
   mapSave => {},
   mapLoad => {},

   ##-- Signature Options
   sigNew => {},
   sigSave => {verboseIO=>0},
   sigLoad => {verboseIO=>0},
  );

##----------------------------------------------------------------------
## Utilities: Getopt::Long: general

## $level = setVerboseOptions($level)
## $level = setVerboseOptions($level,\%opts)
##  + sets all 'verbose' options in \%opts to $level
sub setVerboseOptions {
  my ($level,$opts) = @_;
  $opts = \%opts if (!$opts);
  $opts->{verbose}=$level;
  $_->{verbose}=$level foreach (grep {UNIVERSAL::isa($_,'HASH') && exists($_->{verbose})} values(%$opts));
  return $level;
}

## %specs = generalOptions()
##  + Getopt::Long specs for general values
sub generalOptions {
  (
   'help|h' => \$opts{help},
   'verbose|v=i' => sub { setVerboseOptions($_[1]); },
  );
}

##----------------------------------------------------------------------
## Utilities: Getopt::Long: I/O

## %specs = ioOptions()
##  + Getopt::Long spec for I/O
sub ioOptions {
  (
   #'output-dir|outdir|odir|od|d=s'=> \$opts{outputDir},
   'output-file|outfile|out|of|o=s'=> \$opts{outputFile},
   'input-mode|im=s' => \$opts{load}{mode},
   'output-mode|om=s' => \$opts{save}{mode},
   'format-xml|format|fx|f!' => sub { $opts{save}{format}=$_[1] ? 1 : 0; },
  );
}

##----------------------------------------------------------------------
## Utilities: Getopt::Long: FileChurner

## %specs = fcOptions()
##  + Getopt::Long spec for FileChurner
sub fcOptions {
  (
   'recursive|recurse|r!' => \$opts{fcNew}{recursive},
  );
}

##----------------------------------------------------------------------
## Utilities: Getopt::Long: Corpora


## %specs = corpusOptions()
##  + Getopt::Long spec for corpus I/O
sub corpusOptions {
  (
   'label|l=s' => \$opts{corpusNew}{label},
   'categories|cats|cat!' => \$opts{corpusSave}{saveCats},
   'signatures|sigs|sig!' => \$opts{corpusSave}{saveSigs},
  );
}



##==============================================================================
## Utilities

#(nothing else here; this class is just used as a useful symbolic name)


##==============================================================================
## Footer
1;

__END__
