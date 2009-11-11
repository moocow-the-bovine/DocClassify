## -*- Mode: CPerl -*-
## File: DocClassify::FileChurner.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: file churner


package DocClassify::FileChurner;
use DocClassify::Object;
use DocClassify::Logger;
use DocClassify::Utils ':all';
use File::Basename qw(basename dirname);
use File::Find;
use Cwd;
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Object DocClassify::Logger);

##==============================================================================
## Constructors etc.

## $fc = $CLASS_OR_OBJ->new(%opts)
## %$fc, %opts:
##  ##-- options
##  verbose => $level,         ##-- verbosity level (default=2)
##  label => $label,           ##-- label for verbose messages (default=basename($0))
##  recursive => $bool,        ##-- recursively churn directories? (default=true)
##  inputFileMatch => $re,     ##-- only churn files matching $re (default=/./)
##  inputFileTrim => $re,      ##-- regex to trim from input files (default=/\.[^\.]*$/) : for in2out()
##  outputFileSuffix => $suf,  ##-- suffix for output files (default='.out')             : for in2out()
##  outputFile => $filename,   ##-- all output to a single file (default=undef)          : for in2out()
##  outputFh   => $fh,         ##-- fh for output file                                   : for in2out
##  fileCallback => \&cb,      ##-- file callback, called as cb($infile,\@args) : REQUIRED
##  fileCallbackData => $data, ##-- user data for callback (default=undef)
##  ##
##  ##-- other data
##  #inputs => \@INPUTS,        ##-- file and/or directory list
##  files  => \@files,         ##-- unprocessed files
##  done   => \@files,         ##-- processed files
##  #...
sub new {
  my $that = shift;
  my $fc = $that->SUPER::new(
			     label=>basename($0),
			     verbose=>2,
			     recursive=>1,
			     inputFileMatch=>qr/./,
			     inputFileTrim=>qr/\.[^\.]*$/,
			     outputFileSuffix=>'.out',
			     outputFile=>undef,
			     outputFh=>undef,
			     fileCallback=>undef,
			     fileCallbackData=>undef,
			     files=>[],
			     done=>[],
			     @_,
			    );

  return $fc;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + just returns qw(outputFh)
sub noShadowKeys {
  return qw(outputFh);
}

##==============================================================================
## Methods: File collection

## @files  = $fc->findFiles(@inputs)          ##-- array context
## \@files = $fc->findFiles(@inputs)          ##-- scalar context
##  + collects desired files from @inputs
sub findFiles {
  my ($fc,@inputs) = @_;
  $fc->vlog('info', "findFiles()") if ($fc->{verbose} >= 1);
  foreach (@inputs) {
    if    (-d $_ && $fc->{recursive}) {
      find({wanted=>$fc->wantedSub,follow=>1,no_chdir=>1},$_);
    }
    elsif (-d $_) {
      push(@{$fc->{files}}, grep {$_=~$fc->{inputFileMatch}} map {glob("$_/*")} grep {-d $_} @inputs);
    }
    else {
      push(@{$fc->{files}},$_);
    }
  }
  return wantarray ? @{$fc->{files}} : $fc->{files};
}

## $sub = $fc->wantedSub()
sub wantedSub {
  my $fc = shift;
  my $filere = qr/$fc->{inputFileMatch}/;
  return sub {
    return if ($_ !~ $filere);
    push(@{$fc->{files}},$File::Find::name);
  };
}

##==============================================================================
## Methods: callback application

## @done  = $fc->processFiles()
## \@done = $fc->processFiles()
##  + returns list of newly processed files
sub processFiles {
  my $fc = shift;
  my $cwd = cwd();
  my @files = @{$fc->{files}};
  my ($file);
  my $cb = $fc->{fileCallback};
  confess(ref($fc)."::processFiles(): no fileCallback defined!") if (!defined($cb));
  foreach $file (@files) {
    $fc->vlog('trace', "FILE: $file") if ($fc->{verbose} >= 2);
    #chdir(dirname($file));
    eval { $cb->($file,$file,$fc->{callbackData}); };
    #chdir($cwd);
    if ($@) { $fc->logwarn("in callback: $@"); $@=''; }
  }
  @{$fc->{files}} = qw();
  push(@{$fc->{done}},@files);
  return wantarray ? @files : \@files;
}

## $fc = $fc->churn(@inputs)
##  + wraps findFiles(), processFiles()
sub churn {
  my $fc = shift;
  $fc->findFiles(@_);
  $fc->processFiles();
  return $fc;
}

##==============================================================================
## Methods: Utilities

## ($outfile,$outfh) = $fc->in2out($infile)
sub in2out {
  my ($fc,$infile) = @_;

  ##-- single-file output
  if (defined($fc->{outputFile})) {
    if (!defined($fc->{outputFh})) {
      $fc->{outputFh} = IO::File->new(">$fc->{outputFile}")
	or confess("in2out($infile): open failed for output file '$fc->{outputFile}': $!");
    }
    return @$fc{qw(outputFile outputFh)};
  }

  ##-- multiple-file output
  my $outfile = basename($infile);
  $outfile =~ s/$fc->{inputFileTrim}//;
  $outfile .= $fc->{outputFileSuffix};
  $outfile = dirname($infile).'/'.$outfile;
  my $outfh = IO::File->new(">$outfile")
    or confess("in2out($infile): open failed for output file '$outfile': $!");

  return ($outfile,$outfh);
}

##==============================================================================
## Footer
1;

__END__
