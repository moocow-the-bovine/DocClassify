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

our $verbose = setVerboseOptions(1);
%opts = (%opts,
	 fcNew => {
		   %{$opts{fcNew}},
		   recursive=>1,
		   inputFileMatch=>qr/\.xml$/,
		  },
	 #corpusSave => { saveCats=>undef, saveSigs=>undef, },
	 ##--
	 #corpusSave => { %{$opts{corpusSave}}, saveCats=>1, saveSigs=>1 },
	 ##--
	 corpusSave => { %{$opts{corpusSave}}, saveCats=>undef, saveSigs=>undef },
	 sigLoad => { %{$opts{sigLoad}}, mode=>undef, lemmatized=>0, verboseIO=>0 },
	 sigSave => { %{$opts{sigSave}}, mode=>undef, lemmatized=>0, verboseIO=>0 },
	 outputFile => '-',
	 sigSuffix => '.sig.bin',
	);


our $inputCorpora = 0; ##-- where INPUTs are corpora or document files/dirs
our $compile = undef;  ##-- default: true for new corpora, false for union

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- common options
	   dcOptions(),

	   ##-- local options
	   'union|merge|join|corpora|u|m|j!' => \$inputCorpora,
	   'compile|c!' => \$compile,
	  );
$verbose=$opts{verbose};
$compile=1 if (!defined($compile) && !$inputCorpora);
$opts{corpusSave}{saveCats}=$opts{corpusSave}{saveSigs}=1 if ($compile);
$opts{fcNew}{outputFile} = $opts{outputFile};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

our ($fc,$corpus);
sub fc_callback {
  my ($infile) = @_;
  if ($inputCorpora) {
    ##-- read corpora
    my $c2 = DocClassify::Corpus->loadFile($infile,newOpts('corpus'))
      or die("$0: loadXmlFile() failed for input corpus '$infile': $!");
    $corpus->addCorpus($c2);
  } else {
    ##-- read documents
    my $doc = DocClassify::Document->new(file=>$infile,newOpts('doc'))
      or die("$0: Document->new() failed for '$infile': $!");
    if ($opts{corpusSave}{saveCats}) {
      $doc->cats();            ##-- parse relevant data
      delete($doc->{xdoc});    ##-- cleanup
    }
    $corpus->addDocument($doc);
  }
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- vars
$corpus = DocClassify::Corpus->new( newOpts('corpus') );

##-- load inputs
push(@ARGV,'-') if (!@ARGV);
$fc = DocClassify::FileChurner->new( newOpts('fc'), fileCallback=>\&fc_callback );
$fc->churn(@ARGV);

##-- compile
if ($compile) {
  DocClassify::Program->vlog('info', "compile()") if ($verbose);
  foreach $doc (@{$corpus->{docs}}) {
    DocClassify::Program->vlog('trace', "COMPILE: ", $doc->label) if ($verbose>=2);

    ##-- compile: categories
    if ($opts{corpusSave}{saveCats}) {
      $doc->cats;
    }

    ##-- compile: signature
    if ($opts{corpusSave}{saveSigs}) {
      my $sigFile = $doc->{file}.$opts{sigSuffix};
      $doc->saveSignature($sigFile, optsSave('sig'));
      delete(@$doc{qw(sig xdoc)}); ##-- clear cache, in case of binary save
    }
  }
}

$corpus->saveFile($opts{outputFile}, saveOpts('corpus'));

=pod

=head1 NAME

dc-corpus-create.perl - make a corpus directory (XML or binary)

=head1 SYNOPSIS

 dc-corpus-create.perl [OPTIONS] [INPUT(s)...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -union CORPUS          # load (additional) corpus data from CORPUS (multiples ok)
  -output-file FILE      # set corpus output file (default=-)
  -label LABEL           # set global corpus label
  -dclass CLASS          # set input document class
  -compile , -nocompile  # do/don't (re-)compile corpus (default: false if -union, otherwise true)
  -cats , -nocats        # do/don't save category data (default: true iff -compile)
  -sigs , -nosigs        # do/don't save signature data (default: true iff -compile)
  -input-mode MODE       # I/O mode for input corpora (default=guess)
  -output-mode MODE      # I/O mode for output corpus (default=guess or xml)

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
