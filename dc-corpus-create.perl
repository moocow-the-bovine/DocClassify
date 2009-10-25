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
our ($help,$verbose);

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;

our %fcopts = (
	       verbose=>2,
	       recursive=>1,
	       inputFileMatch=>qr/\.xml$/,
	       #inputFileTrim=>qr/\.[^\.]*$/,
	       outputFile=>'-',
	       #outputFileSuffix=>'.sig',
	      );

our %corpusOpts = (
		   label=>'',
		  );
our $wantCats = 1;

our $inputCorpora = 0; ##-- where INPUTs are corpora or document files/dirs

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$fcopts{verbose},

	   ##-- Misc
	   'label|l=s' => \$corpusOpts{label},
	   'categories|cats|cat!' => \$wantCats,

	   ##-- I/O
	   'union|merge|corpora|u|m|c!' => \$inputCorpora,
	   'recursive|recurse|r!' => \$fcopts{recursive},
	   'output-file|outfile|out|of|o=s'=> \$fcopts{outputFile},
	   #'output-suffix|os=s' => \$fcopts{outputFileSuffix},
	   #'format|f=1' => \$format
	  );
$verbose=$fcopts{verbose};


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

our ($fc,$corpus);
sub cb_make_corpus {
  my ($infile) = @_;
  if ($inputCorpora) {
    ##-- read corpora
    my $c2 = DocClassify::Corpus->loadXmlFile($infile,%corpusOpts)
      or die("$0: loadXmlFile() failed for input corpus '$infile': $!");
    $corpus->addCorpus($c2);
  } else {
    ##-- read documents
    my $doc = DocClassify::Document->new(file=>$infile)
      or die("$0: Document->new() failed for '$infile': $!");
    if ($wantCats) {
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
$corpus = DocClassify::Corpus->new( %corpusOpts );

##-- load inputs
push(@ARGV,'-') if (!@ARGV);
$fc = DocClassify::FileChurner->new( %fcopts, fileCallback=>\&cb_make_corpus );
$fc->churn(@ARGV);

print STDERR "$prog: save($fcopts{outputFile})\n" if ($verbose);
$corpus->saveXmlFile($fcopts{outputFile}, saveCats=>$wantCats);

=pod

=head1 NAME

dc-corpus-create.perl - make an XML corpus directory

=head1 SYNOPSIS

 dc-corpus-create.perl [OPTIONS] [INPUT(s)...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -input-corpus CORPUS   # load (additional) corpus data from CORPUS (multiples ok)
  -output-file FILE      # all output to a single file
  -label LABEL           # set global corpus label
  -cats , -nocats        # do/don't parse document categories (default=do)

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
