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
	 corpusSave => { saveCats=>undef, saveSigs=>undef, },
	);


our $inputCorpora = 0; ##-- where INPUTs are corpora or document files/dirs

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- common options
	   dcOptions(),

	   ##-- local options
	   'union|merge|corpora|u|m|c!' => \$inputCorpora,
	  );
$verbose=$opts{verbose};
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
    my $doc = DocClassify::Document->new(file=>$infile)
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
  -cats , -nocats        # do/don't save category data (default=don't)
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
