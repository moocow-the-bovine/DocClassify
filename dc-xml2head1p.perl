#!/usr/bin/perl -w

use lib ('.','./MUDL');
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
	       inputFileMatch=>qr/(?:\.clean|\.real)\.xml$/,
	       inputFileTrim=>qr/\.[^\.]*$/,
	       outputFile=>undef,
	       outputFileSuffix=>'.head1p.xml',
	      );

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$fcopts{verbose},

	   ##-- I/O
	   'recursive|recurse|r!' => \$fcopts{recursive},
	   'output-file|outfile|out|of|o=s'=> \$fcopts{outputFile},
	   'output-suffix|os=s' => \$fcopts{outputFileSuffix},
	   #'format|f=1' => \$format
	  );
$verbose=$fcopts{verbose};


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

our ($fc);
our $nfiles = 0;
sub fc_callback {
  my ($xmlfile) = @_;
  my $doc = DocClassify::Document->new(file=>$xmlfile);
  my ($outfile,$outfh) = $fc->in2out($xmlfile);
  my $xdoc1 = $doc->xmlDoc;
  my $root1 = $xdoc1->documentElement;
  my $xdoc2 = XML::LibXML::Document->new("1.0","UTF-8");
  my $root2 = $root1->cloneNode(0);
  $xdoc2->setDocumentElement($root2);

  ##-- copy (full): head
  $root2->addChild($_->cloneNode(1)) foreach (@{$root1->findnodes('head')});

  ##-- copy (partial): initial thread
  my $body2 = $root2->addNewChild(undef,'body');
  foreach $tnode1 (@{$root1->findnodes('body/thread[1]')}) {
    my $tnode2 = $tnode1->cloneNode(0);
    $body2->addChild($tnode2);
    $tnode2->addChild($_->cloneNode(1)) foreach (@{$tnode1->findnodes('*[name()!="posts"]')});
    my $posts2 = $tnode2->addNewChild(undef,'posts');
    foreach (@{$tnode1->findnodes('posts[1]/post[1]')}) {
      ##-- copy (full): initial post
      $posts2->addChild($_->cloneNode(1));
    }
  }

  ##-- save
  if (!defined($fc->{outputFile})) {
    $outfh->close() ;
    $xdoc2->toFile($outfile, 1);
  } else {
    $xdoc2->toFH($outfh, 1);
  }
  ++$nfiles;
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
$fc = DocClassify::FileChurner->new( %fcopts, fileCallback=>\&fc_callback );
$fc->churn(@ARGV);

print STDERR "$0: processed $nfiles files.\n" if ($verbose);

=pod

=head1 NAME

dc-xml2head.perl - extract header data from DocClassify xml files

=head1 SYNOPSIS

 dc-xml2head.perl [OPTIONS] [INPUT(s)...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -output-file FILE      # all output to a single file
  -output-suffix SUFFIX  # one outfile per infile, suffix SUFFIX (default=.head.xml)

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
