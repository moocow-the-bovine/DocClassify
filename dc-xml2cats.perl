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
	       verbose=>1,
	       recursive=>1,
	       inputFileMatch=>qr/\.xml$/,
	       inputFileTrim=>qr/\.[^\.]*$/,
	       outputFile=>'-',
	       #outputFileSuffix=>'.csv',
	      );

our $catNames = 1;
our $catIds = 1;

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$fcopts{verbose},

	   ##-- I/O
	   'recursive|recurse|r!' => \$fcopts{recursive},
	   'output-file|outfile|out|of|o=s'=> \$fcopts{outputFile},
	   'names|name|n!' => \$catNames,
	   'ids|i!' => \$catIds,
	   #'output-suffix|os=s' => \$fcopts{outputFileSuffix},
	   #'format|f=1' => \$format
	  );
$verbose=$fcopts{verbose};


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

our ($fc);
our (%cats); ##-- ("$catKey" => undef, ...)
our ($c,%a);
sub cb_main {
  my ($xmlfile) = @_;
  open(XML,"<$xmlfile") or die("$0: open failed for '$xmlfile': $!");
  while (<XML>) {
    next if (!/\<[cv]at\s/);
    chomp;
    s/(?:^\s+)|(?:\s+$)//;
    ($c,%a)=split(/(?:\"?\s)|(?:\=\")|(?:\"?\s*\/>)/);
    $c = join("\t", ($catIds ? ($a{id}||'?') : qw()), ($catNames ? ($a{name}||'?') : qw()));
    $cats{$c} = undef;
  }
  close(XML);
}

sub catcmp {
  my $aid = $a =~ /^(\d+)/ ? $1 : -1;
  my $bid = $b =~ /^(\d+)/ ? $1 : -1;
  return ($aid <=> $bid || $a cmp $b);
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
$fc = DocClassify::FileChurner->new( %fcopts, fileCallback=>\&cb_main );
$fc->churn(@ARGV);

##-- dump
$outfh = IO::File->new(">$fcopts{outputFile}")
  or die("$0: open failed for output file '$fcopts{outputFile}': $!");
$outfh->print(map {"$_\n"} sort catcmp keys(%cats));

=pod

=head1 NAME

dc-xml2cats.perl - get category list for DocClassify xml docs

=head1 SYNOPSIS

 dc-xml2cats.perl [OPTIONS] [INPUT(s)...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -output-file FILE      # specify output file (default=STDOUT)
  -ids , -noids          # do/don't output cat ids (default=do)
  -names , -nonames      # do/don't output cat names (default=do)

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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=cut
