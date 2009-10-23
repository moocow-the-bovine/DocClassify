#!/usr/bin/perl -w

use XML::LibXML;
use Getopt::Long qw(:config no_ignore_case);
use Encode qw(encode decode);
use File::Basename qw(basename dirname);
use File::Find;
use IO::File;
use Cwd;
use Pod::Usage;

use strict;

##------------------------------------------------------------------------------
## Constants & Globals
##------------------------------------------------------------------------------
our $prog = basename($0);
our $verbose=2;
our ($help);

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $gdir   = 'bygroup.d';
our $format = 1; ##-- formatted output?

our $input_file_re  = qr/\.xml$/;
our $recurse = 1;

our $outfile = '-';

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,
	   'quiet|q' => sub { $verbose=0; },

	   ##-- I/O
	   'recursive|recurse|r' => \$recurse,
	   'input-file-re|ifr|if|ir=s' => \$input_file_re,
	   'output-file|of|o=s' => \$outfile,
	   #'format|f!' => sub { $format = $_[1] ? 1 : 0; },
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

our (@files);
sub processInputs {
  my @inputs = @_;
  foreach (@inputs) {
    if    (-d $_ && $recurse) { find({wanted=>\&wanted_xml2catmap,follow=>1,no_chdir=>1},$_); }
    elsif (-d $_) { push(@files, grep {$_=~$input_file_re} map {glob("$_/*")} grep {-d $_} @inputs); }
    else { push(@files,$_); }
  }
  my $cwd = cwd();
  foreach my $file (@files) {
    chdir(dirname($file));
    process_xml2catmap(basename($file),$file);
    chdir($cwd);
  }
}

sub wanted_xml2catmap {
  return if ($_ !~ $input_file_re);
  push(@files,$File::Find::name);
}

our ($parser,$outfh);
sub process_xml2catmap {
  my ($xmlfile,$fullname) = @_;
  $fullname = $xmlfile if (!defined($fullname));

  ##-- be verbose
  #print STDERR "$0: FILE: $xmlfile\n" if ($verbose>=2);

  ##-- parse document
  my $doc = $parser->parse_file($xmlfile)
    or die("$0: could not parse file '$xmlfile': $!");
  my $root = $doc->documentElement;

  ##-- get & write classifications
  my $xmlbase = $fullname;
  $outfh->print(
		map {
		  join("\t",
		       $xmlbase,
		       ($_->getAttribute('degree')||'0'),
		       $_->getAttribute('id'),
		       $_->getAttribute('name'),
		      )."\n"
		    }
		@{$doc->findnodes('/*/head/classification/cat|/*/head/classification/vat')}
	       );

  return;
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- setup XML::LibXML parser
$parser = XML::LibXML->new();
$parser->keep_blanks(0); ##-- ignore "ignorable whitespace"
#$parser->keep_blanks(1); ##-- keep all whitespace
$parser->line_numbers(1);
$parser->load_ext_dtd(1);
$parser->validation(0);
$parser->recover(1);
$parser->expand_entities(1);

##-- guts
$outfh = IO::File->new(">$outfile")
  or die("$0: open failed for output file '$outfile': $!");
$outfh->binmode(':utf8');
$outfh->autoflush(1);

processInputs(@ARGV);

$outfh->close();

__END__
=pod

=head1 NAME

vz-xml2dcmap.perl - get document-class mapping from vz xml corpus file(s)

=head1 SYNOPSIS

 vz-xml2dcmap.perl [OPTIONS] [INPUT(s)...]

 General Options:
  -help                     # this help message
  -verbose , -noverbose     # do/don't be verbose (default=-verbose)

  -recurse , -norecurse     # do/don't recurse into directories (default=do)
  -input-file-re REGEX      # default='\.xml$', only used for directory INPUT(s)
  -output OUTFILE           # output CSV file [XMLBASE DEGREE CAT_ID CAT_NAME] (default=-)

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

