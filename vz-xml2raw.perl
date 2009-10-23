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
our $out_suffix = '.raw';
our $recurse = 1;

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
	   'output-suffix|raw-suffix|os|rs|s=s' => \$out_suffix,
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
    if    (-d $_ && $recurse) { find({wanted=>\&wanted_xml2raw,follow=>1,no_chdir=>1},$_); }
    elsif (-d $_) { push(@files, grep {$_=~$input_file_re} map {glob("$_/*")} grep {-d $_} @inputs); }
    else { push(@files,$_); }
  }
  my $cwd = cwd();
  foreach my $file (@files) {
    chdir(dirname($file));
    process_xml2raw(basename($file),$file);
    chdir($cwd);
  }
}

sub wanted_xml2raw {
  return if ($_ !~ $input_file_re);
  push(@files,$File::Find::name);
}

our ($parser);
sub process_xml2raw {
  my ($xmlfile,$fullname) = @_;
  $fullname = $xmlfile if (!defined($fullname));

  ##-- be verbose
  print STDERR "$0: FILE: $xmlfile\n" if ($verbose>=2);

  ##-- parse document
  my $doc = $parser->parse_file($xmlfile)
    or die("$0: could not parse file '$xmlfile': $!");
  my $root = $doc->documentElement;

  ##-- dump raw text file
  my $outbase = dirname($xmlfile) .'/'. basename($xmlfile,'.xml','.XML');
  my $rawfile = "${outbase}${out_suffix}";
  my $outfh   = IO::File->new(">$rawfile") or die("$0: open failed forraw text file '$rawfile': $!");
  $outfh->binmode(':utf8');

  ##-- dump: raw text
  $outfh->print(map { ("\n\n",$_->textContent,"\n\n") }
		@{$root->findnodes('head/title|head/descrition|head/description')},
		@{$root->findnodes('body/thread/title|body/thread/posts/post/plain')},
	       );
  $outfh->close();

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
processInputs(@ARGV);

print STDERR "$0: done\n" if ($verbose);

__END__
=pod

=head1 NAME

vz-xml2raw.perl - extract raw text from vz-xml files

=head1 SYNOPSIS

 vz-xml2raw.perl [OPTIONS] [INPUT(s)...]

 General Options:
  -help                     # this help message
  -verbose , -noverbose     # do/don't be verbose (default=-verbose)

  -recurse , -norecurse     # do/don't recurse into directories (default=do)
  -input-file-re REGEX      # default='\.xml$', only used for directory INPUT(s)
  -output-suffix SUFFIX     # default='.raw.xml'

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

