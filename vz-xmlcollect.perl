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
our $format = 1; ##-- formatted output?

our $input_file_re  = qr/\.real\.xml$/;
our $out_suffix = '.group.xml';
our $out_dir = undef;
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
	   'output-suffix|group-xml-suffix|gxs|gs|os=s' => \$out_suffix,
	   'output-dir|od|d=s' => \$out_dir,
	   #'format|f!' => sub { $format = $_[1] ? 1 : 0; },
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

sub sanitizeString {
  my $s = shift;
  $s =~ s/[\s\&\:\/\\\:\.\,]/_/g;
  $s =~ s/_+/_/g;
  return $s;
}

our (@files);
sub processInputs {
  my @inputs = @_;
  foreach (@inputs) {
    if (-d $_) {
      $out_dir = $_ if (!defined($out_dir));
      if ($recurse) { find({wanted=>\&wanted,follow=>1,no_chdir=>1},$_); }
      else          { push(@files, grep {$_=~$input_file_re} map {glob("$_/*")} grep {-d $_} @inputs); }
    }
    else {
      $out_dir = dirname($_) if (!defined($out_dir));
      push(@files,$_);
    }
  }
  my $cwd = cwd();
  foreach my $file (@files) {
    chdir(dirname($file));
    process_xmlcollect(basename($file),$file);
    chdir($cwd);
  }
}

sub wanted {
  return if ($_ !~ $input_file_re);
  push(@files,$File::Find::name);
}

our ($parser,%groups);
sub process_xmlcollect {
  my ($xmlfile,$fullname) = @_;
  $fullname = $xmlfile if (!defined($fullname));

  ##-- be verbose?
  print STDERR "$0: INFILE: $xmlfile\n" if ($verbose>=2);

  ##-- parse xml document
  my $doc = $parser->parse_file($xmlfile)
    or die("$0: could not parse file '$xmlfile': $!");
  my $root = $doc->documentElement;
  $root->setAttribute('src',$xmlfile);

  ##-- append document data to each assigned (group,degree) pair
  my ($g_name,$g_id,$g_deg, $g,$deg, $gd_docs);
  foreach my $g_node (@{$root->findnodes('head/classification/vat|head/classification/cat')}) {
    ##-- assure groups is defined
    ($g_name,$g_id,$g_deg) = map {$g_node->getAttribute($_)} qw(name id degree);
    $g = $groups{$g_name} = { id=>$g_id,name=>$g_name } if (!defined($g=$groups{$g_name}));
    $g->{deg2docs} = {} if (!defined($g->{deg2docs}));

    foreach $deg ($g_deg..3) {
      $gd_docs = $g->{deg2docs}{$deg} = [] if (!defined($gd_docs=$g->{deg2docs}{$deg}));
      push(@$gd_docs,$doc);
    }
  }

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

##-- write all output groups
print STDERR "$0: output(dir=$out_dir)\n" if ($verbose);
$out_dir = '.' if (!defined($out_dir));
my ($g,$deg, $g_id,$g_name, $gd_docs,$doc, $gd_doc,$gd_root);
my ($outfile);
foreach $g (values(%groups)) {
  ($g_id,$g_name) = @$g{qw(id name)};
  foreach $deg (keys(%{$g->{deg2docs}})) {
    next if (!defined($gd_docs = $g->{deg2docs}{$deg}));
    $outfile = "${out_dir}/".sprintf("%0.2d_%s.d%d%s",$g_id,sanitizeString($g_name),$deg,$out_suffix);
    print STDERR "$0: OUTFILE: $outfile\n" if ($verbose);

    $gd_doc = XML::LibXML::Document->new('1.0','UTF-8');
    $gd_doc->setDocumentElement( $gd_doc->createElement('groups') );
    $gd_root= $gd_doc->documentElement;
    $gd_root->appendChild( $_->documentElement->cloneNode(1) ) foreach (@$gd_docs);
    $gd_doc->toFile($outfile,$format)
      or die("$0: failed to write '$outfile' for group \"$g_id $g_name\", degree $deg: $!");
  }
}

print STDERR "$0: done\n" if ($verbose);

__END__
=pod

=head1 NAME

vz-xmlcollect.perl - collect vz-xml files by group

=head1 SYNOPSIS

 vz-xmlcollect.perl [OPTIONS] [INPUT(s)...]

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

