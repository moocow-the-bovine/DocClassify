#!/usr/bin/perl -w

use lib qw(. ./MUDL);

use PDL;
use Storable;
use PDL::IO::Storable;

use MUDL;
use PDL::Ngrams;
use PDL::VectorValued;
use PDL::CCS;
use PDL::CCS::Nd;

use IO::File;

use Getopt::Long qw(:config no_ignore_case);
use Encode qw(encode decode);
use File::Basename qw(basename);
use Pod::Usage;

#use strict;

##------------------------------------------------------------------------------
## Constants & Globals
##------------------------------------------------------------------------------
our $prog = basename($0);
our $verbose = 1;
our ($help);


#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;
our $outfile  = '-';


##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,
	   'quiet|q' => sub { $verbose=0; },

	   ##-- I/O
	   #'input-encoding|ie=s'             => \$inputEncoding,
	   #'output-encoding|oe=s'            => \$outputEncoding,
	   'output|o=s'=>\$outfile,
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);
pod2usage({-exitval=>0, -verbose=>0, -msg=>'No input directory specified!'}) if (!@ARGV);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

## $groups_byname = { $g_name => \%g_data, ... }
##  + where %g_data = ( tf=>{$term=>$freq,...} )
our $groups_byname = {};

## undef = profile_csv_file($f)
sub profile_csv_file {
  my $file = shift;
  print STDERR "$0: file=$file\n" if ($verbose);

  my $fh = ref($file) ? $file : IO::File->new("<$file");
  die("$prog: open failed for file '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8');

  my $g_name = <$fh>;
  chomp($g_name);

  my $g_data = $groups_byname->{$g_name};
  $g_data = $groups_byname->{$g_name} = {name=>$g_name} if (!defined($g_data));

  my $tf = defined($g_data->{tf}) ? $g_data->{tf} : ($g_data->{tf}={});

  ##-- parse file data (TAB-separated: TERM FREQ ...)
  my ($line,$term,$freq,$rest);
  while (defined($line=<$fh>)) {
    chomp($line);
    next if ($line =~ /^\s/);
    ($term,$freq,$rest) = split(/\t/,$line,3);
    $tf->{$term} += $freq;
  }
  $fh->close() if (!ref($file));
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
#open(OUT,">$outfile") or die("$0: open failed for '$outfile': $!");

my ($d,$f);
foreach $d (@ARGV) {
  die ("$prog: no such directory: '$d'") if (!(-d $d || -l $d));
  foreach $f (glob("$d/*.csv")) {
    profile_csv_file($f);
  }
}

##-- expand all terms to a MUDL::Enum
print STDERR "$0: enums\n" if ($verbose);
my $tenum = MUDL::Enum->new;
$tenum->addSymbol($_) foreach (map {keys(%{$_->{tf}})} values(%$groups_byname));
my $NT = $tenum->size;

##-- expand all groups to a MUDL::Enum
my $genum = MUDL::Enum->new;
$genum->addSymbol($_) foreach (keys(%$groups_byname));
$groups_byname->{$_}{id} = $genum->{sym2id}{$_} foreach (keys(%$groups_byname));
my $NG = $genum->size;

##-- get a large frequency matrix: $gtf : [$gid,$tid] => f($gid,$tid)
my ($gtf_w,$gtf_nz) = (null,null);
my ($g_id,$gf_w,$gf_nz);
print STDERR "$0: matrix\n" if ($verbose);
foreach $g_data (values %$groups_byname) {
  next if (!defined($g_data->{tf}));
  $g_id = $g_data->{id};
  $gf_w = (zeroes(long,1,1)+$g_id)->glue(0, pdl(long, @{$tenum->{sym2id}}{keys(%{$g_data->{tf}})})->slice("*1,"));
  $gf_nz= pdl(double, values(%{$g_data->{tf}}));

  $gtf_w = $gtf_w->glue(1,$gf_w);
  $gtf_nz = $gtf_nz->append($gf_nz);

  delete($g_data->{tf}); ##-- frequency data all used up
}
my $gtf = PDL::CCS::Nd->newFromWhich($gtf_w,$gtf_nz,dims=>pdl(long,[$NG,$NT]),missing=>0);

##-- $groups: { genum=>$genum, tenum=>$tenum, gtf=>$gtf_ccs, byname=>\%groups_byname, byid=>\@groups_byid }
## + each $g_data in values(%groups_byname) has 'tfp' key: PDL::CCS::Nd frequency-pdl
our $groups = { genum=>$genum, tenum=>$tenum, gtf=>$gtf, byname=>$groups_byname, byid=>[] };
$groups->{byid}[$_->{id}] = $_ foreach (values(%$groups_byname));

##-- output
if ($outfile eq '-') {
  Storable::store_fd($groups,\*STDOUT)
      or die("$prog: Storable::store_fd() failed to STDOUT: $!");
} else {
  Storable::store($groups,$outfile)
      or die("$prog: Storable::store() failed for '$outfile': $!");
}

__END__
=pod

=head1 NAME

profile-dir.perl - recursive profile all files in a directory

=head1 SYNOPSIS

 profile-dir.perl [OPTIONS] [DIR...]

 General Options:
  -help                  # this help message
  -verbose LEVEL         # set verbosity level (default=1)
  -quiet                 # alias for -verbose=0

 I/O Options:
  -output FILE           # specify output file (default='-' (STDOUT))

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
