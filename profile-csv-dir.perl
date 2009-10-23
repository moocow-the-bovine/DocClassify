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
our $verbose = 2;
our ($help);


#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;
our $outfile  = '-';

our $min_freq = 0;
our $unk_term = '__UNKNOWN__';

our $term_sort = 'none'; ##-- one of; 'string', 'freq', or 'none' (default)
our $catCsvFile = undef;

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,
	   'quiet|q' => sub { $verbose=0; },

	   ##-- misc
	   #'input-encoding|ie=s'             => \$inputEncoding,
	   #'output-encoding|oe=s'            => \$outputEncoding,

	   'category-csv|class-csv|cat-csv|gc|cc=s' => \$catCsvFile,
	   'term-sort|sort|ts=s' => \$term_sort,
	   'min-frequency|min-freq|minf|mf|m=f' => \$min_freq,
	   'output|o=s'=>\$outfile,
	  );


pod2usage({-exitval=>0, -verbose=>0}) if ($help);
pod2usage({-exitval=>0, -verbose=>0, -msg=>'No input directory specified!'}) if (!@ARGV);


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

## $cats_byname = { $c_name => \%g_data, ... }
##  + where %g_data = ( tf=>{$term=>$freq,...} )
our $cats_byname = {};
our $tf_global = {}; ## $term=>$freq

## undef = profile_csv_file($f)
sub profile_csv_file {
  my $file = shift;

  my $fh = ref($file) ? $file : IO::File->new("<$file");
  die("$prog: open failed for file '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8');

  my $c_name = <$fh>;
  chomp($c_name);
  $c_name =~ s/^\d+\s*//;

  ##-- check if we're ignoring this file's group
  if ($NC && !defined($cenum->{sym2id}{$c_name})) {
    print STDERR "> SKIP (ignoring unknown category '$c_name')\n";
    return;
  }

  my $c_data = $cats_byname->{$c_name};
  $c_data = $cats_byname->{$c_name} = {name=>$c_name} if (!defined($c_data));

  my $tf = defined($c_data->{tf}) ? $c_data->{tf} : ($c_data->{tf}={});

  ##-- parse file data (TAB-separated: TERM FREQ ...)
  my ($line,$term,$freq,$rest);
  while (defined($line=<$fh>)) {
    chomp($line);
    next if ($line =~ /^\s/);
    ($term,$freq,$rest) = split(/\t/,$line,3);
    $tf->{$term} += $freq;
    $tf_global->{$term} += $freq;
  }
  $fh->close() if (!ref($file));
}

## $cenum = loadCatCsv($cenum,$catCsvFile)
sub loadCatCsv {
  my ($cenum,$cfile) = @_;
  my $fh = ref($cfile) ? $cfile : IO::File->new("<$cfile");
  die("$0: open failed for category enum file '$cfile': $!") if (!$fh);
  $fh->binmode(':utf8');
  my ($id,$name);
  while (<$fh>) {
    chomp;
    next if (/^\s*$/ || /^\s*\#/);
    ($id,$name) = split(/\t/,$_,2);
    $name =~ s/^\s+//;
    $name =~ s/\s+$//;
    $cenum->addIndexedSymbol($name,$id);
  }
  $fh->close() if (!ref($cfile));
  return $cenum;
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
#open(OUT,">$outfile") or die("$0: open failed for '$outfile': $!");

##-- maybe pre-load category enum
our $cenum = MUDL::Enum->new;
if ($catCsvFile) {
  print STDERR "$0: loadCatCsv($catCsvFile)\n" if ($verbose);
  loadCatCsv($cenum,$catCsvFile);
}
our $NC = $cenum->size;

my ($d,$f);
foreach $d (@ARGV) {
  print STDERR "$0: directory: $d\n" if ($verbose);
  $d =~ s/\/$//;
  die ("$prog: no such directory: '$d'") if (!(-d $d || -l $d));
  foreach $f (glob("$d/*.csv")) {
    print STDERR "$0: file: $f\n" if ($verbose>=2);
    profile_csv_file($f);
  }
}

##-- trim global frequency matrix
print STDERR "$0: trim(min_freq=>$min_freq)\n" if ($verbose);
delete(@$tf_global{grep {$tf_global->{$_}<$min_freq} keys(%$tf_global)}) if ($min_freq>0);

##-- expand all terms to a MUDL::Enum
print STDERR "$0: enums(term_sort=>'$term_sort')\n" if ($verbose);
my $tenum = MUDL::Enum->new;
if ($term_sort =~ /^f/) {
  @{$tenum->{id2sym}} = ($unk_term, sort {$tf_global->{$b} <=> $tf_global->{$a}} keys(%$tf_global)); ##-- freq-sorted
} elsif ($term_sort =~ /^s/) {
  @{$tenum->{id2sym}} = ($unk_term, sort keys(%$tf_global)); ##-- string-sorted
} else {
  @{$tenum->{id2sym}} = ($unk_term, keys(%$tf_global)); ##-- unsorted
}
@{$tenum->{sym2id}}{@{$tenum->{id2sym}}} = (0..$#{$tenum->{id2sym}});
my $NT = $tenum->size;

#-- maybe add all groups to a new category MUDL::Enum
if (!$catCsvFile) {
  $cenum->addSymbol($_) foreach (keys(%$cats_byname));
}
$NC = $cenum->size;
$cats_byname->{$_}{id} = $cenum->{sym2id}{$_} foreach (keys(%$cats_byname));

##-- get a large frequency matrix: $tcf : [$tid,$cid] => f($tid,$cid)
print STDERR "$0: matrix(NT=$NT x NC=$NC)\n" if ($verbose);
my ($tcf_w,$tcf_nz) = (null,null);
my ($c_id,$cf_wt,$cf_w,$cf_nz);
foreach $c_data (values %$cats_byname) {
  next if (!defined($c_data->{tf}));
  $c_id = $c_data->{id};

  $cf_wt = pdl(long, grep {defined($_)} @{$tenum->{sym2id}}{keys(%{$c_data->{tf}})});
  $cf_w  = $cf_wt->slice("*1,")->glue(0, zeroes(long,1,1)+$c_id);
  $cf_nz = pdl(double, @{$c_data->{tf}}{@{$tenum->{id2sym}}[$cf_wt->list]});

  $tcf_w  = $tcf_w->glue(1,$cf_w);
  $tcf_nz = $tcf_nz->append($cf_nz);

  delete($c_data->{tf}); ##-- frequency data all used up
}
my $tcf = PDL::CCS::Nd->newFromWhich($tcf_w,$tcf_nz,dims=>pdl(long,[$NT,$NC]),missing=>0);


##-- $cats: { cenum=>$cenum, tenum=>$tenum, tcf=>$tcf_ccs, byname=>\%groups_byname, byid=>\@groups_byid }
## + each $c_data in values(%groups_byname) has 'tfp' key: PDL::CCS::Nd frequency-pdl
our $cats = { cenum=>$cenum, tenum=>$tenum, tcf=>$tcf, byname=>$cats_byname, byid=>[] };
$cats->{byid}[$_->{id}] = $_ foreach (values(%$cats_byname));

##-- output
print STDERR "$0: save($outfile)\n" if ($verbose);
if ($outfile eq '-') {
  Storable::store_fd($cats,\*STDOUT)
      or die("$prog: Storable::store_fd() failed to STDOUT: $!");
} else {
  Storable::store($cats,$outfile)
      or die("$prog: Storable::store() failed for '$outfile': $!");
}

print STDERR "$0: done.\n" if ($verbose);

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

 Other Options:
  -min-freq FREQ         # minimum global frequency to index (default=1)
  -group-csv CSVFILE     # load group ids from CSVFILE
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
