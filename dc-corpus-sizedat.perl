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

our $verbose = setVerboseOptions(2);
%opts = (%opts,
	 outputFile => '-',
	 cats=>1,
	);

our $want_ndocs=1;
our $want_nbytes=0;

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(dcOptions(),

	   ##-- Misc
	   'ndocs|docs|d!' => \$want_ndocs,
           'nbytes|bytes|b!' => \$want_nbytes,
	  );
$verbose=$opts{verbose};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

sub catcmp {
  my ($ai,$bi,$as,$bs) = (0,0,$a,$b);
  ($ai,$as) = ($1,$2) if ($a=~/^(\d+)(.*)$/);
  ($bi,$bs) = ($1,$2) if ($b=~/^(\d+)(.*)$/);
  return ( $ai <=> $bi || $as cmp $bs);
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

our $outfile = $opts{outputFile};
our $outfh = IO::File->new(">$outfile")
  or die("$0: open failed for output file '$outfile': $!");


##-- ye olde guttes
push(@ARGV,'-') if (!@ARGV);
foreach my $cfile (@ARGV) {
  our $corpus = DocClassify::Corpus->loadFile($cfile)
    or die("$0: load failed for corpus file '$cfile': $!");

  my $c2doc = $corpus->docsByCat();
  my $ndocs = scalar(@{$corpus->{docs}});
  my %c2ndocs = (map {$_=>scalar(@{$c2doc->{$_}})} keys(%$c2doc));
  my $ncats = scalar(keys(%c2ndocs));

  my ($nbytes,%c2nbytes);
  if ($want_nbytes) {
   $nbytes = 0;
   $nbytes += $_->sizeBytes foreach (@{$corpus->{docs}});
   %c2nbytes = (map {$_=>0} keys(%$c2doc));
   foreach my $c (keys(%c2nbytes)) {
     $c2nbytes{$c} += $_->sizeBytes foreach (@{$c2doc->{$c}});
   }
  }

  my %c2id = qw();
  my @id2c = qw();
  foreach (keys(%c2ndocs)) {
    $id2c[$1] = $_ if (/^(\d+)/);
  }
  @c2id{map {$id2c[$_]||qw()} (0..$#id2c)} = grep {$_} @id2c;
  foreach (grep {!exists($c2id{$_})} keys(%c2ndocs)) {
    push(@id2c,$_);
    $c2id{$_} = @id2c;
  }
  @c2id{map {$id2c[$_]||qw()} (0..$#id2c)} = grep {$_} @id2c;

  ##-- save dat
  my ($cstr);
  $outfh->print(
		join("\t", "#1:ID", "2:NAME", "3:NDOCS", "4:FRACDOCS"), "\n",
		map {
		  $c=$cstr=$id2c[$_];
		  $cstr =~ s/\s/_/g;
		  join("\t", $_, $cstr, $c2ndocs{$c}, $c2ndocs{$c}/$ndocs)."\n"
		} sort {$a<=>$b} grep {defined($id2c[$_])} (0..$#id2c),
	       );
}

=pod

=head1 NAME

dc-corpus-sizedat.perl - get gnuplot-able data file from corpus

=head1 SYNOPSIS

 dc-corpus-info.perl [OPTIONS] [CORPUS...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -out OUTFILE           # output file (default=STDOUT)

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
