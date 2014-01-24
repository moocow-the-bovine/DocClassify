#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Program ':all';

use File::Copy ('copy','move');
use Cwd 'abs_path'; ##-- for abs_path()

#use PDL;
#use PDL::Ngrams;

use Getopt::Long qw(:config no_ignore_case);
use Encode qw(encode decode);
use File::Basename qw(basename dirname);
use Pod::Usage;

#use strict;
BEGIN { select(STDERR); $|=1; select(STDOUT); }

##------------------------------------------------------------------------------
## Constants & Globals
##------------------------------------------------------------------------------
our $prog = basename($0);

%opts = (%opts,
	 outputFile => '-',
	 outputDir  => undef,
	 corpusNew => { %{$opts{corpusNew}}, label=>'' },
	 corpusLoad => { %{$opts{corpusLoad}}, label=>'' },
	 corpusSave => { %{$opts{corpusSave}}, mode=>undef, saveCats=>undef },
	);

our $copy_how = 'copy'; ##-- one of 'copy', 'hardlink', 'symlink', 'move'
our $copy_glob = 1;     ##-- whether to copy by file-glob (default=true)

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(
	   ##-- common
	   dcOptions(),

	   ##-- Local: I/O
	   'output-dir|outdir|odir|od|d=s'=> \$outdir,

	   ##-- Local: Misc
	   'copy|c' => sub { $copy_how='copy'; },
	   'hardlink|hard|link|hl' => sub { $copy_how='hardlink'; },
	   'symlink|sym|sl|s' => sub { $copy_how='symlink'; },
	   'move|m' => sub { $copy_how='move'; },
	   'glob|g!' => \$copy_glob,
	  );
our $verbose = $opts{verbose};


pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});


##------------------------------------------------------------------------------
## Subs
##------------------------------------------------------------------------------

## %copied: ($oldfile => $newfile, ...)
our %copied = qw();

## $outfile = copyDocFile($infile)
sub copyDocFile {
  my $infile = shift;
  return $copied{$infile} if (exists($copied{$infile}));
  my $outfile = $outdir.'/'.basename($infile);
  print STDERR "+ FILE: $infile -> $outfile" if ($verbose >= 2);
  unlink($outfile) if (-e $outfile);
  if    ($copy_how eq 'copy') {
    copy($infile,$outfile) or die("$0: copy failed for '$infile' -> '$outfile': $!");
  }
  elsif ($copy_how eq 'move') {
    move($infile,$outfile) or die("$0: move failed for '$infile' -> '$outfile': $!");
  }
  elsif ($copy_how eq 'hardlink') {
    #system('ln',$infile,$outfile)==0 or die("$0: ln failed for '$outfile' -> '$infile': $!");
    link($infile,$outfile) or die("$0: hard-link failed '$outfile' -> '$infile': $!");
  }
  elsif ($copy_how eq 'symlink') {
    #system('ln','-s',abs_path($infile),$outfile)==0 or die("$0: ln -s failed for '$outfile' -> '$infile': $!");
    symlink(abs_path($infile),$outfile) or die("$0: symlink failed '$outfile' -> '$infile': $!");
  }
  else {
    die("$0: unknown copy method '$copy_how'!");
  }
  return $copied{$infile} = $outfile;
}

##------------------------------------------------------------------------------
## MAIN
##------------------------------------------------------------------------------

##-- vars
our $corpus = undef;

##-- load input corpora
push(@ARGV,'-') if (!@ARGV);
foreach (@ARGV) {
  my $c2 = DocClassify::Corpus->new( optsNew('corpus') )->loadFile($_,optsLoad('corpus'))
    or die("$0: Corpus->loadFile() failed for '$_': $!");
  if (!$corpus) {
    $corpus=$c2;
    if (!defined($outdir)) {
      $outdir = "$_.d";
    }
    next;
  }
  $corpus->addCorpus($c2);
}

##-- output directory
if (!-d $outdir) {
  system('mkdir','-p',$outdir)==0
    or die("$0: mkdir -p '$outdir' failed: $!");
}

##-- copy
my ($doc,$key,$docfile);
foreach $doc (@{$corpus->{docs}}) {
  print STDERR "DOC: ", $doc->label, "\n" if ($verbose);
  $docfile = $doc->{file};
  if ($copy_glob && defined($docfile)) {
    foreach (glob(dirname($docfile).'/'.basename($docfile,'.xml').'.*')) {
      copyDocFile($_);
    }
  }
  foreach $key (grep {$_ =~ /file$/i} keys(%$doc)) {
    $doc->{$key} = copyDocFile($doc->{$key}) if (defined($doc->{$key}));
  }
  if ($doc->label eq $docfile) {
    $doc->label($doc->{file}); ##-- re-label
  }
}

#print STDERR "$prog: Corpus->saveFile($outfile)\n" if ($verbose);
$corpus->saveFile($opts{outputFile}, optsSave('corpus'));

=pod

=head1 NAME

dc-corpus-copy.perl - copy all corpus data to a new directory

=head1 SYNOPSIS

 dc-corpus-copy.perl [OPTIONS] [CORPUS...]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level

 I/O Options:
  -copy                  # copy data files
  -move                  # move data files
  -hardlink              # hard-link data files
  -symlink               # symbolically link data files
  -glob , -noglob        # do/don't select targets using file glob (default=do)
  -input-mode MODE       # I/O mode for input corpora (default=guess)
  -output-mode MODE      # I/O mode for output corpus (default=guess)
  -output-file FILE      # set corpus output file (default=-)
  -output-dir DIR        # set output data directory (default=CORPUS.d)

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
