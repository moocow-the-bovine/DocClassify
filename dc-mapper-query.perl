#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Program ':all';
use DocClassify::Utils ':all';

#use PDL;
#use PDL::Ngrams;

use Getopt::Long qw(:config no_ignore_case);
use Encode qw(encode decode encode_utf8 decode_utf8);
use File::Basename qw(basename dirname);
use Pod::Usage;

use Benchmark qw(timethese :hireswallclock);

use strict;
BEGIN { select(STDERR); $|=1; select(STDOUT); }

##------------------------------------------------------------------------------
## Constants & Globals
##------------------------------------------------------------------------------
our $prog = basename($0);
(our $pbase = $prog) =~ s/\..*$//;
our $verbose = setVerbose(2);
our $doProfile = 1;
%opts = (%opts,
	);

##-- hack: set only local overrides with '-map-option OPT=VALUE'
our $mapUser = {};
my %dcOpts = dcOptions();
delete @dcOpts{grep {$_ !~ /help|verbose|map-input-mode|map-option|verbose-io/} keys %dcOpts};
$_ = $mapUser foreach (grep {$_ eq $opts{mapNew}} values %dcOpts);

##-- local options
my %qopts =
  (
   mapto=>undef,
   k    =>10,
   minTermFreq => 0,
   minDocFreq  => 0,
   norm => 'no',
   xlate => 'xlate_dta',
   bench_iters => 0,
   do_shell => 0,
  );
my $xlate0 = '';
my $xlate_sub = undef;
my %mqOpts =
  (
   'map-to-categories|to-cats|tocats|cats|tc|c' => sub { $qopts{mapto}='cats'; },
   'map-to-documents|to-docs|todocs|docs|td|d' => sub { $qopts{mapto}='docs'; },
   'map-to-terms|to-terms|toterms|terms|tt|t'  => sub { $qopts{mapto}='terms'; },
   ##-- mapping aliases
   'map-to-books|to-books|books|b' => sub { $qopts{mapto}='books'; },
   'map-to-volumes|to-volumes|volumes|vols' => sub { $qopts{mapto}='volumes'; },
   'map-to-pages|to-pages|pages|p' => sub { $qopts{mapto}='pages'; },
   ##
   'k-best|kbest|n-best|nbest|n|k=i' => \$qopts{k},
   'min-term-freq|min-freq|mtf|tf|f=i' => \$qopts{minTermFreq},
   'min-term-docfreq|min-docfreq|mdf|df|F=i' => \$qopts{minDocFreq},
   'normalize|norm=s' => \$qopts{norm},
   'no-normalize|nonormalize|no-norm|nonorm' => sub {$qopts{norm}='no'},
   'profile-iterations|iterate|iters|i=i' => \$qopts{bench_iters},
   'shell|interactive!' => \$qopts{do_shell},
   'xlate-label|xlate|xl|x=s' => \$qopts{xlate},
  );

##------------------------------------------------------------------------------
## Command-line
##------------------------------------------------------------------------------
GetOptions(%dcOpts,
	   %mqOpts,
	  );
$verbose = $opts{verbose};

pod2usage({-exitval=>0, -verbose=>0}) if ($opts{help});
pod2usage({-exitval=>1, -verbose=>0, -msg=>'No Mapper file specified!'}) if (!@ARGV);
our $mapfile = shift(@ARGV);

##-- logger
DocClassify::Logger->ensureLog;
our $logger = 'DocClassify::Program';

##======================================================================
## Subs
##======================================================================

##--------------------------------------------------------------
## undef = evalCommand($map, $argv)
## undef = evalCommand($map,\@argv)
sub evalCommand {
  my ($map,$argv) = @_;

  ##-- parse options
  my ($rc,$qargs, $qhelp,$qexit,$qinfo);
  my %xopts = ('help|h'=>\$qhelp, 'exit|byte|quit|q'=>\$qexit, 'info|config'=>\$qinfo);
  if (ref($argv)) {
    Getopt::Long::GetOptionsFromArray($argv,%mqOpts,%xopts);
    $qargs = $argv;
  } else {
    ($rc,$qargs) = Getopt::Long::GetOptionsFromString($argv,%mqOpts,%xopts);
  }

  ##-- exit?
  if ($qexit) {
    $logger->info('exiting');
    exit 0;
  }

  ##-- help?
  do { pod2usage({-exitval=>'NOEXIT', -verbose=>0}); return; } if ($qhelp);

  ##-- info?
  if ($qinfo) {
    print
      ("\n ##-- INFO: $pbase options\n",
       (map {" $_ = $qopts{$_}\n"} sort keys %qopts),
       "\n ##-- INFO: mapper configuration\n",
       (map {" map.$_->[0] = $_->[1]\n"}
	[file=>$mapfile],
	[class=>ref($map)],
	[minFreq=>$map->{minFreq}],
	[minDocFreq=>$map->{minDocFreq}],
	[NT=>$map->{tenum}->size],
	[ND=>$map->{denum}->size],
	[NC=>$map->{lcenum}->size],
	[svdR=>$map->{svd}{sigma}->nelem],
       ),
       "\n",
      );
  }

  ##-- re-commpile xlate sub?
  if ($qopts{xlate} ne $xlate0) {
    if (!defined($xlate_sub=UNIVERSAL::can('main',$qopts{xlate}))) {
      $qopts{xlate} .= ';' if ($qopts{xlate} !~ /;\s*$/s);
      $xlate_sub = eval qq( sub {$qopts{xlate}} );
      die("$prog: could not compile -xlate sub {$qopts{xlate}}: $@") if ($@);
    }
    $xlate0 = $qopts{xlate};
  }

  ##-- get query string
  my $query = join(' ',map {utf8::is_utf8($_) ? $_ : Encode::decode_utf8($_)} @{$qargs//[]});
  return if ($query =~ /^\s*$/s);

  if ($qopts{bench_iters}) {
    ##-- benchmark
    Benchmark::timethese($qopts{bench_iters},{"kbest-$qopts{mapto}"=>sub { evalQuery($map,$query); }});
  }
  else {
    ##-- real query
    evalQuery($map,$query);
  }
}

##--------------------------------------------------------------
## undef = evalQuery($map,$query)
sub evalQuery {
  my ($map,$query) = @_;

  ##-- parse query signature
  my $q_sig = $map->querySignature($query);
  #$q_sig->save1gFile('-') if (!$qopts{do_shell} && !$qopts{bench_iters});  ##-- debug: dump signature

  ##-- get k-best items
  my $kbest = $map->mapQuery($q_sig, %qopts);
  return if ($qopts{bench_iters});

  ##-- translate labels
  if ($xlate_sub) {
    foreach (@$kbest) {
      $xlate_sub->($_);
      die("$prog: -xlate sub failed for '$_': $@") if ($@);
    }
  }

  ##-- ... and dump
  $kbest->[$_]{i} = $_ foreach (0..$#$kbest);
  print
    (($q_sig->{qstr_}//'QUERY')." [".join(":",@{$q_sig->{drange_}//['?','?']})."]\n",
     (map {" [$_->{i}]\t$_->{dist}\t$_->{id}\t".($_->{label}//'')."\n"} @$kbest),
    );
}

##--------------------------------------------------------------
## undef = xlate_dta()
##  + translates label of current k-best item in $_
sub xlate_dta {
  return if ($qopts{mapto} !~ /^[dpcbv]/i);
  (my $lab = basename($_->{label})) =~ s/\.\D.*$//;
  $lab =~ s/\.(\d+)$/?p=$1/;
  $_->{label} = $lab;
}

##--------------------------------------------------------------
## undef = shellPrompt()
##  + prompts to stdout, for shell mode
sub shellPrompt {
  print "${pbase}::${mapfile}::$qopts{mapto}> ";
}

##--------------------------------------------------------------
## undef = runShell($map)
sub runShell {
  my $map = shift;
  binmode($_,':utf8') foreach (\*STDIN,\*STDOUT,\*STDERR);
  shellPrompt();
  while (defined($_=<STDIN>)) {
    chomp;
    next if (/^\s*$/);
    evalCommand($map,$_);
  } continue {
    shellPrompt();
  }
}


##======================================================================
## MAIN
##======================================================================

##-- vars
our $map = DocClassify::Mapper->loadFile($mapfile, optsLoad('map') )
  or die("$0: Mapper->load() failed for '$mapfile': $!");
$map->{verbose} = $verbose;
@$map{keys %$mapUser} = values %$mapUser; ##-- user overrides

##-- defaults
$qopts{mapto} //= $map->{lcenum}->size() > 1 ? 'cat' : 'doc';

##-- guts
evalCommand($map,['--',@ARGV]) if (@ARGV);
if ($qopts{do_shell}) {
  runShell($map);
}

=pod

=head1 NAME

dc-mapper-query.perl - query DocClassify::Mapper objects

=head1 SYNOPSIS

 dc-mapper-query.perl [OPTIONS] MAPFILE [QUERY...]

 Initial Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -map-option OPT=VALUE  # override stored mapper option
  -map-input-mode MODE   # I/O mode for input mapfile (default=guess)
  -shell , -noshell      # do/don't run as interactive shell (default=-noshell)

 Mapping Target Options:
  -kbest=K               # retrieve K-best objects (default=10)
  -terms                 # get k-best terms
  -docs	                 # get k-best documents (alias: -pages)
  -cats                  # get k-best categories (aliases: -books, -volumes)

 Query Options:
  -norm=HOW              # distance normalization method: linear|gaussian|none; default='none'
  -nonorm                # alias for -norm=none
  -mtf=FREQ              # [-terms mode] result mask minimum term frequency
  -mdf=DOCFREQ           # [-terms mode] result mask minimum document count
  -iters=NITERS          # profile NITERS iterations of QUERY (default=0)
  -xlate-labels=CODE     # translate labels using CODE on $_
  -quit                  # quit shell

 Query Syntax:
  TERM:WEIGHT            # query TERM with relative weight WEIGHT
  TERM                   # alias for TERM:1
  doc=DOC                # query doc DOC if it exists, else all docs matching regex DQUERY (also page=, pag=)
  cat=CAT                # query class CAT if it exists, else all classes matching regex CQUERY (also book=, vol=)
  QUERY, QUERY           # multiple query conditions can be separated with spaces, tabs, or commas

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
