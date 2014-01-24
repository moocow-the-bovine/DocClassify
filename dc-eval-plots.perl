#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use MUDL;
use DocClassify;
use DocClassify::Utils qw(:all);

use PDL;
use PDL::Graphics::PGPLOT;
use PDL::Graphics::PGPLOT::Window;
use PDL::Graphics::LUT;    ##-- for color tables used by e.g. imag()
use PDL::Image2D;          ##-- for box2d, patch2d

#use PDL::Ngrams;
use MUDL::PDL::Stats;
use MUDL::PDL::Smooth;

use Getopt::Long qw(:config no_ignore_case);
use Encode qw(encode decode);
use File::Basename qw(basename);
use File::Copy;  ##-- for move()
use Pod::Usage;

#use strict;
BEGIN { select(STDERR); $|=1; select(STDOUT); }

##------------------------------------------------------------------------------
## Constants & Globals
##------------------------------------------------------------------------------
our $prog = basename($0);
our ($help);
our $verbose = 2;

#our $outputEncoding = 'UTF-8';
#our $inputEncoding  = 'UTF-8';
#our $format   = 1;

our %evalopts = qw();
our %saveopts = qw();

our $outdir = undef;

##==============================================================================
## Command-line
##==============================================================================
GetOptions(##-- General
	   'help|h' => \$help,
	   'verbose|v=i' => \$verbose,

	   ##-- I/O
	   'output-directory|output-dir|outdir|od=s' => \$outdir,
	   #'format|f=1' => \$format
	  );
#$verbose=$fcopts{verbose};


pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##==============================================================================
## Subs
##==============================================================================

##----------------------------------------------------------------------
## Subs: plot utils

BEGIN {
  our $PG_DEV = '';
  our $PG_ID  = undef;
}
sub _usepgplot {
  my $dev = shift;
  $dev = '/XS' if (!defined($dev));
  return if ($dev eq $PG_DEV); ##-- don't re-use pgplot!
  _closepgplot() if (defined($PG_ID));

  ##-- copy&pasted from DocClassify::Utils.pm
  dev("$dev");
  ctab(lut_data('smooth2')); ##-- color table similar to gnuplot 'pm3d' default; see string list lut_names() for more
  autolog(1);

  $PG_DEV = $dev;
}

sub _closepgplot {
  my ($nums,$names) = window_list();
  close_window($_) foreach (@$nums);
}

sub useplplot {
  require PDL::Graphics::PLplot;
  PDL::Graphics::PLplot->import();
}

sub _plwin {
  my %opts = @_;
  useplplot();
  return $opts{win} if (defined($opts{win}));
  $opts{DEV} = 'xwin' if (!$opts{DEV});
  $opts{FILE} = ':0' if ($opts{DEV} eq 'xwin' ? ':0' : "plplot.$opts{DEV}");
  return PDL::Graphics::PLplot->new(%opts);
}
sub _plpoints {
  my ($x,$y,%opts) = @_;
  return _plwin(%opts)->xyplot($x,$y,PLOTTYPE=>'POINTS',%opts);
}
sub _plline {
  my ($x,$y,%opts) = @_;
  return _plwin(%opts)->xyplot($x,$y,PLOTTYPE=>'LINE',%opts);
}
sub _pllinespoints {
  my ($x,$y,%opts) = @_;
  return _plwin(%opts)->xyplot($x,$y,PLOTTYPE=>'LINESPOINTS',%opts);
}

##----------------------------------------------------------------------
## Subs: data utils

##--
## $psmooth = li1($p,$eps)
##  + linear-interpolated smoothed (1-$eps)*$p + $eps;
sub _li1 {
  my ($p,$eps) = @_;
  $eps = 1e-5 if (!defined($eps));
  return (1-$eps)*$p + $eps;
}

##--
## $F1 = F1($pr,$rc,$eps)
##  + balanced F-score
sub _F1 {
  my ($pr,$rc,$eps) = @_;
  #return Fb($pr,$rc,0.5,$eps);
  my ($pr1,$rc1) = (li1($pr,$eps),li1($rc,$eps));
  #return 2*$pr*$rc / ($pr+$rc);
  return 2/($pr1**-1 + $rc1**-1);
}

##--
## $F_beta = Fb($pr,$rc,$beta,$eps)
##  + beta-weighted F-score (see wikipedia / "F-score")
##     $beta = 2.0 --> weight $rc twice as heavily as $pr
##     $beta = 0.5 --> weight $rc half as heavily as $pr
sub _Fb {
  my ($pr,$rc,$beta,$eps)=@_;
  $beta = 0.5 if (!defined($beta));
  my ($pr1,$rc1) = (li1($pr,$eps),li1($rc,$eps));
  my $Fb = (1+$beta**2) * ($pr1*$rc1) / ($beta**2 * $pr1 + $rc1);
}

## undef = ploterrs(\%opts, [$x1,$mu1,$sd1,\%opts1], ..., [$xN,$muN,$sdN,\%optsN]);
sub ploterrs {
  my ($gopts,@args) = @_;
  my %gopts = %$gopts; #(%eplot,%$gopts);
  my $xvals    = null;
  my $yvals    = null;
  my $x0 = undef;
  my ($arg,$x,$mu,$sd,$opts);
  foreach $arg (@args) {
    ($x,$mu,$sd,$opts) = @$arg;
    $x0       = $x  if (defined($x) && !defined($x0));
    $x        = $x0 if (defined($x0) && !defined($x));
    $xvals    = $xvals->append($x) if (defined($x));
    $yvals    = $yvals->append($mu-$sd)->append($mu+$sd);
  }
  my ($ymin,$ymax) = $yvals->minmax;
  $gopts{xrange}    = [$xvals->min-1,$xvals->max+1] if (!$gopts{xrange} || !@{$gopts{xrange}});
  $gopts{yrange}    = [$ymin-($ymax-$ymin)*.02, $ymax+($ymax-$ymin)*.02] if (!$gopts{yrange});
  ##
  my ($xv,$offset);
  foreach (0..$#args) {
    ($x,$mu,$sd,$opts) = @{$args[$_]};
    $x = $x0 if (!defined($x));
    $opts = {} if (!$opts);
    $offset = $opts->{offset};
    delete($opts->{offset});
    errb($x+$offset,$mu,$sd,{%gopts,%$opts});
    hold();
  }
  release();
}


##----------------------------------------------------------------------
## Subs: plotting

##-- options
our %dpopts_default = (
		       plot2d=>1,
		       plot3d=>1,
		       hardcopy => 1,
		       ##
		       #hardcopy=>0, ##-- DEBUG
		      );
our ($dev2d,$dev3d, $plot2d,$plot3d,$plot_hardcopy);
our (@plots2d, @plots3d); ##-- output filenames

sub doplots {
  my ($eval,%dpopts) = @_;

  ##-- options
  %dpopts = (%dpopts,%dpopts_default);
  ($plot2d,$plot3d,$plot_hardcopy) = @dpopts{qw(plot2d plot3d hardcopy)};
  if ($plot_hardcopy) {
    $dev2d = '/CPS';
    $dev3d = '/WD';
  } else {
    $dev3d = $dev2d = '/XS';
  }

  ##~~ vars
  my $lab2docs = $eval->{lab2docs};

  ##--------------------
  ## create enums
  my $denum  = MUDL::Enum->new;
  @{$denum->{id2sym}} = keys %$lab2docs;
  @{$denum->{sym2id}}{@{$denum->{id2sym}}} = (0..$#{$denum->{id2sym}});
  my $d_sym2id = $denum->{sym2id};
  my $ND = $denum->size;
  ##
  my $gcenum = MUDL::Enum->new;
  my ($d12,$cat);
  foreach $d12 (values(%$lab2docs)) {
    foreach $cat (map {@{$_->{cats}}} @$d12) {
      $cat->{id} = $1 if ($cat->{name} =~ /^(\d+)_/);
      $gcenum->addIndexedSymbol(@$cat{qw(name id)});
    }
  }
  my $lcenum = $gcenum->clone->compact;
  my $lc_sym2id = $lcenum->{sym2id};
  my $NC = $lcenum->size;

  ##--------------------
  ## compile $d2c
  my $d2c  = pdl([@$lc_sym2id{map {$lab2docs->{$_}[0]{cats}[0]{name}} @{$denum->{id2sym}}}]); ##-- [$di] -> $ci_wanted_local

  ##--------------------
  ## ... other vars
  my $gcids  = pdl(long, [@{$gcenum->{sym2id}}{@{$lcenum->{id2sym}}}]);             ##-- [$lci] -> $gci
  my $lcids  = zeroes(long,$gcids->max+1); $lcids->index($gcids) .= $gcids->xvals;  ##-- [$gci] -> $lci
  my $NCg    = $gcids->max+1;
  my $d2cg   = $gcids->index($d2c);

  ##--------------------
  ## $dc_dist (from Mapper::LSI::loadCrossCheckEval() : [$di,$cli] -> d($d,$cl)
  my $dc_dist = zeroes($ND,$NC)+2; ##-- initialize to max
  my ($lab,$dcats2,@dci,@dcdist,$di); #$d12
  while (($lab,$d12)=each(%$lab2docs)) {
    if (!defined($di = $d_sym2id->{$lab})) {
      warn("$0: no internal ID for doc label '$lab' -- skipping");
      next;
    }
    $dcats2 = $d12->[1]{cats};
    @dci    = grep {defined($lc_sym2id->{$dcats2->[$_]{name}})} (0..$#$dcats2);
    @dcdist = map {defined($_->{dist_raw}) ? $_->{dist_raw} : $_->{dist}} map {$dcats2->[$_]} @dci;
    $dc_dist->slice("($di),")->index(pdl(long,[@$lc_sym2id{map {$dcats2->[$_]{name}} @dci}])) .= pdl(\@dcdist);
  }
  my $dcg_dist = zeroes($ND,$NCg)+2; ##-- initialize to max
  $dcg_dist->dice_axis(1,$gcids) .= $dc_dist;

  ##--------------------
  ## baseline accuracy
  my $d2c_bydist = $dc_dist->xchg(0,1)->minimum_ind;
  my $acc_bydist = ($d2c_bydist==$d2c)->nnz / $ND;

  ##--------------------
  ##-- get positive evidence
  my $dc_which1 = sequence($ND)->cat($d2c)->xchg(0,1);  ##-- [$di]     -> [$di,$ci] : $di \in $ci
  my $dc_mask1  = zeroes(byte,$dc_dist->dims);          ##-- [$di,$ci] -> 1($di \in     $ci)
  $dc_mask1->indexND($dc_which1) .= 1;
  my $dc_mask   = $dc_mask1; ##-- alias
  my $dc_which0 = whichND(!$dc_mask1);                   ##-- [$di,$ci] -> 1($di \not\in $ci)
  my $nc  = $dc_mask1->long->sumover->double;            ##-- [$ci] -> |{ $di : $di \in     $ci }|
  my $nnc = $nc->sum - $nc;                              ##-- [$ci] -> |{ $di : $di \not\in $ci }|

  ##--------------------
  ##-- from testme.perl test_errors(), above
  ## $dcdist_mu = pdl(1): global avg    dist($ci,$di)
  ## $dcdist_sd = pdl(1): global stddev dist($ci,$di)
  my $dcdist_mu = $dc_dist->flat->average;
  my $dcdist_sd = (($dc_dist - $dcdist_mu)**2)->flat->average->sqrt;

  my $nc_min = 3;#50; #3; #10; ##-- minimum #/cats to use fit

  ##--------------------
  ## $cdist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \in $ci
  ## $cdist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \in $ci
  my ($cdist_mu,$cdist_sd,$cdist_isgood,$cdist_sd_raw);
  $cdist_mu = $dc_dist->average;
  $cdist_sd = (($dc_dist - $cdist_mu->slice("*1,"))**2)->average->sqrt;
  $cdist_isgood = ($cdist_sd->isfinite)&($cdist_sd>0);
  $cdist_sd_raw = $cdist_sd->pdl;
  ##
  $cdist_sd->where(!$cdist_isgood) .= $cdist_sd->where($cdist_isgood)->minimum/2;

  ##--------------------
  ## $dc1dist: CCS: [$di,$ci] -> dist($ci,$di) : $di \in $ci
  ## $c1dist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \in $ci
  ## $c1dist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \in $ci
  ## $c1dist_mu0, $c1dist_sd0: global fit parameters
  my ($dc1dist,$c1dist_mu,$c1dist_sd,$c1dist_sd_nan0,$c1dist_sd_bad0,$c1dist_isgood,$c1dist_mu_raw,$c1dist_mu_nan0);
  $dc1dist = PDL::CCS::Nd->newFromWhich($dc_which1,$dc_dist->indexND($dc_which1));
  $c1dist_mu0 = $dc1dist->_nzvals->average;
  my $c1dist_sd0 = (($dc1dist->_nzvals-$c1dist_mu0)**2)->average->sqrt;
  $c1dist_mu = $dc1dist->average_nz->decode;
  $c1dist_sd = (($dc1dist - $c1dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  $c1dist_mu_raw = $c1dist_mu->pdl;
  $c1dist_sd_raw = $c1dist_sd->pdl;
  $c1dist_isgood = (($c1dist_sd_raw->isfinite) & ($nc >= $nc_min) & ($c1dist_mu_raw->isfinite));
  #$c1dist_isgood = (($c1dist_sd_raw->isfinite));
  ($c1dist_mu_nan0 = $c1dist_mu->pdl)->where(!$c1dist_mu->isfinite) .= $c1dist_mu->where($c1dist_mu->isfinite)->max;
  ($c1dist_sd_nan0 = $c1dist_sd->pdl)->where(!$c1dist_sd->isfinite) .= 0;
  ($c1dist_sd_bad0 = $c1dist_sd->pdl)->where(!$c1dist_isgood) .= 0;

  ##--------------------
  ## $dc0dist: CCS: [$di,$ci] -> dist($ci,$di) : $di \not\in $ci
  ## $c0dist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \not\in $ci
  ## $c0dist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \not\in $ci
  ## $c0dist_mu0, $c0dist_sd0: global fit parameters
  my ($dc0dist,$c0dist_mu,$c0dist_sd,$c0dist_sd_nan0,$c0dist_sd_bad0, $c0dist_isgood, $c0dist_sd_eps);
  my ($c0dist_mu0,$c0dist_sd0,$c0dist_mu_raw,$c0dist_mu_nan0);
  $dc0dist = PDL::CCS::Nd->newFromWhich($dc_which0,$dc_dist->indexND($dc_which0));
  $c0dist_mu0 = $dc0dist->_nzvals->average;
  $c0dist_sd0 = (($dc0dist->_nzvals-$c0dist_mu0)**2)->average->sqrt;
  $c0dist_mu = $dc0dist->average_nz->decode;
  $c0dist_sd = (($dc0dist - $c0dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  $c0dist_mu_raw = $c0dist_mu->pdl;
  $c0dist_sd_raw = $c0dist_sd->pdl;
  #$c0dist_isgood = (($c0dist_sd->isfinite) & ($nc >= $nc_min));
  $c0dist_isgood = ($c0dist_sd_raw->isfinite & $c0dist_mu_raw->isfinite);
  ($c0dist_mu_nan0 = $c0dist_mu->pdl)->where(!$c0dist_mu->isfinite) .= $c0dist_mu->where($c0dist_mu->isfinite)->max;
  ($c0dist_sd_nan0 = $c0dist_sd->pdl)->where(!$c0dist_sd->isfinite) .= 0;
  ($c0dist_sd_bad0 = $c0dist_sd->pdl)->where(!$c0dist_isgood) .= 0;

  ##-- %eplot: plot options
  our (%eplot);

  if ($plot2d) {
    _usepgplot($dev2d);
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category (Positives & Global:red)'},
	     [$gcids-.1,$c1dist_mu_nan0,$c1dist_sd_nan0,{}],
	     [$gcids+.1,$c0dist_mu_nan0,$c0dist_sd,{color=>'red'}],
	     [$gcids->average->rint+.5, $dcdist_mu,$dcdist_sd,{symbol=>'star',color=>'blue'}]);
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category (Global, Positives:blue & Negatives:red)'},
	     [$gcids, $cdist_mu,$cdist_sd, {}],
	     [$gcids-.2, $c1dist_mu_nan0,$c1dist_sd_nan0, {color=>'blue'}],
	     [$gcids+.2, $c0dist_mu_nan0,$c0dist_sd_nan0, {color=>'red'}],
	     [$gcids->average->rint+.5, $dcdist_mu,$dcdist_sd, {sym=>'plus',color=>'green',lineWidth=>5,charsize=>2}]);
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category (Positives:black, Negatives:red)'},
	     [$gcids-.1, $c1dist_mu_nan0,$c1dist_sd_nan0, {}],
	     [$gcids+.1, $c0dist_mu_nan0,$c0dist_sd_nan0, {color=>'red'}]);
    ##
    push(@plots2d,"fit-by-cat.gp","fit-by-cat.gpn","fit-by-cat.pn");
  }

  ##--------------------
  ##-- hack mu, sd: positives
  #$c1dist_mu->where(!$c1dist_sd->isfinite) .= 0;
  $c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu->where($c1dist_isgood)->minimum;
  #$c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu->where($c1dist_isgood)->minimum/2;
  #$c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu0;
  ##
  #$c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd0;
  #$c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd->where($c1dist_sd->isfinite)->minimum/2;
  $c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd->where($c1dist_isgood)->minimum/2;

  ##--------------------
  ##-- hack mu, sd: negatives
  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu->where($c0dist_isgood)->minimum/2;
  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu->where($c0dist_isgood)->maximum*2;
  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu0;
  ##
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->minimum;
  $c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->maximum;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->maximum*2;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->average;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd0;


  ##--------------------
  ##-- plot: hacked mu,sd
  if ($plot2d) {
    _usepgplot($dev2d);
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked[0] (+:black, -:red)'},
	     [$gcids-.1, $c1dist_mu_nan0,$c1dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.1, $c0dist_mu_nan0,$c0dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu,    $c1dist_sd,      {}],
	     [$gcids+.1, $c0dist_mu,    $c0dist_sd,      {color=>'red'}],
	     [$gcids,    $nc/$nc->max,$nc->zeroes,       {color=>'yellow',charsize=>2}]
	    );
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
    push(@plots2d,"fit-by-cat.hacked0.pn");
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked[1] (+:black, -:red)'},
	     [$gcids-.1, $c1dist_mu_nan0,$nc->zeroes, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.1, $c0dist_mu_nan0,$nc->zeroes, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu,    $c1dist_sd,  {}],
	     [$gcids+.1, $c0dist_mu,    $c0dist_sd,  {color=>'red'}],
	    );
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
    push(@plots2d,"fit-by-cat.hacked1.pn");
  }


  ##--------------------
  ##-- heuristically shovel around fit parameters ($d1)
  my ($conf_nofp,$conf_nofn);
  #($conf_nofp,$conf_nofn) = (.67,.99); ##-- acc=.566
  #($conf_nofp,$conf_nofn) = (.9,.95); ##-- acc=.560
  #($conf_nofp,$conf_nofn) = (.67,.95); ##-- acc=.558
  #($conf_nofp,$conf_nofn) = (.85,.95); ##-- acc=.558
  #($conf_nofp,$conf_nofn) = (.95,.95); ##-- acc=.54934
  #($conf_nofp,$conf_nofn) = (.9,.9); ##-- acc=.55370
  #($conf_nofp,$conf_nofn) = (.85,.85); ##-- acc=.55297
  #($conf_nofp,$conf_nofn) = (.85,.67); ##-- acc=.541
  #($conf_nofp,$conf_nofn) = (.95,.5);   ##-- acc=.526
  ##--
  #($conf_nofp,$conf_nofn) = (.67,.67);
  #($conf_nofp,$conf_nofn) = (.75,.75);
  #($conf_nofp,$conf_nofn) = (.8,.5);
  #($conf_nofp,$conf_nofn) = (.5,.8);
  #($conf_nofp,$conf_nofn) = (.5,.95);
  #($conf_nofp,$conf_nofn) = (.9,.9);
  ($conf_nofp,$conf_nofn) = (.95,.95); ##-- best w/ safe.u1:
  ## : safe.u1.c95-95.log:0.671262699564586	acc:max	Fb((1-$dc1_scdf_adj),(1-$dc0_scdf_adj),(1-($nc/$ND))->slice("*1,"))
  ## : all.c95-95.log    :0.596383121232418	acc:max	F1((1-$dc1_scdf_adj),(1-$dc0_scdf_adj))
  #($conf_nofp,$conf_nofn) = (.90,.95);
  #($conf_nofp,$conf_nofn) = (.95,.90);
  #($conf_nofp,$conf_nofn) = (.99,.99);
  my ($cutoff_avg,$cutoff_wavg,$cutoff_ok,$c1dist_mu_adj,$c0dist_mu_adj);
  #my $cutoff_w1 = ($nc/$ND);
  my $cutoff_w1 = ($nc/$nc->max);
  my $compute_cutoffs = sub {
    print STDERR "compute_cutoffs(): conf_nofp=$conf_nofp; conf_nofn=$conf_nofn\n";
    $cutoff_nofp = $c0dist_mu - scalar(gausswidth($conf_nofp, $c0dist_mu,$c0dist_sd));
    $cutoff_nofn = $c1dist_mu + scalar(gausswidth($conf_nofn, $c1dist_mu,$c1dist_sd));
    $cutoff_avg  = ($cutoff_nofp + $cutoff_nofn)/2;
    $cutoff_wavg = (1-$cutoff_w1)*$cutoff_nofp + $cutoff_w1*$cutoff_nofn;
    #$cutoff_wavg = $nnc/$ND*$cutoff_nofp + $nc/$ND*$cutoff_nofn;
    $cutoff_ok   = ($cutoff_nofn < $cutoff_wavg) & ($cutoff_nofp > $cutoff_wavg);
    our $c1dist_mu_save = $c1dist_mu->pdl;
    $c1dist_mu_adj = $c1dist_mu->pdl;
    $c0dist_mu_adj = $c0dist_mu->pdl;
    $c1dist_mu_adj->where(!$cutoff_ok) .= ($cutoff_wavg - scalar(gausswidth($conf_nofn, $c1dist_mu,$c1dist_sd)))->where(!$cutoff_ok);
  };
  $compute_cutoffs->();

  ##--------------------
  ##-- plot: hacked & adjusted mu,sd
  if ($plot2d) {
    _usepgplot($dev2d);
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ploterrs({%eplot,title=>"Fit by Cat: Hacked & Adjusted (+:black, -:red) [conf=($conf_nofn,$conf_nofp)]"},
	     [$gcids-.2, $c1dist_mu_nan0,$c1dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.2, $c0dist_mu_nan0,$c0dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu_adj,$c1dist_sd,      {}],
	     [$gcids+.1, $c0dist_mu_adj,$c0dist_sd,      {color=>'red'}],
	     [$gcids,    $nc/$nc->max,$nc->zeroes,       {color=>'yellow',charsize=>2}],
	     [$gcids-.1, $cutoff_nofn,  $nc->zeroes,     {symbol=>'square'}],
	     [$gcids+.1, $cutoff_nofp,  $nc->zeroes,     {symbol=>'square',color=>'red'}],
	     [$gcids+.1, $cutoff_wavg,  $nc->zeroes,     {symbol=>'cross',color=>'green'}]);
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
    push(@plots2d,"fit-by-cat.adjusted");
  }

  ##--------------------
  my ($dc1_cdf0,$dc0_cdf0, $dc1_scdf0,$dc0_scdf0);
  my ($dc_cdf,$dc1_cdf,$dc0_scdf,$dc1_scdf);
  my ($dc_cdf_adj,$dc1_cdf_adj,$dc0_scdf_adj,$dc1_scdf_adj);
  my ($dc_F1);
  my $compute_cdfs = sub {
    $dc_cdf = gausscdf($dc_dist, $cdist_mu->slice("*1,"), $cdist_sd->slice("*1,"));

    $dc1_cdf = gausscdf($dc_dist, $c1dist_mu->slice("*1,"), $c1dist_sd->slice("*1,"));
    $dc0_cdf = gausscdf($dc_dist, $c0dist_mu->slice("*1,"), $c0dist_sd->slice("*1,"));
    $dc1_scdf = li1($dc1_cdf,1e-5);
    $dc0_scdf = li1($dc0_cdf,1e-5);

    $dc1_cdf_adj = gausscdf($dc_dist, $c1dist_mu_adj->slice("*1,"), $c1dist_sd->slice("*1,"));
    $dc0_cdf_adj = gausscdf($dc_dist, $c0dist_mu_adj->slice("*1,"), $c0dist_sd->slice("*1,"));
    $dc1_scdf_adj = li1($dc1_cdf_adj);
    $dc0_scdf_adj = li1($dc0_cdf_adj);

    $dc1_cdf0 = gausscdf($dc_dist, $c1dist_mu0, $c1dist_sd0);
    $dc0_cdf0 = gausscdf($dc_dist, $c0dist_mu0, $c0dist_sd0);
    $dc1_scdf0 = li1($dc1_cdf0,1e-5);
    $dc0_scdf0 = li1($dc0_cdf0,1e-5);

    $dc_F1    = F1($dc1_cdf, $dc0_cdf, 1e-5);
  };
  $compute_cdfs->();

  ##--------------------
  #our (%iplot);
  %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'c1', ytitle=>'c2');
  if (0) {
    imag($dc0_cdf*(1-$dc1_cdf)*1,       {%iplot,itf=>'linear'});
    imag($dc0_cdf*(1-$dc1_cdf)*$dc_mask,{%iplot,itf=>'log'});
    imag(1-$dc1_cdf*(1-$dc0_cdf),{%iplot,itf=>'linear'});
  }

  ##--------------------
  ##-- accuracies
  my $acc = sub {
    my ($expr,$minmax) = @_;
    my $dc = eval $expr;
    $minmax = 'min' if (!defined($minmax));
    my $cats = $minmax eq 'max' ? $dc->xchg(0,1)->maximum_ind : $dc->xchg(0,1)->minimum_ind;
    my $acc = ($cats == $d2c)->nnz / $ND;
    print "$acc\tacc:${minmax}\t$expr\n";
    return $acc;
  };

  ##--------------------
  ##-- baseline
  if (0) {
    $acc->('$dc_dist'); ##-- .615

    ##-- stupid p-value tests
    $acc->('$dc_cdf','min'); ##-- .547
    $acc->('$dc1_scdf'); ##-- .428
    $acc->('$dc1_scdf_adj'); ##-- .597, all=.592
    $acc->('F1((1-$dc1_scdf_adj),(1-$dc0_scdf_adj))', 'max'); ##-- .597, all=.591 ##-- USE THIS! 
  }

 if ($plot3d) {
    our $dc_raw_l = 2-$dc_dist; #-- sim=2-d; d=2-1-cos; -> d=2-sim -> sim=1+cos
    our $dc_raw = zeroes($ND,$NCg)+$dc_raw_l->min;
    $dc_raw->dice_axis(1,$gcids) .= $dc_raw_l;
    ##
    %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'doc', ytitle=>'cat');
    _usepgplot($dev3d);
    imag($dc_raw,{%iplot,title=>'Raw Similarity: 1+cos(LSI(doc),LSI(cat))'});
    imag($dc_raw,{%iplot,title=>'Raw Similarity: 1+cos(LSI(doc),LSI(cat)) [log-scale]',itf=>'log'});
    push(@plots3d,"sim-raw.linear","sim-raw.log");
    ##
    our $dc_sim_l = F1((1-$dc1_scdf_adj),(1-$dc0_scdf_adj)); ##-- USE THIS!
    our $dc_sim = zeroes($ND,$NCg)+$dc_sim_l->min;
    $dc_sim->dice_axis(1,$gcids) .= $dc_sim_l;
    imag($dc_sim,{%iplot,title=>'F-Similarity: F1(1-cdf(doc~cat), 1-cdf(doc!~cat))'});
    imag($dc_sim,{%iplot,title=>'F-Similarity: F1(1-cdf(doc~cat), 1-cdf(doc!~cat)) [log-scale]', itf=>'log'});
    push(@plots3d,"sim-adj.linear","sim-adj.log");
    ##
    our $cc_which1 = $d2c->cat($dc_which1->slice("(1),"))->xchg(0,1);
    our $cc_which0 = $d2c->index($dc_which0->slice("(0)"))->cat($dc_which0->slice("(1),"))->xchg(0,1);
    our $asim1_l = PDL::CCS::Nd->newFromWhich($cc_which1,$dc_sim_l->indexND($dc_which1))->dummy(0,1)->average_nz->_missing(0)->decode;
    our $asim0_l = PDL::CCS::Nd->newFromWhich($cc_which0,$dc_sim_l->indexND($dc_which0))->dummy(0,1)->average_nz->_missing(0)->decode;
    our $asim_l  = ($asim1_l+$asim0_l);
    our $asim = zeroes($NCg,$NCg)+$asim_l->min;
    $asim->dice_axis(0,$gcids)->dice_axis(1,$gcids) .= $asim_l;
    ##
    our $asim1_raw_l = PDL::CCS::Nd->newFromWhich($cc_which1,$dc_raw_l->indexND($dc_which1))->dummy(0,1)->average_nz->_missing(0)->decode;
    our $asim0_raw_l = PDL::CCS::Nd->newFromWhich($cc_which0,$dc_raw_l->indexND($dc_which0))->dummy(0,1)->average_nz->_missing(0)->decode;
    our $asim_raw_l = ($asim1_raw_l+$asim0_raw_l);
    our $asim_raw   = zeroes($NCg,$NCg)+$asim_raw_l->min;
    $asim_raw->dice_axis(0,$gcids)->dice_axis(1,$gcids) .= $asim_raw_l;
    ##
    %iplot = (%iplot,xtitle=>'c1 : wanted',ytitle=>'c2 : predicted');
    imag($asim_raw,{%iplot,title=>'Avg Raw Similarity'});
    imag($asim_raw,{%iplot,title=>'Avg Raw Similarity [sqrt-scale]',itf=>'sqrt'});
    imag($asim_raw,{%iplot,title=>'Avg Raw Similarity [log-scale]',itf=>'log'});
    push(@plots3d,"sim-raw.avg.linear","sim-raw.avg.sqrt","sim-raw.avg.log");
    ##
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity'});
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity [sqrt-scale]',itf=>'sqrt'});
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity [log-scale]',itf=>'log'});
    push(@plots3d,"sim-adj.avg.linear","sim-adj.avg.sqrt","sim-adj.avg.log");
    ##
    our $asim_mask1_raw = ($asim_raw->maximum_ind->slice("*1,")==$asim_raw->xvals);
    imag($asim_raw*$asim_mask1_raw,{%iplot,title=>'Best c1 by c2 ~ Precision [raw]',itf=>'linear'});
    imag($asim_raw*$asim_mask1_raw,{%iplot,title=>'Best c1 by c2 ~ Precision [raw,log-scale]',itf=>'log'});
    push(@plots3d,"sim-raw.best1by2.linear","sim-raw.best1by2.log");
    ##
    our $asim_mask1 = ($asim->maximum_ind->slice("*1,")==$asim->xvals);
    imag($asim*$asim_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [adjusted]'});
    imag($asim*$asim_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [adjusted,log-scale]',itf=>'log'});
    push(@plots3d,"sim-adj.best1by2.linear","sim-adj.best1by2.log");
    ##
    our $asim_mask2_raw = ($asim_raw->xchg(0,1)->maximum_ind==$asim_raw->yvals);
    imag($asim_raw*$asim_mask2_raw,{%iplot,title=>'Best c2 by c1 ~ Recall [raw]'});
    imag($asim_raw*$asim_mask2_raw,{%iplot,title=>'Best c2 by c1 ~ Recall [raw,log-scale]',itf=>'log'});
    push(@plots3d,"sim-raw.best2by1.linear","sim-raw.best2by1.log");
    ##
    our $asim_mask2 = ($asim->xchg(0,1)->maximum_ind==$asim->yvals);
    imag($asim*$asim_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [adjusted]'});
    imag($asim*$asim_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [adjusted,log-scale]',itf=>'log'});
    push(@plots3d,"sim-adj.best2by1.linear","sim-adj.best2by1.log");

    if (0) {
      our $asimx = $asim->pdl;
      our $badc = pdl(long,[0,29,30]);
      $asimx->dice_axis(0,$badc) .= 0 if (all($badc<$asimx->dim(0)));
      $asimx->dice_axis(1,$badc) .= 0 if (all($badc<$asimx->dim(1)));
      imag($asimx,{%iplot,title=>'Avg Adjusted Similarity [safe]'});
      imag($asimx,{%iplot,title=>'Avg Adjusted Similarity [safe,log-scale]',itf=>'log'});
      push(@plots3d,"sim-adj.safe.linear","sim-adj.safe.log");
      ##
      our $asimx_mask1 = ($asimx->maximum_ind->slice("*1,")==$asimx->xvals);
      imag($asimx*$asimx_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [safe]'});
      imag($asimx*$asimx_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [safe,log-scale]',itf=>'log'});
      push(@plots3d,"sim-adj.safe.best1by2.linear","sim-adj.safe.best1by2.log");
      ##
      our $asimx_mask2 = ($asimx->xchg(0,1)->maximum_ind==$asimx->yvals);
      imag($asimx*$asimx_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [safe]'});
      imag($asimx*$asimx_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [safe,log-scale]',itf=>'log'});
      push(@plots3d,"sim-adj.safe.best2by1.linear","sim-adj.safe.best2by1.log");
    }
  }

  ##----------------------------------------------------
  ## histogram confusion matrix, <=
  ## $cc1_hist : [$c1g,$c2g] -> |{$d : $c1g=wanted($d) && dist($d,$c2g)<=dist($d,$c1g)}|
  my $d_dist1 = $dcg_dist->index2d($d2cg->xvals,$d2cg); ##-- [$di] -> dist($c1) : $c1=wanted($di)
  my $dc1_e_mask  = ($dcg_dist <= $d_dist1);
  my $dc1_e_which = whichND($dc1_e_mask);
  my $cc1_e_which = $d2cg->index($dc1_e_which->slice("(0),"))->cat($dc1_e_which->slice("(1),"))->xchg(0,1);
  my $cc1_hist = PDL::CCS::Nd->newFromWhich($cc1_e_which,ones($cc1_e_which->dim(1)))->dummy(0,1)->sumover->decode;
  my $cc1_p12  = $cc1_hist / $cc1_hist->sum;
  my $cc1_p1g2 = ($cc1_hist / $cc1_hist->sumover->slice("*1,"))->inplace->setnantobad->inplace->setbadtoval(0);
  my $cc1_p2g1 = ($cc1_hist / $cc1_hist->xchg(0,1)->sumover)->inplace->setnantobad->inplace->setbadtoval(0);

  ## $cc0_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && dist($d,$c2) < dist($d,$c1)}|
  my $dc0_e_mask  = ($dcg_dist <  $d_dist1);
  my $dc0_e_which = whichND($dc0_e_mask);
  my $cc0_e_which = $d2cg->index($dc0_e_which->slice("(0),"))->cat($dc0_e_which->slice("(1),"))->xchg(0,1);
  my $cc0_hist = PDL::CCS::Nd->newFromWhich($cc0_e_which,ones($cc0_e_which->dim(1)))->dummy(0,1)->sumover->decode;
  my $cc0_p12  = $cc0_hist / $cc0_hist->sum;
  my $cc0_p1g2 = ($cc0_hist / $cc0_hist->sumover->slice("*1,"))->inplace->setnantobad->inplace->setbadtoval(0);
  my $cc0_p2g1 = ($cc0_hist / $cc0_hist->xchg(0,1)->sumover)->inplace->setnantobad->inplace->setbadtoval(0);

  ##-- plot
  our (%iplot);
  %iplot = (DrawWedge=>1, itf=>'sqrt', xtitle=>'c1 : wanted', ytitle=>'c2 : measured');
  if ($plot3d) {
    _usepgplot($dev3d);
    imag($cc1_hist,{%iplot,title=>'Histogram: |{d : dist(d,c2) <= dist(d,c1)}|'});
    imag($cc1_p12,{%iplot, title=>'p(dist(d,c2) <= dist(d,c1))'});
    imag($cc1_p1g2,{%iplot,title=>'p(dist(d,c2) <= dist(d,c1) | c2)'});
    imag($cc1_p2g1,{%iplot,title=>'p(dist(d,c2) <= dist(d,c1) | c1)'});
    push(@plots3d,"hist-cc-le.f12","hist-cc-le.p12","hist-cc-le.p1g2","hist-cc-le.p2g1");
    ##
    imag($cc0_hist,{%iplot,title=>'Histogram: |{d : dist(d,c2) < dist(d,c1)}|'});
    imag($cc0_p12,{%iplot, title=>'p(dist(d,c2) < dist(d,c1))'});
    imag($cc0_p1g2,{%iplot,title=>'p(dist(d,c2) < dist(d,c1) | c2)'});
    imag($cc0_p2g1,{%iplot,title=>'p(dist(d,c2) < dist(d,c1) | c1)'});
    push(@plots3d,"hist-cc-lt.f12","hist-cc-lt.p12","hist-cc-lt.p1g2","hist-cc-lt.p2g1");
  }

  ##----------------------------------------------------
  ## histogram confusion matrix, min
  ##  $ccg_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && $c2=got($d)}|
  #my $d_dist1 = $dc_dist->index2d($d2c->xvals,$d2c); ##-- [$di] -> dist($c1) : $c1=wanted($di)
  my $d_c2 = $d2c_bydist = $dcg_dist->xchg(0,1)->minimum_ind;
  my $d_c1 = $d2cg;
  #my $d_c2_mask   = zeroes(byte,$dc_dist->dims); $d_c2_mask->index2d(xvals(long,$ND),$d_c2) .= 1;
  #my $dg_which = which($d_c1 != $d_c2);
  my $dg_which = $d_c1->xvals;
  my $ccg_which = $d_c1->index($dg_which)->cat($d_c2->index($dg_which))->xchg(0,1);
  my $ccg_hist = PDL::CCS::Nd->newFromWhich($ccg_which,$dg_which->ones->double)->dummy(0,1)->sumover->decode;
  my $ccg_p12  = $ccg_hist / $ccg_hist->sum;
  my $ccg_p1g2 = ($ccg_hist / $ccg_hist->sumover->slice("*1,"))->inplace->setnantobad->inplace->setbadtoval(0);
  my $ccg_p2g1 = ($ccg_hist / $ccg_hist->xchg(0,1)->sumover)->inplace->setnantobad->inplace->setbadtoval(0);

  ##-- plot
  #our (%iplot);
  #%iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'c1 : wanted', ytitle=>'c2 : got');
  %iplot = (DrawWedge=>1, itf=>'log', xtitle=>'c1 : wanted', ytitle=>'c2 : got');
  if ($plot3d) {
    _usepgplot($dev3d);
    imag($ccg_hist,{%iplot,title=>'Histogram: |{ d : wanted(d)=c1 & got(d)=c2 }|'});
    imag($ccg_p12,{%iplot, title=>'p(Wanted=c1,Got=c2)'});
    imag($ccg_p1g2,{%iplot,title=>'p(Wanted=c1|Got=c2) ~ Precision'});
    imag($ccg_p2g1,{%iplot,title=>'p(Got=c2|Wanted=c1) ~ Recall'});
    imag(F1($ccg_p2g1,$ccg_p1g2), {%iplot,title=>'F1(p(Wanted=c1|Got=c2),p(Got=c2|Wanted=c1))'});
    push(@plots3d,"hist-wg.f12","hist-wg.p12","hist-wg.p1g2","hist-wg.p2g1");
  }

}


##==============================================================================
## MAIN
##==============================================================================

##-- input
our $efile = @ARGV ? shift(@ARGV) : '-';

##-- out dir
$outdir = "${efile}.plots.d" if (!defined($outdir));
if (! -d $outdir) {
  system("mkdir -p '$outdir'")==0 || die("$0: mkdir -p '$outdir' failed: $!");
}

##-- load input file
print STDERR "$0: load($efile)\n";
our $eval = DocClassify::Eval->loadFile("$efile")
  or die("$0: Eval->loadFile($efile) failed: $!");

##-- change directory
#chdir($outdir) or die("$0: chdir($outdir) failed: $!");

##-- plot
print STDERR "$0: doplots()\n";
{
  local $^W=0;
  doplots($eval);
}

##-- plot: flush?!
#dev('/NULL');
_closepgplot();
sleep(5);

##-- error sub
sub odear { die($@); }
#sub odear { warn($@); }

##-- extract individual plot data: 2d
print STDERR "$0: extract data: 2d\n";
foreach $i (0..$#plots2d) {
  my $out = sprintf("$outdir/plot2d.%0.2d.%s", $i, ($plots2d[$i] ? $plots2d[$i] : 'data'));
  print STDERR "$0: PLOT2D[$i/$#plots2d]: $out\n";

  system("psselect -p".($i+1)." pgplot.ps > '$out.raw.ps'")==0
    or odear("$0: failed to create $out.raw.ps: $!");

  #system("gs -dNOPAUSE -dBATCH -sDEVICE=epswrite -sOutputFile='$out.eps' '$out.raw.ps'")==0
  #  or die("$0: failed to create $out.eps: $!");
  ##--
  system("ps2epsi '$out.raw.ps' '$out.raw.epsi'")==0
    or odear("$0: failed to create $out.raw.epsi: $!");

  system("ps2eps -f -B -R + '$out.raw.epsi'")==0 && move("$out.raw.epsi.eps","$out.eps")
    or odear("$0: failed to create $out.eps: $!");

  system("pstopnm -stdout '$out.eps' | pnmrotate -90 | pnmtopng > '$out.png'")==0
    or odear("$0: failed to create $out.png: $!");
}

##-- plots2d: cleanup
move("pgplot.ps",$outdir);
#unlink("$outdir/pgplot.ps");


##-- extract individual plot data: 3d
print STDERR "$0: extract data: 3d\n";
foreach $i (0..$#plots3d) {
  my $in  = ($i==0 ? "pgplot.xwd" : "pgplot.xwd_".($i+1));
  my $out = sprintf("$outdir/plot3d.%0.2d.%s", $i, ($plots3d[$i] ? $plots3d[$i] : 'data'));
  print STDERR "$0: PLOT3D[$i/$#plots3d]: $out\n";

  system("xwdtopnm $in > $out.pnm")==0
    or odear("$0: failed to create $out.pnm: $!");

  system("pnmtopng $out.pnm > $out.png")==0
    or odear("$0: failed to create $out.png: $!");

  system("gzip $out.pnm")==0
    or odear("$0: failed to create $out.pnm.gz: $!");

  ##-- plots3d: cleanup
  move("$in","$out.xwd");
  #unlink("$out.xwd");
}


=pod

=head1 NAME

dc-eval-plots.perl - create some plots from a DocClassfile eval.xml file

=head1 SYNOPSIS

 dc-eval-plots.perl [OPTIONS] [EVAL_FILE]

 Options:
  -help                  # this help message
  -verbose LEVEL         # verbosity level
  -output-dir DIR        # output directory (default=EVAL_FILE.plots.d/)

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
