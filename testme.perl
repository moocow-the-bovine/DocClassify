#!/usr/bin/perl -w

use lib qw(. ./MUDL);

use PDL;
use Storable;
use PDL::IO::Storable;

use MUDL;
use MUDL::SVD;
use MUDL::Cluster::Tree;

use PDL::Ngrams;
use PDL::VectorValued;
use PDL::CCS;
use PDL::CCS::Nd;
use IO::File;

use Encode qw(encode decode);
use File::Basename qw(basename);

BEGIN { $,=' '; }

##======================================================================
## utils

sub usepgplot {
  require PDL::Graphics::PGPLOT;
  require PDL::Graphics::LUT;    ##-- for color tables used by e.g. imag()
  require PDL::Image2D;          ##-- for box2d, patch2d
  PDL::Graphics::PGPLOT->import();
  PDL::Graphics::LUT->import();
  PDL::Image2D->import();
  #dev('/XWINDOW');
  dev('/XS');
  #ctab('Fire');             ##-- ?
  #ctab('gray');
  ctab(lut_data('smooth2')); ##-- color table similar to gnuplot 'pm3d' default; see string list lut_names() for more
  autolog(1);
}


##======================================================================
sub test1 {
  #my $go = Storable::retrieve('wikidata.bin');
  #my $go = Storable::retrieve('test1.bin');
  #my $go = Storable::retrieve('wikidata.mf2.bin');
  my $go = Storable::retrieve('wikidata.mf5.bin');

  my $NT  = $go->{tenum}->size;
  my $NG  = $go->{genum}->size;
  my $tgf = $go->{tgf};

  my $N   = $tgf->sum;
 print STDERR "NT=$NT , N=$N , NT/N=", ($NT/$N), "\n";

  my $svd = MUDL::SVD->new(
			   #r => 200, ##-- number of target dimensions
			   r => 16,
			  );
  $svd->computeccs_nd( $tgf, 0 );
  my ($u,$s,$v) = @$svd{qw(u sigma v)};
  ##
  #my $tgf1 = $svd->apply($tgf); ##-- SLOW, then MEM CRASH (w/ wikidata, minfreq=1)
  #my $tgf2 = $svd->unapply($tgf1);
  ##
  my $tgfd  = $tgf->decode;          ##-- works w/ wikidata, minfreq=5
  my $tgfd1 = $svd->apply($tgfd);    ##-- fast
  my $tgfd2 = $svd->unapply($tgfd1); ##-- also fast
  my $tgf_chisq = (($tgfd-$tgfd2)**2)->flat->sumover;  ##-- ~=460K

  ##-----
  ## use SVD on log-freqs: re-mapping (unapply()) looks much better!
  my $ltgf  = ($tgf+1)->log;
  my $lsvd  = $svd->new(r=>$svd->{r});
  $lsvd->computeccs_nd($ltgf,0);
  my ($lu,$ls,$lv) = @$lsvd{qw(u sigma v)};
  ##
  #my $ltgf1 = $lsvd->apply($ltgf);
  #my $ltgf2 = $lsvd->unapply($ltgf1);
  ##
  my $ltgfd  = $ltgf->decode;          ##-- works w/ wikidata, minfreq=5
  my $ltgfd1 = $lsvd->apply($ltgfd);    ##-- fast
  my $ltgfd2 = $lsvd->unapply($ltgfd1); ##-- also fast
  my $ltgf_chisq = (($ltgfd-$ltgfd2)**2)->flat->sumover; ##-- ~=13K

  ##--- do some plots
  usepgplot;
  $w = pgwin(DEVICE=>'/XS');
  #$w = pgwin(DEVICE=>'/WD'); ##-- xwindow dump, handle with e.g. xwdtopnm pgplot.xwd | pnmtopng > pgplot.png
  $w->imag($tgfd,  {itf=>'log',xtitle=>'Term',ytitle=>'Group',title=>'Raw Group-Term Matrix'});
  $w->imag($ltgfd1,{itf=>'linear',xtitle=>'Feature',ytitle=>'Group',title=>'SVD-Reduced Group-Feature Matrix'});
  $w->imag($ltgfd2,{itf=>'linear',xtitle=>'Term',ytitle=>'Group',title=>'Re-Expanded Group-Term Matrix'});


  ##----
  ## try clustering groups, on a whim
  my $cm = MUDL::Cluster::Tree->new(dclass=>'c',data=>$ltgfd2,enum=>$go->{genum});
  $cm->cluster();
  $cm->view();
  ##
  my $cm2 = MUDL::Cluster::Tree->new(dclass=>'c',data=>$ltgfd,enum=>$go->{genum});
  $cm2->cluster();
  $cm2->view();

  print STDERR "$0: test1() done -- what now?\n";
}
test1();


##======================================================================
## MAIN (dummy)
foreach $i (1..3) {
  print STDERR "--dummy[$i]--\n";
}
