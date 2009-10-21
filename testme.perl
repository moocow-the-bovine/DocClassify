#!/usr/bin/perl -w

use lib qw(. ./MUDL);

use PDL;
use Storable;
use PDL::IO::Storable;

use MUDL;
use MUDL::SVD;

use PDL::Ngrams;
use PDL::VectorValued;
use PDL::CCS;
use PDL::CCS::Nd;
use IO::File;

use Encode qw(encode decode);
use File::Basename qw(basename);

use PDL::Graphics::PGPLOT;

BEGIN { $,=' '; }

##======================================================================
sub test1 {
  my $go = Storable::retrieve('wikidata.bin');
  #my $go = Storable::retrieve('test1.bin');

  my $NT  = $go->{tenum}->size;
  my $NG  = $go->{genum}->size;
  my $gtf = $go->{gtf};

  my $N   = $gtf->sum;
  print STDERR "NT=$NT , N=$N , NT/N=", ($NT/$N), "\n";

  my $tgf = $gtf->xchg(0,1)->make_physically_indexed;
  my $svd = MUDL::SVD->new(
			   #r => 200, ##-- number of target dimensions
			   r => 16,
			  );
  #$svd->computeccs_nd( $gtf, 1 );
  $svd->computeccs_nd( $tgf, 0 );
  my ($u,$s,$v) = @$svd{qw(u sigma v)};
  ##
  my $tgf1 = $svd->apply($tgf); ##-- MEM CRASH!
  #my $tgf2 = $svd->unapply($tgf1);

  ##-----
  my $ltgf = ($tgf+1)->log;
  my $lsvd = $svd->new(r=>16);
  $lsvd->computeccs_nd($ltgf,0);
  my ($lu,$ls,$lv) = @$lsvd{qw(u sigma v)};
  ##
  #my $ltgf1 = $lsvd->apply($ltgf);
  #my $ltgf2 = $lsvd->unapply($ltgf1);

  print STDERR "$0: test1() done -- what now?\n";
}
test1();


##======================================================================
## MAIN (dummy)
foreach $i (1..3) {
  print STDERR "--dummy[$i]--\n";
}
