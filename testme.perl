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
  require PDL::Graphics::PGPLOT::Window;
  require PDL::Graphics::LUT;    ##-- for color tables used by e.g. imag()
  require PDL::Image2D;          ##-- for box2d, patch2d
  PDL::Graphics::PGPLOT->import();
  PDL::Graphics::PGPLOT::Window->import();
  PDL::Graphics::LUT->import();
  PDL::Image2D->import();
  #dev('/XWINDOW');
  dev('/XS');
  #ctab('Fire');             ##-- ?
  #ctab('gray');
  ctab(lut_data('smooth2')); ##-- color table similar to gnuplot 'pm3d' default; see string list lut_names() for more
  autolog(1);
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


##======================================================================
sub test1 {
  #our $cofile = 'wikidata.bin';
  #our $cofile = 'test1.bin';
  #our $cofile = 'wikidata.mf0.bin'; ##-- == wikidata.mf1.bin
  #our $cofile = 'wikidata.mf1.bin'; ##-- NT=567089 , N=7305120 , NT/N= 0.0776289780318462
  #our $cofile = 'wikidata.mf2.bin'; ##-- NT=270109 , N=7008140 , NT/N= 0.0385421809495815
  #our $cofile = 'wikidata.mf5.bin'; ##-- NT=115301 , N=6603977 , NT/N= 0.0174593279171021
  ##--
  our $cofile = 'wikidata2.mf1.bin'; ##-- NT=?
  our $co = Storable::retrieve($cofile);

  my $NT  = $co->{tenum}->size;
  my $NG  = $co->{cenum}->size;
  my $tcf = $co->{tcf};

  my $N   = $tcf->sum;
  print STDERR "NT=$NT , N=$N , NT/N=", ($NT/$N), "\n";

  my $svd = MUDL::SVD->new(
			   #r => 200, ##-- number of target dimensions
			   r => 16,
			  );
  if (0) {
    ##-----
    ## use SVD on raw-freqs (chisq~=460k for wikidata, minfreq=5, svdr=16)
    $svd->computeccs_nd( $tcf, 0 );
    our ($u,$s,$v) = @$svd{qw(u sigma v)};
    ##
    #my $tcf1 = $svd->apply($tcf); ##-- SLOW, then MEM CRASH (w/ wikidata, minfreq=1)
    #my $tcf2 = $svd->unapply($tcf1);
    ##
    our $tcfd  = $tcf->decode;          ##-- works w/ wikidata, minfreq=5
    our $tcfd1 = $svd->apply($tcfd);    ##-- fast
    our $tcfd2 = $svd->unapply($tcfd1); ##-- also fast
    our $tcf_chisq = (($tcfd-$tcfd2)**2)->flat->sumover;  ##-- ~=460K
  }

  if (1) {
    ##-----
    ## use SVD on log-freqs (chisq~=13K for wikidata, minfreq=5, svdr=16)
    our $ltcf  = ($tcf+1)->log;
    our $lsvd  = $svd->new(r=>$svd->{r});
    $lsvd->computeccs_nd($ltcf,0); ##-- ok: mf=5, mf=2, mf=1
    our ($lu,$ls,$lv) = @$lsvd{qw(u sigma v)};
    ##
    #our $ltcf1 = $lsvd->apply($ltcf);
    #our $ltcf2 = $lsvd->unapply($ltcf1);
    ##
    our $ltcfd  = $ltcf->decode;          ##-- ok: mf=5, mf=2, mf=1
    our $ltcfd1 = $lsvd->apply($ltcfd);    ##-- fast, ok: mf=5, mf=2, mf=1
    our $ltcfd2 = $lsvd->unapply($ltcfd1); ##-- also fast
    our $ltcf_chisq = (($ltcfd-$ltcfd2)**2)->flat->sumover; ##-- ~=13K
  }

  ##--- do some plots
  if (1) {
    useplplot;
    #_plpoints($ltcf->_nzvals->ranks1_dsc->log10, $ltcf->_nzvals/log(10), XBOX=>'bnstl',YBOX=>'bnstl')->close;
    my $w=PDL::Graphics::PLplot->new(DEV=>'xwin',FILE=>':0');
    #$w->plimage($ltcfd, map {PDL->topdl($_)} 0,$ltcfd->dim(0), 0,$ltcfd->dim(1), $ltcfd->minmax, 0,$ltcfd->dim(0), 0,$ltcfd->dim(1), $ltcfd->minmax); ##-- ???
    ##-- need to set up plot box here! plenv(), plw3d()
    plmesh((map {sequence($_)} $ltcfd->dims), $ltcfd, pdl(long,0));
    plend(); ##-- end current plot
  }
  if (1) {
    usepgplot;
    our ($w);
    our %plot = (itf=>'linear',drawwedge=>1,xtitle=>'Term',ytitle=>'Group',title=>'Group-Term Matrix');
    $w = pgwin(DEVICE=>'/XS'); ##-- x-server output, re-usable window
    #$w = pgwin(DEVICE=>'/CPS'); #-- bad postscript output ?!
    #$w = pgwin(DEVICE=>'/PGMF'); #-- pgplot metafile (convertible to???)
    #$w = pgwin(DEVICE=>'/WD'); ##-- xwindow dump, handle with e.g. xwdtopnm pgplot.xwd | pnmtopng > pgplot.png
    $w->ctab(lut_data('smooth2'));
    $w->imag($ltcfd, {%plot,xtitle=>'Term',title=>'Raw Group-Term Matrix'});
    $w->imag($ltcfd1,{%plot,xtitle=>'Feature',title=>'SVD-Reduced Group-Feature Matrix'});
    $w->imag($ltcfd2,{%plot,xtitle=>'Term',title=>'Re-Expanded Group-Term Matrix'});
  }


  ##----
  ## try clustering groups, on a whim: $cm1 (WITH svd) looks pretty good, but $cm2 is not so hot (NO svd)
  if (1) {
    our $cm = MUDL::Cluster::Tree->new(dclass=>'c',data=>$ltcfd2,enum=>$co->{genum});
    $cm->cluster();
    $cm->view();
    ##
    our $cm2 = MUDL::Cluster::Tree->new(dclass=>'c',data=>$ltcfd,enum=>$co->{genum});
    $cm2->cluster();
    $cm2->view();
  }

  print STDERR "$0: test1() done -- what now?\n";
}
#test1();

##======================================================================
## test: classification: utils

## $cb = cbLoadBin($catBinFile)
sub cbLoadBin {
  my $cbfile = shift;
  my $cb = Storable::retrieve($cbfile)
    or die("$0: Storable::retrieve() fialed for '$cbfile': $!");
  return $cb;
}

## $cb = cbSvdCompute($cb,%opts)
##  + %opts (overrides %$cb)
##     svdlog => $bool, ##-- default=1
##     svdr   => $r,    ##-- default=16
sub cbSvdCompute {
  my ($cb,%opts) = @_;
  my $svdlog = defined($opts{svdlog}) ? $opts{svdlog} : (defined($cb->{svdlog}) ? $cb->{svdlog} : 1);
  my $svdr   = defined($opts{svdr})   ? $opts{svdr}   : (defined($cb->{svdr})   ? $cb->{svdr}   : 16);
  @$cb{qw(svdlog svdr)} = ($svdlog,$svdr);

  ##-- create $cb->{svd}, $cb->{xcf} ~= $cb->{svd}->apply($cb->{tcf})
  my $data = ($svdlog ? ($cb->{tcf}+1)->log : $cb->{tcf});
  my $svd  = $cb->{svd} = MUDL::SVD->new(r=>$svdr);
  $svd->computeccs_nd($data);
  $cb->{xcf} = $svd->apply($data->decode);

  return $cb;
}

## $docPdlReduced = cbSvdApply($cb,$docPdlRaw)
sub cbSvdApply {
  my ($cb,$dpdl) = @_;
  $dpdl = cbDocDenseRaw($cb,$dpdl) if (!UNIVERSAL::isa($dpdl,'PDL')); ##-- $dpdl passed as doc-hash?
  $dpdl = $dpdl->todense if (UNIVERSAL::isa($dpdl,'PDL::CCS::Nd'));   ##-- convert to dense
  cbSvdCompute($cb) if (!$cb->{svd});
  $dpdl = ($dpdl+1)->log if ($cb->{svdlog});
  return $cb->{svd}->apply($dpdl->slice(":,*1"));
}

## \%doc = docLoadCsv($filename_or_fh)
##  + %doc: (docFile=>$docFile, catName=>$catName, tf=>\%term2freq, N=>$N, ...)
sub docLoadCsv {
  my $file = shift;
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  die("$0: open failed for file '$file': $!") if (!defined($fh));
  $fh->binmode(':utf8');
  ##
  my $doc = {docFile=>$file,};
  $doc->{catName} = <$fh>;
  $doc->{catName} =~ s/^\d+\s*//;
  chomp($doc->{catName});

  ##-- parse file data (TAB-separated: TERM FREQ ...)
  my $tf = {};
  my $N  = 0;
  my ($line,$term,$freq,$rest);
  while (defined($line=<$fh>)) {
    chomp($line);
    next if ($line =~ /^\s/);
    ($term,$freq,$rest) = split(/\t/,$line,3);
    $tf->{$term} += $freq if ($freq>0);
    $N += $freq;
  }
  $fh->close() if (!ref($file));

  @$doc{qw(tf N)} = ($tf,$N);
  return $doc;
}

## $docPdlRaw = cbDocPdlRaw($cb,$doc, $want_ccs=0)
##  + $docPdlRaw is dense or CCS pdl($NT): [$tid]=>f($tid,$doc)
sub cbDocPdlRaw {
  my ($cb,$doc, $as_ccs) = @_;
  my $tenum = $cb->{tenum};
  my $dtf_wt = pdl(long,   grep{defined($_)} @{$tenum->{sym2id}}{keys(%{$doc->{tf}})});
  my $dtf_nz = pdl(double, @{$doc->{tf}}{@{$tenum->{id2sym}}[$dtf_wt->list]});
  if ($as_ccs) {
    return PDL::CCS::Nd->newFromWhich($dtf_wt->slice("*1,"),$dtf_nz,dims=>pdl(long,[$cb->{tenum}->size]),missing=>0);
  } else {
    ##-- dense mode
    my $dtf = zeroes(double,$tenum->size);
    $dtf->index($dtf_wt) .= $dtf_nz;
    return $dtf;
  }
}


##======================================================================
## test: classification: top-level
sub test_vzdata {
  my ($cbfile,$test_dir) = @ARGV;

  if (!defined($test_dir)) {
    #$test_dir = 'vzdata-nn';
    $test_dir = 'test-nn';
  }
  my $test_ext = '.csv';
  my $out_ext  = '.svdcat';

  ##-- verbosity
  my $verbose =1;
  my $warn_on_null_doc = 1;
  my $ignore_null_docs = 1;

  ##-- base data
  if (!defined($cbfile)) {
    #$cbfile = 'wikidata.mf5.bin';
    #$cbfile = 'wikidata.mf1.bin';
    #$cbfile = 'vzdata-nn.mf5.bin';
    #$cbfile = 'vzdata-nn.mf1.bin';
    $cbfile = 'test-nn.mf1.bin';
    ##--
    #$cbfile = 'wikidata2.mf1.bin';
  }
  print STDERR "$0: cbLoadCsv($cbfile)\n" if ($verbose);
  my $cb = cbLoadBin($cbfile) or die("$0: cbLoadBin() failed for '$cbfile': $!");
  my $cenum = $cb->{cenum};

  ##-- svd
  my $dosvd =1;
  #my $dosvd =0;
  my $dolog =1;
  my $svdr  =64;
  if ($dosvd) {
    print STDERR "$0: cbSvdCompute(svdlog=>$dolog, svdr=>$svdr)\n" if ($verbose);
    cbSvdCompute($cb, svdr=>$svdr, svdlog=>$dolog);
  } elsif ($dolog) {
    print STDERR "$0: NO SVD, computing log-frequencies\n" if ($verbose);
    $cb->{xcf}=($cb->{tcf}+1)->log->decode;
  } else {
    print STDERR "$0: NO SVD, using raw frequencies\n" if ($verbose);
    $cb->{xcf}=$cb->{tcf}->decode;
  }

  ##-- cluster distance measure, centroid data
  #my $cdclass = ($dosvd ? 'c' : 'Pearson');
  my $cdclass = 'c';
  my $cd = $cb->{cdf} = MUDL::Cluster::Distance->new(class=>$cdclass,link=>'avg');
  my $cdata = $cb->{xcf};

  ##-- smoothing hack: global frequencies
  my $do_smooth = 0;
  my $g_tf = $cb->{tcf}->decode->xchg(0,1)->sumover;
  my $N    = $g_tf->sum;
  #my $g_pf = $g_tf/$N;
  #my $g_tx = cbSvdApply($g_tf);
  #my $p_global = $cb->{tenum}->size / $N;
  my $smooth_N = 0.5;
  my $smooth_tf = ($g_tf / $N * $smooth_N);

  ##-- ye olde loope
  print STDERR "$0: TEST_DIR: $test_dir\n" if ($verbose);
  my ($docfile, $doc,$cid_wanted, $dtf,$dtN,$dtx, $cdmat,$cid_got);
  my ($outfh);
  my ($ntp_docs,$n_docs, $ntp_toks,$n_toks) = (0,0,0,0);
  foreach $docfile (glob("$test_dir/*$test_ext")) {
    print STDERR "$0: TEST_FILE: $docfile" if ($verbose);
    $doc = docLoadCsv($docfile) or die("$0: load failed for '$docfile': $!");
    if (!defined($cid_wanted=$cenum->{sym2id}{$doc->{catName}})) {
      print STDERR " > SKIPPED (cat=$doc->{catName})\n" if ($verbose);
      next;
    }
    #$dtf = cbDocPdlRaw($cb,$doc, ($dosvd ? 0 : 1));
    $dtf = cbDocPdlRaw($cb,$doc, 0);
    $dtN = $dtf->sum;
    warn("$0: null vector for test document '$docfile'") if ($warn_on_null_doc && $dtN==0); ##-- warning danger
    if ($ignore_null_docs && $dtN==0) {
      print STDERR " > SKIPPED (null vector)\n" if ($verbose);
      next;
    }

    ##-- smooth
    $dtf += $smooth_tf if ($do_smooth);

    ##-- SVD
    $dtx = $dosvd ? cbSvdApply($cb,$dtf) : ($dolog ? ($dtf+1)->log : $dtf)->dummy(1,1);

    ##-- compute document-cluster distasnce
    $cdmat = $cd->clusterDistanceMatrix(data=>$dtx,cdata=>$cdata);
    $cid_got = $cdmat->flat->minimum_ind->sclr;

    ##-- output data
    $outfh = IO::File->new(">$docfile$out_ext");
    die("$0: open failed for output file '$docfile$out_ext': $!") if (!$outfh);
    $outfh->binmode(':utf8');
    {
      local $,='';
      $outfh->print("FILE\t", basename($doc->{docFile}), "\n",
		    "WANTED\t", $cid_wanted, "\t", $cenum->{id2sym}[$cid_wanted], "\n",
		    "GOT\t", $cid_got, "\t", $cenum->{id2sym}[$cid_got], "\n",
		    "GOOD\t", ($cid_wanted==$cid_got ? "1" : "0"), "\n",
		    "N_KNOWN\t", $dtN, "\n",
		    map {
		      ("DISTANCE\t", $cdmat->at($_,0), "\t", $_, "\t", ($cenum->{id2sym}[$_]||'???'), "\n")
		    } ($cdmat->flat->qsorti->list)
		   );
    }
    $outfh->close();

    ##-- track accuracy
    $n_toks += $doc->{N};
    $n_docs++;
    if ($cid_wanted==$cid_got) {
      print STDERR " +\n";
      $ntp_toks += $doc->{N};
      $ntp_docs++;
    } else {
      print STDERR "\n";
    }

    ##-- incremental report
    if ($verbose && ($n_docs % 10) == 0) {
      print STDERR sprintf("\n---- n_docs=$n_docs, ntp_docs=$ntp_docs, acc=%6.2f %% ----\n\n", 100*$ntp_docs/$n_docs);
    }
  }

  ##-- total eval
  my $llen = 32;
  my $ilen =  6;
  my $flen = "6.2";
  print
    ("$0 Summary\n",
     sprintf(" + %${llen}s = %${ilen}d (%${flen}f %%)\n", "Docs (Total)", $n_docs, 100*$n_docs/$n_docs),
     sprintf(" + %${llen}s = %${ilen}d (%${flen}f %%)\n", "Docs (Good)",  $ntp_docs, 100*$ntp_docs/$n_docs),
     sprintf(" + %${llen}s = %${ilen}d (%${flen}f %%)\n", "Tokens (Total)",  $n_toks, 100*$n_toks/$n_toks),
     sprintf(" + %${llen}s = %${ilen}d (%${flen}f %%)\n", "Tokens (Good)",  $ntp_toks, 100*$ntp_toks/$n_toks),
    );

  print STDERR "$0: test_vzdata(test_dir=>$test_dir) done: what now?\n";
  exit(0);
}
test_vzdata();


##======================================================================
## MAIN (dummy)
foreach $i (1..3) {
  print STDERR "--dummy[$i]--\n";
}
