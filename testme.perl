#!/usr/bin/perl -w

use lib qw(. ./MUDL);

use DocClassify;
use DocClassify::Utils qw(:all);

use MUDL;
use MUDL::SVD;
use MUDL::PDL::Stats;
use MUDL::PDL::Smooth;
use MUDL::Cluster::Tree;

use PDL;
use PDL::Ngrams;
use PDL::VectorValued;
use PDL::CCS;
use PDL::CCS::Nd;
use IO::File;

use Encode qw(encode decode);
use File::Basename qw(basename);

use Benchmark qw(cmpthese timethese);

BEGIN { $,=' '; }

##======================================================================
## utils

sub _usepgplot {
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
#test_vzdata();

##======================================================================
## test dump pdl

use Data::Dumper;

BEGIN { *PDL::ddFreeze = \&PDL::ddFrozen::ddFreeze; }

package PDL::ddFrozen;
BEGIN { our %frozen = qw(); }
sub ddFreeze {
  my $pdl = shift;
  return if (ref($pdl) ne 'PDL');
  my $ref = { cref=>$$pdl, sto=>Storable::freeze($pdl) };
  $$pdl = $ref;
  bless($pdl,'PDL::ddFrozen');
  $frozen{"$pdl"} = $pdl;
}
sub ddToast {
  my $fpdl = shift;
  return $fpdl if (ref($fpdl) ne 'PDL::ddFrozen');
  my $thaw = Storable::thaw($$fpdl->{sto});
  $$fpdl = $$thaw;
  bless($thaw,'PDL::ddFrozen::NoFree');
  return bless($fpdl,'PDL');
}
sub unfreeze {
  foreach (@_) {
    delete($frozen{"$_"});
    $$_ = $$_->{cref};
    bless($_,'PDL');
  }
}
sub unfreezeAll {
  unfreeze($_) foreach (values(%frozen));
  %frozen = qw();
}

sub UNIVERSAL::ddFreeze { ; }
sub UNIVERSAL::ddToast { $_[0]; }

package main;

sub test_dump_pdl {
  my $p = sequence(4);
  my $h = { str=>'foo', p=>$p, obj=>bless([],'MyObject'), };
  my $d = Data::Dumper->new([$h],['h2']);
  $d->Freezer('ddFreeze');
  $d->Toaster('ddToast');
  my $hs = $d->Dump;
  PDL::ddFrozen::unfreezeAll(); ##-- avoid memory leak, must call after Dump()
  eval "$hs";
  #x $h2

  ##-- try with ccs: ok (needs UNIVERSAL::ddFreeze, UNIVERSAL::ddToast)
  my $ccs = PDL::CCS::Nd->newFromDense( sequence(4,3)%2 );
  $d->Reset->Values([$ccs])->Names(['ccs2']);
  my $ds = $d->Dump;
  PDL::ddFrozen::unfreezeAll(); ##-- avoid memory leak, must call after Dump()
  eval "$ds";
  warn("$@") if ($@);
  #x $ccs2

  ##-- try using 2 dumper calls & Seen()
  ##  + ugly; also doesn't work very well
  $d->Reset->Values([$h])->Names(['h2'])->Freezer('')->Toaster('');
  my $hs0 = $d->Dump;
  my @seen0 = $d->Seen;
  my $seen1 = qw();
  my ($name);
  foreach (grep {ref($_) eq 'PDL'} @seen0) {
    $name = "Storable::thaw(q(".Storable::freeze($_)."))";
    $seen1{$name} = $_;
  }
  $d->init_refaddr_format(); ##-- bug in Data::Dumper v2.121_08
  $d->Reset->Seen(\%seen1); ##-- errors?
  my $hs1 = $d->Dump;
  print $hs1;

  print STDERR "$0: test_dump_pdl() done -- what now?\n";
}
#test_dump_pdl();

##======================================================================
## test: signature counting

sub getsig_findnodes {
  my $xdoc = shift;
  my $tf = {};
  my ($ts);
  foreach (@{$xdoc->findnodes('//w')}) {
    $ts = join("\t", map {$_->name.'='.$_->value} $_->attributes);
    $tf->{$ts}++;
  }
  return $tf;
}

use XML::LibXML;
use XML::LibXSLT;
sub getsig_xsl {
  my $xdoc = shift;
  our ($stylesheet);
  my $xxdoc = $stylesheet->transform($xdoc);
  my $xxstr = $stylesheet->output_string($xxdoc);

  my $tf = {};
  $tf->{$_}++ foreach ($xxstr =~ /^(?!%%).+$/mg);
  return $tf;
}

sub test_getsig {
  my $xfile = 'test-big.xml';
  my $xparser = libxmlParser();
  my $xdoc = $xparser->parse_file($xfile) or die("$0: parse failed for '$xfile': $!");

  our $xslt = XML::LibXSLT->new();
  our $style_file = 'dc-xml2t.xsl';
  our $style_doc = $xparser->parse_file($style_file) or die("$0: parse failed for '$style_file': $!");
  our $stylesheet = $xslt->parse_stylesheet($style_doc) or die("$0: style parse failed for '$style_file': $!");

  my $s1 = getsig_findnodes($xdoc);
  my $s2 = getsig_xsl($xdoc);

  cmpthese(3, {'findnodes'=>sub{getsig_findnodes($xdoc)}, 'xsl'=>sub{getsig_xsl($xdoc)}, });
  ## $xfile='test-small.xml', n=100
  ##             Rate findnodes       xsl
  ## findnodes 73.0/s        --      -81%
  ## xsl        385/s      427%        --
  ##----
  ## $xfile='test-big.xml', n=3
  ##           s/iter findnodes       xsl
  ## findnodes   6.29        --      -48%
  ## xsl         3.30       91%        --
}
#test_getsig();


##======================================================================
## test: signature acquisition, lemmatization

sub test_sig_lemmatize {
  my $docfile = 'test-small.xml';
  my $doc = DocClassify::Document->new(file=>$docfile);
  my $sig = $doc->typeSignature();
  $sig->lemmatize;
  $sig->saveFile("$docfile.types.csv",lemmatized=>0);
  $sig->saveFile("$docfile.lemmata.csv",lemmatized=>1);
  $sig->saveBinFile("$docfile.sig.bin");

  my $sig2 = $sig->clone->unlemmatize;
  $sig2->lemmatize( posRegex=>qr/^(?:N|VV|ADJ)/ );
  $sig->saveFile("$docfile.types2.csv",lemmatized=>0);
  $sig->saveFile("$docfile.lemmata2.csv",lemmatized=>1);

  $sig2 = ref($sig)->new;
  $sig2->loadFile("$docfile.types.csv",lemmatized=>0);
  $sig2->loadFile("$docfile.lemmata.csv",lemmatized=>1);

  #my $sig2s = $sig2->saveString(mode=>'csv',lemmatized=>1);

  ##-- bench
  timethese(-3, {
		 #'doc:new' => sub { $doc = DocClassify::Document->new(file=>$docfile); },
		 'doc:new+xml' => sub { $doc = DocClassify::Document->new(file=>$docfile)->xmlDoc; },
		 'doc:new+sig' => sub { $doc = DocClassify::Document->new(file=>$docfile)->typeSignature; },
		 'sig:lemmatize' => sub { $sig->unlemmatize->lemmatize; },
		 'sig:load:bin' => sub { $sig2 = ref($sig)->new->loadFile("$docfile.sig.bin"); },
		 'sig:load:csv' => sub { $sig2 = ref($sig)->new->loadCsvFile("$docfile.types.csv",lemmatized=>0)->loadCsvFile("$docfile.lemmata.csv",lemmatized=>1); },
		});

  print STDERR "test_sig_lemmatize() done: what now?\n";
}
#test_sig_lemmatize();

##======================================================================
## test: eval: i/o

sub test_eval_io {
  my $efile = 'vzdata-testset.lsimap-r128-avg-H.eval.xml';
  #my $eval = DocClassify::Eval->loadFile($efile);
  ##
  my $cfile1 = 'vzdata-testset.corpus.xml';
  my $cfile2 = 'vzdata-testset.lsimap-r128-avg-H.xml';
  my $c1 = DocClassify::Corpus->loadFile($cfile1);
  my $c2 = DocClassify::Corpus->loadFile($cfile2);
  $eval = DocClassify::Eval->new()->compare($c1,$c2, label1=>$cfile1, label2=>$cfile2);
  $eval->saveXmlFile($efile);
  ##
  $eval = ref($eval)->loadXmlFile($efile) or die("$0: Eval->load($efile) failed: $!");
  $eval->saveXmlFile('-', saveDocs=>0) or die("$0: Eval->save('-') failed. $!");
  print STDERR "$0: test_eval_io() done: what now?\n";
}
#test_eval_io();

##======================================================================
## test: group sizes

#use MUDL::PDL::Plot;
sub test_group_sizes {
  require MUDL::PDL::Plot;
  my $nbfile = 'plots/nbytes.dat';
  my $nb = rcols($nbfile);
  usepgplot();
  points($nb->qsort,{axis=>'logy'}); ##-- looks like 'probit' function

  ##-- histogram
  bin(hist($nb->log10));  ##-- looks pretty normal

  ##-- q-q plot
  qqplot($nb->log); ##-- pretty good; tails a bit long, but quite a good fit

  print STDERR "$0: test_group_sizes() done -- what now?\n";
}
#test_group_sizes();

##======================================================================
## test svd (again)
sub test_svd {
  my ($m,$n) = (7,5); ## $m~Terms, $n~Docs
  my $a = sequence($n,$m)+1;
  my $r = 2;
  my $rm1 = $r-1;

  ##-- built-in "correct" version
  my ($u0,$s0,$v0) = svd($a); ## $u0($r<=$n,$m), $s0($r<=$n), $v0($r<=$n,$n)
  my $ss0 = stretcher($s0);
  my $aa0 = $u0 x $ss0 x $v0->xchg(0,1);
  print "a~=(u0 x ss0 x v0^t): OK\n" if (all($a->approx($aa0)));
  ##
  my $u0k = $u0->slice("0:$rm1,:"); ##-- ($r<=$n,$m)
  my $s0k = $s0->slice("0:$rm1");   ##-- ($r<=$n)
  my $ss0k = stretcher($s0k);       ##-- ($r,$r) : diag
  my $v0k = $v0->slice("0:$rm1,:"); ##-- ($r<=$n,$n)
  my $aa0k = $u0k x $ss0k x $v0k->xchg(0,1);
  print "a~=(u0k x ss0k x v0k^t): OK\n" if (all($a->approx($aa0k)));
  ##
  ##-- assertions:
  ##  + T   x T^t = 1_(y~$r<<$m)  : ($u0 x $u0->xchg(0,1))->approx( stretcher(ones($m)) ) : NO
  ##    ----> typo in wikipedia ?!
  ##    ----> maybe should read:
  ##    T^t x T   = 1_(y~?)       : ($u0->xchg(0,1) x $u0)->approx( stretcher(ones($n)) ) : CLOSE, NOT QUITE
  ##  + T   x T^t = 1_(m~$m)      : ($u0 x $u0->xchg(0,1))->approx( stretcher(ones($m)) ) : NO
  ##  + D^t x D   = 1_(y~?)       : ($v0->xchg(0,1) x $v0)->approx( stretcher(ones($n)) ) : YES
  ##  + D   x D^t = 1_(n~$n)      : ($v0 x $v0->xchg(0,1))->approx( stretcher(ones($n)) ) : YES

  my $svd = MUDL::SVD->new(r=>$r);
  $svd->compute($a);
  my ($u,$s,$v) = @$svd{qw(u sigma v)};
  my $ss = stretcher($s);
  my $aa = $u x $ss x $v->xchg(0,1);
  print "a~=(u x ss x v^t): OK\n" if (all($a->approx($aa)));
  ##-- wikipedia:
  ## + RAW: A = T x S x D^t
  ##   - A: (m~Terms,    n~Docs)     : term-doc frequency
  ##   - T: (m~Terms,    r~Concepts) : term-concept vecs
  ##   - S: (r~Concepts, r~Concepts) : diagonal singular values
  ##   - D: (n~Docs,     r~Concepts) : document-concept vecs
  ## + REDUCED: A ~= A_k = T_k x S_k x D_k^t, for k << r
  ##   - A_k: (m~Terms,    n~Docs)       : term-doc frequency
  ##   - T_k: (m~Terms,    k<<r~Concepts): term-concept vecs
  ##   - S_k: (k~Concepts, k<<r~Concepts): diagonal "significant" singular values
  ##   - D_k: (n~Docs,     k<<r~Concepts): document-concept vecs
  ## + for us:
  ##   - A   ~ $a   (m~$m~Terms,    n~$n~Docs)      ##-- PDL col-primary: ($n,$m)
  ##   - T_k ~ $u   (m~$m~Terms,    k~$r~Concepts)  ##-- PDL col-primary: ($r,$m)
  ##   - S_k ~ $ss  (k~$r~Concepts, k~$r~Concepts)  ##-- PDL (square):    ($r,$r)
  ##   - D_k ~ $v   (n~$n~Docs,     k~$r~Concepts)  ##-- PDL col-primary: ($r,$n)
  ##   : A_k ~= T_k  x S_k x D_k^t
  ##          = $u   x $ss x $v->xchg(0,1)
  ## + APPLICATION:
  ##   - [wikip/LSI] ... a simple transformation of the (A = T x S x D^t) equation into the
  ##     equivalent D = A^t x T x inv(S) equation, a new vector, "d", for a query
  ##     or for a new document can be created by computing a new column in A and
  ##     then multiplying the new column by (T x inv(S))
  ##   - [wikip/LSA] ... This means that if you have a query vector "q", you must do
  ##     the translation
  ##        \hat{q} = \Sigma_k^{-1} \times U_k^T \times q
  ##     (me)    qx = inv($s) x $u->xchg(0,1) x $q
  ##     before you compare it with the document vectors in the concept space (V^t)
  my $vx = $v->xchg(0,1); ##-- "document space"
  #my $q  = $a->slice("-1,")+1;
  my $q = ones(1,$m);
  my $qx = inv($ss) x $u->xchg(0,1) x $q;
  my $vqx = $vx->glue(0,$qx);

  ##-- check "document space": my def ($ar) vs. "real thing" ($vx)
  my $ar = $svd->apply($a);

  print STDERR "$0: test_svd() done: what now?\n";
}
#test_svd();

##======================================================================
## test: look at error surfaces

sub truncstr {
  my ($str,$n) = @_;
  return $str if (length($str) <= $n);
  return substr($str,0,$n-3)."...";
}

sub test_errors {
  ##-- eval file
  my $efile = shift;
  $efile = 'xcheck.cp-avg.tw-entropy.r-100.d/eval.all.xml' if (!defined($efile));
  my $eval  = DocClassify::Eval->loadFile($efile) or die("$0: load failed for '$efile': $!");

  ##-- enum: docs
  my $denum = MUDL::Enum->new();
  @{$denum->{id2sym}} = keys(%{$eval->{lab2docs}});
  @{$denum->{sym2id}}{@{$denum->{id2sym}}} = (0..$#{$denum->{id2sym}});
  my $ND = $denum->size;
  my @dids = (0..($ND-1));
  my @docs = @{$eval->{lab2docs}}{@{$denum->{id2sym}}[@dids]};

  ##-- enum: cats (load from file)
  my $cenum = MUDL::Enum->new();
  my $cefile = 'sample_classes.enum';
  if (defined($cefile)) {
    $cenum = $cenum->loadFile($cefile,iolayers=>[':utf8']) or die("$0: Enum->loadFile($cefile) failed: $!");
  }
  $cenum->addSymbol($_) foreach (grep {$_ ne ''} keys(%{$eval->{cat2eval}}));
  my $NC = $cenum->size;
  my @cids = (0..($NC-1));

  ##-- PDLs: by cat
  my %cp = qw();
  my ($which,$unit);
  foreach $which (qw(tp fp fn pr rc F)) {
    foreach $unit (qw(docs bytes)) {
      $cp{"${which}_${unit}"} = pdl( map {$eval->{cat2eval}{$_}{"${which}_${unit}"}||0} @{$cenum->{id2sym}}[@cids] );
    }
  }
  my ($ctp,$cfp,$cfn, $cpr,$crc,$cF) = @cp{map {$_."_docs"} qw(tp fp fn pr rc F)};
  ##-- sanitize
  $cpr->where(!$ctp) .= 0;
  $crc->where(!$ctp) .= 0;
  $cF .= 2/($cpr**-1 + $crc**-1);
  $cF->where(!$cF->isfinite) .= 0;

  ##-- cat sizes
  my $cbytes = $cp{'tp_bytes'} + $cp{'fn_bytes'}; ##-- for cat1

  ##--------------------------------------
  ##-- simple plots
  usepgplot if (1);
  our %plot = (axis=>'logx',xtitle=>'cat size (bytes)',ytitle=>'eval');
  if (0) {
    points($cbytes,$cpr,{%plot,ytitle=>'pr'});
    points($cbytes,$crc,{%plot,ytitle=>'rc'});
    points($cbytes,$cF,{%plot,ytitle=>'F'});
  }

  ##--------------------------------------
  ##-- 3d category plots
  my $cc_docs  = zeroes($NC,$NC);
  my $cc_bytes = zeroes($NC,$NC);
  my ($docs, $c1,$c2, $c1i,$c2i);
  foreach $docs (values(%{$eval->{lab2docs}})) {
    ($c1,$c2)   = map {$_->cats->[0]{name}} @$docs;
    ($c1i,$c2i) = @{$cenum->{sym2id}}{$c1,$c2};
    #if ($c1i == $c2i) {
      $cc_docs->slice("$c1i,$c2i")++;
      $cc_bytes->slice("$c1i,$c2i") += $docs->[0]->sizeBytes if (defined($docs->[0]->sizeBytes));
    #}
  }

  ##--------------------------------------
  ##-- 3d cat plots: p(c1|c2), p(c2|c1)
  our %iplot = (DrawWedge=>1, itf=>'sqrt', xtitle=>'c1', ytitle=>'c2', ztitle=>'Count');
  if (0) {
    imag($cc_docs,{%iplot, itf=>'log', title=>'n_docs(Wanted=c1,Got=c2)'});
    imag($cc_bytes,{%iplot, itf=>'log', title=>'n_bytes(Wanted=c2,Got=c2)'});

    ##-- p1g2: [c1,c2] -> p(c1|c2)
    my $cc_docs_p1g2 = $cc_docs/$cc_docs->sumover->slice("*1,"); $cc_docs_p1g2->where(!$cc_docs_p1g2->isfinite) .= 0;
    imag( $cc_docs_p1g2, {%iplot,itf=>'sqrt',title=>'p(Wanted=c1|Got=c2) by n_docs'} );

    ##-- p2g1: [c1,c2] -> p(c1|c2)
    my $cc_docs_p2g1 = $cc_docs/$cc_docs->xchg(0,1)->sumover; $cc_docs_p2g1->where(!$cc_docs_p2g1->isfinite) .= 0;
    imag( $cc_docs_p2g1, {%iplot,itf=>'sqrt',title=>'p(Got=c2|Wanted=c1) by n_docs'} );
  }

  ##--------------------------------------
  ##-- cat plots: errors only (no tp)
  my $cc_docs_errs = $cc_docs->pdl; $cc_docs_errs->diagonal(0,1) .= 0;
  my $cc_bytes_errs = $cc_bytes->pdl; $cc_bytes_errs->diagonal(0,1) .= 0;
  if (0) {
    imag($cc_docs_errs,{%iplot});
    imag($cc_bytes_errs,{%iplot});
  }

  ##--------------------------------------
  ## doc-catWanted pdls
  my @dcname  = map {$_->[0]{cats}[0]{name}} @docs;         ##-- [$did] -> $catNameWanted
  my $dc      = pdl(long,[@{$cenum->{sym2id}}{@dcname}]);   ##-- [$did] -> $cid_wanted
  my ($dc_v,$dc_vc) = $dc->valcounts;
  my $cnd     = zeroes(long,$NC); $cnd->index($dc_v) .= $dc_vc; ##-- [$cid] -> $ndocs_wanted
  ##
  my $dcdist = zeroes($ND,$NC);  ##-- [$did,$cid_got] -> $dist
  my ($dcats2, @dcids,@dcdist);
  foreach $did (@dids) {
    $dcats2  = $docs[$did][1]{cats};
    @dcids  = @{$cenum->{sym2id}}{map {$_->{name}} @$dcats2};
    @dcdist = map {$_->{dist}} @$dcats2;
    $dcdist->slice("($did),")->index(pdl(long,\@dcids)) .= pdl(\@dcdist);
  }
  ##
  my $dcsim = 2**(-$dcdist); ##-- [$did,$cid] -> sim($did,$cid)
  ##
  ##-- plots
  if (1) {
    imag($dcdist,{%iplot,itf=>'linear',xtitle=>'doc',ytitle=>'cat',title=>'doc-cat similarity'});
    imag($dcsim,{%iplot,itf=>'linear',xtitle=>'doc',ytitle=>'cat',title=>'doc-cat similarity'});
  }

  ##--------------------------------------
  ## catWanted average distance (by member docs)
  my ($dc1dist); ##-- CCS: [$did,$cid_wanted] -> dist($cid_wanted,$did : cat1($did)==$cid_wanted)
  my $dc1dist_w = sequence($ND)->cat($dc)->xchg(0,1);
  my $dc1dist_v = $dcdist->indexND($dc1dist_w);
  $dc1dist = PDL::CCS::Nd->newFromWhich($dc1dist_w,$dc1dist_v);
  ##
  my $c1dist_mu = $dc1dist->average_nz->decode; ##-- dense: [$cid1] -> avg d($cid1,$doc : cat1($doc)==$cid1)
  my $c1dist_sd = (($dc1dist - $c1dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt; ##-- dense: [$cid1] -> stddev d($cid1,$doc : cat1($doc)==$cid1)

  ##--BUGHUNT

  ##--------------------------------------
  ## doc-cat: "adjusted" distance (std-normal (Z) values), pdf, cdf
  my $dcdistz   = ($dcdist-$c1dist_mu->slice("*1,"))/$c1dist_sd->slice("*1,"); ##-- [$ci,$di] -> Z_{$ci}(d($ci,$di))
  my $dcdist_pdf = gausspdf($dcdistz, 0,1);                                    ##-- [$ci,$di] -> pdf_{$ci}(d($ci,$di))
  my $dcdist_cdf = gausscdf($dcdistz, 0,1);                                    ##-- [$ci,$di] -> cdf_{$ci}(d($ci,$di))
  if (1) {
    imag($dcdistz,{%iplot,itf=>'linear',xtitle=>'doc',ytitle=>'cat',title=>'Z_{cat1}(d(doc,cat))'});
    imag($dcdist_pdf,{%iplot,itf=>'linear',xtitle=>'doc',ytitle=>'cat',title=>'pdf_{cat1}(d(doc,cat))'});
    imag($dcdist_cdf,{%iplot,itf=>'linear',xtitle=>'doc',ytitle=>'cat',title=>'cdf_{cat1}(d(doc,cat))'});
  }

  ##--------------------------------------
  ## doc-catWanted Z, cdf
  my ($dc1distz); ##-- CCS: [$did,$cid_wanted] -> Z(d($cid_wanted,$did : cat1($did)==$cid_wanted))
  $dc1distz = PDL::CCS::Nd->newFromWhich($dc1dist_w,$dcdistz->indexND($dc1dist_w),missing=>-10)->decode;
  my ($dc1dist_cdf); ##-- CCS: [$did,$cid_wanted] -> cdf_{$cid1}(d($cid_wanted,$did : cat1($did)==$cid_wanted))
  $dc1dist_cdf = PDL::CCS::Nd->newFromWhich($dc1dist_w,$dcdist_cdf->indexND($dc1dist_w))->decode;

  ##--------------------------------------
  ## cat1-cat2 distance (average by cat1)
  my $ccdist = zeroes($NC,$NC); ##-- [$c1,$c2] -> avg d($c1,$c2)
  my ($dcat1,$cid);
  foreach $did (@dids) {
    $dcat1 = $docs[$did][0]{cats}[0];
    $cid   = $cenum->{sym2id}{$dcat1->{name}};
    $ccdist->slice("($cid),") += $dcdist->slice("($did)");
  }
  my $ccdist2 = $ccdist / $cnd->double->slice("*1,"); ##-- average by $c2
  $ccdist /= $cnd->double;                            ##-- average by $c1
  ##
  ##-- cat-cat dist Z, cdf, pdf
  my $ccdistz   = ($ccdist-$c1dist_mu->slice("*1,"))/$c1dist_sd->slice("*1,"); ##-- [$c1,$c2] -> Z_{$c1}(avg d($c1,$c2))
  my $ccdist_pdf = gausspdf($ccdistz, 0,1);                                    ##-- [$c1,$c2] -> pdf_{$c1}(avg d($c1,$c2))
  my $ccdist_cdf = gausscdf($ccdistz, 0,1);                                    ##-- [$c1,$c2] -> cdf_{$c1}(avg d($c1,$c2))
  if (1) {
    imag($ccdistz,{%iplot,itf=>'linear',xtitle=>'c1',ytitle=>'c2',title=>'Z_{c1}(d(c1,c2))'});
    imag($ccdist_pdf,{%iplot,itf=>'linear',xtitle=>'c1',ytitle=>'c2',title=>'pdf_{c1}(d(c1,c2))'});
    imag($ccdist_cdf,{%iplot,itf=>'linear',xtitle=>'c1',ytitle=>'c2',title=>'cdf_{c1}(d(c1,c2))'});
  }



  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##-- convert to CCS::Nd, sort & report (by docs)
  my $cc_docs_z  = PDL::CCS::Nd->newFromDense($cc_docs);
  my $cc_docs_zw = $cc_docs_z->_whichND;
  my $cc_docs_zv = $cc_docs_z->_nzvals;
  my $cc_docs_qsi = $cc_docs_zv->qsorti->slice("-1:0");
  my ($cci,$status);
  my $llen = 44;
  print "\n${efile}: ERRORS-BY-NDOCS\n";
  foreach $cci ($cc_docs_qsi->list) {
    local $, = '';
    ($c1i,$c2i) = $cc_docs_zw->slice(",($cci)")->list;
    ($c1,$c2)   = @{$cenum->{id2sym}}[$c1i,$c2i];
    $status = $c1i==$c2i ? "[GOOD]" : "[BAD]";
    print sprintf("%-6s : %4d : %${llen}s=wanted : got=%-${llen}s\n",
		  $status, $cc_docs_zv->slice("($cci)")->sclr,
		  "(".sprintf("%2d",$c1i)." ".truncstr($c1,$llen-5).")",
		  "(".sprintf("%2d",$c2i)." ".truncstr($c2,$llen-5).")",
		 );
  }
  print "${efile}: (report-by-ndocs done)\n";
  #exit 0;

  ##-- convert to CCS::Nd, sort & report (by bytes)
  my $cc_bytes_z  = PDL::CCS::Nd->newFromDense($cc_bytes);
  my $cc_bytes_zw = $cc_bytes_z->_whichND;
  my $cc_bytes_zv = $cc_bytes_z->_nzvals;
  my $cc_bytes_qsi = $cc_bytes_zv->qsorti->slice("-1:0");
  print "\n${efile}: ERRORS-BY-BYTES\n";
  foreach $cci ($cc_bytes_qsi->list) {
    ($c1i,$c2i) = $cc_bytes_zw->slice(",($cci)")->list;
    ($c1,$c2)   = @{$cenum->{id2sym}}[$c1i,$c2i];
    $status = $c1i==$c2i ? "[GOOD]" : "[BAD] ";
    print "$status: ", sistr($cc_bytes_zv->slice("($cci)")->sclr,'f','3.0'), ": wanted=($c1i $c1) \t-- got=($c2i $c2)\n";
  }
  print STDERR "${efile}: (report-by-bytes done)\n";
  exit(0);

  print STDERR "$0: test_errors() done: what now?\n";
}
#test_errors(@ARGV);

##======================================================================
## bench: term-doc matrix construction
sub bench_compile_tdm0 {
  my $cfile = 'vzdata-testset.corpus.xml';
  #my $cfile = 'xcheck.d/split.5.xml';
  my $corpus = DocClassify::Corpus->loadFile($cfile);
  my $mapper = DocClassify::Mapper::LSI->new();
  $mapper->trainCorpus($corpus);

  ##-- pre-compile base data
  DocClassify::Mapper::ByLemma::compile($mapper);

  if (1) {
    ##-- test: tcm0
    $mapper->compile_tcm0();

    ##-- bench: tcm0
    $mapper->{verbose}=0;
    timethese(1, {'tcm0_v1'=>sub {$mapper->compile_tcm0_v1;}, 'tcm0_v2'=>sub {$mapper->compile_tcm0;}});
  }

  if (0) {
    ##-- test: tdm0
    $mapper->compile_tdm0_v1();
    my $tdm0_v1 = $mapper->{tdm0}->clone;

    $mapper->compile_tdm0_v2();
    my $tdm0_v2 = $mapper->{tdm0}->clone;

    ##-- bench: tdm0
    $mapper->{verbose}=0;
    timethese(1, {'tdm0_v1'=>sub {$mapper->compile_tdm0_v1;}, 'tdm0_v2'=>sub {$mapper->compile_tdm0_v2;}});
  }

  print STDERR "$0: bench_compile_tdm0 done: what now?\n";
}
#bench_compile_tdm0;

##======================================================================
## test: compile
sub test_compile {
  my ($cfile,$mfile) = @_;

  ##-- load: corpus
  #$cfile = 'vzdata-safe.u1.corpus.xml' if (!defined($cfile));
  $cfile = 'xcheck.d/split.0.xml';
  my $corpus = DocClassify::Corpus->loadFile($cfile);

  ##-- load: mapper
  my $map = DocClassify::Mapper::LSI->new();
  $mfile = "$cfile.test_compile.bin" if (!$mfile);
  if (-r $mfile) {
    $map = $map->loadFile($mfile) or die("$0: Mapper->loadFile($mfile) failed: $!");
  } else {
    ##-- train mapper & save file
    $map->trainCorpus($corpus);
    $map->saveFile($mfile) or die("$0: Mapper->saveFile($mfile) failed: $!");
  }
  $map->{seed} = 0;
  #$map->{catProfile} = 'fold-in';
  $map->compile();

  print STDERR "$0: test_compile() done: what now?\n";
}
#test_compile(@ARGV);

##======================================================================
## test: p-value mapping bug
sub test_pval_map {
  my ($mfile,$cfile) = @_;
  $mfile = 'tmp1.bin' if (!defined($mfile));
  $cfile = 'vzdata-testset.corpus.xml' if (!defined($cfile));

  my $map = DocClassify::Mapper->loadFile($mfile) or die("$0: Mapper->loadFile($mfile) failed: $!");
  my $corpus = DocClassify::Corpus->loadFile($cfile) or die("$0: Corpus->loadFile($cfile) failed: $!");

  $map->mapCorpus($corpus);
  $corpus->saveFile("$mfile.xml") or die("$0: Corpus->saveFile($mfile.xml) failed: $!");

  print STDERR "$0: test_pval_map done: what now?\n";
}
#test_pval_map(@ARGV);

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
;

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
sub test_compile_xcheck {
  my $mfile = shift;
  $mfile  = 'vzdata-testset.train0.bin' if (!defined($mfile)); ##-- carrot:small
  #$mfile = 'vzdata-safe.u1.train0.bin' if (!defined($mfile)); ##-- carrot,uhura
  #$mfile = 'vzdata-all.train0.bin' if (!defined($mfile)); ##-- lal
  $mfile = "$mfile.xcheck.bin" if (-r "$mfile.xcheck.bin"); ##-- pre-xcheckd
  print STDERR "$0: load($mfile)\n";
  my $map = DocClassify::Mapper::LSI->loadFile("$mfile")
    or die("$0: Mapper->loadFile($mfile) failed: $!");

  if (!defined($map->{dc_dist})) {
    ##-- run cross-check
    #$map->{xn} = 3;
    $map->{xn} = 10;
    $map->{svdr} = 128 if ($map->{svdr} < 128);
    my $date = `date -R`; chomp($date);
    print STDERR "$0: cross-check: BEGIN: $date\n";
    $map->compileCrossCheck();
    print STDERR "$0: saveFile($mfile.check.bin)\n";
    $map->saveFile("$mfile.xcheck.bin")
      or die("$0: Mapper->saveFile($mfile.xcheck.bin) failed: $!");
    print STDERR "$0: x-check run & saved to '$mfile.xcheck.bin'\n";
    $date = `date -R`; chomp($date);
    print STDERR "$0: DONE: $date\n";
    exit(0);
  }

  ##-- cross-check env
  my $lcenum = $map->{lcenum};
  my $gcenum = $map->{gcenum};
  my $gcids  = pdl(long, [@{$gcenum->{sym2id}}{@{$lcenum->{id2sym}}}]);
  my $NC     = $map->{lcenum}->size;   my $NCg    = $gcids->max+1;
  my $ND     = $map->{denum}->size;
  my $dcm   = $map->{dcm};
  my $dcm_z = $dcm->missing->sclr;
  my $d2c   = $dcm->xchg(0,1)->_missing('inf')->minimum_ind->decode;  ##-- [$di] -> $ci_best
  $dcm->missing($dcm_z);
  my $dc_dist = $map->{dc_dist};

  ##-- get positive evidence
  my $dc_which1 = sequence($ND)->cat($d2c)->xchg(0,1);  ##-- [$di]     -> [$di,$ci] : $di \in $ci
  my $dc_mask   = zeroes(byte,$dc_dist->dims);          ##-- [$di,$ci] -> 1($di \in     $ci)
  $dc_mask->indexND($dc_which1) .= 1;
  my $dc_which0 = whichND(!$dc_mask);                   ##-- [$di,$ci] -> 1($di \not\in $ci)
  #my $d_dist1   = $dc_dist->indexND($dc_which1);        ##-- [$di]     ->     dist($di,$ci) : $di \in $ci
  #my $d_dist0   =                                       ##-- [$di]     -> avg dist($di,$ci) : $di \not\in $ci
  #  PDL::CCS::Nd->newFromWhich($dc_which0,$dc_dist->indexND($dc_which0))->xchg(0,1)->average_nz->decode;
  my $nc  = $dc_mask->long->sumover->double;             ##-- [$ci] -> |{ $di : $di \in     $ci }|
  my $nnc = $nc->sum - $nc;                              ##-- [$ci] -> |{ $di : $di \not\in $ci }|

  ##-- from testme.perl test_errors(), above
  ## $dcdist_mu = pdl(1): global avg    dist($ci,$di)
  ## $dcdist_sd = pdl(1): global stddev dist($ci,$di)
  my $dcdist_mu = $dc_dist->flat->average;
  my $dcdist_sd = (($dc_dist - $dcdist_mu)**2)->flat->average->sqrt;

  my $nc_min = 50; #3; #10; ##-- minimum #/cats to use fit

  ## $cdist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \in $ci
  ## $cdist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \in $ci
  my ($cdist_mu,$cdist_sd,$cdist_isgood,$cdist_sd_raw);
  $cdist_mu = $dc_dist->average;
  $cdist_sd = (($dc_dist - $cdist_mu->slice("*1,"))**2)->average->sqrt;
  $cdist_isgood = ($cdist_sd->isfinite)&($cdist_sd>0);
  $cdist_sd_raw = $cdist_sd->pdl;
  ##
  $cdist_sd->where(!$cdist_isgood) .= $cdist_sd->where($cdist_isgood)->minimum/2;

  ##-- plots
  our (%eplot);
  if (0) {
    usepgplot;
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu,sigma: dist(cat,doc)',title=>'Normal Fit Paramters by Category (Global)');
    errb($gcids, $cdist_mu,$cdist_sd, {%eplot,xr=>[$gcids->min-1,$gcids->max+1],yr=>[($cdist_mu-$cdist_sd)->min*.99,($cdist_mu+$cdist_sd)->max*1.01]});
    hold(); errb($gcids->average->rint+.5, $dcdist_mu,$dcdist_sd, {symbol=>'star',color=>'blue'});
    release();
  }

  ## $dc1dist: CCS: [$di,$ci] -> dist($ci,$di) : $di \in $ci
  ## $c1dist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \in $ci
  ## $c1dist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \in $ci
  ## $c1dist_mu0, $c1dist_sd0: global fit parameters
  my ($dc1dist,$c1dist_mu,$c1dist_sd,$c1dist_sd_nan0,$c1dist_sd_bad0,$c1dist_isgood,$c1dist_mu_raw);
  $dc1dist = PDL::CCS::Nd->newFromWhich($dc_which1,$dc_dist->indexND($dc_which1));
  $c1dist_mu0 = $dc1dist->_nzvals->average;
  $c1dist_sd0 = (($dc1dist->_nzvals-$c1dist_mu0)**2)->average->sqrt;
  $c1dist_mu = $dc1dist->average_nz->decode;
  $c1dist_sd = (($dc1dist - $c1dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  $c1dist_mu_raw = $c1dist_mu->pdl;
  $c1dist_sd_raw = $c1dist_sd->pdl;
  $c1dist_isgood = (($c1dist_sd_raw->isfinite) & ($nc >= $nc_min));
  #$c1dist_isgood = (($c1dist_sd_raw->isfinite));
  ($c1dist_sd_nan0 = $c1dist_sd->pdl)->where(!$c1dist_sd->isfinite) .= 0;
  ($c1dist_sd_bad0 = $c1dist_sd->pdl)->where(!$c1dist_isgood) .= 0;

  ## $dc0dist: CCS: [$di,$ci] -> dist($ci,$di) : $di \not\in $ci
  ## $c0dist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \not\in $ci
  ## $c0dist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \not\in $ci
  ## $c0dist_mu0, $c0dist_sd0: global fit parameters
  my ($dc0dist,$c0dist_mu,$c0dist_sd,$c0dist_sd_nan0,$c0dist_sd_bad0, $c0dist_isgood, $c0dist_sd_eps);
  my ($c0dist_mu0,$c0dist_sd0,$c0dist_mu_raw);
  $dc0dist = PDL::CCS::Nd->newFromWhich($dc_which0,$dc_dist->indexND($dc_which0));
  $c0dist_mu0 = $dc0dist->_nzvals->average;
  $c0dist_sd0 = (($dc0dist->_nzvals-$c0dist_mu0)**2)->average->sqrt;
  $c0dist_mu = $dc0dist->average_nz->decode;
  $c0dist_sd = (($dc0dist - $c0dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  $c0dist_mu_raw = $c0dist_mu->pdl;
  $c0dist_sd_raw = $c0dist_sd->pdl;
  #$c0dist_isgood = (($c0dist_sd->isfinite) & ($nc >= $nc_min));
  $c0dist_isgood = ($c0dist_sd_raw->isfinite);
  ($c0dist_sd_nan0 = $c0dist_sd->pdl)->where(!$c0dist_sd->isfinite) .= 0;
  ($c0dist_sd_bad0 = $c0dist_sd->pdl)->where(!$c0dist_isgood) .= 0;

  ##-- plots
  if (1) {
    usepgplot;
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category (Positives & Global:red)'},
	     [$gcids-.1,$c1dist_mu,$c1dist_sd_nan0,{}],
	     [$gcids+.1,$cdist_mu,$cdist_sd,{color=>'red'}],
	     [$gcids->average->rint+.5, $dcdist_mu,$dcdist_sd,{symbol=>'star',color=>'blue'}]);
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category (Global, Positives:blue & Negatives:red)'},
	     [$gcids, $cdist_mu,$cdist_sd, {}],
	     [$gcids-.2, $c1dist_mu,$c1dist_sd_nan0, {color=>'blue'}],
	     [$gcids+.2, $c0dist_mu,$c0dist_sd_nan0, {color=>'red'}],
	     [$gcids->average->rint+.5, $dcdist_mu,$dcdist_sd, {sym=>'plus',color=>'green',lineWidth=>5,charsize=>2}]);
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category (Positives:black, Negatives:red)'},
	     [$gcids-.1, $c1dist_mu,$c1dist_sd_nan0, {}],
	     [$gcids+.1, $c0dist_mu,$c0dist_sd_nan0, {color=>'red'}]);
  }


  ##-- Adjust mu, sd
  #$c1dist_mu->where(!$c1dist_sd->isfinite) .= 0;
  $c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu->where($c1dist_isgood)->minimum;
  #$c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu->where($c1dist_isgood)->minimum/2;
  #$c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu0;
  ##
  #$c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd0;
  $c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd->where($c1dist_sd->isfinite)->minimum/2;

  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu->where($c0dist_isgood)->minimum/2;
  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu->where($c0dist_isgood)->maximum*2;
  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu0;
  ##
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->minimum;
  $c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->maximum;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->maximum*2;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->average;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd0;

  ##-- heuristically shovel around fit parameters ($d1)
  my ($conf_nofp,$conf_nofn);
  ($conf_nofp,$conf_nofn) = (.67,.99); ##-- acc=.566
  #($conf_nofp,$conf_nofn) = (.9,.95); ##-- acc=.560
  #($conf_nofp,$conf_nofn) = (.67,.95); ##-- acc=.558
  #($conf_nofp,$conf_nofn) = (.85,.95); ##-- acc=.558
  #($conf_nofp,$conf_nofn) = (.95,.95); ##-- acc=.54934
  #($conf_nofp,$conf_nofn) = (.9,.9); ##-- acc=.55370
  #($conf_nofp,$conf_nofn) = (.85,.85); ##-- acc=.55297
  #($conf_nofp,$conf_nofn) = (.85,.67); ##-- acc=.541
  #($conf_nofp,$conf_nofn) = (.95,.5);   ##-- acc=.526
  my ($cutoff_nofp,$cutoff_nofn,$cutoff_avg,$cutoff_wavg,$cutoff_ok,$c1dist_mu_adj,$c0dist_mu_adj);
  $cutoff_nofp = $c0dist_mu - scalar(gausswidth($conf_nofp, $c0dist_mu,$c0dist_sd));
  $cutoff_nofn = $c1dist_mu + scalar(gausswidth($conf_nofn, $c1dist_mu,$c1dist_sd));
  $cutoff_avg  = ($cutoff_nofp + $cutoff_nofn)/2;
  $cutoff_wavg = $nnc/$ND*$cutoff_nofp + $nc/$ND*$cutoff_nofn;
  $cutoff_ok   = ($cutoff_nofn < $cutoff_wavg) & ($cutoff_nofp > $cutoff_wavg);
  our $c1dist_mu_save = $c1dist_mu->pdl;
  $c1dist_mu_adj = $c1dist_mu->pdl;
  $c0dist_mu_adj = $c0dist_mu->pdl;
  if (any(!$cutoff_ok)) {
    $c1dist_mu_adj->where(!$cutoff_ok) .= ($cutoff_wavg - scalar(gausswidth($conf_nofn, $c1dist_mu,$c1dist_sd)))->where(!$cutoff_ok);
  }

  ##-- BUGHUNT
  my $dc_cdf = gausscdf($dc_dist, $cdist_mu->slice("*1,"), $cdist_sd->slice("*1,"));

  my $dc1_cdf = gausscdf($dc_dist, $c1dist_mu->slice("*1,"), $c1dist_sd->slice("*1,"));
  my $dc0_cdf = gausscdf($dc_dist, $c0dist_mu->slice("*1,"), $c0dist_sd->slice("*1,"));
  my $dc1_scdf = li1($dc1_cdf,1e-5);
  my $dc0_scdf = li1($dc0_cdf,1e-5);

  my $dc1_cdf_adj = gausscdf($dc_dist, $c1dist_mu_adj->slice("*1,"), $c1dist_sd->slice("*1,"));
  my $dc0_cdf_adj = gausscdf($dc_dist, $c0dist_mu_adj->slice("*1,"), $c0dist_sd->slice("*1,"));
  my $dc1_scdf_adj = li1($dc1_cdf_adj);
  my $dc0_scdf_adj = li1($dc0_cdf_adj);

  my $dc1_cdf0 = gausscdf($dc_dist, $c1dist_mu0, $c1dist_sd0);
  my $dc0_cdf0 = gausscdf($dc_dist, $c0dist_mu0, $c0dist_sd0);

  my $dc_F1    = F1($dc1_cdf, $dc0_cdf, 1e-5);

  my $dc1_scdf0 = li1($dc1_cdf0,1e-5);
  my $dc0_scdf0 = li1($dc0_cdf0,1e-5);

  our %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'c1', ytitle=>'c2');
  imag($dc0_cdf*(1-$dc1_cdf)*1,       {%iplot,itf=>'linear'});
  imag($dc0_cdf*(1-$dc1_cdf)*$dc_mask,{%iplot,itf=>'log'});
  imag(1-$dc1_cdf*(1-$dc0_cdf),{%iplot,itf=>'linear'});

  ##-- baseline
  my $accbase = ($dc_dist->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND; ##-- .542

  ##-- stupid p-value base
  my $dnewp = ($dc1_scdf);
  my $accp  = ($dnewp->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND;  ##-- .506; now .595

  my $dnew_adj = $dc1_scdf_adj;
  my $acc_adj = ($dnew_adj->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND;  ##-- .5609

  my $snew_adj_F = F1((1-$dc1_scdf_adj),(1-$dc0_scdf_adj));
  my $acc_adj_F = ($snew_adj_F->xchg(0,1)->maximum_ind == $d2c)->nnz / $ND;  ##-- .5609

  ##-- misc
  my $dnewp0 = ($dc0_scdf);
  my $accp0  = ($dnewp0->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND;  ##-- .5355

  my $snewF1 = F1((1-$dc1_cdf),(1-$dc0_cdf),1e-5);
  my $accF1  = ($snewF1->xchg(0,1)->maximum_ind == $d2c)->nnz / $ND;  ##-- .5936

  my $snewFbNc = Fb((1-$dc1_scdf),(1-$dc0_scdf),$nc->slice("*1,"));
  my $accFbNc  = ($snewFbNc->xchg(0,1)->maximum_ind == $d2c)->nnz / $ND;  ##-- .558

  my $snewFbNcp = Fb((1-$dc1_scdf),(1-$dc0_scdf),($nc/$ND)->slice("*1,"));
  my $accFbNcp  = ($snewFbNcp->xchg(0,1)->maximum_ind == $d2c)->nnz / $ND;  ##-- .5943

  my $snewFbNcp1 = Fb((1-$dc1_scdf),(1-$dc0_scdf),(1-($nc/$ND))->slice("*1,"));
  my $accFbNcp1  = ($snewFbNcp1->xchg(0,1)->maximum_ind == $d2c)->nnz / $ND;  ##-- .5957 !

  my $snewFbNcp1_adj = Fb((1-$dc1_scdf_adj),(1-$dc0_scdf_adj),(1-($nc/$ND))->slice("*1,"));
  my $accFbNcp1_adj  = ($snewFbNcp1_adj->xchg(0,1)->maximum_ind == $d2c)->nnz / $ND;  ##-- .5616

  my $snewFbNcp2 = Fb((1-$dc1_scdf),(1-$dc0_scdf),(1-($nnc/$ND))->slice("*1,"));
  my $accFbNcp2  = ($snewFbNcp2->xchg(0,1)->maximum_ind == $d2c)->nnz / $ND;  ##-- .5943

  my $snewFbNcp3 = Fb((1-$dc1_scdf),(1-$dc0_scdf),(2-($nnc/$ND))->slice("*1,"));
  my $accFbNcp3  = ($snewFbNcp3->xchg(0,1)->maximum_ind == $d2c)->nnz / $ND;  ##-- .5936

  my $snewFbNcp4 = Fb((1-$dc1_scdf),(1-$dc0_scdf),(1+($nc/$ND))->slice("*1,"));
  my $accFbNcp4  = ($snewFbNcp4->xchg(0,1)->maximum_ind == $d2c)->nnz / $ND;  ##-- .5936

  my $snewFbNcp5 = Fb((1-$dc1_scdf),(1-$dc0_scdf),(exp(-$nc/$ND))->slice("*1,"));
  my $accFbNcp5  = ($snewFbNcp5->xchg(0,1)->maximum_ind == $d2c)->nnz / $ND;  ##-- .5943

  my $dnew_cdf = $dc_cdf;
  my $acc_cdf  = ($dnew_cdf->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND;  ##-- .503

  ##-- try this
  my $dnew0 = $dc1_scdf**($nc/$ND)->slice("*1,");
  my $acc0  = ($dnew0->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND;  ##-- WAS: .5827, IS: .5653

  my $dnew0_adj = $dc1_scdf_adj**($nc/$ND)->slice("*1,");
  my $acc0_adj  = ($dnew0_adj->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND;  ##-- .5761

  ##-- ... or this?
  my $dnew1 = $dc1_scdf0**($nc/$ND)->slice("*1,");
  my $acc1  = ($dnew1->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND; ##-- .5696

  ##-- ... or this?
  my $dnew2 = $dc1_scdf0**($nc/$ND)->slice("*1,") + $dc0_scdf0**($nnc/$ND)->slice("*1,");
  my $acc2  = ($dnew2->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND; ##-- .575

  ##-- ... or this?
  my $dnew3 = $dc1_scdf**($nc/$ND)->slice("*1,") + $dc0_scdf**($nnc/$ND)->slice("*1,");
  my $acc3  = ($dnew3->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND; ##-- .5537

  ##-- or this?
  my $dnew4 = $dc1_scdf**($nc/$ND)->slice("*1,") * ($dc0_scdf)**($nnc/$ND)->slice("*1,");
  my $acc4  = ($dnew4->xchg(0,1)->minimum_ind == $d2c)->nnz / $ND; ##-- .523

  ##-- IDEA: give target pr,rc p-values, implement cutoff with gaussian width tradeoff ?!

  print STDERR "$0: test_compile_xcheck() done: what now?\n";
}
test_compile_xcheck(@ARGV);

##--
## $psmooth = li1($p,$eps)
##  + linear-interpolated smoothed (1-$eps)*$p + $eps;
sub li1 {
  my ($p,$eps) = @_;
  $eps = 1e-5 if (!defined($eps));
  return (1-$eps)*$p + $eps;
}

##--
## $F1 = F1($pr,$rc,$eps)
##  + balanced F-score
sub F1 {
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
sub Fb {
  my ($pr,$rc,$beta,$eps)=@_;
  $beta = 0.5 if (!defined($beta));
  my ($pr1,$rc1) = (li1($pr,$eps),li1($rc,$eps));
  my $Fb = (1+$beta**2) * ($pr1*$rc1) / ($beta**2 * $pr1 + $rc1);
}

##-- test doc freeze-thaw
sub test_freeze_thaw {
  my $dfile = 'test-small.xml';
  my $doc = DocClassify::Document->new(file=>$dfile);
  $doc->xmlDoc();

  my $frz  = $doc->saveBinString();
  my $doc2 = ref($doc)->loadBinString($frz);
  undef $doc2;

  $doc2 = Storable::dclone($doc);
  undef $doc2;

  print STDERR "test_freeze_thaw() done -- what now?\n";
  exit 0;
}
#test_freeze_thaw();

##======================================================================
## test: pdl ccs buglet
sub test_ccs_dice {
  my $p = pdl([[0,0,0],[0,1,0],[2,0,0],[0,0,0]]);
  my $pc = $p->toccs;
  my $pd = $p->dice_axis(1,0);
  my $pcd = $pc->dice_axis(1,0);

  print STDERR "$0: test_ccs_dice() done -- what now?\n";
}
#test_ccs_dice();


##======================================================================
## test: look at categorization ambiguity
sub test_catambig {
  my $cfile = shift;
  $cfile = 'vzdata-safe.corpus.xml' if (!defined($cfile));

  my $corpus = DocClassify::Corpus->loadFile($cfile)
    or die("$0: corpus load failed from '$cfile': $!");

  my %cstr2n = qw();
  my %name2n = qw();
  my %union2n = qw();
  my $nuni  = 0; ##-- number of univocally (i.e. un-ambiguously) categorized documents
  my $nuni1 = 0; ##-- number of univocally categorized documents with deg=1
  my ($doc,$cat, $cstr);
  foreach $doc (@{$corpus->{docs}}) {
    foreach $cat (@{$doc->cats}) {
      $cstr = "<$cat->{deg}> $cat->{id} $cat->{name}";
      $cstr2n{$cstr}++;
      $name2n{$cat->{name}}++;
      ++$ndcats;
    }
    ++$nuni  if (@{$doc->{cats}}==1);
    ++$nuni1 if (@{$doc->{cats}}==1 && $doc->{cats}[0]{deg}==1);
    ##
    $cstr = join(" | ", map {"(<$_->{deg}> $_->{id} $_->{name})"} @{$doc->{cats}});
    $union2n{$cstr}++;
  }
  my $ndocs = @{$corpus->{docs}};
  my $ambig = sprintf("%.2f",$ndcats / $ndocs);
  my $unirate = sprintf("%.1f",100*$nuni/$ndocs);
  my $uni1rate = sprintf("%.1f",100*$nuni1/$ndocs);

  print "# ndocs=$ndocs, ndcats=$ndcats, ambig=$ambig; nuni=$nuni ($unirate%); nuni1=$nuni1 ($uni1rate%)\n";
  #exit(0);

  ##-- dump union
  {
    local $,='';
    print map {"$union2n{$_}\t$_\n"} sort {$union2n{$b}<=>$union2n{$a}} keys(%union2n);
    exit(0);
  }

  print STDERR "$0: test_catambig() done: what now?\n";
}
test_catambig(@ARGV);

##======================================================================
## MAIN (dummy)
foreach $i (1..3) {
  print STDERR "--dummy[$i]--\n";
}
