#!/usr/bin/perl -w

use lib qw(. ./MUDL);

use DocClassify;
use DocClassify::Mapper::Train;
use DocClassify::Utils qw(:all);

use DocClassify::Temp::Array;
use DocClassify::Temp::Hash;

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

use Encode qw(encode decode encode_utf8 decode_utf8);
use File::Basename qw(basename dirname);

use Getopt::Long qw(:config no_ignore_case);
use Benchmark qw(cmpthese timethese);

BEGIN {
  $,=' ';
  binmode(STDOUT,':utf8');
  binmode(STDERR,':utf8');
}

##======================================================================
## utils

BEGIN {
  our $PG_DEV = '';
}
sub _usepgplot {
  my $dev = shift;
  $dev = '/XS' if (!defined($dev));
  return if ($dev eq $PG_DEV); ##-- don't re-use pgplot!
  usepgplot();
  dev($PG_DEV=$dev);
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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sub test_compile_xcheck {
  my $mfile = shift;
  #$mfile  = 'vzdata-testset.train0.bin' if (!defined($mfile)); ##-- carrot:small
  #$mfile = 'vzdata-safe.u1.train0.bin' if (!defined($mfile)); ##-- carrot,uhura:bigger
  $mfile = 'vzdata-all.train0.bin' if (!defined($mfile)); ##-- lal:big

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
  my $gcids  = pdl(long, [@{$gcenum->{sym2id}}{@{$lcenum->{id2sym}}}]); ##-- [$ci_local] -> $ci_global
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

  my $nc_min = 3;#50; #3; #10; ##-- minimum #/cats to use fit

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


  ##-- hack mu, sd
  #$c1dist_mu->where(!$c1dist_sd->isfinite) .= 0;
  $c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu->where($c1dist_isgood)->minimum;
  #$c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu->where($c1dist_isgood)->minimum/2;
  #$c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu0;
  ##
  #$c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd0;
  #$c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd->where($c1dist_sd->isfinite)->minimum/2;
  $c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd->where($c1dist_isgood)->minimum/2;

  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu->where($c0dist_isgood)->minimum/2;
  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu->where($c0dist_isgood)->maximum*2;
  #$c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu0;
  ##
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->minimum;
  $c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->maximum;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->maximum*2;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->average;
  #$c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd0;

  ##-- plot: hacked mu,sd
  if (1) {
    usepgplot;
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked (+:black, -:red)'},
	     [$gcids-.1, $c1dist_mu_raw,$c1dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.1, $c0dist_mu_raw,$c0dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu,    $c1dist_sd,      {}],
	     [$gcids+.1, $c0dist_mu,    $c0dist_sd,      {color=>'red'}],
	     [$gcids,    $nc/$nc->max,$nc->zeroes,       {color=>'yellow',charsize=>2}]
	    );
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked (+:black, -:red)'},
	     [$gcids-.1, $c1dist_mu_raw,$nc->zeroes, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.1, $c0dist_mu_raw,$nc->zeroes, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu,    $c1dist_sd,  {}],
	     [$gcids+.1, $c0dist_mu,    $c0dist_sd,  {color=>'red'}],
	    );
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
  }


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

  ##-- plot: hacked & adjusted mu,sd
  if (1) {
    usepgplot;
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ploterrs({%eplot,title=>"Normal Fit by Category: Hacked & Adjusted (+:black, -:red) [conf=($conf_nofn,$conf_nofp)]"},
	     [$gcids-.2, $c1dist_mu_raw,$c1dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.2, $c0dist_mu_raw,$c0dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu_adj,$c1dist_sd,      {}],
	     [$gcids+.1, $c0dist_mu_adj,$c0dist_sd,      {color=>'red'}],
	     [$gcids,    $nc/$nc->max,$nc->zeroes,       {color=>'yellow',charsize=>2}],
	     [$gcids-.1, $cutoff_nofn,  $nc->zeroes,     {symbol=>'square'}],
	     [$gcids+.1, $cutoff_nofp,  $nc->zeroes,     {symbol=>'square',color=>'red'}],
	     [$gcids+.1, $cutoff_wavg,  $nc->zeroes,     {symbol=>'cross',color=>'green'}]);
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
  }



  ##-- BUGHUNT
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

  our %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'c1', ytitle=>'c2');
  if (0) {
    imag($dc0_cdf*(1-$dc1_cdf)*1,       {%iplot,itf=>'linear'});
    imag($dc0_cdf*(1-$dc1_cdf)*$dc_mask,{%iplot,itf=>'log'});
    imag(1-$dc1_cdf*(1-$dc0_cdf),{%iplot,itf=>'linear'});
  }

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

  ##-- baseline
  $acc->('$dc_dist'); ##-- .615

  ##-- stupid p-value tests
  $acc->('$dc_cdf','min'); ##-- .547
  $acc->('$dc1_scdf'); ##-- .428
  $acc->('$dc1_scdf_adj'); ##-- .597, all=.592
  $acc->('F1((1-$dc1_scdf_adj),(1-$dc0_scdf_adj))', 'max'); ##-- .597, all=.591 ##-- USE THIS!
  if (1) {
    our $dc_raw_l = 2-$dc_dist; #-- sim=2-d; d=2-1-cos; -> d=2-sim -> sim=1+cos
    our $dc_raw = zeroes($ND,$NCg)+$dc_raw_l->min;
    $dc_raw->dice_axis(1,$gcids) .= $dc_raw_l;
    ##
    %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'doc', ytitle=>'cat');
    imag($dc_raw,{%iplot,title=>'Raw Similarity: 1+cos(LSI(doc),LSI(cat))'});
    imag($dc_raw,{%iplot,title=>'Raw Similarity: 1+cos(LSI(doc),LSI(cat)) [log-scale]',itf=>'log'});
    ##
    our $dc_sim_l = F1((1-$dc1_scdf_adj),(1-$dc0_scdf_adj)); ##-- USE THIS!
    our $dc_sim = zeroes($ND,$NCg)+$dc_sim_l->min;
    $dc_sim->dice_axis(1,$gcids) .= $dc_sim_l;
    imag($dc_sim,{%iplot,title=>'F-Similarity: F1(1-cdf(doc~cat), 1-cdf(doc!~cat))'});
    imag($dc_sim,{%iplot,title=>'F-Similarity: F1(1-cdf(doc~cat), 1-cdf(doc!~cat)) [log-scale]', itf=>'log'});
    ##
    our $cc_which1 = $d2c->cat($dc_which1->slice("(1),"))->xchg(0,1);
    our $asim1_l = PDL::CCS::Nd->newFromWhich($cc_which1,$dc_sim->indexND($dc_which1))->dummy(0,1)->average_nz->_missing(0)->decode;
    our $cc_which0 = $d2c->index($dc_which0->slice("(0)"))->cat($dc_which0->slice("(1),"))->xchg(0,1);
    our $asim0_l = PDL::CCS::Nd->newFromWhich($cc_which0,$dc_sim->indexND($dc_which0))->dummy(0,1)->average_nz->_missing(0)->decode;
    our $asim_l  = ($asim1_l+$asim0_l);
    our $asim = zeroes($NCg,$NCg)+$asim_l->min;
    $asim->dice_axis(0,$gcids)->dice_axis(1,$gcids) .= $asim_l;
    %iplot = (%iplot,xtitle=>'c1 : wanted',ytitle=>'c2 : predicted');
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity'});
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity [sqrt-scale]',itf=>'sqrt'});
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity [log-scale]',itf=>'log'});
    imag(1-$asim,{%iplot,title=>'Avg Adjusted Distance'});
    imag(1-$asim,{%iplot,title=>'Avg Adjusted Distance [log-scale]',itf=>'log'});
    ##
    our $asim_mask1 = ($asim->maximum_ind->slice("*1,")==$asim->xvals);
    imag($asim*$asim_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision'});
    imag($asim*$asim_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [log-scale]',itf=>'log'});
    ##
    our $asim_mask2 = ($asim->xchg(0,1)->maximum_ind==$asim->yvals);
    imag($asim*$asim_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall'});
    imag($asim*$asim_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [log-scale]',itf=>'log'});

    our $asimx = $asim->pdl;
    our $badc = pdl(long,[0,29,30]);
    $asimx->dice_axis(0,$badc) .= 0 if (all($badc<$asimx->dim(0)));
    $asimx->dice_axis(1,$badc) .= 0 if (all($badc<$asimx->dim(1)));
    imag($asimx,{%iplot,title=>'Avg Adjusted Similarity [safe]'});
    imag($asimx,{%iplot,title=>'Avg Adjusted Similarity [safe,log-scale]',itf=>'log'});
    ##
    our $asimx_mask1 = ($asimx->maximum_ind->slice("*1,")==$asimx->xvals);
    imag($asimx*$asimx_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [safe]'});
    imag($asimx*$asimx_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [safe,log-scale]',itf=>'log'});
    ##
    our $asimx_mask2 = ($asimx->xchg(0,1)->maximum_ind==$asimx->yvals);
    imag($asimx*$asimx_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [safe]'});
    imag($asimx*$asimx_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [safe,log-scale]',itf=>'log'});
  }

  ##-- misc
  $acc->('$dc0_scdf'); ##-- .600
  $acc->('F1((1-$dc1_cdf),(1-$dc0_cdf),1e-5)','max'); ##-- .470
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),$nc->slice("*1,"))','max'); ##-- .626
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),($nc/$ND)->slice("*1,"))','max'); ##-- .430
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(1-($nc/$ND))->slice("*1,"))','max'); ##-- .460
  $acc->('Fb((1-$dc1_scdf_adj),(1-$dc0_scdf_adj),(1-($nc/$ND))->slice("*1,"))','max'); ##-- .592, all=.5824
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(1-($nnc/$ND))->slice("*1,"))','max'); ##-- .430
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(2-($nnc/$ND))->slice("*1,"))','max'); ##-- .478
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(1+($nc/$ND))->slice("*1,"))','max');  ##-- .478
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(exp(-$nc/$ND))->slice("*1,"))','max'); ##-- .460

  ##-- try this
  $acc->('$dc1_scdf**($nc/$ND)->slice("*1,")'); ##-- .630, all=.5832
  $acc->('$dc1_scdf_adj**($nc/$ND)->slice("*1,")'); ##-- .6589

  ##-- ... or this?
  $acc->('$dc1_scdf0**($nc/$ND)->slice("*1,")'); ##-- .6719 all=.5248  ##-- BEST for safe.u1
  $acc->('$dc1_scdf0**($nc/$nc->max)->slice("*1,")'); ##-- .670
  $acc->('$dc0_scdf0**($nc/$ND)->slice("*1,")'); ##-- .642
  $acc->('$dc0_scdf0**($nc/$nc->max)->slice("*1,")'); ##-- .642

  ##-- ... or these?
  $acc->('$dc1_scdf0**($nc/$ND)->slice("*1,") + $dc0_scdf0**($nnc/$ND)->slice("*1,")'); ##-- .671
  $acc->('$dc1_scdf**($nc/$ND)->slice("*1,") + $dc0_scdf**($nnc/$ND)->slice("*1,")'); ##-- .640
  $acc->('$dc1_scdf**($nc/$ND)->slice("*1,") * ($dc0_scdf)**($nnc/$ND)->slice("*1,")'); ##-- .584


  print STDERR "$0: test_compile_xcheck() done: what now?\n";
  exit 0;
}
#test_compile_xcheck(@ARGV);


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sub test_load_xcheck {
  my ($mfile,$efile) = @_;
  if (!defined($mfile)) {
    #$mfile = 'vzdata-safe.u1.train0a.bin'; ##-- carrot,uhura:bigger
    $mfile = 'vzdata-safe.u1.train0b.bin'; ##-- carrot,uhura:bigger
  }
  if (!defined($efile)) {
    #$efile  = 'vzdata-safe.u1.r-256.tw-Hmax.xn-0.tpd-100.mdf-2.lc-1.xcheck.d/eval.all.xml';
    #$efile = 'vzdata-safe.u1.r-512.tw-Hmax.xn-0.tpd-100.mdf-2.lc-1.xcheck.d/eval.all.xml';
    ##--
    #$efile  = 'vzdata-safe.u1.train0a.eval.bin';
    $efile  = 'vzdata-safe.u1.train0b.eval.bin';
  }

  ##----
  if ($mfile) {
    print STDERR "$0: load($mfile)\n";
    my $map = DocClassify::Mapper::LSI->loadFile("$mfile")
      or die("$0: Mapper->loadFile($mfile) failed: $!");
  }

  ##----
  print STDERR "$0: load($efile)\n";
  my $eval = DocClassify::Eval->loadFile("$efile")
    or die("$0: Eval->loadFile($efile) failed: $!");
  $map->loadCrossCheckEval($eval);

  ##--------------------
  ## ... again (vars)
  my $lcenum = $map->{lcenum};
  my $gcenum = $map->{gcenum};
  my $gcids  = pdl(long, [@{$gcenum->{sym2id}}{@{$lcenum->{id2sym}}}]);             ##-- [$lci] -> $gci
  my $lcids  = zeroes(long,$gcids->max+1); $lcids->index($gcids) .= $gcids->xvals;  ##-- [$gci] -> $lci
  my $NC     = $map->{lcenum}->size;
  my $NCg    = $gcids->max+1;
  my $ND     = $map->{denum}->size;
  my $dcm   = $map->{dcm};
  my $dcm_z = $dcm->missing->sclr;
  my $d2c   = $dcm->xchg(0,1)->_missing('inf')->minimum_ind->decode;  ##-- [$di] -> $ci_best
  $dcm->missing($dcm_z);
  my $dc_dist = $map->{dc_dist};

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

  ##----------------------------------------------------
  ## histogram confusion matrix, <=
  ## $cc1_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && dist($d,$c2)<=dist($d,$c1)}|
  my $d_dist1 = $dc_dist->index2d($d2c->xvals,$d2c); ##-- [$di] -> dist($c1) : $c1=wanted($di)
  my $dc1_e_mask  = ($dc_dist <= $d_dist1);
  my $dc1_e_which = whichND($dc1_e_mask);
  my $cc1_e_which = $d2c->index($dc1_e_which->slice("(0),"))->cat($dc1_e_which->slice("(1),"))->xchg(0,1);
  my $cc1_hist = PDL::CCS::Nd->newFromWhich($cc1_e_which,ones($cc1_e_which->dim(1)))->dummy(0,1)->sumover->decode;
  my $cc1_p12  = $cc1_hist / $cc1_hist->sum;
  my $cc1_p1g2 = ($cc1_hist / $cc1_hist->sumover->slice("*1,"))->inplace->setnantobad->inplace->setbadtoval(0);
  my $cc1_p2g1 = ($cc1_hist / $cc1_hist->xchg(0,1)->sumover)->inplace->setnantobad->inplace->setbadtoval(0);

  ## $cc0_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && dist($d,$c2) < dist($d,$c1)}|
  my $dc0_e_mask  = ($dc_dist <  $d_dist1);
  my $dc0_e_which = whichND($dc0_e_mask);
  my $cc0_e_which = $d2c->index($dc0_e_which->slice("(0),"))->cat($dc0_e_which->slice("(1),"))->xchg(0,1);
  my $cc0_hist = PDL::CCS::Nd->newFromWhich($cc0_e_which,ones($cc0_e_which->dim(1)))->dummy(0,1)->sumover->decode;
  my $cc0_p12  = $cc0_hist / $cc0_hist->sum;
  my $cc0_p1g2 = ($cc0_hist / $cc0_hist->sumover->slice("*1,"))->inplace->setnantobad->inplace->setbadtoval(0);
  my $cc0_p2g1 = ($cc0_hist / $cc0_hist->xchg(0,1)->sumover)->inplace->setnantobad->inplace->setbadtoval(0);

  ##-- plot
  our (%iplot);
  %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'c1 : wanted', ytitle=>'c2 : measured');
  if (1) {
    imag($cc1_hist,{%iplot,title=>'Histogram: |{d : dist(d,c2) <= dist(d,c1)}|'});
    imag($cc1_p12,{%iplot, title=>'p(dist(d,c2) <= dist(d,c1))'});
    imag($cc1_p1g2,{%iplot,title=>'p(dist(d,c2) <= dist(d,c1) | c2)'});
    imag($cc1_p2g1,{%iplot,title=>'p(dist(d,c2) <= dist(d,c1) | c1)'});
    ##
    imag($cc0_hist,{%iplot,title=>'Histogram: |{d : dist(d,c2) < dist(d,c1)}|'});
    imag($cc0_p12,{%iplot, title=>'p(dist(d,c2) < dist(d,c1))'});
    imag($cc0_p1g2,{%iplot,title=>'p(dist(d,c2) < dist(d,c1) | c2)'});
    imag($cc0_p2g1,{%iplot,title=>'p(dist(d,c2) < dist(d,c1) | c1)'});
  }

  ##----------------------------------------------------
  ## histogram confusion matrix, min
  ##  $ccg_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && $c2=got($d)}|
  #my $d_dist1 = $dc_dist->index2d($d2c->xvals,$d2c); ##-- [$di] -> dist($c1) : $c1=wanted($di)
  my $d_c2 = $d2c_bydist = $dc_dist->xchg(0,1)->minimum_ind;
  my $d_c1 = $d2c;
  #my $d_c2_mask   = zeroes(byte,$dc_dist->dims); $d_c2_mask->index2d(xvals(long,$ND),$d_c2) .= 1;
  #my $dg_which = which($d_c1 != $d_c2);
  my $dg_which = $d_c1->xvals;
  my $ccg_which = $d_c1->index($dg_which)->cat($d_c2->index($dg_which))->xchg(0,1);
  my $ccg_hist = PDL::CCS::Nd->newFromWhich($ccg_which,$dg_which->ones)->dummy(0,1)->sumover->decode;
  my $ccg_p12  = $ccg_hist / $ccg_hist->sum;
  my $ccg_p1g2 = ($ccg_hist / $ccg_hist->sumover->slice("*1,"))->inplace->setnantobad->inplace->setbadtoval(0);
  my $ccg_p2g1 = ($ccg_hist / $ccg_hist->xchg(0,1)->sumover)->inplace->setnantobad->inplace->setbadtoval(0);

  ##-- plot
  #our (%iplot);
  %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'c1 : wanted', ytitle=>'c2 : got');
  if (1) {
    imag($ccg_hist,{%iplot,title=>'Histogram: |{ d : wanted(d)=c1 & got(d)=c2 }|'});
    imag($ccg_p12,{%iplot, title=>'p(Wanted=c1,Got=c2)'});
    imag($ccg_p1g2,{%iplot,title=>'p(Wanted=c1|Got=c2) ~ Precision'});
    imag($ccg_p2g1,{%iplot,title=>'p(Got=c2|Wanted=c1) ~ Recall'});
    imag(F1($ccg_p2g1,$ccg_p1g2), {%iplot,title=>'F1(p(Wanted=c1|Got=c2),p(Got=c2|Wanted=c1))'});
  }
  ##-- CONTINUE HERE: how to use this data at runtime ?!

  ##----------------------------------------------------
  ## repeat of test_compile_xcheck()


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

  ##-- plots
  our (%eplot);

  ##--------------------
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

  ##--------------------
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

  ##--------------------
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
  if (1) {
    usepgplot;
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked (+:black, -:red)'},
	     [$gcids-.1, $c1dist_mu_raw,$c1dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.1, $c0dist_mu_raw,$c0dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu,    $c1dist_sd,      {}],
	     [$gcids+.1, $c0dist_mu,    $c0dist_sd,      {color=>'red'}],
	     [$gcids,    $nc/$nc->max,$nc->zeroes,       {color=>'yellow',charsize=>2}]
	    );
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked (+:black, -:red)'},
	     [$gcids-.1, $c1dist_mu_raw,$nc->zeroes, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.1, $c0dist_mu_raw,$nc->zeroes, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu,    $c1dist_sd,  {}],
	     [$gcids+.1, $c0dist_mu,    $c0dist_sd,  {color=>'red'}],
	    );
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
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
  if (1) {
    usepgplot;
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ploterrs({%eplot,title=>"Normal Fit by Category: Hacked & Adjusted (+:black, -:red) [conf=($conf_nofn,$conf_nofp)]"},
	     [$gcids-.2, $c1dist_mu_raw,$c1dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.2, $c0dist_mu_raw,$c0dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu_adj,$c1dist_sd,      {}],
	     [$gcids+.1, $c0dist_mu_adj,$c0dist_sd,      {color=>'red'}],
	     [$gcids,    $nc/$nc->max,$nc->zeroes,       {color=>'yellow',charsize=>2}],
	     [$gcids-.1, $cutoff_nofn,  $nc->zeroes,     {symbol=>'square'}],
	     [$gcids+.1, $cutoff_nofp,  $nc->zeroes,     {symbol=>'square',color=>'red'}],
	     [$gcids+.1, $cutoff_wavg,  $nc->zeroes,     {symbol=>'cross',color=>'green'}]);
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
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
  $acc->('$dc_dist'); ##-- .615

  ##-- stupid p-value tests
  $acc->('$dc_cdf','min'); ##-- .547
  $acc->('$dc1_scdf'); ##-- .428
  $acc->('$dc1_scdf_adj'); ##-- .597, all=.592
  $acc->('F1((1-$dc1_scdf_adj),(1-$dc0_scdf_adj))', 'max'); ##-- .597, all=.591 ##-- USE THIS!
  if (1) {
    our $dc_raw_l = 2-$dc_dist; #-- sim=2-d; d=2-1-cos; -> d=2-sim -> sim=1+cos
    our $dc_raw = zeroes($ND,$NCg)+$dc_raw_l->min;
    $dc_raw->dice_axis(1,$gcids) .= $dc_raw_l;
    ##
    #usepgplot;
    %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'doc', ytitle=>'cat');
    imag($dc_raw,{%iplot,title=>'Raw Similarity: 1+cos(LSI(doc),LSI(cat))'});
    imag($dc_raw,{%iplot,title=>'Raw Similarity: 1+cos(LSI(doc),LSI(cat)) [log-scale]',itf=>'log'});
    ##
    our $dc_sim_l = F1((1-$dc1_scdf_adj),(1-$dc0_scdf_adj)); ##-- USE THIS!
    our $dc_sim = zeroes($ND,$NCg)+$dc_sim_l->min;
    $dc_sim->dice_axis(1,$gcids) .= $dc_sim_l;
    imag($dc_sim,{%iplot,title=>'F-Similarity: F1(1-cdf(doc~cat), 1-cdf(doc!~cat))'});
    imag($dc_sim,{%iplot,title=>'F-Similarity: F1(1-cdf(doc~cat), 1-cdf(doc!~cat)) [log-scale]', itf=>'log'});
    ##
    our $cc_which1 = $d2c->cat($dc_which1->slice("(1),"))->xchg(0,1);
    our $asim1_l = PDL::CCS::Nd->newFromWhich($cc_which1,$dc_sim_l->indexND($dc_which1))->dummy(0,1)->average_nz->_missing(0)->decode;
    our $cc_which0 = $d2c->index($dc_which0->slice("(0)"))->cat($dc_which0->slice("(1),"))->xchg(0,1);
    our $asim0_l = PDL::CCS::Nd->newFromWhich($cc_which0,$dc_sim_l->indexND($dc_which0))->dummy(0,1)->average_nz->_missing(0)->decode;
    our $asim_l  = ($asim1_l+$asim0_l);
    our $asim = zeroes($NCg,$NCg)+$asim_l->min;
    $asim->dice_axis(0,$gcids)->dice_axis(1,$gcids) .= $asim_l;
    %iplot = (%iplot,xtitle=>'c1 : wanted',ytitle=>'c2 : predicted');
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity'});
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity [sqrt-scale]',itf=>'sqrt'});
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity [log-scale]',itf=>'log'});
    imag(1-$asim,{%iplot,title=>'Avg Adjusted Distance'});
    imag(1-$asim,{%iplot,title=>'Avg Adjusted Distance [log-scale]',itf=>'log'});
    ##
    our $asim_mask1 = ($asim->maximum_ind->slice("*1,")==$asim->xvals);
    imag($asim*$asim_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision'});
    imag($asim*$asim_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [log-scale]',itf=>'log'});
    ##
    our $asim_mask2 = ($asim->xchg(0,1)->maximum_ind==$asim->yvals);
    imag($asim*$asim_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall'});
    imag($asim*$asim_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [log-scale]',itf=>'log'});

    our $asimx = $asim->pdl;
    our $badc = pdl(long,[0,29,30]);
    $asimx->dice_axis(0,$badc) .= 0 if (all($badc<$asimx->dim(0)));
    $asimx->dice_axis(1,$badc) .= 0 if (all($badc<$asimx->dim(1)));
    imag($asimx,{%iplot,title=>'Avg Adjusted Similarity [safe]'});
    imag($asimx,{%iplot,title=>'Avg Adjusted Similarity [safe,log-scale]',itf=>'log'});
    ##
    our $asimx_mask1 = ($asimx->maximum_ind->slice("*1,")==$asimx->xvals);
    imag($asimx*$asimx_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [safe]'});
    imag($asimx*$asimx_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [safe,log-scale]',itf=>'log'});
    ##
    our $asimx_mask2 = ($asimx->xchg(0,1)->maximum_ind==$asimx->yvals);
    imag($asimx*$asimx_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [safe]'});
    imag($asimx*$asimx_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [safe,log-scale]',itf=>'log'});
  }

  ##-- misc
  $acc->('$dc0_scdf'); ##-- .600
  $acc->('F1((1-$dc1_cdf),(1-$dc0_cdf),1e-5)','max'); ##-- .470
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),$nc->slice("*1,"))','max'); ##-- .626
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),($nc/$ND)->slice("*1,"))','max'); ##-- .430
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(1-($nc/$ND))->slice("*1,"))','max'); ##-- .460
  $acc->('Fb((1-$dc1_scdf_adj),(1-$dc0_scdf_adj),(1-($nc/$ND))->slice("*1,"))','max'); ##-- .592, all=.5824; safe-u1: .700
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(1-($nnc/$ND))->slice("*1,"))','max'); ##-- .430
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(2-($nnc/$ND))->slice("*1,"))','max'); ##-- .478
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(1+($nc/$ND))->slice("*1,"))','max');  ##-- .478
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(exp(-$nc/$ND))->slice("*1,"))','max'); ##-- .460

  ##-- try this
  $acc->('$dc1_scdf**($nc/$ND)->slice("*1,")'); ##-- .630, all=.5832
  $acc->('$dc1_scdf_adj**($nc/$ND)->slice("*1,")'); ##-- .6589

  ##-- ... or this?
  $acc->('$dc1_scdf0**($nc/$ND)->slice("*1,")'); ##-- .6719 all=.5248  ##-- BEST for safe.u1
  $acc->('$dc1_scdf0**($nc/$nc->max)->slice("*1,")'); ##-- .670
  $acc->('$dc0_scdf0**($nc/$ND)->slice("*1,")'); ##-- .642
  $acc->('$dc0_scdf0**($nc/$nc->max)->slice("*1,")'); ##-- .642

  ##-- ... or these?
  $acc->('$dc1_scdf0**($nc/$ND)->slice("*1,") + $dc0_scdf0**($nnc/$ND)->slice("*1,")'); ##-- .671
  $acc->('$dc1_scdf**($nc/$ND)->slice("*1,") + $dc0_scdf**($nnc/$ND)->slice("*1,")'); ##-- .640
  $acc->('$dc1_scdf**($nc/$ND)->slice("*1,") * ($dc0_scdf)**($nnc/$ND)->slice("*1,")'); ##-- .584


  print STDERR "$0: test_load_xcheck() done: what now?\n";
}
#test_load_xcheck(@ARGV);

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
BEGIN {
  our $plot2d = 1;
  our $plot3d = 1;
  ##
  our $plot_hardcopy = 0;
  our ($dev2d,$dev3d);
  if ($plot_hardcopy) {
    $dev2d = '/CPS';
    $dev3d = '/WD';
  } else {
    $dev3d = $dev2d = '/XS';
  }
}
sub test_load_eval {
  my ($efile) = @_;
  if (!defined($efile)) {
    #$efile  = 'vzdata-safe.u1.r-256.tw-Hmax.xn-0.tpd-100.mdf-2.lc-1.xcheck.d/eval.all.xml';
    #$efile = 'vzdata-safe.u1.r-512.tw-Hmax.xn-0.tpd-100.mdf-2.lc-1.xcheck.d/eval.all.xml';
    ##--
    $efile = 'anno-big-size-10.n-3.r-768.mf-0.mdf-0.mtpd-0.tw-Hmax.lzc-vzsep.dist-u.xcheck.d/eval.all.xml';
  }
  $efile = "$efile.bin" if (-r "$efile.bin");

  ##----
  print STDERR "$0: load($efile)\n";
  my $eval = DocClassify::Eval->loadFile("$efile")
    or die("$0: Eval->loadFile($efile) failed: $!");
  $eval->saveFile("$efile.bin") if ($efile !~ /\.bin$/);

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
  my $d2c = pdl([@$lc_sym2id{map {$lab2docs->{$_}[0]{cats}[0]{name}} @{$denum->{id2sym}}}]); ##-- [$di] -> $ci_wanted

  ##--------------------
  ## ... other vars
  my $gcids  = pdl(long, [@{$gcenum->{sym2id}}{@{$lcenum->{id2sym}}}]);             ##-- [$lci] -> $gci
  my $lcids  = zeroes(long,$gcids->max+1); $lcids->index($gcids) .= $gcids->xvals;  ##-- [$gci] -> $lci
  my $NCg    = $gcids->max+1;

  ##--------------------
  ## $dc_dist (from Mapper::LSI::loadCrossCheckEval()
  my $dc_dist = zeroes($ND,$NC)+2; ##-- initialize to max
  my ($lab,$dcats2,@dci,@dcdist,$di); #$d12
  while (($lab,$d12)=each(%$lab2docs)) {
    if (!defined($di = $d_sym2id->{$lab})) {
      warn("$0: no internal ID for doc label '$lab' -- skipping");
      next;
    }
    $dcats2 = $d12->[1]{cats};
    @dci    = grep {defined($lc_sym2id->{$dcats2->[$_]{name}})} (0..$#$dcats2);
    @dcdist = map {$dcats2->[$_]{dist_raw}} @dci;
    $dc_dist->slice("($di),")->index(pdl(long,[@$lc_sym2id{map {$dcats2->[$_]{name}} @dci}])) .= pdl(\@dcdist);
  }

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

  ##----------------------------------------------------
  ## repeat of test_compile_xcheck()


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

  ##-- plots
  our (%eplot);

  ##--------------------
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

  ##--------------------
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

  ##--------------------
  ##-- plots
  if ($plot2d) {
    _usepgplot($dev2d);
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
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked (+:black, -:red)'},
	     [$gcids-.1, $c1dist_mu_raw,$c1dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.1, $c0dist_mu_raw,$c0dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu,    $c1dist_sd,      {}],
	     [$gcids+.1, $c0dist_mu,    $c0dist_sd,      {color=>'red'}],
	     [$gcids,    $nc/$nc->max,$nc->zeroes,       {color=>'yellow',charsize=>2}]
	    );
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked (+:black, -:red)'},
	     [$gcids-.1, $c1dist_mu_raw,$nc->zeroes, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.1, $c0dist_mu_raw,$nc->zeroes, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu,    $c1dist_sd,  {}],
	     [$gcids+.1, $c0dist_mu,    $c0dist_sd,  {color=>'red'}],
	    );
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
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
    ploterrs({%eplot,title=>"Normal Fit by Category: Hacked & Adjusted (+:black, -:red) [conf=($conf_nofn,$conf_nofp)]"},
	     [$gcids-.2, $c1dist_mu_raw,$c1dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted'}],
	     [$gcids+.2, $c0dist_mu_raw,$c0dist_sd_nan0, {symbol=>'plus',linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu_adj,$c1dist_sd,      {}],
	     [$gcids+.1, $c0dist_mu_adj,$c0dist_sd,      {color=>'red'}],
	     [$gcids,    $nc/$nc->max,$nc->zeroes,       {color=>'yellow',charsize=>2}],
	     [$gcids-.1, $cutoff_nofn,  $nc->zeroes,     {symbol=>'square'}],
	     [$gcids+.1, $cutoff_nofp,  $nc->zeroes,     {symbol=>'square',color=>'red'}],
	     [$gcids+.1, $cutoff_wavg,  $nc->zeroes,     {symbol=>'cross',color=>'green'}]);
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
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
  $acc->('$dc_dist'); ##-- .615

  ##-- stupid p-value tests
  $acc->('$dc_cdf','min'); ##-- .547
  $acc->('$dc1_scdf'); ##-- .428
  $acc->('$dc1_scdf_adj'); ##-- .597, all=.592
  $acc->('F1((1-$dc1_scdf_adj),(1-$dc0_scdf_adj))', 'max'); ##-- .597, all=.591 ##-- USE THIS! 
 if ($plot3d) {
    our $dc_raw_l = 2-$dc_dist; #-- sim=2-d; d=2-1-cos; -> d=2-sim -> sim=1+cos
    our $dc_raw = zeroes($ND,$NCg)+$dc_raw_l->min;
    $dc_raw->dice_axis(1,$gcids) .= $dc_raw_l;
    ##
    %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'doc', ytitle=>'cat');
    _usepgplot($dev3d);
    imag($dc_raw,{%iplot,title=>'Raw Similarity: 1+cos(LSI(doc),LSI(cat))'});
    imag($dc_raw,{%iplot,title=>'Raw Similarity: 1+cos(LSI(doc),LSI(cat)) [log-scale]',itf=>'log'});
    ##
    our $dc_sim_l = F1((1-$dc1_scdf_adj),(1-$dc0_scdf_adj)); ##-- USE THIS!
    our $dc_sim = zeroes($ND,$NCg)+$dc_sim_l->min;
    $dc_sim->dice_axis(1,$gcids) .= $dc_sim_l;
    imag($dc_sim,{%iplot,title=>'F-Similarity: F1(1-cdf(doc~cat), 1-cdf(doc!~cat))'});
    imag($dc_sim,{%iplot,title=>'F-Similarity: F1(1-cdf(doc~cat), 1-cdf(doc!~cat)) [log-scale]', itf=>'log'});
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
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity'});
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity [sqrt-scale]',itf=>'sqrt'});
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity [log-scale]',itf=>'log'});
    ##
    our $asim_mask1_raw = ($asim_raw->maximum_ind->slice("*1,")==$asim_raw->xvals);
    imag($asim_raw*$asim_mask1_raw,{%iplot,title=>'Best c1 by c2 ~ Precision [raw]',itf=>'linear'});
    imag($asim_raw*$asim_mask1_raw,{%iplot,title=>'Best c1 by c2 ~ Precision [raw,log-scale]',itf=>'log'});
    ##
    our $asim_mask1 = ($asim->maximum_ind->slice("*1,")==$asim->xvals);
    imag($asim*$asim_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [adjusted]'});
    imag($asim*$asim_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [adjusted,log-scale]',itf=>'log'});
    ##
    our $asim_mask2_raw = ($asim_raw->xchg(0,1)->maximum_ind==$asim_raw->yvals);
    imag($asim_raw*$asim_mask2_raw,{%iplot,title=>'Best c2 by c1 ~ Recall [raw]'});
    imag($asim_raw*$asim_mask2_raw,{%iplot,title=>'Best c2 by c1 ~ Recall [raw,log-scale]',itf=>'log'});
    ##
    our $asim_mask2 = ($asim->xchg(0,1)->maximum_ind==$asim->yvals);
    imag($asim*$asim_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [adjusted]'});
    imag($asim*$asim_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [adjusted,log-scale]',itf=>'log'});

    if (0) {
      our $asimx = $asim->pdl;
      our $badc = pdl(long,[0,29,30]);
      $asimx->dice_axis(0,$badc) .= 0 if (all($badc<$asimx->dim(0)));
      $asimx->dice_axis(1,$badc) .= 0 if (all($badc<$asimx->dim(1)));
      imag($asimx,{%iplot,title=>'Avg Adjusted Similarity [safe]'});
      imag($asimx,{%iplot,title=>'Avg Adjusted Similarity [safe,log-scale]',itf=>'log'});
      ##
      our $asimx_mask1 = ($asimx->maximum_ind->slice("*1,")==$asimx->xvals);
      imag($asimx*$asimx_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [safe]'});
      imag($asimx*$asimx_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [safe,log-scale]',itf=>'log'});
      ##
      our $asimx_mask2 = ($asimx->xchg(0,1)->maximum_ind==$asimx->yvals);
      imag($asimx*$asimx_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [safe]'});
      imag($asimx*$asimx_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [safe,log-scale]',itf=>'log'});
    }
  }

  ##----------------------------------------------------
  ## histogram confusion matrix, <=
  ## $cc1_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && dist($d,$c2)<=dist($d,$c1)}|
  my $d_dist1 = $dc_dist->index2d($d2c->xvals,$d2c); ##-- [$di] -> dist($c1) : $c1=wanted($di)
  my $dc1_e_mask  = ($dc_dist <= $d_dist1);
  my $dc1_e_which = whichND($dc1_e_mask);
  my $cc1_e_which = $d2c->index($dc1_e_which->slice("(0),"))->cat($dc1_e_which->slice("(1),"))->xchg(0,1);
  my $cc1_hist = PDL::CCS::Nd->newFromWhich($cc1_e_which,ones($cc1_e_which->dim(1)))->dummy(0,1)->sumover->decode;
  my $cc1_p12  = $cc1_hist / $cc1_hist->sum;
  my $cc1_p1g2 = ($cc1_hist / $cc1_hist->sumover->slice("*1,"))->inplace->setnantobad->inplace->setbadtoval(0);
  my $cc1_p2g1 = ($cc1_hist / $cc1_hist->xchg(0,1)->sumover)->inplace->setnantobad->inplace->setbadtoval(0);

  ## $cc0_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && dist($d,$c2) < dist($d,$c1)}|
  my $dc0_e_mask  = ($dc_dist <  $d_dist1);
  my $dc0_e_which = whichND($dc0_e_mask);
  my $cc0_e_which = $d2c->index($dc0_e_which->slice("(0),"))->cat($dc0_e_which->slice("(1),"))->xchg(0,1);
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
    ##
    imag($cc0_hist,{%iplot,title=>'Histogram: |{d : dist(d,c2) < dist(d,c1)}|'});
    imag($cc0_p12,{%iplot, title=>'p(dist(d,c2) < dist(d,c1))'});
    imag($cc0_p1g2,{%iplot,title=>'p(dist(d,c2) < dist(d,c1) | c2)'});
    imag($cc0_p2g1,{%iplot,title=>'p(dist(d,c2) < dist(d,c1) | c1)'});
  }

  ##----------------------------------------------------
  ## histogram confusion matrix, min
  ##  $ccg_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && $c2=got($d)}|
  #my $d_dist1 = $dc_dist->index2d($d2c->xvals,$d2c); ##-- [$di] -> dist($c1) : $c1=wanted($di)
  my $d_c2 = $d2c_bydist = $dc_dist->xchg(0,1)->minimum_ind;
  my $d_c1 = $d2c;
  #my $d_c2_mask   = zeroes(byte,$dc_dist->dims); $d_c2_mask->index2d(xvals(long,$ND),$d_c2) .= 1;
  #my $dg_which = which($d_c1 != $d_c2);
  my $dg_which = $d_c1->xvals;
  my $ccg_which = $d_c1->index($dg_which)->cat($d_c2->index($dg_which))->xchg(0,1);
  my $ccg_hist = PDL::CCS::Nd->newFromWhich($ccg_which,$dg_which->ones)->dummy(0,1)->sumover->decode;
  my $ccg_p12  = $ccg_hist / $ccg_hist->sum;
  my $ccg_p1g2 = ($ccg_hist / $ccg_hist->sumover->slice("*1,"))->inplace->setnantobad->inplace->setbadtoval(0);
  my $ccg_p2g1 = ($ccg_hist / $ccg_hist->xchg(0,1)->sumover)->inplace->setnantobad->inplace->setbadtoval(0);

  ##-- plot
  #our (%iplot);
  %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'c1 : wanted', ytitle=>'c2 : got');
  if ($plot3d) {
    _usepgplot($dev3d);
    imag($ccg_hist,{%iplot,title=>'Histogram: |{ d : wanted(d)=c1 & got(d)=c2 }|'});
    imag($ccg_p12,{%iplot, title=>'p(Wanted=c1,Got=c2)'});
    imag($ccg_p1g2,{%iplot,title=>'p(Wanted=c1|Got=c2) ~ Precision'});
    imag($ccg_p2g1,{%iplot,title=>'p(Got=c2|Wanted=c1) ~ Recall'});
    imag(F1($ccg_p2g1,$ccg_p1g2), {%iplot,title=>'F1(p(Wanted=c1|Got=c2),p(Got=c2|Wanted=c1))'});
  }
  ##-- CONTINUE HERE: how to use this data at runtime ?!


  ##-- misc
  $acc->('$dc0_scdf'); ##-- .600
  $acc->('F1((1-$dc1_cdf),(1-$dc0_cdf),1e-5)','max'); ##-- .470
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),$nc->slice("*1,"))','max'); ##-- .626
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),($nc/$ND)->slice("*1,"))','max'); ##-- .430
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(1-($nc/$ND))->slice("*1,"))','max'); ##-- .460
  $acc->('Fb((1-$dc1_scdf_adj),(1-$dc0_scdf_adj),(1-($nc/$ND))->slice("*1,"))','max'); ##-- .592, all=.5824; safe-u1: .700
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(1-($nnc/$ND))->slice("*1,"))','max'); ##-- .430
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(2-($nnc/$ND))->slice("*1,"))','max'); ##-- .478
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(1+($nc/$ND))->slice("*1,"))','max');  ##-- .478
  $acc->('Fb((1-$dc1_scdf),(1-$dc0_scdf),(exp(-$nc/$ND))->slice("*1,"))','max'); ##-- .460

  ##-- try this
  $acc->('$dc1_scdf**($nc/$ND)->slice("*1,")'); ##-- .630, all=.5832
  $acc->('$dc1_scdf_adj**($nc/$ND)->slice("*1,")'); ##-- .6589

  ##-- ... or this?
  $acc->('$dc1_scdf0**($nc/$ND)->slice("*1,")'); ##-- .6719 all=.5248  ##-- BEST for safe.u1
  $acc->('$dc1_scdf0**($nc/$nc->max)->slice("*1,")'); ##-- .670
  $acc->('$dc0_scdf0**($nc/$ND)->slice("*1,")'); ##-- .642
  $acc->('$dc0_scdf0**($nc/$nc->max)->slice("*1,")'); ##-- .642

  ##-- ... or these?
  $acc->('$dc1_scdf0**($nc/$ND)->slice("*1,") + $dc0_scdf0**($nnc/$ND)->slice("*1,")'); ##-- .671
  $acc->('$dc1_scdf**($nc/$ND)->slice("*1,") + $dc0_scdf**($nnc/$ND)->slice("*1,")'); ##-- .640
  $acc->('$dc1_scdf**($nc/$ND)->slice("*1,") * ($dc0_scdf)**($nnc/$ND)->slice("*1,")'); ##-- .584


  print STDERR "$0: test_load_xcheck() done: what now?\n";
  exit 0;
}
#test_load_eval(@ARGV);



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


##======================================================================
sub test_load_eval_2 {
  my ($efile) = @_;
  if (!defined($efile)) {
    #$efile = "data/train_data_2009_12_18_v3.eval.xml";
    $efile = "data/test_data_2009_12_18_v3.eval.xml";
  }
  $efile = "$efile.bin" if (-r "$efile.bin");

  ##----
  print STDERR "$0: load($efile)\n";
  my $eval = DocClassify::Eval->loadFile("$efile")
    or die("$0: Eval->loadFile($efile) failed: $!");
  $eval->saveFile("$efile.bin") if ($efile !~ /\.bin$/);

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
  my $d2c = pdl([@$lc_sym2id{map {$lab2docs->{$_}[0]{cats}[0]{name}} @{$denum->{id2sym}}}]); ##-- [$di] -> $ci_wanted

  ##--------------------
  ## ... other vars
  my $gcids  = pdl(long, [@{$gcenum->{sym2id}}{@{$lcenum->{id2sym}}}]);             ##-- [$lci] -> $gci
  my $lcids  = zeroes(long,$gcids->max+1); $lcids->index($gcids) .= $gcids->xvals;  ##-- [$gci] -> $lci
  my $NCg    = $gcids->max+1;

  ##--------------------
  ## $dc_dist (from Mapper::LSI::loadCrossCheckEval()
  my $dc_dist = zeroes($ND,$NC)+2; ##-- initialize to max
  my ($lab,$dcats2,@dci,@dcdist,$di); #$d12
  while (($lab,$d12)=each(%$lab2docs)) {
    if (!defined($di = $d_sym2id->{$lab})) {
      warn("$0: no internal ID for doc label '$lab' -- skipping");
      next;
    }
    $dcats2 = $d12->[1]{cats};
    @dci    = grep {defined($lc_sym2id->{$dcats2->[$_]{name}})} (0..$#$dcats2);
    @dcdist = map {$dcats2->[$_]{dist_raw}} @dci;
    $dc_dist->slice("($di),")->index(pdl(long,[@$lc_sym2id{map {$dcats2->[$_]{name}} @dci}])) .= pdl(\@dcdist);
  }

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

  ##----------------------------------------------------
  ## repeat of test_compile_xcheck()


  ##--------------------
  ##-- from testme.perl test_errors(), above
  ## $dcdist_mu = pdl(1): global avg    dist($ci,$di)
  ## $dcdist_sd = pdl(1): global stddev dist($ci,$di)
  my $dcdist_mu = $dc_dist->flat->average;
  my $dcdist_sd = (($dc_dist - $dcdist_mu)**2)->flat->average->sqrt;

  my $nc_min = 2;#3; #10; #50; ##-- minimum #/docs to use fit

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

  ##-- plots
  our (%eplot);

  ##--------------------
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

  ##--------------------
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

  ##--------------------
  ##-- plots
  if ($plot2d) {
    _usepgplot($dev2d);
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category (Positives:black, Negatives:red)'},
	     [$gcids-.1, $c1dist_mu,$c1dist_sd_nan0, {}],
	     [$gcids+.1, $c0dist_mu,$c0dist_sd_nan0, {color=>'red'}]);
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
  }

  ##--------------------
  ##-- hack mu, sd: positives
  $c1dist_mu->where(!$c1dist_isgood) .= $c1dist_mu->where($c1dist_isgood)->minimum;
  $c1dist_sd->where(!$c1dist_isgood) .= $c1dist_sd->where($c1dist_isgood)->minimum/2;

  ##--------------------
  ##-- hack mu, sd: negatives
  $c0dist_mu->where(!$c0dist_isgood) .= $c0dist_mu->where($c0dist_isgood)->minimum;
  $c0dist_sd->where(!$c0dist_isgood) .= $c0dist_sd->where($c0dist_isgood)->maximum;

  ##--------------------
  ##-- plot: hacked mu,sd
  if ($plot2d) {
    _usepgplot($dev2d);
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked (Positives:black, Negatives:red)'},
	     [$gcids-.1, $c1dist_mu,$c1dist_sd, {}],
	     [$gcids+.1, $c0dist_mu,$c0dist_sd, {color=>'red'}]);
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
  }

  ##--------------------
  ## extend standard deviations (expect the unexpected)
  my $c1dist_sd2 = $c1dist_sd * 10;
  my $c0dist_sd2 = $c0dist_sd * 10;
  if ($plot2d) {
    _usepgplot($dev2d);
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked*2 (Positives:black, Negatives:red)'},
	     [$gcids-.1, $c1dist_mu,$c1dist_sd2, {}],
	     [$gcids+.1, $c0dist_mu,$c0dist_sd2, {color=>'red'}]);
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
  }


  ##--------------------
  ## compute cdfs
  my ($dc1_cdf0,$dc0_cdf0, $dc1_scdf0,$dc0_scdf0);
  my ($dc_cdf,$dc1_cdf,$dc0_scdf,$dc1_scdf);
  $dc_cdf = gausscdf($dc_dist, $cdist_mu->slice("*1,"), $cdist_sd->slice("*1,")); ##-- [$di,$ci] -> cdf:global(d,c)
  ##
  $dc1_cdf = gausscdf($dc_dist, $c1dist_mu->slice("*1,"), $c1dist_sd2->slice("*1,"));##-- [$di,$ci] -> cdf:pos(d,c)
  $dc0_cdf = gausscdf($dc_dist, $c0dist_mu->slice("*1,"), $c0dist_sd2->slice("*1,"));##-- [$di,$ci] -> cdf:neg(d,c)
  $dc1_scdf = li1($dc1_cdf,1e-5);
  $dc0_scdf = li1($dc0_cdf,1e-5);

  ##-- $dc10_p : [$di,$c1,$c0] -> p(1[$c1]=1|$di) ; p(1[$c0]=0|$di)
  my $dc10_p = zeroes($ND,$NC,$NC);
  #$dc10_p .= $dc0_scdf->slice(",,*$NC");
  #$dc10_p->diagonal(1,2) .= (1-$dc1_scdf);
  $dc10_p .= $dc0_cdf->slice(",,*$NC");
  $dc10_p->diagonal(1,2) .= (1-$dc1_cdf);
  my $dcx_p = $dc10_p->mv(0,-1)->prodover->mv(-1,0); ##-- [$di,$ci] -> p($ci|$di) * \prod_{$cj} p(!$cj|$di)

  ##-- $dc10_pg : [$di,$c1,$c0] -> p(1[$c1]=1|$di) ; p(1[$c0]=0|$di), using global sd
  my $dcdist_sd2 = $dcdist_sd * 10;
  my $dc_cdf_g = gausscdf($dc_dist, $cdist_mu->slice("*1,"), $dcdist_sd2); ##-- [$di,$ci] -> cdf:global(d,c)
  my $dc10_pg = zeroes($ND,$NC,$NC);
  $dc10_pg .= $dc_cdf_g->slice(",,*$NC");
  $dc10_pg->diagonal(1,2) .= (1-$dc_cdf_g);
  my $dcx_pg = $dc10_pg->mv(0,-1)->prodover->mv(-1,0); ##-- [$di,$ci] -> p($ci|$di) * \prod_{$cj} p(!$cj|$di)

  ##-- multiply by cat-prob (post-hoc)
  my $pc1 = $nc/$nc->sum;
  my $pc0 = 1-$pc1;
  my $dcx_pp = $dcx_p*$pc1->slice("*1,");

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
  $acc->('$dc_dist'); ##-- .689, .716

  ##-- stupid p-value tests
  $acc->('$dc_cdf'); ##-- .434, .671
  $acc->('$dc1_scdf',); ##-- .649, .702
  $acc->('$dcx_p','max'); ##-- .564, .706
  $acc->('$dcx_pg','max'); ##-- .557, .702
  $acc->('$dcx_pp','max'); ##-- .527, .381

  ##-- grr...
  if ($plot3d) {
    our $dc_raw_l = 2-$dc_dist; #-- sim=2-d; d=2-1-cos; -> d=2-sim -> sim=1+cos
    our $dc_raw = zeroes($ND,$NCg)+$dc_raw_l->min;
    $dc_raw->dice_axis(1,$gcids) .= $dc_raw_l;
    ##
    %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'doc', ytitle=>'cat');
    _usepgplot($dev3d);
    imag($dc_raw,{%iplot,title=>'Raw Similarity: 1+cos(LSI(doc),LSI(cat))'});
    imag($dc_raw,{%iplot,title=>'Raw Similarity: 1+cos(LSI(doc),LSI(cat)) [log-scale]',itf=>'log'});
    ##
    our $dc_sim_l = $dcx_p;
    our $dc_sim = zeroes($ND,$NCg)+$dc_sim_l->min;
    $dc_sim->dice_axis(1,$gcids) .= $dc_sim_l;
    imag($dc_sim,{%iplot,title=>'P-Similarity: p(d~ci) * PI_j(d!~cj)'});
    imag($dc_sim,{%iplot,title=>'P-Similarity: p(d~ci) * PI_j(d!~cj) [log-scale]', itf=>'log'});
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
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity'});
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity [sqrt-scale]',itf=>'sqrt'});
    imag($asim,{%iplot,title=>'Avg Adjusted Similarity [log-scale]',itf=>'log'});
    ##
    our $asim_mask1_raw = ($asim_raw->maximum_ind->slice("*1,")==$asim_raw->xvals);
    imag($asim_raw*$asim_mask1_raw,{%iplot,title=>'Best c1 by c2 ~ Precision [raw]',itf=>'linear'});
    imag($asim_raw*$asim_mask1_raw,{%iplot,title=>'Best c1 by c2 ~ Precision [raw,log-scale]',itf=>'log'});
    ##
    our $asim_mask1 = ($asim->maximum_ind->slice("*1,")==$asim->xvals);
    imag($asim*$asim_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [adjusted]'});
    imag($asim*$asim_mask1,{%iplot,title=>'Best c1 by c2 ~ Precision [adjusted,log-scale]',itf=>'log'});
    ##
    our $asim_mask2_raw = ($asim_raw->xchg(0,1)->maximum_ind==$asim_raw->yvals);
    imag($asim_raw*$asim_mask2_raw,{%iplot,title=>'Best c2 by c1 ~ Recall [raw]'});
    imag($asim_raw*$asim_mask2_raw,{%iplot,title=>'Best c2 by c1 ~ Recall [raw,log-scale]',itf=>'log'});
    ##
    our $asim_mask2 = ($asim->xchg(0,1)->maximum_ind==$asim->yvals);
    imag($asim*$asim_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [adjusted]'});
    imag($asim*$asim_mask2,{%iplot,title=>'Best c2 by c1 ~ Recall [adjusted,log-scale]',itf=>'log'});
  }

  ##----------------------------------------------------
  ## histogram confusion matrix, <=
  ## $cc1_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && dist($d,$c2)<=dist($d,$c1)}|
  my $d_dist1 = $dc_dist->index2d($d2c->xvals,$d2c); ##-- [$di] -> dist($c1) : $c1=wanted($di)
  my $dc1_e_mask  = ($dc_dist <= $d_dist1);
  my $dc1_e_which = whichND($dc1_e_mask);
  my $cc1_e_which = $d2c->index($dc1_e_which->slice("(0),"))->cat($dc1_e_which->slice("(1),"))->xchg(0,1);
  my $cc1_hist = PDL::CCS::Nd->newFromWhich($cc1_e_which,ones($cc1_e_which->dim(1)))->dummy(0,1)->sumover->decode;
  my $cc1_p12  = $cc1_hist / $cc1_hist->sum;
  my $cc1_p1g2 = ($cc1_hist / $cc1_hist->sumover->slice("*1,"))->inplace->setnantobad->inplace->setbadtoval(0);
  my $cc1_p2g1 = ($cc1_hist / $cc1_hist->xchg(0,1)->sumover)->inplace->setnantobad->inplace->setbadtoval(0);

  ## $cc0_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && dist($d,$c2) < dist($d,$c1)}|
  my $dc0_e_mask  = ($dc_dist <  $d_dist1);
  my $dc0_e_which = whichND($dc0_e_mask);
  my $cc0_e_which = $d2c->index($dc0_e_which->slice("(0),"))->cat($dc0_e_which->slice("(1),"))->xchg(0,1);
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
    ##
    imag($cc0_hist,{%iplot,title=>'Histogram: |{d : dist(d,c2) < dist(d,c1)}|'});
    imag($cc0_p12,{%iplot, title=>'p(dist(d,c2) < dist(d,c1))'});
    imag($cc0_p1g2,{%iplot,title=>'p(dist(d,c2) < dist(d,c1) | c2)'});
    imag($cc0_p2g1,{%iplot,title=>'p(dist(d,c2) < dist(d,c1) | c1)'});
  }
  
  ##----------------------------------------------------
  ## histogram confusion matrix, min
  ##  $ccg_hist : [$c1,$c2] -> |{$d : $c1=wanted($d) && $c2=got($d)}|
  #my $d_dist1 = $dc_dist->index2d($d2c->xvals,$d2c); ##-- [$di] -> dist($c1) : $c1=wanted($di)
  my $d_c2 = $d2c_bydist = $dc_dist->xchg(0,1)->minimum_ind;
  my $d_c1 = $d2c;
  #my $d_c2_mask   = zeroes(byte,$dc_dist->dims); $d_c2_mask->index2d(xvals(long,$ND),$d_c2) .= 1;
  #my $dg_which = which($d_c1 != $d_c2);
  my $dg_which = $d_c1->xvals;
  my $ccg_which = $d_c1->index($dg_which)->cat($d_c2->index($dg_which))->xchg(0,1);
  my $ccg_hist = PDL::CCS::Nd->newFromWhich($ccg_which,$dg_which->ones)->dummy(0,1)->sumover->decode;
  my $ccg_p12  = $ccg_hist / $ccg_hist->sum;
  my $ccg_p1g2 = ($ccg_hist / $ccg_hist->sumover->slice("*1,"))->inplace->setnantobad->inplace->setbadtoval(0);
  my $ccg_p2g1 = ($ccg_hist / $ccg_hist->xchg(0,1)->sumover)->inplace->setnantobad->inplace->setbadtoval(0);

  ##-- plot
  #our (%iplot);
  %iplot = (DrawWedge=>1, itf=>'linear', xtitle=>'c1 : wanted', ytitle=>'c2 : got');
  if ($plot3d) {
    _usepgplot($dev3d);
    imag($ccg_hist,{%iplot,title=>'Histogram: |{ d : wanted(d)=c1 & got(d)=c2 }|'});
    imag($ccg_p12,{%iplot, title=>'p(Wanted=c1,Got=c2)'});
    imag($ccg_p1g2,{%iplot,title=>'p(Wanted=c1|Got=c2) ~ Precision'});
    imag($ccg_p2g1,{%iplot,title=>'p(Got=c2|Wanted=c1) ~ Recall'});
    imag(F1($ccg_p2g1,$ccg_p1g2), {%iplot,title=>'F1(p(Wanted=c1|Got=c2),p(Got=c2|Wanted=c1))'});
  }
  ##-- CONTINUE HERE: how to use this data at runtime ?!


  print STDERR "$0: test_load_eval_2() done: what now?\n";
  exit 0;
}
#test_load_eval_2(@ARGV);

##======================================================================
## EVAL: UTILS

## $eval = load_eval($efile)
##  + loads global $::eval from "$efile.bin" or "$efile"
##  + saves "$efile.bin"
sub load_eval {
  my $efile = shift;
  die("$0: load_eval(): \$efile undefined!") if (!defined($efile));
  $efile = "$efile.bin" if (-r "$efile.bin");

  print STDERR "$0: load_eval($efile)\n";
  my $eval = DocClassify::Eval->loadFile("$efile")
    or die("$0: Eval->loadFile($efile) failed: $!");
  $eval->saveFile("$efile.bin") if ($efile !~ /\.bin$/);

  return $eval;
}

## undef = eval2dcdist($eval)
##  + load populates global vars from $eval:
##    ...
sub eval2dcdist {
  my $eval = shift;

  ##--------------------
  ## vars
  our $lab2docs = $eval->{lab2docs};

  ##--------------------
  ## create enums
  our $denum  = MUDL::Enum->new;
  @{$denum->{id2sym}} = keys %$lab2docs;
  @{$denum->{sym2id}}{@{$denum->{id2sym}}} = (0..$#{$denum->{id2sym}});
  our $d_sym2id = $denum->{sym2id};
  our $ND = $denum->size;
  ##
  our $gcenum = MUDL::Enum->new;
  my ($d12,$cat);
  foreach $d12 (values(%$lab2docs)) {
    foreach $cat (map {@{$_->{cats}}} @$d12) {
      $cat->{id} = $1 if ($cat->{name} =~ /^(\d+)_/);
      $gcenum->addIndexedSymbol(@$cat{qw(name id)});
    }
  }
  our $lcenum = $gcenum->clone->compact;
  our $lc_sym2id = $lcenum->{sym2id};
  our $NC = $lcenum->size;

  ##--------------------
  ## compile $d2c
  our $d2c = pdl([@$lc_sym2id{map {$lab2docs->{$_}[0]{cats}[0]{name}} @{$denum->{id2sym}}}]); ##-- [$di] -> $ci_wanted

  ##--------------------
  ## ... other vars
  our $gcids  = pdl(long, [@{$gcenum->{sym2id}}{@{$lcenum->{id2sym}}}]);             ##-- [$lci] -> $gci
  our $lcids  = zeroes(long,$gcids->max+1); $lcids->index($gcids) .= $gcids->xvals;  ##-- [$gci] -> $lci
  our $NCg    = $gcids->max+1;

  ##--------------------
  ## $dc_dist (from Mapper::LSI::loadCrossCheckEval()
  our $dc_dist = zeroes($ND,$NC)+2; ##-- initialize to max
  my ($lab,@dcats2,@dci,@dcdist,@gotcat,$di); #$d12
  while (($lab,$d12)=each(%$lab2docs)) {
    if (!defined($di = $d_sym2id->{$lab})) {
      warn("$0: no internal ID for doc label '$lab' -- skipping");
      next;
    }
    @gotcat = qw();
    @dcats2 = grep {!$gotcat[$_->{id}] && ($gotcat[$_->{id}]=1)} @{$d12->[1]{cats}}; ##-- handle dup cats (e.g. nullCat)
    @dci    = grep {defined($lc_sym2id->{$dcats2[$_]{name}})} (0..$#dcats2);
    @dcdist = map {$dcats2[$_]{dist_raw}} @dci;
    $dc_dist->slice("($di),")->index(pdl(long,[@$lc_sym2id{map {$dcats2[$_]{name}} @dci}])) .= pdl(\@dcdist);
  }

  ##--------------------
  ##-- get positive & negative evidence
  our $dc_which1 = sequence($ND)->cat($d2c)->xchg(0,1);  ##-- [$di]     -> [$di,$ci] : $di \in $ci
  my $dc_mask1  = zeroes(byte,$dc_dist->dims);          ##-- [$di,$ci] -> 1($di \in     $ci)
  $dc_mask1->indexND($dc_which1) .= 1;
  my $dc_mask   = $dc_mask1; ##-- alias
  our $dc_which0 = whichND(!$dc_mask1);                   ##-- [$di,$ci] -> 1($di \not\in $ci)
  our $nc  = $dc_mask1->long->sumover->double;            ##-- [$ci] -> |{ $di : $di \in     $ci }|
  our $nnc = $nc->sum - $nc;                              ##-- [$ci] -> |{ $di : $di \not\in $ci }|

  ##--------------------
  ## doc-cat distance matrix: by boolean membership
  ##   $dc1dist: CCS: [$di,$ci] -> dist($ci,$di) : $di     \in $ci
  ##   $dc0dist: CCS: [$di,$ci] -> dist($ci,$di) : $di \not\in $ci
  our $dc1dist = PDL::CCS::Nd->newFromWhich($dc_which1,$dc_dist->indexND($dc_which1));
  our $dc0dist = PDL::CCS::Nd->newFromWhich($dc_which0,$dc_dist->indexND($dc_which0));

  ##--------------------
  ## fit parameters: global
  ##   $dcdist_mu = pdl(1): global avg    dist($ci,$di)
  ##   $dcdist_sd = pdl(1): global stddev dist($ci,$di)
  our $dcdist_mu = $dc_dist->flat->average;
  our $dcdist_sd = (($dc_dist - $dcdist_mu)**2)->flat->average->sqrt;

  ##--------------------
  ## fit parameters: by category
  ##   $cdist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \in $ci
  ##   $cdist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \in $ci
  ##   $cdist_isgood: [$ci] -> isfinite($cddist_sd)
  our $cdist_mu = $dc_dist->average;
  our $cdist_sd = (($dc_dist - $cdist_mu->slice("*1,"))**2)->average->sqrt;
  our $cdist_isgood = ($cdist_sd->isfinite)&($cdist_sd>0);
  our $cdist_sd_raw = $cdist_sd->pdl;
  $cdist_sd = fixvals($cdist_sd, $cdist_isgood, $cdist_sd->where($cdist_isgood)->minimum/2);

  ##--------------------
  ## fit parameters: by boolean membership
  ##   $c1dist_mu0: constant: [] ->    avg d($cat,$doc) : $doc     \in $cat
  ##   $c1dist_sd0: constant: [] -> stddev d($cat,$doc) : $doc     \in $cat
  ##   $c0dist_mu0: constant: [] ->    avg d($cat,$doc) : $doc \not\in $cat
  ##   $c0dist_sd0: constant: [] -> stddev d($cat,$doc) : $doc \not\in $cat
  our $c1dist_mu0 = $dc1dist->_nzvals->average;
  our $c1dist_sd0 = (($dc1dist->_nzvals-$c1dist_mu0)**2)->average->sqrt;
  our $c0dist_mu0 = $dc0dist->_nzvals->average;
  our $c0dist_sd0 = (($dc0dist->_nzvals-$c0dist_mu0)**2)->average->sqrt;

  our $nc_min = 2;#3; #10; #50; ##-- minimum #/docs to use fit

  ##--------------------
  ## fit parameters: by category & boolean membership: positive
  ##   $c1dist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \in $ci
  ##   $c1dist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \in $ci
  ##   $c1dist_sd_nan0: like $c1dist_sd, but NaN->0
  ##   $c1dist_sd_bad0: like $c1dist_sd, but (!$c1dist_sd_isgood)->0
  our $c1dist_mu = $dc1dist->average_nz->decode;
  our $c1dist_sd = (($dc1dist - $c1dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  our $c1dist_mu_raw = $c1dist_mu->pdl;
  our $c1dist_sd_raw = $c1dist_sd->pdl;
  our $c1dist_isgood = (($c1dist_sd_raw->isfinite) & ($nc >= $nc_min));
  our $c1dist_sd_nan0 = fixvals($c1dist_sd, $c1dist_sd->isfinite, 0);
  our $c1dist_sd_bad0 = fixvals($c1dist_sd, $c1dist_isgood, 0);
  $c1dist_mu = fixvals($c1dist_mu, $c1dist_isgood, $c1dist_mu->where($c1dist_isgood)->minimum);
  $c1dist_sd = fixvals($c1dist_sd, $c1dist_isgood, $c1dist_sd->where($c1dist_isgood)->minimum/2);

  ##--------------------
  ## fit parameters: by category & boolean membership: negative
  ##   $c0dist_mu: dense: [$ci] ->    avg d($ci,$doc) : $doc \not\in $ci
  ##   $c0dist_sd: dense: [$ci] -> stddev d($ci,$doc) : $doc \not\in $ci
  ##   $c0dist_sd_nan0: like $c0dist_sd, but NaN->0
  ##   $c0dist_sd_bad0: like $c0dist_sd, but (!$c0dist_sd_isgood)->0
  our $c0dist_mu = $dc0dist->average_nz->decode;
  our $c0dist_sd = (($dc0dist - $c0dist_mu->slice("*1,"))**2)->average_nz->decode->sqrt;
  our $c0dist_mu_raw = $c0dist_mu->pdl;
  our $c0dist_sd_raw = $c0dist_sd->pdl;
  #our $c0dist_isgood = (($c0dist_sd_raw->isfinite) & ($nc >= $nc_min));
  our $c0dist_isgood = $c0dist_sd_raw->isfinite;
  our $c0dist_sd_nan0 = fixvals($c1dist_sd, $c1dist_sd->isfinite, 0);
  our $c0dist_sd_bad0 = fixvals($c1dist_sd, $c1dist_isgood, 0);
  $c0dist_mu = fixvals($c0dist_mu, $c0dist_isgood, $c0dist_mu->where($c0dist_isgood)->minimum);
  $c0dist_sd = fixvals($c0dist_sd, $c0dist_isgood, $c0dist_sd->where($c0dist_isgood)->maximum);

  return;
}

## $vals_fixed = fixvals($vals,$isgood_mask,$fixval)
sub fixvals {
  my ($vals,$isgood,$fixval) = @_;
  $isgood = (($vals->isfinite)&($vals>0)) if (!defined($isgood) || $isgood->isnull);
  $fixval = $vals->where($isgood)->average if (!defined($fixval));
  my $fixed = $vals->pdl;
  $fixed->where(!$isgood) .= $fixval;
  return $fixed;
}

## $acc = testacc($expr)
## $acc = testacc($expr,'max')
##  + test accuracy of class-predictor $expr, which should return a pdl (dense or CCS): [$di,$ci] -> $dist
##  + requires defined $d2c pdl (e.g. call eval2dcdist($eval) first)
sub testacc {
  my ($expr,$minmax) = @_;
  $minmax  = 'min' if (!defined($minmax));
  my $cdm  = (eval $expr)->xchg(0,1);
  my $cats = $minmax eq 'max' ? $cdm->maximum_ind : $cdm->minimum_ind;
  our ($d2c);
  my $ND   = $cdm->dim(1);
  my $acc  = ($cats == $d2c)->nnz->double / $ND;
  print "$acc\tacc:${minmax}\t$expr\n";
  return $acc;
}

##======================================================================
sub test_eval_cutoff {
  my ($efile) = @_;
  if (!defined($efile)) {
    #$efile = "data/train_data_2009_12_30_v2a.r256-null0-du.eval.xml";
    #$efile = "data/test_data_2009_12_30_v2a.r256-null0-du.eval.xml";
    ##--
    $efile = "data/test_data_2009_12_30_v2a.r256-null1-du.eval.xml";
  }

  ##--- load & populate eval
  my $eval = load_eval($efile);
  eval2dcdist($eval);

  ##--------------------
  ## plots: basic fit
  if ($plot2d) {
    _usepgplot($dev2d);
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category (Positives:black, Negatives:red)'},
	     [$gcids-.1, $c1dist_mu,$c1dist_sd_nan0, {}],
	     [$gcids+.1, $c0dist_mu,$c0dist_sd_nan0, {color=>'red'}]);
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked (Positives:black, Negatives:red)'},
	     [$gcids-.1, $c1dist_mu,$c1dist_sd, {}],
	     [$gcids+.1, $c0dist_mu,$c0dist_sd, {color=>'red'}]);
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    release();
  }

  ##--------------------
  ##-- cutoffs
  my $cutval   = 10;             ##-- pseudo-distance value to add on cutoff
  my $dcd_raw  = $dc_dist;       ##-- raw data (alias)
  ##
  ##-- cutoffs: global, by negative evidence
  my $dcd_gn = cutoff($dc_dist, $c0dist_mu0-_gausswidth(.80, $c0dist_mu0,$c0dist_sd0));
  testacc('$dc_dist'); ##-- .747, .767
  testacc('cutoff($dc_dist, $c0dist_mu0-_gausswidth(.80, $c0dist_mu0,$c0dist_sd0))'); ##-- .760     , .7758
  testacc('cutoff($dc_dist, $c0dist_mu0-_gausswidth(.85, $c0dist_mu0,$c0dist_sd0))'); ##-- .76216   , .7758
  testacc('cutoff($dc_dist, $c0dist_mu0-_gausswidth(.90, $c0dist_mu0,$c0dist_sd0))'); ##-- .76380   , .7758
  testacc('cutoff($dc_dist, $c0dist_mu0-_gausswidth(.91, $c0dist_mu0,$c0dist_sd0))'); ##-- .76435 * , .7763 *
  testacc('cutoff($dc_dist, $c0dist_mu0-_gausswidth(.92, $c0dist_mu0,$c0dist_sd0))'); ##-- .76435 * , .7747
  testacc('cutoff($dc_dist, $c0dist_mu0-_gausswidth(.93, $c0dist_mu0,$c0dist_sd0))'); ##-- .73632   , .7763 *
  testacc('cutoff($dc_dist, $c0dist_mu0-_gausswidth(.95, $c0dist_mu0,$c0dist_sd0))'); ##-- .76271   , .7747
  testacc('cutoff($dc_dist, $c0dist_mu0-_gausswidth(.97, $c0dist_mu0,$c0dist_sd0))'); ##-- .76216   , .7714
  testacc('cutoff($dc_dist, $c0dist_mu0-_gausswidth(.99, $c0dist_mu0,$c0dist_sd0))'); ##-- .757     , .7670

  ##-- cutoffs: cat-local, by negative evidence
  print STDERR (("#" x 64), "\n");
  my $dcd_cn = cutoff($dc_dist, ($c0dist_mu-_gausswidth(.80, $c0dist_mu,$c0dist_sd))->slice("*1,"));
  testacc('$dc_dist');                                     ##-- .747    , .7676
  testacc('cutoff($dc_dist, cutoff0(.80)->slice("*1,"))'); ##-- .7545   , .7709
  testacc('cutoff($dc_dist, cutoff0(.90)->slice("*1,"))'); ##-- .7561   , .7709
  testacc('cutoff($dc_dist, cutoff0(.95)->slice("*1,"))'); ##-- .7594   , .7714 *
  testacc('cutoff($dc_dist, cutoff0(.99)->slice("*1,"))'); ##-- .7648 * , .7698
  testacc('cutoff($dc_dist, cutoff0(.999)->slice("*1,"))');##-- .7577   , .7621

  ##-- cutoffs: cat-local, by positive evidence
  print STDERR (("#" x 64), "\n");
  my $dcd_cp = cutoff($dc_dist, ($c1dist_mu+_gausswidth(.80, $c1dist_mu,$c1dist_sd))->slice("*1,"));
  testacc('$dc_dist');                                     ##-- .747    , .7676
  testacc('cutoff($dc_dist, cutoff1(.80)->slice("*1,"))'); ##-- .7545   , .7731
  testacc('cutoff($dc_dist, cutoff1(.90)->slice("*1,"))'); ##-- .7495   , .7709
  testacc('cutoff($dc_dist, cutoff1(.70)->slice("*1,"))'); ##-- .7648   , .7791
  testacc('cutoff($dc_dist, cutoff1(.60)->slice("*1,"))'); ##-- .7709   , .7802
  testacc('cutoff($dc_dist, cutoff1(.61)->slice("*1,"))'); ##-- .7698   , .7813 *
  testacc('cutoff($dc_dist, cutoff1(.62)->slice("*1,"))'); ##-- .7714 * , .7813 *
  testacc('cutoff($dc_dist, cutoff1(.63)->slice("*1,"))'); ##-- .7698   , .7813 *
  testacc('cutoff($dc_dist, cutoff1(.50)->slice("*1,"))'); ##-- .7698   , .7741
  testacc('cutoff($dc_dist, cutoff1(.49)->slice("*1,"))'); ##-- .7698   , .7736
  testacc('cutoff($dc_dist, cutoff1(.48)->slice("*1,"))'); ##-- .7709   , .7736
  testacc('cutoff($dc_dist, cutoff1(.47)->slice("*1,"))'); ##-- .7714 * , .7752
  testacc('cutoff($dc_dist, cutoff1(.45)->slice("*1,"))'); ##-- .7709   , .7731
  testacc('cutoff($dc_dist, cutoff1(.42)->slice("*1,"))'); ##-- .7659   , .7714
  testacc('cutoff($dc_dist, cutoff1(.40)->slice("*1,"))'); ##-- .7648   , .7681
  testacc('cutoff($dc_dist, cutoff1(.30)->slice("*1,"))'); ##-- .7627   , .7659
  testacc('cutoff($dc_dist, cutoff1(.25)->slice("*1,"))'); ##-- .7605   , .7643

  ##-- cutoffs: cat-local, by weighted positive and negative evidence
  print STDERR (("#" x 64), "\n");
  my $dcd_pn = cutoff2($dc_dist, .5,.5, .5);
  testacc('$dc_dist');                         ##-- .747    , .7676
  testacc('cutoff2($dc_dist, .50,.50, .50)');  ##-- .767    , .7780
  testacc('cutoff2($dc_dist, .50,.50, .60)');  ##-- .7687   , .7796
  testacc('cutoff2($dc_dist, .50,.50, .65)');  ##-- .7741   , .7845 *
  testacc('cutoff2($dc_dist, .50,.50, .66)');  ##-- .7752 * , .7840
  testacc('cutoff2($dc_dist, .50,.50, .67)');  ##-- .7747   , .7840
  testacc('cutoff2($dc_dist, .50,.50, .70)');  ##-- .7752 * , .7823
  testacc('cutoff2($dc_dist, .50,.50, .71)');  ##-- .7752 * , .7829
  testacc('cutoff2($dc_dist, .50,.50, .72)');  ##-- .7752 * , .7834
  testacc('cutoff2($dc_dist, .50,.50, .73)');  ##-- .7747   , .7818
  testacc('cutoff2($dc_dist, .50,.50, .75)');  ##-- .7747   , .7807
  testacc('cutoff2($dc_dist, .50,.50, .76)');  ##-- .7741   , .7796
  testacc('cutoff2($dc_dist, .50,.50, .77)');  ##-- .7736   , .7796
  testacc('cutoff2($dc_dist, .50,.50, .80)');  ##-- .7731   , .7791
  testacc('cutoff2($dc_dist, .50,.50, .85)');  ##-- .7692   , .7747
  testacc('cutoff2($dc_dist, .50,.50, .95)');  ##-- .7681   , .7747
  ##
  #testacc('cutoff2($dc_dist, .50,.50, .71)');  ##-- .7752  *, -
  #testacc('cutoff2($dc_dist, .50,.50, .65)');  ##-- .7741   , .7845 **
  testacc('cutoff2($dc_dist, .66,.66, .71)');  ##-- .7643   , .7780
  testacc('cutoff2($dc_dist, .99,.47, .50)');  ##-- .7692   , .7725
  ##
  #testacc('cutoff2($dc_dist, .50,.50, .71)');  ##-- .7752  *, -
  #testacc('cutoff2($dc_dist, .50,.50, .65)');  ##-- .7741   , .7845 **
  testacc('cutoff2($dc_dist, .90,.50, .40)');  ##-- .7709   , .7796
  testacc('cutoff2($dc_dist, .90,.50, .50)');  ##-- .7720   , .7807
  testacc('cutoff2($dc_dist, .90,.50, .52)');  ##-- .7720   , .7813 +
  testacc('cutoff2($dc_dist, .90,.50, .55)');  ##-- .7731 + , .7813 +
  testacc('cutoff2($dc_dist, .90,.50, .60)');  ##-- .7725   , .7791
  testacc('cutoff2($dc_dist, .90,.50, .70)');  ##-- .7720   , .7769
  testacc('cutoff2($dc_dist, .90,.50, .75)');  ##-- .7676   , .7758
  ##
  #testacc('cutoff2($dc_dist, .50,.50, .71)');  ##-- .7752  *, -
  #testacc('cutoff2($dc_dist, .50,.50, .65)');  ##-- .7741   , .7845 **
  testacc('cutoff2($dc_dist, .80,.60, .50)');  ##-- .7670   , .7785
  testacc('cutoff2($dc_dist, .80,.60, .75)');  ##-- .7703   , .7802
  testacc('cutoff2($dc_dist, .80,.60, .95)');  ##-- .7714   , .7807
  testacc('cutoff2($dc_dist, .80,.60, .999)'); ##-- .7736 + , .7802
  ##
  #testacc('cutoff2($dc_dist, .50,.50, .71)');  ##-- .7752  *, -
  #testacc('cutoff2($dc_dist, .50,.50, .65)');  ##-- .7741   , .7845 *
  testacc('cutoff2($dc_dist, .90,.60, .30)');  ##-- .7665   , .7736
  testacc('cutoff2($dc_dist, .90,.60, .40)');  ##-- .7687   , .7763
  testacc('cutoff2($dc_dist, .90,.60, .50)');  ##-- .7670   , .7769
  testacc('cutoff2($dc_dist, .90,.60, .67)');  ##-- .7709   , .7818
  testacc('cutoff2($dc_dist, .90,.60, .70)');  ##-- .7714   , .7829 +
  testacc('cutoff2($dc_dist, .90,.60, .75)');  ##-- .7731 + , .7823
  testacc('cutoff2($dc_dist, .90,.60, .77)');  ##-- .7714   , .7813
  testacc('cutoff2($dc_dist, .90,.60, .80)');  ##-- .7725   , .7807

  print STDERR "$0: test_eval_cutoff() done: what now?\n";
  exit 0;
}
#test_eval_cutoff(@ARGV);


## $dc_dist_pseudo = cutoff($dc_dist,$cutoff)
##  + requires globals: $lcenum
sub cutoff {
  my ($dc_dist,$cutoff) = @_;
  my $cutcname = '1_Sonstiges';                ##-- don't cutoff this cat
  my $cutcid   = $lcenum->{sym2id}{$cutcname};
  my $cutval   = 10;                           ##-- pseudo-distance value to add on cutoff
  ##
  my $dc_cut   = $dc_dist->pdl;
  my $cut_mask = ($dc_cut>$cutoff);
  $cut_mask->slice(",$cutcid") .= 0;           ##-- don't cut here
  if (0 && defined($lcenum->{sym2id}{'(null)'})) {
    $cut_mask->slice(",".$lcenum->{sym2id}{'(null)'}) .= 0; ##-- ... don't cut here either ?
  }
  $dc_cut->where($cut_mask) += $cutval;
  return $dc_cut;
}

## $cutoff_flat = cutoff1($conf_pos)
##   + uses globals: $c1dist_mu, $c1dist_sd
sub cutoff1 { return $c1dist_mu+_gausswidth($_[0],$c1dist_mu,$c1dist_sd); }

## $cutoff_flat = cutoff0($conf_neg)
##   + uses globals: $c1dist_mu, $c1dist_sd
sub cutoff0 { return $c0dist_mu-_gausswidth($_[0],$c0dist_mu,$c0dist_sd); }

## $weighted_cutoff_flat = wcutoff($conf_neg,$conf_pos,$wt_pos)
##  + uses globals: $c0dist_mu,$c0dist_sd, $c1dist_mu,$c1dist_sd
sub wcutoff {
  my ($conf0,$conf1,$wt1) = @_;
  $wt1 = .5 if (!defined($wt1));
  return ((1-$wt1)*cutoff0($conf0)) + ($wt1*cutoff1($conf1));
}

## $dc_dist_pseudo = cutoff2($dc_dist,$conf0,$conf1,$wt1)
sub cutoff2 {
  my ($dc_dist,$conf0,$conf1,$wt1) = @_;
  my $cut = wcutoff($conf0,$conf1,$wt1)->slice("*1,");
  return cutoff($dc_dist,$cut);
}

## $dc_dist_pseudo = cutoff2_old($dc_dist,$conf0,$conf1,$wt1)
sub cutoff2_old {
  local $c0dist_mu = $c0dist_mu_old;
  local $c0dist_sd = $c0dist_sd_old;
  local $c1dist_mu = $c1dist_mu_old;
  local $c1dist_sd = $c1dist_sd_old;
  local $d2c = $d2c_old;
  local $lcenum = $lcenum_old;
  return cutoff2($dc_dist_old, @_);
}


##======================================================================
sub test_map_cutoff {
  my ($mfile,$efile) = @_;
  if (!defined($mfile)) {
    $mfile = 'tmp.map.bin';
  }
  if (!defined($efile)) {
    $efile = 'tmp.test.eval.xml';
  }
  $efile = "$efile.bin" if (-f "$efile.bin"); ##-- prefer binary eval files

  ##--- load map, eval
  my $map  = DocClassify::Mapper->loadFile($mfile) or die("$0: Mapper->load($mfile) failed: $!");
  my $eval = DocClassify::Eval->loadFile($efile) or die("$0: Eval->load($efile) failed: $!");
  $eval->saveFile("$efile.bin") if ($efile !~ /\.bin$/); ##-- cache binary

  ##--- DEBUG: populate dc_dist globals
  if (1) {
    eval2dcdist($eval);
    our $dc_dist_old = $dc_dist;
    ##
    our $c1dist_mu_old = $c1dist_mu;
    our $c1dist_sd_old = $c1dist_sd;
    our $c1dist_mu0_old = $c1dist_mu0;
    our $c1dist_sd0_old = $c1dist_sd0;
    ##
    our $c0dist_mu_old = $c0dist_mu;
    our $c0dist_sd_old = $c0dist_sd;
    our $c0dist_mu0_old = $c0dist_mu0;
    our $c0dist_sd0_old = $c0dist_sd0;
    ##
    our $denum_old = $denum;
    our $lcenum_old = $lcenum;
    our $gcenum_old = $gcenum;
    ##
    our $d2c_old = $d2c;
    our $gcids_old = pdl(long,[map {$_||0} @{$map->{gcenum}{sym2id}}{map {$_||''} @{$lcenum_old->{id2sym}}}]);
    our $lcids_old = pdl(long,[map {$_||0} @{$lcenum_old->{sym2id}}{map {$_||''} @{$map->{gcenum}{id2sym}}}]);
  }

  ##--------------------
  ##-- train fit
  $map->loadCrossCheckEval($eval);
  $map->compileFit();
  #$map->compileCutoffs();
  $map->compileCutoffs(cut0p=>.5,cut1p=>.5,cut1w=>.65);

  ##--------------------
  ## test cutoffs: vars
  our $lcenum = $map->{lcenum}; ##-- required for cutoff() util, above
  our ($dc_dist,$d2c) = @$map{qw(dc_dist dc_d2c)};
  our ($c0dist_mu,$c0dist_sd,$c1dist_mu,$c1dist_sd) = @$map{qw(c0dist_mu c0dist_sd c1dist_mu c1dist_sd)};
  our $cutoff = $map->{cutoff};
  our $map_cutoff = $cutoff;
  our $c0dist_mu0 = $c0dist_mu->average;
  our $c1dist_mu0 = $c1dist_mu->average;
  our $gcids = pdl(long,[@{$map->{gcenum}{sym2id}}{@{$map->{lcenum}{id2sym}}}]);

  ##-- DEBUG
  if (defined($lcids_old)) {
    our $lcids_new2old = pdl(long,[map {$_||0} @{$lcenum_old->{sym2id}}{@{$lcenum->{id2sym}}}]); ##-- [$ci_new] -> $ci_old
    our $lcids_old2new = pdl(long,[map {$_||0} @{$lcenum->{sym2id}}{@{$lcenum_old->{id2sym}}}]); ##-- [$ci_old] -> $ci_new
  }

  ##--------------------
  ## plots: fit + cutoffs
  if (0 && $plot2d) {
    _usepgplot($dev2d);
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked (Positives:black, Negatives:red, Cutoffs:blue)'},
	     [$gcids-.1, $c1dist_mu,$c1dist_sd, {}],
	     [$gcids+.1, $c0dist_mu,$c0dist_sd, {color=>'red'}]);
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    hold(); points($gcids, $gcids->zeroes+$map->{cutoff}, {plotline=>1,symbol=>'plus',color=>'blue',charsize=>2});
    release();
  }

  ##--------------------
  ## plots: old vs. new fit
  if (0 && $plot2d) {
    _usepgplot($dev2d);
    %eplot = (symbol=>'circle',xtitle=>'cat',ytitle=>'mu +/- sigma : dist(cat,doc)');
    ##
    ploterrs({%eplot,title=>'Normal Fit by Category: Hacked (Positives:black, Negatives:red, Cutoffs:blue)'},
	     [$gcids_old-.2, $c1dist_mu_old,$c1dist_sd_old, {linestyle=>'dotted'}],
	     [$gcids_old+.2, $c0dist_mu_old,$c0dist_sd_old, {linestyle=>'dotted',color=>'red'}],
	     [$gcids-.1, $c1dist_mu,$c1dist_sd, {}],
	     [$gcids+.1, $c0dist_mu,$c0dist_sd, {color=>'red'}],
	    );
    hold(); line($gcids_old, $gcids_old->zeroes+$c1dist_mu0_old, {linestyle=>'dotted'});
    hold(); line($gcids_old, $gcids_old->zeroes+$c0dist_mu0_old, {linestyle=>'dotted',color=>'red'});
    ##
    hold(); line($gcids, $gcids->zeroes+$c1dist_mu0, {linestyle=>'dashed'});
    hold(); line($gcids, $gcids->zeroes+$c0dist_mu0, {linestyle=>'dashed',color=>'red'});
    ##
    hold(); points($gcids, $gcids->zeroes+$map->{cutoff}, {plotline=>1,symbol=>'plus',color=>'blue',charsize=>2});
    release();
  }


  ##-- test accuracy
  my %xlate = qw();
  $xlate{$map->{lcenum}{sym2id}{'(null)'}} = $map->{lcenum}{sym2id}{$map->{nullCat}}
    if (defined($map->{nullCat}));
  testaccx('$dc_dist', %xlate);                         ##-- .7676
  testaccx('cutoff2($dc_dist, .50,.50, .50)', %xlate);  ##-- .7774
  testaccx('cutoff2($dc_dist, .50,.50, .65)', %xlate);  ##-- .7845 **
  testaccx('cutoff($dc_dist,$map_cutoff->slice("*1,"))', %xlate);  ##-- .7845

  print STDERR "$0: test_eval_cutoff() done: what now?\n";
  exit 0;
}
#test_map_cutoff(@ARGV);

## $acc = testaccx($expr)
## $acc = testaccx($expr, $fromCatId,$toCatId, ...)
##  + like testacc() but maps minimum_ind==0 to 1 (nullCat hack)
##  + requires defined $d2c pdl (e.g. call eval2dcdist($eval) first)
sub testaccx {
  my $expr = shift;
  my $cdm  = (eval $expr)->xchg(0,1);
  my $cats = $cdm->minimum_ind;
  while (@_) {
    my ($from,$to)=(shift,shift);
    $cats->where($cats==$from) .= $to;
  }
  our ($d2c);
  my $ND   = $cdm->dim(1);
  my $acc  = ($cats == $d2c)->nnz->double / $ND;
  print "$acc\taccx:min\t$expr\n";
  return $acc;
}


##======================================================================
sub test_store_regex {
  my $re = qr/(?:foo|bar)/;
  my $re_f = Storable::freeze($re);
  my $re_t = Storable::thaw($re_f);
  print STDERR "$0: test_store_regex(): ", ("$re" eq "$re_t" ? "ok" : "NOT ok"), "\n";
  print "$0: test_store_regex() done: what now?\n";
}
#test_store_regex;

##======================================================================
sub test_cluster_mapper {
  my $mfile = shift;
  if (!defined($mfile)) {
    #$mfile = 'vzdata-safe.u1.train0.bin'; ##-- uhura, r=128
    #$mfile = 'vzdata-safe.u1.r-256.tw-Hmax.xn-0.tpd-100.lsimap.bin'; ##-- uhura
    #$mfile = 'vzdata-safe.u1.r-512.tw-Hmax.xn-0.tpd-1000.mdf-2.lsimap.bin';
    $mfile = 'vzdata-all.r-512.tw-Hmax.xn-0.tpd-100.mdf-2.lc-1.lsimap.nbin'; ##-- uhura, from lal0 (error: "out of memory!")
  }

  ##----
  print STDERR "$0: load($mfile)\n";
  my $map = DocClassify::Mapper::LSI->loadFile("$mfile")
    or die("$0: Mapper->loadFile($mfile) failed: $!");

  ##----
  my $xcm = $map->{xcm};
  my $tck = 16;
  my ($distf,$cm,%cm);
  my %ldopts = ( avg=>{ dexp=>1, dmult=>12 }, max=>{ dexp=>1, dmult=>12 }, min=>{ dexp=>1, dmult=>12 }, );
  foreach my $link (qw(avg max min)) {
    print STDERR "$0: cluster(link=$link)\n";
    $distf = MUDL::Cluster::Distance->new(class=>$map->{dist}, link=>$link);
    $cm = $cm{link} = MUDL::Cluster::Tree->new(distf=>$distf,data=>$xcm,enum=>$map->{lcenum});
    $cm->cluster();
    $cm->cut($tck);
    ##
    my $ld0 = $cm->{linkdist}->slice("0:-2");
    ##
    my $ld = ($ldopts{$link}{dexp} ? $ld0->exp : $ld0);
    $cm->view(dists=>$ld, dmult=>($ldopts{$link}{dmult}||1));
    $cm->saveFile("$mfile.ctree-$link.bin");
  }

  print STDERR "$0: test_cluster_mapper() done: what now?\n";
}
#test_cluster_mapper(@ARGV);

sub test_cluster_tree {
  my $mfile = shift;
  if (!defined($mfile)) {
    #$mfile = 'vzdata-safe.u1.train0.bin'; ##-- uhura, r=128
    #$mfile = 'vzdata-safe.u1.r-256.tw-Hmax.xn-0.tpd-100.lsimap.bin'; ##-- uhura
    #$mfile = 'vzdata-safe.u1.r-512.tw-Hmax.xn-0.tpd-1000.mdf-2.lsimap.bin';
    $mfile = 'vzdata-all.r-512.tw-Hmax.xn-0.tpd-100.mdf-2.lc-1.lsimap.nbin'; ##-- uhura, from lal0 (error: "out of memory!")
  }

  ##----
  my $tck = 16;
  my ($cm,%cm);
  my %ldopts = ( avg=>{ dexp=>1, dmult=>12 }, max=>{ dexp=>1, dmult=>12 }, min=>{ dexp=>1, dmult=>12 }, );
  foreach my $link (qw(avg max min)) {
    $treefile = "$mfile.ctree-$link.bin";
    print STDERR "$0: tree($treefile)\n";
    $cm = MUDL::Cluster::Tree->loadFile($treefile) or die("$0: load($treefile) failed: $!");
    ##
    my $ld0 = $cm->{linkdist}->slice("0:-2");
    my $ld = ($ldopts{$link}{dexp} ? $ld0->exp : $ld0);
    my %vopts = (dists=>$ld, dmult=>($ldopts{$link}{dmult}||1));
    $cm->view(%vopts);
    $cm->toDendogram(%vopts)->savePs("$treefile.ps");
  }
  print STDERR "$0: test_cluster_tree() done: what now?\n";
  exit 0;
}
#test_cluster_tree(@ARGV);

##======================================================================
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
#test_catambig(@ARGV);

##======================================================================
sub test_cab_profile {
  my $mapfile = shift || 'cab-ner.map.bin';

  { select STDERR; $|=1; select STDOUT; }
  print STDERR "$0: loading $mapfile ... ";
  my $map = DocClassify::Mapper->loadFile($mapfile)
    or die("$0: failed to load $mapfile: $!");
  print STDERR "loaded.\n";

  ##-- use tdm0 or tdm?
  my $use_raw  =  (grep {$_ eq '-raw'} @_);
  my $use_json = (grep {$_ eq '-json'} @_);
  my $ntpd     = (map {$_ =~ /^-n=(\d+)/ ? $1 : qw()} @_)[0] || 500; ##-- number of terms by doc
  require JSON if ($use_json);

  ##-- get extended term frequency
  my $tw  = $map->{tw};
  my $tw0 = $map->{tw0} // $map->{tw};
  my $tdm = $use_raw ? $map->get_tdm0() : $map->{tdm};

  ##-- dump "best" terms by doc (see dc-mapper-info.perl)
  my $denum   = $map->{denum};
  my $tenum   = $map->{tenum};
  my $prof    = {};
  binmode(STDOUT,':utf8');
  foreach my $dname (sort keys %{$denum->{sym2id}}) {
    (my $dbase = File::Basename::basename($dname)) =~ s/\..*$//;
    my $di  = $denum->{sym2id}{$dname};
    my $tf  = $tdm->dice_axis(1,$di)->decode->flat;
    my $tfi = $tf->qsorti->slice("-1:0");

    print "$dbase\n" if (!$use_json);
    foreach (0..($ntpd-1)) {
      my $ti  = $tfi->at($_);
      my $term = $tenum->{id2sym}[$ti];
      my $val = $tf->at($ti);
      next if ($val <= 0);
      my $f   = $use_raw ? $val : exp($val/$tw->at($ti))-$map->{smoothf};
      my $w   = $tw0->at($ti);
      if ($use_json) {
	push(@{$prof->{$dbase}}, {text=>$term, weight=>$val});
      } else {
	printf("\t%s  <%.1f ~ f=%d, w=%.2f>\n", $term, $val,$f,$w);
      }
    }
    print "\n" if (!$use_json);
  }
  print "doctags = ", JSON::to_json($prof, {utf8=>0,pretty=>1,canonical=>1}), ";\n" if ($use_json);

  exit 0;
  print STDERR "$0: test_cab_profile() done: what now?\n";
}
#test_cab_profile(@ARGV);

##======================================================================

##--------------------------------------------------------------
## ([$dist,$name],...) = kbestObjects($dist_pdl,$enum,$k, [\&munge_name_sub])
sub kbestObjects {
  my ($qx_dist,$enum,$k,$munge_name) = @_;
  $k //= 10;
  $munge_name //= sub { return $_[0] };

  ##-- report k-nearest output docs
  my @names = qw();
  $qx_dist = $qx_dist->flat->lclip(0);
  my $qxi  = $qx_dist->qsorti;
  my ($xi,$xname);
  foreach $xi ($qxi->slice("0:".($k-1))->list) {
    $xname = $munge_name->( $enum->{id2sym}[$xi] );
    push(@names, [$qx_dist->at($xi),$xname]);
  }
  return @names;
}

##--------------------------------------------------------------
## @docname_dist_pairs = kbestDocs($mapper,$dist_pdl,$k=10)
sub kbestDocs {
  my ($map,$qd_dist,$k) = @_;
  my ($docname);
  return kbestObjects($qd_dist, $map->{denum}, $k,
		      sub {
			($docname = basename(shift)) =~ s/\.\D.*$//;
			return $docname;
		      });
}

##--------------------------------------------------------------
## @strings = bestDocStrings($mapper,$dist_pdl,$k=10)
sub bestDocStrings {
  my @kbest = kbestDocs(@_);
  return map {"$_ ".(ref($kbest[$_]) ? join("\t",@{$kbest[$_]}) : $kbest[$_])."\n"} (0..$#kbest)
}

##--------------------------------------------------------------
## @catname_dist_pairs = kbestCats($mapper,$dist_pdl,$k=10)
sub kbestCats {
  my ($map,$qc_dist,$k) = @_;
  my ($name);
  return kbestObjects($qc_dist, $map->{lcenum}, $k,
		      sub {
			($name = basename(shift)) =~ s/\.\D.*$//;
			return $name;
		      });
}

##--------------------------------------------------------------
## @strings = bestCatStrings($mapper,$dist_pdl,$k=10)
sub bestCatStrings {
  my @kbest = kbestCats(@_);
  return map {"$_ ".(ref($kbest[$_]) ? join("\t",@{$kbest[$_]}) : $kbest[$_])."\n"} (0..$#kbest)
}

##--------------------------------------------------------------
## @term_dist_pairs = kbestTerms($mapper,$dist_pdl,$k=10)
sub kbestTerms {
  my ($map,$qt_dist,$k) = @_;
  my ($name);
  return kbestObjects($qt_dist, $map->{tenum}, $k);
}

##--------------------------------------------------------------
## @strings = bestTermStrings($mapper,$dist_pdl,$k=10)
sub bestTermStrings {
  my @kbest = kbestTerms(@_);
  return map {"  + $_ ".(ref($kbest[$_]) ? join("\t",@{$kbest[$_]}) : $kbest[$_])."\n"} (0..$#kbest)
}

##--------------------------------------------------------------
## $q_sig = parse_query($map,@qstrs)
sub parse_query {
  ##-- parse query into signature
  my $map   = shift;
  my $qstr  = join(' ',map {utf8::is_utf8($_) ? $_ : Encode::decode_utf8($_)} @_);
  my $qf    = {};
  my $qn    = 0;
  my @docs    = qw();
  my @classes = qw();
  my ($t,$f);
  foreach (split(/[\,\s\;]+/,$qstr)) {
    ($t,$f) = /^(.+):([0-9eE\+\-]+)$/ ? ($1,$2) : ($_,1);
    $f        ||= 1;

    if ($t =~ /^doc:(.*)/) {
      ##-- add a document
      my $darg = $1;
      my $did  = $map->{denum}{sym2id}{$darg};
      if (!defined($did)) {
	##-- approximate search for doc name regex
	my $darg_re = qr{$darg};
	my $dsym    = (grep {($_//'') =~ $darg_re} @{$map->{denum}{id2sym}})[0];
	if (!$dsym || !defined($did = $map->{denum}{sym2id}{$dsym})) {
	  warn("$0: no document found matching m/$darg/ - skipping");
	  next;
	}
      }
      push(@docs,$did);
    }
    elsif ($t =~ /^(?:class:|cls:)(.*)/) {
      ##-- add a document
      my $carg = $1;
      my $cid  = $map->{lcenum}{sym2id}{$carg};
      if (!defined($cid)) {
	##-- approximate search for class name regex
	my $carg_re = qr{$carg};
	my $csym    = (grep {($_//'') =~ $carg_re} @{$map->{lcenum}{id2sym}})[0];
	if (!$csym || !defined($cid = $map->{lcenum}{sym2id}{$csym})) {
	  warn("$0: no class found matching m/$carg/ - skipping");
	  next;
	}
      }
      push(@classes,$cid);
    }
    else {
      ##-- "normal" TERM:FREQ query
      $qf->{$t} += $f;
      $qn       += $f;
    }
  }
  my $q_sig = DocClassify::Signature->new(tf=>$qf,lf=>$qf,N=>0,Nl=>$qn, str=>$qstr,docs_=>\@docs,classes_=>\@classes);
  return $q_sig;
}

##--------------------------------------------------------------
## cosine comparison, tweaked from MUDL::Cluster::Distance::Cosine

## $dist = vcosine($data, $cdata, [$norm=0])
##  + args:
##       data    # pdl($d,$nR)  : $d=N_features, $nR=N_rows
##       cdata   # pdl($d,$nC)  : $d=N_features, $nC=N_centers
##    [o]dist    # pdl($nC,$nR) : output distances
##      $norm    # one of 'g(aussian)', 'a(dditive)'
##  + local implementation with no mask, weight, etc: ca 2x faster than MUDL::Cluster::Distance
use MUDL::PDL::Smooth;
use MUDL::PDL::Stats;
sub vcosine {
  my ($data,$cdata,$norm) = @_;

  ##-- dist(x,y) = 1 - 1/d * (\sum_{i=1}^d (x[i]-mean(x))/stddev(x) * (y[i]-mean(y))/stddev(y))
  ##             = 1 - 1/d * 1/stddev(x) * 1/stddev(y) * (\sum_{i=1}^d (x[i]-mean(x)) * (y[i]-mean(y)))
  ##             = 1 - (\sum_{i=1}^d (x[i]-mean(x)) * (y[i]-mean(y))) / (d * stddev(x) * stddev(y))
  ## + where:
  ##     mean(x)   := 0
  ##     stddev(x) := sqrt( E(X^2) )
  my $dr1    = $data;
  my $dr2    = $cdata;
  my $d      = $dr1->dim(0);
  my $sigma1 = $dr1->pow(2)->average; $sigma1->inplace->sqrt;
  my $sigma2 = $dr2->pow(2)->average; $sigma2->inplace->sqrt;

  my $dist = ($dr1*$dr2)->sumover;
  ($dist
   ->inplace->divide($sigma1,0)
   ->inplace->divide($sigma2,0)
   ->inplace->divide($d,0)
  );
  $dist = $dist->todense;
  $dist->minus(1,$dist,1);
  $dist->inplace->setnantobad->inplace->setbadtoval(2);
  $dist->inplace->clip(0,2);

  if ($norm && $norm =~ /^a/i) {
    $dist->inplace->divide(2,0);
  }
  elsif ($norm && $norm =~ /^g/i) {
    $dist = $dist->gausscdf($dist->average,$dist->stddev);
  }

  return $dist;
}

##--------------------------------------------------------------
## get similar documents
sub test_cab_query {
  my $mapby   = undef;
  my $mapfile = 'data/dta-ner.pages.map.bin';
  my $nbest = 10;
  my $get   = 'docs';
  my $min_term_freq = 0;
  my $min_term_ndocs = 0;
  my $niters = 0;
  my $shell = 0;
  my $dnorm = 'a';
  my ($help);
  my %optSpec = ('help|h'  => \$help,
		 'mapfile|map|m=s' => \$mapfile,
		 'map-by-category|by-category|bycat|bc' => sub { $mapby='cat'; },
		 'map-by-document|by-document|bydoc|bd' => sub { $mapby='doc'; },
		 'similar-terms|terms|t' => sub { $get='terms'; },
		 'similar-documents|documents|docs|d' => sub { $get='docs'; },
		 'nbest|n=i' => \$nbest,
		 'min-term-frequency|mtf|tf|f=i' => \$min_term_freq,
		 'min-term-docfrequency|mdf|df|F=i' => \$min_term_ndocs,
		 'profile-iterations|iterate|i=i' => \$niters,
		 'shell|interactive!' => \$shell,
		 'normalize|norm=s' => \$dnorm,
		 'no-normalize|nonormalize|no-norm|nonorm' => sub {$dnorm='no'},
		);
  Getopt::Long::GetOptionsFromArray(\@_,%optSpec);
  if ($help) {
    print STDERR "$0 [-bycat|-bydoc] [-terms|-docs] [-norm=HOW|-nonorm] [-n NBEST] [-i ITERS] [-shell] [-m MAPFILE=$mapfile] QUERY...\n";
    exit 1;
  }
  $mapby //= ($mapfile =~ /pages/ || $get eq 'terms' ? 'cat' : 'doc');

  { select STDERR; $|=1; select STDOUT; }
  my $map = DocClassify::Mapper->loadFile($mapfile, verboseIO=>1)
    or die("$0: failed to load $mapfile: $!");
  $map->info("loaded.");

  ##-- vars
  my $svd = $map->{svd};

  ##-- parse query into signature
  my $q_sig = parse_query($map,@_ ? @_ : 'Eisen:9 Erz:3 Stahl:2');
  $q_sig->save1gFile('-') if (!$shell && !$niters);  ##-- debug: dump signature

  ##-- tweaked version of DocClassify::Mapper::LSI::mapDocument

  ##-- query a "real" doc (debug)
  #my $q_tdm0_doc = $map->{tdm0}->dice_axis(1,42)->pdl;

  ##-- user query pdl
  #$map->{mapccs} = 0; ##-- doesn't seem to work
  my $q_tdm0_user = $map->sigPdlRaw($q_sig, $map->{mapccs});
  $map->logwarn("mapQuery(): null vector for query-string '$q_sig->{str}'")
    if ($map->{verbose} && $map->{warnOnNullDoc} && $q_tdm0_user->sum==0 && !$shell && !@{$q_sig->{docs_}} && !@{$q_sig->{classes_}});

  ##-- query dispatch
  my $q_tdm0   = $q_tdm0_user;

  ##-- guts
  my $getBest = sub {
    if ($get eq 'docs') {
      ##-- merge in doc data
      my $q_xdm = $map->svdApply($q_tdm0->pdl);
      my $n_t0  = $q_tdm0->sum;
      my $n_src = scalar(@{$q_sig->{docs_}}) + scalar(@{$q_sig->{classes_}}) + ($n_t0 > 0 ? 1 : 0);
      if ($n_t0 > 0) { $q_xdm /= $n_src; }
      else           { $q_xdm .= 0; }
      $q_xdm += ($map->{xdm}->dice_axis(1,pdl(long,$q_sig->{docs_}))    / $n_src)->xchg(0,1)->sumover->slice(",*1") if (@{$q_sig->{docs_}});
      $q_xdm += ($map->{xcm}->dice_axis(1,pdl(long,$q_sig->{classes_})) / $n_src)->xchg(0,1)->sumover->slice(",*1") if (@{$q_sig->{classes_}});

      if ($mapby eq 'doc') {
	##~~~~~~ get=docs, mapby=doc: compute distance to each *document* (WAS:to each *centroid*)
	my ($qd_dist);
	#$qd_dist = $map->{disto}->clusterDistanceMatrix(data=>$map->svdApply($q_tdm0->pdl), cdata=>$map->{xdm}); ##-- orig
	$qd_dist = vcosine($q_xdm, $map->{xdm}, $dnorm);
	return bestDocStrings($map,  $qd_dist, $nbest);
      } else {
	##~~~~~~ get=docs, mapby=cat
	my ($qc_dist);
	#$qc_dist = $map->{disto}->clusterDistanceMatrix(data=>$map->svdApply($q_tdm0->pdl), cdata=>$map->{xcm}); ##-- orig, looks good w/doc~page , cat~book
	$qc_dist = vcosine($q_xdm, $map->{xcm}, $dnorm);
	return bestCatStrings($map,  $qc_dist, $nbest);
      }
    } else {
      ##~~~~~~ get=terms

      ##-- get reduced (term x R) matrix
      $map->trace('get xtm') if (!$niters);
      my $xtm = $mapby eq 'cat' ? $svd->{v} : $map->{tdm}->xchg(0,1);

      ##-- group-average query terms
      $map->trace('group-average query terms') if (!$niters);
      my $q_w = $q_tdm0->_nzvals / $q_tdm0->_nzvals->sumover;
      my $xqm = ($xtm->dice_axis(1, $q_tdm0->_whichND->slice("(0),")) * $q_w->slice("*1,"))->xchg(0,1)->sumover->dummy(1,1);

      $map->trace("clusterDistanceMatrix [R=".$xtm->dim(0)."; NT=".$xtm->dim(1)."]") if (!$niters);
      my ($qt_dist);
      #$qt_dist = $map->{disto}->clusterDistanceMatrix(data=>$xqm, cdata=>$xtm);
      $qt_dist  = vcosine($xqm,$xtm,$dnorm);
      my @qt_drng = $qt_dist->minmax;

      ##-- get result filter mask
      if ($min_term_freq || $min_term_ndocs) {
	$map->trace('result filter mask');
	my $mask = $map->get_tf0 < $min_term_freq;
	$mask   |= $map->get_tdf0 < $min_term_ndocs;
	(my $tmp=$qt_dist->where($mask)) += 'inf';
      }
      $map->trace('bestTermStrings') if (!$niters);
      return "$q_sig->{str} \[".join(":",@qt_drng)."]\n", bestTermStrings($map, $qt_dist, $nbest);
    }
  };

  if ($shell) {
    binmode($_,':utf8') foreach (\*STDIN,\*STDOUT,\*STDERR);
    (my $prompt = sub { print "NBEST:$mapfile:$get-by-$mapby> "; })->();
    while (defined($_=<STDIN>)) {
      chomp;
      next if (/^\s*$/);
      if (/^(?:set)?opt(?:ion(?:s?))?\s(.*)/) {
	my $optstr = $1;
	Getopt::Long::GetOptionsFromString($optstr,%optSpec);
	next;
      }
      $q_sig  = parse_query($map,$_);
      $q_tdm0 = $map->sigPdlRaw($q_sig, $map->{mapccs});
      $map->logwarn("mapQuery(): null vector for query-string '$q_sig->{str}'")
	if ($map->{verbose} && $map->{warnOnNullDoc} && $q_tdm0->sum==0);
      print $getBest->();
    } continue {
      $prompt->();
    }
  }
  elsif ($niters) {
    Benchmark::timethese($niters,{"nbest-$get,by-$mapby"=>$getBest});
  }
  else {
    $map->trace("get=$get ; mapby=$mapby ; nbest=$nbest\n");
    my @best = $getBest->();
    print @best;
  }


  exit 0;
  print STDERR "$0: test_cab_query() done: what now?\n";
}
#test_cab_query(@ARGV);

##--------------------------------------------------------------
## test file2dir

##-- trace object inheritance
# DocClassify::Mapper::LSI
# DocClassify::Mapper::ByLemma
# DocClassify::Mapper
# DocClassify::Object
# DocClassify::Logger

sub test_file2dir {
  my $op = shift || 'file2dir';
  $op    = ($op =~ /^-*d/ ? 'dir2file' : 'file2dir');
  my ($mapfile,$mapdir) = ($op eq 'file2dir' ? @_[0,1] : @_[1,0]);
  my $map;

  if ($op eq 'file2dir') {
    $mapfile ||= 'map.bin';
    $mapdir ||= 'map.d';
    $map = DocClassify::Mapper->loadFile($mapfile, verboseIO=>1)
      or die("$0: failed to load file $mapfile: $!");
    $map->info("loaded file $mapfile");

    $map->saveDir($mapdir)
      or die("$0: failed to save directory $mapdir: $!");
    $map->info("saved directory $mapdir");
  }
  else {
    $mapdir ||= 'map.d';
    $mapfile ||= 'map.out';
    $map = DocClassify::Mapper->loadDir($mapdir, verboseIO=>1, mmap=>0)
      or die("$0: failed to load directory $mapdir: $!");
    $map->info("loaded directory $mapdir");

    $map->saveBinFile($mapfile)
      or die("$0: failed to save file $mapfile: $!");
    $map->info("saved file $mapfile");
  }

  exit 0;
}
#test_file2dir(@ARGV);

##--------------------------------------------------------------
## test temp objects

sub isok {
  my $label = shift;
  print STDERR "$label: ", ($_[0] ? "ok" : "NOT ok"), "\n";
}

sub test_tmphash {
  my (%h, %w);
  my $file = 'tmp.db';
  isok("tiehash", tie(%h, 'DocClassify::Temp::Hash', $file));

  ##-- check if file exists
  isok("tie: file: exists", (-e $file));

  ##-- check data
  #%h = %w = map {((chr($_+ord('a')))=>$_)} (0..25);
  %h = %w = (foo=>'bar', baz=>'bonk');
  isok("tie: data: size", keys(%h)==keys(%w));
  isok("tie: data: $_", $h{$_} eq $w{$_}) foreach (sort keys %w);

  ##-- try to delete in-place
  do {
    %h = %w = map {((chr($_+ord('a')))=>$_)} (0..25);
    my $tied = tied(%h);
    my ($key,$val,$status);
    for ($status = $tied->seq($key,$val,DB_File::R_FIRST); $status == 0; $status = $tied->seq($key,$val,DB_File::R_NEXT)) {
      $tied->del($key, DB_File::R_CURSOR) if ($val >= 10);
    }
    $tied->sync();
    system("tt-db2dict.perl $file");
  };

  #exit 1; ##-- check unlink on premature exit

  ##-- untie
  isok("untie", untie(%h));
  isok("untie: file: !exists", (!-e $file));

  ##-- re-tie (-temp)
  tie(%h, 'DocClassify::Temp::Hash', $file, UNLINK=>0)
    or die("$0: re-tie failed for temp-hash $file: $!");
  isok("!temp: tie: file: exists", (-e $file));
  isok("!temp: untie", untie(%h));
  isok("!temp: untie: file: exists", (-e $file));

  unlink($file);
  exit 0;
}
test_tmphash(@ARGV);

sub test_tmparray {
  my (@a,@w);
  my $file = 'tmp.ary';
  isok("tiearray", tie(@a, 'DocClassify::Temp::Array', $file));

  ##-- check if file exists
  isok("tie: file: exists", (-e $file));

  ##-- check data
  @a = @w = qw(foo bar baz bonk);
  isok("tie: data: size", @a==@w);
  isok("tie: data[$_]", $a[$_] eq $w[$_]) foreach (0..$#w);

  #exit 1; ##-- check unlink on premature exit

  ##-- untie
  isok("untie", untie(@a));
  isok("untie: file: !exists", (!-e $file));

  ##-- re-tie (-temp)
  tie(@a, 'DocClassify::Temp::Array', $file, UNLINK=>0)
    or die("$0: re-tie failed for temp-array $file: $!");
  isok("!temp: tie: file: exists", (-e $file));
  isok("!temp: untie", untie(@a));
  isok("!temp: untie: file: exists", (-e $file));

  unlink("$file$_") foreach ('',qw(.idx .hdr));
  exit 0;
}
#test_tmparray(@ARGV);


##--------------------------------------------------------------
## test reflect

## sum of squared errors
sub errsum { return ($_[0]-$_[1])->pow(2)->dsum; }
BEGIN { *PDL::errsum = \&errsum; }

## relative sum of squared errors rerrsum($a,$b) = errsum($a,$b) / variance($a)
sub rerrsum { return errsum(@_) / $_[0]->variance; }
BEGIN { *PDL::rerrsum = \&rerrsum; }

sub log2 {
  my ($x,$eps) = @_;
  $eps //= 1e-38;
  return $x->log->inplace->setnantobad->inplace->setbadtoval(log($eps)) / log(2);
}

sub dist_kl {
  my ($p,$q,$eps) = @_;
  return ($p * (log2($p,$eps) - log2($q,$eps)))->sumover;
}
sub dist_js {
  my ($p,$q,$eps) = @_;
  my $m    = ($p+$q)/2;
  #return (dist_kl($p,$m,$eps) + dist_kl($q,$m,$eps))/2;
  my $logm = log2($m,$eps);
  return (($p*(log2($p,$eps)-$logm))->sumover + ($q*(log2($q,$eps)-$logm))->sumover)/2;
}
sub dist_l2 {
  my ($p,$q) = @_;
  my $dist = ($p-$q)->inplace->pow(2)->sumover->inplace->sqrt;
  $dist->inplace->setnantobad->inplace->setbadtoval('inf');
  return $dist;
}

## dump_kbest($map,$qv,$k=10,$label='kbest',$xdm=$map->{svd}{v},%opts)
##  + %opts:
##     dist=>\&distsub,
sub dump_kbest {
  my ($map,$qv,$k,$label,$xtm,%opts) = @_;
  my $dsub = $opts{dist} // sub { $map->qdistance($_[0],$_[1]) };
  $k     //= 10;
  $label //= 'kbest';
  $xtm   //= $map->{svd}{v};

  my $dist  = $dsub->($qv, $xtm);
  my $kbest = $map->kBestItems($dist, $map->{tenum}, k=>$k);
  foreach (0..$#$kbest) {
    $kbest->[$_]{i} = $_;
    utf8::decode($kbest->[$_]{label}) if (!utf8::is_utf8($kbest->[$_]{label}));
  }
  my @drange = $dist->minmax;
  print
    (($label//'QUERY')." [".join(":",@drange)."]\n",
     (map {" [$_->{i}]\t$_->{dist}\t$_->{id}\t".($_->{label}//'')."\n"} @$kbest),
    );
}

sub test_reflect {
  my $mapfile = shift || 'dta-dc-xpages-map.64.d';
  $map = DocClassify::Mapper->loadFile($mapfile, verboseIO=>1, mmap=>1)
    or die("$0: failed to load file $mapfile: $!");
  $map->info("loaded file $mapfile");

  ##-- debug: doc
  # rerror(svd.u, apply0 : doc=marx_kapital0301_1894.0347.csv.1g ) = [2.6988193e-28]
  # rdist (svd.u, apply0 : doc=marx_kapital0301_1894.0347.csv.1g ) = [0]
  # rerror(tdm,   tdm1   : doc=marx_kapital0301_1894.0347.csv.1g ) = 1.35205828774224e-27
  if (0) {
    my $dsym  = 'dta-pages.d/marx_kapital0301_1894.d/marx_kapital0301_1894.0347.csv.1g';
    my $dlab  = basename($dsym);
    $map->info("test: doc=$dlab");
    my $di    = $map->{denum}{sym2id}{$dsym};
    my $tdm   = $map->{tdm};
    my $d_tdm = $tdm->dice_axis(1,$di);
    my $d_xdm  = $map->{svd}->apply0($d_tdm->todense);
    print "rerror(svd.u, apply0 : doc=$dlab ) = ".rerrsum($map->{svd}{u}->slice(",$di"), $d_xdm)."\n";
    print "rdist (svd.u, apply0 : doc=$dlab ) = ".$map->qdistance($d_xdm, $map->{svd}{u}->slice(",$di"))."\n";
    ##
    my $d_tdm0 = (($d_tdm/$map->{tw})->exp - $map->{smoothf})->_missing(0);
    $d_tdm0->_vals->inplace->rint;
    my $d_tdm1 = ($d_tdm0 + $map->{smoothf});
    $d_tdm1->inplace->log;
    $d_tdm1 *= $map->{tw};
    $d_tdm1->_missing(0);
    print "rerror(tdm,   tdm1   : doc=$dlab ) = ".errsum($d_tdm->_nzvals, $d_tdm1->_nzvals)."\n";
    #exit 0;
  }

  ##-- debug: term: ok
  # rerror(svd.v, apply1 : term=Produktion ) = [3.6163335e-21]
  # rdist (svd.v, apply1 : term=Produktion ) = [0]
  # rerror(tdm,   tdm1   : term=Produktion ) = 7.99365081212095e-27
  if (0) {
    my $tsym  = 'Produktion';
    $map->info("test: term=$tsym");
    my $ti    = $map->{tenum}{sym2id}{$tsym};
    my $tdm   = $map->{tdm};
    my $t_tdm = $tdm->dice_axis(0,$ti);
    my $t_xdm  = $map->{svd}->apply1($t_tdm->todense)->xchg(0,1);
    print "rerror(svd.v, apply1 : term=$tsym ) = ".rerrsum($map->{svd}{v}->slice(",$ti"), $t_xdm)."\n";
    print "rdist (svd.v, apply1 : term=$tsym ) = ".$map->qdistance($map->{svd}{v}->slice(",$ti"), $t_xdm)."\n";
    ##
    my $t_tdm0 = (($t_tdm/$map->{tw}->slice("($ti)"))->exp - $map->{smoothf})->_missing(0);
    $t_tdm0->_vals->inplace->rint;
    my $t_tdm1 = ($t_tdm0 + $map->{smoothf});
    $t_tdm1->inplace->log;
    $t_tdm1 *= $map->{tw}->slice("($ti)");
    $t_tdm1->_missing(0);
    print "rerror(tdm,   tdm1   : term=$tsym ) = ".errsum($t_tdm->_nzvals, $t_tdm1->_nzvals)."\n";
    #exit 0;
  }

  ##-- test: approximate term as by averaging over k-best docs (k==0 --> all docs)
  if (1) {
    my $tsym  = 'Produktion';
    $map->info("test/approx: term=$tsym");
    my $ti    = $map->{tenum}{sym2id}{$tsym};

    ##-- common
    my $svd   = $map->{svd};
    my $tdm   = $map->{tdm};
    my $xdm   = $svd->{u};
    my ($how);

    ##-- raw svd.v
    # Produktion:svd.v [0:1.47134703592471]
    #   [0]	0	77231	Produktion
    #   [1]	0.0185670031345129	56722	Rohstoff
    #   [2]	0.019787124129197	19431	produzieren
    #   [3]	0.0260195556111262	1289	industriell
    #   [4]	0.0272619341872566	151136	??berproduktion
    #   [5]	0.0338075293462893	27006	Produzent
    #   [6]	0.0347882276209674	81227	Arbeitslohn
    #   [7]	0.0359281409715821	159765	Konsumtion
    #   [8]	0.0373910179575001	156536	Verkaufspreis
    #   [9]	0.0381823960712526	100014	Kapital
    print "-- ".($how='svd.v')."\n";
    my $t_x1  = $svd->{v}->slice(",$ti");
    dump_kbest($map, $t_x1,  10, "$tsym:$how");

    ##-- get distances
    my $ddist = $map->qdistance($t_x1, $xdm);
    my $ddisti= $ddist->qsorti;
    my $k = 100;
    my $dik    = ($k<=0 ? $ddisti->xvals : $ddisti->slice("0:".($k-1)));
    my $ddistk = $ddist->index($dik);
    $dik->sever;
    $ddistk->sever;
    undef($ddist);
    undef($ddisti);

    if (0) {
      ##-- svd.vp- : row-normalized svd.vp (sums to 1 for each t) : GOOD: pretty much identical to svd.v (NOT P --> includes negative values!)
      # Produktion:svd.vp- [1.11022302462516e-16:1.92240125065685]
      #   [0]	1.11022302462516e-16	77231	Produktion
      #   [1]	0.0185670031345131	56722	Rohstoff
      #   [2]	0.0197871241291971	19431	produzieren
      #   [3]	0.0260195556111266	1289	industriell
      #   [4]	0.0272619341872568	151136	??berproduktion
      #   [5]	0.0338075293462893	27006	Produzent
      #   [6]	0.0347882276209673	81227	Arbeitslohn
      #   [7]	0.0359281409715823	159765	Konsumtion
      #   [8]	0.0373910179575001	156536	Verkaufspreis
      #   [9]	0.0381823960712526	100014	Kapital
      print "-- ".($how='svd.vp-')."\n";
      my $vp    = $svd->{v} / $svd->{v}->sumover->slice("*1,");
      my $t_p1  = $vp->slice(",$ti");
      dump_kbest($map, $t_p1,  10, "$tsym:$how", $vp);
    }

    if (0) {
      ##-- svd.vp+ : row-normalized svd.vp (sums to 1 for each t, global-positive): ok
      # Produktion:svd.vp+ [0:0.0178217411001815]
      #   [0]	0	77231	Produktion
      #   [1]	8.80492678634326e-06	115471	Kapitalist
      #   [2]	1.15977541089718e-05	69130	Mehrwert
      #   [3]	1.58567623866901e-05	148705	Arbeitskraft
      #   [4]	2.29300948351163e-05	9734	Profit
      #   [5]	2.58138900672833e-05	149004	kapitalistisch
      #   [6]	2.72690675240383e-05	81227	Arbeitslohn
      #   [7]	2.84411370307103e-05	152541	Arbeiter
      #   [8]	2.90657302141062e-05	129291	Produktionsmittel
      #   [9]	2.92795355021358e-05	19431	produzieren
      print "-- ".($how='svd.vp+')."\n";
      my $vp    = ($svd->{v}-$svd->{v}->min); $vp /= $vp->sumover->slice("*1,");
      my $t_p1  = $vp->slice(",$ti");
      dump_kbest($map, $t_p1,  10, "$tsym:$how", $vp);
    }

    if (0) {
      ##-- svd.vpr+ : row-normalized svd.vp (sums to 1 for each t, row-positive): GOOD (~svd.v)
      # Produktion:svd.v [0:0.264042797194421]
      #   [0]	0	77231	Produktion
      #   [1]	0.00173636246891218	56722	Rohstoff
      #   [2]	0.00180276862733897	19431	produzieren
      #   [3]	0.0024463300871812	151136	??berproduktion
      #   [4]	0.00245948615733804	1289	industriell
      #   [5]	0.00310889913335155	27006	Produzent
      #   [6]	0.00317209864135559	81227	Arbeitslohn
      #   [7]	0.003194959826053	159765	Konsumtion
      #   [8]	0.00328181959821261	156536	Verkaufspreis
      #   [9]	0.00338841121159972	100014	Kapital
      print "-- ".($how='svd.vpr+')."\n";
      my $vmin  = $svd->{v}->minimum->slice("*1,");
      my $vp    = $svd->{v}-$vmin; $vp /= $vp->sumover->slice("*1,");
      my $t_p1  = $vp->slice(",$ti");
      dump_kbest($map, $t_p1,  10, "$tsym:$how", $vp);
    }

    if (0) {
      ##-- svd.vpr+.js : row-normalized svd.vp, dist via jensen-shannon divergence D_{JS} = .5*D(P||M) + .5*D(Q||M) with M=.5(P+Q)
      ## + not too bad: -{Produzent,Verkaufspreis,Kapital} ; +{Arbeitskraft,Produktionszweig,Exploitation}
      # Produktion:svd.vpr+.js [0:0.144952201209124]
      #   [0]	0	77231	Produktion
      #   [1]	0.000798724427789069	56722	Rohstoff
      #   [2]	0.00130882038091908	1289	industriell
      #   [3]	0.00137934542935164	81227	Arbeitslohn
      #   [4]	0.00143272089381112	19431	produzieren
      #   [5]	0.00146633847645853	148705	Arbeitskraft
      #   [6]	0.00148706790278635	13108	Produktionszweig
      #   [7]	0.00150903871324483	151136	??berproduktion
      #   [8]	0.00157296827766291	159765	Konsumtion
      #   [9]	0.00159901073762117	11315	Exploitation
      print "-- ".($how='svd.vpr+.js')."\n";
      my $vmin  = $svd->{v}->minimum->slice("*1,");
      my $vp    = $svd->{v}-$vmin; $vp /= $vp->sumover->slice("*1,");
      my $t_p1  = $vp->slice(",$ti");
      ##
      dump_kbest($map, $t_p1,  10, "$tsym:$how", $vp, dist=>\&dist_js);
    }

    if (0) {
      ##-- svd.v2 : row-unit-length svd.v (l2-length = 1 foreach t): GOOD (==svd.v)
      # Produktion:svd.v2 [0:1.47134703592471]
      #   [0]	0	77231	Produktion
      #   [1]	0.0185670031345129	56722	Rohstoff
      #   [2]	0.0197871241291971	19431	produzieren
      #   [3]	0.0260195556111262	1289	industriell
      #   [4]	0.0272619341872566	151136	??berproduktion
      #   [5]	0.0338075293462892	27006	Produzent
      #   [6]	0.0347882276209672	81227	Arbeitslohn
      #   [7]	0.0359281409715821	159765	Konsumtion
      #   [8]	0.0373910179575001	156536	Verkaufspreis
      #   [9]	0.0381823960712526	100014	Kapital
      print "-- ".($how='svd.v2')."\n";
      my $v2    = $svd->{v} / $svd->{v}->pow(2)->sumover->sqrt->slice("*1,");
      my $t_x2  = $v2->slice(",$ti");
      dump_kbest($map, $t_x2,  10, "$tsym:$how", $v2);
    }

    if (0) {
      ##-- kbest-l2-avg: group-average of k-best l2-row-normalized doc-vectors in svd.u (via l2-normalized svd.v) : OK (~ kbest-avg)
      # rerror(svd.v2, kbest-l2-avg(100) : term=Produktion ) = [ 5.1654862]
      # rdist (svd.v2, kbest-l2-avg(100) : term=Produktion ) = [0.040097239]
      # Produktion:kbest-l2-avg(100) [0.0117268986850964:1.4759828913358]
      #   [0]	0.0117268986850964	40587	Verausgabung
      #   [1]	0.0119158123141767	116890	Produktionsproze??
      #   [2]	0.0121966842165488	115471	Kapitalist
      #   [3]	0.0122318431419975	129291	Produktionsmittel
      #   [4]	0.0128862266735164	60592	Warenproduktion
      #   [5]	0.0130077618486985	149004	kapitalistisch
      #   [6]	0.0130912015116419	148705	Arbeitskraft
      #   [7]	0.0130987559841615	123833	Produktionselement
      #   [8]	0.0133941328145437	146525	Wertsumme
      #   [9]	0.0141278196906331	98013	produktiv
      print "--".($how="kbest-l2-avg($k)")."\n";
      my $u2    = $svd->{u} / $svd->{u}->pow(2)->sumover->sqrt->slice("*1,");
      my $v2    = $svd->{v} / $svd->{v}->pow(2)->sumover->sqrt->slice("*1,");
      my $t_x2  = $v2->slice(",$ti");
      my $ddist2  = $map->qdistance($t_x2, $u2);
      my $ddisti2 = $ddist2->qsorti;
      my $dik2    = ($k<=0 ? $ddisti2->xvals : $ddisti2->slice("0:".($k-1)));
      my $ddistk2 = $ddist2->index($dik2);
      ##
      my $t_xk2   = $u2->dice_axis(1,$dik2)->xchg(0,1)->average->slice(",*1");
      $t_xk2    /=  $t_xk2->pow(2)->sumover->inplace->sqrt->slice("*1,");
      ##
      print "rerror(svd.v2, $how : term=$tsym ) = ".rerrsum($t_x2, $t_xk2)."\n";
      print "rdist (svd.v2, $how : term=$tsym ) = ".$map->qdistance($t_x2, $t_xk2)."\n";
      dump_kbest($map, $t_xk2, 10, "$tsym:$how", $v2);
    }

    if (0) {
      ##-- kbest-l2-bavg : boltzmann-average of k-best l2-row-normalized doc-vectors in svd.u (via l2-normalized svd.v) ~ ok (~bavg)
      ##
      # --kbest-l2-bavg(k=100,b=2,beta=-1)
      # rerror(svd.v2, kbest-l2-bavg(k=100,b=2,beta=-1) : term=Produktion ) = [ 62.716522]
      # rdist (svd.v2, kbest-l2-bavg(k=100,b=2,beta=-1) : term=Produktion ) = [0.040735215]
      # Produktion:kbest-l2-bavg(k=100,b=2,beta=-1) [0.0114082501564637:1.47475029498996]
      #   [0]	0.0114082501564637	116890	Produktionsproze??
      #   [1]	0.0118299379829657	129291	Produktionsmittel
      #   [2]	0.01191843839326	115471	Kapitalist
      #   [3]	0.0122205767975829	40587	Verausgabung
      #   [4]	0.0126338960221017	123833	Produktionselement
      #   [5]	0.0127172705041801	60592	Warenproduktion
      #   [6]	0.0130559368587654	149004	kapitalistisch
      #   [7]	0.0133846135177287	146525	Wertsumme
      #   [8]	0.013648280935277	98013	produktiv
      #   [9]	0.0137983337603055	148705	Arbeitskraft
      ##
      # --kbest-l2-bavg(k=0,b=2,beta=-100)
      # rerror(svd.v2, kbest-l2-bavg(k=0,b=2,beta=-100) : term=Produktion ) = [ 62.928751]
      # rdist (svd.v2, kbest-l2-bavg(k=0,b=2,beta=-100) : term=Produktion ) = [0.039261567]
      # Produktion:kbest-l2-bavg(k=0,b=2,beta=-100) [0.0157172190421844:1.47814329052672]
      #   [0]	0.0157172190421844	60592	Warenproduktion
      #   [1]	0.0160573297647488	148705	Arbeitskraft
      #   [2]	0.0177617821685755	40587	Verausgabung
      #   [3]	0.0179849754443903	114859	Produktionsbedingung
      #   [4]	0.0180683769966997	149004	kapitalistisch
      #   [5]	0.0183035122358343	123833	Produktionselement
      #   [6]	0.0192859647037391	146525	Wertsumme
      #   [7]	0.0193623336513081	116890	Produktionsproze??
      #   [8]	0.0196963445378234	52421	vergegenst??ndlicht
      #   [9]	0.0198621571648722	129291	Produktionsmittel
      ##
      my ($b,$beta) = (2,-100);
      print "--".($how="kbest-l2-bavg(k=$k,b=$b,beta=$beta)")."\n";
      my $u2    = $svd->{u} / $svd->{u}->pow(2)->sumover->sqrt->slice("*1,");
      my $v2    = $svd->{v} / $svd->{v}->pow(2)->sumover->sqrt->slice("*1,");
      my $t_x2  = $v2->slice(",$ti");
      my $ddist2  = $map->qdistance($t_x2, $u2);
      my $ddisti2 = $ddist2->qsorti;
      my $dik2    = ($k<=0 ? $ddisti2->xvals : $ddisti2->slice("0:".($k-1)));
      my $ddistk2 = $ddist2->index($dik2);
      ##
      my $dsim = pdl($b)->pow($beta * $ddistk2);
      $dsim   /= $dsim->sumover;
      my $t_xk2b = ($xdm->dice_axis(1,$dik2)->xchg(0,1) * $dsim)->sumover;
      ##
      print "rerror(svd.v2, $how : term=$tsym ) = ".rerrsum($t_x2, $t_xk2b)."\n";
      print "rdist (svd.v2, $how : term=$tsym ) = ".$map->qdistance($t_x2, $t_xk2b)."\n";
      dump_kbest($map, $t_xk2b, 10, "$tsym:$how", $v2);
    }
    #exit 0;

    if (0) {
      ##-- kbest-2avg : l2-distance weighted average of k-best (l2) doc-vectors in svd.u : so-so
      # --kbest-2avg(k=100,beta=-1)
      # rerror(svd.v2, kbest-2avg(k=100,beta=-1) : term=Produktion ) = [ 39.735048]
      # rdist (svd.v2, kbest-2avg(k=100,beta=-1) : term=Produktion ) = [0.049791168]
      # Produktion:kbest-2avg(k=100,beta=-1) [0.0108282852056867:1.46074752835697]
      #   [0]	0.0108282852056867	115471	Kapitalist
      #   [1]	0.0109064173155509	129291	Produktionsmittel
      #   [2]	0.012200192626966	116890	Produktionsproze??
      #   [3]	0.0124558008467881	98013	produktiv
      #   [4]	0.0128778783053302	54397	Warenwert
      #   [5]	0.0130848809960051	69130	Mehrwert
      #   [6]	0.0134480336063074	70124	Zirkulation
      #   [7]	0.0137110082912136	148366	produziert
      #   [8]	0.0137177167330791	153018	Mehrprodukt
      #   [9]	0.0139232253535294	155105	Kapitalwert
      ##
      # --kbest-2avg(k=0,beta=-1)
      # rerror(svd.v2, kbest-2avg(k=0,beta=-1) : term=Produktion ) = [ 64.226939]
      # rdist (svd.v2, kbest-2avg(k=0,beta=-1) : term=Produktion ) = [0.91048616]
      # Produktion:kbest-2avg(k=0,beta=-1) [0.50995566728076:1.07216023245732]
      #   [0]	0.50995566728076	94065	f??gen
      #   [1]	0.53549658025045	9183	abgeben
      #   [2]	0.545092393389295	154007	schlie??en
      #   [3]	0.603070629440469	154863	aufnehmen
      #   [4]	0.615285173187793	66178	kommend
      #   [5]	0.615326163656242	101471	aufh??ren
      #   [6]	0.618147450618196	122641	haben
      #   [7]	0.620910132396963	45303	gefallen
      #   [8]	0.623354767117017	67129	vergessen
      #   [9]	0.62364595623985	71157	einzig
      ##
      # --kbest-2avg(k=0,beta=-100)
      # rerror(svd.v2, kbest-2avg(k=0,beta=-100) : term=Produktion ) = [ 35.740615]
      # rdist (svd.v2, kbest-2avg(k=0,beta=-100) : term=Produktion ) = [0.097080886]
      # Produktion:kbest-2avg(k=0,beta=-100) [0.0365309863558141:1.42125382510814]
      #   [0]	0.0365309863558141	129432	Goldproduktion
      #   [1]	0.0401575026212001	40277	Wertteil
      #   [2]	0.0411393886696994	50409	Zirkulationskapital
      #   [3]	0.0415757018623155	42003	Schatzbildung
      #   [4]	0.0420358743997047	158184	Goldproduzent
      #   [5]	0.0424644336281436	153018	Mehrprodukt
      #   [6]	0.0435088940933253	16376	R??ckzuverwandeln
      #   [7]	0.0444105545882917	58585	r??ckverwandelt
      #   [8]	0.0444698574078793	36807	Naturalform
      #   [9]	0.0448591648024118	142553	Revenue
      my $beta = -10;
      print "--".($how="kbest-2avg(k=$k,beta=$beta)")."\n";
      my $ddist2  = dist_l2($t_x1, $xdm);
      my $ddisti2 = $ddist2->qsorti;
      my $dik2    = ($k<=0 ? $ddisti2->xvals : $ddisti2->slice("0:".($k-1)));
      my $ddistk2 = $ddist2->index($dik2);
      ##
      my $dsim   = $ddistk2->pow($beta); $dsim /= $dsim->sumover;
      my $t_xk2 = ($xdm->dice_axis(1,$dik2)->xchg(0,1) * $dsim)->sumover;
      ##
      print "rerror(svd.v2, $how : term=$tsym ) = ".rerrsum($t_x1, $t_xk2)."\n";
      print "rdist (svd.v2, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_xk2)."\n";
      dump_kbest($map, $t_xk2, 10, "$tsym:$how");
    }
    #exit 0;

    if (0) {
      ##-- kbest-avg: group-average of k-best doc-vectors in svd.u ; crappy for k=0
      # rerror(svd.v, kbest-avg(100) : term=Produktion ) = [ 42.296226]
      # rdist (svd.v, kbest-avg(100) : term=Produktion ) = [0.040787276]
      # Produktion:kbest-avg(100) [0.0113999018334485:1.47476003527591]
      #   [0]	0.0113999018334485	116890	Produktionsproze??
      #   [1]	0.0118339310130481	129291	Produktionsmittel
      #   [2]	0.0119070398281698	115471	Kapitalist
      #   [3]	0.0122209921656428	40587	Verausgabung
      #   [4]	0.0126411596660747	123833	Produktionselement
      #   [5]	0.0127142086788566	60592	Warenproduktion
      #   [6]	0.0130523132675067	149004	kapitalistisch
      #   [7]	0.013389542514514	146525	Wertsumme
      #   [8]	0.01362754654162	98013	produktiv
      #   [9]	0.0138245208056922	148705	Arbeitskraft
      print "--".($how="kbest-avg($k)")."\n";
      my $t_x1k = $xdm->dice_axis(1,$dik)->xchg(0,1)->average->slice(",*1");
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_x1k)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_x1k)."\n";
      dump_kbest($map, $t_x1k, 10, "$tsym:$how");
    }

    if (0) {
      ##-- kbest-bavg: boltzmann-weighting of k-best doc vectors from svd.u (not very different, even for k=10000; crappy for k=0)
      # --kbest-bavg(k=0,b=2,beta=-1)
      # rerror(svd.v, kbest-bavg(k=100,b=2,beta=-1) : term=Produktion ) = [ 42.300564]
      # rdist (svd.v, kbest-bavg(k=100,b=2,beta=-1) : term=Produktion ) = [0.040735215]
      # Produktion:kbest-bavg(k=100,b=2,beta=-1) [0.0114082501564636:1.47475029498996]
      #   [0]	0.0114082501564636	116890	Produktionsproze??
      #   [1]	0.0118299379829657	129291	Produktionsmittel
      #   [2]	0.0119184383932601	115471	Kapitalist
      #   [3]	0.0122205767975828	40587	Verausgabung
      #   [4]	0.0126338960221016	123833	Produktionselement
      #   [5]	0.01271727050418	60592	Warenproduktion
      #   [6]	0.0130559368587655	149004	kapitalistisch
      #   [7]	0.0133846135177287	146525	Wertsumme
      #   [8]	0.0136482809352771	98013	produktiv
      #   [9]	0.0137983337603055	148705	Arbeitskraft
      ##
      # --kbest-bavg(k=0,b=2,beta=-100)
      # rerror(svd.v, kbest-bavg(k=0,b=2,beta=-100) : term=Produktion ) = [ 44.797449]
      # rdist (svd.v, kbest-bavg(k=0,b=2,beta=-100) : term=Produktion ) = [0.039261567]
      # Produktion:kbest-bavg(k=0,b=2,beta=-100) [0.0157172190421841:1.47814329052672]
      #   [0]	0.0157172190421841	60592	Warenproduktion
      #   [1]	0.0160573297647487	148705	Arbeitskraft
      #   [2]	0.0177617821685753	40587	Verausgabung
      #   [3]	0.0179849754443901	114859	Produktionsbedingung
      #   [4]	0.0180683769966995	149004	kapitalistisch
      #   [5]	0.0183035122358342	123833	Produktionselement
      #   [6]	0.019285964703739	146525	Wertsumme
      #   [7]	0.0193623336513081	116890	Produktionsproze??
      #   [8]	0.0196963445378233	52421	vergegenst??ndlicht
      #   [9]	0.0198621571648722	129291	Produktionsmittel
      my ($b,$beta) = (2,-100);
      print "--".($how="kbest-bavg(k=$k,b=$b,beta=$beta)")."\n";
      my $dsim = pdl($b)->pow($beta * $ddistk);
      $dsim   /= $dsim->sumover;
      my $t_x1kb = ($xdm->dice_axis(1,$dik)->xchg(0,1) * $dsim)->sumover;
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_x1kb)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_x1kb)."\n";
      dump_kbest($map, $t_x1kb, 10, "$tsym:$how");
    }

    if (0) {
      ##-- kbest-gavg: gaussian-weighting of k-best doc vectors from svd.u ~ tavg,bavg , crappy for k=0
      # --kbest-gavg(k=100)
      # rerror(svd.v, kbest-gavg(k=100) : term=Produktion ) = [ 42.389441]
      # rdist (svd.v, kbest-gavg(k=100) : term=Produktion ) = [0.039020867]
      # Produktion:kbest-gavg(k=100) [0.0119692620628209:1.47395068789919]
      #   [0]	0.0119692620628209	129291	Produktionsmittel
      #   [1]	0.0121482275255931	116890	Produktionsproze??
      #   [2]	0.0125920127802549	40587	Verausgabung
      #   [3]	0.0126742193126104	115471	Kapitalist
      #   [4]	0.0128049526674902	123833	Produktionselement
      #   [5]	0.0131313571758523	148705	Arbeitskraft
      #   [6]	0.013459151408022	60592	Warenproduktion
      #   [7]	0.0135848741298209	146525	Wertsumme
      #   [8]	0.0136723671900842	149004	kapitalistisch
      #   [9]	0.0148040911292477	98013	produktiv
      print "--".($how="kbest-gavg(k=$k)")."\n";
      my $dsim = _gausscdf($ddistk, $ddistk->average, $ddistk->stddev);
      $dsim->minus(1,$dsim,1);
      $dsim /= $dsim->sumover;
      my $t_x1kg = ($xdm->dice_axis(1,$dik)->xchg(0,1) * $dsim)->sumover;
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_x1kg)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_x1kg)."\n";
      dump_kbest($map, $t_x1kg, 10, "$tsym:$how");
    }

    if (0) {
      ##-- kbest-tavg: tdm-weighting of k-best doc vectors from svd.u : pretty good for $k=0
      # -- kbest-tavg(100)
      # rerror(svd.v, kbest-tavg(100) : term=Produktion ) = [ 50.667737]
      # rdist (svd.v, kbest-tavg(100) : term=Produktion ) = [0.040108406]
      # Produktion:kbest-tavg(100) [0.0371659356496629:1.44638669196081]
      #   [0]	0.0371659356496629	159765	Konsumtion
      #   [1]	0.0375371189774607	27006	Produzent
      #   [2]	0.0401084059802561	77231	Produktion
      #   [3]	0.042055690131776	114859	Produktionsbedingung
      #   [4]	0.0422924259160886	76441	produktions-
      #   [5]	0.0424957478171955	19431	produzieren
      #   [6]	0.0426125832558132	13108	Produktionszweig
      #   [7]	0.0437180534433687	11315	Exploitation
      #   [8]	0.0440532107869019	98013	produktiv
      #   [9]	0.0443085578832868	119000	Produktivkraft
      # -- kbest-tavg(0)
      # rerror(svd.v, kbest-tavg(0) : term=Produktion ) = [ 55.228536]
      # rdist (svd.v, kbest-tavg(0) : term=Produktion ) = [0.046454206]
      # Produktion:kbest-tavg(0) [0.0464542057507583:1.38476997115507]
      #   [0]	0.0464542057507583	77231	Produktion
      #   [1]	0.0680245881865382	56722	Rohstoff
      #   [2]	0.0690173737806131	40035	Verwertung
      #   [3]	0.0750803418624154	1289	industriell
      #   [4]	0.0759822670883686	19431	produzieren
      #   [5]	0.0797753344818528	111535	Industriezweig
      #   [6]	0.0872513911197084	4260	Smith
      #   [7]	0.0888970063856764	32900	Konsument
      #   [8]	0.0895056379633526	103654	funktionierend
      #   [9]	0.08952439768719	4245	disponibel
      print "-- ".($how="kbest-tavg($k)")."\n";
      my $pd   = $tdm->dice_axis(0,$ti);
      $pd     /= $pd->_nzvals->sumover;
      my $pd_nzi  = $pd->_nzvals->qsorti->slice("-1:0");
      my $pd_nzik = $pd_nzi->slice("0:".($k-1));
      my $pik    = $pd->_whichND->slice("(1),")->index($pd_nzik);
      my $ppk    = $pd->_nzvals->index($pd_nzik);
      my $t_x1kt = ($xdm->dice_axis(1,$pik)->xchg(0,1) * ($ppk/$ppk->sumover))->sumover->slice(",*1");
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_x1kt)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_x1kt)."\n";
      dump_kbest($map, $t_x1kt, 10, "$tsym:$how");
    }

    if (0) {
      ##-- tdm-apply : apply1 of term-vector from tdm : GOOD (universal only)
      # rerror(svd.v, tdm-apply : term=Produktion ) = [3.6167841e-21]
      # rdist (svd.v, tdm-apply : term=Produktion ) = [0]
      # Produktion:tdm-apply [0:1.47134703592439]
      #   [0]	0	77231	Produktion
      #   [1]	0.0185670031344831	56722	Rohstoff
      #   [2]	0.0197871241291993	19431	produzieren
      #   [3]	0.0260195556112294	1289	industriell
      #   [4]	0.0272619341871018	151136	??berproduktion
      #   [5]	0.0338075293463966	27006	Produzent
      #   [6]	0.0347882276209726	81227	Arbeitslohn
      #   [7]	0.0359281409711207	159765	Konsumtion
      #   [8]	0.0373910179576675	156536	Verkaufspreis
      #   [9]	0.0381823960712092	100014	Kapital
      print "-- ".($how="tdm-apply")."\n";
      my $t_tdm = $tdm->dice_axis(0,$ti);
      my $t_x1a = $svd->apply1($t_tdm)->xchg(0,1);
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_x1a)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_x1a)."\n";
      dump_kbest($map, $t_x1a, 10, "$tsym:$how");
    }

    if (0) {
      ##-- kbest-avg-apply : apply1 of group-average over k-best doc-vectors from tdm --> ~ kbest-avg (but much more expensive)
      # rerror(svd.v, kbest-avg-apply(100) : term=Produktion ) = [ 42.296226]
      # rdist (svd.v, kbest-avg-apply(100) : term=Produktion ) = [0.040787276]
      # Produktion:kbest-avg-apply(100) [0.0113999018334485:1.47476003527591]
      #   [0]	0.0113999018334485	116890	Produktionsproze??
      #   [1]	0.0118339310130481	129291	Produktionsmittel
      #   [2]	0.0119070398281698	115471	Kapitalist
      #   [3]	0.0122209921656428	40587	Verausgabung
      #   [4]	0.0126411596660747	123833	Produktionselement
      #   [5]	0.0127142086788566	60592	Warenproduktion
      #   [6]	0.0130523132675067	149004	kapitalistisch
      #   [7]	0.0133895425145141	146525	Wertsumme
      #   [8]	0.0136275465416201	98013	produktiv
      #   [9]	0.0138245208056922	148705	Arbeitskraft
      print "-- ".($how="kbest-avg-apply($k)")."\n";
      my $t_tdm = $tdm->dice_axis(1,$dik)->xchg(0,1)->average->dummy(1,1);
      my $t_x1aa = $svd->apply0($t_tdm);
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_x1aa)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_x1aa)."\n";
      dump_kbest($map, $t_x1aa, 10, "$tsym:$how");
    }

    if (0) {
      ##-- tdm0-apply-ksum : WEIRD, expensive
      # rerror(svd.v, tdm0-apply-ksum(100) : term=Produktion ) = [ 171.36049]
      # rdist (svd.v, tdm0-apply-ksum(100) : term=Produktion ) = [0.07869679]
      # Produktion:tdm0-apply-ksum(100) [0.0497729096585258:1.44059066019192]
      #   [0]	0.0497729096585258	1678	??quivalent
      #   [1]	0.058785606258818	64024	unterstellen
      #   [2]	0.0607827590603948	117410	Wertbildung
      #   [3]	0.0611397859818161	143070	Arbeitsbedingung
      #   [4]	0.0623912393391791	148705	Arbeitskraft
      #   [5]	0.0627086161689305	147010	verwertend
      #   [6]	0.0636987003632646	47293	Gebrauchswert
      #   [7]	0.0637253361978216	19431	produzieren
      #   [8]	0.0647278467415294	40587	Verausgabung
      #   [9]	0.064958286138542	69582	potentiell
      print "-- ".($how="tdm0-apply-ksum($k)")."\n";
      my $t_tdm0k = $tdm->dice_axis(1,$dik);
      $t_tdm0k /= $map->{tw};
      $t_tdm0k->inplace->exp;
      $t_tdm0k -= $map->{smoothf};
      $t_tdm0k->_vals->inplace->rint;
      $t_tdm0k->_missing(0);
      my $t_x1ak  = $map->svdApply($t_tdm0k->xchg(0,1)->sumover->dummy(1,1));
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_x1ak)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_x1ak)."\n";
      dump_kbest($map, $t_x1ak, 10, "$tsym:$how");
    }

    if (0) {
      ##-- tdm0-apply-kavg : apply1 of group-average over k-best doc-vectors from tdm0 --> ~ kbest-avg (but much more expensive)
      # rerror(svd.v, tdm0-apply-kavg(100) : term=Produktion ) = [ 35.608157]
      # rdist (svd.v, tdm0-apply-kavg(100) : term=Produktion ) = [0.041259831]
      # Produktion:tdm0-apply-kavg(100) [0.0129851255977779:1.47374399647646]
      #   [0]	0.0129851255977779	116890	Produktionsproze??
      #   [1]	0.0133013001091843	40587	Verausgabung
      #   [2]	0.0139899026914477	129291	Produktionsmittel
      #   [3]	0.0140100164545256	60592	Warenproduktion
      #   [4]	0.0142486997813975	149004	kapitalistisch
      #   [5]	0.0142641539017214	123833	Produktionselement
      #   [6]	0.0144312401711492	115471	Kapitalist
      #   [7]	0.0147504704512738	148705	Arbeitskraft
      #   [8]	0.0149098272976049	146525	Wertsumme
      #   [9]	0.0153913127332446	98013	produktiv
      print "-- ".($how="tdm0-apply-kavg($k)")."\n";
      my $t_tdm0k = $tdm->dice_axis(1,$dik);
      $t_tdm0k /= $map->{tw};
      $t_tdm0k->inplace->exp;
      $t_tdm0k -= $map->{smoothf};
      $t_tdm0k->_vals->inplace->rint;
      $t_tdm0k->_missing(0);
      my $t_x1ak = $map->svdApply($t_tdm0k->xchg(0,1)->average->dummy(1,1)); ##-- copy since svdApply alters values in-place
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_x1ak)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_x1ak)."\n";
      dump_kbest($map, $t_x1ak, 10, "$tsym:$how");
    }

    if (0) {
      ##-- docre-apply : apply1 of tdm over all regex-selected docs : looks VERY REASONABLE
      ## + problems arise if selected term doesn't actually occur in any doc matching the regex (null vector)
      ##   - maybe fall back to group-average over doc vectors in this case?
      #
      # -- docre-apply(re=/\bmarx_/)
      # rerror(svd.v, docre-apply(re=/\bmarx_/) : term=Produktion ) = [ 10.487007]
      # rdist (svd.v, docre-apply(re=/\bmarx_/) : term=Produktion ) = [0.041779172]
      # Produktion:docre-apply(re=/\bmarx_/) [0.00326961800753545:1.50347557656497]
      #   [0]	0.00326961800753545	129291	Produktionsmittel
      #   [1]	0.00342129026690896	149004	kapitalistisch
      #   [2]	0.00477166762638459	115471	Kapitalist
      #   [3]	0.00482928181555564	116890	Produktionsproze??
      #   [4]	0.00598627542477803	69130	Mehrwert
      #   [5]	0.00822093306043403	98013	produktiv
      #   [6]	0.00857306770851585	100014	Kapital
      #   [7]	0.00873209619127735	148705	Arbeitskraft
      #   [8]	0.00882926153357166	162009	Akkumulation
      #   [9]	0.00901116404815061	13108	Produktionszweig
      ##
      # -- docre-apply(re=/\bsimmel_geld_1900/)
      # rerror(svd.v, docre-apply(re=/\bsimmel_geld_1900/) : term=Produktion ) = [ 62.247039]
      # rdist (svd.v, docre-apply(re=/\bsimmel_geld_1900/) : term=Produktion ) = [0.24870116]
      # Produktion:docre-apply(re=/\bsimmel_geld_1900/) [0.0663281523504333:1.56354595938608]
      #   [0]	0.0663281523504333	142495	Geldbesitz
      #   [1]	0.0914691581358091	142858	Zweckreihe
      #   [2]	0.100378799696184	55443	Wertgef??hl
      #   [3]	0.101263236297754	62919	geldwirtschaftlich
      #   [4]	0.103182936273932	133067	Entgegengesetztheit
      #   [5]	0.112283746424405	63301	Geldstoff
      #   [6]	0.114340863167315	70191	Wertung
      #   [7]	0.124375588656746	70091	Formung
      #   [8]	0.132919423532695	134566	Differenziertheit
      #   [9]	0.132973655319544	35653	Eigenwert
      ##
      # -- docre-apply(re=/\bmenger_volkswirtschaftslehre/)
      # rerror(svd.v, docre-apply(re=/\bmenger_volkswirtschaftslehre/) : term=Produktion ) = [ 63.311839]
      # rdist (svd.v, docre-apply(re=/\bmenger_volkswirtschaftslehre/) : term=Produktion ) = [0.34515962]
      # Produktion:docre-apply(re=/\bmenger_volkswirtschaftslehre/) [0.0895739234677774:1.44060374512241]
      #   [0]	0.0895739234677774	159227	Bodenben??tzung
      #   [1]	0.0909001105566937	158114	Unternehmert??tigkeit
      #   [2]	0.110942410122433	83889	Kapitalnutzung
      #   [3]	0.112288959302766	101490	Bodennutzung
      #   [4]	0.126824869692736	144804	Productes
      #   [5]	0.136210017443832	29212	wirtschaftend
      #   [6]	0.160225838491399	155907	verf??gbar
      #   [7]	0.167049727874011	48223	Bed??rfnisbefriedigung
      #   [8]	0.193323797575237	51605	Konkurrenzverh??ltnis
      #   [9]	0.201701112098627	160999	Preisbildung
      ##
      # -- docre-apply(re=/\bmangoldt_unternehmergewinn/)
      # rerror(svd.v, docre-apply(re=/\bmangoldt_unternehmergewinn/) : term=Produktion ) = [ 61.911685]
      # rdist (svd.v, docre-apply(re=/\bmangoldt_unternehmergewinn/) : term=Produktion ) = [0.16776621]
      # Produktion:docre-apply(re=/\bmangoldt_unternehmergewinn/) [0.106508863099801:1.46757942362968]
      #   [0]	0.106508863099801	119830	Kapitalkraft
      #   [1]	0.111554947595897	23980	Unternehmerrente
      #   [2]	0.111850416762876	31844	Unternehmergewinn
      #   [3]	0.112731035067973	111997	unternehmungsweise
      #   [4]	0.112871984455021	3982	Arbeitsrente
      #   [5]	0.113334237158507	65148	entwerten
      #   [6]	0.115077260598618	86905	Produktionsfaktor
      #   [7]	0.116422495800068	47211	Ausnutzer
      #   [8]	0.116528806065549	87384	Eigengesch??ft
      #   [9]	0.117941383880686	65880	Zinsrente
      my $docre = '\bmarx_';
      print "-- ".($how="docre-apply(re=/$docre/)")."\n";
      my $docre_di;
      if (defined(my $dtied=tied(@{$map->{denum}{id2sym}}))) {
	$docre_di = pdl(long, $$dtied->re2i(qr{$docre}));
      } else {
	$docre_di = pdl(long, @{$map->{denum}{sym2id}}[grep {$_ =~ m{qr{$docre}}} @{$map->{denum}{id2sym}}]);
      }
      my $t_wnd  = pdl(long,[$ti])->slice(",*".($docre_di->nelem))->glue(0,$docre_di->slice("*1,"));
      my $t_vals = $tdm->indexND($t_wnd)->append($tdm->missing);
      my $t_tdm  = PDL::CCS::Nd->newFromWhich($t_wnd, $t_vals, sorted=>1,steal=>1, dims=>[1,$tdm->dim(1)])->recode;  #- recode() should be optional
      (my $tmp = $t_wnd->slice("(0),")) .= 0;
      ##
      my $t_xdocre = $svd->apply1($t_tdm)->xchg(0,1);
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_xdocre)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_xdocre)."\n";
      dump_kbest($map, $t_xdocre, 10, "$tsym:$how");
    }

    if (0) {
      ##-- docre-apply3 : apply1 of tdm over all regex-selected docs : ==docre-apply : CURRENT
      my $docre = '\bmarx_'; #'_18[0-9][0-9]\.'; 
      print "-- ".($how="docre-apply3(re=/$docre/)")."\n";
      my $trace = sub { 0 || print STDERR "TRACE:", @_, "\n"; };
      my $docre_di;
      $trace->('re2i');
      if (defined(my $dtied=tied(@{$map->{denum}{id2sym}}))) {
	$docre_di = pdl($tdm->_whichND->type, $$dtied->re2i(qr{$docre}));
      } else {
	$docre_di = pdl($tdm->_whichND->type, @{$map->{denum}{sym2id}}[grep {$_ =~ m{qr{$docre}}} @{$map->{denum}{id2sym}}]);
      }

      #$trace->('wnd');
      #my $t_wnd = pdl(long,[$ti])->slice(",*".($docre_di->nelem))->glue(0,$docre_di->slice("*1,"));
      #my $t_wnd = pdl($tdm->_whichND->type,[$ti])->slice("*1,")->cat($docre_di)->clump(2)->xchg(0,1);
      #my $t_nzi = $t_wnd->vsearchvec($tdm->_whichND);
      #my $t_nzi_mask = ($t_wnd==$tdm->_whichND->dice_axis(1,$t_nzi))->andover;
      #$t_nzi         = $t_nzi->where($t_nzi_mask);
      #my $t_vals     = $tdm->_vals->index($t_nzi);
      #my $t_tdm  = PDL::CCS::Nd->newFromWhich($tdm->_whichND->dice_axis(1,$t_nzi), $t_vals->append($tdm->missing), sorted=>1,steal=>1, dims=>[1,$tdm->dim(1)]);
      #$t_tdm->sever;
      #(my $tmp=$t_tdm->_whichND->slice("(0),")) .= 0;
      ##
      #$trace->('xindex2d');
      #my $t_nzi1 = $tdm->_whichND->ccs_xindex2d(pdl($tdm->_whichND->type,[$ti]), $docre_di);
      #my $t_vals1 = $tdm->_vals->index($t_nzi1);
      #my $t_tdm  = PDL::CCS::Nd->newFromWhich($tdm->_whichND->dice_axis(1,$t_nzi1), $t_vals1->append($tdm->missing), sorted=>1,steal=>1, dims=>[1,$tdm->dim(1)]);
      ##
      $trace->('t_tdm');
      my $t_tdm = $tdm->xsubset2d(pdl($tdm->_whichND->type,[$ti]), $docre_di);
      $t_tdm->_whichND->sever;
      (my $tmp=$t_tdm->_whichND->slice("(0),")) .= 0;
      $t_tdm->setdims_p(1,$t_tdm->dim(1));

      $trace->('apply');
      my $t_xdocre = $svd->apply1($t_tdm)->xchg(0,1);

      $trace->('results');
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_xdocre)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_xdocre)."\n";
      dump_kbest($map, $t_xdocre, 10, "$tsym:$how");
    }

    if (1) {
      ##-- catre-apply : apply1 of tdm over all regex-selected cats : GOOD ==docre-apply (~CURRENT)
      # -- catre-apply(re=/\bmarx_/)
      # rerror(svd.v, catre-apply(re=/\bmarx_/) : term=Produktion ) = [ 10.487007]
      # rdist (svd.v, catre-apply(re=/\bmarx_/) : term=Produktion ) = [0.041779172]
      # Produktion:catre-apply(re=/\bmarx_/) [0.00326961800753545:1.50347557656497]
      #   [0]	0.00326961800753545	129291	Produktionsmittel
      #   [1]	0.00342129026690896	149004	kapitalistisch
      #   [2]	0.00477166762638459	115471	Kapitalist
      #   [3]	0.00482928181555564	116890	Produktionsproze??
      #   [4]	0.00598627542477803	69130	Mehrwert
      #   [5]	0.00822093306043403	98013	produktiv
      #   [6]	0.00857306770851585	100014	Kapital
      #   [7]	0.00873209619127735	148705	Arbeitskraft
      #   [8]	0.00882926153357166	162009	Akkumulation
      #   [9]	0.00901116404815061	13108	Produktionszweig
      my $catre = '\bmarx_'; #'_18[0-9][0-9]\.'; 
      print "-- ".($how="catre-apply(re=/$catre/)")."\n";
      my $trace = sub { 0 || print STDERR "TRACE:", @_, "\n"; };
      my $catre_ci;
      $trace->('re2i');
      if (defined(my $dtied=tied(@{$map->{lcenum}{id2sym}}))) {
	$catre_ci = pdl($tdm->_whichND->type, $$dtied->re2i(qr{$catre}));
      } else {
	$catre_ci = pdl($tdm->_whichND->type, @{$map->{lcenum}{sym2id}}[grep {$_ =~ m{qr{$catre}}} @{$map->{lcenum}{id2sym}}]);
      }

      $trace->('cati->doci');
      my $catre_di = $map->{dcm}->dice_axis(1,$catre_ci)->_whichND->slice("(0),");

      $trace->('t_tdm');
      my $t_tdm = $tdm->xsubset2d(pdl($tdm->_whichND->type,[$ti]), $catre_di);
      $t_tdm->_whichND->sever;
      (my $tmp=$t_tdm->_whichND->slice("(0),")) .= 0;
      $t_tdm->setdims_p(1,$t_tdm->dim(1));

      $trace->('apply');
      my $t_xcatre = $svd->apply1($t_tdm)->xchg(0,1);

      $trace->('results');
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_xcatre)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_xcatre)."\n";
      dump_kbest($map, $t_xcatre, 10, "$tsym:$how");
    }

    if (0) {
      ##-- docre-apply2 : apply1 of tdm over all regex-selected docs : ==docre-apply but SLOW (b/c of slow qsort on nzi for doc-subset)
      # -- docre-apply2(re=/\bmarx_/)
      # rerror(svd.v, docre-apply2(re=/\bmarx_/) : term=Produktion ) = [ 10.487007]
      # rdist (svd.v, docre-apply2(re=/\bmarx_/) : term=Produktion ) = [0.041779172]
      # Produktion:docre-apply2(re=/\bmarx_/) [0.00326961800753545:1.50347557656497]
      #   [0]	0.00326961800753545	129291	Produktionsmittel
      #   [1]	0.00342129026690896	149004	kapitalistisch
      #   [2]	0.00477166762638459	115471	Kapitalist
      #   [3]	0.00482928181555564	116890	Produktionsproze??
      #   [4]	0.00598627542477803	69130	Mehrwert
      #   [5]	0.00822093306043403	98013	produktiv
      #   [6]	0.00857306770851585	100014	Kapital
      #   [7]	0.00873209619127735	148705	Arbeitskraft
      #   [8]	0.00882926153357166	162009	Akkumulation
      #   [9]	0.00901116404815061	13108	Produktionszweig
      my $docre = '\bmarx_'; # '_18[0-4][0-9]\.';
      print "-- ".($how="docre-apply2(re=/$docre/)")."\n";
      my $docre_di;
      my $trace = sub { 0 || print STDERR "TRACE:", @_, "\n"; };
      $trace->('re2i');
      if (defined(my $dtied=tied(@{$map->{denum}{id2sym}}))) {
	$docre_di = pdl(long, $$dtied->re2i(qr{$docre}));
      } else {
	$docre_di = pdl(long, @{$map->{denum}{sym2id}}[grep {$_ =~ m{qr{$docre}}} @{$map->{denum}{id2sym}}]);
      }

      if (0) {
	my $cmp_intersect = sub {
		    my ($ptr0,$pix0)  = $tdm->ptr(0);
		    my ($ptr1,$pix1)  = $tdm->ptr(1);
		    my $t_nzi         = $pix0->index([$ptr0->ccs_decode_pointer($ti)]->[1]);
		    my $d_nzi         = $pix1->index([$ptr1->ccs_decode_pointer($docre_di)]->[1])->qsort;
		    my $td_nzi        = $t_nzi->v_intersect($d_nzi);
		  };
	my $cmp_indexnd = sub {
		    my $t_wnd = pdl(long,[$ti])->slice(",*".($docre_di->nelem))->glue(0,$docre_di->slice("*1,"));
		    my $t_ndi = $t_wnd->vsearchvec($tdm->_whichND);
		    my $t_ndi_mask = ($t_wnd==$tdm->_whichND->dice_axis(1,$t_ndi))->andover;
		    $t_ndi    = $t_ndi->where($t_ndi_mask);
		  };
	#             Rate intersect   indexnd
	# intersect 27.4/s        --      -98%
	# indexnd   1110/s     3957%        --
	#cmpthese(-1, {'intersect'=>$cmp_intersect, 'indexnd'=>$cmp_indexnd});
	##
	##
	my ($ptr0,$pix0)  = $tdm->ptr(0);
	my ($ptr1,$pix1)  = $tdm->ptr(1);
	my $t_nzi         = $pix0->index([$ptr0->ccs_decode_pointer($ti)]->[1]);
	my $d_nzi         = $pix1->index([$ptr1->ccs_decode_pointer($docre_di)]->[1]);
	my ($na,$nb)      = ($t_nzi->nelem, $d_nzi->nelem);
	print "na=$na , nb=$nb\n";
	# na=2973 , nb=221013
	#
	##-- v_intersect vs setops: with implicit qsort() on 1 argument
	# Rate           setops qsort+v_intersec
	# setops           15.5/s               --             -52%
	# qsort+v_intersec 32.1/s             108%               --
	cmpthese(-1, {"qsort+v_intersect"=>sub { my $td_nzi=$t_nzi->v_intersect($d_nzi->qsort) }, "setops"=>sub { my $td_nzi=$t_nzi->setops('AND',$d_nzi) }});
	##
	##-- v_intersect vs setops: pre-sorted
	#               Rate      setops v_intersect
	# setops      22.2/s          --        -98%
	# v_intersect  948/s       4165%          --
	$d_nzi->inplace->qsort;
	cmpthese(-1, {"v_intersect"=>sub { my $td_nzi=$t_nzi->v_intersect($d_nzi) }, "setops"=>sub { my $td_nzi=$t_nzi->setops('AND',$d_nzi) }});
      }
      $trace->("ptrs (nt=1,nd=".($docre_di->nelem).")");
      my ($ptr0,$pix0)  = $tdm->ptr(0);
      my ($ptr1,$pix1)  = $tdm->ptr(1);
      ##
      $trace->("nzi (intersect)");
      my $t_nzi         = $pix0->index([$ptr0->ccs_decode_pointer($ti)]->[1]);
      my $d_nzi         = $pix1->index([$ptr1->ccs_decode_pointer($docre_di)]->[1]);
      #my $td_nzi = $t_nzi->intersect($d_nzi);  ##-- "out of memory" for regex=/_18[0-9][0-9]\./, nd=238507 ; MUCH slower than vv_intersect or v_intersect
      #my $td_nzi = $t_nzi->slice("*1,")->vv_intersect($d_nzi->qsort->slice("*1,"))->flat; ##-- intersect is fast; qsort() is slow!
      my $td_nzi = $t_nzi->slice("*1,")->vv_intersect($d_nzi->qsort->slice("*1,"))->flat;  ##-- intersect is fast; qsort() is slow!
      ##
      $trace->("t_tdm");
      my $t_tdm  = PDL::CCS::Nd->newFromWhich($tdm->_whichND->dice_axis(1,$td_nzi),
					      $tdm->_vals->index($td_nzi)->append($tdm->missing),
					      sorted=>1, steal=>1, dims=>[1,$tdm->dim(1)]);
      ##
      $trace->('uisigma');
      $svd->uisigma;
      $trace->('apply');
      my $t_xdocre = $svd->apply1($t_tdm)->xchg(0,1);
      $trace->('error');
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_xdocre)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_xdocre)."\n";
      $trace->('kbest');
      dump_kbest($map, $t_xdocre, 10, "$tsym:$how");
    }

    if (0) {
      ##-- nodocre-apply : apply1 of tdm over all except regex-matching docs : looks QUITE GOOD (== svd.v)
      # -- nodocre-apply(re=/\bmarx_/)
      # rerror(svd.v, nodocre-apply(re=/\bmarx_/) : term=Produktion ) = [7.6684803e-07]
      # rdist (svd.v, nodocre-apply(re=/\bmarx_/) : term=Produktion ) = [5.9524482e-09]
      # Produktion:nodocre-apply(re=/\bmarx_/) [5.9524480899853e-09:1.47136365975534]
      #   [0]	5.9524480899853e-09	77231	Produktion
      #   [1]	0.0185670456237266	56722	Rohstoff
      #   [2]	0.0197835120256553	19431	produzieren
      #   [3]	0.026019693455971	1289	industriell
      #   [4]	0.0272573849144648	151136	??berproduktion
      #   [5]	0.0338051585913129	27006	Produzent
      #   [6]	0.0347821707402035	81227	Arbeitslohn
      #   [7]	0.0359238360458716	159765	Konsumtion
      #   [8]	0.0373877748013933	156536	Verkaufspreis
      #   [9]	0.0381755585914764	100014	Kapital
      my $docre = '\bmarx_';
      print "-- ".($how="nodocre-apply(re=/$docre/)")."\n";
      my $docre_di;
      if (defined(my $dtied=tied(@{$map->{denum}{id2sym}}))) {
	$docre_di = pdl(long, $$dtied->re2i(qr{$docre}));
      } else {
	$docre_di = pdl(long, @{$map->{denum}{sym2id}}[grep {$_ =~ m{qr{$docre}}} @{$map->{denum}{id2sym}}]);
      }
      my $t_tdm       = $tdm->dice_axis(0,$ti);
      my $t_wnd_docre = pdl(long,[$ti])->slice(",*".($docre_di->nelem))->glue(0,$docre_di->slice("*1,"));
      my $t_nzi_docre = $t_tdm->indexNDi($t_wnd_docre);
      (my $tmp=$t_tdm->_vals->index($t_nzi_docre)) .= $t_tdm->missing;
      $t_tdm->recode();
      ##
      my $t_xnodocre = $svd->apply1($t_tdm)->xchg(0,1);
      print "rerror(svd.v, $how : term=$tsym ) = ".rerrsum($t_x1, $t_xnodocre)."\n";
      print "rdist (svd.v, $how : term=$tsym ) = ".$map->qdistance($t_x1, $t_xnodocre)."\n";
      dump_kbest($map, $t_xnodocre, 10, "$tsym:$how");
    }

    exit 0;
  }

  ##-- test: k-best via queryVector() vs svdApply() : WONKY!
  foreach my $term (@_ ? @_ : 'Produktion') {
    my $q_sig   = $map->querySignature($term);
    my $q_v     = $map->queryVector($q_sig);
    my $q_tdm0  = $map->sigPdlRaw($q_sig, $map->{mapccs});
    my $q_tdm0d = $map->sigPdlRaw($q_sig, 0);
    my $q_xv0   = $map->svdApply($q_tdm0->pdl,0); ##-- copy since svdApply modifies values in-place

    #my $svd    = $map->{svd};
    #my $q_xv1  = $q_tdm0d x $svd->{v} x $svd->isigma;
    #my $q_xv2  = $q_tdm0->matmult2d_zdd($svd->{v}->matmult($svd->isigma));
    #my $q_xv3  = $q_tdm0->matmult2d_sdd($svd->{v}->matmult($svd->isigma),undef,$map->ccsSvdNil);
    #my $q_xv4  = $q_tdm0->matmult2d_sdd($svd->{v}->matmult($svd->isigma),undef,0);

    ##-- BUG: even after changing SVD code, we get q_xv0 != q_xv1 : why? --> contribution of ccsSvdNil (now honors $map->{applynil})
    dump_kbest($map, $q_v, 10, "$term:V");
    dump_kbest($map, $q_xv0, 10, "$term:XV0");
    #dump_kbest($map, $q_xv1, 10, "$term:XV1");
    #dump_kbest($map, $q_xv2, 10, "$term:XV2");
    #dump_kbest($map, $q_xv3, 10, "$term:XV3");
    #dump_kbest($map, $q_xv4, 10, "$term:XV4");
  }

  print STDERR "test_reflect(): done\n";
  exit 0;
}
#test_reflect(@ARGV);

##======================================================================
## MAIN (dummy)
foreach $i (1..3) {
  print STDERR "--dummy[$i]--\n";
}
