## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::LSI.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: document-to-class mapper: LSI / SVD

package DocClassify::Mapper::LSI;
use DocClassify::Mapper::ByLemma;
use DocClassify::Utils ':all';

use MUDL::Enum;
use MUDL::SVD;
use MUDL::Cluster::Distance;
use MUDL::Cluster::Distance::Builtin;
use MUDL::PDL::Stats;
use MUDL::PDL::Smooth;

use PDL;
use PDL::CCS::Nd;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Mapper::ByLemma);


#our $verbose = 2;
our $verbose = 3;

##==============================================================================
## Constructors etc.

## $map = $CLASS_OR_OBJ->new(%opts)
##  + defaults in constructor are obfuscated; see "real" program defaults in
##     DocClassify::Mapper::Train (and DocClassify::Program defaults)
## %$map, %opts:
##  ##
##  ##==== NEW in Mapper::LSI
##  ##-- options
##  svdr => $svdr,                   ##-- number of reduced dimensions (default=256)
##  catProfile => $how,              ##-- cate profiling method ('fold-in','fold-avg','average','weighted-average'...): default='average'
##  xn => $xn,                       ##-- number of splits for compile-time cross-check (0 for none; default=0)#
##  seed => $seed,                   ##-- random seed for corpus splitting (undef (default) for none)
##  #conf_nofp => $conf,              ##-- confidence level for negative-evidence parameter fitting (.95)
##  #conf_nofn => $conf,              ##-- confidence level for positive-evidence parameter fitting (.95)
##  mapccs => $bool,                 ##-- if true, map documents using sparse PDL::CCS::Nd (default=false)
##  ##
##  ##-- data: post-compile()
##  svd => $svd,                     ##-- a MUDL::SVD object
##  xdm => $xdm_pdl,                 ##-- dense PDL ($svdr,$ND) = $svd->apply( $tdm_pdl )
##  xcm => $xcm_pdl,                 ##-- dense PDL ($svdr,$NC) = $svd->apply( $TERM_CAT_MATRIX($NT,$NC) )
##  #dc_dist => $dc_dist,             ##-- dense PDL ($NDx,$NC) : [$dxi,$ci] -> dist($ci,$dxi)
##  #dc_d2c  => $dc_d2c,              ##-- dense PDL ($NDx)     : [$dxi]     -> $ci : $dxi \in $ci
##  #                                 ##   + NOTE: $NDx may be != $ND
##  #c1dist_mu => $c1dist_mu,         ##-- dense PDL ($NC) : [$ci] ->    avg { dist($ci,$di) : $di  \in $ci }
##  #c1dist_sd => $c1dist_sd,         ##-- dense PDL ($NC) : [$ci] -> stddev { dist($ci,$di) : $di  \in $ci }
##  #c0dist_mu => $c0dist_mu,         ##-- dense PDL ($NC) : [$ci] ->    avg { dist($ci,$di) : $di !\in $ci }
##  #c0dist_sd => $c0dist_sd,         ##-- dense PDL ($NC) : [$ci] -> stddev { dist($ci,$di) : $di !\in $ci }
##  ##
##  ##==== INHERITED from Mapper::ByLemma
##  ##-- options
##  verbose => $vlevel,              ##-- verbosity level (default=$verbose)
##  warnOnNullDoc => $bool,          ##-- do/don't warn about null docs (default=do)
##  lemmatize => \%opts,             ##-- options for $doc->typeSignature->lemmatize()
##  trainExclusive => $bool,         ##-- use each doc to train at most 1 cat? (default=true)
##  minFreq => $f,                   ##-- minimum global frequency f(t) for term-inclusion (default=0)
##  minDocFreq => $ndocs,            ##-- minimum number of docs with f(t,d)>0 for term-inclusion (default=0)
##  smoothf => $f0,                  ##-- global frequency smoothing constant (undef~(NTypes/NTokens); default=1.00001)
##  termWeight => $how,              ##-- term "weighting" method ('uniform', 'entropy'): default='entropy'
##  twRaw    => $wRaw,               ##-- coefficient for term raw-frequency (unweighted) in tdm, cdm (default=0)
##  twCooked => $wCooked,            ##-- coefficient for weighted term-frequency (weighted) in tdm, cdm (default=1)
##  cleanDocs => $bool,              ##-- whether to implicitly clean $doc->{sig} on train, map [default=true]
##  byCat => $bool,                  ##-- compile() tcm instead of tdm0, tdm? (default=0)
##  weightByCat => $bool,            ##-- compile() tw using tcm0 insteadm of tdm0? (default=0)
##  dist => $distSpec,               ##-- distance spec for MUDL::Cluster::Distance (default='u')
##                                   ##   + 'c'=Pearson, 'u'=Cosine, 'e'=Euclid, ...
##  nullCat => $catName,             ##-- cat name for null prototype (default='(auto)': use min id); enum name='(null)'; false for none
##  ##
##  ##-- data: enums
##  lcenum => $globalCatEnum,        ##-- local cat enum, compcat ($NC=$catEnum->size())
##  gcenum => $localCatEnum,         ##-- global cat enum         ($NCg=$globalCatEnum->size())
##  tenum => $termEnum,              ##-- term (lemma) enum       ($NT=$termEnum->size())
##  denum => $docEnum,               ##-- document (label) enum   ($ND=$docEnum->size()=scalar(@docs))
##  ##
##  ##-- data: training
##  gf => \%global_tf,               ##-- global term-frequency hash: ($term=>$freq, ...)
##  df => \%global_df,               ##-- global term-docfrequency hash: ($term=>$ndocs, ...)
##  docs   => \@docs,                ##-- training docs, indexed by local $docid ($ND=$docEnum->size()=scalar(@docs))
##  sigs   => \@sigs,                ##-- training sigs, indexed by local $docid
##  ##
##  ##-- data: post-compile()
##  disto => $distObj,               ##-- MUDL::Cluster::Distance object
##  dcm => $dcm_pdl,                 ##-- doc-cat matrix:  PDL::CCS::Nd ($ND,$NC): [$di,$ci] -> deg($di \in $ci)||0
##  tdm0=> $tdm0_pdl,                ##-- raw term-doc mx: PDL::CCS::Nd ($NT,$ND): [$ti,$di] ->     f($ti,$di)
##  tcm0=> $tcm0_pdl,                ##-- raw term-cat mx: PDL::CCS::Nd ($NT,$NC): [$ti,$ci] ->     f($ti,$ci)
##  tw0 => $tw0_pdl,                 ##-- raw tweight pdl: dense:       ($NT)    : [$ti]     -> w($ti)
##  tw  => $tw_pdl,                  ##-- term-weight pdl: dense:       ($NT)    : [$ti]     -> $wRaw + $wCooked*w($ti)
##  tdm => $tdm_pdl,                 ##-- term-doc matrix: PDL::CCS::Nd ($NT,$ND): [$ti,$di] -> log(f($ti,$di)+$f0)*w($ti)
##  tcm => $tcm_pdl,                 ##-- term-cat matrix: PDL::CCS::Nd ($NT,$NC): [$ti,$ci] -> log(f($ti,$ci)+$f0)*w($ti)
##  ##
##  xcm_sigma => $xcm_sigma,         ##-- cached stddev $xcm:      dense ($NC) : [$ci] -> stddev($xcm->[$ci])
##  xdm_sigma => $xdm_sigma,         ##-- cached stddev $xdm:      dense ($ND) : [$di] -> stddev($xdm->[$di])
##  xtm_sigma => $xtm_sigma,         ##-- cached stddev $svd->{v}: dense ($NT) : [$ti] -> stddev($xtm->[$ti])
sub new {
  my $that = shift;
  my $obj =  $that->SUPER::new(
			       ##-- options
			       svdr => 42,
			       catProfile => 'fold-in',
			       termWeight  => 'uniform',
			       seed => undef,
			       smoothf=>1,
			       xn => 0,
			       nullCat => '(auto)',
			       mapccs => 0,

			       ##-- data: post-compile
			       svd=>undef,
			       xdm=>undef,
			       xcm=>undef,

			       ##-- warnings
			       warnOnNullDoc => 1,

			       ##-- user args
			       @_,
			      );
  return $obj;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override appends qw(svd xdm xcm dc_dist dc_d2c c1dist_mu c1dist_sd c0dist_mu c0dist_sd cutoff)
sub noShadowKeys {
  return ($_[0]->SUPER::noShadowKeys(@_[1..$#_]),
	  qw(svd xdm xcm),
	  qw(dc_dist dc_d2c c1dist_mu c1dist_sd c0dist_mu c0dist_sd cutoff));
}

##==============================================================================
## Methods: API: Training
##  + see DocClassify::Mapper::LSITrain

##==============================================================================
## Methods: API: Compilation
##  + see DocClassify::Mapper::LSITrain

##==============================================================================
## Methods: API: Classification

## $corpus = $map->mapCorpus($corpus)
##  + Attempt to classify each $doc in $corpus,
##    destructively altering $doc->{cats} to reflect classification results.
##  + Inherited default implementation just calls $map->mapDocument() on each $doc in $corpus.

## $doc = $map->mapDocument($doc)
##  + attempt to classify $doc
##  + destructively alters $doc->{cats} to reflect classification results
sub mapDocument {
  my ($map,$doc) = @_;

  ##-- be verbose
  $map->vlog('trace', "mapDocument(".$doc->label.")") if ($map->{verbose}>=3);
  #$map->debug("mapDocument(".$doc->label."): [mapccs=".($map->{mapccs}||0)."]");

  ##-- sanity check(s)
  $map->logconfess("mapDocument(): no feature-category matrix 'xcm'!") if (!defined($map->{xcm}));

  ##-- get doc pdl
  my $tdm = $map->docPdlRaw($doc, $map->{mapccs});
  my $tdN = $tdm->sum;
  $map->logwarn("mapDocument(): null vector for document '$doc->{label}'")
    if ($map->{verbose} && $map->{warnOnNullDoc} && $tdN==0);

  ##-- compute distance to each centroid
  my $xdm   = $map->svdApply($tdm);
  #my $cd_dist = $map->{disto}->clusterDistanceMatrix(data=>$xdm,cdata=>$map->{xcm})->lclip(0);
  my $cd_dist = $map->qdistance($xdm,$map->{xcm})->lclip(0);

  ##-- convert distance to similarity
  my ($cd_sim);
  if (1 || !defined($map->{c1dist_mu})) {
    ##-- just invert $cdmat
    #$cd_sim = $cd_dist->max-$cd_dist;
    #$cd_sim  = 2-$cd_dist;
    $cd_sim = $cd_dist**-1;
  } else {
    ##-- OBSOLETE: use fit parameters to estimate similarity
    my $cd_cdf1 = gausscdf($cd_dist, $map->{c1dist_mu}, $map->{c1dist_sd});
    my $cd_cdf0 = gausscdf($cd_dist, $map->{c0dist_mu}, $map->{c0dist_sd});
    $cd_sim = F1( (1-li1($cd_cdf1)), (1-li1($cd_cdf0)), 1e-5);
  }
  $cd_sim->inplace->clip(0,1e38);

  ##-- dump similarities to $doc->{cats}
  my ($cname,$ename);
  @{$doc->{cats}} = map {
    $cname = $map->{lcenum}{id2sym}[$_];
    $ename = $cname eq '(null)' && $map->{nullCat} ? $map->{nullCat} : $cname;
    scalar({
	    sim=>$cd_sim->at($_,0),
	    dist_raw=>$cd_dist->at($_,0),
	    name=>$ename,
	    id  =>$map->{gcenum}{sym2id}{$ename},
	    ($ename ne $cname ? (proto=>$cname) : qw()),
	   })
  } $cd_sim->flat->qsorti->slice("-1:0")->list;
  $doc->{cats}[$_]{deg} = $_+1 foreach (0..$#{$doc->{cats}});

  return $doc;
}

##==============================================================================
## Methods: API: Query

## \@kbest = $map->mapQuery($querySignatureOrString,%opts)
## + %opts
##    mapto => $mapto,		  ##-- one of qw(cats docs terms)
##    kbest => $k,		  ##-- get $k nearest neighbors
##    minFreq => $minFreq,	  ##-- [mapto=>'terms'] post-filter; default=0
##    minDocFreq => $minDocFreq,  ##-- [mapto=>'terms'] post-filter; default=0
##    norm => $normHow,           ##-- normalization method qw(linear|cosine|vcosine|2 gaussian|normal none); default=none
## + returns @kbest = ([$id,$dist,$label],...)
## + side-effects:
##   - sets $q_sig->{drange_} = [$min,$max] if $querySigatureOrString is passed as a DocClassify::Signature and it's not already set
sub mapQuery {
  my ($map,$query,%opts) = @_;

  ##-- parse options
  my $mapto = $opts{mapto} || ($map->{lcenum}->size > 1 ? 'cats' : 'docs');

  ##-- parse query
  my $q_sig = UNIVERSAL::isa($query,'DocClassify::Signature') ? $query : $map->querySignature($query);
  my $q_str = $q_sig->{qstr_} // "$q_sig";

  ##-- pdl-ize query
  my $q_tdm0 = $map->sigPdlRaw($q_sig, $map->{mapccs});
  my $n_tdm0 = $q_tdm0->sum;
  my $qdocs  = pdl(long,$q_sig->{qdocs_}//[]);
  my $qcats  = pdl(long,$q_sig->{qcats_}//[]);
  $map->logwarn("mapQuery(): null vector for query-string '$q_str'")
    if ($map->{verbose}
	&& $map->{warnOnNullDoc}
	&& $n_tdm0==0
	&& $qdocs->isempty
	&& $qcats->isempty
       );

  ##-- retrieve doc and class sub-queries
  my $n_qsrc = $qdocs->nelem + $qcats->nelem + ($n_tdm0 ? 1 : 0);
  my $qq_xdm = $map->{xdm}->dice_axis(1,$qdocs);
  my $qq_xcm = $map->{xcm}->dice_axis(1,$qcats);

  ##-- target object dispatch
  my ($qx_dist,$qx_enum);
  if ($mapto =~ /^[dpcbv]/) {
    ##~~~~~~ mapto=(docs~pages|cats~books~volume): merge in qdocs_, qcats_ sub-queries
    my ($q_xdm);

    if (0) {
      ##-- treat query as a document: old DocClassify/VZ classification method
      $q_xdm = $map->svdApply($q_tdm0->pdl);
      if ($n_tdm0 > 0) { $q_xdm /= $n_qsrc; }
      else             { $q_xdm .= 0; }
    } else {
      ##-- treat query as weighted terms: term->term method (group-average query terms)
      my $xtm = $map->{svd}{v};
      my $q_w = $q_tdm0->_nzvals / $q_tdm0->_nzvals->sumover;
      $q_xdm  = ($xtm->dice_axis(1, ($q_tdm0->allmissing ? null : $q_tdm0->_whichND->slice("(0),"))) * $q_w->dummy(0,1))->xchg(0,1)->sumover->dummy(1,1);
      if (!$n_tdm0) { $q_xdm .= 0; }
    }
    $q_xdm += ($qq_xdm / $n_qsrc)->xchg(0,1)->sumover->dummy(1,1) if (!$qq_xdm->isempty);
    $q_xdm += ($qq_xcm / $n_qsrc)->xchg(0,1)->sumover->dummy(1,1) if (!$qq_xcm->isempty);

    if ($mapto =~ /^[dp]/i) {
      ##-- mapto=docs~pages: compute distance to each *document*
      $qx_dist = $map->qdistance($q_xdm, $map->{xdm}, sigma2=>$map->xdm_sigma);
      $qx_enum = $map->{denum};
    } else {
      ##-- mapto=cats~books~volumne: compute distance to each *centroid*
      $qx_dist = $map->qdistance($q_xdm, $map->{xcm}, sigma2=>$map->xcm_sigma);
      $qx_enum = $map->{lcenum};
    }
  }
  elsif ($mapto =~ /^[t]/i) {
    ##~~~~~~ mapto=terms

    ##-- get reduced (term x R) matrix
    my $xtm = $map->{svd}{v};

    ##-- group-average query terms
    my $q_w = $q_tdm0->_nzvals / $q_tdm0->_nzvals->sumover;
    my $xqm = ($xtm->dice_axis(1, ($q_tdm0->allmissing ? null : $q_tdm0->_whichND->slice("(0),"))) * $q_w->dummy(0,1))->xchg(0,1)->sumover->dummy(1,1);
    if (!$n_tdm0) { $xqm .= 0; }

    ##-- merge in qdocs_, qcats_ sub-queries
    $xqm += ($qq_xdm / $n_qsrc)->xchg(0,1)->sumover->dummy(1,1) if (!$qq_xdm->isempty);
    $xqm += ($qq_xcm / $n_qsrc)->xchg(0,1)->sumover->dummy(1,1) if (!$qq_xcm->isempty);

    ##-- distance guts :: EXPENSIVE (esp. sigma computation)
    #$map->debug("mapQuery(): distance guts");
    $qx_dist = $map->qdistance($xqm, $xtm, sigma2=>$map->xtm_sigma);
    $qx_enum = $map->{tenum};
    $q_sig->{drange_} //= [$qx_dist->minmax];

    ##-- apply result-filter mask
    if (($opts{minFreq}//0) > $map->{minFreq} || ($opts{minDocFreq}//0) > $map->{minDocFreq}) {
      my $mask = $map->get_tf0  < $opts{minFreq};
      $mask   |= $map->get_tdf0 < $opts{minDocFreq};
      (my $tmp=$qx_dist->where($mask)) += 'inf';
    }
  }

  ##-- set distance range
  $q_sig->{drange_} //= [$qx_dist->minmax];

  ##-- format distance+enum into k-best list
  return $map->kBestItems($qx_dist, $qx_enum, %opts);
}

##==============================================================================
## Methods: Misc

## $docpdl_ccs_missing = $map->ccsDocMissing()
##   + returns cached $map->{ccsDocMissing} if available
##   + otherwise computes & caches
##       $map->{ccsDocMissing} = $map->{tw}->average * log($map->{smoothf})
sub ccsDocMissing {
  return $_[0]{ccsDocMissing} if (defined($_[0]{ccsDocMissing}));
  return $_[0]{ccsDocMissing} = $_[0]{tw}->average * log($_[0]{smoothf});
}

## $docpdl_abnil = $map->ccsSvdNil()
##   + returns cached $map->{ccsSvdNil} if available
##   + otherwise computes & caches $map->{ccsSvdNil} from $map->{svd}, $map->ccsDocMissing()
sub ccsSvdNil {
  return $_[0]{ccsSvdNil} if (defined($_[0]{ccsSvdNil}));
  return $_[0]{ccsSvdNil} = $_[0]{svd}->apply0($_[0]->ccsDocMissing->squeeze->slice('*'.($_[0]{tw}->nelem).',*1'))->flat;
}

## $dxpdl = $map->svdApply($fpdl)
##  + input $fpdl is dense pdl($NT,1):     [$tid,0]     => f($tid,$doc)
##  + output $dxpdl is dense pdl(1,$svdr): [$svd_dim,0] => $svd_val
sub svdApply {
  my ($map,$fpdl) = @_;

  if ($map->{mapccs}) {
    ##-- ccs document-mapping mode
    $fpdl = $fpdl->toccs if (UNIVERSAL::isa($fpdl,'PDL') && !UNIVERSAL::isa($fpdl,'PDL::CCS::Nd'));  ##-- $fpdl passed as dense PDL?
    $fpdl = $map->sigPdlRaw($fpdl,1) if (!UNIVERSAL::isa($fpdl,'PDL::CCS::Nd')); ##-- $fpdl passed as doc?
    $fpdl = $fpdl->dummy(0,1) if ($fpdl->ndims != 2);
    $fpdl = $fpdl->make_physically_indexed();
    my $wnd  = $fpdl->_whichND;
    my $vals = $fpdl->_vals;
    $vals += $map->{smoothf};
    $vals->inplace->log;
    if (defined($map->{tw}) && $vals->nelem > 1) {
      (my $tmp=$vals->slice("0:-2")) *= $map->{tw}->index($wnd->slice("(0),")) ##-- apply term weights
    }
    $fpdl->missing($map->ccsDocMissing);                             ##-- approximate "missing" value
    return $map->{svd}->apply0($fpdl,$map->ccsSvdNil);               ##-- apply SVD
  }

  ##-- dense document-mapping mode
  $fpdl = $fpdl->todense if (UNIVERSAL::isa($fpdl,'PDL::CCS::Nd')); ##-- avoid memory explosion in Nd::inner()
  $fpdl = $map->sigPdlRaw($fpdl) if (!UNIVERSAL::isa($fpdl,'PDL')); ##-- $fpdl passed as doc?
  $fpdl = ($fpdl+$map->{smoothf})->log;
  $fpdl = $fpdl->slice(":,*1") if ($fpdl->ndims != 2);              ##-- ... someone passed in a flat term pdl...
  $fpdl *= $map->{tw} if (defined($map->{tw}));                     ##-- apply term-weights if available
  return $map->{svd}->apply0($fpdl);                                ##-- apply SVD
}

## $sig = $map->lemmaSignature($doc_or_sig)
##  + wrapper for $doc->termSignature->lemmatize() with options %{$map->{lemmatize}}
##  + inherited from Mapper::ByLemma

## $fpdl = $map->docPdlRaw($doc, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tidl,1]=>f($tid,$doc)
##  + inherited from Mapper::ByLemma

## $fpdl = $map->sigPdlRaw($sig, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tid,1]=>f($tid,$sig)
##  + inherited from Mapper::ByLemma

## $key_sigma = $map->pdl_sigma($key)
##  + wrapper for stddev computation or cache access
sub pdl_sigma {
  my ($map,$key,$pdl) = @_;
  return $map->{"${key}_sigma"} if (defined($map->{"${key}_sigma"}));
  (my $sigma = ($pdl//$map->{$key})->pow(2)->average)->inplace->sqrt;
  return $map->{"${key}_sigma"} = $sigma;
}

## $xcm_sigma = $map->xcm_sigma()
##  + compute or get cached stddev for $map->{xcm}
sub xcm_sigma {
  return $_[0]->pdl_sigma('xcm');
}

## $xdm_sigma = $map->xdm_sigma()
##  + compute or get cached stddev for $map->{xdm}
sub xdm_sigma {
  return $_[0]->pdl_sigma('xdm');
}

## $xtm_sigma = $map->xtm_sigma()
##  + compute or get cached stddev for $map->{xtm} --> $map->{svd}{v}
sub xtm_sigma {
  return $_[0]->pdl_sigma('xtm',$_[0]{svd}{v});
}

##==============================================================================
## Methods: API: I/O
##  + see DocClassify::Object

##==============================================================================
## Methods: API: I/O: Directory

## $bool = $map->saveDirData($dirname)
sub saveDirData {
  my ($map,$dir,%opts) = @_;

  ##-- save: inherited
  $map->SUPER::saveDirData($dir,%opts);

  ##-- save: pdls
  foreach (qw(xcm xdm)) { #tw0 tf0 tdf0
    $map->writePdlFile($map->{$_}, "$dir/$_.pdl", %opts);
  }

  ##-- save: caches
  foreach (qw(xcm_sigma xdm_sigma xtm_sigma)) {
    $map->writePdlFile($map->can($_)->($map), "$dir/$_.pdl", %opts);
  }

  ##-- save: svd
  $map->trace("save SVD $dir/svd.*") if ($opts{verboseIO});
  $map->{svd}->saveRawFiles("$dir/svd") if ($map->{svd});

  return 1;
}

## $map = $map->loadDirData($dirname,%opts)
##  + %opts
##    mmap => $bool,
sub loadDirData {
  my ($map,$dir,%opts) = @_;

  ##-- load: inherited
  $map->SUPER::loadDirData($dir,%opts);

  ##-- load: pdls
  ## "xcm" : "PDL"
  ## "xdm" : "PDL"
  foreach (qw(xcm xdm)) {
    $map->{$_} = $map->readPdlFile("$dir/$_.pdl",%opts,class=>'PDL');
  }

  ##-- load: caches
  foreach (qw(xcm_sigma xdm_sigma xtm_sigma)) {
    $map->{$_} = $map->readPdlFile("$dir/$_.pdl",%opts,class=>'PDL');
  }

  ##-- load: svd
  $map->trace("load SVD $dir/svd.* [mmap=".($opts{mmap}//0)."]") if ($opts{verboseIO});
  $map->{svd} = MUDL::SVD->loadRawFiles("$dir/svd",$opts{mmap})
    or $map->logconfess("loadDirData(): MUDL::SVD::loadRawFiles() failed for $dir/svd.*: $!");

  return $map;
}

##==============================================================================
## Methods: API: I/O: textdir

## $bool = $map->saveTextDirData($dirname)
sub saveTextDirData {
  my ($map,$dir,%opts) = @_;

  ##-- save: inherited
  $map->SUPER::saveTextDirData($dir,%opts);

  ##-- save: pdls
  foreach (qw(xcm xdm)) { #tw0 tf0 tdf0
    $map->writePdlTextFile($map->{$_}, "$dir/$_.txt", %opts);
  }

  ##-- save: caches
  foreach (qw(xcm_sigma xdm_sigma xtm_sigma)) {
    $map->writePdlTextFile($map->can($_)->($map), "$dir/$_.txt", %opts);
  }

  ##-- save: svd
  if ($map->{svd}) {
    $map->trace("save SVD $dir/svd.*") if ($opts{verboseIO});
    $map->{svd}->saveJsonFile("$dir/svd.json");
    foreach (qw(u sigma v)) {
      $map->writePdlTextFile($map->{svd}{$_}, "$dir/svd.$_.txt", %opts);
    }
  }

  return 1;
}

## $map = $map->loadTextDirData($dirname,%opts)
##  + %opts: none
sub loadTextDirData {
  my ($map,$dir,%opts) = @_;

  ##-- load: inherited
  $map->SUPER::loadTextDirData($dir,%opts);

  ##-- load: pdls
  foreach (qw(xcm xdm)) {
    $map->{$_} = $map->readPdlTextFile("$dir/$_.txt", %opts);
  }

  ##-- load: caches
  foreach (qw(xcm_sigma xdm_sigma xtm_sigma)) {
    $map->{$_} = $map->readPdlTextFile("$dir/$_.txt", %opts);
  }

  ##-- load: svd
  $map->trace("load SVD $dir/svd.*") if ($opts{verboseIO});
  $map->{svd} = MUDL::SVD->loadJsonFile("$dir/svd.json");
  foreach (qw(u sigma v)) {
    $map->{svd}{$_} = $map->readPdlTextFile("$dir/svd.$_.txt", %opts);
  }

  return $map;
}

##==============================================================================
## Footer
1;

__END__
