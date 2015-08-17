## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::ByLemma.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: document classifier: document-to-class mapper: by lemma

package DocClassify::Mapper::ByLemma;
#use DocClassify::Train::ByLemma;
use DocClassify::Mapper;
use DocClassify::Lemmatizer;
use DocClassify::Utils ':all';

use MUDL::Enum;
#use MUDL::SVD;
use MUDL::Cluster::Distance;
use MUDL::Cluster::Distance::Builtin;

use PDL;
use PDL::CCS::Nd;
use PDL::VectorValued;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Mapper);


#our $verbose = 2;
our $verbose = 3;

##==============================================================================
## Constructors etc.

## $map = $CLASS_OR_OBJ->new(%opts)
##  + defaults in constructor are obfuscated; see "real" program defaults in
##     DocClassify::Mapper::Train (and DocClassify::Program defaults)
## %$map, %opts:
##  ##-- options
##  verbose => $vlevel,              ##-- verbosity level (default=$verbose)
##  saveMem => $bool,                ##-- save memory by using disk files for training temporaries? (default=0)
##  lzClass => $lzClass,             ##-- lemmatizer class (see DocClassify::Lemmatizer::new(); default='default')
##  lzOpts => \%lzOpts,              ##-- options for $lzClass->new();
##  trainExclusive => $bool,         ##-- use each doc to train at most 1 cat? (default=true)
##  minFreq => $f,                   ##-- minimum global frequency f(t) for term-inclusion (default=0)
##  minDocFreq => $ndocs,            ##-- minimum number of docs with f(t,d)>0 for term-inclusion (default=0)
##  maxTermsPerDoc => $nterms,       ##-- maximum number of terms per document (0~no max (default))
##  smoothf => $f0,                  ##-- global frequency smoothing constant (undef~(NTypes/NTokens); default=1)
##  termWeight => $how,              ##-- term "weighting" method: one of:
##                                   ##    'uniform'                 ##-- w($t) = 1; aka 'no','id'
##                                   ##    'max-entropy-quotient'    ##-- w($t) = 1 - H(Doc|T=$t) / H_max(Doc); aka 'Hmax'
##                                   ##    'entropy-quotient'        ##-- w($t) = H(Doc|T=$t) / H(Doc); aka 'Hq'
##                                   ##    'conditional-entropy'     ##-- w($t) = H(Doc|T=$t); aka 'Hc'
##                                   ##    'entropy'                 ##-- alias for 'max-entropy-quotient' (default); aka 'H'
##  twRaw    => $wRaw,               ##-- coefficient for raw term log-frequency in tw (default=0)
##  twCooked => $wCooked,            ##-- coefficient for estimated term weight in tw (default=1)
##  cleanDocs => $bool,              ##-- whether to implicitly clean $doc->{sig} on train, map [default=true]
##  byCat => $bool,                  ##-- compile() tcm instead of tdm0, tdm? (default=0)
##  weightByCat => $bool,            ##-- compile() tw using tcm0 insteadm of tdm0? (default=1)
##  dist => $distSpec,               ##-- distance spec for MUDL::Cluster::Distance (default='u')
##                                   ##   + 'c'=Pearson, 'u'=Cosine, 'e'=Euclid, ...
##  nullCat => $catName,             ##-- cat name for null prototype (default=undef (none)); enum name='(null)'; '(auto)': use min id; false for none
##  ##
##  ##-- data: enums
##  lcenum => $globalCatEnum,        ##-- local cat enum, compcat ($NC=$catEnum->size())
##  gcenum => $localCatEnum,         ##-- global cat enum         ($NCg=$globalCatEnum->size())
##  tenum => $termEnum,              ##-- term (lemma) enum       ($NT=$termEnum->size())
##  denum => $docEnum,               ##-- document (label) enum   ($ND=$docEnum->size()=scalar(@docs))
##  #docids => $docIdPdl,             ##-- document id subset: pdl($ND_local): [$doc_pdl_index] -> $docid_denum  ##-- OBSOLETE (support removed in v0.16)
##  ##
##  ##-- data: training
##  lz => $lemmatizer,               ##-- DocClassify::Lemmatizer object
##  gf => \%global_tf,               ##-- global term-frequency hash: ($term=>$freq, ...)
##  df => \%global_df,               ##-- global term-docfrequency hash: ($term=>$ndocs, ...)
##  docs   => \@docs,                ##-- training docs, indexed by local $docid ($ND=$docEnum->size()=scalar(@docs))
##  sigs   => \@sigs,                ##-- training sigs, indexed by local $docid
##  ##
##  ##-- data: post-compile()
##  disto => $distObj,               ##-- MUDL::Cluster::Distance object
##  dcm => $dcm_pdl,                 ##-- doc-cat matrix:  PDL::CCS::Nd ($ND,$NC): [$di,$ci] -> deg($di \in $ci) || 0
##  tdm0=> $tdm0_pdl,                ##-- raw term-doc mx: PDL::CCS::Nd ($NT,$ND): [$ti,$di] ->     f($ti,$di)
##  tcm0=> $tcm0_pdl,                ##-- raw term-cat mx: PDL::CCS::Nd ($NT,$NC): [$ti,$ci] ->     f($ti,$ci)
##  tw0 => $tw0_pdl,                 ##-- raw tweight pdl: dense:       ($NT)    : [$ti]     -> w($ti)
##  tw  => $tw_pdl,                  ##-- term-weight pdl: dense:       ($NT)    : [$ti]     -> $wRaw + $wCooked*w($ti)
##  #tf0 => $tf0_pdl,                ##-- raw term-freq  : dense:       ($NT)    : [$ti]     -> f($ti)         : see get_tf0()
##  #tdf0=> $tdn0_pdl,               ##-- raw term-dfreq : dense:       ($NT)    : [$ti]     -> ndocs($ti)     : "doc-frequency", see get_tdf0()
##  tdm => $tdm_pdl,                 ##-- term-doc matrix: PDL::CCS::Nd ($NT,$ND): [$ti,$di] -> log(f($ti,$di)+$f0)*w($ti)
##  tcm => $tcm_pdl,                 ##-- term-cat matrix: PDL::CCS::Nd ($NT,$NC): [$ti,$ci] -> log(f($ti,$ci)+$f0)*w($ti)
##  ##
##  ##-- data: compile() caches
##  #doc_wt => \@doc_wt,              ##-- doc term indices: $di => pdl($NnzDocI) : [$nzi] -> $ti : f($ti,$di)>0  -- UNUSED in v>=0.16
sub new {
  my $that = shift;
  my $obj =  $that->SUPER::new(
			       ##-- options
			       verbose=>$verbose,
			       saveMem => 0,
			       lzClass => 'default',
			       lzOpts  => {},
			       trainExclusive => 1,
			       minFreq =>10,
			       minDocFreq =>4,
			       maxTermsPerDoc =>0,

			       smoothf =>1,
			       termWeight  => 'uniform',
			       twRaw => 0,
			       twCooked => 1,
			       cleanDocs => 1,
			       byCat => 0,
			       weightByCat => 0,
			       dist => 'u',
			       nullCat => undef,

			       ##-- data: enums
			       lcenum => MUDL::Enum->new,
			       gcenum => MUDL::Enum->new,
			       tenum => MUDL::Enum->new,
			       denum => MUDL::Enum->new,

			       ##-- data: training
			       gf => {},
			       df => {},
			       docs => [],
			       sigs => [],

			       ##-- data: post-compile
			       disto=>undef,
			       dcm=>undef,
			       tw=>undef,
			       tw0=>undef,
			       tdm0=>undef,
			       tcm0=>undef,
			       tdm=>undef,
			       tcm=>undef,

			       ##-- data: compile caches
			       doc_wt=>undef,

			       ##-- user args
			       @_,
			      );

  return $obj;
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(gf df lcenum denum tenum docs sigs dcm tw tdm0 tcm0 tdm tcm doc_wt disto)
sub noShadowKeys {
  return qw(gf df clenum denum tenum docs sigs dcm tw tdm0 tcm0 tf0 td0 tw0 tdm tcm doc_wt disto);
}

##==============================================================================
## Methods: API: Training
##  + see DocClassify::Mapper::ByLemmaTrain

##==============================================================================
## Methods: API: Compilation
##  + see DocClassify::Mapper::ByLemmaTrain

##==============================================================================
## Methods: API: Classification

## $corpus = $map->mapCorpus($corpus)
##  + Attempt to classify each $doc in $corpus,
##    destructively altering $doc->{cats} to reflect classification results.
##  + Inherited default implementation just calls $map->mapDocument() on each $doc in $corpus.

## $doc = $map->mapDocument($doc)
##  + attempt to classify $doc by matching with $map->{tcm}
##  + destructively alters $doc->{cats} to reflect classification results
sub mapDocument {
  my ($map,$doc) = @_;

  ##-- be verbose
  $map->vlog('trace', "mapDocument(".$doc->label.")") if ($map->{verbose}>=3);

  ##-- sanity check(s)
  $map->logconfess("mapDocument(): no term-category matrix 'tcm'!") if (!defined($map->{tcm}));

  ##-- get centroid pdl (force-decode if required)
  my $tcm = $map->{tcm};
  $tcm    = $map->{tcm} = $tcm->todense if (ref($tcm) ne 'PDL');

  ##-- get doc pdl
  my $tdm = $map->docPdlRaw($doc);
  my $tdN = $tdm->sum;
  if ($map->{verbose} && $tdN==0) {
    $map->logwarn("mapDocument(): null vector for document '$doc->{label}'");
  }

  ##-- compute distance to each centroid
  my $cd_dist = $map->{disto}->clusterDistanceMatrix(data=>$tdm,cdata=>$tcm)->lclip(0);

  ##-- convert distance to similarity
  my ($cd_sim);
  if (!defined($map->{c1dist_mu})) {
    ##-- just invert $cdmat
    #$cd_sim = $cd_dist->max-$cd_dist;
    #$cd_sim  = 2-$cd_dist;
    $cd_sim = $cd_dist**-1;
  } else {
    ##-- use fit parameters to estimate similarity
    my $cd_cdf1 = gausscdf($cd_dist, $map->{c1dist_mu}, $map->{c1dist_sd});
    my $cd_cdf0 = gausscdf($cd_dist, $map->{c0dist_mu}, $map->{c0dist_sd});
    $cd_sim = F1( (1-li1($cd_cdf1)), (1-li1($cd_cdf0)), 1e-5);
  }
  $cd_sim->inplace->clip(0,1e38);

  ##-- dump similarities to $doc->{cats}
  my $cname;
  @{$doc->{cats}} = map {
    $cname = $map->{lcenum}{id2sym}[$_];
    {id=>$map->{gcenum}{sym2id}{$cname}, name=>$cname, sim=>$cd_sim->at($_,0), dist_raw=>$cd_dist->at($_,0)}
  } $cd_sim->flat->qsorti->slice("-1:0")->list;
  $doc->{cats}[$_]{deg} = $_+1 foreach (0..$#{$doc->{cats}});

  return $doc;
}

##==============================================================================
## Methods: API: Query: Utils

## $q_sig = $map->querySignature(@query_strings)
##  + also sets @$q_sig{qw(qstr_ qdocs_ qcats_)}
sub querySignature {
  my $map   = shift;
  my $qstr  = join(' ',map {utf8::is_utf8($_) ? $_ : Encode::decode_utf8($_)} @_);

  ##-- variables
  my $qf    = {};
  my $qn    = 0;
  my @docs  = qw();
  my @cats  = qw();
  my ($which,$t,$f, $xarg,$xid,$xre,@xsyms);
  foreach (split(/[\,\s\;]+/,$qstr)) {
    ($which,$t,$f) = /^(?i:(doc|term|class|cls|cat|page|pag|book|bk|volume|vol)[:=])?(.+?)(?:\:([0-9eE\+\-]+))?$/ ? ($1,$2,$3) : ('term',$_,1);
    $which ||= 'term';
    $f     ||= 1;

    if ($which =~ /^[dp]/i) {
      ##-- doc=LABEL_OR_REGEX : add a document (aliases: page, pag)
      if (defined($xid = $map->{denum}{sym2id}{$t})) {
	##-- add a document given full label
	push(@docs,$xid);
      } else {
	##-- approximate search for doc-label regex: add all matches
	$xre   = qr{$t};
	@xsyms = grep {($_//'') =~ $xre} @{$map->{denum}{id2sym}};
	$map->logwarn("querySignature(): no documents found matching m/$xre/ - skipping") if (!@xsyms);
	push(@docs,grep {defined($_)} @{$map->{denum}{sym2id}}{@xsyms});
      }
    }
    elsif ($which =~ /^[cbv]/i) {
      ##-- cat=LABEL_OR_REGEX : add a category (aliases: book, bk, volume, vol)
      if (defined($xid = $map->{lcenum}{sym2id}{$t})) {
	##-- add a single cat given exact label match
	push(@cats,$xid);
      } else {
	##-- approximate search for cat-label regex: add all matches
	$xre   = qr{$t};
	@xsyms = grep {($_//'') =~ $xre} @{$map->{lcenum}{id2sym}};
	$map->logwarn("querySignature(): no categories found matching m/$xre/ - skipping") if (!@xsyms);
	push(@cats,grep {defined($_)} @{$map->{lcenum}{sym2id}}{@xsyms});
      }
    }
    else {
      ##-- "normal" TERM:FREQ query
      $qf->{$t} += $f;
      $qn       += $f;
    }
  }
  my $q_sig = DocClassify::Signature->new(tf=>$qf,lf=>$qf,N=>0,Nl=>$qn,
					  qstr_=>$qstr,
					  qdocs_=>\@docs,
					  qcats_=>\@cats,
					 );
  return $q_sig;
}

## \@kbest = $map->kBestItems($dist_pdl, $obj_enum, %opts)
##  + gets k-best indices from $dist_pdl as labelled by $obj_enum
##  + suitable for use by mapQuery()
##  + %opts:
##     k    => $k,                 ##-- get $k best items (default=1)
##     norm => $normHow,           ##-- normalization method qw((linear|cosine|vcosine)|(gaussian|normal)|none); default=none
##  + returns @kbest = ({id=>$id,dist=>$dist,label=>$label},...)
sub kBestItems {
  my ($map,$dist,$enum,%opts) = @_;

  ##-- normalize $qx_dist
  if ($opts{norm} && $opts{norm} =~ /^(?:[lvc2])/i) {
    $dist->inplace->divide(2,0);
  }
  elsif ($opts{norm} && $opts{norm} =~ /^(?:g|nor)/i) {
    $dist = _gausscdf($dist,$dist->average,$dist->stddev);
  }

  ##-- get k-best items
  my @kbest = qw();
  $dist     = $dist->flat;
  my $k     = $opts{k} || 1;
  if ($k == 1) {
    ##-- special case for k=1
    my $i = $dist->minimum_ind->sclr;
    @kbest = ( {id=>$i, dist=>$dist->at($i), label=>$enum->{id2sym}[$i]} );
  }
  else {
    ##-- general case for k-best
    my $xi = $dist->qsorti;
    foreach ($xi->slice("0:".($k-1))->list) {
      push(@kbest, {id=>$_, dist=>$dist->at($_), label=>$enum->{id2sym}[$_]});
    }
  }
  return \@kbest;
}

##==============================================================================
## Methods: Misc

## $dist_pdl = $map->qdistance($data,$cdata,%opts)
##  + like $map->{disto}->clusterDistanceMatrix(data=>$data, cdata=>$cdata, %opts)
##    but uses DocClassify::Utils::_vcos() if appropriate (faster)
sub qdistance {
  my ($map,$data,$cdata,%opts) = @_;
  return _vcos($data,$cdata,%opts)
    if (!defined($opts{mask}) && !defined($opts{weight})
	&& ((UNIVERSAL::isa($map->{disto},'MUDL::Cluster::Distance::Builtin') && ($map->{disto}{distFlag}//'') eq 'u')
	    || UNIVERSAL::isa($map->{disto},'MUDL::Cluster::Distance::Cosine')
	   ));
  return $map->{disto}->clusterDistanceMatrix(data=>$data, cdata=>$cdata, %opts);
}

## $lz = $map->lemmatizer()
##  + gets or creates $map->{lz}
sub lemmatizer {
  return $_[0]{lz} if (defined($_[0]{lz}));
  return $_[0]{lz} = DocClassify::Lemmatizer->new(%{$_[0]{lzOpts}},class=>$_[0]{lzClass});
}

## $sig = $map->lemmaSignature($doc_or_sig)
##  + wrapper for $map->lemmatizer->lemmatize($doc_or_sig->typeSignature)
sub lemmaSignature {
  my ($map,$sig) = @_;
  $sig = $sig->typeSignature if (!UNIVERSAL::isa($sig,'DocClassify::Signature'));
  $sig = ($map->{lz}||$map->lemmatizer)->lemmatize($sig) if (!$sig->lemmatized);
  return $sig;
}

## $fpdl = $map->docPdlRaw($doc, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tidl,1]=>f($tid,$doc)
sub docPdlRaw {
  return $_[0]->sigPdlRaw(@_[1..$#_]);
}

## $fpdl = $map->sigPdlRaw($sig_or_doc, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tid,1]=>f($tid,$sig)
sub sigPdlRaw {
  my ($map,$sig_or_doc, $as_ccs) = @_;
  my $sig = $map->lemmaSignature($sig_or_doc); ##-- ensure lemmatized signature
  my $tenum = $map->{tenum};
  my $dtf_wt = pdl(long,   [grep{defined($_)} @{$tenum->{sym2id}}{keys(%{$sig->{lf}})}]);
  my $dtf_nz = pdl(double, [@{$sig->{lf}}{@{$tenum->{id2sym}}[$dtf_wt->list]}]);
  if ($map->{cleanDocs} || !exists($map->{cleanDocs})) {
    ##-- cleanup
    #$sig->unlemmatize;
    $sig_or_doc->clearCache() if ($sig_or_doc->can('clearCache'));
  }
  if ($as_ccs) {
    ##-- ccs mode
    return PDL::CCS::Nd->newFromWhich($dtf_wt->dummy(0,1),$dtf_nz,dims=>pdl(long,[$tenum->size]),missing=>0)->dummy(1,1);
  }
  ##-- dense mode
  my $dtf = zeroes(double,$tenum->size);
  (my $tmp=$dtf->index($dtf_wt)) .= $dtf_nz;
  return $dtf->slice(",*1");
}

##==============================================================================
## Methods: API: I/O
##  + see DocClassify::Object

##==============================================================================
## Methods: API: I/O: Directory

##--------------------------------------------------------------
## Methods: I/O: Directory: save

## @keys = $obj->dirHeaderKeys()
##  + keys for header save
sub dirHeaderKeys {
  my $obj = shift;
  return ($obj->SUPER::dirHeaderKeys(), qw(lzOpts));
}

## $bool = $map->saveDirData($dirname,%opts)
##  + %opts:
##      saveEnums => $bool,   ##-- save enums? (default=1)
##      saveTdmPtr0 => $bool, ##-- save tdm ptr0? (default=0)
##      saveTdmPtr1 => $bool, ##-- save tdm ptr0? (default=0)
sub saveDirData {
  my ($map,$dir,%opts) = @_;

  ##-- DEBUG: save reference map
  #$map->writeJsonFile({map {($_=>ref($map->{$_}))} grep {ref($map->{$_})} keys %$map}, "$dir/refs.json");

  ##-- save: enums
  # "denum" : "MUDL::Enum"
  # "gcenum" : "MUDL::Enum"
  # "lcenum" : "MUDL::Enum"
  # "tenum" : "MUDL::Enum"
  if (!exists($opts{saveEnums}) || $opts{saveEnums}) {
    foreach my $ekey (qw(denum gcenum lcenum tenum)) {
      if (!defined($map->{$ekey})) {
	foreach (grep {-e $_} map {"$dir/$ekey.$_"} qw(hdr es eix esx)) {
	  $map->trace("unlinking enum file(s) $dir/$ekey.*") if ($opts{verboseIO});
	  unlink($_)
	    or $map->logconfess("saveDirData(): failed to unlink enum file $_: $!");
	}
      } else {
	$map->trace("saving enum file(s) $dir/$ekey.*") if ($opts{verboseIO});
	$map->{$ekey}->saveRawFiles("$dir/$ekey")
	  or $map->logconfess("saveDirData(): failed to save enum file(s) $dir/$ekey.*: $!")
	}
    }
  }

  ##-- save: ccs
  ## "dcm" : "PDL::CCS::Nd"
  ## "tdm" : "PDL::CCS::Nd"
  ## -"tdm0" : "PDL::CCS::Nd" : recoverable as ($tdm/$tw)->exp - $smoothf
  foreach (qw(dcm tdm)) { #tdm0
    $map->writePdlFile($map->{$_}, "$dir/$_.ccs", %opts);
  }

  ##-- save: pdls
  ## "tw" : "PDL"
  ## -"tw0" : "PDL"
  ## -?tf0
  ## -?tdf0
  ## -?dc_dist
  foreach (qw(tw)) { #tw0 tf0 tdf0
    $map->writePdlFile($map->{$_}, "$dir/$_.pdl", %opts);
  }
  if (defined($map->{tdm}) && $opts{saveTdmPtr0}) {
    my ($p,$pix) = $map->{tdm}->getptr(0);
    $map->writePdlFile($p,   "$dir/tdm.ptr0.pdl", %opts);
    $map->writePdlFile($pix, "$dir/tdm.pix0.pdl", %opts);
  }
  if (defined($map->{tdm}) && $opts{saveTdmPtr1}) {
    my ($p,$pix) = $map->{tdm}->getptr(1);
    $map->writePdlFile($p,   "$dir/tdm.ptr1.pdl", %opts);
    $map->writePdlFile($pix, "$dir/tdm.pix1.pdl", %opts);
  }

  ##-- save: misc
  ## "disto" : "MUDL::Cluster::Distance::Builtin"
  ## "lz" : "DocClassify::Lemmatizer::Cab"
  ## "lzOpts" : "HASH" -> in global header.json
  ## "svd" : "MUDL::SVD" --> LSI.pm
  $map->{lz}->saveDirHeader("$dir/lz.json", %opts) if ($map->{lz});
  $map->{disto}->saveJsonFile("$dir/disto.json") if ($map->{disto});

  ##-- ?save: training temps
  # -"df" : "HASH"
  # -"doc_wt" : "ARRAY"
  # -"docs" : "ARRAY"
  # -"gf" : "HASH"
  # -"sigs" : "ARRAY"

  return 1;
}

##--------------------------------------------------------------
## Methods: I/O: Directory: load

## $obj = $obj->loadDirData($dirname,%opts)
##  + %opts:
##     mmap => $bool,       ##-- mmap pdls (default=0)
sub loadDirData {
  my ($map,$dir,%opts) = @_;

  ##-- load: enums
  foreach my $ekey (qw(denum gcenum lcenum tenum)) {
    if (-e "$dir/$ekey.hdr") {
      $map->trace("loading enum $dir/$ekey \[mmap=".($opts{mmap}//0)."]") if ($opts{verboseIO});
      $map->{$ekey} = MUDL::Enum->loadRawFiles("$dir/$ekey", mmap=>$opts{mmap})
	or $map->logconfess("loadDirData(): failed to load enum file(s) $dir/$ekey.*: $!");
    } else {
      delete $map->{$ekey};
    }
  }

  ##-- load: ccs
  ## "dcm" : "PDL::CCS::Nd"
  ## "tdm" : "PDL::CCS::Nd"
  ## -"tdm0" : "PDL::CCS::Nd" : recoverable as ($tdm/$tw)->exp - $smoothf
  foreach (qw(dcm tdm)) { #tdm0
    $map->{$_} = $map->readPdlFile("$dir/$_.ccs", %opts, class=>'PDL::CCS::Nd');
  }
  if (defined($map->{tdm}) && -e "$dir/tdm.ptr0.pdl") {
    my $p   = $map->readPdlFile("$dir/tdm.ptr0.pdl", %opts);
    my $pix = $map->readPdlFile("$dir/tdm.pix0.pdl", %opts);
    $map->{tdm}->setptr(0,[$p,$pix]);
  }
  if (defined($map->{tdm}) && -e "$dir/tdm.ptr1.pdl") {
    my $p   = $map->readPdlFile("$dir/tdm.ptr1.pdl", %opts);
    my $pix = $map->readPdlFile("$dir/tdm.pix1.pdl", %opts);
    $map->{tdm}->setptr(1,[$p,$pix]);
  }

  ##-- load: pdls
  ## "tw" : "PDL"
  ## -"tw0" : "PDL"
  ## -?tf0
  ## -?tdf0
  ## -?dc_dist
  foreach (qw(tw)) { #tw0 tf0 tdf0
    $map->{$_} = $map->readPdlFile("$dir/$_.pdl", %opts, class=>'PDL');
  }

  ##-- load: misc
  ## "disto" : "MUDL::Cluster::Distance::Builtin"
  ## "lz" : "DocClassify::Lemmatizer::Cab"
  ## "lzOpts" : "HASH" -> in global header.json
  ## "svd" : "MUDL::SVD" --> LSI.pm
  $map->{lz} = DocClassify::Lemmatizer->loadDirHeader("$dir/lz.json", %opts)
    or $map->logconfess("loadDirData(): failed to load lemmatizer from $dir/lz.json: $!");
  $map->{disto} = MUDL::Cluster::Distance->loadJsonFile("$dir/disto.json")
    or $map->logconfess("loadDirData(): failed to load distance object from $dir/disto.json: $!");

  ##-- ?save: training temps
  # -"df" : "HASH"
  # -"doc_wt" : "ARRAY"
  # -"docs" : "ARRAY"
  # -"gf" : "HASH"
  # -"sigs" : "ARRAY"

  return $map;
}

##==============================================================================
## Methods: API: I/O: textdir

##--------------------------------------------------------------
## Methods: I/O: textdir: save

## $bool = $map->saveTextDirData($dirname,%opts)
sub saveTextDirData {
  my ($map,$dir,%opts) = @_;

  ##-- save: enums
  foreach my $ekey (qw(denum gcenum lcenum tenum)) {
    if (!defined($map->{$ekey})) {
      $map->trace("unlinking enum file $dir/$ekey.txt") if ($opts{verboseIO});
      !-e "$dir/$ekey.txt"
	or unlink("$dir/$ekey.txt")
	  or $map->logconfess("saveTextDirData(): failed to unlink enum file $dir/$ekey.txt: $!");
    } else {
      $map->trace("saving enum file $dir/$ekey.txt") if ($opts{verboseIO});
      $map->{$ekey}->saveNativeFile("$dir/$ekey.txt", invert=>1,utf8=>1)
	or $map->logconfess("saveTextDirData(): failed to save enum file(s) $dir/$ekey.txt: $!")
      }
  }

  ##-- save: ccs
  foreach (qw(dcm tdm)) { #tdm0
    $map->writePdlTextFile($map->{$_}, "$dir/$_.txt", %opts);
  }

  ##-- save: pdls
  foreach (qw(tw)) { #tw0 tf0 tdf0
    $map->writePdlTextFile($map->{$_}, "$dir/$_.txt", %opts);
  }

  ##-- save: misc
  $map->{lz}->saveDirHeader("$dir/lz.json",%opts) if ($map->{lz});
  $map->{disto}->saveJsonFile("$dir/disto.json") if ($map->{disto});

  return 1;
}

##--------------------------------------------------------------
## Methods: I/O: Directory: load

## $obj = $obj->loadTextDirData($dirname,%opts)
##  + %opts: none
sub loadTextDirData {
  my ($map,$dir,%opts) = @_;

  ##-- load: enums
  foreach my $ekey (qw(denum gcenum lcenum tenum)) {
    if (-e "$dir/$ekey.txt") {
      $map->trace("loading enum $dir/$ekey.txt") if ($opts{verboseIO});
      $map->{$ekey} = MUDL::Enum->loadNativeFile("$dir/$ekey.txt", invert=>1,utf8=>1)
	or $map->logconfess("loadTextDirData(): failed to load enum file $dir/$ekey.txt: $!");
    } else {
      delete $map->{$ekey};
    }
  }

  ##-- load: ccs
  foreach (qw(dcm tdm)) { #tdm0
    $map->{$_} = $map->readPdlTextFile("$dir/$_.txt",%opts,class=>'PDL::CCS::Nd');
  }

  ##-- load: pdls
  foreach (qw(tw)) { #tw0 tf0 tdf0
    $map->{$_} = $map->readPdlTextFile("$dir/$_.txt",%opts,class=>'PDL');
  }

  ##-- load: misc
  $map->{lz} = DocClassify::Lemmatizer->loadDirHeader("$dir/lz.json",%opts)
    or $map->logconfess("loadTextDirData(): failed to load lemmatizer from $dir/lz.json: $!");
  $map->{disto} = MUDL::Cluster::Distance->loadJsonFile("$dir/disto.json")
    or $map->logconfess("loadTextDirData(): failed to load distance object from $dir/disto.json: $!");

  return $map;
}


##==============================================================================
## Footer
1;

__END__
