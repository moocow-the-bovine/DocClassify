## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::ByLemma.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
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
##  docids => $docIdPdl,             ##-- document id subset: pdl($ND_local): [$doc_pdl_index] -> $docid_denum
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
##  tdm => $tdm_pdl,                 ##-- term-doc matrix: PDL::CCS::Nd ($NT,$ND): [$ti,$di] -> log(f($ti,$di)+$f0)*w($ti)
##  tcm => $tcm_pdl,                 ##-- term-cat matrix: PDL::CCS::Nd ($NT,$NC): [$ti,$ci] -> log(f($ti,$ci)+$f0)*w($ti)
##  ##
##  ##-- data: compile() caches
##  doc_wt => \@doc_wt,              ##-- doc term indices: $di => pdl($NnzDocI) : [$nzi] -> $ti : f($ti,$di)>0
sub new {
  my $that = shift;
  my $obj =  $that->SUPER::new(
			       ##-- options
			       verbose=>$verbose,
			       lzClass => 'default',
			       lzOpts  => {},
			       trainExclusive => 1,
			       minFreq =>10,
			       minDocFreq =>4,
			       maxTermsPerDoc =>1,
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
  return qw(gf df clenum denum tenum docs sigs dcm tw tdm0 tcm0 tdm tcm doc_wt disto);
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
## Methods: Misc

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
  my $dtf_wt = pdl(long,   grep{defined($_)} @{$tenum->{sym2id}}{keys(%{$sig->{lf}})});
  my $dtf_nz = pdl(double, @{$sig->{lf}}{@{$tenum->{id2sym}}[$dtf_wt->list]});
  if ($map->{cleanDocs} || !exists($map->{cleanDocs})) {
    ##-- cleanup
    #$sig->unlemmatize;
    $sig_or_doc->clearCache() if ($sig_or_doc->can('clearCache'));
  }
  if ($as_ccs) {
    ##-- ccs mode
    return PDL::CCS::Nd->newFromWhich($dtf_wt->slice("*1,"),$dtf_nz,dims=>pdl(long,[$tenum->size]),missing=>0)->dummy(1,1);
  }
  ##-- dense mode
  my $dtf = zeroes(double,$tenum->size);
  $dtf->index($dtf_wt) .= $dtf_nz;
  return $dtf->slice(",*1");
}

##==============================================================================
## Methods: API: I/O
##  + see DocClassify::Object


##==============================================================================
## Footer
1;

__END__
