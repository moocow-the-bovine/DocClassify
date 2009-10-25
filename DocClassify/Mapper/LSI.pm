## -*- Mode: CPerl -*-
## File: DocClassify::Mapper::LSI.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: document classifier: document-to-class mapper: "latent semantic indexing" / SVD

package DocClassify::Mapper::LSI;
use DocClassify::Object;
use DocClassify::Mapper;
use DocClassify::Utils ':all';

use MUDL::Enum;
use MUDL::SVD;
use MUDL::Cluster::Distance;

use PDL;
use PDL::CCS::Nd;

use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals

our @ISA = qw(DocClassify::Mapper);

our $verbose = 2;

##==============================================================================
## Constructors etc.

## $map = $CLASS_OR_OBJ->new(%opts)
## %$map, %opts:
##  ##-- options
##  sigopts => \%opts,               ##-- options for $doc->termSignature()
##  trainExclusive => $bool,         ##-- use each doc to train at most 1 cat? (default=true)
##  minFreq => $f,                   ##-- minimum global term-frequency (default=0)
##  smoothf => $f0,                  ##-- global frequency smoothing constant (default=NTypes/NTokens)
##  unkTerm => $str,                 ##-- unknown term string (default='__UNKNOWN__')
##  svdr => $svdr,                   ##-- number of reduced dimensions (default=64)
##  dist => $distSpec,               ##-- distance spec for MUDL::Cluster::Distance (default='u')
##                                   ##   + 'c'=Pearson, 'u'=Cosine, 'e'=Euclid, ...
##  ##
##  ##-- data: enums
##  cenum => $catEnum,               ##-- category enum ($NC=$catEnum->size())
##  tenum => $termEnum,              ##-- term enum     ($NT=$termEnum->size())
##  ##
##  ##-- data: training
##  gf => \%global_tf,               ##-- global term-frequency hash
##  c2sigs => \%cat2sigs,            ##-- maps category names to term-signature-arrays
##  ##
##  ##-- data: post-compile()
##  distf => $distObj,               ##-- MUDL::Cluster::Distance object
##  tcf => $tcf_pdl,                 ##-- PDL::CCS::Nd ($NT,$NC): [$ti,$ci] -> log(f($ti,$ci)+$f0)
##  svd => $svd,                     ##-- a MUDL::SVD object
##  xcf => $xcf_pdl,                 ##-- dense PDL ($svdr,$NC) = $svd->apply($tcf->decode)
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   ##-- options
			   sigopts => {},
			   trainExclusive => 1,
			   minFreq => 1,
			   smoothf=>undef,
			   unkTerm => '__UNKNOWN__',
			   svdr => 64,
			   dist => 'u',

			   ##-- data: enums
			   cenum => MUDL::Enum->new,
			   tenum => MUDL::Enum->new,

			   ##-- data: training
			   gf => {},
			   c2sigs => {},

			   ##-- data: post-compile
			   tcf=>undef,
			   svd=>undef,
			   xcf=>undef,
			   distf=>undef,

			   ##-- user args
			   @_,
			  );
}


## @noShadowKeys = $obj->noShadowKeys()
##  + returns list of keys not to be passed to $CLASS->new() on shadow()
##  + override returns qw(gf c2sigs tcf svd xcf distf)
sub noShadowKeys {
  return qw(gf c2sigs tcf svd xcf distf);
}

##==============================================================================
## Methods: API: Training

## $map = $map->trainCorpus($corpus)
##  + add training data from $corpus
##  + inherited default just calls $map->trainDocument($doc) foreach doc in corpus

## $map = $map->trainDocument($doc)
##  + add training data from $doc
##  + calls $doc->termSignature()
sub trainDocument {
  my ($map,$doc) = @_;
  my $sig = $doc->termSignature(%{$map->{sigopts}});

  ##-- add sig frequency data to global hash
  $map->{gf}{$_} += $sig->{tf}{$_} foreach (keys(%{$sig->{tf}}));

  ##-- add sig to category-local hashes
  my ($cat);
  foreach $cat ($map->{trainExclusive} ? ($doc->{cats}[0]) : @{$doc->{cats}}) {
    next if (!defined($map->addCat(@$cat{qw(name id)})));
    push(@{$map->{c2sigs}{$cat->{name}}}, $sig);
  }

  ##-- that's it for here...
  return $doc;
}

## $catId = $map->addCat($catName)
## $catId = $map->addCat($catName,$wantId)
##  + requests addition of a category $catName (with id $wantId) in $map->{cenum}
sub addCat {
  my ($map,$name,$id) = @_;
  return undef if (!defined($name));
  if (defined($id) && !defined($map->{cenum}{id2sym}[$id])) {
    return $map->{cenum}->addIndexedSymbol($name,$id);
  }
  return $map->{cenum}->addSymbol($name);
}

##==============================================================================
## Methods: API: Compilation

## $map = $map->compile()
##  + compile underlying map data
##  + should be called only after all training data have been added
sub compile {
  my $map = shift;

  ##-- trim global frequency hash
  print STDERR ref($map)."::compile(): trim(minFreq=>$map->{minFreq})\n" if ($verbose);
  delete(@{$map->{gf}}{ grep {$map->{gf}{$_} < $map->{minFreq}} keys(%{$map->{gf}}) }) if ($map->{minFreq}>0);

  ##-- expand all terms to $map->{tenum}
  print STDERR ref($map)."::compile(): enums\n" if ($verbose);
  my $tenum = $map->{tenum};
  $tenum->clear();
  @$tenum{id2sym} = ($map->{unkTerm}, keys(%{$map->{gf}}));
  @{$tenum->{sym2id}}{@$tenum{id2sym}} = (0..$#{$tenum->{id2sym}});
  my $NT = $tenum->size;

  ##-- ensure all relevant categories are defined in $map->{cenum}
  my $cenum = $map->{cenum};
  $cenum->clear();
  $cenum->addSymbol($_) foreach (keys(%{$map->c2sigs}));
  my $NC = $cenum->size;

  ##-- get term-cat-frequency matrix: $tcf : [$tid,$cid] => f($tid,$cid)
  print STDERR ref($map)."::compile(): matrix(NT=$NT x NC=$NC)\n" if ($verbose);
  my ($tcf_w,$tcf_nz) = (null,null);
  my ($c_name,$c_sigs,$c_id);
  my ($sig, $sig_wt,$sig_w,$sig_nz);
  while (($c_name,$c_sigs) = each(%{$map->{c2sigs}})) {
    $c_id = $cenum->{sym2id}{$c_name};

    foreach $sig (@$c_sigs) {
      $sig_wt = pdl(long, grep {defined($_)} @{$tenum->{sym2id}}{keys(%{$sig->{tf}})});
      $sig_w  = $sig_wt->slice("*1,")->glue(0, zeroes(long,1,1)+$c_id);
      $sig_nz = pdl(double, @{$sig->{tf}}{@{$tenum->{id2sym}}[$sig_wt->list]});

      $tcf_w  = $tcf_w->glue(1,$sig_w);
      $tcf_nz = $tcf_nz->append($sig_nz);

      #delete($sig->{tf});	##-- frequency data all used up
    }
  }
  my $dims = pdl(long,$tenum->size,$cenum->size);
  my $tcf = $map->{tcf} = PDL::CCS::Nd->newFromWhich($tcf_w,$tcf_nz,dims=>$dims,missing=>0)->dummy(0,1)->sumover;

  ##-- clear expensive perl signature structs
  %{$map->{c2sigs}} = qw();

  ##-- smooth & log-transform term-cat matrix
  $map->{smoothf} = $NT/$tcf->sum if (!defined($map->{smoothf}));
  $tcf = ($tcf+$map->{smoothf})->inplace->log;

  ##-- create $map->{svd}, $map->{xcf} ~= $map->{svd}->apply($map->{tcf})
  my $svd  = $map->{svd} = MUDL::SVD->new(r=>$map->{svdr});
  $svd->computeccs_nd($tcf);
  $map->{xcf} = $svd->apply($tcf->decode);

  ##-- create distance object
  $map->{disto} = MUDL::Cluster::Distance->new(class=>$map->{dist});

  return $map;
}


## $bool = $map->compiled()
##  + returns true iff $map has been compiled
##  + override checks for $map->{xcf}
sub compiled { return defined($_[0]{xcf}); }


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

  ##-- get doc pdl
  my $dtf = $map->docPdlRaw($doc);
  my $dtN = $dtf->sum;
  warn(ref($map)."::mapDocument(): null vector for document '$doc->{label}'") if ($verbose && $dtN==0);

  ##-- compute distance to each centroid
  my $dtx   = $map->svdApply($dtf);
  my $cdmat = $map->{disto}->clusterDistanceMatrix(data=>$dtx,cdata=>$map->{xcf});

  ##-- dump distances to $doc->{cats}
  @{$doc->{cats}} = map {
    {id=>$_,name=>$map->{cenum}{id2sym}{$_},dist=>$cdmat->at($_,0),deg=>99}
  } @{$cdmat->flat->qsorti->list};
  $doc->{cats}[0]{deg} = 1;

  return $doc;
}

##==============================================================================
## Methods: Misc

## $dxpdl = $map->svdApply($fpdl)
##  + input $fpdl is dense pdl($NT):       [$tid]       => f($tid,$doc)
##  + output $dxpdl is dense pdl(1,$svdr): [$svd_dim,0] => $svd_val
sub svdApply {
  my ($map,$fpdl) = @_;
  $fpdl = $map->sigPdlRaw($fpdl) if (!UNIVERSAL::isa($fpdl,'PDL')); ##-- $fpdl passed as doc?
  $fpdl = $fpdl->todense if (UNIVERSAL::isa($fpdl,'PDL::CCS::Nd')); ##-- avoid memory explosion in Nd::inner()
  $fpdl = ($fpdl+$map->{smoothf})->log;
  return $map->{svd}->apply($fpdl->slice(":,*1"));
}

## $sig = $map->docSignature($doc)
##  + wrapper for $doc->termSignature with options %{$map->{sigopts}}
sub docSignature {
  my ($map,$doc) = @_;
  return $doc->termSignature(%{$map->{sigopts}});
}

## $fpdl = $map->docPdlRaw($doc, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tid]=>f($tid,$doc)
sub docPdlRaw {
  return $_[0]->sigPdlRaw(@_[1..$#_]);
}

## $fpdl = $map->sigPdlRaw($sig, $want_ccs=0)
##  + $fpdl is dense or CCS::Nd pdl($NT): [$tid]=>f($tid,$sig)
sub sigPdlRaw {
  my ($map,$sig, $as_ccs) = @_;
  $sig = $map->docSignature($sig) if (!UNIVERSAL::isa($sig,'DocClassify::Signature')); ##-- handle docs
  my $tenum = $map->{tenum};
  my $dtf_wt = pdl(long,   grep{defined($_)} @{$tenum->{sym2id}}{keys(%{$sig->{tf}})});
  my $dtf_nz = pdl(double, @{$sig->{tf}}{@{$tenum->{id2sym}}[$dtf_wt->list]});
  if ($as_ccs) {
    ##-- ccs mode
    return PDL::CCS::Nd->newFromWhich($dtf_wt->slice("*1,"),$dtf_nz,dims=>pdl(long,[$tenum->size]),missing=>0);
  }
  ##-- dense mode
  my $dtf = zeroes(double,$tenum->size);
  $dtf->index($dtf_wt) .= $dtf_nz;
  return $dtf;
}

##==============================================================================
## Methods: API: I/O
##  + see DocClassify::Object


##==============================================================================
## Footer
1;

__END__
