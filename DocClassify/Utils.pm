## -*- Mode: CPerl -*-
## File: DocClassify::Utils.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Descript: language identification utilities: generic utilities


package DocClassify::Utils;

use PDL;
use PDL::VectorValued;
#use PDL::Ngrams;
use PDL::CCS;
#use PDL::Fit::Linfit; ##-- weirdness in tests (DynaLoader crashes on PDL::Slatec sub-dependency)
use IO::Handle;
use IO::File;
use Encode qw(encode decode);
#use POSIX; ##-- setuid, setgid
use Exporter;
use English; ##-- $UID=$<, $EUID=$>, $GID=$(, $EGID=$)

use File::Spec qw(); ##-- for tmpdir()
use File::Temp qw(); ##-- for tempdir(), tempfile()

use XML::LibXML;
use XML::LibXSLT;

use Carp;
use strict;

##==============================================================================
## Globals & Constants

our @ISA = qw(Exporter);

our %EXPORT_TAGS =
  (
   si=>[qw(sistr)],
   fit=>[qw(ylinfit ylogfit li1 F1 Fb)],
   norm=>[qw(_gausscdf _gausswidth)],
   dist=>[qw(_vcos)],
   cmp=>[qw(min2 max2 catcmp)],
   io=>[qw(slurpFile slurpLines stringfh)],
   libxml=>[qw(libxmlParser)],
   libxslt=>[qw(xsl_stylesheet)],
   plot=>[qw(usepgplot)],
   encode => [qw(deep_encode deep_decode deep_recode deep_utf8_upgrade)],
   perms => [qw(setuids setgids)],
   profile => [qw(profile_reset profile_start profile_stop profile_elapsed profile_string)],
   temp => [qw($TMPDIR tmpdir tmpfile tmparray tmphash)],
  );
our @EXPORT_OK = map {@$_} values(%EXPORT_TAGS);
our @EXPORT    = @EXPORT_OK;
$EXPORT_TAGS{all} = [@EXPORT_OK];

##==============================================================================
## Functions: profiling

our @tv_values = qw();
sub profile_reset {
  @tv_values = [];
}
sub profile_start {
  require Time::HiRes;
  return if (scalar(@tv_values) % 2 != 0); ##-- timer already running
  push(@tv_values,[Time::HiRes::gettimeofday]);
}
sub profile_stop {
  require Time::HiRes;
  return if (scalar(@tv_values) % 2 == 0); ##-- timer already stopped
  push(@tv_values,[Time::HiRes::gettimeofday]);
}
sub profile_elapsed {
  require Time::HiRes;
  my @values = @tv_values;
  my $elapsed = 0;
  my ($started,$stopped);
  while (@values) {
    ($started,$stopped) = splice(@values,0,2);
    $stopped  = [Time::HiRes::gettimeofday] if (!defined($stopped));
    $elapsed += Time::HiRes::tv_interval($started,$stopped);
  }
  return $elapsed;
}

## $stats_str = profile_string($ndocs, [$what='docs'])
##  + implicitly calls profile_stop(), profile_elapsed();
sub profile_string {
  my ($nitems,$what) = @_;
  $what   ||= 'docs';
  $nitems //= 0 if (!defined($nitems));
  profile_stop();
  my $elapsed = profile_elapsed();
  my $itemsPerSec = sistr($nitems>0 && $elapsed>0 ? ($nitems/$elapsed) : 0);
  return sprintf("%d %s in %.2f sec: %s %s/sec", $nitems,$what, $elapsed, $itemsPerSec,$what);
}


##==============================================================================
## Functions: permissions (real & effective ids)

## $new_uid = setuids($username_or_uid)
##  + sets real & effective UIDs to $username_or_uid
##  + be sure to call setgids() first if you're using
##    this to drop root permissions
sub setuids {
  my $user = shift;
  my $uid   = 0+($user =~ /^\d+$/ ? $user : scalar(getpwnam($user)));
  $EUID = $uid;
  confess(__PACKAGE__ . "::setuids($user): set EUID=$uid failed: $!") if ($! || $EUID != $uid);
  $UID = $uid;
  confess(__PACKAGE__ . "::setuids($user): set UID=$uid failed: $!") if ($! || $UID != $uid);
  return $UID;
}

## $new_gid = setgids($groupname_or_gid)
##  + sets real & effective GIDs to $groupname_or_gid
##  + be sure to call setgids() first if you're using
##    this to drop root permissions
sub setgids {
  my $group = shift;
  my $gid   = 0+($group =~ /^\d+$/ ? $group : scalar(getgrnam($group)));
  $EGID = $gid;
  confess(__PACKAGE__ . "::setgids($group): set EGID=$gid failed: $!") if ($! || $EGID != $gid);
  $GID = $gid;
  confess(__PACKAGE__ . "::setgids($group): set GID=$gid failed: $!") if ($! || $GID != $gid);
  return $GID;
}

##==============================================================================
## Functions: plotting utilities

## undef = usepgplot()
## undef = usepgplot($PACKAGE)
## undef = usepgplot($PACKAGE,$dev)
sub usepgplot {
  my ($pkg,$dev) = @_;
  $pkg = 'main' if (!defined($pkg));
  $pkg = ref($pkg) if (ref($pkg));
  $dev = '/XS' if (!defined($dev));
  my $s = "package $pkg;".q{
    require PDL::Graphics::PGPLOT;
    require PDL::Graphics::PGPLOT::Window;
    require PDL::Graphics::LUT;    ##-- for color tables used by e.g. imag()
    require PDL::Image2D;          ##-- for box2d, patch2d
    PDL::Graphics::PGPLOT->import();
    PDL::Graphics::PGPLOT::Window->import();
    PDL::Graphics::LUT->import();
    PDL::Image2D->import();
    }."dev('$dev');".q{
    #ctab('Fire');             ##-- ?
    #ctab('gray');
    ctab(lut_data('smooth2')); ##-- color table similar to gnuplot 'pm3d' default; see string list lut_names() for more
    autolog(1);
  };
  eval $s;
  warn("usepgplot(): $@") if ($@);
}



##==============================================================================
## Utils: SI
##==============================================================================

## $si_str = PACKAGE::sistr($val, $printfFormatChar, $printfFormatPrecision, $printfFormatSpace)
sub sistr {
  my ($x, $how, $prec, $ws) = @_;
  $how  = 'f' if (!defined($how));
  $prec = '.2' if (!defined($prec));
  $ws   = '' if (!defined($ws));
  my $fmt = "%${prec}${how}";
  return sprintf("$fmt${ws}T", $x/10**12) if ($x >= 10**12);
  return sprintf("$fmt${ws}G", $x/10**9)  if ($x >= 10**9);
  return sprintf("$fmt${ws}M", $x/10**6)  if ($x >= 10**6);
  return sprintf("$fmt${ws}K", $x/10**3)  if ($x >= 10**3);
  return sprintf("$fmt${ws} ", $x)        if ($x >=  1);
  return sprintf("$fmt${ws}m", $x*10**3)  if ($x >= 10**-3);
  return sprintf("$fmt${ws}u", $x*10**6)  if ($x >= 10**-6);
  return sprintf("$fmt${ws}n", $x*10**9)  if ($x >= 10**-9);
  return sprintf("$fmt${ws} ", $x); ##-- default
}

##==============================================================================
## Functions: libxml utilities

## $LIBXML_PARSER
##   shared XML::LibXML parser object
our ($LIBXML_PARSER);

## $parser = libxmlParser()
##   get shared XML::LibXML parser object
sub libxmlParser {
  return $LIBXML_PARSER if (defined($LIBXML_PARSER));
  $LIBXML_PARSER = XML::LibXML->new();
  $LIBXML_PARSER->keep_blanks(0); ##-- ignore "ignorable whitespace"
  #$LIBXML_PARSER->keep_blanks(1); ##-- keep all whitespace
  $LIBXML_PARSER->line_numbers(1);
  $LIBXML_PARSER->load_ext_dtd(1);
  $LIBXML_PARSER->validation(0);
  $LIBXML_PARSER->recover(1);
  $LIBXML_PARSER->expand_entities(1);
  return $LIBXML_PARSER;
}

##==============================================================================
## Functions: libxslt utilities
##  + see also DTA::TokWrap::Utils

## $XSLT
##  + package-global shared XML::LibXSLT object (or undef)
our $XSLT = undef;

## $xslt = PACKAGE::xsl_xslt()
##  + returns XML::LibXSLT object
sub xsl_xslt {
  $XSLT = XML::LibXSLT->new() if (!$XSLT);
  return $XSLT;
}

## $stylesheet = PACKAGE::xsl_stylesheet(file=>$xsl_file)
## $stylesheet = PACKAGE::xsl_stylesheet(fh=>$xsl_fh)
## $stylesheet = PACKAGE::xsl_stylesheet(doc=>$xsl_doc)
## $stylesheet = PACKAGE::xsl_stylesheet(string=>$xsl_string)
sub xsl_stylesheet {
  my ($what,$src) = @_;
  my $xmlparser = libxmlParser();

  my ($doc);
  if ($what eq 'file') {
    $doc = $xmlparser->parse_file($src)
      or croak(__PACKAGE__, "::xsl_stylesheet(): failed to parse XSL source file '$src' as XML: $!");
  } elsif ($what eq 'fh') {
    $doc = $xmlparser->parse_fh($src)
      or croak(__PACKAGE__, "::xsl_stylesheet(): failed to parse XSL source filehandle as XML: $!");
  } elsif ($what eq 'doc') {
    $doc = $src;
  } elsif ($what eq 'string') {
    $doc = $xmlparser->parse_string($src)
      or croak(__PACKAGE__, "::xsl_stylesheet(): failed to parse XSL source string as XML: $!");
  } else {
    DocClassify::Logger->logwarn(__PACKAGE__, "::xsl_stylesheet(): treating unknown type key '$what' as 'string'");
    $doc = $xmlparser->parse_string(defined($src) ? $src : $what)
      or croak(__PACKAGE__, "::xsl_stylesheet(): failed to parse XSL source string as XML: $!");
  }
  croak(__PACKAGE__, "::xsl_stylesheet(): no XSL source document!") if (!$doc);

  my $xslt = xsl_xslt();
  my $stylesheet = $xslt->parse_stylesheet($doc)
    or croak(__PACKAGE__, "::xsl_stylesheet(): could not parse XSL stylesheet: $!");

  return $stylesheet;
}


##==============================================================================
## Functions: I/O
##  + mostly lifted from MUDL::PDL::Smooth

## \$str = slurpFile($file_or_fh)
## \$str = slurpFile($file_or_fh,\$str)
## \$str = slurpFile($file_or_fh,\$str,@binmodes)
sub slurpFile {
  my ($file,$ref,@binmodes) = @_;
  if (!defined($ref)) {
    my $str = '';
    $ref = \$str;
  }
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  confess(__PACKAGE__."::slurpFile(): open failed for '$file': $!") if (!defined($fh));
  if (@binmodes && $fh->can('binmode')) {
    $fh->binmode($_) foreach (@binmodes);
  }
  local $/=undef;
  $$ref = <$fh>;
  $fh->close() if (!ref($file));
  return $ref;
}

## \@lines = slurpLines($file_or_fh)
## \@lines = slurpLines($file_or_fh,\@lines)
## \@lines = slurpLines($file_or_fh,\@lines,@binmodes)
sub slurpLines {
  my ($file,$ref,@binmodes) = @_;
  $ref =[] if (!defined($ref));
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  confess(__PACKAGE__."::slurpFile(): open failed for '$file': $!") if (!defined($fh));
  if (@binmodes && $fh->can('binmode')) {
    $fh->binmode($_) foreach (@binmodes);
  }
  @$ref = <$fh>;
  $fh->close() if (!ref($file));
  return $ref;
}


## ($outfh,\$str) = stringfh($mode)
## ($outfh,\$str) = stringfh($mode,\$str)
##  + returns new filehandle for I/O from/to \$str, which defaults to a new string
##  + $mode defaults to '+<' (read/write)
sub stringfh {
  my ($mode,$ref) = @_;
  if (!defined($ref)) { my $str=''; $ref=\$str; }
  $mode = '+<' if (!defined($mode));
  my $fh = IO::Handle->new();
  open($fh,$mode,$ref);
  return ($fh,$ref);
}

##==============================================================================
## Functions: Math

## $min = min2($x,$y)
sub min2 { return $_[0] < $_[1] ? $_[0] : $_[1]; }

## $max = max2($x,$y)
sub max2 { return $_[0] > $_[1] ? $_[0] : $_[1]; }

## $cmp = catcmp($catname1, $catname2)
sub catcmp($$) {
  my ($cat1,$cat2) = @_;
  my $ci1 = $cat1 =~ /^(\d+)/ ? $1 : -1;
  my $ci2 = $cat2 =~ /^(\d+)/ ? $1 : -1;
  return $ci1 <=> $ci2 || $cat1 cmp $cat2;
}

##==============================================================================
## Functions: PDL utils
##  + mostly lifted from MUDL::PDL::Smooth

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


## $coefs         = ylinfit($y,$x) ##-- scalar context
## ($yfit,$coefs) = ylinfit($y,$x) ##-- array context
##  + linear fits $y to $x
##  + $coeffs are [$a,$b] such that all($yfit == $a*$x + $b)
sub ylinfit {
  require PDL::Fit::Linfit;
  my ($y,$x) = @_;
  my ($yfit,$coefs) = $y->linfit1d($x->setnantobad->setbadtoval(0)->cat($y->ones));
  return wantarray ? ($yfit,$coefs) : $coefs;
}

## $coefs         = ylogfit($y,$x) ##-- scalar context
## ($yfit,$coefs) = ylogfit($y,$x) ##-- array context
##  + linear fits log($y) to log($x)
##  + $coeffs are [$a,$b] such that all($yfit == $b*($x**$a))
sub ylogfit {
  my ($y,$x) = @_;
  my ($yfit,$coefs) = ylinfit($y->log,$x->log);
  $yfit->inplace->exp;
  $coefs->slice("(1)")->inplace->exp;
  return wantarray ? ($yfit,$coefs) : $coefs;
}

##-- from MUDL::PDL::Smooth
## $pvals = gausscdf($xvals, $mu,$sigma);
sub _gausscdf {
  my ($x,$mu,$sigma) = @_;
  $sigma = 1 if (!defined($sigma));
  $mu    = 0 if (!defined($mu));
  return 0.5*(1 + erf( ($x-$mu) / ($sigma*sqrt(2)) ));
}

##-- from MUDL::PDL::Smooth
## $width = gausswidth($confidence, $mu,$sigma);                  ##-- scalar context
## ($mu-$width,$mu+$width) = gausswidth($confidence, $mu,$sigma); ##-- array context
##  + In scalar context, returns width around mean corresponding to confidence level $confidence
##  + In array context, returns interval around mean for level $confidence.
sub _gausswidth {
  my ($conf,$mu,$sigma) = @_;
  $sigma = 1 if (!defined($sigma));
  $mu    = 0 if (!defined($mu));
  my $w  = erfi($conf) * sqrt(2) * $sigma;
  return wantarray ? ($mu-$w,$mu+$w) : $w;
}

## $dist = _vcos($data, $cdata, %opts)
##  + args:
##       data    # pdl($d,$nR)  : $d=N_features, $nR=N_rows
##       cdata   # pdl($d,$nC)  : $d=N_features, $nC=N_centers
##    #[o]dist    # pdl($nC,$nR) : output distances
##  + local implementation with no mask, weight, etc: ca 2x faster than MUDL::Cluster::Distance::Builtin
##  + %opts:
##     sigma1  => $sigma1,   # stddev for $data  ($nR)
##     sigma2  => $sigma2,   # stddev for $cdata ($nC)
sub _vcos {
  my ($dr1,$dr2,%opts) = @_;

  ##-- dist(x,y) = 1 - 1/d * (\sum_{i=1}^d (x[i]-mean(x))/stddev(x) * (y[i]-mean(y))/stddev(y))
  ##             = 1 - 1/d * 1/stddev(x) * 1/stddev(y) * (\sum_{i=1}^d (x[i]-mean(x)) * (y[i]-mean(y)))
  ##             = 1 - (\sum_{i=1}^d (x[i]-mean(x)) * (y[i]-mean(y))) / (d * stddev(x) * stddev(y))
  ## + where:
  ##     mean(x)   := 0
  ##     stddev(x) := sqrt( E(X^2) )
  my $d = $dr1->dim(0);
  my ($sigma1,$sigma2) = @opts{qw(sigma1 sigma2)};
  if (!defined($sigma1)) { $sigma1 = $dr1->pow(2)->average; $sigma1->inplace->sqrt; } ##-- expensive!
  if (!defined($sigma2)) { $sigma2 = $dr2->pow(2)->average; $sigma2->inplace->sqrt; } ##-- expensive!

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

  return $dist;
}


##==============================================================================
## Hacks: Regexp::Storable fixes

BEGIN {
  no warnings 'redefine';

  ##-- from Regexp::Storable.pm, distribution Regexp::Copy
  package Regexp;
  sub STORABLE_freeze_forgetaboutit {
    #my $serialized = substr($_[0], rindex($_[0],':')+1, -1); ##-- original
    my $str        = "$_[0]";
    my $serialized =  substr($str,CORE::index($_[0],':')+1,-1);    ##-- fixed
    return $serialized;
  }

  ##-- nothing changed here
  sub STORABLE_thaw_donteventhinkaboutit {
    my ( $original, $cloning, $thaw ) = @_;
    my $final = ($thaw) ? qr/$thaw/ : qr//;
    Regexp::Copy::re_copy($final, $original);
  }
}

##==============================================================================
## Functions: Deep recoding
##==============================================================================

## $decoded = deep_decode($encoding,$thingy,%options)
##  + %options:
##     force    => $bool,   ##-- decode even if the utf8 flag is set
##     skipvals => \@vals,  ##-- don't decode (or recurse into)  $val (overrides $force)
##     skiprefs => \@refs,  ##-- don't decode (or recurse into) $$ref (overrides $force)
##     skippkgs => \@pkgs,  ##-- don't decode (or recurse into) anything of package $pkg (overrides $force)
sub deep_decode {
  my ($enc,$thingy,%opts) = @_;
  my %skipvals = defined($opts{skipvals}) ? (map {($_=>undef)} @{$opts{skipvals}}) : qw();
  my %skiprefs = defined($opts{skiprefs}) ? (map {($_=>undef)} @{$opts{skiprefs}}) : qw();
  my %skippkgs = defined($opts{skippkgs}) ? (map {($_=>undef)} @{$opts{skippkgs}}) : qw();
  my $force    = $opts{force};
  my @queue = (\$thingy);
  my ($ar);
  while (defined($ar=shift(@queue))) {
    if (exists($skiprefs{$ar}) || exists($skipvals{$$ar}) || (ref($$ar) && exists($skippkgs{ref($$ar)}))) {
      next;
    } elsif (UNIVERSAL::isa($$ar,'ARRAY')) {
      push(@queue, map { \$_ } @{$$ar});
    } elsif (UNIVERSAL::isa($$ar,'HASH')) {
      push(@queue, map { \$_ } values %{$$ar});
    } elsif (UNIVERSAL::isa($$ar, 'SCALAR') || UNIVERSAL::isa($$ar,'REF')) {
      push(@queue, $$ar);
    } elsif (!ref($$ar)) {
      $$ar = decode($enc,$$ar) if (defined($$ar) && ($force || !utf8::is_utf8($$ar)));
    }
  }
  return $thingy;
}

## $encoded = deep_encode($encoding,$thingy,%opts)
##  + %opts:
##     force => $bool,            ##-- encode even if the utf8 flag is NOT set
##     skipvals => \@vals,        ##-- don't encode (or recurse into)  $val (overrides $force)
##     skiprefs => \@refs,        ##-- don't encode (or recurse into) $$ref (overrides $force)
##     skippkgs => \@pkgs,        ##-- don't encode (or recurse into) anything of package $pkg (overrides $force)
sub deep_encode {
  my ($enc,$thingy,%opts) = @_;
  my %skipvals = defined($opts{skipvals}) ? (map {($_=>undef)} @{$opts{skipvals}}) : qw();
  my %skiprefs = defined($opts{skiprefs}) ? (map {($_=>undef)} @{$opts{skiprefs}}) : qw();
  my %skippkgs = defined($opts{skippkgs}) ? (map {($_=>undef)} @{$opts{skippkgs}}) : qw();
  my $force    = $opts{force};
  my @queue = (\$thingy);
  my ($ar);
  while (defined($ar=shift(@queue))) {
    if (exists($skiprefs{$ar}) || !defined($$ar) || exists($skipvals{$$ar}) || (ref($$ar) && exists($skippkgs{ref($$ar)}))) {
      next;
    } elsif (UNIVERSAL::isa($$ar,'ARRAY')) {
      push(@queue, map { \$_ } @{$$ar});
    } elsif (UNIVERSAL::isa($$ar,'HASH')) {
      push(@queue, map { \$_ } values %{$$ar});
    } elsif (UNIVERSAL::isa($$ar, 'SCALAR') || UNIVERSAL::isa($$ar,'REF')) {
      push(@queue, $$ar);
    } elsif (!ref($$ar)) {
      $$ar = encode($enc,$$ar) if (defined($$ar) && ($force || utf8::is_utf8($$ar)));
    }
  }
  return $thingy;
}

## $recoded = deep_recode($from,$to,$thingy, %opts);
sub deep_recode {
  my ($from,$to,$thingy,%opts) = @_;
  return deep_encode($to,deep_decode($from,$thingy,%opts),%opts);
}

## $upgraded = deep_utf8_upgrade($thingy)
sub deep_utf8_upgrade {
  my ($thingy) = @_;
  my @queue = (\$thingy);
  my ($ar);
  while (defined($ar=shift(@queue))) {
    if (UNIVERSAL::isa($$ar,'ARRAY')) {
      push(@queue, map { \$_ } @{$$ar});
    } elsif (UNIVERSAL::isa($$ar,'HASH')) {
      push(@queue, map { \$_ } values %{$$ar});
    } elsif (UNIVERSAL::isa($$ar, 'SCALAR') || UNIVERSAL::isa($$ar,'REF')) {
      push(@queue, $$ar);
    } elsif (!ref($$ar)) {
      utf8::upgrade($$ar) if (defined($$ar));
    }
  }
  return $thingy;
}

##==============================================================================
## Functions: temporaries

## $TMPDIR : global temp directory to use
our $TMPDIR  = undef;

## $tmpdir = CLASS->tmpdir()
## $tmpdir = CLASS_>tmpdir($template, %opts)
##  + in first form, get name of global tempdir ($TMPDIR || File::Spec::tmpdir())
##  + in second form, create and return a new temporary directory via File::Temp::tempdir()
sub tmpdir {
  my $that = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my $tmpdir = $TMPDIR || File::Spec->tmpdir();
  return @_ ? File::Temp::tempdir($_[0], DIR=>$tmpdir, @_[1..$#_]) : $tmpdir;
}

## $filename = CLASS->tmpfile()
## $filename = CLASS->tmpfile($template, %opts)
sub tmpfile {
  my $that = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my $template = shift // 'tmpXXXXX';
  my ($fh,$filename) = File::Temp::tempfile($template, DIR=>$that->tmpdir(), @_)
    or return undef;
  $fh->close();
  return $filename;
}

## \@tmparray = CLASS->tmparray($template, %opts)
##  + ties a new temporary array via $class (default='Tie::File::Indexed::JSON')
##  + calls tie(my @tmparray, 'DocClassify::Temp::Array', $tmpfilename, %opts)
sub tmparray {
  my $that  = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my ($template,%opts) = @_;

  ##-- load target module
  eval { require "DocClassify/Temp/Array.pm" }
    or DocClassify->logconfess("tmparray(): failed to load class DocClassify::Temp::Array: $@");

  ##-- default options
  $template     //= 'dcXXXXXX';
  $opts{SUFFIX} //= '.dba';
  $opts{UNLINK}   = 1 if (!exists($opts{UNLINK}));

  ##-- tie it up
  my $tmpfile = $that->tmpfile($template, %opts);
  tie(my @tmparray, 'DocClassify::Temp::Array', $tmpfile, %opts)
    or DocClassify->logconfess("tmparray(): failed to tie file '$tmpfile' via DocClassify::Temp::Array: $@");
  return \@tmparray;
}

## \%tmphash = CLASS->tmphash($class, $template, %opts)
##  + ties a new temporary hash via $class (default='DB_File')
##  + calls tie(my @tmparray, $class, $tmpfilename, temp=>1, %opts)
sub tmphash {
  my $that  = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my ($template,%opts) = @_;

  ##-- load target module
  eval { require "DocClassify/Temp/Hash.pm" }
    or DocClassify->logconfess("tmparray(): failed to load class DocClassify::Temp::Hash: $@");

  ##-- default options
  $template     //= 'dcXXXXXX';
  $opts{SUFFIX} //= '.dbh';
  $opts{UNLINK}   = 1 if (!exists($opts{UNLINK}));

  ##-- tie it up
  my $tmpfile = $that->tmpfile($template, %opts);
  tie(my %tmphash, 'DocClassify::Temp::Hash', $tmpfile, %opts)
    or DocClassify->logconfess("tmphash(): failed to tie file '$tmpfile' via DocClassify::Temp::Hash: $@");
  return \%tmphash;
}


##==============================================================================
## Footer
1;


__END__
##========================================================================
## POD DOCUMENTATION, auto-generated by podextract.perl, edited

##========================================================================
## NAME
=pod

=head1 NAME

DocClassify::Utils - language guesser: generic utilities

=cut

##========================================================================
## SYNOPSIS
=pod

=head1 SYNOPSIS

 ##========================================================================
 ## PRELIMINARIES
 
 use DocClassify::Utils;
 
 ##========================================================================
 ## Functions: generic utilities
 
 ($outfh,\$str) = stringfh($mode);
 $min = min2($x,$y);
 $max = max2($x,$y);
 $coefs         = ylinfit($y,$x) ##-- scalar context;
 $coefs         = ylogfit($y,$x) ##-- scalar context;
 

=cut

##========================================================================
## DESCRIPTION
=pod

=head1 DESCRIPTION

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Utils: Globals & Constants
=pod

=head2 Globals & Constants

=over 4

=item Variable: %EXPORT_TAGS

Following tags are exportabe:

 fit=>[qw(ylinfit ylogfit)],
 norm=>[qw(_gausscdf _gausswidth)],
 cmp=>[qw(min2 max2 catcmp)],
 io=>[qw(stringfh)],
 all=>$ALL_OF_THE_ABOVE

=item Variable: @EXPORT_OK

All functions exportable under any tag are exportable.

=item Variable: @EXPORT

Everything is exported by default.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Utils: Functions: generic utilities
=pod

=head2 Functions: generic utilities

=over 4

=item stringfh

 ($outfh,\$str) = stringfh($mode);
 ($outfh,\$str) = stringfh($mode,\$str)

Returns new filehandle for I/O from/to \$str, which defaults to a new string.
$mode defaults to '+E<lt>' (read/write).

=item min2

 $min = min2($x,$y);

Binary minimum operator for numeric scalars.

=item max2

 $max = max2($x,$y);

Binary maximum operator for numeric scalars.

=item ylinfit

 $coefs         = ylinfit($y,$x) ##-- scalar context;
 ($yfit,$coefs) = ylinfit($y,$x) ##-- array context

Linear fits $y to $x using PDL::Fit::linfit::linfit1d().

$coeffs are [$a,$b] such that:

 all($yfit == $a*$x + $b)


=item ylogfit

 $coefs         = ylogfit($y,$x) ##-- scalar context;
 ($yfit,$coefs) = ylogfit($y,$x) ##-- array context

Linear fits log($y) to log($x) using ylinfit().

$coeffs are [$a,$b] such that:

 all($yfit == $b*($x**$a))


=item _gausscdf

 $pvals = gausscdf($xvals, $mu,$sigma);

Returns p-values (pdl) corrsponding to $xvals for normal distribution with parameters ($mu,$sigma).


=item _gausswidth

 $width = gausswidth($confidence, $mu,$sigma);                  ##-- scalar context
 ($mu-$width,$mu+$width) = gausswidth($confidence, $mu,$sigma); ##-- array context

In scalar context, returns width around mean corresponding to confidence level $confidence
In array context, returns interval around mean for level $confidence.

=back

=cut

##========================================================================
## END POD DOCUMENTATION, auto-generated by podextract.perl

##======================================================================
## See Also
=pod

=head1 SEE ALSO

DocClassify(3pm)

=cut

##======================================================================
## Footer
=pod

=head1 AUTHOR

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009-2015 by Bryan Jurish

This package is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.20.2 or,
at your option, any later version of Perl 5 you may have available.

=cut
