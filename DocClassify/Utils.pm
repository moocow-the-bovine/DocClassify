## -*- Mode: CPerl -*-
## File: DocClassify::Utils.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: language identification utilities: generic utilities


package DocClassify::Utils;

use PDL;
use PDL::VectorValued;
use PDL::Ngrams;
use PDL::CCS;
#use PDL::Fit::Linfit; ##-- weirdness in tests (DynaLoader crashes on PDL::Slatec sub-dependency)
use IO::Handle;
use IO::File;
use Exporter;

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
   cmp=>[qw(min2 max2)],
   io=>[qw(slurpFile slurpLines stringfh)],
   libxml=>[qw(libxmlParser)],
   libxslt=>[qw(xsl_stylesheet)],
   plot=>[qw(usepgplot)],
  );
our @EXPORT_OK = map {@$_} values(%EXPORT_TAGS);
our @EXPORT    = @EXPORT_OK;
$EXPORT_TAGS{all} = [@EXPORT_OK];

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
  $ws   = ' ' if (!defined($ws));
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
 cmp=>[qw(min2 max2)],
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

Bryan Jurish E<lt>jurish@uni-potsdam.deE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Bryan Jurish

This package is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut
