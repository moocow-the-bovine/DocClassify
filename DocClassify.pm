## -*- Mode: CPerl -*-
## File: DocClassify.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Descript: language identification utilities: top-level


package DocClassify;

use DocClassify::Object;
use DocClassify::Document;
use DocClassify::FileChurner;
use DocClassify::Corpus;
use DocClassify::Signature;

use DocClassify::Lemmatizer;
use DocClassify::Lemmatizer::VzContent;
use DocClassify::Lemmatizer::VzSep;

use DocClassify::Mapper;
use DocClassify::Mapper::ByLemma;
use DocClassify::Mapper::LSI;

use DocClassify::Eval;

use strict;

##==============================================================================
## Globals & Constants

our $VERSION = 0.01;

##==============================================================================
## Constructors etc.


##==============================================================================
## Methods: ...


##==============================================================================
## Footer
1;

__END__

##========================================================================
## POD DOCUMENTATION, auto-generated by podextract.perl
=pod

=cut

##========================================================================
## NAME
=pod

=head1 NAME

DocClassify - Descript: language identification utilities: top-level

=cut

##========================================================================
## SYNOPSIS
=pod

=head1 SYNOPSIS

 ##========================================================================
 ## PRELIMINARIES

 use DocClassify;

=cut

##========================================================================
## DESCRIPTION
=pod

=head1 DESCRIPTION

The DocClassify package provides some language guessing routines
using Kullback-Leibler divergence over character n-gram signatures.

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify: Globals & Constants
=pod

=head2 Globals & Constants

=over 4

=item Variable: $VERSION

Package version.

=back

=cut

##========================================================================
## END POD DOCUMENTATION, auto-generated by podextract.perl

##======================================================================
## See Also
=pod

=head1 SEE ALSO

...

=cut

=pod

##======================================================================
## Footer
##======================================================================

=pod

=head1 AUTHOR

Bryan Jurish E<lt>jurish@uni-potsdam.deE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Bryan Jurish

This package is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut
