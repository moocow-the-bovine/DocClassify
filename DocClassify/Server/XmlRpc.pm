## -*- Mode: CPerl -*-
##
## File: DocClassify::Server::XmlRpc.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description: DocClassify XML-RPC server using RPC::XML

package DocClassify::Server::XmlRpc;
use DocClassify::Server;
use RPC::XML;
use RPC::XML::Server;
use Encode qw(encode decode);
use Socket qw(SOMAXCONN);
use Carp;
use strict;

##==============================================================================
## Globals
##==============================================================================

our @ISA = qw(DocClassify::Server);

##==============================================================================
## Constructors etc.
##==============================================================================

## $obj = CLASS_OR_OBJ->new(%args)
##  + object structure: HASH ref
##    {
##     ##-- Underlying server
##     xsrv => $xsrv,      ##-- low-level server, an RPC::XML::Server object
##     xopt => \%opts,     ##-- options for RPC::XML::Server->new()
##     xrun => \%opts,     ##-- options for RPC::XML::Server->server_loop()
##     ##
##     ##-- XML-RPC procedure naming
##     procNamePrefix => $prefix, ##-- default: 'dc.'
##     ##
##     ##-- hacks
##     encoding => $enc,   ##-- sets $RPC::XML::ENCODING on prepare(), used by underlying server
##     ##
##     ##-- (inherited from DocClassify::Server)
##     maps => \@maps,        ##-- [$mapper1,...]
##    }
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   ##-- underlying server
			   xsrv => undef,
			   xopt => {
				    #path => '/',         ##-- URI path for underlying server (HTTP::Daemon)
				    #host => '0.0.0.0',   ##-- host for underlying server (HTTP::Daemon)
				    port => 9099,         ##-- port for underlying server (HTTP::Daemon)
				    queue => SOMAXCONN,   ##-- queue size for underlying server (HTTP::Daemon)
				    #timeout => 10,       ##-- connection timeout (HTTP::Daemon)
				    ##
				    #no_default => 1,     ##-- disable default methods (default=enabled)
				    #auto_methods => 1,   ##-- enable auto-method seek (default=0)
				   },
			   xrun => {
				    #signal => [qw(INT HUP TERM)],
				    signal => 0, ##-- don't catch any signals by default
				   },
			   ##
			   ##-- XML-RPC procedure naming
			   procNamePrefix => 'dc.',
			   ##
			   ##-- hacks
			   encoding => 'UTF-8',
			   ##
			   ##-- user args
			   @_
			  );
}

## undef = $obj->initialize()
##  + called to initialize new objects after new()

##==============================================================================
## Methods: Encoding Hacks
##==============================================================================

## \%rpcProcHash = $srv->wrapMethodEncoding(\%rpcProcHash)
##  + wraps an RPC::XML::procedure spec into $srv->{encoding}-safe code,
##    only if $rpcProcHash{wrapEncoding} is set to a true value
##  + UNUSED
sub wrapMethodEncoding {
  my $srv = shift;
  if (defined($srv->{encoding}) && $_[0]{wrapEncoding}) {
    my $code_orig = $_[0]{code_orig} = $_[0]{code};
    $_[0]{code} = sub {
      my $rv  = $code_orig->(@_);
      my $rve = DocClassify::Utils::deep_encode($srv->{encoding}, $rv);
     return $rve;
    };
  }
  return $_[0];
}


##==============================================================================
## Methods: Generic Server API
##==============================================================================

## $rc = $srv->prepareLocal()
##  + subclass-local initialization
sub prepareLocal {
  my $srv = shift;

  ##-- get RPC::XML object
  #my $xsrv = $srv->{xsrv} = RPC::XML::Server->new(%{$srv->{xopt}});
  my $xsrv = $srv->{xsrv} = DocClassify::Server::XmlRpc::RPC::XML::Server->new(%{$srv->{xopt}});
  if (!ref($xsrv)) {
    $srv->fatal("could not create underlying RPC::XML::Server object"
		." (is port ".($srv->{xopt}{port}||80)." already bound?)");
    $srv->logconfess("RPC::XML->new returned: $xsrv\n");
  }

  ##-- hack: set server encoding
  if (defined($srv->{encoding})) {
    $srv->debug("setting RPC::XML::ENCODING = $srv->{encoding}");
    $RPC::XML::ENCODING = $srv->{encoding};
  }
  ##-- hack: set $RPC::XML::FORCE_STRING_ENCODINTG
  $srv->debug("setting RPC::XML::FORCE_STRING_ENCODING = 1");
  $RPC::XML::FORCE_STRING_ENCODING = 1;

  ##-- load mappers if required
  foreach (@{$srv->{maps}}) {
    if (ref($_) eq 'HASH') {
      my $mh    = $_;
      my $class = $_->{class} || 'DocClassify::Mapper';
      my $file  = $_->{file};
      $srv->logconfess("no 'file' attribute for non-mapper 'maps' element $_") if (!$file);
      $_ = $class->loadFile($file, verboseIO=>1)
	or $srv->logconfess("load failed for mapper file '$file'");
      @$_{keys(%$mh)} = values(%$mh); ##-- override options
      $_->{name} = $file if (!$_->{name});
      $srv->debug("loaded mapper with name='$_->{name}' from file='$_->{file}'");
    }
  }

  ##-- register analysis method(s)
  my ($xp);
  foreach ($srv->xmlRpcMethods) {
    $xp = DocClassify::Server::XmlRpc::Procedure->new($_);
    $xp = $xsrv->add_method($xp);
    if (!ref($xp)) {
      $srv->error("could not register XML-RPC procedure ".(ref($_) ? "$_->{name}()" : "'$_'")."\n",
		  " + RPC::XML::Server error: $xp\n",
		   );
    } else {
      $srv->debug("registered XML-RPC procedure $_->{name}()\n");
    }
  }
  return 1;
}

## $rc = $srv->run()
##  + run the server
sub run {
  my $srv = shift;
  $srv->prepare() if (!$srv->{xsrv}); ##-- sanity check
  $srv->logcroak("run(): no underlying RPC::XML object!") if (!$srv->{xsrv});
  $srv->info("server starting on host ", $srv->{xsrv}->host, ", port ", $srv->{xsrv}->port, "\n");
  $srv->{xsrv}->server_loop(%{$srv->{runopt}});
  $srv->info("server exiting\n");
  return $srv->finish();
}

##==============================================================================
## Methods: Additional
##==============================================================================

## \@procs = $srv->xmlRpcMethods()
##  + returns an array-ref of valid XML-RPC method-specs
##    { name=>$name, code=>$code, signature=>\@sigs, help=>$str, ... }
##  + see RPC::XML(3perl) for details
##  + Signatures are:
##    [ "$returnType1 $argType1_1 $argType1_2 ...", ..., "$returnTypeN ..." ]
##  + known types (see http://www.xmlrpc.com/spec):
##    Tag	          Type                                             Example
##    "i4" or "int"	  four-byte signed integer                         42
##    "boolean"	          0 (false) or 1 (true)                            1
##    "string"	          string                                           hello world
##    "double"            double-precision signed floating point number    -24.7
##    "dateTime.iso8601"  date/time	                                   19980717T14:08:55
##    "base64"	          base64-encoded binary                            eW91IGNhbid0IHJlYWQgdGhpcyE=
##    "struct"            complex structure                                { x=>42, y=>24 }
sub xmlRpcMethods {
  my $srv = shift;
  my $prefix = $srv->{procNamePrefix};

  return
    (
     {
      ##-- Analyze Document
      name => "${prefix}analyzeDocument",
      code => $srv->analyzeDocumentSub,
      signature => [
		    'struct array', 'struct array struct',   ## array ?opts -> struct
		    'struct struct', 'struct struct struct', ## struct ?opts -> struct
		   ],
      help => 'Analyze a single document (array of sentences or struct with "body" array field)',
     },
    );
}

## \&code = $srv->analyzeDocumentSub()
##  + XML-RPC coderef
##  + returns cached $srv->{_analyzeDocumentSub} if defined, otherwise caches it
sub analyzeDocumentSub {
  return $_[0]{_analyzeDocumentSub} if ($_[0]{_analyzeDocumentSub});

  ##-- closure variables
  my $srv = shift;
  my $maps = $srv->{maps};
  my $dcdoc = DocClassify::Document->new(string=>"<doc type=\"dummy\" src=\"$srv\"/>\n",label=>(ref($srv)." dummy document"));
  my $dcsig = DocClassify::Signature->new();
  my $sig_tf = $dcsig->{tf};
  my $sig_Nr = \$dcsig->{N};

  ##-- analysis sub
  my ($indoc,$body,$s,$sl,$w,$wkey, $map);
  my $sub = sub {
    ##-- traverse input document, building signature (ignore any refs in input tokens)
    $indoc = shift;
    $indoc = { body=>$indoc } if (UNIVERSAL::isa($indoc,'ARRAY'));  ##-- DTA::CAB compatibility hack
    foreach $s (@{$indoc->{body}}) {
      $sl = UNIVERSAL::isa($s,'ARRAY') ? $s : $s->{tokens};         ##-- DTA::CAB compatibility hack
      foreach $w (@$sl) {
	$wkey = join("\t", map {"$_=$w->{$_}"} grep {!ref($w->{$_})} sort keys(%$w));
	$sig_tf->{$wkey}++;
	$$sig_Nr++;
      }
    }

    ##-- map & anntoate
    $dcdoc->{sig} = $dcsig;
    foreach $map (@$maps) {
      $map->mapDocument($dcdoc);
      $indoc->{cats} = [ $dcdoc->cats() ];
    }
    #delete($indoc->{body});  ##-- don't bother returning this (except that some DTA::CAB formats puke without it)
    @{$indoc->{body}} = qw(); ##   ... so we just clear it instead

    ##-- cleanup
    @{$dcdoc->{cats}} = qw();
    $dcdoc->clearCache();
    $dcsig->clear();

    ##-- return
    $indoc = DocClassify::Utils::deep_encode($srv->{encoding},$indoc) if ($srv->{encoding});
    return $indoc;
  };

  return $srv->{_analyzeDocumentSub} = $sub;
}


##========================================================================
## PACKAGE: DocClassify::Server::XmlRpc::RPC::XML::Server
##  + wraps RPC::XML::Server, so that system.identity() etc. come out nicely
package DocClassify::Server::XmlRpc::RPC::XML::Server;
use RPC::XML::Server;
use strict;
our @ISA = qw(RPC::XML::Server);

sub version { return "$DocClassify::VERSION+".$_[0]->SUPER::version; }

1;

##========================================================================
## PACKAGE: DocClassify::Server::XmlRpc::Procedure
##  + subclass of RPC::XML::Procedure
package DocClassify::Server::XmlRpc::Procedure;
use RPC::XML::Procedure;
use strict;
our @ISA = ('RPC::XML::Procedure','DocClassify::Logger');

## $proc = CLASS->new(\%methodHash)

## $rv = $proc->call($XML_RPC_SERVER, @PARAMLIST)
sub call {
  $_[0]->debug("$_[0]{name}(): client=".($_[1]{peerhost}||'(unavailable)').")"); #:$_[1]{peerport}
  if (@_ > 3) {
    return $_[0]->SUPER::call(@_[1..($#_-1)],
			      bless({
				     ($_[0]{opts} ? (%{$_[0]{opts}}) : qw()),
				     ($_[$#_]     ? (%{$_[$#_]})     : qw()),
				    },'RPC::XML::struct'),
			     );
  }
  elsif ($_[0]{opts}) {
    return $_[0]->SUPER::call(@_[1..$#_],
			      bless( { %{$_[0]{opts}} },'RPC::XML::struct'),
			     );
  }
  else {
    return $_[0]->SUPER::call(@_[1..$#_]);
  }
}


1; ##-- be happy

__END__

##========================================================================
## POD DOCUMENTATION, auto-generated by podextract.perl, edited

##========================================================================
## NAME
=pod

=head1 NAME

DocClassify::Server::XmlRpc - DocClassify XML-RPC server using RPC::XML

=cut

##========================================================================
## SYNOPSIS
=pod

=head1 SYNOPSIS

 use DocClassify::Server::XmlRpc;
 
 ##========================================================================
 ## Constructors etc.
 
 $srv = DocClassify::Server::XmlRpc->new(%args);
 
 ##========================================================================
 ## Methods: Encoding Hacks
 
 \%rpcProcHash = $srv->wrapMethodEncoding(\%rpcProcHash);
 
 ##========================================================================
 ## Methods: Generic Server API
 
 $rc = $srv->prepareLocal();
 $rc = $srv->run();
 
=cut

##========================================================================
## DESCRIPTION
=pod

=head1 DESCRIPTION

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Server::XmlRpc: Globals
=pod

=head2 Globals

=over 4

=item Variable: @ISA

DocClassify::Server::XmlRpc
inherits from
L<DocClassify::Server|DocClassify::Server>.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Server::XmlRpc: Constructors etc.
=pod

=head2 Constructors etc.

=over 4

=item new

 $srv = $CLASS_OR_OBJ->new(%args);

Constructor.

%args, %$srv:

 ##-- Underlying server
 xsrv => $xsrv,             ##-- low-level server, an RPC::XML::Server object
 xopt => \%opts,            ##-- options for RPC::XML::Server->new()
 xrun => \%opts,            ##-- options for RPC::XML::Server->server_loop()
 ##
 ##-- XML-RPC procedure naming
 procNamePrefix => $prefix, ##-- default: 'dta.cab.'
 ##
 ##-- hacks
 encoding => $enc,          ##-- sets $RPC::XML::ENCODING on prepare(), used by underlying server
 ##
 ##-- (inherited from DocClassify::Server)
 maps => \@maps

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Server::XmlRpc: Methods: Encoding Hacks
=pod

=head2 Methods: Encoding Hacks

=over 4

=item wrapMethodEncoding

 \%rpcProcHash = $srv->wrapMethodEncoding(\%rpcProcHash);

Wraps an RPC::XML::procedure spec into $srv-E<gt>{encoding}-safe code,
only if $rpcProcHash{wrapEncoding} is set to a true value.
This is a hack to which we may need to resort because RPC::XML is so stupid.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Server::XmlRpc: Methods: Generic Server API
=pod

=head2 Methods: Generic Server API

=over 4

=item prepareLocal

 $rc = $srv->prepareLocal();

Subclass-local post-constructor initialization.
Registers analysis methods, generates wrapper closures, etc.
Returns true on success, false otherwise.

=item run

 $rc = $srv->run();

Runs the server.
Doesn't return until the server dies (or is killed).

=back

=cut

##========================================================================
## END POD DOCUMENTATION, auto-generated by podextract.perl

##======================================================================
## Footer
##======================================================================
=pod

=head1 AUTHOR

Bryan Jurish E<lt>jurish@retresco.deE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009-2010 by Bryan Jurish

This package is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.


=cut
