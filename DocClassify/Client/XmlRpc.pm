## -*- Mode: CPerl -*-
##
## File: DocClassify::Client::XmlRpc.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description: DocClassify XML-RPC server clients

package DocClassify::Client::XmlRpc;
use DocClassify;
use DocClassify::Client;
use RPC::XML;
use RPC::XML::Client;
use DocClassify::Utils ':all';
use Carp;
use strict;

##==============================================================================
## Globals
##==============================================================================

our @ISA = qw(DocClassify::Client);

##==============================================================================
## Constructors etc.
##==============================================================================

## $obj = CLASS_OR_OBJ->new(%args)
##  + object structure: HASH ref
##    {
##     ##-- server
##     serverURL => $url,             ##-- default: localhost:9099
##     serverEncoding => $encoding,   ##-- default: UTF-8
##     timeout => $timeout,           ##-- timeout in seconds, default: 300 (5 minutes)
##
##     ##-- underlying RPC::XML client
##     xcli => $xcli,                 ##-- RPC::XML::Client object
##    }
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   ##-- server
			   serverURL      => 'http://localhost:9099',
			   serverEncoding => 'UTF-8', ##-- default server encoding
			   timeout => 300,
			   ##
			   ##-- RPC::XML stuff
			   xcli => undef,
			   xargs => {
				     compress_requests => 0,    ##-- send compressed requests?
				     compress_thresh   => 8192, ##-- byte limit for compressed requests
				     ##
				     message_file_thresh => 0,  ##-- disable file-based message spooling
				    },
			   ##
			   ##-- user args
			   @_,
			  );
}

##==============================================================================
## Methods: Generic Client API: Connections
##==============================================================================

## $bool = $cli->connected()
sub connected {
  my $cli = shift;
  return 0 if (!$cli->{xcli});

  ##-- send a test query (system.identity())
  my $req = $cli->newRequest('system.identity');
  my $rsp = $cli->{xcli}->send_request( $req );
  return ref($rsp) ? 1 : 0;
}

## $bool = $cli->connect()
##  + establish connection
sub connect {
  my $cli = shift;
  $cli->{xcli} = RPC::XML::Client->new($cli->{serverURL}, %{$cli->{xargs}})
    or $cli->logdie("could not create underlying RPC::XML::Client: $!");
  $cli->{xcli}->message_file_thresh(0)
    if (defined($cli->{xargs}{message_file_thresh}) && !$cli->{xargs}{message_file_thresh});
  $cli->{xcli}->useragent->timeout($cli->{timeout})
    if (defined($cli->{timeout}));
  return $cli->connected();
}

## $bool = $cli->disconnect()
sub disconnect {
  my $cli = shift;
  delete($cli->{xcli});
  return 1;
}

##==============================================================================
## Methods: Utils
##==============================================================================

## $rsp_or_error = $cli->request($req)
## $rsp_or_error = $cli->request($req, $doDeepEncoding=1)
##  + send XML-RPC request, log if error occurs
sub request {
  my ($cli,$req,$doRecode) = @_;

  ##-- cache RPC::XML encoding
  $doRecode   = 1 if (!defined($doRecode));
  my $enc_tmp = $RPC::XML::ENCODING;
  $RPC::XML::ENCODING = $cli->{serverEncoding};

  $cli->connect() if (!$cli->{xcli});
  $req = DocClassify::Utils::deep_encode($cli->{serverEncoding}, $req) if ($doRecode);

  ##-- workaround for RPC::XML <-> LWP::Protocol::http conflict wrt Content-Length
  ##   + RPC::XML uses byte length
  ##   + LWP::Protocol::http uses character length (argh!)
  local $SIG{__WARN__} = sub { warn(@_) unless (join('',@_) =~ /^Content-Length header value was wrong/); };

  my ($rsp) = $cli->{xcli}->send_request( $req );
  if (!ref($rsp)) {
    $cli->error("RPC::XML::Client::send_request() failed:");
    $cli->error($rsp);
  }
  elsif ($rsp->is_fault) {
    $cli->error("XML-RPC fault (".$rsp->code.") ".$rsp->string);
  }

  ##-- cleanup & return
  $RPC::XML::ENCODING = $enc_tmp;
  return $doRecode ? DocClassify::Utils::deep_decode($cli->{serverEncoding}, $rsp) : $rsp;
}


##==============================================================================
## Methods: Generic Client API: Queries
##==============================================================================

## $req = $cli->newRequest($methodName, @args)
##  + returns new RPC::XML::request
##  + encodes all elementary data types as strings
sub newRequest {
  my ($cli,$method,@args) = @_;
  my $str_tmp = $RPC::XML::FORCE_STRING_ENCODING;
  $RPC::XML::FORCE_STRING_ENCODING = 1;
  my $req = RPC::XML::request->new($method,@args);
  $RPC::XML::FORCE_STRING_ENCODING = $str_tmp;
  return $req;
}

## $rsp = $cli->sendRequest($req)
## $rsp = $cli->sendRequest($method,@args)
sub sendRequest {
  my $cli = shift;
  my ($req);
  if (@_==1 && ref($_[0])) {
    $req = shift;
  } else {
    $req = $cli->newRequest(@_);
  }
  my $rsp = $cli->request($req);
  return ref($rsp) && !$rsp->is_fault ? $rsp->value : $rsp;
}

## $rsp = $cli->analyzeDocument($analyzer, $doc, \%opts)
sub analyzeDocument {
  my ($cli,$aname,$doc,$opts) = @_;
  return $cli->analyzeSignature($cli,$aname,$doc->typeSignature,$opts);
}

## $rsp = $cli->analyzeSignature($aname, $sig, \%opts)
sub analyzeSignature {
  my ($cli,$aname,$sig,$opts) = @_;
  return $cli->analyzeCabDocument($cli,$aname,sig2cabdoc($sig),$opts);
}

## $rsp = $cli->analyzeCabDocument($aname, $cabdoc, \%opts)
sub analyzeCabDocument {
  my ($cli,$aname,$cdoc,$opts) = @_;
  my $rsp = $cli->request($cli->newRequest("$aname.analyzeDocument",
					   $cdoc,
					   (defined($opts) ? $opts : qw())
					  ));
  return ref($rsp) && !$rsp->is_fault ? $rsp->value : $rsp;
}


##==============================================================================
## HACKS: convert signature to DTA::CAB-style document
##==============================================================================

## \%cabdoc = sig2cabdoc($sig)
sub sig2cabdoc {
  my $sig  = shift;
  my $toks = [];
  my ($y,$f,$ya);
  while (($y,$f)=each(%{$sig->{tf}})) {
    $ya = {map {split(/=/,$_,2)} split(/\t/,$y)};
    push(@$toks, map {$ya} (1..$f));
  }
  return {body=>[{tokens=>$toks}]};
}


1; ##-- be happy

__END__

##========================================================================
## POD DOCUMENTATION, auto-generated by podextract.perl, edited

##========================================================================
## NAME
=pod

=head1 NAME

DocClassify::Client::XmlRpc - DocClassify XML-RPC server clients

=cut

##========================================================================
## SYNOPSIS
=pod

=head1 SYNOPSIS

 use DocClassify::Client::XmlRpc;
 
 ##========================================================================
 ## Constructors etc.
 
 $cli = DocClassify::Client::XmlRpc->new(%args);
 
 ##========================================================================
 ## Methods: Generic Client API: Connections
 
 $bool = $cli->connected();
 $bool = $cli->connect();
 $bool = $cli->disconnect();
 
 ##========================================================================
 ## Methods: Utils
 
 $rsp_or_error = $cli->request($req);
 
 ##========================================================================
 ## Methods: Generic Client API: Queries
 
 $req  = $cli->newRequest($methodName, @args);
 $rsp  = $cli->analyzeDocument($analyzer, $doc, \%opts);
 $rsp  = $cli->analyzeSignature($analyzer, $sig, \%opts);
 $rsp  = $cli->analyzeCabDocument($analyzer, $cabdoc, \%opts);

=cut

##========================================================================
## DESCRIPTION
=pod

=head1 DESCRIPTION

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Client::XmlRpc: Globals
=pod

=head2 Globals

=over 4

=item Variable: @ISA

DocClassify::Client::XmlRpc
inherits from
L<DocClassify::Client|DocClassify::Client>.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Client::XmlRpc: Constructors etc.
=pod

=head2 Constructors etc.

=over 4

=item new

 $cli = CLASS_OR_OBJ->new(%args);

Constructor.

%args, %$cli:

 ##-- server selection
 serverURL      => $url,         ##-- default: localhost:8000
 serverEncoding => $encoding,    ##-- default: UTF-8
 timeout        => $timeout,     ##-- timeout in seconds, default: 300 (5 minutes)
 ##
 ##-- underlying RPC::XML client
 xcli           => $xcli,        ##-- RPC::XML::Client object

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Client::XmlRpc: Methods: Generic Client API: Connections
=pod

=head2 Methods: Generic Client API: Connections

=over 4

=item connected

 $bool = $cli->connected();

Override: returns true iff $cli is connected to a server.

=item connect

 $bool = $cli->connect();

Override: establish connection to the selected server.

=item disconnect

 $bool = $cli->disconnect();

Override: close current server connection, if any.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Client::XmlRpc: Methods: Utils
=pod

=head2 Methods: Utils

=over 4

=item request

 $rsp_or_error = $cli->request($req);
 $rsp_or_error = $cli->request($req, $doDeepEncoding=1)

Send an XML-RPC request $req, log if error occurs.

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Client::XmlRpc: Methods: Generic Client API: Queries
=pod

=head2 Methods: Generic Client API: Queries

=over 4

=item newRequest

 $req = $cli->newRequest($methodName, @args);

Returns new RPC::XML::request for $methodName(@args).
Encodes all atomic data types as strings

=item analyzeDocument

 $rsp = $cli->analyzeDocument($analyzer, $doc, \%opts);

Override: server-side document analysis.  Wraps analyzeSignature().

=item analyzeSignature

 $rsp = $cli->analyzeSignature($analyzer, $sig, \%opts);

Analyze a document by signature.  Wraps analyzeCabDocument().

=item analyzeCabDocument

  $rsp = $cli->analyzeCabDocument($analyzer, $cabdoc, \%opts);

Analyze a DTA::CAB-style document.  Guts for analyzeDocument() and analyzeSignature().

=back

=cut

##========================================================================
## END POD DOCUMENTATION, auto-generated by podextract.perl

##======================================================================
## Footer
##======================================================================

=pod

=head1 AUTHOR

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009-2010 by Bryan Jurish

This package is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut
