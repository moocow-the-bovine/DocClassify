#!/usr/bin/perl -w

use lib qw(.);
use DocClassify;
use DocClassify::Client::XmlRpc;
use DocClassify::Utils ':all';
use Encode qw(encode decode);
use File::Basename qw(basename);
use Getopt::Long qw(:config no_ignore_case);
use Time::HiRes qw(gettimeofday tv_interval);
use Data::Dumper;
use Pod::Usage;

##==============================================================================
## DEBUG
##==============================================================================
#do "storable-debug.pl" if (-f "storable-debug.pl");

##==============================================================================
## Constants & Globals
##==============================================================================

##-- program identity
our $prog = basename($0);
our $VERSION = $DocClassify::VERSION;

##-- General Options
our ($help,$man,$version,$verbose);
#$verbose = 'default';

##-- Log options
our %logOpts = (rootLevel=>'WARN', level=>'INFO'); ##-- options for DocClassify::Logger::ensureLog()

##-- Server Options
our $defaultPort = 9099;
our $serverURL  = "http://localhost:${defaultPort}";
our $serverEncoding = 'UTF-8';
our $localEncoding  = 'UTF-8';
our $timeout = 65535;   ##-- wait for a *long* time (65535 = 2**16-1 ~ 18.2 hours)

##-- Analysis & Action Options
our $analyzer = 'dc';
our $action = 'document';
our @req    = qw(); ##-- request for 'request' mode
our $doProfile = 1;

##-- I/O Options
our $outfile     = '-';

our $bench_iters = 1; ##-- number of benchmark iterations for -bench mode

##==============================================================================
## Command-line
GetOptions(##-- General
	   'help|h'    => \$help,
	   'man|m'     => \$man,
	   'version|V' => \$version,
	   'verbose|v|log-level=s' => sub { $logOpts{level}=uc($_[1]); },

	   ##-- Server Options
	   'server-url|serverURL|server|url|s|u=s' => \$serverURL,
	   'local-encoding|le=s'  => \$localEncoding,
	   'server-encoding|se=s' => \$serverEncoding,
	   'timeout|T=i' => \$timeout,

	   ##-- Analysis Options
	   'analyzer|a=s' => \$analyzer,
	   'profile|p!' => \$doProfile,
	   'list-methods|list|lm|l'   => sub { $action='request'; @req=('system.listMethods'); },
	   'status|st' => sub { $action='request'; @req=('system.status'); },
	   'request|req|r=s@' => sub { $action='request'; push(@req,$_[1]); },
	   'document|doc|d' => sub { $action='document'; },
	   'corpus|c' => sub { $action='corpus'; },
	   'bench|b:i' => sub { $bench_iters=$_[1]; },

	   ##-- I/O: output
	   'output-file|output|o=s' => \$outfile,
	  );

if ($version) {
  print STDERR
    ("${prog} (DocClassify version $DocClassify::VERSION) by Bryan Jurish <jurish\@uni-potsdam.de>\n",
    );
  exit(0);
}

pod2usage({-exitval=>0, -verbose=>1}) if ($man);
pod2usage({-exitval=>0, -verbose=>0}) if ($help);


##==============================================================================
## MAIN
##==============================================================================

##-- log4perl initialization
DocClassify::Logger->ensureLog(undef,%logOpts);

##-- sanity checks
$serverURL = "http://$serverURL" if ($serverURL !~ m|[^:]+://|);
if ($serverURL =~ m|([^:]+://[^/:]*)(/[^:]*)$|) {
  $serverURL = "$1:${defaultPort}$2";
}

##-- create client object
our $cli = DocClassify::Client::XmlRpc->new(
					    serverURL=>$serverURL,
					    serverEncoding=>$serverEncoding,
					    timeout=>$timeout,
					   );
$cli->connect() or die("$0: connect() failed: $!");


##======================================================
## Input & Output Formats

##-- output file
our $outfh = IO::File->new(">$outfile")
  or die("$0: open failed for output file '$outfile': $!");

##======================================================
## Profiling: see DocClassify::Utils ':profile' methods

our $ndocs = 0;
profile_start();

##======================================================
## Subs: Actions

## global response(s)
our $rsp = undef;

## \@docs = files2docs(\@files)
sub files2docs {
  return [map {DocClassify::Document->new(file=>$_)} @{$_[0]}];
}

## \@docs = corpus2docs($corpusFile)
sub corpus2docs {
  my $cfile = shift;
  my $corpus = DocClassify::Corpus->loadFile($cfile);
  return $corpus->{docs};
}

## \@docs = expandDocs(\@docs)
##  + expands $doc->{cabdoc} for each $doc in \@docs
sub expandDocs {
  foreach (@{$_[0]}) {
    $_->{cabdoc} = DocClassify::Client::XmlRpc::sig2cabdoc($_->typeSignature)
      if (!$_->{cabdoc});
  }
  return $_[0];
}

## \@docs = queryDocs($cli,\@docs)
##  + runs query for docs \@docs, sets response in $doc->{rsp}
sub queryDocs {
  my ($cli,$docs) = @_;
  foreach (@$docs) {
    $_->{rsp} = $cli->analyzeCabDocument($analyzer, $_->{cabdoc});
  }
  return $docs;
}

##======================================================
## Main: Actions

if ($action eq 'request') {
  ##-- action: literal request
  $rsp = $cli->sendRequest(@req);
}
elsif ($action eq 'document') {
  ##-- action: 'document': interpret args as filenames & parse 'em!
  profile_stop();
  my $docs = expandDocs(files2docs([@ARGV]));
  profile_start();
  queryDocs($cli,$docs) for (1..$bench_iters);
  profile_stop();
  $rsp = $#$docs==0 ? $docs->[0]{rsp} : [map { {file=>$_->{file},rsp=>$_->{rsp}} } @$docs];
  $ndocs += $bench_iters * @$docs;
}
elsif ($action eq 'corpus') {
  ##-- action: 'corpus': interpret args as corpus filenames
  profile_stop();
  foreach my $corpusfile (@ARGV) {
    my $docs = expandDocs(corpus2docs($corpusfile));
    profile_start();
    queryDocs($cli,$docs) for (1..$bench_iters);
    profile_stop();
    push(@$rsp, $#$docs==0 ? $docs->[0]{rsp} : (map { {file=>$_->{file},rsp=>$_->{rsp}} } @$docs));
    $ndocs += $bench_iters * @$docs;
  }
}
else {
  die("$0: unknown action '$action'");
}
$cli->disconnect();

##-- dump output
$outfh->print(Data::Dumper->new([$rsp],['$rsp'])->Indent(1)->Dump)
  if ($bench_iters < 2);

if ($doProfile && $ndocs>0) {
  print STDERR "$prog: ", profile_string($ndocs), "\n";
}

__END__
=pod

=head1 NAME

dc-xmlrpc-client.perl - XML-RPC client for DocClassify server queries

=head1 SYNOPSIS

 dc-xmlrpc-client.perl [OPTIONS...] ARGUMENTS

 General Options:
  -help                           ##-- show short usage summary
  -man                            ##-- show longer help message
  -version                        ##-- show version & exit
  -verbose LEVEL                  ##-- set default log level
  -bench ITERS                    ##-- benchmark selected action ITERS times

 Server Options:
  -server URL                     ##-- set server URL (default: http://localhost:9099)
  -server-encoding ENCODING       ##-- set server encoding (default: UTF-8)
  -local-encoding ENCODING        ##-- set local encoding (default: UTF-8)
  -timeout SECONDS                ##-- set server timeout in seconds (default: lots)

 Analysis Options:
  -analyzer NAME                  ##-- set analyzer name (default: 'dc')
  -list                           ##-- just dump registered methods (default)
  -status                         ##-- just dump system status
  -profile , -noprofile           ##-- do/don't report profiling information (default: do)
  -document                       ##-- ARGUMENTS are document filenames
  -corpus                         ##-- ARGUMENTS are corpus filenames

 I/O Options:
  -output-file FILE               ##-- set output file (default: STDOUT)

=cut

##==============================================================================
## Description
##==============================================================================
=pod

=head1 DESCRIPTION

dc-xmlrpc-client.perl is a command-line client for L<DocClassify|DocClassify>
analysis of token(s), sentence(s), and/or document(s) by
querying a running L<DocClassify::Server::XmlRpc|DocClassify::Server::XmlRpc> server
with the L<DocClassify::Client::XmlRpc|DocClassify::Client::XmlRpc> module.

See L<dc-xmlrpc-server.perl(1)|dc-xmlrpc-server.perl> for a
corresponding server.

=cut

##==============================================================================
## Options and Arguments
##==============================================================================
=pod

=head1 OPTIONS AND ARGUMENTS

=cut

##==============================================================================
## Options: General Options
=pod

=head2 General Options

=over 4

=item -help

Display a short help message and exit.

=item -man

Display a longer help message and exit.

=item -version

Display program and module version information and exit.

=item -verbose

Set default log level (trace|debug|info|warn|error|fatal).

=back

=cut

##==============================================================================
## Options: Server Options
=pod

=head2 Server Options

=over 4

=item -server URL

Set server URL (default: localhost:9099).

=item -server-encoding ENCODING

Set server encoding (default: UTF-8).

=item -local-encoding ENCODING

Set local encoding (default: UTF-8).

=item -timeout SECONDS

Set server timeout in seconds (default: lots).

=back

=cut

##==============================================================================
## Options: Analysis Options
=pod

=head2 Analysis Options

=over 4

=item -list

Don't actually perform any analysis;
rather,
just print a list of analyzers registered with the server.
This is the default action.

=item -analyzer NAME

Request analysis by the analyzer registered under name NAME (default: 'dta.cab.default').

=item -analyze-option OPT=VALUE

Set an arbitrary analysis option C<OPT> to C<VALUE>.
May be multiply specified.

Available options depend on the analyzer class to be called.

=item -profile , -noprofile

Do/don't report profiling information (default: do).

=item -token

Interpret ARGUMENTS as token text.

=item -sentence

Interpret ARGUMENTS as a sentence (list of tokens).

=item -document

Interpret ARGUMENTS as filenames, to be analyzed as documents.

=item -raw

Interpret ARGUMENTS as filenames (as for L</-document>),
but file contents are passed as raw strings to the server,
which then becomes responsible for parsing and formatting.

This is the recommended way to analyze large documents,
because of the large overhead
involved when the L</-document> option is used
(slow translations to and from complex XML-RPC structures).

=back

=cut

##==============================================================================
## Options: I/O Options
=pod

=head2 I/O Options

=over 4

=item -input-class CLASS

Select input parser class (default: Text)

=item -input-option OPT=VALUE

Set an arbitrary input parser option.
May be multiply specified.

=item -output-class CLASS

Select output formatter class (default: Text)
May be multiply specified.

=item -output-option OPT=VALUE

Set an arbitrary output formatter option.
May be multiply specified.

=item -output-encoding ENCODING

Override output encoding (default: -local-encoding).

=item -output-level LEVEL

Override output formatter level (default: 1).

=item -output-file FILE

Set output file (default: STDOUT).

=back

=cut


##======================================================================
## Footer
##======================================================================
=pod

=head1 ACKNOWLEDGEMENTS

Perl by Larry Wall.

RPC::XML by Randy J. Ray.

=head1 AUTHOR

Bryan Jurish E<lt>jurish@uni-potsdam.deE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009-2010 by Bryan Jurish

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=head1 SEE ALSO

L<dc-xmlrpc-server.perl(1)|dc-xmlrpc-server.perl>,
L<DocClassify(3pm)|DocClassify>,
L<RPC::XML(3pm)|RPC::XML>,
L<perl(1)|perl>,
...

=cut
