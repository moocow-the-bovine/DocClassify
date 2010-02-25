#!/usr/bin/perl -w

use lib qw(.);
use DocClassify;
use DocClassify::Server::XmlRpc;
use DocClassify::Utils;
use Encode qw(encode decode);
use File::Basename qw(basename);
use Getopt::Long qw(:config no_ignore_case);
use Cwd qw(getcwd abs_path);
use Pod::Usage;

##==============================================================================
## Constants & Globals
##==============================================================================

##-- program identity
our $prog = basename($0);
our $VERSION = $DocClassify::VERSION;

##-- General Options
our ($help,$man,$version);
our $verbose = 'INFO';   ##-- default log level

no warnings 'utf8';

##-- Server config
our $serverConfigFile = undef;
our $serverHost = undef;
our $serverPort = undef;
our $serverEncoding = undef;

##-- Daemon mode options
our $daemonMode = 0;       ##-- do a fork() ?
our $pidFile  = undef;     ##-- save PID to a file?
our $user = undef;         ##-- if set, switch real & effective uid to $user
our $group = undef;        ##-- if set, switch real & effective gid to $group

##-- Log config
our $logConfigFile = undef;
our $logWatch = undef;

our %logOpts = (
		rootLevel=>'WARN',
		#level=>'TRACE',     ##-- default log level for internal configuration (no 'TRACE' not in log4perl 1.07)
		level=>'INFO',
		stderr=>1,

		file=>undef,
		rotate=>1,

		syslog=>0,
		sysIdent=>basename($0),
		sysName =>basename($0),
		sysFacility=>'daemon',
		sysLevel=>undef,      ##-- default: from 'level' key
	       );

##==============================================================================
## Command-line
GetOptions(##-- General
	   'help|h'    => \$help,
	   'man|m'     => \$man,
	   'version|V' => \$version,
	   #'verbose|v=s' => \$verbose, ##-- see '-log-level' option

	   ##-- Server configuration
	   'config|c=s' => \$serverConfigFile,
	   'bind|b=s'   => \$serverHost,
	   'port|p=i'   => \$serverPort,
	   'encoding|e=s' => \$serverEncoding,

	   ##-- Daemon mode options
	   'daemon|fork|d!'                 => \$daemonMode,
	   'pid-file|pidfile|pid|P=s'  => \$pidFile,
	   'user|uid|u=s' => \$user,
	   'group|gid|g=s' => \$group,

	   ##-- Log4perl stuff
	   'verbose|v|log-level|loglevel|ll|L=s'  => sub { $logOpts{level}=uc($_[1]); },
	   'log-config|logconfig|lc|l=s' => \$logConfigFile,
	   'log-watch|logwatch|watch|lw|w!' => \$logWatch,
	   'log-stderr|stderr|le!' => \$logOpts{stderr},
	   'log-file|lf=s' => \$logOpts{file},
	   'nolog-file|nolf' => sub { delete($logOpts{file}); },
	   'log-rotate|rotate|lr!' => \$logOpts{rotate},
	   'log-syslog|syslog|ls!' => \$logOpts{syslog},
	  );

if ($version) {
  print STDERR
    ("${prog} (DocClassify version $DocClassify::VERSION) by Bryan Jurish <jurish\@retresco.de>\n",
    );
  exit(0);
}

pod2usage({-exitval=>0, -verbose=>1}) if ($man);
pod2usage({-exitval=>0, -verbose=>0}) if ($help);
pod2usage({-exitval=>1, -verbose=>0, -msg=>'No server configuration specified (use -config)!'}) if (!$serverConfigFile);

##==============================================================================
## Subs
##==============================================================================

##--------------------------------------------------------------
## Subs: daemon-mode stuff

## CHLD_REAPER()
##  + lifted from perlipc(1) manpage
sub CHLD_REAPER {
  my $waitedpid = wait();

  ##-- remove pidfile if defined
  unlink($pidFile) if (defined($pidFile) && -r $pidFile);

  # loathe sysV: it makes us not only reinstate

  # the handler, but place it after the wait
  $SIG{CHLD} = \&CHLD_REAPER;
}

##==============================================================================
## MAIN
##==============================================================================

##-- check for daemon mode

##-- log4perl initialization
if (defined($logConfigFile)) {
  DocClassify::Logger->logInit($logConfigFile,$logWatch);
} else {
  DocClassify::Logger->logInit(undef, %logOpts);
}

##-- create / load server object
our $srv = DocClassify::Server::XmlRpc->loadFile($serverConfigFile);
$srv->{user} = $user if (defined($user) && $user ne '');
$srv->{group} = $group if (defined($group) && $group ne '');
$srv->{pidfile} = $pidFile if ($pidFile);
$srv->{xopt}{host} = $serverHost if (defined($serverHost));
$srv->{xopt}{port} = $serverPort if (defined($serverPort));
$srv->{encoding}   = $serverEncoding if (defined($serverEncoding));

##-- serverMain(): main post-preparation code; run in subprocess if we're in daemon mode
sub serverMain {
  ##-- prepare & run server
  $srv->info("serverMain(): initializing server");
  $srv->info("serverMain(): using DocClassify version $DocClassify::VERSION");
  $srv->info("serverMain(): CWD ", abs_path(getcwd));
  $srv->prepare()
    or $srv->logdie("prepare() failed!");
  $srv->run();
  $srv->finish();
  $srv->info("exiting");
}

##-- check for daemon mode
if ($daemonMode) {
  $SIG{CHLD} = \&CHLD_REAPER; ##-- set handler

  if ( ($pid=fork()) ) {
    ##-- parent
    DocClassify->info("spawned daemon subprocess with PID=$pid\n");
  } else {
    ##-- daemon-child
    DocClassify->logdie("$prog: fork() failed: $!") if (!defined($pid));
    serverMain();
  }
} else {
  ##-- just run server
  serverMain();
}

__END__
=pod

=head1 NAME

dc-xmlrpc-server.perl - XML-RPC server for DocClassify queries

=head1 SYNOPSIS

 dc-xmlrpc-server.perl [OPTIONS...]

 General Options:
  -help                           ##-- show short usage summary
  -man                            ##-- show longer help message
  -version                        ##-- show version & exit
  -verbose LEVEL                  ##-- really just an alias for -log-level=LEVEL

 Server Configuration Options:
  -config PLFILE                  ##-- load server config from PLFILE
  -bind   HOST                    ##-- override host to bind (default=all)
  -port   PORT                    ##-- override port to bind (default=8088)
  -encoding ENCODING              ##-- override server encoding (default=UTF-8)

 Daemon Mode Options:
  -fork , -nofork                 ##-- do/don't fork() a server subprocess
  -pidfile PIDFILE                ##-- save server PID to PIDFILE
  -user USER                      ##-- run server setuid as USER
  -group GROUP                    ##-- run server setgid as GROUP

 Logging Options:                 ##-- see Log::Log4perl(3pm)
  -log-level LEVEL                ##-- set minimum log level (default=INFO)
  -log-stderr , -nolog-stderr     ##-- do/don't log to stderr (default=true)
  -log-syslog , -nolog-syslog     ##-- do/don't log to syslog (default=false)
  -log-file LOGFILE               ##-- log directly to FILE (default=none)
  -log-rotate , -nolog-rotate     ##-- do/don't auto-rotate log files (default=true)
  -log-config L4PFILE             ##-- log4perl config file (overrides -log-stderr, etc.)
  -log-watch  , -nowatch          ##-- do/don't watch log4perl config file (default=false)

=cut

##==============================================================================
## Description
##==============================================================================
=pod

=head1 DESCRIPTION

dc-xmlrpc-server.perl starts an XML-RPC
server to perform L<DocClassify|DocClassify> document classification
using the L<DocClassify::Server::XmlRpc|DocClassify::Server::XmlRpc>
module.

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

=item -verbose LEVEL

Alias for L<-log-level LEVEL>.

=back

=cut

##==============================================================================
## Options: Server Configuration Options
=pod

=head2 Server Configuration Options

=over 4

=item -config PLFILE

Load server configuration from PLFILE,
which should be a perl source file parseable
by L<DocClassify::Object::loadFile()|DocClassify::Object/item_loadFile>
as a L<DocClassify::Server::XmlRpc|DocClassify::Server::XmlRpc> object.

=item -bind HOST

Override host on which to bind server socket.
Default is to bind on all interfaces of the current host.

=item -port PORT

Override port number to which to bind the server socket.
Default is whatever
L<DocClassify::Server::XmlRpc|DocClassify::Server::XmlRpc>
defaults to (usually 9099).

=item -encoding ENCODING

Override server encoding.
Default=UTF-8.

=back

=cut

##==============================================================================
## Options: Daemon Mode Options
=pod

=head2 Daemon Mode Options

=over 4

=item -fork , -nofork

Do/don't fork() a server subprocess (default: don't).
If running in daemon mode, the program should simply spawn
a single server subprocess and exit, reporting the PID
of the child process.

Useful for starting persistent servers from system-wide
init scripts.  See also the other options in this section.

=item -pidfile PIDFILE

Writes PID of the server process to PIDFILE before running the server.
PIDFILE and the directory containing it must be writable by the
user invoking the server.

=item -user USER

Run server setuid (real and effective) as USER, which may
be either a user name as accepted by getpwnam() or a numeric UID.
Useful if the server is invoked by root, e.g. via a system init script,
to distinguish between the priviledged user (e.g. root) I<invoking>
the server and a non-privileged user (e.g. daemon) associated
with the I<running> server.

Caveat: running setuid is inherently dangerous.  Use of the
C<-user> and C<-group> options should serve to eliminate the
worst security risks associated with invoking this server as
root, but some risk remains (exactly how much depends on your
system, kernel, etc.).  The truly paranoid will ignore these
options altogether and just invoke the server as a non-privileged
user in the first place.

=item -group GROUP

Run server setgid (real and effective) as GROUP, which may be
either a group name as accepted by getgrnam() or a numeric GID.
See the caveats above under L</"-user USER">.

=back

=cut

##==============================================================================
## Options: Logging Options
=pod

=head2 Logging Options

The L<DocClassify|DocClassify> family of modules uses
the Log::Log4perl logging mechanism.
See L<Log::Log4perl(3pm)|Log::Log4perl> for details
on the general logging mechanism.

=over 4

=item -log-level LEVEL

Set minimum log level.  Has no effect if you also specify L</-log-config>.
Known levels: (trace|debug|info|warn|error|fatal).

=item -log-stderr , -nolog-stderr

Do/don't send log output log to stderr (default=true).

=item -log-syslog , -nolog-syslog

Do/don't send log output log to syslog (default=false).

=item -log-file LOGFILE

Send log output directly to LOGFILE (default=none).
LOGFILE (and the directory containing it, if it doesn't yet exist or if you're
using the -log-rotate option) must be writable by the
user owning the running server - see the L</"-user"> and L</"-group"> options
for details.

=item -log-rotate , -nolog-rotate

Do/don't auto-rotate log files (default=true).
Auto-rotation only works if Log::Dispatch::FileRotate is installed on your system.

=item -log-config L4PFILE

User log4perl config file L4PFILE.
Default behavior uses the log configuration
string returned by L<DocClassify::Logger-E<gt>defaultLogConf()|DocClassify::Logger/item_defaultLogConf>.
If specified, the log4perl configuration file L4PFILE overrides
the logging options set with e.g. the -log-stderr, -log-syslog, -log-file, etc. options.

=item -log-watch , -nowatch

Do/don't watch log4perl config file (default=don't).
Only sensible if you also specify L</-log-config>.

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

Bryan Jurish E<lt>jurish@retresco.deE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009-2010 by Bryan Jurish

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=head1 SEE ALSO

L<DocClassify(3pm)|DocClassify>,
L<RPC::XML(3pm)|RPC::XML>,
L<perl(1)|perl>,
...

=cut
