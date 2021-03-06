## -*- Mode: CPerl -*-
##
## File: DocClassify::Server.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description: abstract class for DocClassify servers

package DocClassify::Server;
use DocClassify;
use DocClassify::Utils (':perms');
use English;
use IO::File;
use Carp;
use strict;

##==============================================================================
## Globals
##==============================================================================

our @ISA = qw(DocClassify::Object DocClassify::Logger);

##==============================================================================
## Constructors etc.
##==============================================================================

## $obj = CLASS_OR_OBJ->new(%args)
##  + object structure: HASH ref
##    {
##     maps => \@maps,       ##-- chain of DocClassify::Mapper objects
##     pidfile => $pidfile,  ##-- if defined, process PID will be written to $pidfile on prepare()
##     pid => $pid,          ##-- server PID (default=$$) to write to $pidfile
##     wrotepid => $bool,    ##-- true if this instance has written the $pidfile
##     user => $user,        ##-- set real & effective user ids to $user (if defined)
##     group => $group,      ##-- set real & effective group ids to $group (if defined)
##     #...
##    }
sub new {
  my $that = shift;
  my $obj = bless({
		   maps => [],
		   #pidfile=>undef,
		   #pid=>$$,
		   #wrotepid=>0,
		   ##
		   ##-- user args
		   @_
		  },
		  ref($that)||$that);
  $obj->initialize();
  return $obj;
}

## undef = $obj->initialize()
##  + called to initialize new objects after new()
sub initialize { return $_[0]; }

##==============================================================================
## Methods: Generic Server API
##==============================================================================

## $rc = $srv->prepare()
##  + default implementation initializes logger & pre-loads all maps
sub prepare {
  my $srv = shift;
  my $rc  = 1;

  ##-- prepare: logger
  DocClassify::Logger->ensureLog();

  ##-- prepare: mappers (NYI)
  #foreach (@{$srv->{maps}}) {
  #  $srv->info("initializing mapper '".$_->name."'");
  #  if (!$_->ensureLoaded) {
  #    $srv->logconfess("initialization failed for analyzer '".$_->name."'; skipping");
  #    $rc = 0;
  #  }
  #}
  ##-- check: mappers
  if (!$srv->{maps} || !@{$srv->{maps}}) {
    $srv->logconfess("prepare(): no mapers defined!");
  }
  @{$srv->{maps}} = grep {defined($_)} @{$srv->{maps}};

  ##-- prepare: signal handlers
  $rc &&= $srv->prepareSignalHandlers();

  ##-- prepare: subclass-local
  $rc &&= $srv->prepareLocal(@_);

  ##-- prepare: setgid, setuid
  if (defined($srv->{group})) {
    $srv->debug("before setgids($srv->{group}): EGID=".($EGID+0).", GID=".($GID+0));
    $rc &&= defined(setgids($srv->{group}));
    $srv->info("after setgids($srv->{group}): EGID=".($EGID+0).", GID=".($GID+0));
  }
  if (defined($srv->{user})) {
    $srv->debug("before setuids($srv->{user}): EUID=".($EUID+0).", UID=".($UID+0));
    $rc &&= defined(setuids($srv->{user}));
    $srv->info("after setuids($srv->{user}): EUID=".($EUID+0).", UID=".($UID+0));
  }

  ##-- log & return
  $srv->debug("initialization complete");

  return $rc;
}

## $rc = $srv->prepareSignalHandlers()
##  + initialize signal handlers
sub prepareSignalHandlers {
  my $srv = shift;
  my $catcher = sub {
    my $signame = shift;
    $srv->finish();
    $srv->fatal("caught signal '$signame' - exiting");
    exit(255);
  };
  my ($sig);
  foreach $sig (qw(INT HUP TERM KILL __DIE__)) {  ##-- INT signal (Ctrl+C) doesn't seem to get caught...
    $SIG{$sig} = $catcher;
  }
  return $catcher;
}

## $rc = $srv->prepareLocal(@args_to_prepare)
##  + subclass-local initialization
##  + called by prepare() after default prepare() guts have run
sub prepareLocal { return 1; }

## $bool = $srv->writePidFile()
## $bool = $srv->writePidFile($pid)
##  + writes $srv->{pidfile} if defined
sub writePidFile {
  my $srv = shift;
  $srv->{pid} = shift if (@_);
  $srv->{pid} = $$ if (!$srv->{pid});
  if (defined($srv->{pidfile})) {
    my $pidfh = IO::File->new(">$srv->{pidfile}")
      or $srv->logconfess("writePidFile($srv->{pid}): could not write PID to file '$srv->{pidfile}': $!");
    $pidfh->print($srv->{pid}, "\n");
    $pidfh->close();
    $srv->debug("saved PID=$srv->{pid} to file '$srv->{pidfile}'");
    $srv->{wrotepid}=1;
  }
  return 1;
}

## $rc = $srv->run()
##  + run the server (just a dummy method)
sub run {
  my $srv = shift;
  $srv->logcroak("run() method not implemented!");
  $srv->finish(); ##-- cleanup
}

## $rc = $srv->finish()
##  + cleanup method; should be called when server dies or after run() has completed
sub finish {
  my $srv = shift;
  unlink($srv->{pidfile}) if ($srv->{pidfile} && $srv->{wrotepid});
  delete @SIG{qw(HUP TERM KILL __DIE__)}; ##-- unset signal handlers
  return 1;
}

1; ##-- be happy
