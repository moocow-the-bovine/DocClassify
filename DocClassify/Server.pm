## -*- Mode: CPerl -*-
##
## File: DocClassify::Server.pm
## Author: Bryan Jurish <jurish@uni-potsdam.de>
## Description: abstract class for DocClassify servers

package DocClassify::Server;
use DocClassify;
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
##     #...
##    }
sub new {
  my $that = shift;
  my $obj = bless({
		   maps => [],
		   #pidfile=>undef,
		   #pid=>$pid,
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

  ##-- prepare: PID file
  if (defined($srv->{pidfile})) {
    my $pidfh = IO::File->new(">$srv->{pidfile}")
      or $srv->logconfess("prepare(): could not write PID file '$srv->{pidfile}': $!");
    $pidfh->print(($srv->{pid} || $$), "\n");
    $pidfh->close()
  }

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
  foreach $sig (qw(HUP TERM KILL __DIE__)) {
    $SIG{$sig} = $catcher;
  }
  return $catcher;
}

## $rc = $srv->prepareLocal(@args_to_prepare)
##  + subclass-local initialization
##  + called by prepare() after default prepare() guts have run
sub prepareLocal { return 1; }


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
  unlink($srv->{pidfile}) if ($srv->{pidfile});
  delete @SIG{qw(HUP TERM KILL __DIE__)}; ##-- unset signal handlers
  return 1;
}

1; ##-- be happy
