## -*- Mode: CPerl -*-
##
## File: DocClassify::Logger.pm
## Author: Bryan Jurish <jurish@uni-potsdam.de>
## Description: generic logging base class (using Log::Log4perl)

package DocClassify::Logger;
use Carp;
use Log::Log4perl;
use File::Basename;
use strict;

##==============================================================================
## Globals
##==============================================================================

our @ISA = qw();

## $DEFAULT_LOG_CONF = PACKAGE->defaultLogConf(%opts)
##  + default configuration for Log::Log4perl
##  + see Log::Log4perl(3pm), Log::Log4perl::Config(3pm) for details
##  + %opts:
##     rootLevel => $LEVEL_OR_UNDEF,
##     level     => $LEVEL_OR_UNDEF,
sub defaultLogConf {
  my ($that,%opts) = @_;
  $opts{rootLevel} = ($^W ? 'WARN'  : 'FATAL')  if (!exists($opts{rootLevel}));
  $opts{level}     = ($^W ? 'TRACE' : 'DEBUG')  if (!exists($opts{level}));
  my $cfg = "
##-- Loggers
log4perl.oneMessagePerAppender = 1     ##-- suppress duplicate messages to the same appender
".($opts{rootLevel} ? "log4perl.rootLogger = $opts{rootLevel}, AppStderr" : '')."
".($opts{level} ? "log4perl.logger.DocClassify = $opts{level}, AppStderr" : '')."

##-- Appenders: Utilities
log4perl.PatternLayout.cspec.G = sub { return File::Basename::basename(\"$::0\"); }

##-- Appender: AppStderr
log4perl.appender.AppStderr = Log::Log4perl::Appender::Screen
log4perl.appender.AppStderr.stderr = 1
log4perl.appender.AppStderr.layout = Log::Log4perl::Layout::PatternLayout
log4perl.appender.AppStderr.layout.ConversionPattern = %G[%P] %p: %c: %m%n
#log4perl.appender.AppStderr.layout.ConversionPattern = %d{yyyy-MM-dd HH:mm:ss} %G[%P] %p: %c: %m%n
";
  return $cfg;
}

##==============================================================================
## Functions: Initialization
##==============================================================================

## undef = PACKAGE->logInit()             ##-- use default configuration
## undef = PACKAGE->logInit(undef,%opts)  ##-- use default configuration with %opts
## undef = PACKAGE->logInit($file)        ##-- read configuration from a file
## undef = PACKAGE->logInit($file,$watch) ##-- watch configuration file
##  + all log calls in the DTA::CAB should use a subcategory of 'DTA::CAB'
##  + only needs to be called once; see Log::Log4perl->initialized()
sub logInit {
  my ($that,$file,$watch) = @_;
  if (!defined($file)) {
    my $confstr = $that->defaultLogConf(@_[2..$#_]);
    Log::Log4perl::init(\$confstr);
  } elsif (defined($watch)) {
    Log::Log4perl::init_and_watch($file,$watch);
  } else {
    Log::Log4perl::init($file);
  }
  #__PACKAGE__->info("initialized logging facility");
}

## undef = PACKAGE->ensureLog(@args)        ##-- ensure a Log::Log4perl has been initialized
sub ensureLog {
  my $that = shift;
  $that->logInit(@_) if (!Log::Log4perl->initialized);
}

##==============================================================================
## Methods: get logger
##==============================================================================

## $logger = $class_or_obj->logger()
## $logger = $class_or_obj->logger($category)
##  + wrapper for Log::Log4perl::get_logger($category)
##  + $category defaults to ref($class_or_obj)||$class_or_obj
sub logger {
  my $that = (shift || __PACKAGE__);
  $that->ensureLog;
  Log::Log4perl::get_logger(ref($that)||$that);
  ##--
  #Log::Log4perl::get_logger(ref($_[0])||$_[0]);
}

##==============================================================================
## Methods: messages
##==============================================================================

## undef = $class_or_obj->trace(@msg)
##   + be sure you have called Log::Log4perl::init() or similar first
##     - e.g. DTA::CAB::Logger::logInit()
sub trace { $_[0]->logger->trace(@_[1..$#_]); }
sub debug { $_[0]->logger->debug(@_[1..$#_]); }
sub info  { $_[0]->logger->info(@_[1..$#_]); }
sub warn  { $_[0]->logger->warn(@_[1..$#_]); }
sub error { $_[0]->logger->error(@_[1..$#_]); }
sub fatal { $_[0]->logger->fatal(@_[1..$#_]); }

## undef = $class_or_obj->llog($level, @msg)
##  + $level is some constant exported by Log::Log4perl::Level, e.g.:
##     $OFF, $FATAL, $ERROR, $WARN, $INFO, $DEBUG, $TRACE, $ALL
##  + import these with 'use Log::Log4perl::Level qw(:levels);'
sub llog { $_[0]->logger->log(@_[1..$#_]); }

## undef = $class_or_obj->vlog($methodname_or_coderef_or_undef, @msg)
##  + calls $methodname_or_coderef_or_undef($class_or_obj,@msg) if defined
##  + e.g. $class_or_obj->vlog('trace', @msg)
sub vlog {
  return if (!defined($_[1]));
  my $sub = UNIVERSAL::isa($_[1],'CODE') ? $_[1] : UNIVERSAL::can($_[0],$_[1]);
  return if (!defined($sub));
  return $sub->($_[0],@_[2..$#_]);
}

##==============================================================================
## Methods: carp & friends
##==============================================================================

## undef = $class_or_obj->logcroak(@msg)
sub logwarn { $_[0]->logger->logwarn(@_[1..$#_]); }     # warn w/o stack trace
sub logcarp { $_[0]->logger->logcarp(@_[1..$#_]); }     # warn w/ 1-level stack trace
sub logcluck { $_[0]->logger->logcluck(@_[1..$#_]); }   # warn w/ full stack trace

sub logdie { $_[0]->logger->logdie(@_[1..$#_]); }         # die w/o stack trace
sub logcroak { $_[0]->logger->logcroak(@_[1..$#_]); }     # die w/ 1-level stack trace
sub logconfess { $_[0]->logger->logconfess(@_[1..$#_]); } # die w/ full stack trace

1; ##-- be happy

__END__
##========================================================================
## POD DOCUMENTATION, auto-generated by podextract.perl and edited

##========================================================================
## NAME
=pod

=head1 NAME

DocClassify::Logger - DocClassify logging facility using Log::Log4perl

=cut

##========================================================================
## SYNOPSIS
=pod

=head1 SYNOPSIS

 use DocClassify::Logger;
 
 ##========================================================================
 ## Functions: Initialization
 
 PACKAGE->logInit();
 PACKAGE->ensureLog();
 $bool = CLASS_OR_OBJECT->logInitialized();
 
 ##========================================================================
 ## Methods: get logger
 
 $logger = $class_or_obj->logger();
 
 ##========================================================================
 ## Methods: messages
 
 $class_or_obj->trace (@msg);  ##-- 'TRACE'-level message
 $class_or_obj->debug (@msg);  ##-- 'DEBUG'-level message
 $class_or_obj->info  (@msg);  ##-- 'INFO'-level message
 $class_or_obj->warn  (@msg);  ##-- 'WARN'-level message
 $class_or_obj->error (@msg);  ##-- 'ERROR'-level message
 $class_or_obj->fatal (@msg);  ##-- 'FATAL'-level message
 
 $class_or_obj->llog($level, @msg);  ##-- variable-level message (numeric)
 $class_or_obj->vlog($how,   @msg);  ##-- variable-level message (symbolic)
 
 ##========================================================================
 ## Methods: carp, croak & friends
 
 $class_or_obj->logwarn  (@msg);   ##-- warn w/o stack trace
 $class_or_obj->logcarp  (@msg);   ##-- warn w/ 1-level stack trace
 $class_or_obj->logcluck (@msg);   ##-- warn w/ full stack trace
 
 $class_or_obj->logdie    (@msg);  ##-- die w/o stack trace
 $class_or_obj->logcroak  (@msg);  ##-- die w/ 1-level stack trace
 $class_or_obj->logconfess(@msg);  ##-- die w/ full stack trace

=cut

##========================================================================
## DESCRIPTION
=pod

=head1 DESCRIPTION

DocClassify::Logger provides an abstract base class for
object-oriented access to the Log::Log4perl logging facility.

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Logger: Functions: Initialization
=pod

=head2 Functions: Initialization

=over 4

=item logInit

 $PACKAGE->logInit()             ##-- use default configuration
 $PACKAGE->logInit($file)        ##-- read configuration from a file
 $PACKAGE->logInit(\$str)        ##-- read configuration from a string
 $PACKAGE->logInit($file,$watch) ##-- watch configuration file

Initialize the logging facility.

All log calls in the 'DocClassify' namespace should use a subcategory of 'DocClassify'.
This function only needs to be called once; see
L<Log::Log4perl-E<gt>initialized()|Log::Log4perl> for details.

=item ensureLog

 PACKAGE->ensureLog()

Ensures that L<Log::Log4perl|Log::Log4perl> has been initialized.

=item logInitialized

 $bool = CLASS_OR_OBJECT->logInitialized();

Wrapper for L<Log::Log4perl|Log::Log4perl>-E<gt>initialized().

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Logger: Methods: get logger
=pod

=head2 Methods: get logger

=over 4

=item logger

 $logger = $class_or_obj->logger();
 $logger = $class_or_obj->logger($category)

Wrapper for L<Log::Log4perl|Log::Log4perl>::get_logger($category).

$category defaults to ref($class_or_obj)||$class_or_obj

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Logger: Methods: messages
=pod

=head2 Methods: messages

=over 4

=item trace

=item debug

=item info

=item warn

=item error

=item fatal

 $class_or_obj->trace (@msg);  ##-- 'TRACE'-level message
 $class_or_obj->debug (@msg);  ##-- 'DEBUG'-level message
 $class_or_obj->info  (@msg);  ##-- 'INFO'-level message
 $class_or_obj->warn  (@msg);  ##-- 'WARN'-level message
 $class_or_obj->error (@msg);  ##-- 'ERROR'-level message
 $class_or_obj->fatal (@msg);  ##-- 'FATAL'-level message

Log messages at an explicit log-level.

Be sure you have called L<Log::Log4perl|Log::Log4perl>::init() or similar first,
e.g. L<DocClassify::Logger::logInit()|/logInit>.


=item llog

 $class_or_obj->llog($level, @msg);

Log message @msg at log-level $level, which should be a (numeric)
constant exported by Log::Log4perl::Level.

=item vlog

 $class_or_obj->vlog($how, @msg);

Log message @msg at log-level $how, which may be one of the
following:

=over 4

=item *

a CODE reference (e.g. $how=\&logsub) will cause
$how-E<gt>($class_or_obj,@msg) to be called.

=item *

a method name (e.g. $how='trace') will case
calls $class_or_obj-E<gt>${how}(@msg) to be called.

=item *

anything else (e.g. $how='none', $how=undef, ...)
will be ignored.

=back

=back

=cut

##----------------------------------------------------------------
## DESCRIPTION: DocClassify::Logger: Methods: carp & friends
=pod

=head2 Methods: carp, croak & friends

=over 4

=item logwarn

=item logcarp

=item logcluck

 $class_or_obj->logwarn  (@msg);   ##-- warn w/o stack trace
 $class_or_obj->logcarp  (@msg);   ##-- warn w/ 1-level stack trace
 $class_or_obj->logcluck (@msg);   ##-- warn w/ full stack trace

=item logdie

=item logcroak

=item logconfess

 $class_or_obj->logdie    (@msg);  ##-- die w/o stack trace
 $class_or_obj->logcroak  (@msg);  ##-- die w/ 1-level stack trace
 $class_or_obj->logconfess(@msg);  ##-- die w/ full stack trace

=back

=cut

##========================================================================
## END POD DOCUMENTATION, auto-generated by podextract.perl

##======================================================================
## Footer
##======================================================================

=pod

=head1 AUTHOR

Bryan Jurish E<lt>jurish@uni-potsdam.deE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Bryan Jurish

This package is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut
