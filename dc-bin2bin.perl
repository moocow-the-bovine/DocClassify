#!/usr/bin/perl -w

use lib qw(. ./MUDL);
use DocClassify;
use IO::File;
use Getopt::Long;

our $ifile = '-';
our $ofile = '-';
our $netorder = 1;

GetOptions(
  'output|out|o=s' => \$ofile,
  'network-order|netorder|net|n!' => \$netorder,
  'local-order|machine-order|machorder|l|m!'    => sub { $netorder=!$_[1]; },
  );

$ifile = shift if (@ARGV);
print STDERR "$0: LOAD($ifile)\n";
my $obj = DocClassify::Object->loadBinFile($ifile)
  or die("$0 DocClassify::Object->loadBinFile() failed for '$ifile': $!");

$ofile = shift if (@ARGV && $ofile eq '-');
print STDERR "$0: SAVE($ofile, netorder=".($netorder ? 1 : 0).")\n";
my $ofh = IO::File->new(">$ofile") or die("$0: open failed for '$ofile': $!");
$ofh->binmode();
if ($netorder) {
  Storable::nstore_fd($obj,$ofh)
    or die("$0: Storable::nstore_fd() failed to '$ofile': $!");
} else {
  Storable::store_fd($obj,$ofh)
    or die("$0: Storable::store_fd() failed to '$ofile': $!");
}
$ofh->close();
