#!/bin/bash

## + requires perl-reversion from Perl::Version (debian package libperl-version-perl)
## + example call:
##    ./reversion.sh -bump -dryrun

pmfiles=(./Makefile.PL ./DocClassify.pm)

exec perl-reversion "$@" "${pmfiles[@]}"
