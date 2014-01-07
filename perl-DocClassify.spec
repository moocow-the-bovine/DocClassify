%{!?perl_vendorlib: %define perl_vendorlib %(eval "`perl -V:installvendorlib`"; echo $installvendorlib)}
%define perl_installdirs vendor
%define perl_dstdir     %{perl_vendorlib}

%define arch noarch

Summary: DocClassify Perl module
Name: perl-DocClassify
Version: 0.07
Release: 1
Vendor: Retresco GmbH
License: GPL or Artistic
Group: Development/Libraries
URL: http://www.ling.uni-potsdam.de/~moocow/projects/perl
BuildArch: noarch
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
Source0: DocClassify-%{version}.tar.gz
Requires: perl >= 2:5.8.0
Requires: perl-MUDL >= 0.01001
Requires: perl-PDL-CCS >= 1.12
Requires: perl-PDL-Cluster >= 0.23
Requires: perl(XML::LibXML) >= 1.58
Requires: perl(XML::LibXSLT) >= 1.58
BuildRequires: perl >= 2:5.8.0

%package devel
Group: Development/Libraries
Summary: DocClassify perl module: development
Provides: perl(DocClassify::Mapper::ByLemmaTrain) = %{version}
Provides: perl(DocClassify::Mapper::CutoffTrain) = %{version}
Provides: perl(DocClassify::Mapper::LSITrain) = %{version}
Provides: perl(DocClassify::Mapper::LSI::KNNTrain) = %{version}
Provides: perl(DocClassify::Mapper::LSI::ByCatTrain) = %{version}
Provides: perl(DocClassify::Mapper::Train) = %{version}

%description
%{summary}.

%description devel
%{summary}.

%prep
%setup -q -n DocClassify-%{version}

%build
perl Makefile.PL INSTALLDIRS=%{perl_installdirs}
make


%install
rm -rf $RPM_BUILD_ROOT
make install \
  PERL_INSTALL_ROOT=$RPM_BUILD_ROOT
#  INSTALLARCHLIB=$RPM_BUILD_ROOT%{perl_archlib}

find $RPM_BUILD_ROOT -type f -a \
  \( -name perllocal.pod -o -name .packlist \
     -o \( -name '*.bs' -a -empty \) \
  \) \
  -exec rm -f {} ';'

##-- clean up scripts (remove 'use lib qw(.);' etc)
find $RPM_BUILD_ROOT -type f \
    -a -path '*/bin/dc-*.perl' \
    -exec perl -pi -e'$_="#$_" if (/^use\s+lib\b.*\./);' {} ';'

find $RPM_BUILD_ROOT -type d -depth -exec rmdir {} 2>/dev/null ';'
chmod -R u+w $RPM_BUILD_ROOT/*

%check || :
make test
##
##-- find runtime files (in check vs. install, otherwise rpm default scripts haven't run)
find $RPM_BUILD_ROOT -type f \
  -a \( \
	-path '*/bin/dc-xmlrpc-*.perl' \
	-o -path '*/man/man1/dc-xmlrpc-*.perl.1*' \
	-o \( -name '*.pm' -a '!' -name '*Train.pm' \) \
     \) \
  | sed "s#^$RPM_BUILD_ROOT/*#/#" \
  > files.default

##-- find devel files
find $RPM_BUILD_ROOT -type f \
  -a \( \
	   \( -path '*/bin/dc-*.perl'         -a '!' -name 'dc-xmlrpc-*.perl'    \) \
	-o \( -path '*/man/man1/dc-*.perl.1*' -a '!' -name 'dc-xmlrpc-*.perl.1*' \) \
	-o \( -name '*Train.pm' \) \
     \) \
  | sed "s#^$RPM_BUILD_ROOT/*#/#" \
  > files.devel

 ##-- obfuscate runtime library (uncomment)
 for f in `grep '\.pm$' files.default`; do
   perl -ni \
     -e 's/\#\#\-\-\s.*$//; print if (!/^\s*\#/);' \
     "$RPM_BUILD_ROOT/$f"
 done

%clean
rm -rf $RPM_BUILD_ROOT

%files -f files.default
%defattr(-,root,root,-)
%doc README.txt

%files devel -f files.devel
%{_mandir}/man3/*.3*

%changelog
* Sat Sep 18 2010 Bryan Jurish <jurish@retresco.de> - 0.07-1
- upstream v0.07

* Tue Apr  6 2010 Bryan Jurish <jurish@retresco.de> - 0.06-1
- upstream v0.06

* Mon Mar  1 2010 Bryan Jurish <jurish@retresco.de> - 0.05-1
- upstream v0.05

* Mon Feb 22 2010 Bryan Jurish <jurish@retresco.de> - 0.04-1
- upstream v0.04

* Sat Feb 20 2010 Bryan Jurish <jurish@retresco.de> - 0.03-1
- Packaged from source
