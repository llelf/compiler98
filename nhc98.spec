# A "sumo"-style spec file for building nhc98/hmake/hoodui from CVS

%define name    nhc98
%define version 1.17
%define release 1

Name:           %{name}
Version:        %{version}
Release:        %{release}
License:        Freely available
Group:          Development/Languages/Haskell
URL:            http://haskell.org/nhc98/
Source:         ftp://ftp.cs.york.ac.uk/pub/haskell/nhc98/nhc98-%{version}.tar.gz
Packager:       Sven Panne <sven.panne@aedion.de>
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
Prefix:         %{_prefix}
Requires:       readline
BuildRequires:  ghc happy java
Provides:       haskell hmake hoodui
Summary:        York compiler for Haskell 98

%description
nhc98 is a small, easy to install, standards-compliant compiler for
Haskell 98, the lazy functional programming language. It is very
portable, and aims to produce small executables that run in small
amounts of memory. It produces medium-fast code, and compilation is
itself quite fast. It also comes with extensive tool support for
automatic compilation, foreign language interfacing, heap and time
profiling, tracing, and debugging. (Some of its advanced kinds of heap
profiles are not found in any other Haskell compiler.)

%prep
%setup -q -n nhc98-%{version}

%build
./configure --prefix=%{prefix} --mandir=%{_mandir}/man1 --docdir=html --buildwith=ghc --buildopts=-O
make all hoodui

%install
DESTDIR=${RPM_BUILD_ROOT} make install
( cd ${RPM_BUILD_ROOT}%{_mandir}/man1 && gzip -9 harch.1 hmake.1 hp2graph.1 nhc98.1 )
./configure --install -bin -lib -inc -man +docs

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)

%doc COPYRIGHT
%doc INSTALL
%doc README
%doc html/*

%{_prefix}/bin/cpphs
%{_prefix}/bin/greencard-nhc98
%{_prefix}/bin/harch
%{_prefix}/bin/hi
%{_prefix}/bin/hmake
%{_prefix}/bin/hmake-config
%{_prefix}/bin/hood
%{_prefix}/bin/hp2graph
%{_prefix}/bin/nhc98
%{_prefix}/bin/tprofprel

%{_prefix}/include/nhc98

%{_prefix}/lib/hmake
%{_prefix}/lib/nhc98

%{_mandir}/man1/harch.1.gz
%{_mandir}/man1/hmake.1.gz
%{_mandir}/man1/hp2graph.1.gz
%{_mandir}/man1/nhc98.1.gz
