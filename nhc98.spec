Summary: York compiler for Haskell 98
Name: nhc98
Version: 1.00
Release: 3
Copyright: Freely available
Group: Development/Languages
URL: http://www.cs.york.ac.uk/fp/nhc98/
Source: ftp://ftp.cs.york.ac.uk/pub/haskell/nhc98/%{name}src-%{version}.tar.gz
Patch: nhc98-1.00.patch
BuildRoot: %{_tmppath}/%{name}-buildroot
Packager: José Romildo Malaquias <romildo@iceb.ufop.br>
Provides: haskell
Requires: hmake
BuildRequires: ghc

%description
nhc98 is a fully-fledged compiler for Haskell 98, the standard lazy functional
programming language. It based on Niklas Röjemo's nhc13, a compiler for an
earlier version of the language. Written in Haskell, it is small and very
portable, and aims to produce small executables that run in small amounts of
memory. Is a pattern becoming obvious here? :-) It also comes with extensive
tool support.

%prep
%setup -q
%patch0 -p1

%build
./configure \
    --prefix=%{_prefix} \
    --mandir=%{_mandir}/man1 \
    --docdir=docs_/docs +docs \
    --buildwith=ghc --buildopts=-O
make OPT="$RPM_OPT_FLAGS" all

%install
rm -rf $RPM_BUILD_ROOT
./configure \
    --install \
    --prefix=$RPM_BUILD_ROOT%{_prefix} \
    --mandir=$RPM_BUILD_ROOT%{_mandir}/man1
make install

# correct hardcoded paths in some scripts
for f in $RPM_BUILD_ROOT%{_prefix}/{bin/{harch,hp2graph,hood,nhc98,rtb},lib/nhc98/ix86-Linux/hmake.config} ; do
ed -s $f <<EOF || :
,s|$RPM_BUILD_ROOT||g
wq
EOF
done

# remove hmake
rm $RPM_BUILD_ROOT%{_prefix}/bin/{harch,hi,hmake}
rm $RPM_BUILD_ROOT%{_mandir}/man1/hmake*

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc COPYRIGHT INSTALL README docs_/docs
%{_prefix}/bin/*
%{_prefix}/include/nhc98
%{_prefix}/lib/nhc98
%{_mandir}/*/*

%changelog
* Tue Nov 14 2000 José Romildo Malaquias <romildo@iceb.ufop.br> 1.00-3
- remove hmake from files (it has its own rpm now)
- requires hmake

* Wed Sep 20 2000 José Romildo Malaquias <romildo@iceb.ufop.br> 1.00-2
- remove $RPM_BUILD_ROOT from some scritps
- patch to put the echo argument inside double quotes in rtb.ins,
  as it the (java is slow :-) phrase causes error with bash

* Sat Sep 16 2000 José Romildo Malaquias <romildo@iceb.ufop.br> 1.00-1
- new version: 1.00 final
- updated description
- added the "BuildRequires: ghc" tag
- force ghc to do optimizations (-O option to ghc)

* Tue Jun 07 2000 José Romildo Malaquias <romildo@iceb.ufop.br> 1.0pre19-1
- new version
- added %defattr in %files section

* Tue Apr 25 2000 José Romildo Malaquias <romildo@iceb.ufop.br> 1.0pre18-2
- make all

* Wed Apr 19 2000 José Romildo Malaquias <romildo@iceb.ufop.br> 1.0pre18-1
- new version
- remove previous patches (no more needed)
- correct a compilation error

* Sat Mar 20 2000 José Romildo Malaquias <romildo@iceb.ufop.br> 1.0pre17-2
- correct the install directory in the scripts in %{_prefix}/bin/
  which are created using $RPM_BUILD_ROOT%{_prefix} instead of
  %{_prefix}

* Sat Mar 18 2000 José Romildo Malaquias <romildo@iceb.ufop.br> 1.0pre17-1
- new version
- patch to build with ghc-4.06

* Sat Sep 25 1999 José Romildo Malaquias <romildo@iceb.ufop.br> 1.0pre9-1
- first rpm version

