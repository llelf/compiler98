Summary: hmake is a compilation manager for Haskell programs.
Name: hmake
Version: 2.01
Release: 1
Copyright: Freely available
Group: Development/Languages
URL: http://www.cs.york.ac.uk/fp/hmake/
Source: ftp://ftp.cs.york.ac.uk/pub/haskell/hmake/%{name}-%{version}.tar.gz
Patch: hmake-2.01.config.patch
BuildRoot: %{_tmppath}/%{name}-buildroot
Packager: José Romildo Malaquias <romildo@iceb.ufop.br>
BuildRequires: ghc

%description
hmake is a make(1) like command for compiling Haskell programs.
Dependencies are automatically extracted from the source files;
there is no need to construct or maintain a Makefile.

Hmake interactive, or hi for short, is an interpreter-like
environment that you can wrap over any common Haskell
compiler to achieve an interactive development style rather
like Hugs.

%prep
%setup -q
%patch0 -p1

%build
./configure \
    --prefix=%{_prefix} \
    --mandir=%{_mandir}/man1 \
    --buildwith=ghc --buildopts=-O
make OPT="$RPM_OPT_FLAGS"

%install
rm -rf $RPM_BUILD_ROOT
./configure \
    --install \
    --prefix=$RPM_BUILD_ROOT%{_prefix} \
    --mandir=$RPM_BUILD_ROOT%{_mandir}/man1
make install

# correct hardcoded build-root path in some scripts
for f in $RPM_BUILD_ROOT%{_prefix}/bin/{hi,hmake} ; do
ed -s $f <<EOF || :
,s|$RPM_BUILD_ROOT||g
wq
EOF
done

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc COPYRIGHT INSTALL README docs/hmake/*.*
%{_prefix}/bin/*
%{_prefix}/lib/hmake
%{_mandir}/*/*

%changelog
* Sat Nov 11 2000 José Romildo Malaquias <romildo@iceb.ufop.br> 2.01-1
- First independent rpm version (it used to be built within the nhc98 rpm)
- Patch to correctly detect readline (it depends on the curses library)
- Correct hardcoded build-root path in some scripts
