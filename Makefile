# Default definitions filled in by config script, included from Makefile.inc
include Makefile.inc
.SUFFIXES: 		# To remove default rules like .cpp -> C++

VERSION = 1.04
# When incrementing the version number, don't forget to change the
# corresponding version in the configure script!
#   (odd minor number = CVS version;  even minor number = release version)

HVERSION = 2.02
# HVERSION is the separate version number for hmake.
#   (odd/even minor number is irrelevant - all are release versions)

BASIC = Makefile.inc Makefile README INSTALL COPYRIGHT configure

PRELUDEA = \
	src/prelude/Main.hi src/prelude/Main.T.hi src/prelude/Makefile* \
	src/prelude/Array/Makefile* src/prelude/Array/*.hs \
	src/prelude/Bit/Makefile* src/prelude/Bit/*.hs \
	src/prelude/Binary/Makefile* src/prelude/Binary/*.hs \
	src/prelude/Binary/*.gc src/prelude/Binary/cLowBinary.[ch] \
	src/prelude/BinArray/Makefile* src/prelude/BinArray/*.hs \
	src/prelude/BinArray/*.gc src/prelude/BinArray/cLowUnboxedArray.[ch] \
	src/prelude/CPUTime/Makefile* src/prelude/CPUTime/*.hs \
	src/prelude/CPUTime/*.gc \
	src/prelude/Char/Makefile* src/prelude/Char/*.hs \
	src/prelude/Complex/Makefile* src/prelude/Complex/*.hs \
	src/prelude/DErrNo.hs \
	src/prelude/Debug/Makefile* src/prelude/Debug/*.hs \
	src/prelude/Directory/Makefile* src/prelude/Directory/*.hs \
	src/prelude/Directory/*.gc \
	src/prelude/FFI/Makefile* src/prelude/FFI/*.hs src/prelude/FFI/*.cpp \
	src/prelude/GreenCard/Makefile* src/prelude/GreenCard/*.gc \
	src/prelude/Haskell/Makefile* src/prelude/Haskell/*.hs \
	src/prelude/IO/Makefile* src/prelude/IO/*.hs \
	src/prelude/IOExtras/Makefile* src/prelude/IOExtras/*.hs \
	src/prelude/Ix/Makefile* src/prelude/Ix/*.hs
PRELUDEB = \
	src/prelude/List/Makefile* src/prelude/List/*.hs \
	src/prelude/Locale/Makefile* src/prelude/Locale/*.hs \
	src/prelude/LowB/Makefile* src/prelude/LowB/*.hs \
	src/prelude/LowT/Makefile* src/prelude/LowT/*.hs \
	src/prelude/Maybe/Makefile* src/prelude/Maybe/*.hs \
	src/prelude/Monad/Makefile* src/prelude/Monad/*.hs \
	src/prelude/NonStd/Makefile* src/prelude/NonStd/*.hs \
	src/prelude/NonStd/*.gc \
	src/prelude/Numeric/Makefile* src/prelude/Numeric/*.hs \
	src/prelude/Observe/Makefile* src/prelude/Observe/*.lhs \
	src/prelude/PackedString/Makefile* src/prelude/PackedString/*.hs \
	src/prelude/Prelude/Makefile* src/prelude/Prelude/*.hs \
	src/prelude/PreludeDebug/Makefile* src/prelude/PreludeDebug/*.hs \
	src/prelude/PreludeIO/Makefile* src/prelude/PreludeIO/*.hs \
	src/prelude/PreludeList/Makefile* src/prelude/PreludeList/*.hs \
	src/prelude/PreludeText/Makefile* src/prelude/PreludeText/*.hs \
	src/prelude/Random/Makefile* src/prelude/Random/*.hs \
	src/prelude/Ratio/Makefile* src/prelude/Ratio/*.hs \
	src/prelude/System/Makefile* src/prelude/System/*.hs \
	src/prelude/Time/Makefile* src/prelude/Time/*.hs \
	src/prelude/Time/*.gc
PRELUDEC = \
	src/prelude/Array/*.c \
	src/prelude/Bit/*.c \
	src/prelude/Binary/*.c \
	src/prelude/BinArray/*.c \
	src/prelude/CPUTime/*.c \
	src/prelude/Char/*.c \
	src/prelude/Complex/*.c \
	src/prelude/DErrNo.c \
	src/prelude/Debug/*.c \
	src/prelude/Directory/*.c \
	src/prelude/GreenCard/*.c \
	src/prelude/Haskell/*.c \
	src/prelude/IO/*.c \
	src/prelude/IOExtras/*.c \
	src/prelude/Ix/*.c \
	src/prelude/List/*.c \
	src/prelude/Locale/*.c \
	src/prelude/LowB/*.c \
	src/prelude/Maybe/*.c \
	src/prelude/Monad/*.c \
	src/prelude/NonStd/*.c \
	src/prelude/Numeric/*.c \
	src/prelude/Observe/*.c \
	src/prelude/PackedString/*.c \
	src/prelude/Prelude/*.c \
	src/prelude/PreludeIO/*.c \
	src/prelude/PreludeList/*.c \
	src/prelude/PreludeText/*.c \
	src/prelude/Random/*.c \
	src/prelude/Ratio/*.c \
	src/prelude/System/*.c \
	src/prelude/Time/*.c \
	src/prelude/FFI/*.c


COMPILER = src/compiler98/Makefile*  src/compiler98/*.hs \
	   src/compiler98/*.gc src/compiler98/*.c.inst src/compiler98/*.h
COMPILERC = src/compiler98/*.c
DATA2C = src/data2c/Makefile* src/data2c/*.hs
SCRIPT = script/hmake.inst script/greencard.inst script/nhc98.inst \
         script/hmakeconfig.inst script/hi.inst script/hat-trail.inst \
         script/nhc98heap.c script/harch script/confhc script/mangler \
	 script/errnogen.c script/GenerateErrNo.hs script/fixghc \
	 script/echo.c script/hood.inst script/tprofprel \
	 lib/hat-trail.jar lib/hood.jar \
	 script/hmake-PRAGMA.hs script/hmake-PRAGMA.c
GREENCARD = src/greencard/*.lhs src/greencard/*.hs \
	    src/greencard/Makefile*
GREENCARDC = src/greencard/*.c
HP2GRAPH = src/hp2graph/Makefile* src/hp2graph/README \
	   src/hp2graph/doc src/hp2graph/*.[hc1]
HMAKE = src/hmake/Makefile* src/hmake/*.hs src/hmake/README* \
	src/hmake/HISTORY src/hmake/Summary* \
	src/interpreter/Makefile* src/interpreter/*.hs
HMAKEC = src/hmake/*.c src/interpreter/*.c
RUNTIME = \
	src/Makefile.inc \
	src/runtime/Makefile* \
	src/runtime/Builtin/Makefile* \
	src/runtime/Builtin/*.c \
	src/runtime/Integer/Makefile* \
	src/runtime/Integer/*.c \
	src/runtime/Integer/*.h \
	src/runtime/Kernel/Makefile* \
	src/runtime/Kernel/*.c \
	src/runtime/Kernel/*.h \
	src/runtime/Mk/Makefile* \
	src/runtime/Mk/*.c

RUNTIMET = \
	src/tracer/Makefile* \
	src/tracer/runtime/Makefile* \
	src/tracer/runtime/*.[ch]
PRAGMA  = lib/$(MACHINE)/hmake-PRAGMA
TRACEUI = src/tracer/ui/Makefile* src/tracer/ui/nhctracer/*
HATUI	= src/tracer/hat/Makefile* src/tracer/hat/*.[ch]
HOODUI  = src/tracer/hoodui/Makefile* src/tracer/hoodui/*.java \
	  src/tracer/hoodui/com/microstar/xml/*
INCLUDE = include/*.hi include/*.h include/*.gc
DOC = docs/*
MAN = man/*.1
HATTOOLS= lib/$(MACHINE)/hat-stack lib/$(MACHINE)/hat-connect \
	lib/$(MACHINE)/hat-check lib/$(MACHINE)/hat-observe \
	lib/$(MACHINE)/hat-detect lib/$(MACHINE)/hat-checki

TARGDIR= targets
TARGETS= runtime prelude greencard hp2graph hattools \
	 profruntime profprelude \
	 timeruntime timeprelude \
	 timetraceruntime timetraceprelude \
	 traceruntime traceprelude \
	 compiler-nhc compiler-hbc compiler-ghc compiler-$(CC) \
	 hmake-nhc hmake-hbc hmake-ghc hmake-$(CC) \
	 greencard-nhc greencard-hbc greencard-ghc greencard-$(CC) \
	 prelude-$(CC) pragma-$(CC)

.PHONY: basic all tracer compiler help config install hattools


##### compiler build + install scripts

basic: basic-${BUILDCOMP}
all:   all-${BUILDCOMP}
compiler: compiler-${BUILDCOMP}
hmake: hmake-${BUILDCOMP}
help:
	@echo "Default target is:      basic"
	@echo "Other targets include:  heapprofile timeprofile tracer"
	@echo "                        all (= basic + heapprofile + timeprofile + tracer)"
	@echo "                        config install clean realclean"
	@echo "  (other subtargets:    compiler hmake runtime prelude"
	@echo "                        hp2graph hattools hoodui)"
	@echo "For a specific build-compiler: basic-hbc basic-ghc basic-nhc basic-gcc"
	@echo "                               all-hbc   all-ghc   all-nhc   all-gcc"
	@echo "                               etc..."

config: script/errnogen.c
	./configure --config
install:
	./configure --install

basic-nhc: $(PRAGMA) runtime hmake-nhc greencard-nhc compiler-nhc prelude
basic-hbc: $(PRAGMA) runtime hmake-hbc greencard-hbc compiler-hbc prelude
basic-ghc: $(PRAGMA) runtime hmake-ghc greencard-ghc compiler-ghc prelude
basic-$(CC):   runtime prelude-$(CC) pragma-$(CC) compiler-$(CC) \
		 greencard-$(CC) hmake-$(CC)

all-$(BUILDCOMP): basic-$(BUILDCOMP) heapprofile timeprofile tracer hoodui

heapprofile: compiler profruntime profprelude hp2graph
timeprofile: compiler timeruntime timeprelude
tracer: $(PRAGMA) runtime hmake-$(BUILDCOMP) greencard-$(BUILDCOMP) \
	compiler-$(BUILDCOMP) traceruntime traceprelude hattools
timetraceprofile: timetraceruntime timetraceprelude

$(TARGETS): % : $(TARGDIR)/$(MACHINE)/%

$(TARGDIR)/$(MACHINE)/runtime: $(RUNTIME)
	cd src/runtime;        $(MAKE) install nhc98heap$(EXE)
	cd src/tracer/runtime; $(MAKE) install
	touch $(TARGDIR)/$(MACHINE)/runtime


$(TARGDIR)/$(MACHINE)/compiler-nhc: $(COMPILER)
	cd src/compiler98;     $(MAKE) HC=nhc98 install
	touch $(TARGDIR)/$(MACHINE)/compiler-nhc
$(TARGDIR)/$(MACHINE)/compiler-hbc: $(COMPILER)
	cd src/compiler98;     $(MAKE) HC=hbc install
	touch $(TARGDIR)/$(MACHINE)/compiler-hbc
$(TARGDIR)/$(MACHINE)/compiler-ghc: $(COMPILER)
	cd src/compiler98;     $(MAKE) HC=ghc install
	touch $(TARGDIR)/$(MACHINE)/compiler-ghc


$(TARGDIR)/$(MACHINE)/prelude: $(PRELUDEA) $(PRELUDEB)
	cd src/prelude;        $(MAKE) install
	touch $(TARGDIR)/$(MACHINE)/prelude


$(TARGDIR)/$(MACHINE)/greencard-nhc: $(GREENCARD)
	cd src/greencard;      $(MAKE) HC=nhc98 install
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/greencard-nhc
$(TARGDIR)/$(MACHINE)/greencard-hbc: $(GREENCARD)
	cd src/greencard;      $(MAKE) HC=hbc install
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/greencard-hbc
$(TARGDIR)/$(MACHINE)/greencard-ghc: $(GREENCARD)
	cd src/greencard;      $(MAKE) HC=ghc install
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/greencard-ghc


$(PRAGMA): script/hmake-PRAGMA.hs
	$(BUILDWITH) $(shell echo $(BUILDOPTS)) -cpp -o $@ $<


$(TARGDIR)/$(MACHINE)/hmake-nhc: $(HMAKE)
	cd src/hmake;          $(MAKE) HC=nhc98 install
	cd src/interpreter;    $(MAKE) HC=nhc98 install
	touch $(TARGDIR)/$(MACHINE)/hmake-nhc
$(TARGDIR)/$(MACHINE)/hmake-hbc: $(HMAKE)
	cd src/hmake;          $(MAKE) HC=hbc install
	cd src/interpreter;    $(MAKE) HC=hbc install
	touch $(TARGDIR)/$(MACHINE)/hmake-hbc
$(TARGDIR)/$(MACHINE)/hmake-ghc: $(HMAKE)
	cd src/hmake;          $(MAKE) HC=ghc install
	cd src/interpreter;    $(MAKE) HC=ghc install
	touch $(TARGDIR)/$(MACHINE)/hmake-ghc


$(TARGDIR)/$(MACHINE)/hp2graph: $(HP2GRAPH)
	cd src/hp2graph;       $(MAKE) install
	touch $(TARGDIR)/$(MACHINE)/hp2graph


$(TARGDIR)/$(MACHINE)/profruntime: $(RUNTIME)
	cd src/runtime;        $(MAKE) CFG=p install
	cd src/tracer/runtime; $(MAKE) CFG=p install
	touch $(TARGDIR)/$(MACHINE)/profruntime
$(TARGDIR)/$(MACHINE)/profprelude: greencard $(PRELUDEA) $(PRELUDEB)
	cd src/prelude;        $(MAKE) CFG=p install
	touch $(TARGDIR)/$(MACHINE)/profprelude


$(TARGDIR)/$(MACHINE)/traceruntime: $(RUNTIME) $(RUNTIMET)
	cd src/runtime;        $(MAKE) CFG=T install
	cd src/tracer/runtime; $(MAKE) CFG=T install
	touch $(TARGDIR)/$(MACHINE)/traceruntime
$(TARGDIR)/$(MACHINE)/traceprelude: $(PRELUDEA) $(PRELUDEB)
	cd src/prelude;	       $(MAKE) CFG=T install
	touch $(TARGDIR)/$(MACHINE)/traceprelude


$(TARGDIR)/$(MACHINE)/timetraceruntime: $(RUNTIME) $(RUNTIMET)
	cd src/runtime;        $(MAKE) CFG=tT install
	cd src/tracer/runtime; $(MAKE) CFG=tT install
	touch $(TARGDIR)/$(MACHINE)/timetraceruntime
$(TARGDIR)/$(MACHINE)/timetraceprelude: $(PRELUDEA) $(PRELUDEB)
	cd src/prelude;	       $(MAKE) CFG=tT install
	touch $(TARGDIR)/$(MACHINE)/timetraceprelude


hoodui: $(TARGDIR)/hoodui
$(TARGDIR)/hoodui: lib/hood.jar
	touch $(TARGDIR)/hoodui
$(TARGDIR)/$(MACHINE)/hattools: $(HATTOOLS) lib/hat-trail.jar
	touch $(TARGDIR)/$(MACHINE)/hat


lib/hat-trail.jar: $(TRACEUI)
	cd src/tracer/ui;      $(MAKE) CFG=T install
lib/hood.jar: $(HOODUI)
	cd src/tracer/hoodui;  $(MAKE) install
$(HATTOOLS): $(HATUI)
	cd src/tracer/hat;     $(MAKE) install


$(TARGDIR)/$(MACHINE)/timeruntime: $(RUNTIME)
	cd src/runtime;        $(MAKE) CFG=t install
	cd src/tracer/runtime; $(MAKE) CFG=t install
	touch $(TARGDIR)/$(MACHINE)/timeruntime
$(TARGDIR)/$(MACHINE)/timeprelude: greencard $(PRELUDEA) $(PRELUDEB)
	cd src/prelude;        $(MAKE) CFG=t install
	touch $(TARGDIR)/$(MACHINE)/timeprelude


$(TARGDIR)/$(MACHINE)/prelude-$(CC): $(PRELUDEC)
	cd src/prelude;        $(MAKE) fromC 
	touch $(TARGDIR)/$(MACHINE)/prelude-$(CC)
$(TARGDIR)/$(MACHINE)/compiler-$(CC): $(COMPILERC)
	cd src/compiler98;     $(MAKE) fromC
	cd src/prelude/$(MACHINE); $(MAKE) clean all	# Patch machine-specific parts.
	cd src/prelude;        $(MAKE) relink
	cd src/compiler98;     $(MAKE) relink
	touch $(TARGDIR)/$(MACHINE)/compiler-$(CC)
$(TARGDIR)/$(MACHINE)/greencard-$(CC): $(GREENCARDC)
	cd src/greencard;      $(MAKE) fromC
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/greencard-$(CC)
$(TARGDIR)/$(MACHINE)/pragma-$(CC): script/hmake-PRAGMA.c
	script/nhc98 -o $(PRAGMA) script/hmake-PRAGMA.c
	touch $(TARGDIR)/$(MACHINE)/pragma-$(CC)
$(TARGDIR)/$(MACHINE)/hmake-$(CC): $(HMAKEC)
	cd src/hmake;          $(MAKE) fromC
	cd src/interpreter;    $(MAKE) fromC
	touch $(TARGDIR)/$(MACHINE)/hmake-$(CC)


script/errnogen.c: script/GenerateErrNo.hs
	hmake script/GenerateErrNo
	script/GenerateErrNo +RTS -H2M -RTS >script/errnogen.c


##### scripts for packaging various distribution formats

binDist:
	rm -f nhc98-$(VERSION)-$(MACHINE).tar nhc98-$(VERSION)-$(MACHINE).tar.gz
	tar cf nhc98-$(VERSION)-$(MACHINE).tar $(BASIC)
	tar rf nhc98-$(VERSION)-$(MACHINE).tar lib/$(MACHINE)
	tar rf nhc98-$(VERSION)-$(MACHINE).tar $(SCRIPT)
	tar rf nhc98-$(VERSION)-$(MACHINE).tar $(MAN)
	tar rf nhc98-$(VERSION)-$(MACHINE).tar $(INCLUDE)
	tar rf nhc98-$(VERSION)-$(MACHINE).tar $(DOC)
	mkdir nhc98-$(VERSION)
	cd nhc98-$(VERSION); tar xf ../nhc98-$(VERSION)-$(MACHINE).tar
	tar cf nhc98-$(VERSION)-$(MACHINE).tar nhc98-$(VERSION)
	rm -r nhc98-$(VERSION)
	gzip nhc98-$(VERSION)-$(MACHINE).tar

srcDist: $(TARGDIR)/preludeC $(TARGDIR)/compilerC $(TARGDIR)/greencardC $(TARGDIR)/hmakeC $(TARGDIR)/pragmaC nolinks
	rm -f nhc98src-$(VERSION).tar nhc98src-$(VERSION).tar.gz
	tar cf nhc98src-$(VERSION).tar $(BASIC)
	tar rf nhc98src-$(VERSION).tar $(COMPILER)
	tar rf nhc98src-$(VERSION).tar $(COMPILERC)
	tar rf nhc98src-$(VERSION).tar $(SCRIPT)
	tar rf nhc98src-$(VERSION).tar $(RUNTIME)
	tar rf nhc98src-$(VERSION).tar $(RUNTIMET)
	tar rf nhc98src-$(VERSION).tar $(PRELUDEA)
	tar rf nhc98src-$(VERSION).tar $(PRELUDEB)
	tar rf nhc98src-$(VERSION).tar $(PRELUDEC)
	tar rf nhc98src-$(VERSION).tar $(TRACEUI)
	tar rf nhc98src-$(VERSION).tar $(HOODUI)
	tar rf nhc98src-$(VERSION).tar $(HATUI)
	tar rf nhc98src-$(VERSION).tar $(GREENCARD)
	tar rf nhc98src-$(VERSION).tar $(GREENCARDC)
	tar rf nhc98src-$(VERSION).tar $(HP2GRAPH)
	tar rf nhc98src-$(VERSION).tar $(HMAKE)
	tar rf nhc98src-$(VERSION).tar $(HMAKEC)
	tar rf nhc98src-$(VERSION).tar $(MAN)
	tar rf nhc98src-$(VERSION).tar $(INCLUDE)
	tar rf nhc98src-$(VERSION).tar $(DOC)
	mkdir nhc98-$(VERSION)
	cd nhc98-$(VERSION); tar xf ../nhc98src-$(VERSION).tar
	tar cf nhc98src-$(VERSION).tar nhc98-$(VERSION)
	rm -r nhc98-$(VERSION)
	gzip nhc98src-$(VERSION).tar

nolinks:
	cd src/runtime;    $(MAKE) nolinks
$(TARGDIR)/preludeC: $(PRELUDEA) $(PRELUDEB)
	cd src/prelude;    $(MAKE) cfiles
	touch $(TARGDIR)/preludeC
$(TARGDIR)/compilerC: $(COMPILER)
	cd src/compiler98; $(MAKE) cfiles
	touch $(TARGDIR)/compilerC
$(TARGDIR)/greencardC: $(GREENCARD)
	cd src/greencard;  $(MAKE) cfiles
	touch $(TARGDIR)/greencardC
$(TARGDIR)/pragmaC: script/hmake-PRAGMA.hs
	script/nhc98 -cpp -C script/hmake-PRAGMA.hs
	touch $(TARGDIR)/pragmaC
$(TARGDIR)/hmakeC: $(HMAKE)
	cd src/hmake;        $(MAKE) cfiles
	cd src/interpreter;  $(MAKE) cfiles
	touch $(TARGDIR)/hmakeC



##### package up hmake separately

HBASIC  = src/hmake/README src/hmake/INSTALL
HMFILE  = src/hmake/Makefile.toplevel
HAUX1   = Makefile.inc COPYRIGHT
HAUX2   = src/Makefile*
HSCRIPT = script/hmake.inst script/hmakeconfig.inst \
	  script/harch script/hi.inst script/confhc \
	  script/echo.c script/fixghc
HMAN    = man/hmake.1 docs/hmake
HCONF   = hmake-configure
HBIN    = lib/$(MACHINE)/MkProg$(EXE) lib/$(MACHINE)/Older$(EXE) \
	  lib/$(MACHINE)/hi$(EXE) targets/$(MACHINE)/config.cache

hmakeDist:
	rm -f hmake-$(HVERSION).tar hmake-$(HVERSION).tar.gz
	tar cf hmake-$(HVERSION).tar $(HAUX1)
	tar rf hmake-$(HVERSION).tar $(HAUX2)
	tar rf hmake-$(HVERSION).tar $(HMAKE)
	tar rf hmake-$(HVERSION).tar $(HSCRIPT)
	tar rf hmake-$(HVERSION).tar $(HMAN)
	mkdir hmake-$(HVERSION)
	cd hmake-$(HVERSION); tar xf ../hmake-$(HVERSION).tar
	cp $(HBASIC) hmake-$(HVERSION)
	cp $(HMFILE) hmake-$(HVERSION)/Makefile
	cp $(HCONF)  hmake-$(HVERSION)/configure
	tar cf hmake-$(HVERSION).tar hmake-$(HVERSION)
	rm -r hmake-$(HVERSION)
	gzip hmake-$(HVERSION).tar
hmakeBinDist:
	rm -f hmake-$(HVERSION)-$(MACHINE).tar hmake-$(HVERSION)-$(MACHINE).tar.gz
	tar cf hmake-$(HVERSION)-$(MACHINE).tar $(HAUX1)
	tar rf hmake-$(HVERSION)-$(MACHINE).tar $(HSCRIPT)
	tar rf hmake-$(HVERSION)-$(MACHINE).tar $(HBIN)
	tar rf hmake-$(HVERSION)-$(MACHINE).tar $(HMAN)
	mkdir hmake-$(HVERSION)
	cd hmake-$(HVERSION); tar xf ../hmake-$(HVERSION)-$(MACHINE).tar
	cp $(HBASIC) hmake-$(HVERSION)
	cp $(HMFILE) hmake-$(HVERSION)/Makefile
	cp $(HCONF)  hmake-$(HVERSION)/configure
	tar cf hmake-$(HVERSION)-$(MACHINE).tar hmake-$(HVERSION)
	rm -r hmake-$(HVERSION)
	gzip hmake-$(HVERSION)-$(MACHINE).tar


##### cleanup

clean: cleanhi
	#cd data2c;             $(MAKE) clean
	cd src/compiler98;      $(MAKE) clean
	cd src/greencard;       $(MAKE) clean
	cd src/hp2graph;        $(MAKE) clean
	cd src/hmake;           $(MAKE) clean
	cd src/interpreter;     $(MAKE) clean
	rm -f  script/hmake-PRAGMA.o
	rm -rf $(BUILDDIR)/obj*			# all object files

cleanhi:
	rm -f  script/hmake-PRAGMA.hi
	cd src/prelude; $(MAKE) cleanhi
	cd src/prelude; $(MAKE) CFG=T cleanhi

cleanC:
	rm -f src/compiler98/*.c
	rm -f src/greencard/*.c
	rm -f src/hmake/*.c
	rm -f src/interpreter/*.c
	rm -f script/hmake-PRAGMA.c
	cd src/prelude;		$(MAKE) cleanC
	cd $(TARGDIR);  rm -f preludeC compilerC greencardC hmakeC pragmaC

realclean: clean cleanC
	#cd data2c;        $(MAKE) realclean
	cd src/compiler98; $(MAKE) realclean
	cd $(TARGDIR)/$(MACHINE);  rm -f $(TARGETS)
	cd $(TARGDIR)/$(MACHINE);  rm -f hmake.config config.cache
	rm -rf src/prelude/$(MACHINE)
	rm -rf $(LIBDIR)/$(MACHINE)
	rm -f  script/hmake-PRAGMA.o script/hmake-PRAGMA.hi
	rm -f  script/nhc98 script/greencard script/hmake
