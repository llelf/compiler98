# Default definitions filled in by config script, included from Makefile.inc
include Makefile.inc

VERSION = 1.0pre18
# When incrementing the version number, don't forget to change the
# corresponding version in the configure script!
#   (A trailing x means this version has not been released yet.)

HVERSION = 1.7.2
# HVERSION is the separate version number for hmake.

BASIC = Makefile.inc Makefile README INSTALL COPYRIGHT configure

PRELUDEA = \
	src/prelude/Main.hi src/prelude/Makefile* \
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
	src/prelude/Debug/Makefile* src/prelude/Debug/*.hs \
	src/prelude/Directory/Makefile* src/prelude/Directory/*.hs \
	src/prelude/Directory/*.gc \
	src/prelude/FFI/Makefile* src/prelude/FFI/*.hs \
	src/prelude/GreenCard/Makefile* src/prelude/GreenCard/*.gc \
	src/prelude/Haskell/Makefile* src/prelude/Haskell/*.hs \
	src/prelude/IO/Makefile* src/prelude/IO/*.hs \
	src/prelude/IOExtras/Makefile* src/prelude/IOExtras/*.hs \
	src/prelude/Interrupt/Makefile* src/prelude/Interrupt/*.hs \
	src/prelude/Ix/Makefile* src/prelude/Ix/*.hs
PRELUDEB = \
	src/prelude/List/Makefile* src/prelude/List/*.hs \
	src/prelude/Locale/Makefile* src/prelude/Locale/*.hs \
	src/prelude/LowB/Makefile* src/prelude/LowB/*.hs \
	src/prelude/Maybe/Makefile* src/prelude/Maybe/*.hs \
	src/prelude/Monad/Makefile* src/prelude/Monad/*.hs \
	src/prelude/NonStd/Makefile* src/prelude/NonStd/*.hs \
	src/prelude/NonStd/*.gc \
	src/prelude/Numeric/Makefile* src/prelude/Numeric/*.hs \
	src/prelude/PackedString/Makefile* src/prelude/PackedString/*.hs \
	src/prelude/Prelude/Makefile* src/prelude/Prelude/*.hs \
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
	src/prelude/Debug/*.c \
	src/prelude/Directory/*.c \
	src/prelude/GreenCard/*.c \
	src/prelude/Haskell/*.c \
	src/prelude/IO/*.c \
	src/prelude/IOExtras/*.c \
	src/prelude/Interrupt/*.c \
	src/prelude/Ix/*.c \
	src/prelude/List/*.c \
	src/prelude/Locale/*.c \
	src/prelude/LowB/*.c \
	src/prelude/Maybe/*.c \
	src/prelude/Monad/*.c \
	src/prelude/NonStd/*.c \
	src/prelude/Numeric/*.c \
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
PRELUDET = \
	src/tracer/prelude/Makefile* \
	src/tracer/prelude/*.hi \
	src/tracer/prelude/*/Makefile* \
	src/tracer/prelude/Char/*.hs \
	src/tracer/prelude/DontKnow/*.hs \
	src/tracer/prelude/IO/*.hs \
	src/tracer/prelude/List/*.hs \
	src/tracer/prelude/LowB/*.hs \
	src/tracer/prelude/LowB/LowSystem.hi \
	src/tracer/prelude/Maybe/*.hs \
	src/tracer/prelude/NonStd/*.hs \
	src/tracer/prelude/PackedString/*.hs \
	src/tracer/prelude/Prelude/*.hs \
	src/tracer/prelude/PreludeDebug/*.hs \
	src/tracer/prelude/PreludeIO/*.hs \
	src/tracer/prelude/PreludeList/*.hs \
	src/tracer/prelude/PreludeText/*.hs \
	src/tracer/prelude/Ratio/*.hs \
	src/tracer/prelude/System/*.hs \
	src/tracer/prelude/Text/*.hs

#	src/tracer/prelude/Array/*.hs \
#	src/tracer/prelude/Complex/*.hs \
#	src/tracer/prelude/Debug/*.hs \
#	src/tracer/prelude/Directory/*.hs \
#	src/tracer/prelude/Interrupt/*.hs \
#	src/tracer/prelude/Ix/*.hs \
#	src/tracer/prelude/Monad/*.hs \

COMPILER = src/compiler98/Makefile*  src/compiler98/*.hs \
	   src/compiler98/*.gc src/compiler98/*.c.inst
COMPILERC = src/compiler98/*.c
DATA2C = src/data2c/Makefile* src/data2c/*.hs
SCRIPT = script/hmake.inst script/greencard.inst script/nhc98.inst \
         script/hmakeconfig.inst script/nhc98tracer.inst lib/nhctracer.jar \
         script/nhc98heap.c script/harch script/confhc script/mangler \
	 script/errnogen.c script/GenerateErrNo.hs script/fixghc
GREENCARD = src/greencard/*.lhs src/greencard/*.hs \
	    src/greencard/Makefile*
GREENCARDC = src/greencard/*.c
HP2GRAPH = src/hp2graph/Makefile* src/hp2graph/README \
	   src/hp2graph/doc src/hp2graph/*.[hc1]
HMAKE = src/hmake/Makefile* src/hmake/*.hs src/hmake/README* \
	src/hmake/HISTORY src/hmake/Summary*
HMAKEC = src/hmake/*.c
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
#	src/runtime/Integer/stmpstddfh \
#	src/runtime/Builtin/*.h \
#	src/runtime/Mk/*.h

RUNTIMET = \
	src/tracer/Makefile* \
	src/tracer/runtime/Makefile* \
	src/tracer/runtime/*.[ch] targets/traceui
TRACEUI = src/tracer/ui/*
INCLUDE = include/*.hi include/*.h include/*.gc include/tracer/*.hi
DOC = docs/*
MAN = man/*.1

TARGDIR= targets
TARGETS= runtime bootprelude prelude greencard hp2graph \
	 profruntime profprelude \
	 timeruntime timeprelude \
	 traceruntime traceprelude \
	 compiler-nhc compiler-hbc compiler-ghc \
	 hmake-nhc hmake-hbc hmake-ghc \
	 greencard-nhc greencard-hbc greencard-ghc \
	 ccompiler cprelude cgreencard chmake \
	 tracecompiler-nhc tracecompiler-hbc tracecompiler-ghc


##### compiler build + install scripts

basic: basic-${BUILDWITH}
all:   all-${BUILDWITH}
help:
	@echo "Common targets include:        basic all install clean realclean config"
	@echo "For a specific build-compiler: basic-hbc basic-ghc basic-nhc basic-gcc"
	@echo "                               all-hbc   all-ghc   all-nhc   all-gcc"
	@echo "  (other subtargets: runtime prelude profile timeprof hp2graph"
	@echo "                     compiler-hbc  compiler-ghc  compiler-nhc"
	@echo "                     tracer-hbc    tracer-ghc    tracer-nhc"
	@echo "                     hmake-hbc     hmake-ghc     hmake-nhc"
	@echo "                     greencard-hbc greencard-ghc greencard-nhc)"

config: script/errnogen.c
	./configure --config
install:
	./configure --install

basic-nhc: runtime hmake-nhc greencard-nhc compiler-nhc prelude
basic-hbc: runtime hmake-hbc greencard-hbc compiler-hbc prelude
basic-ghc: runtime hmake-ghc greencard-ghc compiler-ghc prelude
basic-gcc: runtime cprelude ccompiler cgreencard chmake
all-nhc: basic-nhc profile hp2graph tracer-nhc   #timeprof
all-hbc: basic-hbc profile hp2graph tracer-hbc   #timeprof
all-ghc: basic-ghc profile hp2graph tracer-ghc   #timeprof
all-gcc: basic-gcc profile hp2graph tracer-nhc   #timeprof

profile: profruntime profprelude
timeprof: timeruntime timeprelude
tracer-nhc: tracecompiler-nhc traceruntime traceprelude $(TARGDIR)/traceui
tracer-hbc: tracecompiler-hbc traceruntime traceprelude $(TARGDIR)/traceui
tracer-ghc: tracecompiler-ghc traceruntime traceprelude $(TARGDIR)/traceui

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

#$(TARGDIR)/$(MACHINE)/bootprelude:
#	cd src/prelude;        $(MAKE) boot
#	touch $(TARGDIR)/$(MACHINE)/bootprelude
$(TARGDIR)/$(MACHINE)/prelude: $(PRELUDEA) $(PRELUDEB)
	cd src/prelude;        $(MAKE) install
	touch $(TARGDIR)/$(MACHINE)/prelude

#$(TARGDIR)/$(MACHINE)/greencard: $(TARGDIR)/$(MACHINE)/bootprelude $(GREENCARD)
#	cd src/greencard;      $(MAKE) install
#	touch $(TARGDIR)/$(MACHINE)/greencard
$(TARGDIR)/$(MACHINE)/greencard-nhc: $(GREENCARD)
	cd src/greencard;      $(MAKE) HC=nhc98 install
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/greencard-nhc
$(TARGDIR)/$(MACHINE)/greencard-hbc: $(GREENCARD)
	cd src/greencard;      $(MAKE) HC=hbc install
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/greencard-hbc
$(TARGDIR)/$(MACHINE)/greencard-ghc: $(GREENCARD)
	cd src/greencard;      $(MAKE) HC=ghc install
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/greencard-ghc

$(TARGDIR)/$(MACHINE)/hmake-nhc: $(HMAKE)
	cd src/hmake;          $(MAKE) HC=nhc98 install
	touch $(TARGDIR)/$(MACHINE)/hmake-nhc
$(TARGDIR)/$(MACHINE)/hmake-hbc: $(HMAKE)
	cd src/hmake;          $(MAKE) HC=hbc install
	touch $(TARGDIR)/$(MACHINE)/hmake-hbc
$(TARGDIR)/$(MACHINE)/hmake-ghc: $(HMAKE)
	cd src/hmake;          $(MAKE) HC=ghc install
	touch $(TARGDIR)/$(MACHINE)/hmake-ghc

$(TARGDIR)/$(MACHINE)/hp2graph: $(HP2GRAPH)
	cd src/hp2graph;       $(MAKE) install
	touch $(TARGDIR)/$(MACHINE)/hp2graph

$(TARGDIR)/$(MACHINE)/profruntime: $(RUNTIME)
	cd src/runtime;        $(MAKE) CFG=p install
	cd src/tracer/runtime; $(MAKE) CFG=p install
	touch $(TARGDIR)/$(MACHINE)/profruntime
$(TARGDIR)/$(MACHINE)/profprelude: greencard $(PRELUDEA) $(PRELUDEB)
	cd src/prelude;        $(MAKE) CFG=p install  #cleanhi 
	touch $(TARGDIR)/$(MACHINE)/profprelude

$(TARGDIR)/$(MACHINE)/tracecompiler-nhc: $(COMPILER)
	cd src/compiler98;     $(MAKE) CFG=T HC=nhc98 install
	touch $(TARGDIR)/$(MACHINE)/tracecompiler-nhc
$(TARGDIR)/$(MACHINE)/tracecompiler-hbc: $(COMPILER)
	cd src/compiler98;     $(MAKE) CFG=T HC=hbc install
	touch $(TARGDIR)/$(MACHINE)/tracecompiler-hbc
$(TARGDIR)/$(MACHINE)/tracecompiler-ghc: $(COMPILER)
	cd src/compiler98;     $(MAKE) CFG=T HC=ghc install
	touch $(TARGDIR)/$(MACHINE)/tracecompiler-ghc
$(TARGDIR)/$(MACHINE)/traceruntime: $(RUNTIME) $(RUNTIMET)
	cd src/runtime;        $(MAKE) CFG=T install
	cd src/tracer/runtime; $(MAKE) CFG=T install
	touch $(TARGDIR)/$(MACHINE)/traceruntime
$(TARGDIR)/$(MACHINE)/traceprelude: $(PRELUDET)
	cd src/tracer/prelude; $(MAKE) CFG=T install
	touch $(TARGDIR)/$(MACHINE)/traceprelude
$(TARGDIR)/traceui: $(TRACEUI)
	cd src/tracer/ui;      $(MAKE) CFG=T install
	touch $(TARGDIR)/traceui

$(TARGDIR)/$(MACHINE)/timeruntime: $(RUNTIME)
	cd src/runtime;        $(MAKE) CFG=t install
	cd src/tracer/runtime; $(MAKE) CFG=t install
	touch $(TARGDIR)/$(MACHINE)/timeruntime
$(TARGDIR)/$(MACHINE)/timeprelude: greencard $(PRELUDEA) $(PRELUDEB)
	cd src/prelude;        $(MAKE) CFG=t install
	touch $(TARGDIR)/$(MACHINE)/timeprelude

$(TARGDIR)/$(MACHINE)/cprelude: $(PRELUDEC)
	cd src/prelude;        $(MAKE) fromC 
	touch $(TARGDIR)/$(MACHINE)/bootprelude $(TARGDIR)/$(MACHINE)/cprelude
$(TARGDIR)/$(MACHINE)/ccompiler: $(COMPILERC)
	cd src/compiler98;     $(MAKE) fromC
	touch $(TARGDIR)/$(MACHINE)/ccompiler
$(TARGDIR)/$(MACHINE)/cgreencard: $(GREENCARDC)
	cd src/greencard;      $(MAKE) fromC
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/cgreencard
$(TARGDIR)/$(MACHINE)/chmake: $(HMAKEC)
	cd src/hmake;          $(MAKE) fromC
	touch $(TARGDIR)/$(MACHINE)/chmake

script/errnogen.c: script/GenerateErrNo.hs
	hmake script/GenerateErrNo
	script/GenerateErrNo +RTS -H2M -RTS >script/errnogen.c


##### scripts for packaging various distribution formats

# Old, Haskell-only source distribution
#srcDist:
#	rm -f nhc98src-$(VERSION).tar nhc98src-$(VERSION).tar.gz
#	tar cf nhc98src-$(VERSION).tar $(BASIC)
#	#cd src/compiler98/; $(MAKE) cleandephs
#	tar rf nhc98src-$(VERSION).tar $(COMPILER)
#	#tar rf nhc98src-$(VERSION).tar $(DATA2C)
#	tar rf nhc98src-$(VERSION).tar $(SCRIPT)
#	tar rf nhc98src-$(VERSION).tar $(RUNTIME)
#	tar rf nhc98src-$(VERSION).tar $(PRELUDEA)
#	tar rf nhc98src-$(VERSION).tar $(PRELUDEB)
#	tar rf nhc98src-$(VERSION).tar $(RUNTIMET)
#	tar rf nhc98src-$(VERSION).tar $(PRELUDET)
#	tar rf nhc98src-$(VERSION).tar $(TRACEUI)
#	tar rf nhc98src-$(VERSION).tar $(HP2GRAPH)
#	tar rf nhc98src-$(VERSION).tar $(GREENCARD)
#	tar rf nhc98src-$(VERSION).tar $(HMAKE)
#	tar rf nhc98src-$(VERSION).tar $(MAN)
#	tar rf nhc98src-$(VERSION).tar $(INCLUDE)
#	tar rf nhc98src-$(VERSION).tar $(DOC)
#	mkdir nhc98-$(VERSION)
#	cd nhc98-$(VERSION); tar xf ../nhc98src-$(VERSION).tar
#	tar cf nhc98src-$(VERSION).tar nhc98-$(VERSION)
#	rm -r nhc98-$(VERSION)
#	gzip nhc98src-$(VERSION).tar

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

# This srcDist used to be the cDist.
srcDist: $(TARGDIR)/preludeC $(TARGDIR)/compilerC $(TARGDIR)/greencardC $(TARGDIR)/hmakeC nolinks
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
	tar rf nhc98src-$(VERSION).tar $(PRELUDET)
	tar rf nhc98src-$(VERSION).tar $(TRACEUI)
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
$(TARGDIR)/hmakeC: $(HMAKE)
	cd src/hmake;  $(MAKE) cfiles
	touch $(TARGDIR)/hmakeC



##### package up hmake separately

HBASIC  = src/hmake/README src/hmake/INSTALL
HMFILE  = src/hmake/Makefile.toplevel
HAUX1   = Makefile.inc COPYRIGHT
HAUX2   = src/Makefile*
HSCRIPT = script/hmake.inst script/hmakeconfig.inst \
	  script/harch script/confhc
HMAN    = man/hmake.1 docs/hmake
HCONF   = hmake-configure
HBIN    = lib/$(MACHINE)/MkProg lib/$(MACHINE)/Older \
	  targets/$(MACHINE)/config.cache

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

clean:
	#cd data2c;             $(MAKE) clean
	cd src/compiler98;      $(MAKE) clean
	cd src/runtime;         $(MAKE) clean;        $(MAKE) CFG=p  clean; $(MAKE) CFG=t clean
	cd src/prelude;         $(MAKE) clean;        $(MAKE) CFG=p  clean; $(MAKE) CFG=t clean
	cd src/tracer/runtime;  $(MAKE) CFG=T clean;  $(MAKE) CFG=pT clean
	cd src/tracer/prelude;  $(MAKE) CFG=T clean;  $(MAKE) CFG=pT clean
	cd src/greencard;       $(MAKE) clean
	cd src/hp2graph;        $(MAKE) clean
	cd src/hmake;           $(MAKE) clean

realclean: clean
	#cd data2c;        $(MAKE) realclean
	cd src/compiler98; $(MAKE) realclean
	cd $(TARGDIR)/$(MACHINE);  rm -f $(TARGETS)
	cd $(TARGDIR);  rm -f preludeC compilerC greencardC hmakeC traceui
	rm -f $(LIBDIR)/$(MACHINE)/*
	rm -f $(TARGDIR)/$(MACHINE)/config.cache
	rm -f script/nhc98 script/greencard script/hmake
