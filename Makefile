# Default definitions filled in by config script, included from Makefile.inc
include Makefile.inc

VERSION = 1.0pre12
# When incrementing the version number, don't forget to change the
# corresponding version in the configure script!
#   (A trailing x means this version has not been released yet.)

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
	src/prelude/Char/Makefile* src/prelude/Char/*.hs \
	src/prelude/Complex/Makefile* src/prelude/Complex/*.hs \
	src/prelude/Debug/Makefile* src/prelude/Debug/*.hs \
	src/prelude/Directory/Makefile* src/prelude/Directory/*.hs \
	src/prelude/Directory/*.gc \
	src/prelude/GreenCard/Makefile* src/prelude/GreenCard/*.gc \
	src/prelude/Haskell/Makefile* src/prelude/Haskell/*.hs \
	src/prelude/IO/Makefile* src/prelude/IO/*.hs \
	src/prelude/Interrupt/Makefile* src/prelude/Interrupt/*.hs \
	src/prelude/Ix/Makefile* src/prelude/Ix/*.hs
PRELUDEB = \
	src/prelude/List/Makefile* src/prelude/List/*.hs \
	src/prelude/Locale/Makefile* src/prelude/Locale/*.hs \
	src/prelude/LowB/Makefile* src/prelude/LowB/*.hs \
	src/prelude/Maybe/Makefile* src/prelude/Maybe/*.hs \
	src/prelude/Monad/Makefile* src/prelude/Monad/*.hs \
	src/prelude/NonStd/Makefile* src/prelude/NonStd/*.hs \
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
	src/prelude/Time/*.c
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

COMPILER = src/compiler98/Makefile*  src/compiler98/*.hs  src/compiler98/*.gc
COMPILERC = src/compiler98/*.c
DATA2C = src/data2c/Makefile* src/data2c/*.hs
SCRIPT = script/hmake.inst script/greencard.inst script/nhc98.inst \
         script/hmakeconfig.inst script/nhc98tracer.inst lib/nhctracer.jar \
         script/nhc98heap.c script/harch script/confhc
GREENCARD = src/greencard/*.lhs src/greencard/*.hs \
	    src/greencard/Makefile*
GREENCARDC = src/greencard/*.c
HP2GRAPH = src/hp2graph/Makefile* src/hp2graph/README \
	   src/hp2graph/doc src/hp2graph/*.[hc1]
HMAKE = src/hmake/Makefile* src/hmake/*.hs src/hmake/README* \
	src/hmake/HISTORY src/hmake/Summary*
HMAKEC = src/hmake/*.c
RUNTIME = \
	src/Makefile* \
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
	 compiler_n compiler_b compiler_g \
	 hmake_n hmake_b hmake_g \
	 greencard_n greencard_b greencard_g \
	 ccompiler cprelude cgreencard chmake \
	 tracecompiler_n tracecompiler_b tracecompiler_g


##### compiler build + install scripts

basic: basic-${BUILDWITH}
all:   all-${BUILDWITH}
help:
	@echo "Common targets include:        basic all install clean realclean config"
	@echo "For a specific build-compiler: basic-hbc basic-ghc basic-nhc basic-gcc"
	@echo "                               all-hbc   all-ghc   all-nhc   all-gcc"
	@echo "  (other subtargets: runtime prelude profile timeprof hp2graph"
	@echo "                     compiler_b compiler_g compiler_n"
	@echo "                     tracer_b tracer_g tracer_n "
	@echo "                     hmake_b hmake_g hmake_n"
	@echo "                     greencard_b greencard_g greencard_n)"

config:
	./configure --config
install:
	./configure --install

basic-nhc: runtime hmake_n greencard_n compiler_n prelude
basic-hbc: runtime hmake_b greencard_b compiler_b prelude
basic-ghc: runtime hmake_g greencard_g compiler_g prelude
basic-gcc: runtime cprelude ccompiler cgreencard chmake
all-nhc: basic-nhc profile hp2graph tracer_n   #timeprof
all-hbc: basic-hbc profile hp2graph tracer_b   #timeprof
all-ghc: basic-ghc profile hp2graph tracer_g   #timeprof
all-gcc: basic-gcc profile hp2graph #tracer??  #timeprof

profile: profruntime profprelude
timeprof: timeruntime timeprelude
tracer_n: tracecompiler_n traceruntime traceprelude $(TARGDIR)/traceui
tracer_b: tracecompiler_b traceruntime traceprelude $(TARGDIR)/traceui
tracer_g: tracecompiler_g traceruntime traceprelude $(TARGDIR)/traceui


$(TARGETS): % : $(TARGDIR)/$(MACHINE)/%

$(TARGDIR)/$(MACHINE)/runtime: $(RUNTIME)
	cd src/runtime;        $(MAKE) install nhc98heap$(EXE)
	cd src/tracer/runtime; $(MAKE) install
	touch $(TARGDIR)/$(MACHINE)/runtime

$(TARGDIR)/$(MACHINE)/compiler_n: $(COMPILER)
	cd src/compiler98;     $(MAKE) HC=nhc98 install
	touch $(TARGDIR)/$(MACHINE)/compiler_n
$(TARGDIR)/$(MACHINE)/compiler_b: $(COMPILER)
	cd src/compiler98;     $(MAKE) HC=hbc install
	touch $(TARGDIR)/$(MACHINE)/compiler_b
$(TARGDIR)/$(MACHINE)/compiler_g: $(COMPILER)
	cd src/compiler98;     $(MAKE) HC=ghc install
	touch $(TARGDIR)/$(MACHINE)/compiler_g

#$(TARGDIR)/$(MACHINE)/bootprelude:
#	cd src/prelude;        $(MAKE) boot
#	touch $(TARGDIR)/$(MACHINE)/bootprelude
$(TARGDIR)/$(MACHINE)/prelude: $(PRELUDEA) $(PRELUDEB)
	cd src/prelude;        $(MAKE) install
	touch $(TARGDIR)/$(MACHINE)/prelude

#$(TARGDIR)/$(MACHINE)/greencard: $(TARGDIR)/$(MACHINE)/bootprelude $(GREENCARD)
#	cd src/greencard;      $(MAKE) install
#	touch $(TARGDIR)/$(MACHINE)/greencard
$(TARGDIR)/$(MACHINE)/greencard_n: $(GREENCARD)
	cd src/greencard;      $(MAKE) HC=nhc98 install
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/greencard_n
$(TARGDIR)/$(MACHINE)/greencard_b: $(GREENCARD)
	cd src/greencard;      $(MAKE) HC=hbc install
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/greencard_b
$(TARGDIR)/$(MACHINE)/greencard_g: $(GREENCARD)
	cd src/greencard;      $(MAKE) HC=ghc install
	touch $(TARGDIR)/$(MACHINE)/greencard $(TARGDIR)/$(MACHINE)/greencard_g

$(TARGDIR)/$(MACHINE)/hmake_n: $(HMAKE)
	cd src/hmake;          $(MAKE) HC=nhc98 install
	touch $(TARGDIR)/$(MACHINE)/hmake_n
$(TARGDIR)/$(MACHINE)/hmake_b: $(HMAKE)
	cd src/hmake;          $(MAKE) HC=hbc install
	touch $(TARGDIR)/$(MACHINE)/hmake_b
$(TARGDIR)/$(MACHINE)/hmake_g: $(HMAKE)
	cd src/hmake;          $(MAKE) HC=ghc install
	touch $(TARGDIR)/$(MACHINE)/hmake_g

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

$(TARGDIR)/$(MACHINE)/tracecompiler_n: $(COMPILER)
	cd src/compiler98;     $(MAKE) CFG=T HC=nhc98 install
	touch $(TARGDIR)/$(MACHINE)/tracecompiler_n
$(TARGDIR)/$(MACHINE)/tracecompiler_b: $(COMPILER)
	cd src/compiler98;     $(MAKE) CFG=T HC=hbc install
	touch $(TARGDIR)/$(MACHINE)/tracecompiler_b
$(TARGDIR)/$(MACHINE)/tracecompiler_g: $(COMPILER)
	cd src/compiler98;     $(MAKE) CFG=T HC=ghc install
	touch $(TARGDIR)/$(MACHINE)/tracecompiler_g
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

hmakeDist:
	rm -f hmake-(VERSION).tar hmake-(VERSION).tar.gz
	tar cf hmake-(VERSION).tar $(BASIC)
	tar rf hmake-(VERSION).tar $(HMAKE)
	tar rf hmake-(VERSION).tar $(HMAKEC)
	tar rf hmake-(VERSION).tar src/Makefile*
	tar rf hmake-(VERSION).tar $(SCRIPT)
	tar rf hmake-(VERSION).tar $(DOC)
	tar rf hmake-(VERSION).tar $(MAN)
	mkdir hmake-$(VERSION)
	cd hmake-$(VERSION); tar xf ../hmake-$(VERSION).tar
	tar cf hmake-$(VERSION).tar hmake-$(VERSION)
	rm -r hmake-$(VERSION)
	gzip hmake-$(VERSION).tar


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
