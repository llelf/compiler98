include Makefile.inc

PROG =	$(MACHINE)/hi
SRCS =	$(MACHINE)/hi.hs $(MACHINE)/HmakeConfig.hs $(MACHINE)/LocalConfig.hs


ifeq "${BUILDWITH}" "ghc"
HFLAGS = -fglasgow-exts -syslib lang
export HFLAGS
endif
ifeq "${BUILDWITH}" "hbc"
endif

all: $(PROG)
install: all
	mv $(PROG) $(DST)
clean:
	-rm -f $(PROG)
	cd $(MACHINE); rm -f *.hi *.o

$(PROG): $(SRCS)
	cd $(MACHINE); $(LOCAL)hmake -$(BUILDWITH) $(shell echo $(BUILDOPTS)) hi

$(MACHINE)/hi.hs: hi.hs
	cp hi.hs $(MACHINE)
$(MACHINE)/HmakeConfig.hs: HmakeConfig.hs
	cp HmakeConfig.hs $(MACHINE)


paper.ps: paper.dvi
	dvips paper.dvi -O -1.0cm,-2.6cm -o paper.ps
paper.dvi: paper.tex
	latex paper.tex
