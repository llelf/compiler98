include Makefile.inc

OBJDIR = ${BUILDDIR}/obj/interpreter
TARGET = ${DST}/HInteractive$(EXE)

SRCS   = HInteractive.hs HmakeConfig.hs SimpleLineEditor.hs \
	 Unlit.hs LexModule.hs

#CFILES = $(patsubst %.hs, *.c, $(SRCS))


ifeq "${HC}" "ghc"
HFLAGS = $(shell $(LOCAL)fixghc $(GHCSYM) -fglasgow-exts -syslib exts)
export HFLAGS
endif
ifeq "${HC}" "hbc"
endif
ifeq "${HC}" "nhc98"
HFLAGS = -p
export HFLAGS
endif

all: $(TARGET)
install: $(TARGET)
cfiles: $(SRCS)
	$(LOCAL)hmake -clean HInteractive.hs
	$(LOCAL)hmake -nhc98 -C HInteractive.hs
	touch cfiles
clean:
	-rm -f *.hi *.o $(OBJDIR)/*.o $(OBJDIR)/HInteractive$(EXE)
realclean: clean
	-rm -f *.c $(TARGET)

$(TARGET): $(OBJDIR) $(SRCS)
	$(LOCAL)hmake -$(HC) HInteractive -d$(OBJDIR) $(shell echo $(BUILDOPTS))
	mv $(OBJDIR)/HInteractive$(EXE) $(TARGET)
	strip $(TARGET)

$(OBJDIR):
	mkdir -p $(OBJDIR)

fromC: $(OBJDIR)
	$(LOCAL)nhc98 -cpp -o $(TARGET) -d$(OBJDIR) *.c
	strip $(TARGET)


paper.ps: paper.dvi
	#dvips paper.dvi -O -1.0cm,-2.6cm -o paper.ps
	dvips paper.dvi -o paper.ps
paper.dvi: paper.tex
	latex paper.tex
