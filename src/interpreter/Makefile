include Makefile.inc

OBJDIR = ${BUILDDIR}/obj/interpreter
TARGET = ${DST}/HInteractive$(EXE)

SRCS   = HInteractive.hs HmakeConfig.hs SimpleLineEditor.hs \
	 Unlit.hs LexModule.hs

#CFILES = $(patsubst %.hs, *.$C, $(SRCS))


ifeq "${HC}" "ghc"
HFLAGS = $(shell $(LOCAL)fixghc $(GHCSYM) -fglasgow-exts \
                 -package lang -package util -package posix $(READLINE) )
export HFLAGS
endif
ifeq "${HC}" "hbc"
HFLAGS =
export HFLAGS
endif
ifeq "${HC}" "nhc98"
HFLAGS =
export HFLAGS
endif

all: $(TARGET)
install: $(TARGET)
cfiles: $(SRCS)
	$(HMAKE) -hc=$(LOCAL)nhc98 -clean HInteractive.hs
	$(HMAKE) -hc=$(LOCAL)nhc98 -C HInteractive.hs
clean:
	-rm -f *.hi *.o $(OBJDIR)/*.o $(OBJDIR)/HInteractive$(EXE)
realclean: clean
	-rm -f *.hc *.c $(TARGET) cfiles

$(TARGET): $(OBJDIR) $(SRCS)
	$(HMAKE) -$(HC) HInteractive -d$(OBJDIR) $(shell echo "$(BUILDOPTS)")
	mv $(OBJDIR)/HInteractive$(EXE) $(TARGET)
	strip $(TARGET)

$(OBJDIR):
	mkdir -p $(OBJDIR)

fromC: $(OBJDIR)
	$(LOCAL)nhc98 -cpp -o $(TARGET) -d$(OBJDIR) *.$C
	strip $(TARGET)


paper.ps: paper.dvi
	#dvips paper.dvi -O -1.0cm,-2.6cm -o paper.ps
	dvips paper.dvi -o paper.ps
paper.dvi: paper.tex
	latex paper.tex
