include Makefile.inc

OBJDIR = ${BUILDDIR}/obj/interpreter
TARGET = ${DST}/hi$(EXE)

SRCS   = hi.hs HmakeConfig.hs SimpleLineEditor.hs


ifeq "${HC}" "ghc"
HFLAGS = -fglasgow-exts -syslib lang -syslib posix
export HFLAGS
endif
ifeq "${HC}" "hbc"
endif

all: $(TARGET)
install: $(TARGET)
cfiles:
	$(LOCAL)hmake -nhc98 -C hi.hs
clean:
	-rm -f *.hi *.o $(OBJDIR)/*.o $(OBJDIR)/hi$(EXE)
realclean: clean
	-rm -f *.c $(TARGET)

$(TARGET): $(OBJDIR) $(SRCS)
	$(LOCAL)hmake -$(HC) hi -d$(OBJDIR) $(shell echo $(BUILDOPTS))
	mv $(OBJDIR)/hi$(EXE) $(TARGET)
	strip $(TARGET)

$(OBJDIR):
	mkdir -p $(OBJDIR)

fromC: $(OBJDIR)
	$(LOCAL)nhc98 -cpp -o $(TARGET) -d$(OBJDIR) *.c
	strip $(TARGET)


paper.ps: paper.dvi
	dvips paper.dvi -O -1.0cm,-2.6cm -o paper.ps
paper.dvi: paper.tex
	latex paper.tex
