include Makefile.inc

OBJDIR = ${BUILDDIR}/obj/interpreter
TARGET = ${DST}/HInteractive$(EXE)

SRCS   = HInteractive.hs HiConfig.hs SimpleLineEditor.hs LexModule.hs
OTHERS = Unlit.hs Compiler.hs Config.hs	# from src/hmake

CHFILES = $(patsubst %.hs, ../hmake/%.$C, $(OTHERS))


ifeq "${HC}" "ghc"
HFLAGS = $(shell $(LOCAL)fixghc $(GHCSYM) \
                 -package lang -package util $(READLINE) )
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
	-rm -f *.hi *.hc
	$(HMAKE) -hc=$(LOCAL)nhc98 -I../hmake -C HInteractive.hs
clean:
	-rm -f *.hi *.o $(OBJDIR)/*.o $(OBJDIR)/HInteractive$(EXE)
realclean: clean
	-rm -f *.hc *.c

$(TARGET): $(OBJDIR) $(SRCS)
	$(HMAKE) -$(HC) HInteractive -d$(OBJDIR) $(shell echo "$(BUILDOPTS)") \
		-i../hmake
	mv $(OBJDIR)/HInteractive$(EXE) $(TARGET)
	strip $(TARGET)

$(OBJDIR):
	mkdir -p $(OBJDIR)

fromC: $(OBJDIR)
	cp $(CHFILES) .
	$(LOCAL)nhc98 -cpp -o $(TARGET) -d$(OBJDIR) *.$C
	strip $(TARGET)


paper.ps: paper.dvi
	#dvips paper.dvi -O -1.0cm,-2.6cm -o paper.ps
	dvips paper.dvi -o paper.ps
paper.dvi: paper.tex
	latex paper.tex
