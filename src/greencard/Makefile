include Makefile.inc

OBJDIR = $(BUILDDIR)/$(OBJ)/greencard
TARGET = $(DST)/greencard-nhc98$(EXE)

ifeq "nhc98" "$(HC)"
  HFLAGS = $(shell echo $(BUILDOPTS)) $(NHEAP)
endif
ifeq "ghc" "$(HC)"
  HFLAGS = $(shell echo $(BUILDOPTS)) +CTS $(shell $(LOCAL)fixghc $(GHCSYM) -syslib exts) -CTS
endif


SRCS =	Casm GetOpt GreenCard ListUtils Name Package \
	PrettyUtils Target Type NHCBackend DIS Decl  \
	FillIn NameSupply Pretty Proc Process \
	HandLex HandParse ParseLib
CFILES =  $(patsubst %, %.$C, ${SRCS})
OBJS   =  $(patsubst %, ${OBJDIR}/%.$O, ${SRCS})


all: $(TARGET)
install: $(TARGET)
cfiles:
	$(HMAKE) -HC=$(LOCAL)nhc98 -C GreenCard.lhs
fromC: $(OBJDIR)
	$(HC) -c -d $(OBJDIR) ${CFILES}
	$(HC) -H4Mb -o $(TARGET) $(OBJS)
	strip $(TARGET)
clean:
	rm -f $(OBJS) *.hi
realclean: clean
	rm -f $(CFILES) $(TARGET)


$(OBJDIR):
	mkdir -p $(OBJDIR) || /bin/true
$(TARGET): $(OBJDIR) $(OBJDIR)/GreenCard$(EXE)
	mv $(OBJDIR)/GreenCard$(EXE) $(TARGET)
	strip $(TARGET)
$(OBJDIR)/GreenCard$(EXE):
	$(HMAKE) -$(HC) $(HFLAGS) -d $(OBJDIR) GreenCard



## C-files deps
#
#Casm.c:        Pretty.c PrettyUtils.c Target.c
#DIS.c:         Name.c Pretty.c PrettyUtils.c Casm.c
#Decl.c:        Name.c DIS.c Type.c Pretty.c
#FillIn.c:      Decl.c Proc.c Name.c DIS.c Type.c Casm.c Pretty.c PrettyUtils.c ListUtils.c NameSupply.c
#GreenCard.c:   Package.c GetOpt.c Process.c Target.c ListUtils.c
#HandParse.c:   HandLex.c Name.c Type.c DIS.c Decl.c ParseLib.c
#NameSupply.c:  Name.c
#NHCBackend.c:  Pretty.c PrettyUtils.c Decl.c DIS.c Casm.c FillIn.c NameSupply.c
#ParseLib.c:    HandLex.c
#PrettyUtils.c: Pretty.c
#Proc.c:        Pretty.c PrettyUtils.c Name.c Type.c DIS.c Decl.c Casm.c NameSupply.c Target.c #NHCBackend.c
#Process.c:     ListUtils.c Pretty.c PrettyUtils.c HandParse.c HandLex.c Name.c DIS.c Decl.c Proc.c FillIn.c Target.c NHCBackend.c
#Type.c:        Pretty.c PrettyUtils.c

