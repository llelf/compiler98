include Makefile.inc

OBJDIR = $(BUILDDIR)/$(OBJ)/greencard
TARGET = $(DST)/greencard-nhc98$(EXE)

ifeq "nhc98" "$(HC)"
  HFLAGS = $(shell echo $(BUILDOPTS)) $(NHEAP)
endif
ifeq "ghc" "$(HC)"
  HFLAGS = $(shell echo $(BUILDOPTS)) +CTS $(shell $(LOCAL)fixghc $(GHCSYM) -syslib exts) -CTS
endif

## Since adopting hmake, all of this junk is obsolete
#
#LSRCS = Casm.lhs GetOpt.lhs GreenCard.lhs \
#        ListUtils.lhs Name.lhs Package.lhs \
#        PrettyUtils.lhs Target.lhs Type.lhs
#USRCS = NHCBackend.hs
#CPPLSRCS = DIS.lhs Decl.lhs FillIn.lhs NameSupply.lhs Pretty.lhs \
#           Proc.lhs Process.lhs
#CPPUSRCS = HandLex.hs HandParse.hs ParseLib.hs
#
#LOBJS    =  $(patsubst %.lhs,${OBJDIR}/%.o,${LSRCS})
#UOBJS    =  $(patsubst %.hs,${OBJDIR}/%.o,${USRCS})
#CPPLOBJS =  $(patsubst %.lhs,${OBJDIR}/%.o,${CPPLSRCS})
#CPPUOBJS =  $(patsubst %.hs,${OBJDIR}/%.o,${CPPUSRCS})
#
#OBJS = ${LOBJS} ${UOBJS} ${CPPLOBJS} ${CPPUOBJS}
#
#LCFILES    =  $(patsubst %.lhs,%.c,${LSRCS})
#UCFILES    =  $(patsubst %.hs,%.c,${USRCS})
#CPPLCFILES =  $(patsubst %.lhs,%.c,${CPPLSRCS})
#CPPUCFILES =  $(patsubst %.hs,%.c,${CPPUSRCS})
#
#
#CFILES = ${LCFILES} ${UCFILES} ${CPPLCFILES} ${CPPUCFILES}
##

## Replaced by:
#
SRCS =	Casm GetOpt GreenCard ListUtils Name Package \
	PrettyUtils Target Type NHCBackend DIS Decl  \
	FillIn NameSupply Pretty Proc Process \
	HandLex HandParse ParseLib
CFILES =  $(patsubst %, %.c, ${SRCS})
OBJS   =  $(patsubst %, ${OBJDIR}/%.o, ${SRCS})


all: $(TARGET)
install: $(TARGET)
cfiles:  #${CFILES}
	$(LOCAL)hmake -nhc98 -C GreenCard.lhs
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
	$(LOCAL)hmake -$(HC) $(HFLAGS) -d $(OBJDIR) GreenCard

## Obsolete
#
#$(TARGET): $(OBJDIR) $(OBJS)
#	$(HC) -H4Mb -o $(TARGET) $(OBJS)
#	strip $(TARGET)
##


## Since adopting hmake, everything else is obsolete (except the C-deps)
#
#.SUFFIXES: .lhs .hs .c .o
#
#${LOBJS}: $(OBJDIR)/%.o : %.lhs
#	$(HC) -c $(HFLAGS) -o $@ $<
#${UOBJS}: $(OBJDIR)/%.o : %.hs
#	$(HC) -c $(HFLAGS) -o $@ $<
#${CPPLOBJS}: $(OBJDIR)/%.o : %.lhs
#	$(HC) -c -cpp $(HFLAGS) -o $@ $<
#${CPPUOBJS}: $(OBJDIR)/%.o : %.hs
#	$(HC) -c -cpp $(HFLAGS) -o $@ $<
#
#${LCFILES}: %.c : %.lhs
#	$(HC) -C $(HFLAGS) $<
#${UCFILES}: %.c : %.hs
#	$(HC) -C $(HFLAGS) $<
#${CPPLCFILES}: %.c : %.lhs
#	$(HC) -C -cpp $(HFLAGS) $<
#${CPPUCFILES}: %.c : %.hs
#	$(HC) -C -cpp $(HFLAGS) $<
#
## deps
#$(OBJDIR)/Casm.o: Casm.lhs $(OBJDIR)/Pretty.o $(OBJDIR)/PrettyUtils.o $(OBJDIR)/Target.o
#$(OBJDIR)/DIS.o: DIS.lhs $(OBJDIR)/Name.o $(OBJDIR)/Pretty.o $(OBJDIR)/PrettyUtils.o $(OBJDIR)/Casm.o
#$(OBJDIR)/Decl.o: Decl.lhs $(OBJDIR)/Name.o $(OBJDIR)/DIS.o $(OBJDIR)/Type.o $(OBJDIR)/Pretty.o
#$(OBJDIR)/FillIn.o: FillIn.lhs $(OBJDIR)/Decl.o $(OBJDIR)/Proc.o $(OBJDIR)/Name.o $(OBJDIR)/DIS.o $(OBJDIR)/Type.o $(OBJDIR)/Casm.o $(OBJDIR)/Pretty.o $(OBJDIR)/PrettyUtils.o $(OBJDIR)/ListUtils.o $(OBJDIR)/NameSupply.o
#$(OBJDIR)/GetOpt.o: GetOpt.lhs
#$(OBJDIR)/GreenCard.o: GreenCard.lhs $(OBJDIR)/Package.o $(OBJDIR)/GetOpt.o $(OBJDIR)/Process.o $(OBJDIR)/Target.o $(OBJDIR)/ListUtils.o
#$(OBJDIR)/HandLex.o: HandLex.hs
#$(OBJDIR)/HandParse.o: HandParse.hs $(OBJDIR)/HandLex.o $(OBJDIR)/Name.o $(OBJDIR)/Type.o $(OBJDIR)/DIS.o $(OBJDIR)/Decl.o $(OBJDIR)/ParseLib.o
#$(OBJDIR)/ListUtils.o: ListUtils.lhs
#$(OBJDIR)/Name.o: Name.lhs
#$(OBJDIR)/NameSupply.o: NameSupply.lhs $(OBJDIR)/Name.o
#$(OBJDIR)/NHCBackend.o: NHCBackend.hs $(OBJDIR)/Pretty.o $(OBJDIR)/PrettyUtils.o $(OBJDIR)/Decl.o $(OBJDIR)/DIS.o $(OBJDIR)/Casm.o $(OBJDIR)/FillIn.o $(OBJDIR)/NameSupply.o
#$(OBJDIR)/Package.o: Package.lhs
#$(OBJDIR)/ParseLib.o: ParseLib.hs $(OBJDIR)/HandLex.o
#$(OBJDIR)/Pretty.o: Pretty.lhs
#$(OBJDIR)/PrettyUtils.o: PrettyUtils.lhs $(OBJDIR)/Pretty.o
#$(OBJDIR)/Proc.o: Proc.lhs $(OBJDIR)/Pretty.o $(OBJDIR)/PrettyUtils.o $(OBJDIR)/Name.o $(OBJDIR)/Type.o $(OBJDIR)/DIS.o $(OBJDIR)/Decl.o $(OBJDIR)/Casm.o $(OBJDIR)/NameSupply.o $(OBJDIR)/Target.o #NHCBackend.o
#$(OBJDIR)/Process.o: Process.lhs $(OBJDIR)/ListUtils.o $(OBJDIR)/Pretty.o $(OBJDIR)/PrettyUtils.o $(OBJDIR)/HandParse.o $(OBJDIR)/HandLex.o $(OBJDIR)/Name.o $(OBJDIR)/DIS.o $(OBJDIR)/Decl.o $(OBJDIR)/Proc.o $(OBJDIR)/FillIn.o $(OBJDIR)/Target.o $(OBJDIR)/NHCBackend.o
#$(OBJDIR)/Target.o: Target.lhs
#$(OBJDIR)/Type.o: Type.lhs $(OBJDIR)/Pretty.o $(OBJDIR)/PrettyUtils.o
#
##

## C-files deps
#
Casm.c:        Pretty.c PrettyUtils.c Target.c
DIS.c:         Name.c Pretty.c PrettyUtils.c Casm.c
Decl.c:        Name.c DIS.c Type.c Pretty.c
FillIn.c:      Decl.c Proc.c Name.c DIS.c Type.c Casm.c Pretty.c PrettyUtils.c ListUtils.c NameSupply.c
GreenCard.c:   Package.c GetOpt.c Process.c Target.c ListUtils.c
HandParse.c:   HandLex.c Name.c Type.c DIS.c Decl.c ParseLib.c
NameSupply.c:  Name.c
NHCBackend.c:  Pretty.c PrettyUtils.c Decl.c DIS.c Casm.c FillIn.c NameSupply.c
ParseLib.c:    HandLex.c
PrettyUtils.c: Pretty.c
Proc.c:        Pretty.c PrettyUtils.c Name.c Type.c DIS.c Decl.c Casm.c NameSupply.c Target.c #NHCBackend.c
Process.c:     ListUtils.c Pretty.c PrettyUtils.c HandParse.c HandLex.c Name.c DIS.c Decl.c Proc.c FillIn.c Target.c NHCBackend.c
Type.c:        Pretty.c PrettyUtils.c

