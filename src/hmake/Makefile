include Makefile.inc

OBJDIR = $(BUILDDIR)/$(OBJ)/hmake
MKPROG = $(DST)/MkProg
OLDER  = $(DST)/Older
TARGETS = $(MKPROG) $(OLDER)

SRCS    = QSort.hs Unlit.hs Utils.hs Tsort.hs FileName.hs \
          SymTab.hs Imports.hs \
	  Output.hs Order.hs ListUtil.hs Getmodtime.hs \
          Argv.hs MkProg.hs
CPPSRCS = Graph.hs GetDep.hs ParseLib.hs Compat.hs

OBJS    = $(patsubst %.hs,$(OBJDIR)/%.o,$(SRCS))
CPPOBJS = $(patsubst %.hs,$(OBJDIR)/%.o,$(CPPSRCS))
CFILES    = $(patsubst %.hs,%.c,$(SRCS))
CPPCFILES = $(patsubst %.hs,%.c,$(CPPSRCS))

AUX     = Makefile* BuildNew* HISTORY README* Summary TODO nhcmakeR cfiles \
          hmake hmake.1

HC = $(LOCAL)nhc98
HFLAGS = #-$(CFG)

ifeq "nhc98" "$(HC)"
HEAP = -H2M
endif

all: $(TARGETS)
install: $(TARGETS)
cfiles: $(CFILES) $(CPPCFILES) Older.c
fromC: $(OBJDIR)
	$(HC) $(HFLAGS) $(HEAP) -o $(MKPROG) -d $(OBJDIR) $(CFILES) $(CPPCFILES)
	$(HC) $(HFLAGS)         -o $(OLDER)  -d $(OBJDIR) Older.c
	strip $(MKPROG) $(OLDER)
clean:
	rm -f $(OBJS) $(CPPOBJS) $(OBJDIR)/Older.o *.hi
realclean: clean
	rm -f $(TARGETS) *.c
package:
	tar zcvf hmake.tar.gz $(SRCS) $(GCSRCS) $(CPPSRCS) Older.hs $(AUX)


$(MKPROG): $(OBJDIR) $(OBJS) $(CPPOBJS)
	$(HC) $(HFLAGS) $(HEAP) -o $@  $(OBJS) $(CPPOBJS)
	strip $@
$(OLDER): $(OBJDIR) $(OBJDIR)/Older.o
	$(HC) $(HFLAGS)         -o $@  $(OBJDIR)/Older.o
	strip $@

$(OBJDIR):
	mkdir -p $(OBJDIR) || /bin/true
$(OBJS): $(OBJDIR)/%.o : %.hs
	$(HC) $(HFLAGS) -c -o $@ $<
$(CPPOBJS) $(OBJDIR)/Older.o: $(OBJDIR)/%.o : %.hs
	$(HC) $(HFLAGS) -c -cpp -o $@ $<

$(CFILES): %.c : %.hs
	$(HC) $(HFLAGS) -C $<
$(CPPCFILES) Older.c: %.c : %.hs
	$(HC) $(HFLAGS) -C -cpp $<


$(OBJDIR)/Argv.o: Argv.hs $(OBJDIR)/ListUtil.o
$(OBJDIR)/Compat.o: Compat.hs $(OBJDIR)/ListUtil.o $(OBJDIR)/QSort.o
$(OBJDIR)/FileName.o: FileName.hs
$(OBJDIR)/GetDep.o: GetDep.hs $(OBJDIR)/Getmodtime.o $(OBJDIR)/Imports.o $(OBJDIR)/FileName.o $(OBJDIR)/Unlit.o
$(OBJDIR)/Getmodtime.o: Getmodtime.hs
$(OBJDIR)/Graph.o: Graph.hs $(OBJDIR)/Compat.o $(OBJDIR)/ListUtil.o
$(OBJDIR)/Imports.o: Imports.hs $(OBJDIR)/SymTab.o $(OBJDIR)/ParseLib.o $(OBJDIR)/ListUtil.o
$(OBJDIR)/ListUtil.o: ListUtil.hs
$(OBJDIR)/MkProg.o: MkProg.hs $(OBJDIR)/Argv.o $(OBJDIR)/GetDep.o $(OBJDIR)/Getmodtime.o $(OBJDIR)/ListUtil.o $(OBJDIR)/Order.o $(OBJDIR)/Output.o
$(OBJDIR)/Older.o: Older.hs
$(OBJDIR)/Order.o: Order.hs $(OBJDIR)/Compat.o $(OBJDIR)/Graph.o $(OBJDIR)/ListUtil.o $(OBJDIR)/Tsort.o $(OBJDIR)/Utils.o
$(OBJDIR)/Output.o: Output.hs $(OBJDIR)/ListUtil.o $(OBJDIR)/FileName.o $(OBJDIR)/Argv.o
$(OBJDIR)/QSort.o: QSort.hs
$(OBJDIR)/ParseLib.o: ParseLib.hs
$(OBJDIR)/SymTab.o: SymTab.hs
$(OBJDIR)/Tsort.o: Tsort.hs $(OBJDIR)/Compat.o
$(OBJDIR)/Unlit.o: Unlit.hs
$(OBJDIR)/Utils.o: Utils.hs $(OBJDIR)/Compat.o $(OBJDIR)/ListUtil.o

#$(OBJDIR)/Cpp.o: Cpp.gc
#$(OBJDIR)/Env.o: Env.hs $(OBJDIR)/Compat.o $(OBJDIR)/ListUtil.o $(OBJDIR)/Unlit.o $(OBJDIR)/Utils.o


# C-files dependencies.
Argv.c:    ListUtil.c
Compat.c:  ListUtil.c QSort.c
GetDep.c:  Getmodtime.c Imports.c FileName.c Unlit.c
Graph.c:   Compat.c ListUtil.c
Imports.c: SymTab.c ParseLib.c ListUtil.c
MkProg.c:  Argv.c GetDep.c Getmodtime.c ListUtil.c Order.c Output.c
Order.c:   Compat.c Graph.c ListUtil.c Tsort.c Utils.c
Output.c:  ListUtil.c FileName.c Argv.c
Tsort.c:   Compat.c
Utils.c:   Compat.c ListUtil.c
