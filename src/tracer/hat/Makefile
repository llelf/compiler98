include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/hat

MALCLIBS = nodecache.c minisocklib.c utils.c browsercomms.c
THORLIBS = hatfileops.c FunTable.c Expressions.c hashtable.c

MALCOBJS = $(patsubst %.c,$(OBJDIR)/%.o,$(MALCLIBS))
THOROBJS = $(patsubst %.c,$(OBJDIR)/%.o,$(THORLIBS))


PROGS = hat-stack hat-connect hat-check hat-observe hat-detect hat-interact

all: $(OBJDIR) $(PROGS)
install: $(OBJDIR) $(PROGS)
	$(INSTALL) $(PROGS) $(LIBDIR)/$(MACHINE)
objdir: $(OBJDIR)
objs: $(MALCOBJS) $(THORLIBS) $(patsubst %, %.o, $(PROGS))
clean:
	rm -f $(MALCOBJS)
realclean: clean
	rm -f $(PROGS)
$(OBJDIR):
	mkdir -p $(OBJDIR)


# Malcolm's libraries and tools
browsercomms.h: nodecache.h utils.h
$(MALCOBJS) : $(OBJDIR)/%.o : %.c
	$(CC) $(OPT) -I$(INCDIR) -c $< -o $@
$(OBJDIR)/nodecache.o: nodecache.h
$(OBJDIR)/utils.o: utils.h 
$(OBJDIR)/browsercomms.o: browsercomms.h
hat-connect: hat-connect.c browsercomms.h $(MALCOBJS)
	$(CC) $(OPT) -I$(INCDIR) -DPRELSRCDIR=$(PRELSRCDIR) hat-connect.c \
		-o hat-connect $(MALCOBJS)
hat-stack: hat-stack.c utils.h $(MALCOBJS)
	$(CC) $(OPT) -I$(INCDIR) -o hat-stack hat-stack.c $(OBJDIR)/utils.o


# Colin's libraries and tools
hat-check: hat-check.c
	$(CC) -o hat-check hat-check.c


# Thorsten's libraries and tools
THORFLAGS = -g
$(THOROBJS) : $(OBJDIR)/%.o : %.c
	$(CC) -c $(THORFLAGS) -o $@ $<
$(OBJDIR)/hatfileops.o: hatfileops.h hatfile.h \
			$(OBJDIR)/FunTable.o $(OBJDIR)/Expressions.o
$(OBJDIR)/FunTable.o: FunTable.h $(OBJDIR)/Expressions.o hatfileops.h
$(OBJDIR)/Expressions.o: Expressions.h hatfile.h
$(OBJDIR)/hashtable.o: hashtable.h
hat-detect: hat-detect.c $(OBJDIR)/hatfileops.o
	$(CC) $(THORFLAGS) hat-detect.c -o hat-detect $(THOROBJS)
hat-observe: hat-observe.c $(OBJDIR)/hatfileops.o $(OBJDIR)/hashtable.o
	$(CC) $(THORFLAGS) hat-observe.c -o hat-observe $(THOROBJS)
hat-interact: hat-interact.c $(OBJDIR)/hatfileops.o
	$(CC) $(THORFLAGS) hat-interact.c -o hat-interact $(THOROBJS)


