include Makefile.inc

OBJDIR	= ${BUILDDIR}/${OBJ}/hat
DEST	= $(LIBDIR)/$(MACHINE)

MALCLIBS = nodecache.c minisocklib.c utils.c browsercomms.c
THORLIBS = detect.c nodelist.c menu.c hatinterface.c FunTable.c Expressions.c \
           hashtable.c observe.c hatgeneral.c

MALCOBJS = $(patsubst %.c,$(OBJDIR)/%.o,$(MALCLIBS))
THOROBJS = $(patsubst %.c,$(OBJDIR)/%.o,$(THORLIBS))


PROGNAMES = hat-stack hat-connect hat-check hat-observe hat-detect hat-checki
PROGS = $(patsubst %, $(DEST)/%, $(PROGNAMES))

all: $(OBJDIR) $(PROGS)
install: $(OBJDIR) $(PROGS)
objdir: $(OBJDIR)
objs: $(MALCOBJS) $(THORLIBS) $(patsubst %, %.o, $(PROGS))
clean:
	rm -f $(MALCOBJS) $(THOROBJS)
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
$(DEST)/hat-connect: hat-connect.c browsercomms.h $(MALCOBJS)
	$(CC) $(OPT) -I$(INCDIR) -DPRELSRCDIR=$(PRELSRCDIR) hat-connect.c \
		-o $(DEST)/hat-connect $(MALCOBJS)
$(DEST)/hat-stack: hat-stack.c utils.h $(MALCOBJS)
	$(CC) $(OPT) -I$(INCDIR) hat-stack.c $(OBJDIR)/utils.o \
		-o $(DEST)/hat-stack


# Colin's libraries and tools
$(DEST)/hat-check: hat-check.c
	$(CC) hat-check.c -o $(DEST)/hat-check


# Thorsten's libraries and tools
THORFLAGS = -g
LINKFLAGS = -lncurses -g

$(THOROBJS) : $(OBJDIR)/%.o : %.c
	$(CC) -c $(THORFLAGS) -o $@ $<
$(OBJDIR)/hatinterface.o: hatinterface.h hatfile.h $(OBJDIR)/nodelist.o
$(OBJDIR)/FunTable.o: FunTable.h $(OBJDIR)/Expressions.o $(OBJDIR)/menu.o $(OBJDIR)/hatinterface.o
$(OBJDIR)/Expressions.o: Expressions.h hatfile.h $(OBJDIR)/hatinterface.o
$(OBJDIR)/hashtable.o: hashtable.h
$(OBJDIR)/observe.o : $(OBJDIR)/hatinterface.o $(OBJDIR)/hashtable.o
$(OBJDIR)/menu.o : menu.h $(OBJDIR)/Expressions.o
$(OBJDIR)/nodelist.o : nodelist.h FunTable.h hatinterface.h Expressions.h
$(OBJDIR)/hatgeneral.o : hatgeneral.h hatgeneral.c
$(OBJDIR)/detect.o : detect.h FunTable.h hatinterface.h Expressions.h nodelist.h hashtable.h
$(DEST)/hat-detect: hat-detect.c $(OBJDIR)/hatinterface.o $(OBJDIR)/observe.o $(OBJDIR)/detect.o $(OBJDIR)/Expressions.o $(OBJDIR)/hatgeneral.o $(OBJDIR)/FunTable.o
	$(CC) $(LINKFLAGS) hat-detect.c $(THOROBJS) \
		-o $(DEST)/hat-detect
$(DEST)/hat-observe: hat-observe.c $(OBJDIR)/observe.o $(OBJDIR)/hatinterface.o $(OBJDIR)/hashtable.o $(OBJDIR)/detect.o $(OBJDIR)/Expressions.o $(OBJDIR)/hatgeneral.o $(OBJDIR)/FunTable.o
	$(CC) $(LINKFLAGS) hat-observe.c $(THOROBJS) $(ncurses) \
		-o $(DEST)/hat-observe
$(DEST)/hat-checki: hat-checki.c $(OBJDIR)/hatinterface.o $(OBJDIR)/hatgeneral.o
	$(CC) $(LINKFLAGS) hat-checki.c $(THOROBJS) $(ncurses) \
		-o $(DEST)/hat-checki


