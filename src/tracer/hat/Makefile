include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/hat

SRCS  = nodecache.c minisocklib.c \
	utils.c browsercomms.c \
	hat-stack.c

OBJS = $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))

PROGS = hat-stack hat-connect hat-check

all: $(OBJDIR) $(PROGS)
install: $(OBJDIR) $(PROGS)
	$(INSTALL) $(PROGS) $(LIBDIR)/$(MACHINE)
objdir: $(OBJDIR)
objs: $(OBJS)
clean:
	rm -f $(OBJS)
realclean: clean
	rm -f $(PROGS)


hat-stack: $(OBJS)
	$(CC) -o hat-stack $(OBJDIR)/hat-stack.o $(OBJDIR)/utils.o
hat-connect: $(OBJS)
	$(CC) -o hat-connect $(OBJDIR)/browsercomms.o $(OBJDIR)/utils.o \
			$(OBJDIR)/nodecache.o $(OBJDIR)/minisocklib.o
hat-check: hat-check.c
	$(CC) -o hat-check hat-check.c


${OBJDIR}:
	mkdir -p $(OBJDIR)

browsercomms.h: nodecache.h utils.h

${OBJDIR}/minisocklib.o: minisocklib.c
	$(CC) ${OPT} -c minisocklib.c -o $@
${OBJDIR}/browsercomms.o: browsercomms.c browsercomms.h
	$(CC) ${OPT} -DPRELSRCDIR=${PRELSRCDIR} -I${INCDIR} -c browsercomms.c -o $@
${OBJDIR}/hat-stack.o: hat-stack.c utils.h
	$(CC) ${OPT} -I${INCDIR} -c hat-stack.c -o $@
${OBJDIR}/nodecache.o: nodecache.c nodecache.h
	$(CC) ${OPT} -I${INCDIR} -c nodecache.c -o $@
${OBJDIR}/utils.o: utils.c utils.h 
	$(CC) ${OPT} -I${INCDIR} -c utils.c -o $@
