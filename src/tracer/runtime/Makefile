include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/runtime

apa:
	@echo "TRACING = ${TRACING}"
	@echo "DCFG = ${DCFG}"

ifeq "${TRACING}" "T"
SRCS  = io.c getconstr.c nodecache.c minisocklib.c \
	stub.c vector.c numeric.c browsercomms.c ident.c trust.c \
	stacktrace.c fileformat.c
else
SRCS  = dummy.c
endif

OBJS= $(patsubst %.c,${OBJDIR}/%.o,${SRCS})
ARCHIVE = ${DST}/libdebug${CFG}.a

DEMO = ${OBJDIR}/demo_server

all: ${ARCHIVE} ${DEMO}
install: ${ARCHIVE}
demo: ${DEMO}
objdir: ${OBJDIR}
dbglib: ${DBGLIB}
cleanhc: cleanc
objs: $(OBJS)
rt: $(OBJS)
clean:
	rm -f $(OBJS)
realclean:
	rm -f ${ARCHIVE}
chdist:
	tar cf chdist.tar ${SRCS}


${DEMO}: ${OBJS} ${DEMO}.o
	$(CC) -O -o $@ ${OBJDIR}/minisocklib.o ${OBJDIR}/demo_server.o

${OBJDIR}:
	mkdir -p ${OBJDIR}
${ARCHIVE}: objdir ${OBJS}
	rm -f $@
	ar cr $@ ${OBJS}
	ranlib $@

ui.h: getconstr.h nodecache.h fileformat.h
fileformat.h: ident.h
ident.h:

${OBJDIR}/demo_server.o: demo_server.c
	$(CC) -O -c demo_server.c -o $@

${OBJDIR}/dummy.o: dummy.c
	$(CC) -c -o $@ $<
${OBJDIR}/minisocklib.o: minisocklib.c
	$(CC) ${DCFG} ${OPT} -c minisocklib.c -o $@
${OBJDIR}/stub.o: stub.c
	$(CC) ${DCFG} -I${INCDIR} ${ENDIAN} -c $< -o $@
${OBJDIR}/getconstr.o: getconstr.c getconstr.h 
	$(CC) ${DCFG} ${OPT} -I${INCDIR} -c getconstr.c -o $@
${OBJDIR}/numeric.o: numeric.c getconstr.h 
	$(CC) ${DCFG} ${OPT} -I${INCDIR} -c numeric.c -o $@
${OBJDIR}/browsercomms.o: browsercomms.c ui.h
	$(CC) ${DCFG} ${OPT} -DPRELSRCDIR=${PRELSRCDIR} -I${INCDIR} ${ENDIAN} -c browsercomms.c -o $@
${OBJDIR}/io.o: io.c ui.h getconstr.h
	$(CC) ${DCFG} ${OPT} -I${INCDIR} ${ENDIAN} -c io.c -o $@
${OBJDIR}/trust.o: trust.c ui.h
	$(CC) ${DCFG} ${OPT} -I${INCDIR} ${ENDIAN} -c trust.c -o $@
${OBJDIR}/ident.o: ident.c ui.h ident.h
	$(CC) ${DCFG} ${OPT} -I${INCDIR} ${ENDIAN} -c ident.c -o $@
${OBJDIR}/stacktrace.o: stacktrace.c ui.h
	$(CC) ${DCFG} ${OPT} -I${INCDIR} ${ENDIAN} -c stacktrace.c -o $@
${OBJDIR}/outputtrace.o: outputtrace.c outputtrace.h
	$(CC) ${DCFG} ${OPT} -I${INCDIR} -I${SRCDIR}/runtime/Kernel -c outputtrace.c -o $@
${OBJDIR}/nodecache.o: nodecache.c
	$(CC) ${DCFG} ${OPT} -I${INCDIR} -c  -o $@ $<
${OBJDIR}/vector.o: vector.c getconstr.h 
	$(CC) ${DCFG} ${OPT} -I${INCDIR} -c vector.c -o $@
${OBJDIR}/fileformat.o: fileformat.c fileformat.h 
	$(CC) ${DCFG} ${OPT} -I${INCDIR} -c fileformat.c -o $@
