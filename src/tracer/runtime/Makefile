include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/runtime

apa:
	@echo "TRACING = ${TRACING}"
	@echo "DCFG = ${DCFG}"

ifeq "${TRACING}" "T"
SRCS  = io.c getconstr.c nodecache.c outputtrace.c minisocklib.c \
	stub.c demo_server.c vector.c
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


${DEMO}: ${OBJS}
	$(CC) -O -o $@ ${OBJDIR}/minisocklib.o ${OBJDIR}/demo_server.o

${OBJDIR}:
	mkdir -p ${OBJDIR}
${ARCHIVE}: objdir ${OBJS}
	rm -f $@
	ar cr $@ ${OBJS}
	ranlib $@


${OBJDIR}/demo_server.o: demo_server.c
	$(CC) -O -c demo_server.c -o $@

${OBJDIR}/minisocklib.o: minisocklib.c
	$(CC) ${DCFG} ${OPT} -c minisocklib.c -o $@
${OBJDIR}/stub.o: stub.c
	$(CC) ${DCFG} -I${INCDIR} ${ENDIAN} -c $< -o $@
${OBJDIR}/getconstr.o: getconstr.c getconstr.h 
	$(CC) ${DCFG} ${OPT} -I${INCDIR} -c getconstr.c -o $@
${OBJDIR}/io.o: io.c getconstr.h nodecache.h outputtrace.h
	$(CC) ${DCFG} ${OPT} -DPRELSRCDIR=${PRELSRCDIR} -I${INCDIR} ${ENDIAN} -c io.c -o $@
${OBJDIR}/outputtrace.o: outputtrace.c outputtrace.h
	$(CC) $${DCFG} ${OPT} -I${INCDIR} -I${SRCDIR}/runtime/Kernel -c outputtrace.c -o $@
${OBJDIR}/nodecache.o: nodecache.c
	$(CC) ${DCFG} ${OPT} -I${INCDIR} -c  -o $@ $<
${OBJDIR}/dummy.o: dummy.c
	$(CC) -c -o $@ $<
${OBJDIR}/vector.o: vector.c getconstr.h 
	$(CC) ${DCFG} ${OPT} -I${INCDIR} -c vector.c -o $@
