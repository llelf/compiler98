include Makefile.inc

#VPATH = $(INCDIR)

DIR=LiterateKernel
OBJDIR=${BUILDDIR}/${OBJ}/runtime/${DIR}

FWS   = $(FWM) $(FWIS)              
FWM   = doc.fw
FWIS  = bytecode.fwi cdata.fwi cinterface.fwi collector.fwi debug.fwi \
        graph.fwi main.fwi mark.fwi mp.fwi mutator.fwi newbuiltin.fwi \
        newtables.fwi profile.fwi stableptr.fwi system.fwi unix.fwi \
        util.fwi

OSRCS = mutator.c mutlib.c main.c
SSRCS = newtables.c newbuiltin.c
ASRCS = bytecode.c cdata.c collector.c ffiexport.c mark.c stableptr.c \
        timeUnix.c system.c util.c

LINKS = bytecode.h cinterface.h mutlib.h newmacros.h node.h runtime.h \
        stableptr.h newbytecode.h

HDRS  = ${LINKS} mark.h

ifneq "${PROFILING}" ""
ASRCS += profile.c remark.c xmem.c retainer.c timer.c
endif
ifneq "${TPROF}" ""
ASRCS += tprof.c timer.c
TPLOBJS = $(patsubst %,${OBJDIR}/tpl%Builtin.o,1 2 3)
endif

SRCS = ${ASRCS} ${SSRCS} ${OSRCS}

OOBJS = $(patsubst %.c,${OBJDIR}/%.o,${OSRCS})
SOBJS = $(patsubst %.c,${OBJDIR}/%.o,${SSRCS})
AOBJS = $(patsubst %.c,${OBJDIR}/%.o,${ASRCS})

ifeq "$(CFG)" ""
  ARCHIVE=${OBJDIR}/Runtime.a 
else
  ARCHIVE=${OBJDIR}/Runtime.${CFG}.a 
endif

OBJS = ${AOBJS} ${SOBJS} ${OOBJS}

CFLAGSNOOPT = ${DCFG} ${BUGFIX} ${ENDIAN} -I${INCDIR} \
         -DVERSION="\"$(VERSION)\""  #-DBYTECODE_PROF
CFLAGS = ${CFLAGSNOOPT} ${OPT}
CPP = $(CC) ${DCFG} ${ENDIAN} -E 
LDFLAGS = -r
AS = $(CC) -c

${AOBJS} ${OOBJS}: ${OBJDIR}/%.o: %.c
	${CC} -c ${CFLAGS} -o $@ $<
${SOBJS}: ${OBJDIR}/%.o: %.c
	${CC} -S ${CFLAGSNOOPT} -o - $< |\
            sed -e '/.align 32/s/32/4/' -e '/.p2align 5/s/5/2/' |\
            ${CC} -c -x assembler-with-cpp ${CFLAGSNOOPT} -o $@ -

TANGLE = ${FW}

srcs: ${SRCS}

${SRCS}: ${FWS}
	${TANGLE} ${FWM}

links:
	@cd ${INCDIR}; \
	for f in ${LINKS}; do \
        ln -s ../src/runtime/${DIR}/$$f >/dev/null 2>&1 || \
            echo "Link ${INCDIR}/$$f already exists."; \
    done
nolinks:
	@for f in ${LINKS}; do \
        rm -f ${INCDIR}/$$f; \
    done

xxx: 
	echo ${BUILDDIR}

ifeq "${TPROF}" ""

${ARCHIVE}: ${AOBJS} ${SOBJS}
	cd ${OBJDIR}; \
            $(AR) $(ARFLAGS) $@ $(patsubst ${OBJDIR}/%,%,${AOBJS} ${SOBJS})
else

${ARCHIVE}: ${AOBJS} ${SOBJS} ${TPLOBJS}
	cd ${OBJDIR}; $(AR) $(ARFLAGS) $@ \
            $(patsubst ${OBJDIR}/%,%,${TPLOBJS} ${AOBJS} ${SOBJS})
	rm -f ${TPLOBJS}

${TPLOBJS}:
	echo "int TMIP_$(patsubst ${OBJDIR}/tpl%.o,%,$@)[] = {0};" \
	> ${OBJDIR}/tpltmp.c
	${CC} -c ${CFLAGS} -o $@ ${OBJDIR}/tpltmp.c
	rm ${OBJDIR}/tpltmp.c
endif

all: ${SRCS} links bytecode_o.h ${OOBJS} ${ARCHIVE}

${OBJDIR}:
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

codemacros.h: bytecode.h macros.h
	touch codemacros.h

cinterface.h: runtime.h
	touch cinterface.h

node.h: runtime.h newmacros.h     # newmacros was formerly: codemacros.h
	touch node.h

clean:
	rm -f ${OBJDIR}/*.o ${OBJDIR}/*.a   #tables.s builtin.s

realclean: clean
	rm -f ${SRCS} ${HDRS} doc.lis
	
depend:
	${CC} -MM ${CFLAGS} ${SRCS} | sed -e "s#${INCDIR}#../../../include#"

bytecode.o: bytecode.c runtime.h
cdata.o: cdata.c ../../../include/mk.h ../../../cinterface.h \
  ../../../include/node.h ../../../include/runtime.h \
  ../../../include/newmacros.h ../../../include/bytecode.h \
  ../../../include/newbytecode.h ../../../include/mutlib.h \
  ../../../include/mkByHand.h ../../../include/mkByProg.h stableptr.h
collector.o: collector.c node.h runtime.h newmacros.h bytecode.h \
  newbytecode.h mark.h
ffiexport.o: ffiexport.c ../../../include/haskell2c.h \
  ../../../include/mk.h ../../../include/cinterface.h \
  ../../../include/node.h ../../../include/runtime.h \
  ../../../include/newmacros.h ../../../include/bytecode.h \
  ../../../include/newbytecode.h ../../../include/mutlib.h \
  ../../../include/mkByHand.h ../../../include/mkByProg.h \
  ../../../include/flt.h ../../../include/stableptr.h \
  ../../../include/twopass.h
main.o: main.c runtime.h stableptr.h
mark.o: mark.c node.h runtime.h newmacros.h bytecode.h newbytecode.h \
  mark.h mutlib.h
mutator.o: mutator.c mutlib.h runtime.h cinterface.h node.h newmacros.h \
  bytecode.h newbytecode.h
mutlib.o: mutlib.c runtime.h mutlib.h newmacros.h bytecode.h \
  newbytecode.h
newbuiltin.o: newbuiltin.c node.h runtime.h newmacros.h bytecode.h \
  newbytecode.h
newtables.o: newtables.c newmacros.h bytecode.h runtime.h newbytecode.h \
  mutlib.h
stableptr.o: stableptr.c runtime.h mark.h stableptr.h
system.o: system.c runtime.h
timeUnix.o: timeUnix.c runtime.h

bytecode_o.h: newbytecode.h
	$(LOCAL)mangler newbytecode.h bytecode_o.h

#all: prof0 $(OBJS)
#	#$(LD) $(LDFLAGS) -o Runtime0.o $(OBJS_O)
#	cp mutlib.o mutlib0.o
#	cp mutator.o mutator0.o
#	$(AR) $(ARFLAGS) Runtime0.a $(OBJS_A)
#
#all1: prof1 $(OBJS) $(POBJS)
#	#$(LD) $(LDFLAGS) -o Runtime1.o $(OBJS_O)
#	cp mutlib.o mutlib1.o
#	cp mutator.o mutator1.o
#	$(AR) $(ARFLAGS) Runtime1.a $(OBJS_A) $(POBJS)
#
#allt: prof0t $(OBJS)
#	#$(LD) $(LDFLAGS) -o Runtime0t.o $(OBJS_O)
#	cp mutlib.o mutlib0t.o
#	cp mutator.o mutator0t.o
#	$(AR) $(ARFLAGS) Runtime0t.a $(OBJS_A)
#
#all1t: prof1t $(OBJS) $(POBJS)
#	#$(LD) $(LDFLAGS) -o Runtime1t.o $(OBJS_O)
#	cp mutlib.o mutlib1t.o
#	cp mutator.o mutator1t.o
#	$(AR) $(ARFLAGS) Runtime1t.a $(OBJS_A) $(POBJS)

#prof0:
#	$(MAKE) clean
#	rm -f prof?
#	rm -f prof??
#	touch prof0
#
#prof1:
#	$(MAKE) clean
#	rm -f prof?
#	rm -f prof??
#	touch prof1
#
#prof0t:
#	$(MAKE) clean
#	rm -f prof?
#	rm -f prof??
#	touch prof0t
#
#prof1t:
#	$(MAKE) clean
#	rm -f prof?
#	rm -f prof??
#	touch prof1t

#tables.s: tables.c codemacros.h
#	$(CPP) $(OPT) tables.c > tables.s
#
#builtin.s: builtin.c codemacros.h
#	$(CPP) $(OPT) builtin.c > builtin.s
#
#tables.o: tables.s
#	$(AS) tables.s
#
#builtin.o: builtin.s
#	$(AS) builtin.s

