include Makefile.inc

OBJDIR=${BUILDDIR}/${OBJ}/runtime

SUBDIRS = Kernel Mk Builtin Integer
OBJDIRS=$(patsubst %,${OBJDIR}/%,${SUBDIRS})

RUNTIME = Runtime

ifeq "${CFG}" ""
  SUF=
else
  SUF=.${CFG}
endif

TRUE = /bin/true
LDFLAGS = -r
ARFLAGS = r


all:  ofiles afile nhc98heap$(EXE)
install: ofiles afile
	${INSTALL} ${OBJDIR}/Kernel/$(RUNTIME)$(SUF).a ${DST}
	$(AR) $(ARFLAGS) ${DST}/$(RUNTIME)$(SUF).a ${OBJDIR}/Integer/*.o ${OBJDIR}/Builtin/*.o ${OBJDIR}/Mk/*.o
	ranlib ${DST}/$(RUNTIME)$(SUF).a
	${INSTALL} ${OBJDIR}/Kernel/mutator.o ${DST}/mutator$(SUF).o
	${INSTALL} ${OBJDIR}/Kernel/mutlib.o ${DST}/mutlib$(SUF).o
	${INSTALL} ${OBJDIR}/Kernel/main.o ${DST}/main$(SUF).o
ofiles: ${OBJDIR} ${OBJDIRS}
	cd Kernel; $(MAKE) all
afile: ${OBJDIR} ${OBJDIRS}
	cd Integer; $(MAKE) all
	cd Builtin; $(MAKE) all
	cd Mk;      $(MAKE) all
nhc98heap$(EXE): $(DST)/nhc98heap$(EXE) ;
clean:
	cd Kernel;  $(MAKE) clean
	cd Integer; $(MAKE) clean
	cd Builtin; $(MAKE) clean
	cd Mk;      $(MAKE) clean
nolinks:
	cd Kernel;  $(MAKE) nolinks
	cd Integer; $(MAKE) nolinks
	cd Builtin; $(MAKE) nolinks
	cd Mk;      $(MAKE) nolinks
realclean: clean
	rm -f $(DST)/nhc98heap$(EXE) $(DST)/$(RUNTIME)$(SUF).a


${OBJDIR}:
	mkdir -p ${OBJDIR}

${OBJDIRS}: ${OBJDIR}/% :
	mkdir -p $@  || /bin/true

objdir: ${OBJDIR}
	cd Kernel;  $(MAKE) objdir
	cd Integer; $(MAKE) objdir
	cd Builtin; $(MAKE) objdir
	cd Mk;      $(MAKE) objdir

$(DST)/nhc98heap$(EXE): nhc98heap.c
	$(CC) -o $@ $<
	strip $@

