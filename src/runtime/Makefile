include Makefile.inc
#MACHINE = `uname -s`-`uname -r`
#LIBDIR = ../../lib
#INCDIR = ../../include


OBJDIR=${BUILDDIR}/${OBJ}/runtime

SUBDIRS = Kernel Mk Builtin Integer
OBJDIRS=$(patsubst %,${OBJDIR}/%,${SUBDIRS})

RUNTIME = Runtime

LD = ld
LDFLAGS = -r
TRUE = /bin/true
CC = gcc
AR = ar
ARFLAGS = r


all:  ofiles afile nhc98heap$(EXE)
install: ofiles afile
	${INSTALL} ${OBJDIR}/Kernel/$(RUNTIME)${CFG}.a ${DST}
	$(AR) $(ARFLAGS) ${DST}/$(RUNTIME)${CFG}.a ${OBJDIR}/Integer/*.o ${OBJDIR}/Builtin/*.o ${OBJDIR}/Mk/*.o
	ranlib ${DST}/$(RUNTIME)${CFG}.a
	${INSTALL} ${OBJDIR}/Kernel/mutator.o ${DST}/mutator${CFG}.o
	${INSTALL} ${OBJDIR}/Kernel/mutlib.o ${DST}/mutlib${CFG}.o
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
realclean: clean
	rm -f $(DST)/nhc98heap$(EXE) $(DST)/$(RUNTIME)$(CFG).a


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

