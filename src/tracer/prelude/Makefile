include Makefile.inc

ifeq "${TRACING}" "T"

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude

.SUFFIXES: .subdir

SUBDIRS= Maybe List PreludeIO System LowB Char IO Text PreludeText PreludeDebug PreludeList Prelude Ratio PackedString

NOTYET= Array Complex Directory Interrupt Ix 

#SUBDIROBJS=$(patsubst %,${OBJ}/%/*.o,${SUBDIRS})
SUBDIRSMK=$(patsubst %,%.subdir,${SUBDIRS})
SUBDIRSINSTALL=$(patsubst %,%.install,${SUBDIRS})
#OBJDIRS=$(patsubst %,%.objdir,${SUBDIRS})
OBJDIRS = $(patsubst %,${OBJDIR}/%,${SUBDIRS})
CLEANDIRS=$(patsubst %,%.clean,${SUBDIRS})
MKFILES=$(patsubst %,%.mkfiles,${SUBDIRS})

ARCHIVE = ${DST}/Prelude${CFG}.a

all: ${ARCHIVE}
install: ${ARCHIVE}
clean: ${CLEANDIRS}
realclean: clean
	rm -f ${ARCHIVE}
mkfiles: ${MKFILES}
objdir: ${OBJDIR}


%.mkfiles:
	rm $(patsubst %.mkfiles,%,$@)/obj
%.objdir:
	mkdir -p ${OBJDIR}/$(patsubst %.objdir,%,$@) || /bin/true
%.clean:
	cd $(patsubst %.clean,%,$@); ${MAKE} clean
%.subdir:
	cd $(patsubst %.subdir,%,$@); ${MAKE} all
%.install:
	cd $(patsubst %.install,%,$@); ${MAKE} install


${OBJDIRS}: ${OBJDIR}/% : %
	mkdir -p $@ || /bin/true
${OBJDIR}:
	mkdir -p ${OBJDIR} || /bin/true

${ARCHIVE}: ${OBJDIR} ${OBJDIRS} ${SUBDIRSMK}
	rm -f $@
	ar cr $@ ${OBJDIR}/*/*.o
	ranlib $@


else
all install objdir clean:
	@echo "No trace prelude created when not building for tracing"
endif
