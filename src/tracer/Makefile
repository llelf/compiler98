include Makefile.inc

ifeq "${TRACING}" "T"

SUBDIRS = runtime ui

SUBDIRTARGETS=$(patsubst %,%.make,${SUBDIRS})
SUBDIRCLEAN=$(patsubst %,%.clean,${SUBDIRS})
SUBDIROBJDIR=$(patsubst %,%.objdir,${SUBDIRS})
SUBDIRINSTALL=$(patsubst %,%.install,${SUBDIRS})

%.make: 
	cd $(patsubst %.make,%,$@); ${MAKE} all

%.clean: 
	cd $(patsubst %.clean,%,$@); ${MAKE} clean

%.objdir: 
	cd $(patsubst %.objdir,%,$@); ${MAKE} objdir

%.install: 
	cd $(patsubst %.install,%,$@); ${MAKE} install

all: ${SUBDIRTARGETS}
clean: ${SUBDIRCLEAN}
objdir: ${SUBDIROBJDIR}
install: ${SUBDIRINSTALL}

else
all install objdir clean:
	@echo "Not building for tracing"
endif
