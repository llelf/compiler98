include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/Maybe

VPATH = .

.SUFFIXES: .hi .hs .o

SRCS	= IsJust.hs
NOTDONE = \
	FromMaybe.hs    MapMaybe.hs     \
	CatMaybes.hs	MaybeToList.hs  \
	FromJust.hs     ListToMaybe.hs  Unfoldr.hs      

MAINSRC = Maybe.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})

MAINIFC=$(patsubst %.hs,%.hi,${MAINSRC})

all: ${MAINOBJ}

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -part -o $@ $<

${MAINOBJ}: ${OBJS} ${MAINSRC}
	$(DBG_COMPILE) +CTS -lib -CTS -o $@ ${MAINSRC}

install:

install_ifc:
	cp ${MAINIFC} ${IFCDEST}

clean:
	rm -f ${MAINOBJ} ${OBJS}
