include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/prelude
PRELUDE = Prelude
ARFLAGS = cr
ARCHIVE=${DST}/${PRELUDE}${CFG}.a

ifneq "${TRACING}" "T"

FSTSUBDIRS = Char Ratio Numeric
PRELSUBDIRS = $(MACHINE) LowB PreludeList PreludeIO PreludeText Prelude
BOOTLIBSUBDIRS = Ix NonStd List Array Maybe Monad System IO PackedString Debug
LIBSUBDIRS = ${BOOTLIBSUBDIRS} Complex Locale Time Directory Interrupt CPUTime Random Haskell Binary Bit GreenCard BinArray FFI IOExtras

BOOTSUBDIRS = ${FSTSUBDIRS} ${PRELSUBDIRS} ${BOOTLIBSUBDIRS}
SUBDIRS = ${FSTSUBDIRS} ${PRELSUBDIRS} ${LIBSUBDIRS}

SUBDIRTARGETS=$(patsubst %,%.make,${SUBDIRS})
SUBDIRBOOT=$(patsubst %,%.boot,${BOOTSUBDIRS})
SUBDIRCFILES=$(patsubst %,%.cfiles,${SUBDIRS})
SUBDIRFROMC=$(patsubst %,%.fromC,${SUBDIRS})
SUBDIRCLEAN=$(patsubst %,%.clean,${SUBDIRS})
SUBDIRCLEANC=$(patsubst %,%.cleanC,${SUBDIRS})
XXX=$(patsubst %,%.xxx,${SUBDIRS})

SUBDIROBJS = $(patsubst %,${OBJDIR}/%/*.o,${SUBDIRS})
BOOTOBJS = $(patsubst %,${OBJDIR}/%/*.o,${BOOTSUBDIRS})
OBJDIRS = $(patsubst %,${OBJDIR}/%,${SUBDIRS} DErrNo)



all: ${ARCHIVE}
install: ${ARCHIVE}
lib:  $(patsubst %,%.make,${LIBSUBDIRS})
prel: $(patsubst %,%.make,${PRELSUBDIRS})
boot: ${OBJDIR} ${OBJDIRS} ${SUBDIRBOOT}
	cd $(OBJDIR); $(AR) $(ARFLAGS) ${ARCHIVE} */*.o  #BOOTOBJS
	ranlib ${ARCHIVE}
objdir: ${OBJDIR} ${OBJDIRS}
cfiles: ${SUBDIRCFILES}
fromC: ${OBJDIR} ${OBJDIRS} ${SUBDIRFROMC}
	cd $(OBJDIR); $(AR) $(ARFLAGS) $(ARCHIVE) */*.o  #SUBDIROBJS
	ranlib $(ARCHIVE)
clean: ${SUBDIRCLEAN}
realclean: clean
	rm $(ARCHIVE)
cleanhi:
	-rm -f */*.hi
cleanC: ${SUBDIRCLEANC}
 

%.make: 
	cd $(patsubst %.make,%,$@); ${MAKE} all
%.boot: 
	cd $(patsubst %.boot,%,$@); ${MAKE} boot
%.cfiles: 
	cd $(patsubst %.cfiles,%,$@); ${MAKE} cfiles
%.fromC: 
	cd $(patsubst %.fromC,%,$@); $(MAKE) fromC
%.clean: 
	cd $(patsubst %.clean,%,$@); ${MAKE} clean
%.cleanC: 
	cd $(patsubst %.cleanC,%,$@); ${MAKE} cleanC
%.xxx: 
	cd $(patsubst %.xxx,%,$@); cp ../../Makefile.inc .


${OBJDIR}:
	mkdir -p ${OBJDIR}

${OBJDIRS}: ${OBJDIR}/% :
	mkdir -p $@ || /bin/true

${ARCHIVE}: ${OBJDIR} ${OBJDIRS} ${SUBDIRTARGETS}
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ */*.o  #SUBDIROBJS
	ranlib $@
relink:
	cd $(OBJDIR); $(AR) $(ARFLAGS) ${ARCHIVE} */*.o  #SUBDIROBJS
	ranlib ${ARCHIVE}



#newinterface: prelude lib
#	mv -f $(INCDIR)/Prelude.hi $(INCDIR)/Prelude.hi.BAK
#	cp Prelude/Prelude.hi $(INCDIR)/Prelude.hi
#	mv -f $(INCDIR)/Char.hi $(INCDIR)/Char.hi.BAK
#	cp Char/Char.hi $(INCDIR)/Char.hi
#	mv -f $(INCDIR)/Ix.hi $(INCDIR)/Ix.hi.BAK
#	cp Ix/Ix.hi $(INCDIR)/Ix.hi
#	mv -f $(INCDIR)/Ratio.hi $(INCDIR)/Ratio.hi.BAK
#	cp Ratio/Ratio.hi $(INCDIR)/Ratio.hi
#	mv -f $(INCDIR)/Text.hi $(INCDIR)/Text.hi.BAK
#	cp Text/Text.hi $(INCDIR)/Text.hi
#	mv -f $(INCDIR)/List.hi $(INCDIR)/List.hi.BAK
#	cp List/List.hi $(INCDIR)/List.hi
#	mv -f $(INCDIR)/Array.hi $(INCDIR)/Array.hi.BAK
#	cp Array/Array.hi $(INCDIR)/Array.hi
#	mv -f $(INCDIR)/Complex.hi $(INCDIR)/Complex.hi.BAK
#	cp Complex/Complex.hi $(INCDIR)/Complex.hi
#	mv -f $(INCDIR)/Monad.hi $(INCDIR)/Monad.hi.BAK
#	cp Monad/Monad.hi $(INCDIR)/Monad.hi
#	mv -f $(INCDIR)/Maybe.hi $(INCDIR)/Maybe.hi.BAK
#	cp Maybe/Maybe.hi $(INCDIR)/Maybe.hi
#	mv -f $(INCDIR)/System.hi $(INCDIR)/System.hi.BAK
#	cp System/System.hi $(INCDIR)/System.hi
#	mv -f $(INCDIR)/Directory.hi $(INCDIR)/Directory.hi.BAK
#	cp Directory/Directory.hi $(INCDIR)/Directory.hi
#	mv -f $(INCDIR)/Interrupt.hi $(INCDIR)/Interrupt.hi.BAK
#	cp Interrupt/Interrupt.hi $(INCDIR)/Interrupt.hi
#	mv -f $(INCDIR)/IO.hi $(INCDIR)/IO.hi.BAK
#	cp IO/IO.hi $(INCDIR)/IO.hi
#	mv -f $(INCDIR)/PackedString.hi $(INCDIR)/PackedString.hi.BAK
#	cp PackedString/PackedString.hi $(INCDIR)/PackedString.hi
#	mv -f $(INCDIR)/NonStdProfile.hi $(INCDIR)/NonStdProfile.hi.BAK
#	cp NonStd/NonStdProfile.hi $(INCDIR)/NonStdProfile.hi
#	mv -f $(INCDIR)/NonStdTrace.hi $(INCDIR)/NonStdTrace.hi.BAK
#	cp NonStd/NonStdTrace.hi $(INCDIR)/NonStdTrace.hi
#	#mv -f $(INCDIR)/DbgIface.hi $(INCDIR)/DbgIface.hi.BAK
#	#cp Debug/DbgIface.hi $(INCDIR)/DbgIface.hi
#	mv -f $(INCDIR)/Bit.hi $(INCDIR)/Bit.hi.BAK		# MW
#	cp Bit/Bit.hi $(INCDIR)/Bit.hi				# MW
#	mv -f $(INCDIR)/Binary.hi $(INCDIR)/Binary.hi.BAK	# MW
#	cp Binary/Binary.hi $(INCDIR)/Binary.hi			# MW
#	mv -f $(INCDIR)/BinArray.hi $(INCDIR)/BinArray.hi.BAK	# MW
#	cp BinArray/BinArray.hi $(INCDIR)/BinArray.hi		# MW
#	mv -f $(INCDIR)/GreenCard.hi $(INCDIR)/GreenCard.hi.BAK	# MW
#	cp GreenCard/GreenCard.hi $(INCDIR)/GreenCard.hi	# MW

else	#### tracing


# .SUFFIXES: .subdir
SUBDIRS= Char Ratio Numeric $(MACHINE) LowT \
	PreludeDebug PreludeList PreludeIO PreludeText Prelude \
	Maybe List System IO PackedString
NOTYET= Array Complex Directory Interrupt Ix 

#SUBDIROBJS=$(patsubst %,${OBJ}/%/*.o,${SUBDIRS})
SUBDIRSMK=$(patsubst %,%.subdir,${SUBDIRS})
SUBDIRSINSTALL=$(patsubst %,%.install,${SUBDIRS})
OBJDIRS = $(patsubst %,${OBJDIR}/%,${SUBDIRS} DErrNo)
CLEANDIRS=$(patsubst %,%.clean,${SUBDIRS})
MKFILES=$(patsubst %,%.mkfiles,${SUBDIRS})
HICLEAN=$(patsubst %,%.hiclean,${SUBDIRS})


all: ${ARCHIVE}
install: ${ARCHIVE}
hiclean: ${HICLEAN}
clean: ${CLEANDIRS}
realclean: clean
	rm -f ${ARCHIVE}
mkfiles: ${MKFILES}
objdir: ${OBJDIR}


%.mkfiles:
	rm $(patsubst %.mkfiles,%,$@)/obj
%.clean:
	cd $(patsubst %.clean,%,$@); ${MAKE} clean
%.hiclean:
	cd $(patsubst %.hiclean,%,$@); ${MAKE} hiclean
%.subdir:
	cd $(patsubst %.subdir,%,$@); ${MAKE} all
%.install:
	cd $(patsubst %.install,%,$@); ${MAKE} install


${OBJDIR}:
	mkdir -p ${OBJDIR} || /bin/true
${OBJDIRS}: ${OBJDIR}/% :
	mkdir -p $@ || /bin/true

${ARCHIVE}: ${OBJDIR} ${OBJDIRS} ${SUBDIRSMK}
	rm -f $@
	cd $(OBJDIR); $(AR) $(ARFLAGS) ${ARCHIVE} */*.o
	ranlib $@


endif
