include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/prelude
PRELUDE = Prelude
ARFLAGS = cr
ARCHIVE=${DST}/${PRELUDE}${CFG}.a

ifeq "${PROFILING}" "p"
  ifeq "${TPROF}" "z" #PHtprof
    O=p.z.o 
  else
    O=p.o
  endif
else
  ifeq "${TPROF}" "z"
    O=z.o 
  else
    O=o
  endif
endif

ifneq "${TRACING}" "T"

FSTSUBDIRS = Char Ratio Numeric
PRELSUBDIRS = $(MACHINE) LowB PreludeList PreludeIO PreludeText Prelude
BOOTLIBSUBDIRS = Ix NonStd List Array Maybe Monad System IO PackedString Debug
LIBSUBDIRS = ${BOOTLIBSUBDIRS} Complex Locale Time Directory CPUTime \
	     Random Haskell Binary Bit GreenCard BinArray FFI IOExtras Observe

BOOTSUBDIRS = ${FSTSUBDIRS} ${PRELSUBDIRS} ${BOOTLIBSUBDIRS}
SUBDIRS = ${FSTSUBDIRS} ${PRELSUBDIRS} ${LIBSUBDIRS}

SUBDIRTARGETS=$(patsubst %,%.make,${SUBDIRS})
SUBDIRBOOT=$(patsubst %,%.boot,${BOOTSUBDIRS})
SUBDIRCFILES=$(patsubst %,%.cfiles,${SUBDIRS})
SUBDIRFROMC=$(patsubst %,%.fromC,${SUBDIRS})
SUBDIRCLEAN=$(patsubst %,%.clean,${SUBDIRS})
SUBDIRCLEANC=$(patsubst %,%.cleanC,${SUBDIRS})
XXX=$(patsubst %,%.xxx,${SUBDIRS})

OBJDIRS = $(patsubst %,${OBJDIR}/%,${SUBDIRS} DErrNo)



all: ${ARCHIVE}
install: ${ARCHIVE}
lib:  $(patsubst %,%.make,${LIBSUBDIRS})
prel: $(patsubst %,%.make,${PRELSUBDIRS})
boot: ${OBJDIR} ${OBJDIRS} ${SUBDIRBOOT}
	cd $(OBJDIR); $(AR) $(ARFLAGS) ${ARCHIVE} */*.$O
	ranlib ${ARCHIVE}
objdir: ${OBJDIR} ${OBJDIRS}
cfiles: ${SUBDIRCFILES}
fromC: ${OBJDIR} ${OBJDIRS} ${SUBDIRFROMC}
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


ifeq "${TPROF}" ""

relink:
	cd $(OBJDIR); $(AR) $(ARFLAGS) ${ARCHIVE} */*.$O
	ranlib ${ARCHIVE}

${ARCHIVE}: ${OBJDIR} ${OBJDIRS} ${SUBDIRTARGETS}
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ */*.$O
	ranlib $@

else    #### PHtprof

#LowB partitions
TPLLOWBPR = Blackhole Ieee LowI PrimDecodeDouble PrimDecodeFloat \
            PrimDoubleFromInteger PrimEncodeDouble PrimEncodeFloat PrimError \
            PrimFloatFromInteger PrimGetArgs PrimIntFromInteger PrimIntegerAbs \
            PrimIntegerAdd PrimIntegerAnd PrimIntegerEq PrimIntegerFromInt \
            PrimIntegerGe PrimIntegerGt PrimIntegerLe PrimIntegerLt PrimIntegerMul \
            PrimIntegerNe PrimIntegerNeg PrimIntegerOr PrimIntegerQuotRem \
            PrimIntegerSub PrimOpenSocket
TPLLOWBPS = LowPS PrimComparePS PrimIndex PrimLength PrimPackString PrimUnpackPS
TPLLOWBSY = CExitWith LowSystem PrimExitWith
TPLLOWBCS = CString
TPLLOWBDR = _Driver
#Prelude
TPLABELS3 = Prelude 
${DST}/tpPrelude${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?Prelude.o \
	$(patsubst %,LowB/%.$O,${TPLLOWBPR}) \
	Prelude/*.$O PreludeIO/*.$O \
	PreludeList/*.$O PreludeText/*.$O 
#PackedString
TPLABELS3 += PackedString
${DST}/tpPackedString${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?PackedString.o \
	$(patsubst %,LowB/%.$O,${TPLLOWBPS}) PackedString/*.$O 
#System
TPLABELS3 += System
${DST}/tpSystem${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?System.o \
	$(patsubst %,LowB/%.$O,${TPLLOWBSY}) System/*.$O 
#CString
TPLABELS3 += CString
${DST}/tpCString${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?CString.o \
	$(patsubst %,LowB/%.$O,${TPLLOWBCS})
#_Driver
TPLABELS3 += Driver
${DST}/tpDriver${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?Driver.o \
	$(patsubst %,LowB/%.$O,${TPLLOWBDR})
#NonStd
TPLABELS3 += NonStdEval NonStdProfile NonStdTrace \
             NonStdUnsafeCoerce NonStdGetPID Warning 
${DST}/tpNonStdEval${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?NonStdEval.o \
	NonStd/NonStdEval.$O 
${DST}/tpNonStdProfile${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?NonStdProfile.o \
	NonStd/NonStdProfile.$O 
${DST}/tpNonStdTrace${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?NonStdTrace.o \
	NonStd/NonStdTrace.$O 
${DST}/tpNonStdUnsafeCoerce${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?NonStdUnsafeCoerce.o \
	NonStd/NonStdUnsafeCoerce.$O
${DST}/tpNonStdGetPID${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?NonStdGetPID.o \
	NonStd/NonStdGetPID.$O 
${DST}/tpWarning${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?Warning.o \
	NonStd/Warning.$O
#UnboxedArray
TPLABELS3 += UnboxedArray
${DST}/tpUnboxedArray${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?UnboxedArray.o \
	BinArray/UnboxedArray.$O BinArray/Binary_UBA.$O
#BinArray
TPLABELS3 += BinArray
${DST}/tpBinArray${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?BinArray.o \
	BinArray/BinArray.$O BinArray/AllocUBA.$O \
        BinArray/DUnboxedArray.$O BinArray/GetUBAEnd.$O \
        BinArray/GetUBAFree.$O BinArray/RUBA.$O \
        BinArray/WUBA.$O BinArray/cLowUnboxedArray.$O
#Debug
TPLABELS3 += DbgIface
${DST}/tpDbgIface${CFG}.a: 
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ tpl?DbgIface.o \
	Debug/*.$O

TPLARSDEF := $(patsubst %,${DST}/tp%${CFG}.a,${TPLABELS3})

#and finally the straightforward ones
TPLABELS1  = Bit Complex Directory GreenCard Monad Time
TPLTHEREST = Array Binary CPUTime Char DErrNo FFI Haskell IO \
             IOExtras Ix List Locale Maybe Numeric Observe Random Ratio
TPLABELS3 += ${TPLTHEREST}
TPLARSAUTO = $(patsubst %,${DST}/tp%${CFG}.a,${TPLABELS1} ${TPLTHEREST})
${TPLARSAUTO}:
	cd $(OBJDIR); $(AR) $(ARFLAGS) $@ \
	$(patsubst ${DST}/tp%${CFG}.a, ${OBJDIR}/tpl?%.o,$@) \
	$(patsubst ${DST}/tp%${CFG}.a, ${OBJDIR}/%/*.$O,$@)
	ranlib $@

#All the tpl?*.o are in ${BUILDDIR}/objt/prelude/
TPLABELS:= ${TPLABELS3} ${TPLABELS1}
TPLOBJS := $(patsubst %,${OBJDIR}/tpl1%.o,${TPLABELS}) \
	   $(patsubst %,${OBJDIR}/tpl2%.o,${TPLABELS3}) \
	   $(patsubst %,${OBJDIR}/tpl3%.o,${TPLABELS3}) 
${TPLOBJS}:
	echo "int TMIP_$(patsubst ${OBJDIR}/tpl%.o,%,$@)[] = {0};" \
	> ${OBJDIR}/tpltmp.c
	${CC} -c ${CFLAGS} -o $@ ${OBJDIR}/tpltmp.c
	rm ${OBJDIR}/tpltmp.c

#tprofprel?.o
TPPREL1 := $(patsubst %,${OBJDIR}/tprofprel%.o,1)
TPPREL3 := $(patsubst %,${OBJDIR}/tprofprel%.o,2 3)
TPPRELS := ${TPPREL1} ${TPPREL3}
${TPPREL1}: #All lables
	$(LOCAL)tprofprel 1 \
	${TPLABELS} > ${OBJDIR}/tpltmp.c
	${CC} -c ${CFLAGS} -o $@ ${OBJDIR}/tpltmp.c
	rm ${OBJDIR}/tpltmp.c
${TPPREL3}: #Only those lables that are needed more than once
	$(LOCAL)tprofprel $(patsubst ${OBJDIR}/tprofprel%.o,%,$@) \
	${TPLABELS3} > ${OBJDIR}/tpltmp.c
	${CC} -c ${CFLAGS} -o $@ ${OBJDIR}/tpltmp.c
	rm ${OBJDIR}/tpltmp.c

#ARCHIVE
${ARCHIVE}: ${OBJDIR} ${OBJDIRS} ${SUBDIRTARGETS} \
	${TPLOBJS} ${TPPRELS} ${TPLARSDEF} ${TPLARSAUTO}
	${INSTALL} ${OBJDIR}/tprofprel?.o ${DST}/
relink: ${TPLOBJS} ${TPPRELS} ${TPLARSDEF} ${TPLARSAUTO}
	${INSTALL} ${OBJDIR}/tprofprel?.o ${DST}/
endif

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
	PreludeList PreludeIO PreludeText PreludeDebug Prelude \
	Maybe List Monad System IO PackedString \
	Ix Array Complex GreenCard Directory CPUTime Random \
	FFI IOExtras
NOTYET=	NonStd \
	Locale Time Haskell \
	Binary Bit BinArray Observe

SUBDIRSMK=$(patsubst %,%.subdir,${SUBDIRS})
SUBDIRSINSTALL=$(patsubst %,%.install,${SUBDIRS})
OBJDIRS = $(patsubst %,${OBJDIR}/%,${SUBDIRS} DErrNo)
SUBDIRCFILES=$(patsubst %,%.cfiles,${SUBDIRS})
SUBDIRFROMC=$(patsubst %,%.fromC,${SUBDIRS})
SUBDIRCLEANC=$(patsubst %,%.cleanC,${SUBDIRS})
CLEANDIRS=$(patsubst %,%.clean,${SUBDIRS})
MKFILES=$(patsubst %,%.mkfiles,${SUBDIRS})
HICLEAN=$(patsubst %,%.hiclean,${SUBDIRS})


all: ${ARCHIVE}
install: ${ARCHIVE}
cfiles: ${SUBDIRCFILES}
fromC: ${OBJDIR} ${OBJDIRS} ${SUBDIRFROMC}
	cd $(OBJDIR); $(AR) $(ARFLAGS) $(ARCHIVE) */*.o
	ranlib $(ARCHIVE)
hiclean: ${HICLEAN}
clean: ${CLEANDIRS}
realclean: clean
	rm -f ${ARCHIVE}
mkfiles: ${MKFILES}
objdir: ${OBJDIR}
cleanhi:
	-rm -f */*.T.hi
cleanC: ${SUBDIRCLEANC}


%.mkfiles:
	rm $(patsubst %.mkfiles,%,$@)/obj
%.cfiles: 
	cd $(patsubst %.cfiles,%,$@); ${MAKE} cfiles
%.fromC: 
	cd $(patsubst %.fromC,%,$@); $(MAKE) fromC
%.clean:
	cd $(patsubst %.clean,%,$@); ${MAKE} clean
%.cleanC: 
	cd $(patsubst %.cleanC,%,$@); ${MAKE} cleanC
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
	cd $(OBJDIR); $(AR) $(ARFLAGS) ${ARCHIVE} */*.T.o
	ranlib $@

relink:
	cd $(OBJDIR); $(AR) $(ARFLAGS) ${ARCHIVE} */*.T.o
	ranlib ${ARCHIVE}

endif
