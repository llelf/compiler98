include Makefile.inc

#ifeq "T" "${TRACING}"
#DBGFLAG=-DDBGTRANS
#endif

OBJDIR = ${BUILDDIR}/obj/compiler98
TARGET = ${DST}/nhc98comp${EXE}
HATTRANS = ${DST}/hat-trans${EXE}

SRCS = \
	AssocTree.hs Bind.hs Case.hs CaseHelp.hs CaseLib.hs CaseOpt.hs \
	DbgDataTrans.hs DbgDumpSRIDTable.hs DbgDumpSRIDTableC.hs DbgId.hs \
	DbgTrans.hs Depend.hs Derive.hs DeriveBinary.hs \
	DeriveBounded.hs DeriveEnum.hs DeriveEq.hs \
	DeriveEval.hs DeriveIx.hs DeriveLib.hs DeriveOrd.hs DeriveRead.hs \
	DeriveShow.hs EmitState.hs Error.hs Export.hs Extra.hs Extract.hs \
	FSLib.hs FixSyntax.hs Fixity.hs Flags.hs FreeVar.hs Gcode.hs \
	GcodeFix.hs GcodeLow.hs GcodeLowC.hs GcodeMem.hs GcodeOpt1.hs \
	GcodeOpt2.hs GcodeRel.hs GcodeSpec.hs HbcOnly.hs IExtract.hs \
	Import.hs ImportState.hs Info.hs IntState.hs IdKind.hs Lex.hs LexLow.hs\
	LexPre.hs LexStr.hs Lexical.hs Lift.hs Machine.hs Memo.hs \
	MergeSort.hs MkSyntax.hs NT.hs Need.hs NeedLib.hs Nice.hs OsOnly.hs \
	PrettyLib.hs PrettySyntax.hs Parse.hs Parse2.hs ParseCore.hs ParseI.hs \
	ParseLex.hs ParseLib.hs PosAtom.hs PosCode.hs PreImp.hs PreImport.hs \
	Prim.hs PrimCode.hs Reduce.hs Remove1_3.hs Rename.hs RenameLib.hs \
	RmClasses.hs STGArity.hs STGBuild.hs STGGcode.hs STGState.hs Scc.hs \
	SccModule.hs State.hs StrPos.hs Syntax.hs SyntaxPos.hs \
	SyntaxUtil.hs TokenId.hs TokenInt.hs Tree234.hs Type.hs TypeCtx.hs \
	TypeData.hs TypeEnv.hs TypeLib.hs TypeSubst.hs TypeUnify.hs \
	TypeUtil.hs Unlit.hs Foreign.hs IsPrefixOf.hs Floats.hs FFITrans.hs \
	ReportImports.hs 
GCSRCS = NhcFloats.gc
CFILES = $(patsubst %.hs, %.$C, ${SRCS})
GCCFILES = $(patsubst %.gc, %.$C, ${GCSRCS}) 
OBJS = $(patsubst %.hs, $(OBJDIR)/%.$O, ${SRCS})
GCOBJS = $(patsubst %.gc, $(OBJDIR)/%.$O, ${GCSRCS}) $(patsubst %.gc,$(OBJDIR)/%_.$O,${GCSRCS})

#CPPFILES=FixSyntax STGBuild STGGcode	#GcodeLowC
#DBGOBJS=$(patsubst %, $(OBJDIR)/%.$O, ${CPPFILES})
HATSRCS = TraceDerive.hs TraceTrans.hs SyntaxUtil.hs AuxLabelAST.hs \
	AuxFixity.hs Overlap.hs DbgId.hs TokenInt.hs NeedLib.hs IExtract.hs \
	State.hs ImportState.hs PreImp.hs ParseI.hs Memo.hs Import.hs \
	AuxFile.hs ParseLex.hs ParseLib.hs Parse2.hs SyntaxPos.hs \
	MkSyntax.hs Parse.hs LexStr.hs LexLow.hs LexPre.hs Lexical.hs \
	Unlit.hs Lex.hs AuxTypes.hs TraceId.hs Nice.hs Reduce.hs \
	MergeSort.hs Info.hs IdKind.hs NT.hs Tree234.hs AssocTree.hs \
	IntState.hs PrettyLib.hs PrettySyntax.hs OsOnly.hs Flags.hs \
	ParseCore.hs Id.hs TokenId.hs HbcOnly.hs Extra.hs Syntax.hs \
	Error.hs HatTrans.hs

HC = nhc98	# can be overridden by caller with 'make HC=...'

ifeq "hbc" "${HC}"
HMAKEFLAGS := -H32M
endif
ifeq "nhc98" "${HC}"
HMAKEFLAGS := -H16M -K2M +CTS -H16M -CTS
endif
ifeq "ghc" "${HC}"
IMPROVE     = #-O
EXTRALINK   = $(OBJDIR)/ghc_floats.o
HMAKEFLAGS := $(shell $(LOCAL)fixghc $(GHCSYM) -package lang -syslib misc )  $(IMPROVE) $(EXTRALINK)
$(TARGET): $(EXTRALINK)
$(HATTRANS): $(EXTRALINK)
endif

HMAKEFLAGS += $(shell echo $(BUILDOPTS)) # $(DBGFLAG)


all: ${TARGET} #${HATTRANS}
hat-trans: ${HATTRANS}
install: ${TARGET} #${HATTRANS}
objdir: ${OBJDIR}
cfiles: $(OBJDIR)/$(HC)
	$(HMAKE) -HC=$(LOCAL)nhc98 -K2M -C MainNew.hs
fromC: $(OBJDIR)
	$(LOCAL)/nhc98 -c -d $(OBJDIR) *.$C
	cd $(OBJDIR); $(LOCAL)nhc98 -H8M -o $(TARGET) *.$O
	strip $(TARGET)
relink:
	cd $(OBJDIR); $(LOCAL)nhc98 -H8M -o $(TARGET) *.$O
	strip $(TARGET)
clean:
	rm -f $(OBJDIR)/*.$O *.$O *.hi *.hc
realclean: clean
	rm -f *.hc *.c $(TARGET)

$(TARGET): ${OBJDIR} $(OBJDIR)/$(HC) $(SRCS)
	$(HMAKE) -$(HC) $(HMAKEFLAGS) -d $(OBJDIR) MainNew
	mv $(OBJDIR)/MainNew$(EXE) $(TARGET)
	strip $(TARGET)
$(HATTRANS): ${OBJDIR} $(OBJDIR)/$(HC) $(HATSRCS)
	$(HMAKE) -$(HC) $(HMAKEFLAGS) -d $(OBJDIR) HatTrans
	mv $(OBJDIR)/HatTrans$(EXE) $(HATTRANS)
	strip $(HATTRANS)
${OBJDIR}:
	mkdir -p ${OBJDIR}
$(OBJDIR)/$(HC):
	rm -f $(OBJDIR)/nhc98 $(OBJDIR)/hbc $(OBJDIR)/ghc
	$(MAKE) clean
	touch "$(OBJDIR)/$(HC)"
 
$(OBJDIR)/ghc_floats.o: ghc_floats.c
	$(CC) -c -o $@ $<
ghc_floats.c: ghc_floats.c.inst
	cp ghc_floats.c.inst ghc_floats.c


.SUFFIXES: .hs .o .c .gc

${CFILES}: %.$C : %.hs
	$(HC) -C $(HFLAGS) $<
${GCCFILES}: %.$C : %.gc
	$(HC) -C $(HFLAGS) $<

