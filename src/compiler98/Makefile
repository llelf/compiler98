include Makefile.inc

OBJDIR = ${BUILDDIR}/obj/compiler98
TARGET = ${DST}/nhc98comp${EXE}

SRCS = \
	AssocTree.hs Bind.hs Case.hs CaseHelp.hs CaseLib.hs CaseOpt.hs \
	Depend.hs Derive.hs DeriveBinary.hs \
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


HC = nhc98	# can be overridden by caller with 'make HC=...'

ifeq "hbc" "$(findstring hbc, ${HC})"
HMAKEFLAGS := -H32M
endif
ifeq "nhc98" "$(findstring nhc98, ${HC})"
HMAKEFLAGS := -H16M -K2M +CTS -H16M -CTS
endif
ifeq "ghc" "$(findstring ghc, ${HC})"
IMPROVE     = #-O
EXTRALINK   = $(OBJDIR)/ghc_floats.o
HMAKEFLAGS := $(shell $(LOCAL)fixghc $(GHCSYM) -package lang -syslib misc )  $(IMPROVE) $(EXTRALINK)
$(TARGET): $(EXTRALINK)
endif

HMAKEFLAGS += $(shell echo $(BUILDOPTS))


all: ${TARGET}
install: ${TARGET}
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
	rm -f $(OBJDIR)/*.$O *.$O *.hi
cleanC: clean
	rm -f *.hc *.p.c *.z.c
realclean: cleanC
	rm -f $(TARGET)

$(TARGET): ${OBJDIR} $(OBJDIR)/$(HC) $(SRCS)
	$(HMAKE) -hc=$(HC) $(HMAKEFLAGS) -d $(OBJDIR) MainNew
	mv $(OBJDIR)/MainNew$(EXE) $(TARGET)
	strip $(TARGET)
${OBJDIR}:
	mkdir -p ${OBJDIR}
$(OBJDIR)/$(HC):
	rm -f $(OBJDIR)/nhc98 $(OBJDIR)/hbc $(OBJDIR)/ghc*
	$(MAKE) cleanC
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

