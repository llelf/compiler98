include Makefile.inc

ifeq "T" "${TRACING}"
DBGFLAG=-DDBGTRANS
endif

OBJDIR = ${BUILDDIR}/obj/compiler98
TARGET = ${DST}/nhc98comp${TRACING}${EXE}

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
	Import.hs ImportState.hs Info.hs IntState.hs IdKind.hs Lex.hs LexLow.hs \
	LexPre.hs LexStr.hs Lexical.hs Lift.hs Machine.hs Main.hs Memo.hs \
	MergeSort.hs MkSyntax.hs NT.hs Need.hs NeedLib.hs Nice.hs OsOnly.hs \
	PrettyLib.hs PrettySyntax.hs Parse.hs Parse2.hs ParseCore.hs ParseI.hs \
	ParseLex.hs ParseLib.hs PosAtom.hs PosCode.hs PreImp.hs PreImport.hs \
	Prim.hs PrimCode.hs Reduce.hs Remove1_3.hs Rename.hs RenameLib.hs \
	RmClasses.hs STGArity.hs STGBuild.hs STGGcode.hs STGState.hs Scc.hs \
	SccModule.hs State.hs StrPos.hs Syntax.hs SyntaxPos.hs \
	SyntaxUtil.hs TokenId.hs TokenInt.hs Tree234.hs Type.hs TypeCtx.hs \
	TypeData.hs TypeEnv.hs TypeLib.hs TypeSubst.hs TypeUnify.hs \
	TypeUtil.hs Unlit.hs Foreign.hs IsPrefixOf.hs Floats.hs FFITrans.hs
#	DbgReplaceConstr.hs DbgReplaceListInsts.hs DbgReplacePrelude.hs
GCSRCS = NhcFloats.gc
CFILES = $(patsubst %.hs,%.c,${SRCS})
GCCFILES = $(patsubst %.gc,%.c,${GCSRCS}) 
OBJS = $(patsubst %.hs,$(OBJDIR)/%.o,${SRCS})
GCOBJS = $(patsubst %.gc,$(OBJDIR)/%.o,${GCSRCS}) $(patsubst %.gc,$(OBJDIR)/%_.o,${GCSRCS})

CPPFILES=FixSyntax STGBuild STGGcode	#GcodeLowC
DBGOBJS=$(patsubst %,$(OBJDIR)/%.o,${CPPFILES})

HC = nhc98

ifeq "hbc" "${HC}"
HMAKEFLAGS := -H32M
endif
ifeq "nhc98" "${HC}"
HMAKEFLAGS := -H8M +CTS -H12M -CTS
endif
ifeq "ghc" "${HC}"
IMPROVE     = #-O
EXTRALINK   = $(OBJDIR)/ghc_floats.o
HMAKEFLAGS := $(shell $(LOCAL)fixghc $(GHCSYM) +CTS -fglasgow-exts -syslib exts -syslib misc -CTS)  $(IMPROVE) $(EXTRALINK)
$(TARGET): $(EXTRALINK)
endif

HMAKEFLAGS += $(shell echo $(BUILDOPTS)) $(DBGFLAG)


all: ${TARGET}
install: ${TARGET}
objdir: ${OBJDIR}
cfiles:
	$(LOCAL)hmake -nhc98 -C Main.hs
fromC: $(OBJDIR)
	$(LOCAL)/nhc98 -c -d $(OBJDIR) *.c
	cd $(OBJDIR); $(LOCAL)/nhc98 -H8M -o $(TARGET) *.o
	strip $(TARGET)
relink:
	cd $(OBJDIR); $(LOCAL)/nhc98 -H8M -o $(TARGET) *.o
	strip $(TARGET)
clean:
	rm -f $(OBJDIR)/*.o *.o *.hi
realclean: clean
	rm -f *.c $(TARGET)

$(TARGET): ${OBJDIR} $(OBJDIR)/$(HC) $(OBJDIR)/last${TRACING} $(SRCS)
	$(LOCAL)hmake -$(HC) $(HMAKEFLAGS) -d $(OBJDIR) Main
	mv $(OBJDIR)/Main$(EXE) $(TARGET)
	strip $(TARGET)
${OBJDIR}:
	mkdir -p ${OBJDIR}
$(OBJDIR)/$(HC):
	rm -f $(OBJDIR)/nhc98 $(OBJDIR)/hbc $(OBJDIR)/ghc
	$(MAKE) clean
	touch "$(OBJDIR)/$(HC)"
$(OBJDIR)/last${TRACING}:
	rm -f $(OBJDIR)/last $(OBJDIR)/lastT
	rm -f $(DBGOBJS)
	touch $(OBJDIR)/last${TRACING}
 
$(OBJDIR)/ghc_floats.o: ghc_floats.c
	$(CC) -c -o $@ $<
ghc_floats.c: ghc_floats.c.inst
	cp ghc_floats.c.inst ghc_floats.c


.SUFFIXES: .hs .o .c .gc

${CFILES}: %.c : %.hs
	$(HC) -C $(HFLAGS) $<
${GCCFILES}: %.c : %.gc
	$(HC) -C $(HFLAGS) $<

