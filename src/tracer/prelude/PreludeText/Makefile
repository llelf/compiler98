include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/PreludeText

VPATH = .

.SUFFIXES: .hi .hs .o

SRCS= CShow.hs ShowChar.hs ShowParen.hs ShowString.hs \
	  Show_2.hs Show_3.hs Show_4.hs Show_Bool.hs Show_Char.hs \
	  Show_Int.hs Show_List.hs Shows.hs TShowS.hs \
	  Show_Double.hs Show_Maybe.hs Show_Float.hs Show_Integer.hs \
	  Show_Either.hs 

#Show_IOError.hs ????

NOTDONE= CRead.hs Lex.hs LexDigit.hs LexLitChar.hs Nonnull.hs Read.hs \
	 ReadParen.hs Read_2.hs Read_3.hs Read_4.hs Read_Bool.hs Read_Char.hs \
	 Read_Double.hs Read_Either.hs Read_Float.hs Read_Int.hs \
	 Read_Integer.hs Read_List.hs Read_Ordering.hs Read_Maybe.hs \
	 Read_Unit.hs ShowType.hs \
	 Show_Fun.hs Show_IO.hs Show_Ordering.hs\
	 Show_Unit.hs TReadS.hs _ReadCon.hs _ReadCon0.hs _ReadConArg.hs \
	 _ReadConInfix.hs

MAINSRC= PreludeText.hs
MAINOBJ=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})


OBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SRCS})

$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -P../Text/ -o $@ $<

all: ${MAINOBJ}

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

${MAINOBJ}: ${OBJS} ${MAINSRC}
	$(DBG_COMPILE) -o $@ ${MAINSRC}

install:

clean:
	rm -f ${OBJS} ${MAINOBJ}
