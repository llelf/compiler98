include Makefile.inc

THISLIB	= PreludeText
SEARCH	= -P../PreludeIO -P../Numeric -P../Char
EXTRA_H_FLAGS	= -prelude
EXTRA_C_FLAGS	=

SRCS	= \
	TReadS.hs        Lex.hs           ReadParen.hs  \
	CRead.hs         Read_2.hs        Reads.hs         Show_Fun.hs 	\
	CShow.hs         Read_3.hs        Show_IO.hs 	\
	Read_4.hs        ShowChar.hs      Show_Int.hs 	   ShowType.hs  \
	                 Read_Bool.hs     ShowParen.hs     Show_Integer.hs \
	Read_Char.hs     ShowString.hs    Show_List.hs 	\
	Read_Double.hs   Show_2.hs        Show_Maybe.hs 	\
	Read_Either.hs   Show_3.hs        Show_Unit.hs 	\
	Read.hs          Read_Float.hs    Show_4.hs        Shows.hs 	\
	_ReadCon.hs      Read_Int.hs      Show_Bool.hs               	\
	_ReadCon0.hs     Read_Integer.hs  Show_Char.hs     TShowS.hs 	\
	_ReadConArg.hs   Read_List.hs     Show_Double.hs   		\
	_ReadConInfix.hs Read_Maybe.hs    Show_Either.hs   		\
	                 Read_Unit.hs     Show_Float.hs    \
	Read_Ordering.hs Show_Ordering.hs
SRCS_T	= \
	TReadS.hs        Lex.hs           ReadParen.hs  \
	CRead.hs         Reads.hs         Read_2.hs 	\
	CShow.hs         Read_3.hs        Show_IO.hs 	\
	Read_4.hs        ShowChar.hs      Show_Int.hs 	   ShowType.hs  \
	                 Read_Bool.hs     ShowParen.hs     Show_Integer.hs \
	Read_Char.hs     ShowString.hs    Show_List.hs 	\
	Read_Double.hs   Show_2.hs        Show_Maybe.hs 	\
	Read_Either.hs   Show_3.hs        Show_Unit.hs 	\
	Read.hs          Read_Float.hs    Show_4.hs        Shows.hs 	\
	_ReadCon.hs      Read_Int.hs      Show_Bool.hs               	\
	_ReadCon0.hs     Read_Integer.hs  Show_Char.hs     TShowS.hs 	\
	_ReadConArg.hs   Read_List.hs     Show_Double.hs   		\
	_ReadConInfix.hs Read_Maybe.hs    Show_Either.hs   		\
	                 Read_Unit.hs     Show_Float.hs    \
	Read_Ordering.hs Show_Ordering.hs
NOTDONE= Show_Fun.hs


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.
${OBJDIR}/Show_IO.o: Show_IO.hs ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/_ReadCon0.o: _ReadCon0.hs ${OBJDIR}/_ReadCon.o

${OBJDIR}/../PreludeIO/DIO.o:
	cd ../PreludeIO; ${MAKE} ${OBJDIR}/../PreludeIO/DIO.o

