include Makefile.inc

THISLIB	= Locale
SEARCH	=

SRCS	= DTimeLocale.hs       DefaultTimeLocale.hs
SRCS_T	=


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.

${OBJDIR}/DTimeLocale.o: DTimeLocale.hs
${OBJDIR}/DefaultTimeLocale.o: DefaultTimeLocale.hs ${OBJDIR}/DTimeLocale.o

DefaultTimeLocale.c: DTimeLocale.c


