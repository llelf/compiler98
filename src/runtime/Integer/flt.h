
float sencode(MP_INT *man, int iexp);
int sdecode(float f,MP_INT *man);
double dencode(MP_INT *man, int iexp);
int ddecode(double f,MP_INT *man);


#if defined(sequent) || defined(sun) || defined(mips) || defined(hp300) || defined(_IBMR2) || defined(linux) || defined(__386BSD__) || defined(__alpha) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(bsdi) || defined(__CYGWIN32__)
|| defined(__OpenBSD__)

#define DBL_RADIX 2
#define DBL_DIGITS 53
#define DBL_MINEXP (-1021)
#define DBL_MAXEXP 1024

#define FLT_RADIX 2
#define FLT_DIGITS 24
#define FLT_MINEXP (-125)
#define FLT_MAXEXP 128
#endif
