#include <math.h>

char primSignumInt8 (char d)		{ return (d<0?-1:(d==0?0:1)); }
char primAbsInt8 (char d)		{ return (d<0?-d:d); }
char primAddInt8 (char d1, char d2)	{ return (d1+d2); }
char primSubInt8 (char d1, char d2)	{ return (d1-d2); }
char primMulInt8 (char d1, char d2)	{ return (d1*d2); }
char primDivInt8 (char d1, char d2)	{ return (d1/d2); }

int  primEqInt8 (char d1, char d2)	{ return (d1==d2); }
int  primLeInt8 (char d1, char d2)	{ return (d1<=d2); }

char primQuotInt8 (char d1, char d2)	{ return (d1/d2); }
char primRemInt8  (char d1, char d2)	{ return (d1%d2); }
