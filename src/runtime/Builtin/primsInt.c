#include <math.h>
#include "HsFFI.h"

/* Int8 */
int     primEqInt8  (HsInt8 d1, HsInt8 d2)	{ return (d1==d2); }
int     primLeInt8  (HsInt8 d1, HsInt8 d2)	{ return (d1<=d2); }
HsInt8  primAddInt8 (HsInt8 d1, HsInt8 d2)	{ return (d1+d2); }
HsInt8  primMulInt8 (HsInt8 d1, HsInt8 d2)	{ return (d1*d2); }

HsInt8  primAbsInt8    (HsInt8 d)		{ return (d<0?-d:d); }
HsInt8  primSignumInt8 (HsInt8 d)		{ return (d<0?-1:(d==0?0:1)); }
HsInt8  primQuotInt8 (HsInt8 d1, HsInt8 d2)	{ return (d1/d2); }
HsInt8  primRemInt8  (HsInt8 d1, HsInt8 d2)	{ return (d1%d2); }


/* Int16 */
int     primEqInt16  (HsInt16 d1, HsInt16 d2)	{ return (d1==d2); }
int     primLeInt16  (HsInt16 d1, HsInt16 d2)	{ return (d1<=d2); }
HsInt16 primAddInt16 (HsInt16 d1, HsInt16 d2)	{ return (d1+d2); }
HsInt16 primMulInt16 (HsInt16 d1, HsInt16 d2)	{ return (d1*d2); }

HsInt16 primAbsInt16    (HsInt16 d)		{ return (d<0?-d:d); }
HsInt16 primSignumInt16 (HsInt16 d)		{ return (d<0?-1:(d==0?0:1)); }
HsInt16 primQuotInt16 (HsInt16 d1, HsInt16 d2)	{ return (d1/d2); }
HsInt16 primRemInt16  (HsInt16 d1, HsInt16 d2)	{ return (d1%d2); }


/* Int32 */
int     primEqInt32  (HsInt32 d1, HsInt32 d2)	{ return (d1==d2); }
int     primLeInt32  (HsInt32 d1, HsInt32 d2)	{ return (d1<=d2); }
HsInt32 primAddInt32 (HsInt32 d1, HsInt32 d2)	{ return (d1+d2); }
HsInt32 primMulInt32 (HsInt32 d1, HsInt32 d2)	{ return (d1*d2); }

HsInt32 primAbsInt32    (HsInt32 d)		{ return (d<0?-d:d); }
HsInt32 primSignumInt32 (HsInt32 d)		{ return (d<0?-1:(d==0?0:1)); }
HsInt32 primQuotInt32 (HsInt32 d1, HsInt32 d2)	{ return (d1/d2); }
HsInt32 primRemInt32  (HsInt32 d1, HsInt32 d2)	{ return (d1%d2); }


/* Int64 */
int     primEqInt64  (HsInt64 d1, HsInt64 d2)	{ return (d1==d2); }
int     primLeInt64  (HsInt64 d1, HsInt64 d2)	{ return (d1<=d2); }
HsInt64 primAddInt64 (HsInt64 d1, HsInt64 d2)	{ return (d1+d2); }
HsInt64 primMulInt64 (HsInt64 d1, HsInt64 d2)	{ return (d1*d2); }

HsInt64 primAbsInt64    (HsInt64 d)		{ return (d<0?-d:d); }
HsInt64 primSignumInt64 (HsInt64 d)		{ return (d<0?-1:(d==0?0:1)); }
HsInt64 primQuotInt64 (HsInt64 d1, HsInt64 d2)	{ return (d1/d2); }
HsInt64 primRemInt64  (HsInt64 d1, HsInt64 d2)	{ return (d1%d2); }



/* Integer conversions */
/* Note that Int64 is incorrectly downcast to Int32 here. */
extern int   primIntFromIntegerC (void* i);
extern void* primIntegerFromIntC (int   i);
#define FROM_INT(i)	primIntFromIntegerC(i)
#define TO_INT(i)	primIntegerFromIntC(i)
HsInt8	primInt8FromInteger  (void* i)		{ return (HsInt8)FROM_INT(i); }
HsInt16	primInt16FromInteger (void* i)		{ return (HsInt16)FROM_INT(i); }
HsInt32	primInt32FromInteger (void* i)		{ return (HsInt32)FROM_INT(i); }
HsInt64	primInt64FromInteger (void* i)		{ return (HsInt64)FROM_INT(i); }
void*	primInt8ToInteger  (HsInt8 i)		{ return TO_INT((int)i); }
void*	primInt16ToInteger (HsInt16 i)		{ return TO_INT((int)i); }
void*	primInt32ToInteger (HsInt32 i)		{ return TO_INT((int)i); }
void*	primInt64ToInteger (HsInt64 i)		{ return TO_INT((int)i); }

