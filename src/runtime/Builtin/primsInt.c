#include <math.h>

#define Int8  signed char
#define Int16 signed short
#define Int32 signed long
#define Int64 signed long long


/* Int8 */
int  primEqInt8  (Int8 d1, Int8 d2)		{ return (d1==d2); }
int  primLeInt8  (Int8 d1, Int8 d2)		{ return (d1<=d2); }
Int8 primAddInt8 (Int8 d1, Int8 d2)		{ return (d1+d2); }
Int8 primMulInt8 (Int8 d1, Int8 d2)		{ return (d1*d2); }

Int8 primAbsInt8    (Int8 d)			{ return (d<0?-d:d); }
Int8 primSignumInt8 (Int8 d)			{ return (d<0?-1:(d==0?0:1)); }
Int8 primQuotInt8 (Int8 d1, Int8 d2)		{ return (d1/d2); }
Int8 primRemInt8  (Int8 d1, Int8 d2)		{ return (d1%d2); }


/* Int16 */
int   primEqInt16  (Int16 d1, Int16 d2)		{ return (d1==d2); }
int   primLeInt16  (Int16 d1, Int16 d2)		{ return (d1<=d2); }
Int16 primAddInt16 (Int16 d1, Int16 d2)		{ return (d1+d2); }
Int16 primMulInt16 (Int16 d1, Int16 d2)		{ return (d1*d2); }

Int16 primAbsInt16    (Int16 d)			{ return (d<0?-d:d); }
Int16 primSignumInt16 (Int16 d)			{ return (d<0?-1:(d==0?0:1)); }
Int16 primQuotInt16 (Int16 d1, Int16 d2)	{ return (d1/d2); }
Int16 primRemInt16  (Int16 d1, Int16 d2)	{ return (d1%d2); }


/* Int32 */
int   primEqInt32  (Int32 d1, Int32 d2)		{ return (d1==d2); }
int   primLeInt32  (Int32 d1, Int32 d2)		{ return (d1<=d2); }
Int32 primAddInt32 (Int32 d1, Int32 d2)		{ return (d1+d2); }
Int32 primMulInt32 (Int32 d1, Int32 d2)		{ return (d1*d2); }

Int32 primAbsInt32    (Int32 d)			{ return (d<0?-d:d); }
Int32 primSignumInt32 (Int32 d)			{ return (d<0?-1:(d==0?0:1)); }
Int32 primQuotInt32 (Int32 d1, Int32 d2)	{ return (d1/d2); }
Int32 primRemInt32  (Int32 d1, Int32 d2)	{ return (d1%d2); }


/* Int64 */
int   primEqInt64  (Int64 d1, Int64 d2)		{ return (d1==d2); }
int   primLeInt64  (Int64 d1, Int64 d2)		{ return (d1<=d2); }
Int64 primAddInt64 (Int64 d1, Int64 d2)		{ return (d1+d2); }
Int64 primMulInt64 (Int64 d1, Int64 d2)		{ return (d1*d2); }

Int64 primAbsInt64    (Int64 d)			{ return (d<0?-d:d); }
Int64 primSignumInt64 (Int64 d)			{ return (d<0?-1:(d==0?0:1)); }
Int64 primQuotInt64 (Int64 d1, Int64 d2)	{ return (d1/d2); }
Int64 primRemInt64  (Int64 d1, Int64 d2)	{ return (d1%d2); }

