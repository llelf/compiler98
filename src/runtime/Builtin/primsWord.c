#include <math.h>
#include "HsFFI.h"

/* Word8 */
int     primEqWord8  (HsWord8 d1, HsWord8 d2)		{ return (d1==d2); }
int     primLeWord8  (HsWord8 d1, HsWord8 d2)		{ return (d1<=d2); }
HsWord8 primAddWord8 (HsWord8 d1, HsWord8 d2)		{ return (d1+d2); }
HsWord8 primMulWord8 (HsWord8 d1, HsWord8 d2)		{ return (d1*d2); }

HsWord8 primSignumWord8 (HsWord8 d)			{ return (d==0?0:1); }
HsWord8 primQuotWord8 (HsWord8 d1, HsWord8 d2)		{ return (d1/d2); }
HsWord8 primRemWord8  (HsWord8 d1, HsWord8 d2)		{ return (d1%d2); }


/* Word16 */
int      primEqWord16  (HsWord16 d1, HsWord16 d2)	{ return (d1==d2); }
int      primLeWord16  (HsWord16 d1, HsWord16 d2)	{ return (d1<=d2); }
HsWord16 primAddWord16 (HsWord16 d1, HsWord16 d2)	{ return (d1+d2); }
HsWord16 primMulWord16 (HsWord16 d1, HsWord16 d2)	{ return (d1*d2); }

HsWord16 primSignumWord16 (HsWord16 d)			{ return (d==0?0:1); }
HsWord16 primQuotWord16 (HsWord16 d1, HsWord16 d2)	{ return (d1/d2); }
HsWord16 primRemWord16  (HsWord16 d1, HsWord16 d2)	{ return (d1%d2); }


/* Word32 */
int      primEqWord32  (HsWord32 d1, HsWord32 d2)	{ return (d1==d2); }
int      primLeWord32  (HsWord32 d1, HsWord32 d2)	{ return (d1<=d2); }
HsWord32 primAddWord32 (HsWord32 d1, HsWord32 d2)	{ return (d1+d2); }
HsWord32 primMulWord32 (HsWord32 d1, HsWord32 d2)	{ return (d1*d2); }

HsWord32 primSignumWord32 (HsWord32 d)			{ return (d==0?0:1); }
HsWord32 primQuotWord32 (HsWord32 d1, HsWord32 d2)	{ return (d1/d2); }
HsWord32 primRemWord32  (HsWord32 d1, HsWord32 d2)	{ return (d1%d2); }


/* Word64 */
int      primEqWord64  (HsWord64 d1, HsWord64 d2)	{ return (d1==d2); }
int      primLeWord64  (HsWord64 d1, HsWord64 d2)	{ return (d1<=d2); }
HsWord64 primAddWord64 (HsWord64 d1, HsWord64 d2)	{ return (d1+d2); }
HsWord64 primMulWord64 (HsWord64 d1, HsWord64 d2)	{ return (d1*d2); }

HsWord64 primSignumWord64 (HsWord64 d)			{ return (d==0?0:1); }
HsWord64 primQuotWord64 (HsWord64 d1, HsWord64 d2)	{ return (d1/d2); }
HsWord64 primRemWord64  (HsWord64 d1, HsWord64 d2)	{ return (d1%d2); }


/* Integer conversions */
/* Note that Word64 is incorrectly downcast to Word32 here */
extern int   primIntFromIntegerC (void* i);
extern void* primIntegerFromIntC (int   i);
#define FR_INT(i)     primIntFromIntegerC(i)
#define TO_INT(i)       primIntegerFromIntC(i)
HsWord8   primWord8FromInteger  (void* i)	{ return (HsWord8)FR_INT(i); }
HsWord16  primWord16FromInteger (void* i)	{ return (HsWord16)FR_INT(i); }
HsWord32  primWord32FromInteger (void* i)	{ return (HsWord32)FR_INT(i); }
HsWord64  primWord64FromInteger (void* i)	{ return (HsWord64)FR_INT(i); }
void*   primWord8ToInteger  (HsWord8 i)		{ return TO_INT((int)i); }
void*   primWord16ToInteger (HsWord16 i)	{ return TO_INT((int)i); }
void*   primWord32ToInteger (HsWord32 i)	{ return TO_INT((int)i); }
void*   primWord64ToInteger (HsWord64 i)	{ return TO_INT((int)i); }

