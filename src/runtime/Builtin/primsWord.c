#include <math.h>

#define Word8  unsigned char
#define Word16 unsigned short
#define Word32 unsigned long
#define Word64 unsigned long long

/* Word8 */
int   primEqWord8  (Word8 d1, Word8 d2)		{ return (d1==d2); }
int   primLeWord8  (Word8 d1, Word8 d2)		{ return (d1<=d2); }
Word8 primAddWord8 (Word8 d1, Word8 d2)		{ return (d1+d2); }
Word8 primMulWord8 (Word8 d1, Word8 d2)		{ return (d1*d2); }

Word8 primAbsWord8    (Word8 d)			{ return (d<0?-d:d); }
Word8 primSignumWord8 (Word8 d)			{ return (d<0?-1:(d==0?0:1)); }
Word8 primQuotWord8 (Word8 d1, Word8 d2)	{ return (d1/d2); }
Word8 primRemWord8  (Word8 d1, Word8 d2)	{ return (d1%d2); }


/* Word16 */
int    primEqWord16  (Word16 d1, Word16 d2)	{ return (d1==d2); }
int    primLeWord16  (Word16 d1, Word16 d2)	{ return (d1<=d2); }
Word16 primAddWord16 (Word16 d1, Word16 d2)	{ return (d1+d2); }
Word16 primMulWord16 (Word16 d1, Word16 d2)	{ return (d1*d2); }

Word16 primAbsWord16    (Word16 d)		{ return (d<0?-d:d); }
Word16 primSignumWord16 (Word16 d)		{ return (d<0?-1:(d==0?0:1)); }
Word16 primQuotWord16 (Word16 d1, Word16 d2)	{ return (d1/d2); }
Word16 primRemWord16  (Word16 d1, Word16 d2)	{ return (d1%d2); }


/* Word32 */
int    primEqWord32  (Word32 d1, Word32 d2)	{ return (d1==d2); }
int    primLeWord32  (Word32 d1, Word32 d2)	{ return (d1<=d2); }
Word32 primAddWord32 (Word32 d1, Word32 d2)	{ return (d1+d2); }
Word32 primMulWord32 (Word32 d1, Word32 d2)	{ return (d1*d2); }

Word32 primAbsWord32    (Word32 d)		{ return (d<0?-d:d); }
Word32 primSignumWord32 (Word32 d)		{ return (d<0?-1:(d==0?0:1)); }
Word32 primQuotWord32 (Word32 d1, Word32 d2)	{ return (d1/d2); }
Word32 primRemWord32  (Word32 d1, Word32 d2)	{ return (d1%d2); }


/* Word64 */
int    primEqWord64  (Word64 d1, Word64 d2)	{ return (d1==d2); }
int    primLeWord64  (Word64 d1, Word64 d2)	{ return (d1<=d2); }
Word64 primAddWord64 (Word64 d1, Word64 d2)	{ return (d1+d2); }
Word64 primMulWord64 (Word64 d1, Word64 d2)	{ return (d1*d2); }

Word64 primAbsWord64    (Word64 d)		{ return (d<0?-d:d); }
Word64 primSignumWord64 (Word64 d)		{ return (d<0?-1:(d==0?0:1)); }
Word64 primQuotWord64 (Word64 d1, Word64 d2)	{ return (d1/d2); }
Word64 primRemWord64  (Word64 d1, Word64 d2)	{ return (d1%d2); }

