#ifndef _HSFFI_H
#define _HSFFI_H

/* We currently assume a 32-bit architecture. */

#define HsChar		char
#define HsInt		int
#define HsFloat		float
#define HsDouble	double
#define HsBool		int
#define HsAddr		void*
#define HsForeignObj	void*
#define HsStablePtr	void*

#define HsInt8		signed char
#define HsInt16		signed short
#define HsInt32		signed long
#define HsInt64		signed long long
#define HsWord8		unsigned char
#define HsWord16	unsigned short
#define HsWord32	unsigned long
#define HsWord64	unsigned long long

#define HS_CHAR_MIN	0
#define HS_CHAR_MAX	0xff
#define HS_INT_MIN	HS_INT32_MIN
#define HS_INT_MAX	HS_INT32_MAX

#define HS_INT8_MIN	(-127-1)
#define HS_INT8_MAX	127
#define HS_INT16_MIN	(-32767-1)
#define HS_INT16_MAX	32767
#define HS_INT32_MIN	(-2147483647-1)
#define HS_INT32_MAX	2147483647
#define HS_INT64_MIN	(-9223372036854775807-1)
#define HS_INT64_MAX	9223372036854775807

#define HS_WORD8_MIN	0
#define HS_WORD8_MAX	0xff
#define HS_WORD16_MIN	0
#define HS_WORD16_MAX	0xffff
#define HS_WORD32_MIN	0
#define HS_WORD32_MAX	(HsWord32)0xffffffff
#define HS_WORD64_MIN	0
#define HS_WORD64_MAX	(HsWord64)0xffffffffffffffff

#endif
