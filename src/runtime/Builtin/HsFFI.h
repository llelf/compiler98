#ifndef _HSFFI_H
#define _HSFFI_H

#include <inttypes.h>

#define HsChar		char
#define HsInt		int
#define HsFloat		float
#define HsDouble	double
#define HsBool		int
#define HsAddr		void*
#define HsForeignObj	void*
#define HsStablePtr	void*

#define HsInt8		int8_t
#define HsInt16		int16_t
#define HsInt32		int32_t
#define HsInt64		int64_t
#define HsWord8		uint8_t
#define HsWord16	uint16_t
#define HsWord32	uint32_t
#define HsWord64	uint64_t

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
