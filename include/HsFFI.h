#ifndef HSFFI_H
#define HSFFI_H

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

#endif
