#ifndef _HAT_H
#define _HAT_H

typedef unsigned long FileOffset;

/* Upper 3 bits of tag byte */
#define Trace		0
#define ModuleInfo	1
#define NmType		2
#define SR		3

/* Trace: lower 5 bits of tag byte */
#define TAp	0
#define TNm	1
#define TInd	2
#define THidden	3
#define TSatA	4
#define TSatB	5
#define TSatC	6
#define TSatAL  12
#define TSatBL  13
#define TSatCL  14

/* NmType: lower 5 bits of tag byte */
#define NTInt		0
#define NTChar		1
#define NTInteger	2
#define NTRational	3
#define NTFloat		4
#define NTDouble	5
#define NTId		6
#define NTConstr	7
#define NTTuple		8
#define NTFun		9
#define NTCase		10
#define NTLambda	11
#define NTDummy		12
#define NTCString	13
#define NTIf		14
#define NTGuard		15
#define NTContainer	16
#define NTTrusted	22

#ifndef False
#define False	0
#endif
#ifndef True
#define True	1
#endif

#endif
