#ifndef GETCONSTR_H
#define GETCONSTR_H

#include "fileformat.h"

#if 0
/* Don't change anything here!!! (See runtimeB/Kernel/builtins.c) */
#define NTInt		0
#define NTChar		1
#define NTInteger	2
#define NTRational	3
#define NTFloat		4
#define NTDouble	5
#define NTId		6
#define NTConstr	7
#define NTTuple		8
#define	NTFun		9
#define NTCase		10
#define NTLambda	11
#define NTDummy		12
#define NTCString	13
#define NTIf		14
#define NTGuard		15
#define NTContainer	16
/* NTTrusted == NTId + 16 */
#define NTTrusted 	22

#endif

#if 0
typedef enum {NTInt		/* Int */,
	      NTChar		/* Char */,
	      NTInteger 	/* Integer */,
	      NTRational	/* Rational */,
	      NTFloat		/* Float */,
	      NTDouble		/* Double */,
	      NTId		/* Int */,
	      NTConstr		/* Int */,
	      NTTuple,
	      NTFun,
	      NTCase,
	      NTLambda,
	      NTDummy} NTTags;
#endif

extern Node D_Prelude_46_58[];
extern Node D_Prelude_46_91_93[];
extern Node D_Prelude_46False[];
extern Node D_Prelude_46True[];
extern Node D__40_41[];
extern Node D_Prelude_46Right[];

#define mkNmInt(x)	primNTInt(GET_INT_VALUE(x))
#define mkNmBool(x)	primNTId((IdEntry*)(CONINFO_NUMBER(*x) == 0 ? (&D_Prelude_46False[0]) : (&D_Prelude_46True[0])))
#define mkNmChar(x)	primNTChar(GET_CHAR_VALUE(x))
#if 0
#define mkNmFun(x)	mkNmWithArg(NTFun, x)
#endif
#define mkNmCons()	primNTId((IdEntry*)&D_Prelude_46_58[0])
#define mkNmNil()	primNTId((IdEntry*)&D_Prelude_46_91_93[0])
#define mkNmUnit()	primNTId((IdEntry*)&D__40_41[0])
#define mkNmRight()	primNTId((IdEntry*)&D_Prelude_46Right[0])
#define mkNmTuple()	primNTTuple()
#define mkNmVector()	primNTContainer()
#define mkNmCString(x)	primNTCString(x)

CNmType mkNmWithArg(int tag, NodePtr x);
CNmType mkNm(int tag);
NodePtr mkR(NodePtr v, NodePtr t);
NodePtr mkTAp1(NodePtr t, NodePtr tfn, NodePtr ta1, NodePtr sr);
NodePtr mkTAp2(NodePtr t, NodePtr tfn, NodePtr ta1, NodePtr ta2, NodePtr sr);
NodePtr mkTNm(NodePtr t, CNmType nm, NodePtr sr);
NodePtr mkTInd(NodePtr t1, NodePtr t2);

NodePtr shortCircuitSelectors(NodePtr node);
NodePtr mkhString(char *s);
NodePtr mkRString(NodePtr sr, NodePtr t, NodePtr str);
char   *profName(UInt *p);
extern void prGraph(NodePtr nodeptr,Int flags,Int d);

/* extern Node C0_Prelude_46SR[]; */
/* #define mkSR() ((NodePtr)C0_Prelude_46SR) */
extern Node noSR[];
#define mkSR() ((NodePtr)noSR)

#define TagAp		0
#define TagNm		1
#define TagInd		2
#define TagRoot		3
#define TagSat		4
#define TagPruned	5
#define TagHidden	6

#define TagExitSuccess 0
#define TagExitFailure 1

#define EVALUATED	0
#define EVALUATING	1
#define CLOSURE		2

#endif
