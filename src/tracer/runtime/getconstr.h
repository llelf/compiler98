#ifndef GETCONSTR_H
#define GETCONSTR_H

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
/* NTTrusted == NTId + 16 */
#define NTTrusted 	22


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

extern Node D_Prelude_46_91_93[];
extern Node D_Prelude_46_58[];
extern Node D_Prelude_46False[];
extern Node D_Prelude_46True[];
extern Node D_Prelude_46_91_93[];
extern Node D_Prelude_46Right[];

#define mkNmInt(x) mkNmWithArg(NTInt, x)
#define mkNmBool(x) (CONINFO_NUMBER(*x) == 0 ? (&D_Prelude_46False[0]) : (&D_Prelude_46True[0]))
#define mkNmChar(x) mkNmWithArg(NTChar, x)
#if 0
#define mkNmFun(x) mkNmWithArg(NTFun, x)
#endif
#define mkNmCons() (&D_Prelude_46_58[0])
#define mkNmNil() (&D_Prelude_46_58[0])
#define mkNmUnit() (&D_Prelude_46_91_93[0])
#define mkNmRight() (&D_Prelude_46Right[0])
#define mkNmTuple() mkNm(NTTuple)

NodePtr mkNmWithArg(int tag, NodePtr x);
NodePtr mkNm(int tag);
NodePtr mkR(NodePtr v, NodePtr t);
NodePtr mkTAp(NodePtr t, NodePtr ts, NodePtr sr);
NodePtr mkTNm(NodePtr t, NodePtr nm, NodePtr sr);

extern Node C0_Prelude_46SR[];
#define mkSR() ((NodePtr)C0_Prelude_46SR)

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
