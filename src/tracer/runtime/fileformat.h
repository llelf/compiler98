#ifndef _FILEFORMAT_H
#define _FILEFORMAT_H

#include "ident.h"
#include "runtime.h"

extern FILE *HatFile;

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

typedef struct {
    int constr;
#ifdef PROFILE
    int profInfo[EXTRA];
#endif
    FileOffset ptr;
    int trust;
} *CNmType;


FileOffset	primTAp1	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset sr);
FileOffset	primTAp2	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset sr);
FileOffset	primTAp3	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset sr);
FileOffset	primTAp4	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset sr);
FileOffset	primTAp5	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset sr);

FileOffset	primTNm		(FileOffset tnm, CNmType nm, FileOffset sr);
FileOffset	primTInd	(FileOffset t1, FileOffset t2);
FileOffset	primTHidden	(FileOffset t1);
FileOffset	primTSatA	(FileOffset t1);
FileOffset	primTSatB	(FileOffset t1);
FileOffset	primTSatC	(FileOffset t1, FileOffset t2);

CNmType		primNTInt	(int i);
CNmType		primNTChar	(char c);
CNmType		primNTInteger	(NodePtr i);
CNmType		primNTRational	(NodePtr i,NodePtr j);
CNmType		primNTFloat	(float f);
CNmType		primNTDouble	(double d);
CNmType		primNTId	(IdEntry *id);	/* believed not necessary */
CNmType		primNTConstr	(IdEntry *id);	/* believed not necessary */
CNmType		primNTTuple	(void);
CNmType		primNTFun	(void);
CNmType		primNTCase	(void);
CNmType		primNTLambda	(void);
CNmType		primNTDummy	(void);
CNmType		primNTCString	(char *s);
CNmType		primNTIf	(void);
CNmType		primNTGuard	(void);
CNmType		primNTContainer	(void);

int		primTrustedFun	(CNmType nm);

FileOffset	primSR0		(void);
FileOffset	primSR3		(SrcRef *sr);


#ifndef False
#define False	0
#endif
#ifndef True
#define True	1
#endif

#endif
