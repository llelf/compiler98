#ifndef _FILEFORMAT_H
#define _FILEFORMAT_H

#include "ident.h"

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
#define NTID		6
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


FileOffset	mkTAp1		(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset sr);
FileOffset	mkTAp2		(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset sr);
FileOffset	mkTAp3		(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset sr);
FileOffset	mkTAp4		(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset sr);
FileOffset	mkTAp5		(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset sr);

FileOffset	mkTNm		(FileOffset tnm, FileOffset nm, FileOffset sr);
FileOffset	mkTInd		(FileOffset t1, FileOffset t2);
FileOffset	mkTHidden	(FileOffset t1);
FileOffset	mkTSatA		(FileOffset t1);
FileOffset	mkTSatB		(FileOffset t1);
FileOffset	mkTSatC		(FileOffset t1);

FileOffset	mkNTInt		(int i);
FileOffset	mkNTChar	(char c);
FileOffset	mkNTInteger	(NodePtr i);
FileOffset	mkNTRational	(NodePtr i,NodePtr j);
FileOffset	mkNTFloat	(float f);
FileOffset	mkNTDouble	(double d);
FileOffset	mkNTId		(IdEntry *id);
FileOffset	mkNTConstr	(IdEntry *id);
FileOffset	mkNTTuple	(void);
FileOffset	mkNTFun		(void);
FileOffset	mkNTCase	(void);
FileOffset	mkNTLambda	(void);
FileOffset	mkNTDummy	(void);
FileOffset	mkNTCString	(char *s);
FileOffset	mkNTIf		(void);
FileOffset	mkNTGuard	(void);
FileOffset	mkNTContainer	(void);

FileOffset	mkSR0		(void);
FileOffset	mkSR3		(SrcRef *sr);

#endif
