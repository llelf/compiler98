#ifndef _FILEFORMAT_H
#define _FILEFORMAT_H

#include "ident.h"
#include "runtime.h"
#include "hat.h"

extern FILE *HatFile;
extern FILE *HatOutput;
extern FILE *HatBridge;

typedef struct {
    int constr;
#ifdef PROFILE
    int profInfo[EXTRA];
#endif
    FileOffset ptr;
    int trust;
} CNmType;

typedef struct {
    int constr;
#ifdef PROFILE
    int profInfo[EXTRA];
#endif
    FileOffset ptr;
    int trust;
    int hidden;
} CTrace;

FileOffset	primModInfo	(ModInfo *m);

FileOffset	primTRoot	(void);
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

FileOffset	primTAp6	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset targ6
                                               , FileOffset sr);

FileOffset	primTAp7	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset targ6
                                               , FileOffset targ7
                                               , FileOffset sr);

FileOffset	primTAp8	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset targ6
                                               , FileOffset targ7
                                               , FileOffset targ8
                                               , FileOffset sr);

FileOffset	primTAp9	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset targ6
                                               , FileOffset targ7
                                               , FileOffset targ8
                                               , FileOffset targ9
                                               , FileOffset sr);

FileOffset	primTAp10	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset targ6
                                               , FileOffset targ7
                                               , FileOffset targ8
                                               , FileOffset targ9
                                               , FileOffset targ10
                                               , FileOffset sr);

FileOffset	primTAp11	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset targ6
                                               , FileOffset targ7
                                               , FileOffset targ8
                                               , FileOffset targ9
                                               , FileOffset targ10
                                               , FileOffset targ11
                                               , FileOffset sr);

FileOffset	primTAp12	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset targ6
                                               , FileOffset targ7
                                               , FileOffset targ8
                                               , FileOffset targ9
                                               , FileOffset targ10
                                               , FileOffset targ11
                                               , FileOffset targ12
                                               , FileOffset sr);

FileOffset	primTNm		(FileOffset tnm, CNmType* nm, FileOffset sr);
FileOffset	primTInd	(FileOffset t1, FileOffset t2);
FileOffset	primTHidden	(FileOffset t1);
FileOffset	primTSatA	(FileOffset t1);
FileOffset	primTSatB	(FileOffset t1);
FileOffset	primTSatC	(FileOffset t1, FileOffset t2);

void		updateSatBs	(void);
void		updateSatCs	(void);

CNmType*	primNTInt	(int i);
CNmType*	primNTChar	(char c);
CNmType*	primNTInteger	(NodePtr i);
CNmType*	primNTRational	(NodePtr i,NodePtr j);
CNmType*	primNTFloat	(float f);
CNmType*	primNTDouble	(double d);
CNmType*	primNTId	(IdEntry *id);
CNmType*	primNTConstr	(IdEntry *id);
CNmType*	primNTTuple	(void);
CNmType*	primNTFun	(void);
CNmType*	primNTCase	(void);
CNmType*	primNTLambda	(void);
CNmType*	primNTDummy	(void);
CNmType*	primNTCString	(char *s);
CNmType*	primNTIf	(void);
CNmType*	primNTGuard	(void);
CNmType*	primNTContainer	(void);

int		primTrustedNm	(CNmType* nm);
int		primSameTrace	(FileOffset t1, FileOffset t2);

FileOffset	primTracePtr	(CTrace* t);
int		primTrustedFun	(CTrace* t);
int		primHidden	(CTrace* t);
CTrace*		mkTrace		(FileOffset p, int tr, int hid);

FileOffset	primSR0		(void);
FileOffset	primSR3		(SrcRef *sr);


#endif
