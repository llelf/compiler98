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


void		initialiseSATstack	(void);

FileOffset	primModInfo	(ModInfo *m);

CTrace*		primTRoot	(void);
CTrace*		primTAp1	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , FileOffset sr);
CTrace*		primTAp2	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , FileOffset sr);
CTrace*		primTAp3	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , FileOffset sr);
CTrace*		primTAp4	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , FileOffset sr);

CTrace*		primTAp5	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , FileOffset sr);

CTrace*		primTAp6	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , CTrace* targ6
                                               , FileOffset sr);

CTrace*		primTAp7	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , CTrace* targ6
                                               , CTrace* targ7
                                               , FileOffset sr);

CTrace*		primTAp8	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , CTrace* targ6
                                               , CTrace* targ7
                                               , CTrace* targ8
                                               , FileOffset sr);

CTrace*		primTAp9	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , CTrace* targ6
                                               , CTrace* targ7
                                               , CTrace* targ8
                                               , CTrace* targ9
                                               , FileOffset sr);

CTrace*		primTAp10	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , CTrace* targ6
                                               , CTrace* targ7
                                               , CTrace* targ8
                                               , CTrace* targ9
                                               , CTrace* targ10
                                               , FileOffset sr);

CTrace*		primTAp11	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , CTrace* targ6
                                               , CTrace* targ7
                                               , CTrace* targ8
                                               , CTrace* targ9
                                               , CTrace* targ10
                                               , CTrace* targ11
                                               , FileOffset sr);

CTrace*		primTAp12	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , CTrace* targ6
                                               , CTrace* targ7
                                               , CTrace* targ8
                                               , CTrace* targ9
                                               , CTrace* targ10
                                               , CTrace* targ11
                                               , CTrace* targ12
                                               , FileOffset sr);

CTrace*		primTAp13	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , CTrace* targ6
                                               , CTrace* targ7
                                               , CTrace* targ8
                                               , CTrace* targ9
                                               , CTrace* targ10
                                               , CTrace* targ11
                                               , CTrace* targ12
                                               , CTrace* targ13
                                               , FileOffset sr);

CTrace*		primTAp14	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , CTrace* targ6
                                               , CTrace* targ7
                                               , CTrace* targ8
                                               , CTrace* targ9
                                               , CTrace* targ10
                                               , CTrace* targ11
                                               , CTrace* targ12
                                               , CTrace* targ13
                                               , CTrace* targ14
                                               , FileOffset sr);

CTrace*		primTAp15	(CTrace* tap, CTrace* tfn
                                               , CTrace* targ1
                                               , CTrace* targ2
                                               , CTrace* targ3
                                               , CTrace* targ4
                                               , CTrace* targ5
                                               , CTrace* targ6
                                               , CTrace* targ7
                                               , CTrace* targ8
                                               , CTrace* targ9
                                               , CTrace* targ10
                                               , CTrace* targ11
                                               , CTrace* targ12
                                               , CTrace* targ13
                                               , CTrace* targ14
                                               , CTrace* targ15
                                               , FileOffset sr);

CTrace*		primTNm		(CTrace* tnm, CNmType* nm, FileOffset sr);
CTrace*		primTInd	(CTrace* t1, CTrace* t2);
CTrace*		primTHidden	(CTrace* t1);
CTrace*		primTSatA	(CTrace* t1);
CTrace*		primTSatB	(CTrace* t1);
CTrace*		primTSatC	(CTrace* t1, CTrace* t2);

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
int		primSameTrace	(CTrace* t1, CTrace* t2);

FileOffset	primTracePtr	(CTrace* t);
int		primTrustedFun	(CTrace* t);
int		primHidden	(CTrace* t);
CTrace*		mkTrace		(FileOffset p, int tr, int hid);

FileOffset	primSR0		(void);
FileOffset	primSR3		(SrcRef *sr);

FileOffset	readCurrentSatB	(void);
void		hat_exit	(char* errmsg,	CTrace* locatn,	int ecode);

#endif
