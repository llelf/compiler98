/* from initexit ********************************************************* */

#ifndef _HAT_H
#define _HAT_H

#include "art.h"


void            openTrace (char *progname);
void            closeTrace (void);
void            hat_interrupted(int);


void		initialiseSATstack	(void);

FileOffset	primTRoot	(void);
FileOffset	primTAp1	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset sr);
FileOffset      primTAp2	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset sr);
FileOffset	primTAp3	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset sr);
FileOffset      primTAp4	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset sr);

FileOffset		primTAp5	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset sr);

FileOffset		primTAp6	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset targ6
                                               , FileOffset sr);

FileOffset		primTAp7	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset targ6
                                               , FileOffset targ7
                                               , FileOffset sr);

FileOffset		primTAp8	(FileOffset tap, FileOffset tfn
                                               , FileOffset targ1
                                               , FileOffset targ2
                                               , FileOffset targ3
                                               , FileOffset targ4
                                               , FileOffset targ5
                                               , FileOffset targ6
                                               , FileOffset targ7
                                               , FileOffset targ8
                                               , FileOffset sr);

FileOffset		primTAp9	(FileOffset tap, FileOffset tfn
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

FileOffset		primTAp10	(FileOffset tap, FileOffset tfn
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

FileOffset		primTAp11	(FileOffset tap, FileOffset tfn
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

FileOffset		primTAp12	(FileOffset tap, FileOffset tfn
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

FileOffset		primTAp13	(FileOffset tap, FileOffset tfn
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
                                               , FileOffset targ13
                                               , FileOffset sr);

FileOffset		primTAp14	(FileOffset tap, FileOffset tfn
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
                                               , FileOffset targ13
                                               , FileOffset targ14
                                               , FileOffset sr);

FileOffset		primTAp15	(FileOffset tap, FileOffset tfn
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
                                               , FileOffset targ13
                                               , FileOffset targ14
                                               , FileOffset targ15
                                               , FileOffset sr);

FileOffset		primTNm		(FileOffset tnm, FileOffset nm, FileOffset sr);
FileOffset		primTInd	(FileOffset t1, FileOffset t2);
FileOffset		primTHidden	(FileOffset t1);
FileOffset		primTSatA	(FileOffset t1);
FileOffset		primTSatB	(FileOffset t1);
FileOffset		primTSatC	(FileOffset t1, FileOffset t2);

void		updateSatBs	(void);
void		updateSatCs	(void);

FileOffset	primNTInt	(int i);
FileOffset	primNTChar	(char c);
FileOffset	primNTInteger	(int i);
FileOffset	primNTRational	(int numerator,int denominator);
/* FileOffset	primNTInteger	(NodePtr i); */
/* FileOffset	primNTRational	(NodePtr i,NodePtr j); */
FileOffset	primNTFloat	(float f);
FileOffset	primNTDouble	(double d);
FileOffset	primNTTuple	(void);
FileOffset	primNTFun	(void);
FileOffset	primNTCase	(void);
FileOffset	primNTLambda	(void);
FileOffset	primNTDummy	(void);
FileOffset	primNTCString	(char *s);
FileOffset	primNTIf	(void);
FileOffset	primNTGuard	(void);
FileOffset	primNTContainer	(void);

int		primTrustedNm	(FileOffset nm);
int		primSameTrace	(FileOffset t1, FileOffset t2);

FileOffset	primTracePtr	(FileOffset t);
int		primTrustedFun	(FileOffset t);
int		primHidden	(FileOffset t);
FileOffset		mkTrace		(FileOffset p, int tr, int hid);


FileOffset	readCurrentSatB	(void);

FileOffset      primSourceRef   (FileOffset moduleTraceInfo,int pos);
FileOffset        primAtomCon     (FileOffset moduleTraceInfo, int pos, int fixPri, char *name);
FileOffset        primAtomId      (FileOffset moduleTraceInfo, int pos, int fixPri, char *name);
FileOffset        primAtomIdToplevel (FileOffset moduleTraceInfo, int pos, int fixPri, char *name);
FileOffset      primModule      (char *modname, char *srcfile);
void            outputTrace     (FileOffset trace, char *output);


#endif
