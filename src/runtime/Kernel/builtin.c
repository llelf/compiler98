/* This file is now obsolete - use newbuiltin.c instead */

#include "codemacros.h"

 STARTBYTECODE

 AL
  EX L(C0_Builtin_46PrimToken)
DL(C0_Builtin_46PrimToken)
 DW CONSTR(0,0,0)
#ifdef PROFILE
 DW L(PROF_primToken), 0, 0, 0
#endif

 AL
  EX L(CF__95Builtin_46hputc_95ok)
DL(CF__95Builtin_46hputc_95ok)
 DW CONSTR(1,1,0)
#ifdef PROFILE
 DW L(PROF_hputc_ok), 0, 0, 0
#endif
 DW L(C0_Prelude_46_91_93)

 AL
  EX L(Start_World)
DL(Start_World)
 DW CONSTR(0,2,0)
#ifdef PROFILE
 DW L(PROF_Start_World), 0, 0, 0
#endif
 DW L(C0_Prelude_46_91_93)
 DW L(C0_Prelude_46_91_93)

 AL
 DB FILL4toWORD 1,0,0,1
 DW L(CT_primLeave)
  EX L(FN_Prelude_46primLeave)
DL(FN_Prelude_46primLeave)
#ifdef TPROF
   DW 0		/* DAVID/PH */
#endif
 DB EXIT
 DB ENDCODE	/* DAVID */

 AL
 DW 0
 DW L(ST_primLeave)
DL(CT_primLeave)
 DW HW(0,1)
 DW 0
DL(F0_Prelude_46primLeave)
 DW CAPTAG(FN_Prelude_46primLeave,1)
#ifdef PROFILE
 DW L(PROF_primLeave), 0, 0, 0
#endif

 AL
 DB FILL4toWORD 1,0,0,1
 DW L(CT_unpackCString)
  EX L(FN_Builtin_46primUnpackCString)
DL(FN_Builtin_46primUnpackCString)
#ifdef TPROF
   DW 0		/* DAVID/PH */
#endif
 DB NEEDHEAP_I32
 DB NEEDSTACK_I16
 DB PUSH_ARG_I1
 DB STRING
 DB RETURN
 DB ENDCODE	/* DAVID */

 AL
 DW 0
 DW L(ST_Builtin_46primUnpackCString)
DL(CT_unpackCString)
 DW HW(0,1)
 DW 0
DL(F0_Builtin_46primUnpackCString)
 DW CAPTAG(FN_Builtin_46primUnpackCString,1)
#ifdef PROFILE
 DW L(PROF_46primUnpackCString), 0, 0, 0
DL(PROF_46primUnpackCString)
 DW L(PM_Prelude),L(PP_compiletime),L(PC_Builtin_46primUnpackCString)
#endif

#ifndef DBGTRANS
  AL
  EX L(CF_Prelude_46stdout)
DL(CF_Prelude_46stdout)
  DW CONSTRW(1,0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdout), 0, 0, 0
#endif
  DW L(fo_stdout)

  AL
  EX L(CF_Prelude_46stdin)
DL(CF_Prelude_46stdin)
  DW CONSTRW(1,0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdin), 0, 0, 0
#endif
  DW L(fo_stdin)

  AL
  EX L(CF_Prelude_46stderr)
DL(CF_Prelude_46stderr)
  DW CONSTRW(1,0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stderr), 0, 0, 0
#endif
  DW L(fo_stderr)
#endif /* DBGTRANS */

#ifdef DBGTRANS

/* Don't change anything here!!! (See hsruntime/getconstr.h) */
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

#define TagAp		0
#define TagNm		1
#define TagInd		2
#define TagRoot		3
#define TagSat		4
#define TagPruned	5

#define TagSR		0
#define TagSR2		1
#define TagSR3		2


  AL
  EX L(CF_Prelude_46_95stdin)
DL(CF_Prelude_46_95stdin)
  DW CONSTRR(0, 2, 0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdin), 0, 0, 0
#endif
  DW L(fo_stdin_node), L(stdin_t)

DL(fo_stdin_node)
  DW CONSTRW(1,0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdin), 0, 0, 0
#endif
  DW L(fo_stdin)

DL(stdin_t)
  DW CONSTR(TagNm,3,0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdin), 0, 0, 0
#endif
  DW L(root_t), L(stdin_nm), L(sr_t)

DL(stdin_nm)
  DW CONSTR(NTId,3,3)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdin), 0, 0, 0
#endif
  DW L(prelude_s), 0, L(stdin_s)

  EX L(CF_Prelude_46_95stdout)
DL(CF_Prelude_46_95stdout)
  DW CONSTRR(0, 2, 0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdout), 0, 0, 0
#endif
  DW L(fo_stdout_node), L(stdout_t)

DL(fo_stdout_node)
  DW CONSTRW(1,0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdout), 0, 0, 0
#endif
  DW L(fo_stdout)

DL(stdout_t)
  DW CONSTR(TagNm,3,0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdout), 0, 0, 0
#endif
  DW L(root_t), L(stdout_nm), L(sr_t)

DL(stdout_nm)
  DW CONSTR(NTId,3,3)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdout), 0, 0, 0
#endif
  DW L(prelude_s), 0, L(stdout_s)

  EX L(CF_Prelude_46_95stderr)
DL(CF_Prelude_46_95stderr)
  DW CONSTRR(0, 2, 0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stderr), 0, 0, 0
#endif
  DW L(fo_stderr_node), L(stderr_t)

DL(fo_stderr_node)
  DW CONSTRW(1,0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stderr), 0, 0, 0
#endif
  DW L(fo_stderr)

DL(stderr_t)
  DW CONSTR(TagNm,3,0)
#ifdef PROFILE
  DW L(PROF_Prelude_46stderr), 0, 0, 0
#endif
  DW L(root_t), L(stderr_nm), L(sr_t)

DL(stderr_nm)
  DW CONSTR(NTId,3,3)
#ifdef PROFILE
  DW L(PROF_Prelude_46stderr), 0, 0, 0
#endif
  DW L(prelude_s), 0, L(stderr_s)

DL(prelude_s)
  DS "Prelude"
  DB 0

DL(stdin_s)
  DS "<stdin>"
  DB 0

DL(stdout_s)
  DS "<stdout>"
  DB 0

DL(stderr_s)
  DS "<stderr>"
  DB 0
  AL

DL(root_t)
  DW CONSTRW(0,TagRoot)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdin), 0, 0, 0
#endif

DL(sr_t)
  DW CONSTRW(0,TagSR)
#ifdef PROFILE
  DW L(PROF_Prelude_46stdin), 0, 0, 0
#endif

#endif /* DBGTRANS */


#ifdef PROFILE

AL
DL(PROF_primLeave)
 DW L(PM_Prelude),L(PP_compiletime),L(PC_Prelude_46primLeave)

DL(PROF_primToken)
 DW L(PM_Prelude),L(PP_compiletime),L(PC_primToken)

DL(PROF_hputc_ok)
 DW L(PM_Prelude),L(PP_compiletime),L(PC_hputc_ok)

DL(PROF_Start_World)
 DW L(PM_Prelude),L(PP_compiletime),L(PC_Start_World)

DL(PROF_Prelude_46stdin)
 DW L(PM_Prelude),L(PP_compiletime),L(PC_Prelude_46stdin)

DL(PROF_Prelude_46stdout)
 DW L(PM_Prelude),L(PP_compiletime),L(PC_Prelude_46stdout)

DL(PROF_Prelude_46stderr)
 DW L(PM_Prelude),L(PP_compiletime),L(PC_Prelude_46stderr)

DL(PC_Prelude_46stdin)
 DS "Prelude.stdin"
 DB 0

DL(PC_Prelude_46stdout)
 DS "Prelude.stdout"
 DB 0

DL(PC_Prelude_46stderr)
 DS "Prelude.stderr"
 DB 0

DL(PC_primToken)
 DS "Prelude.primToken"
 DB 0

DL(PC_hputc_ok)
 DS "Builtin.hputc_ok"
 DB 0

DL(PC_Start_World)
 DS "Prelude.Start_World"
 DB 0

#endif

  
#ifdef PROFILE
  AL
  EX L(PP_Prelude_46primLeave)
DL(PP_Prelude_46primLeave)
  EX L(PC_Prelude_46primLeave)
DL(PC_Prelude_46primLeave)
#endif
DL(ST_primLeave)
  DS "Prelude.primLeave"
  DB 0

#ifdef PROFILE
  AL
  EX L(PP_Builtin_46primUnpackCString)
DL(PP_Builtin_46primUnpackCString)
  EX L(PC_Builtin_46primUnpackCString)
DL(PC_Builtin_46primUnpackCString)
#endif
DL(ST_Builtin_46primUnpackCString)
  DS "Builtin.primUnpackString"
  DB 0

