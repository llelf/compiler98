#include "newmacros.h"

extern unsigned C0_Builtin_46PrimToken[];
extern unsigned CF__95Builtin_46hputc_95ok[];
extern unsigned Start_World[];
extern unsigned FN_Prelude_46primLeave[];
extern unsigned FN_Builtin_46primUnpackCString[];
extern unsigned FN_Builtin_46hgets[];
extern unsigned CF_IO_46stdout[];
extern unsigned CF_IO_46stdin[];
extern unsigned CF_IO_46stderr[];
extern unsigned CF_IO_46_95stdin[];
extern unsigned CF_IO_46_95stdout[];
extern unsigned CF_IO_46_95stderr[];

extern unsigned C0_Prelude_46_91_93[];
extern unsigned CT_primLeave[];
extern unsigned ST_primLeave[];
extern unsigned CT_unpackCString[];
extern unsigned ST_Builtin_46primUnpackCString[];
extern unsigned CT_hgets[];
extern unsigned ST_Builtin_46hgets[];
extern unsigned fo_stdout[];
extern unsigned fo_stdin[];
extern unsigned fo_stderr[];

#ifdef PROFILE
extern unsigned PP_Prelude_46primLeave[];
extern unsigned PC_Prelude_46primLeave[];
extern unsigned PP_Builtin_46primUnpackCString[];
extern unsigned PC_Builtin_46primUnpackCString[];
extern unsigned PP_Builtin_46hgets[];
extern unsigned PC_Builtin_46hgets[];

extern unsigned PROF_primToken[];
extern unsigned PROF_hputc_ok[];
extern unsigned PROF_Start_World[];
extern unsigned PROF_primLeave[];
extern unsigned PROF_46primUnpackCString[];
extern unsigned PROF_46hgets[];
extern unsigned PM_Prelude[];
extern unsigned PM_IO[];
extern unsigned PP_compiletime[];
extern unsigned PROF_IO_46stdout[];
extern unsigned PROF_IO_46stdin[];
extern unsigned PROF_IO_46stderr[];
extern unsigned PC_primToken[];
extern unsigned PC_hputc_ok[];
extern unsigned PC_Start_World[];
extern unsigned PC_IO_46stdin[];
extern unsigned PC_IO_46stdout[];
extern unsigned PC_IO_46stderr[];
#endif

#ifdef DBGTRANS
extern unsigned fo_stdin_node[];
extern unsigned stdin_t[];
extern unsigned stdin_nm[];
extern unsigned stdin_s[];
extern unsigned root_t[];
extern unsigned noSR[];
extern unsigned io_s[];
extern unsigned fo_stdout_node[];
extern unsigned fo_stderr_node[];
extern unsigned stdout_t[];
extern unsigned stdout_nm[];
extern unsigned stdout_s[];
extern unsigned stderr_t[];
extern unsigned stderr_nm[];
extern unsigned stderr_s[];
#endif

unsigned C0_Builtin_46PrimToken[] = {
  CONSTR(0,0,0)
#ifdef PROFILE
, useLabel(PROF_primToken)
, 0
, 0
, 0
#endif
};
unsigned CF__95Builtin_46hputc_95ok[] = {
  CONSTR(1,1,0)
#ifdef PROFILE
, useLabel(PROF_hputc_ok)
, 0
, 0
, 0
#endif
, useLabel(C0_Prelude_46_91_93)
};
unsigned Start_World[] = {
 CONSTR(0,2,0)
#ifdef PROFILE
, useLabel(PROF_Start_World)
, 0
, 0
, 0
#endif
, useLabel(C0_Prelude_46_91_93)
, useLabel(C0_Prelude_46_91_93)

  FILL4toWORD
, bytes2word(1,0,0,1)
, useLabel(CT_primLeave)
};
unsigned FN_Prelude_46primLeave[] = {
#ifdef TPROF
  0,		/* DAVID/PH */
#endif
  bytes2word(EXIT,ENDCODE,0,0)	/* DAVID */

, 0
, useLabel(ST_primLeave)
};
unsigned CT_primLeave[] = {
  HW(0,1)
, 0
};
unsigned F0_Prelude_46primLeave[] = {
  CAPTAG(FN_Prelude_46primLeave,1)
#ifdef PROFILE
, useLabel(PROF_primLeave)
, 0
, 0
, 0
#endif

, bytes2word(1,0,0,1)
, useLabel(CT_unpackCString)
};
unsigned FN_Builtin_46primUnpackCString[] = {
#ifdef TPROF
  0,		/* DAVID/PH */
#endif
  bytes2word(NEEDHEAP_I32,PUSH_ARG_I1,STRING,RETURN)
, bytes2word(ENDCODE,0,0,0)	/* DAVID */

, 0
, useLabel(ST_Builtin_46primUnpackCString)
};
unsigned CT_unpackCString[] = {
  HW(0,1)
, 0
};
unsigned F0_Builtin_46primUnpackCString[] = {
  CAPTAG(FN_Builtin_46primUnpackCString,1)
#ifdef PROFILE
, useLabel(PROF_46primUnpackCString)
, 0
, 0
, 0
};
unsigned PROF_46primUnpackCString[] = {
  useLabel(PM_Prelude)
, useLabel(PP_compiletime)
, useLabel(PC_Builtin_46primUnpackCString)
#endif

, bytes2word(1,0,0,1)
, useLabel(CT_hgets)
};
unsigned FN_Builtin_46hgets[] = {
#ifdef TPROF
  0,		/* DAVID/PH */
#endif
  bytes2word(NEEDHEAP_I32,PUSH_ARG_I1,HGETS,RETURN)
, bytes2word(ENDCODE,0,0,0)	/* DAVID */

, 0
, useLabel(ST_Builtin_46hgets)
};
unsigned CT_hgets[] = {
  HW(0,1)
, 0
};
unsigned F0_Builtin_46hgets[] = {
  CAPTAG(FN_Builtin_46hgets,1)
#ifdef PROFILE
, useLabel(PROF_46hgets)
, 0
, 0
, 0
};
unsigned PROF_46hgets[] = {
  useLabel(PM_Prelude)
, useLabel(PP_compiletime)
, useLabel(PC_Builtin_46hgets)
#endif

#ifndef DBGTRANS
};
unsigned CF_IO_46stdout[] = {
  CONSTRW(1,0)
#ifdef PROFILE
, useLabel(PROF_IO_46stdout)
, 0
, 0
, 0
#endif
, useLabel(fo_stdout)

};
unsigned CF_IO_46stdin[] = {
  CONSTRW(1,0)
#ifdef PROFILE
, useLabel(PROF_IO_46stdin)
, 0
, 0
, 0
#endif
,  useLabel(fo_stdin)

};
unsigned CF_IO_46stderr[] = {
  CONSTRW(1,0)
#ifdef PROFILE
, useLabel(PROF_IO_46stderr)
, 0
, 0
, 0
#endif
, useLabel(fo_stderr)
#endif /* !DBGTRANS */

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


};
unsigned CF_IO_46_95stdin[] = {
  CONSTRR(0, 2, 0)
#ifdef PROFILE
, useLabel(PROF_IO_46stdin)
, 0
, 0
, 0
#endif
, useLabel(fo_stdin_node)
, useLabel(stdin_t)

};
unsigned fo_stdin_node[] = {
  CONSTRW(1,0)
#ifdef PROFILE
, useLabel(PROF_IO_46stdin)
, 0
, 0
, 0
#endif
, useLabel(fo_stdin)

};
unsigned stdin_t[] = {
  CONSTRT(TagNm,3,0)
#ifdef PROFILE
, useLabel(PROF_IO_46stdin)
, 0
, 0
, 0
#endif
, useLabel(root_t)
, useLabel(stdin_nm)
, useLabel(noSR)

};
unsigned stdin_nm[] = {
  CONSTR(NTId,3,3)
#ifdef PROFILE
, useLabel(PROF_IO_46stdin)
, 0
, 0
, 0
#endif
, useLabel(io_s)
, 0
, useLabel(stdin_s)
, 0
, 0

};
unsigned CF_IO_46_95stdout[] = {
  CONSTRR(0, 2, 0)
#ifdef PROFILE
, useLabel(PROF_IO_46stdout)
, 0
, 0
, 0
#endif
, useLabel(fo_stdout_node)
, useLabel(stdout_t)

};
unsigned fo_stdout_node[] = {
  CONSTRW(1,0)
#ifdef PROFILE
, useLabel(PROF_IO_46stdout)
, 0
, 0
, 0
#endif
, useLabel(fo_stdout)

};
unsigned stdout_t[] = {
  CONSTRT(TagNm,3,0)
#ifdef PROFILE
, useLabel(PROF_IO_46stdout)
, 0
, 0
, 0
#endif
, useLabel(root_t)
, useLabel(stdout_nm)
, useLabel(noSR)

};
unsigned stdout_nm[] = {
  CONSTR(NTId,3,3)
#ifdef PROFILE
, useLabel(PROF_IO_46stdout)
, 0
, 0
, 0
#endif
, useLabel(io_s)
, 0
, useLabel(stdout_s)
, 0
, 0

};
unsigned CF_IO_46_95stderr[] = {
  CONSTRR(0, 2, 0)
#ifdef PROFILE
, useLabel(PROF_IO_46stderr)
, 0
, 0
, 0
#endif
, useLabel(fo_stderr_node)
, useLabel(stderr_t)

};
unsigned fo_stderr_node[] = {
  CONSTRW(1,0)
#ifdef PROFILE
, useLabel(PROF_IO_46stderr)
, 0
, 0
, 0
#endif
, useLabel(fo_stderr)

};
unsigned stderr_t[] = {
  CONSTRT(TagNm,3,0)
#ifdef PROFILE
, useLabel(PROF_IO_46stderr)
, 0
, 0
, 0
#endif
, useLabel(root_t)
, useLabel(stderr_nm)
, useLabel(noSR)

};
unsigned stderr_nm[] = {
  CONSTR(NTId,3,3)
#ifdef PROFILE
, useLabel(PROF_IO_46stderr)
, 0
, 0
, 0
#endif
, useLabel(io_s)
, 0
, useLabel(stderr_s)
, 0
, 0

};
unsigned io_s[] = {
  bytes2word('I','O',0,0)
};
unsigned stdin_s[] = {
  bytes2word('<','s','t','d')
, bytes2word('i','n','>',0)
};
unsigned stdout_s[] = {
  bytes2word('<','s','t','d')
, bytes2word('o','u','t','>')
, 0
};
unsigned stderr_s[] = {
  bytes2word('<','s','t','d')
, bytes2word('e','r','r','>')
, 0

};
unsigned root_t[] = {
  CONSTRT(TagRoot,0,0)
#ifdef PROFILE
, useLabel(PROF_IO_46stdin)
, 0
, 0
, 0
#endif

};

unsigned noSR[] = {
  CONSTRW(3,TagSR)
#ifdef PROFILE
, useLabel(PROF_IO_46stdin)
, 0
, 0
, 0
#endif
, 0
, 0
, 0

#endif /* DBGTRANS */


#ifdef PROFILE
};
unsigned PROF_primLeave[] = {
  useLabel(PM_Prelude)
, useLabel(PP_compiletime)
, useLabel(PC_Prelude_46primLeave)
};
unsigned PROF_primToken[] = {
  useLabel(PM_Prelude)
, useLabel(PP_compiletime)
, useLabel(PC_primToken)
};
unsigned PROF_hputc_ok[] = {
  useLabel(PM_Prelude)
, useLabel(PP_compiletime)
, useLabel(PC_hputc_ok)
};
unsigned PROF_Start_World[] = {
  useLabel(PM_Prelude)
, useLabel(PP_compiletime)
, useLabel(PC_Start_World)
};
unsigned PROF_IO_46stdin[] = {
  useLabel(PM_IO)
, useLabel(PP_compiletime)
, useLabel(PC_IO_46stdin)
};
unsigned PROF_IO_46stdout[] = {
  useLabel(PM_IO)
, useLabel(PP_compiletime)
, useLabel(PC_IO_46stdout)
};
unsigned PROF_IO_46stderr[] = {
  useLabel(PM_IO)
, useLabel(PP_compiletime)
, useLabel(PC_IO_46stderr)

};
unsigned PC_IO_46stdin[] = {
  bytes2word('I','O','.','s')
, bytes2word('t','d','i','n')
, 0
};
unsigned PC_IO_46stdout[] = {
  bytes2word('I','O','.','s')
, bytes2word('t','d','o','u')
, bytes2word('t',0,0,0)
};
unsigned PC_IO_46stderr[] = {
  bytes2word('I','O','.','s')
, bytes2word('t','d','e','r')
, bytes2word('r',0,0,0)
};
unsigned PC_primToken[] = {
  bytes2word('P','r','e','l')
, bytes2word('u','d','e','.')
, bytes2word('p','r','i','m')
, bytes2word('T','o','k','e')
, bytes2word('n',0,0,0)
};
unsigned PC_hputc_ok[] = {
  bytes2word('B','u','i','l')
, bytes2word('t','i','n','.')
, bytes2word('h','p','u','t')
, bytes2word('c','_','o','k')
, 0
};
unsigned PC_Start_World[] = {
  bytes2word('P','r','e','l')
, bytes2word('u','d','e','.')
, bytes2word('S','t','a','r')
, bytes2word('t','_','w','o')
, bytes2word('r','l','d',0)

#endif

  
#ifdef PROFILE
};
unsigned PP_Prelude_46primLeave[] = {
};
unsigned PC_Prelude_46primLeave[] = {
#endif
};
unsigned ST_primLeave[] = {
  bytes2word('P','r','e','l')
, bytes2word('u','d','e','.')
, bytes2word('p','r','i','m')
, bytes2word('L','e','a','v')
, bytes2word('e',0,0,0)

#ifdef PROFILE
};
unsigned PP_Builtin_46primUnpackCString[] = {
};
unsigned PC_Builtin_46primUnpackCString[] = {
#endif
};
unsigned ST_Builtin_46primUnpackCString[] = {
  bytes2word('B','u','i','l')
, bytes2word('t','i','n','.')
, bytes2word('p','r','i','m')
, bytes2word('U','n','p','a')
, bytes2word('c','k','C','S')
, bytes2word('t','r','i','n')
, bytes2word('g',0,0,0)

#ifdef PROFILE
};
unsigned PP_Builtin_46hgets[] = {
};
unsigned PC_Builtin_46hgets[] = {
#endif
};
unsigned ST_Builtin_46hgets[] = {
  bytes2word('B','u','i','l')
, bytes2word('t','i','n','.')
, bytes2word('h','g','e','t')
, bytes2word('s',0,0,0)
};

