#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <setjmp.h>
#include "comp.h"

#include "node.h"
#include "mutlib.h"
/* #include "runtime.h" -- already included in node.h */

#ifdef DBGTRANS
extern Node FN_Prelude_46_95startDbg[];
#endif


/****/

#ifdef PROFILE
static SInfo mainProfInfo = { "<Main>","<Main>","<Main>"};
#endif

extern int exit_code;

#ifdef __CYGWIN32__
extern jmp_buf exit_mutator;
#else
extern sigjmp_buf exit_mutator;
#endif


int main(int argc, char **argv)
{
  haskellInit(argc,argv);	/* all cmd-line-arg setup is separate now */
#ifdef BYTECODE_PROF
  instr_prof_init();
#endif

  Fp = Sp;
  *--Sp = TOPLEVEL;
  *--Sp = (NodePtr)Fp;
  *--Sp = TOPLEVEL_code;
  Fp = Sp;
  spStart = Sp;
#ifndef DBGTRANS
  MK_VAP1(Hp,C_VAPTAG(MAIN), GET_WORLD);
  INIT_PROFINFO(Hp,&mainProfInfo);
  *--Sp = Hp;
  Hp += SIZE_VAP1;
#else
#define STARTDBG ((Node)FN_Prelude_46_95startDbg)
#define SIZE_VAP3  (EXTRA+4)
#define SIZE_VAP4  (EXTRA+5)
#define SIZE_VAP5  (EXTRA+6)
#define MK_VAP3(r,f,a1,a2,a3) (r)[0] = ((UInt)f) | VAP_TAG; (r)[1+EXTRA] = (a1); (r)[2+EXTRA] = (a2) ; (r)[3+EXTRA] = (a3)
#define MK_VAP4(r,f,a1,a2,a3,a4) (r)[0] = ((UInt)f) | VAP_TAG; (r)[1+EXTRA] = (a1); (r)[2+EXTRA] = (a2) ; (r)[3+EXTRA] = (a3) ; (r)[4+EXTRA] = (a4)
#define MK_VAP5(r,f,a1,a2,a3,a4,a5) (r)[0] = ((UInt)f) | VAP_TAG; (r)[1+EXTRA] = (a1); (r)[2+EXTRA] = (a2) ; (r)[3+EXTRA] = (a3) ; (r)[4+EXTRA] = (a4) ; (r)[5+EXTRA] = (a5)
  {
      
  MK_VAP1(Hp,C_VAPTAG(STARTDBG), GET_WORLD);
  INIT_PROFINFO(Hp,&mainProfInfo);
  *--Sp = Hp;
  Hp += SIZE_VAP1;
  }
#endif
#ifdef __CYGWIN32__
  if(!setjmp(exit_mutator)) {
#else
  if(!sigsetjmp(exit_mutator,0)) {
#endif
    run(TOPLEVEL);
    fprintf(stderr,"What, run() returned!\n");
    exit_code = -1;
  }

#ifdef BYTECODE_PROF
  instr_prof_results();
#endif

  return haskellEnd(argc,argv);

} /* end of main() */
