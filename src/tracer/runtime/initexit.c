#include "ui.h"
#include "ident.h"
#include "fileformat.h"


/* exit handlers */

NodePtr dbg_last_trace = NULL;
extern  int exit_code;

void
hat_exit(char* errmsg, CTrace* location, int ecode)
{
  CNmType* nt;
  FileOffset fo;
  fprintf(stderr, "%s\n", errmsg);
  updateSatBs();
  updateSatCs();
  nt = primNTCString(errmsg);
  fo = nt->ptr;
  fseek(HatFile,8+sizeof(FileOffset),SEEK_SET);
  fwrite(&fo, sizeof(FileOffset), 1, HatFile);
  fo = location->ptr;
  fseek(HatFile,8,SEEK_SET);
  fwrite(&fo, sizeof(FileOffset), 1, HatFile);
  exit_code = ecode;
  haskellEnd();
  exit(ecode);	/* should never reach here */
}

C_HEADER(fatal)
{ hat_exit("No match in pattern.", (CTrace*)C_GETARG1(1), 1); }

void
dbg_blackhole()
{ hat_exit("Blackhole detected.", (CTrace*)dbg_last_trace, 2); }

void
hat_interrupted()
{
  CTrace ct;
  ct.ptr = readCurrentSatB();
  hat_exit("Program interrupted. (^C)", &ct, 3);
}



/* initialisation handler */

C_HEADER(cInitializeDebugger)
{
    NodePtr nodeptr;

  /*add_user_gc(otMark, otFlip);*/
    signal(SIGQUIT, hat_interrupted);
    signal(SIGINT, hat_interrupted);

    nodeptr = C_GETARG1(1);
    IND_REMOVE(nodeptr);
    C_RETURN(GET_POINTER_ARG1(nodeptr,1));
}

