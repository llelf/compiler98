#include <string.h>
#include <stdlib.h>
#include <setjmp.h>

#include "haskell2c.h"
#include "initend.h"

/* cExitWith :: ExitCode -> a */

#ifdef __CYGWIN32__
extern jmp_buf exit_mutator;
#else
extern sigjmp_buf exit_mutator;
#endif

C_HEADER(cExitWith)
{
  NodePtr nodeptr;
  char *src;
  int i;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  switch(GET_CONSTR(nodeptr)) {
  case ExitSuccess :
    exit_code = 0; break;
  case ExitFailure :
    nodeptr = GET_POINTER_ARG1(nodeptr,1);
    IND_REMOVE(nodeptr);
    exit_code = GET_INT_VALUE(nodeptr);
    break;
  }
#ifdef __CYGWIN32__
  longjmp(exit_mutator, 1);
#else
  siglongjmp(exit_mutator,1);
#endif
  C_RETURN(0);
}
