
#include "cinterface.h"
/*#include "node.h"  -- already included in cinterface.h */

#if 0

Node Prim_primApply1[1];

int Main_46cexit(int s, NodePtr vapptr, NodePtr *hp, NodePtr **sp)
{
  fflush(stdout);
  fprintf(stderr, "Success!!!!\n");
  exit(0);
  return 0;
}

Node DIO[2];

int Main_46cprint(int s, NodePtr vapptr, NodePtr *hp, NodePtr **sp)
{
  NodePtr nodeptr = GET_POINTER_ARG1(vapptr,1);
  char c;
  IND_REMOVE(nodeptr);
  c = GET_INT_VALUE(nodeptr);
  putchar(c);
  fflush(stdout);
  DIO[0] = CONSTR(0,1,0);
  DIO[1] = (Node)DIO;
  *--(*sp) = DIO;
  return 0;
}

void FN_Prelude_46error(void)
{
  fprintf(stderr,"Not implemented error yet");
  exit(-1);
}

#endif

