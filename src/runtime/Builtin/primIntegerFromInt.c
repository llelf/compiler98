
#include "haskell2c.h"

#ifdef PROFILE
static SInfo nodeProfInfo = { "Builtin","Builtin.primIntegerFromInt","Prelude.Integer"};
#endif

C_HEADER(primIntegerFromInt)
{
  Int tag,size,i;
  NodePtr nodeptr;
  C_CHECK(SIZE_INT);
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  i = GET_INT_VALUE(nodeptr);
  if(i==0) {
    nodeptr = C_ALLOC(SIZE_ENUM);
    *nodeptr = CONSTRW(0,0);
    INIT_PROFINFO(nodeptr,&nodeProfInfo)
  } else {
    nodeptr = C_ALLOC(SIZE_INT);
    if (i<0) {
      nodeptr[0] = CONSTRW(1,1);
      INIT_PROFINFO(nodeptr,&nodeProfInfo)
      nodeptr[1+EXTRA] = -i;
    } else {
      nodeptr[0] = CONSTRW(1,0);
      INIT_PROFINFO(nodeptr,&nodeProfInfo)
      nodeptr[1+EXTRA] = i;
    }
  }
  C_RETURN(nodeptr);
}
