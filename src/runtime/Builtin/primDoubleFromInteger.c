
#include "haskell2c.h"

#ifdef PROFILE
static SInfo nodeProfInfo = { "Builtin","Builtin.primDoubleFromInteger","Prelude.Double"};
#endif

C_HEADER(primDoubleFromInteger)
{
  Int tag,size,i;
  NodePtr nodeptr,ret;
  C_CHECK(SIZE_DOUBLE);
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  ret = C_ALLOC(SIZE_DOUBLE);
  mk_double(ret,sencode((MP_INT*) nodeptr, 0));
  INIT_PROFINFO(ret,&nodeProfInfo)
  C_RETURN(ret);
}
