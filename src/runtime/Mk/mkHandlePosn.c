#include "mk.h"

NodePtr mkHandlePosn(fpos_t *fpos)
{
  NodePtr n = C_ALLOC(1+EXTRA+sizeHandlePosn);
  n[0] = CONSTR(0,1,0);
  INIT_PROFINFO(n,&dummyProfInfo)
  memcpy(&n[1+EXTRA],&fpos,sizeof(fpos_t));
  return n;
}

fpos_t *getHandlePosn(NodePtr n)
{
  return (fpos_t *)(&n[1+EXTRA]);
}
