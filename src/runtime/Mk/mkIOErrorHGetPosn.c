#include "mk.h"

NodePtr mkIOErrorHGetPosn(NodePtr a1,NodePtr a2)
{
  NodePtr n = C_ALLOC(1+EXTRA+2);
  n[0] = CONSTR(8,2,0);
  INIT_PROFINFO(n,&dummyProfInfo)
  n[EXTRA+1] = (Node)a1;
  n[EXTRA+2] = (Node)a2;
  return n;
}
