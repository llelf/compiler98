#include "mk.h"

NodePtr mkInt(Int i)
{
  NodePtr n;
  n = C_ALLOC(1+EXTRA+1);
  n[0] = CONSTRW(1,0);
  INIT_PROFINFO(n,&dummyProfInfo)
  n[EXTRA+1] = i;
  return n;
}
