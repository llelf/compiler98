#include "mk.h"

NodePtr mkCInt(Int i)
{
  NodePtr n;
  n = C_ALLOC(1+EXTRA+1);
  n[0] = CONSTRC(0,1,1);
  INIT_PROFINFO(n,&dummyProfInfo)
  n[EXTRA+1] = i;
  return n;
}
