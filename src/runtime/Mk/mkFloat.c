#include "mk.h"

NodePtr mkFloat(float f)
{
  NodePtr n;
  n = C_ALLOC(1+EXTRA+1);
  INIT_PROFINFO(n,&dummyProfInfo)
  mk_float(n,f);
  return n;
}
