#include "mk.h"

NodePtr mkDouble(double d)
{
  NodePtr n;
  n = C_ALLOC(1+EXTRA+2);
  INIT_PROFINFO(n,&dummyProfInfo)
  mk_double(n,d);
  return n;
}
