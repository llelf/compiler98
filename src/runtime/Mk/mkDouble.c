#include "mk.h"
#include "mutlib.h"

NodePtr mkDouble(double d)
{
  NodePtr n;
  n = C_ALLOC(SIZE_DOUBLE);
  INIT_PROFINFO(n,&dummyProfInfo)
  mk_double(n,d);
  return n;
}
