
#include "haskell2c.h"

#ifdef PROFILE
static SInfo nodeProfInfo = { "Builtin","Builtin.primDecodeFloat","Prelude.Integer"};
#endif

C_HEADER(primDecodeFloat)
{
  Int tag,size;
  int exp;
  float f;
  NodePtr nodeptr,manptr,expptr;
  C_CHECK(1+SIZE_FLOAT+sizeInt+SIZE_TUPLE(2));
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  f = get_float_value(nodeptr);
  manptr = C_HP;
  exp = sdecode(f,(MP_INT *)manptr);
  INIT_PROFINFO(manptr,&nodeProfInfo)
  C_ADDHP(1+EXTRA+CONINFO_LARGESIZES(GET_CONINFO(manptr)));

  C_RETURN(mkTuple2(manptr,mkInt(exp)));
}
