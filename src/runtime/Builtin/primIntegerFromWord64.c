#include "haskell2c.h"
#include "HsFFI.h"

#ifdef PROFILE
static SInfo nodeProfInfo = { "Builtin","Builtin.primIntegerFromWord64","Prelude.Integer"};
#endif

NodePtr primIntegerFromWord64 (HsWord64 i)
{
  Int tag,size;
  NodePtr result;
  C_CHECK(SIZE_INT64);
  if(i==0) {
    result = C_ALLOC(SIZE_ENUM);
    *result = CONSTRW(0,0);
    INIT_PROFINFO(result,&nodeProfInfo)
  } else if ((HS_WORD32_MIN<i) && (i<=HS_WORD32_MAX)) {
    result = C_ALLOC(SIZE_INT);
    result[0] = CONSTRW(1,0);
    INIT_PROFINFO(result,&nodeProfInfo)
    result[1+EXTRA] = (HsWord32)i;
  } else {
    result = C_ALLOC(SIZE_INT64);
    result[0] = CONSTRW(2,0);
    INIT_PROFINFO(result,&nodeProfInfo)
    result[1+EXTRA] = (HsWord32)(i&0x00000000ffffffff);
    result[2+EXTRA] = (HsWord32)((i&0xffffffff00000000)>>32);
  }
  return (result);
}


