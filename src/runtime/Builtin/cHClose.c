
#include "haskell2c.h"

/* cHClose primitive 1 :: Handle -> () */


/* primPutChar :: File -> Char -> () */
C_HEADER(cHClose)
{
  Arg *a;
  NodePtr nodeptr;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  UPDATE_PROFINFO(nodeptr)
  a = cdataArg((CData *)GET_INT_VALUE(nodeptr));

#ifdef PROFILE
  if(!replay)
#endif
    fclose(a->fp);
  a->gc = 0;
  a->bm = -1;

  C_RETURN(mkUnit());
}


