#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include <errno.h>

#include "haskell2c.h"

/* primEqHandlePosn primitive 2 :: HandlePosn -> HandlePosn -> Bool */

C_HEADER(primEqHandlePosn)
{
  NodePtr nodeptr;
  fpos_t *fpos1, *fpos2;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  fpos1 = getHandlePosn(nodeptr);

  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  fpos2 = getHandlePosn(nodeptr);

  C_RETURN(mkBool(!memcmp(fpos1,fpos2,sizeof(fpos_t))));
}	
