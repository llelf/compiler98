
#include "haskell2c.h"

/* length :: Prelude.PackedString -> Prelude.Int */

C_HEADER(length)
{
  int length;
  NodePtr nodeptr;
  Coninfo cinfo;

  C_CHECK(sizeInt);

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  cinfo =  GET_CONINFO(nodeptr);
  length = CONINFO_LARGESIZES(cinfo) * sizeof(Node) - CONINFO_LARGEEXTRA(cinfo);

  C_RETURN(mkInt(length));
}	
