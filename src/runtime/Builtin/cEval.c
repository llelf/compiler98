#include <string.h>
#include "haskell2c.h"

/* cEvaluating :: E a -> Bool */

C_HEADER(cEvaluating)
{
  NodePtr nodeptr;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  nodeptr = GET_POINTER_ARG1(nodeptr,1);
  IND_REMOVE(nodeptr);

  if((GET_TAG(nodeptr) & VAP_TAG) && !CINFO_NEED(GET_CINFO(nodeptr)) && ZAPPED(nodeptr))
    nodeptr = mkTrue();
  else
    nodeptr = mkFalse();

  C_RETURN(nodeptr);
}

/* cEvaluated :: E a -> Bool */

C_HEADER(cEvaluated)
{
  NodePtr nodeptr;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  nodeptr = GET_POINTER_ARG1(nodeptr,1);
  IND_REMOVE(nodeptr);

  if((GET_TAG(nodeptr) & VAP_TAG) && !CINFO_NEED(GET_CINFO(nodeptr)))
    nodeptr = mkFalse();
  else
    nodeptr = mkTrue();

  C_RETURN(nodeptr);
}


