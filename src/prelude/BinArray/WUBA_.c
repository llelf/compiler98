#include <haskell2c.h>
#include "cLowUnboxedArray.h"

C_HEADER (gr_wUBA)
{ NodePtr nodeptr;
  UBA uba;
  unsigned int p;
  unsigned int end;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  uba = (UBA)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  p = (unsigned int)GET_INT_VALUE(nodeptr);
  
  nodeptr = C_GETARG1(3);
  IND_REMOVE(nodeptr);
  end = (unsigned int)GET_INT_VALUE(nodeptr);
  
  { int idx;
    
    /* User code starts here */
    
    uba->block[uba->write] = htonl(p);
    uba->free--;
    uba->end = end;
    idx = uba->write++;
    /* User code ends here */
    
    nodeptr = mkInt(idx);
    C_RETURN(nodeptr);
  }
  ;
}
