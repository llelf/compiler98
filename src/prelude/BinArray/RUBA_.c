#include <haskell2c.h>
#include "cLowUnboxedArray.h"

C_HEADER (gr_rUBA)
{ NodePtr nodeptr;
  UBA uba;
  int idx;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  uba = (UBA)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  idx = (int)GET_INT_VALUE(nodeptr);
  
  { 
    /* User code starts here */
    
    /* User code ends here */
    
    nodeptr = mkInt(ntohl(uba->block[idx]));
    C_RETURN(nodeptr);
  }
  ;
}
