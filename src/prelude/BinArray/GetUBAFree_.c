#include <haskell2c.h>
#include "cLowUnboxedArray.h"

C_HEADER (gr_getUBAFree)
{ NodePtr nodeptr;
  UBA uba;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  uba = (UBA)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  { 
    /* User code starts here */
    
    /* User code ends here */
    
    nodeptr = mkInt(uba->free);
    C_RETURN(nodeptr);
  }
  ;
}
