#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_closeBin)
{ NodePtr nodeptr;
  BinHandle bh;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  { 
    /* User code starts here */
    preFinaliseBH(bh);
    /* User code ends here */
    
    nodeptr = mkUnit();
    C_RETURN(nodeptr);
  }
  ;
}
