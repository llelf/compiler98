#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_tellBin)
{ NodePtr nodeptr;
  BinHandle bh;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  { unsigned int p;
    
    /* User code starts here */
    p = (bh->file ? vtell(bh) : mtell(bh));
    /* User code ends here */
    
    nodeptr = mkInt(p);
    C_RETURN(nodeptr);
  }
  ;
}
