#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_skipBits)
{ NodePtr nodeptr;
  BinHandle bh;
  int width;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  width = (int)GET_INT_VALUE(nodeptr);
  
  { 
    /* User code starts here */
    
    while (width > 0) {
    int avail = (CACHESIZE*8) - bh->cptr;
    if (width >= avail) {
    width    -= avail;
    nextcache(bh);
    } else {
    bh->cptr += width;
    width     = 0;
    }
    }
    /* User code ends here */
    
    nodeptr = mkUnit();
    C_RETURN(nodeptr);
  }
  ;
}
