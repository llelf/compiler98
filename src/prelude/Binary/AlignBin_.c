#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_alignBin)
{ NodePtr nodeptr;
  BinHandle bh;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  { 
    /* User code starts here */
    if (bh->cptr / 8 > CACHESIZE-1) {
    nextcache(bh);
    } else {
    int m = bh->cptr % 8;
    if (m) bh->cptr += 8 - m;
    }
    /* User code ends here */
    
    nodeptr = mkUnit();
    C_RETURN(nodeptr);
  }
  ;
}
