#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_isEOFBin)
{ NodePtr nodeptr;
  BinHandle bh;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  { int b;
    
    /* User code starts here */
    
    if (bh->file)
    b = bh->cptr >= 8*(CACHESIZE-bh->attrib.eof);
    else
    b = bh->adjust.here + (bh->cptr/8) >= bh->attrib.size;
    /* User code ends here */
    
    nodeptr = mkBool(b);
    C_RETURN(nodeptr);
  }
  ;
}
