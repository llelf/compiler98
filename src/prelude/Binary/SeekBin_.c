#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_seekBin)
{ NodePtr nodeptr;
  BinHandle bh;
  unsigned int p;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  p = (unsigned int)GET_INT_VALUE(nodeptr);
  
  { 
    /* User code starts here */
    
    /* fprintf(stderr,"seekBinFile %d: ",p); */
    bh->cptr = p - forceCacheTo(bh,p)*8;
    /* fprintf(stderr,"bh->cptr=%d\n",bh->cptr); */
    /* User code ends here */
    
    nodeptr = mkUnit();
    C_RETURN(nodeptr);
  }
  ;
}
