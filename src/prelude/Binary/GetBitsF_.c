#include <haskell2c.h>
#include "cLowBinary.h"
#include <stdio.h>

C_HEADER (gr_getBitsFAux)
{ NodePtr nodeptr;
  BinHandle bh;
  int width;
  unsigned int p;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  width = (int)GET_INT_VALUE(nodeptr);
  
  nodeptr = C_GETARG1(3);
  IND_REMOVE(nodeptr);
  p = (unsigned int)GET_INT_VALUE(nodeptr);
  
  { unsigned value;
    
    /* User code starts here */
    
    if (bh->mode != RO) {
    fprintf(stderr,"Binary.getBitsF: BinHandle is not read-only.");
    exit(1);
    }
    bh->cptr = p - forceCacheTo(bh,p)*8;
    value = 0;
    while (width > 0) {
    int byte  = bh->cptr / 8;
    int avail = 8 - (bh->cptr % 8);
    if (width >= avail) {
    value = (value<<avail)
    | rhs(avail,bh->cache[byte]);
    bh->cptr += avail;
    width    -= avail;
    if ((byte+1) == CACHESIZE) nextcache(bh);
    } else {
    value = (value<<width)
    | (rhs(avail,bh->cache[byte]) >> (avail-width));
    bh->cptr += width;
    width     = 0;
    }
    }
    /* User code ends here */
    
    nodeptr = mkInt(value);
    C_RETURN(nodeptr);
  }
  ;
}
