#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_putBits)
{ NodePtr nodeptr;
  BinHandle bh;
  int width;
  int value;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  width = (int)GET_INT_VALUE(nodeptr);
  
  nodeptr = C_GETARG1(3);
  IND_REMOVE(nodeptr);
  value = (int)GET_INT_VALUE(nodeptr);
  
  { unsigned int p;
    
    /* User code starts here */
    
    /* fprintf(stderr,"putBits %d %d: cache=%x cptr=%d\n",
    width,value,bh->cache[0],bh->cptr); */
    p = (bh->file ? vtell(bh) : mtell(bh));
    if (bh->highwater < p+width) bh->highwater = p+width;
    while (width > 0) {
    int byte    = bh->cptr / 8;
    int avail   = 8 - (bh->cptr % 8);
    if  (width >= avail) {
    bh->cache[byte] = lhs(8-avail,bh->cache[byte])
    | (value>>(width-avail));
    value     = rhs(width-avail,value);
    bh->cptr += avail;
    width    -= avail;
    bh->w     = ((bh->w > byte+1) ? bh->w : byte+1);
    if ((byte+1) == CACHESIZE) nextcache(bh);
    } else {
    int rsegment = avail-width;
    bh->cache[byte] = lhs(8-avail,bh->cache[byte])
    | (value << rsegment)
    | rhs(rsegment,bh->cache[byte]);
    bh->cptr += width;
    width     = 0;
    bh->w     = ((bh->w > byte+1) ? bh->w : byte+1);
    }
    }
    /* fprintf(stderr,"putBits end: cache=%x cptr=%d\n",
    bh->cache[0],bh->cptr); */
    /* User code ends here */
    
    nodeptr = mkInt(p);
    C_RETURN(nodeptr);
  }
  ;
}
