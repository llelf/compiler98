#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_equalsBin)
{ NodePtr nodeptr;
  int size;
  BinHandle bhx;
  unsigned int x;
  BinHandle bhy;
  unsigned int y;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  size = (int)GET_INT_VALUE(nodeptr);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  bhx = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(3);
  IND_REMOVE(nodeptr);
  x = (unsigned int)GET_INT_VALUE(nodeptr);
  
  nodeptr = C_GETARG1(4);
  IND_REMOVE(nodeptr);
  bhy = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(5);
  IND_REMOVE(nodeptr);
  y = (unsigned int)GET_INT_VALUE(nodeptr);
  
  { int retval;
    
    /* User code starts here */
    
    /* size of x and y are not necessarily byte-aligned */
    /* but bits of x and y are guaranteed byte-aligned at both ends */
    unsigned int bytes = (size%8 ? size/8+1 : size/8);
    int xptr, yptr;
    xptr = (x - forceCacheTo(bhx,x)) / 8;
    yptr = (y - forceCacheTo(bhy,y)) / 8;
    retval = 1;
    while (bytes--) {
    if (bhx->cache[xptr] != bhy->cache[yptr]) {
    retval = 0;
    break;
    }
    xptr++; yptr++;
    if (xptr==CACHESIZE) nextcache(bhx);
    if (yptr==CACHESIZE) nextcache(bhy);
    }
    /* User code ends here */
    
    nodeptr = mkBool(retval);
    C_RETURN(nodeptr);
  }
  ;
}
