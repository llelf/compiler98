#include <haskell2c.h>
#include "cLowBinary.h"
#define LT 0
#define EQ 1
#define GT 2

C_HEADER (gr_compareBin)
{ NodePtr nodeptr;
  int sizex;
  int sizey;
  BinHandle bhx;
  unsigned int x;
  BinHandle bhy;
  unsigned int y;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  sizex = (int)GET_INT_VALUE(nodeptr);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  sizey = (int)GET_INT_VALUE(nodeptr);
  
  nodeptr = C_GETARG1(3);
  IND_REMOVE(nodeptr);
  bhx = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(4);
  IND_REMOVE(nodeptr);
  x = (unsigned int)GET_INT_VALUE(nodeptr);
  
  nodeptr = C_GETARG1(5);
  IND_REMOVE(nodeptr);
  bhy = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(6);
  IND_REMOVE(nodeptr);
  y = (unsigned int)GET_INT_VALUE(nodeptr);
  
  { int retval;
    
    /* User code starts here */
    
    /* size of x and y are not necessarily byte-aligned */
    /* but bits of x and y are guaranteed byte-aligned at both ends */
    int bytex = (sizex%8 ? (sizex/8)+1 : sizex/8);
    int bytey = (sizey%8 ? (sizey/8)+1 : sizey/8);
    int xptr, yptr;
    if ((bhx->mode != RO) || (bhy->mode != RO)) {
    fprintf(stderr,"Binary.compareBin: BinHandle is not Read-Only.");
    exit(1);
    }
    xptr = (x - forceCacheTo(bhx,x)) / 8;
    yptr = (y - forceCacheTo(bhy,y)) / 8;
    /* fprintf(stderr,"compareBin: sizex=%d sizey=%d xoffset=%d yoffset=%d\n",
    sizex,sizey,x,y); */
    retval = EQ;
    while (bytex-- && bytey--) {
    if (bhx->cache[xptr] != bhy->cache[yptr]) {
    if (bhx->cache[xptr] < bhy->cache[yptr]) {
    retval = LT;
    } else {
    retval = GT;
    }
    break;
    }
    xptr++; yptr++;
    if (xptr==CACHESIZE) nextcache(bhx);
    if (yptr==CACHESIZE) nextcache(bhy);
    }
    /* User code ends here */
    
    nodeptr = mkInt(retval);
    C_RETURN(nodeptr);
  }
  ;
}
