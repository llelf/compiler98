#include <haskell2c.h>
#include "cLowBinary.h"
#include "cLowUnboxedArray.h"

C_HEADER (gr_putUBA)
{ NodePtr nodeptr;
  BinHandle bh;
  UBA uba;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  uba = (UBA)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  { unsigned int p;
    
    /* User code starts here */
    /* BH is always a file - never memory */
    unsigned temp;
    p = vtell(bh);
    closecache(bh);
    vsync(bh);
    temp = htonl(uba->free);
    write(bh->loc.fd,&temp,sizeof(unsigned));
    temp = htonl(uba->write);
    write(bh->loc.fd,&temp,sizeof(unsigned));
    temp = htonl(uba->end);
    write(bh->loc.fd,&temp,sizeof(unsigned));
    write(bh->loc.fd,uba->block,uba->write*sizeof(unsigned));
    opencache(bh);
    /* User code ends here */
    
    nodeptr = mkInt(p);
    C_RETURN(nodeptr);
  }
  ;
}


C_HEADER (gr_getUBA)
{ NodePtr nodeptr;
  BinHandle bh;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  { UBA uba;
    
    /* User code starts here */
    /* BH is always a file - never memory */
    unsigned temp;
    closecache(bh);
    vsync(bh);
    uba = (UBA)malloc(sizeof(struct UnboxedArray));
    read(bh->loc.fd,&temp,sizeof(unsigned));
    uba->free = ntohl(temp);
    read(bh->loc.fd,&temp,sizeof(unsigned));
    uba->write = ntohl(temp);
    read(bh->loc.fd,&temp,sizeof(unsigned));
    uba->end = ntohl(temp);
    uba->block = (unsigned*)malloc((uba->write+uba->free)*sizeof(unsigned));
    read(bh->loc.fd,uba->block,uba->write*sizeof(unsigned));
    opencache(bh);
    /* User code ends here */
    
    nodeptr = mkForeign((void*)uba,(gccval)finaliseUBA);
    C_RETURN(nodeptr);
  }
  ;
}
