#include <haskell2c.h>
#include "cLowUnboxedArray.h"
#include <malloc.h>

C_HEADER (gr_allocUBA)
{ NodePtr nodeptr;
  int size;
  unsigned int end;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  size = (int)GET_INT_VALUE(nodeptr);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  end = (unsigned int)GET_INT_VALUE(nodeptr);
  
  { UBA uba;
    
    /* User code starts here */
    
    uba = (UBA)malloc(sizeof(struct UnboxedArray));
    uba->block = (unsigned*)malloc(size*sizeof(unsigned));
    uba->free  = size;
    uba->write = 0;
    uba->end   = end;
    /* User code ends here */
    
    nodeptr = mkForeign((void*)uba,(gccval)finaliseUBA);
    C_RETURN(nodeptr);
  }
  ;
}
