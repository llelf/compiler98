#include <string.h>
#include "haskell2c.h"

/* primVector :: Int -> [(Int,a)] -> Vector a */
/* The list and all index must be evaluated before calling cPrimVector */
/* Index out of range is ignored */

extern Node CF_Array_46_95arrayUndefined[];
extern Node CF_Array_46_95arrayMultiple[];

#define UNDEFINED ((Node)CF_Array_46_95arrayUndefined)
#define MULTIPLE ((Node)CF_Array_46_95arrayMultiple)

#ifdef PROFILE
static SInfo nodeProfInfo = { "Builtin","Builtin.primVector","Vector.Vector"};
#endif

C_HEADER(primVector)
{
  int size,i;
  NodePtr res,list;
  NodePtr dstptr;

  res = C_GETARG1(1);
  IND_REMOVE(res);
  size = GET_INT_VALUE(res);

  res = C_ALLOC(1+EXTRA+size);
  res[0] = CONSTRW(size,0);
  INIT_PROFINFO(res,&nodeProfInfo)
  
  dstptr = (NodePtr)&res[1+EXTRA];
  for(i=0; i<size; i++)
    dstptr[i] = UNDEFINED;

  list = C_GETARG1(2);
  IND_REMOVE(list);
  UPDATE_PROFINFO(list)
  while(Cons == GET_CONSTR(list)) {
    NodePtr pair,index;
    Node oldelement;
    pair = GET_POINTER_ARG1(list,1);
    IND_REMOVE(pair);
    UPDATE_PROFINFO(pair)

    index = GET_POINTER_ARG1(pair,1);
    IND_REMOVE(index);
    UPDATE_PROFINFO(index)
    i = GET_INT_VALUE(index);

    if(i >= 0 && i < size) {
      oldelement = dstptr[i];

      if(oldelement == UNDEFINED) {
	NodePtr element = GET_POINTER_ARG1(pair,2);
	IND_REMOVE(element);
	dstptr[i] = (Node)element;
      } else {
	dstptr[i] = MULTIPLE;
      }
    }
    list = GET_POINTER_ARG1(list,2);
    IND_REMOVE(list);
  }
  C_RETURN(res);
}	





