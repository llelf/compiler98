#include <string.h>
#include "haskell2c.h"
#include "getconstr.h"



extern Node CF_Array_46_95arrayUndefined[];
extern Node CF_Array_46_95arrayMultiple[];

#define UNDEFINED ((Node)CF_Array_46_95arrayUndefined)
#define MULTIPLE ((Node)CF_Array_46_95arrayMultiple)

#ifdef PROFILE
static SInfo nodeProfInfo = { "Builtin","Builtin.primNewVector","Vector.Vector"};
static SInfo nodeProfInfoCopy = { "Builtin","Builtin.primCopyVector","Vector.Vector"};
#endif

/* _tprim_newVector :: Trace -> R Int -> R (_E a) -> R (Vector a) */
/*                                  actually:     -> R (Vector (R a)) */
C_HEADER(_tprim_newVector)
{
  int i, size;
  NodePtr t, np, res, val;
  NodePtr dstptr;
  t  = C_GETARG1(1);		/* t is the trail for (newVector n x) */

  np = C_GETARG1(2);		/* :: R Int */
  IND_REMOVE(np);
  np = GET_POINTER_ARG1(np,1);	/* select v from (R v t) */
  IND_REMOVE(np);
  size = GET_INT_VALUE(np);	/* :: Int */

  np = C_GETARG1(3);		/* :: R (_E a) */
  IND_REMOVE(np);
  np = GET_POINTER_ARG1(np,1);	/* select v from (R v t) */
  IND_REMOVE(np);
  np = GET_POINTER_ARG1(np,1);	/* select v (:: R a) from (_E v) */
  IND_REMOVE(np);
  val = np;			/* :: R a -- do not unwrap further! */

  /*fprintf(stderr,"newVector: size %d, val %x\n",size,val);*/

  res = C_ALLOC(1+EXTRA+size);
  res[0] = CONSTRP(size,0);
  INIT_PROFINFO(res,&nodeProfInfo)

  dstptr = (NodePtr)&res[1+EXTRA];
  for(i=0; i<size; i++)
    dstptr[i] = (Node)val;

  C_RETURN(mkR(res,mkTNm(t,mkNmVector(),mkSR())));
}

/* _tprim_copyVector :: Trace -> R (Vector a) -> R (Vector a) */
C_HEADER(_tprim_copyVector)
{
  int size,i;
  NodePtr t,res,arg;
  NodePtr srcptr,dstptr;
  t  = C_GETARG1(1);		/* t is the trail for (copyVector v) */
  
  arg = C_GETARG1(2);		/* :: R (Vector a) */
  IND_REMOVE(arg);
  arg = GET_POINTER_ARG1(arg,1);/* select v from (R v t) */
  IND_REMOVE(arg);
  size = CONINFO_LARGESIZES(GET_CONINFO(arg));

  res = C_ALLOC(1+EXTRA+size);
  res[0] = CONSTRP(size,0);
  INIT_PROFINFO(res,&nodeProfInfoCopy)

  /*fprintf(stderr,"copyVector: size %d\n",size);*/

  srcptr = (NodePtr)&arg[1+EXTRA];
  dstptr = (NodePtr)&res[1+EXTRA];
  for(i=0; i<size; i++)
    dstptr[i] = srcptr[i];

  C_RETURN(mkR(res,mkTNm(t,mkNmVector(),mkSR())));
}	


/* _tprim_updateVector :: Trace -> R Int -> R (_E a) -> R (Vector a) -> R () */
C_HEADER(_tprim_updateVector)
{
  int idx,size;
  NodePtr t,val,arg;
  NodePtr dstptr;
  t  = C_GETARG1(1);		/* t is the trail for (updateVector i x v) */

  arg = C_GETARG1(2);		/* :: R Int */
  IND_REMOVE(arg);
  arg = GET_POINTER_ARG1(arg,1);/* select v from (R v t) */
  IND_REMOVE(arg);
  idx = GET_INT_VALUE(arg);

  arg = C_GETARG1(3);		/* :: R (_E a) */
  IND_REMOVE(arg);
  arg = GET_POINTER_ARG1(arg,1);/* select v (:: _E a) from (R v t) */
  IND_REMOVE(arg);
  arg = GET_POINTER_ARG1(arg,1);/* select v (:: R a) from (_E v) */
  IND_REMOVE(arg);		/* :: R a -- do not unwrap further! */
  val = arg;

  arg = C_GETARG1(4);		/* :: R (Vector a) */
  IND_REMOVE(arg);
  arg = GET_POINTER_ARG1(arg,1);/* select v from (R v t) */
  IND_REMOVE(arg);

  dstptr = (NodePtr)&arg[1+EXTRA];

  size = CONINFO_LARGESIZES(GET_CONINFO(arg));

  /*fprintf(stderr,"updateVector: size %d, index %d\n",size,idx);*/

  if (idx<=size) dstptr[idx] = (Node)val;

  C_RETURN(mkR(mkUnit(),mkTNm(t,mkNmUnit(),mkSR())));
}	



#ifdef PROFILE
static SInfo nodeProfInfo = { "Builtin","Builtin.primIndex","Builtin.Vector"};
#endif

/* _tprim_indexVector :: Trace -> R (Vector a) -> R Int -> R a */
/* The index must be evaluated before calling cPrimVector */
/* Index must be in range */
C_HEADER(_tprim_indexVector)
{
  int i;
  NodePtr t, np, res;
  NodePtr dstptr;
  t  = C_GETARG1(1);		/* t is the trail for (indexVector v i) */

  np = C_GETARG1(3);		/* :: R Int */
  IND_REMOVE(np);
  np = GET_POINTER_ARG1(np,1);	/* select v from (R v t) */
  IND_REMOVE(np);
  UPDATE_PROFINFO(np)
  i = GET_INT_VALUE(np);

  /*fprintf(stderr,"indexVector: index %d\n",i);*/

  np = C_GETARG1(2);		/* :: R (Vector a) */
  IND_REMOVE(np);
  np = GET_POINTER_ARG1(np,1);	/* select v from (R v t) */
  IND_REMOVE(np);
  UPDATE_PROFINFO(np)

  dstptr = (NodePtr)&np[1+EXTRA];
				/* Result already R-wrapped when stored */
  res    = (NodePtr)dstptr[i];	/* :: R a */
  IND_REMOVE(res);
  np  = GET_POINTER_ARG1(res,2);/* select t from (R v t) */
  res = GET_POINTER_ARG1(res,1);/* select v from (R v t) */
  IND_REMOVE(res);

  C_RETURN(mkR(res,mkTInd(t,np)));
}	

