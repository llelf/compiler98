#include "mk.h"

#ifndef CDBGTRANS

NodePtr mkRight(NodePtr a1)
{
  NodePtr n = C_ALLOC(1+EXTRA+1);
  n[0] = CONSTR(1,1,0);
  INIT_PROFINFO(n,&dummyProfInfo)
  n[EXTRA+1] = (Node)a1;
  return n;
}

#else

extern Node D_Prelude_46Right[];
extern Node C0_Prelude_46SR[];

NodePtr mktRight(NodePtr *rt, NodePtr t, NodePtr v, NodePtr vt)
{
  NodePtr np, np2;
  *rt = mkTAp(t, mkCons(D_Prelude_46Right, mkCons(vt, mkTNil())), 
	     C0_Prelude_46SR);
  np = C_ALLOC(1+EXTRA+1);
  np[0] = CONSTR(1,1,0);
  INIT_PROFINFO(np,&dummyProfInfo)
  np[EXTRA+1] = (Node)v;  
  return mkR(np, *rt);
}

#endif
