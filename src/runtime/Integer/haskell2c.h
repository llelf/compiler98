
#ifndef _HASKELL2C_H
#define _HASKELL2C_H

#include "mutlib.h"
#include "cinterface.h"

#define INTEGER1(haskellName,cName) \
    C_HEADER(haskellName) \
    { \
      NodePtr nodeptr; \
      MP_INT *a; \
      nodeptr = C_GETARG1(1); \
      IND_REMOVE(nodeptr); \
      C_CHECK_STMT(1+(Int)CONINFO_LARGESIZEU(*nodeptr)+EXTRA,nodeptr = C_GETARG1(1);); \
      a = (MP_INT*)nodeptr; \
      nodeptr = C_HP; \
      C_HP = cName ((MP_INT *)nodeptr, a); \
      C_RETURN(nodeptr); \
    }


#define INTEGER2(haskellName,cName,need) \
    C_HEADER(haskellName) \
    { \
      NodePtr nodeptr; \
      MP_INT *u,*v; \
      Int size; \
      nodeptr = C_GETARG1(1); \
      IND_REMOVE(nodeptr); \
      u = (MP_INT*)nodeptr; \
      nodeptr = C_GETARG1(2); \
      IND_REMOVE(nodeptr);  \
      v = (MP_INT*)nodeptr; \
      size = need(u,v); \
      C_CHECK_STMT(size,u = (MP_INT*)C_GETARG1(1);v = (MP_INT*)C_GETARG1(2);); \
      nodeptr = C_HP; \
      C_HP = cName ((MP_INT *)nodeptr, u, v); \
      C_RETURN(nodeptr); \
    }


#define INTEGER2CMP(haskellName,cOp) \
    C_HEADER(haskellName) \
    { \
      NodePtr nodeptr; \
      MP_INT *a,*b; \
      nodeptr = C_GETARG1(1); \
      IND_REMOVE(nodeptr); \
      a = (MP_INT*)nodeptr; \
      nodeptr = C_GETARG1(2); \
      IND_REMOVE(nodeptr);  \
      b = (MP_INT*)nodeptr; \
      C_RETURN(GET_BOOL(cOp)); \
    }


#endif
