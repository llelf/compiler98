#include <stdio.h>
#include <string.h>
#include "haskell2c.h"
#include "bytecode.h"
#include "getconstr.h"
#include "fileformat.h"

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif


#define PRIM_INT_OP1(fun,op,res,mknm)\
C_HEADER(fun)\
{\
    NodePtr result;\
    CTrace* t; NodePtr nt, a;\
    nt = C_GETARG1(1);\
    a = C_GETARG1(2);\
    IND_REMOVE(nt);\
    IND_REMOVE(a);\
    t = (CTrace*)nt;\
    a = GET_POINTER_ARG1(a, 1);\
    IND_REMOVE(a);\
    result = res(op(GET_INT_VALUE(a)));\
    C_RETURN(mkR(result, mkTNm(t, mknm(result), mkSR())));\
}

#define PRIM_INT_OP2(fun,op,res,mknm)\
C_HEADER(fun)\
{\
    NodePtr result;\
    CTrace* t; NodePtr nt,a,b;\
    nt = C_GETARG1(1);\
    a = C_GETARG1(2);\
    b = C_GETARG1(3);\
    IND_REMOVE(nt);\
    IND_REMOVE(a);\
    IND_REMOVE(b);\
    t = (CTrace*)nt;\
    a = GET_POINTER_ARG1(a, 1);\
    b = GET_POINTER_ARG1(b, 1);\
    IND_REMOVE(a);\
    IND_REMOVE(b);\
    /* fprintf(stderr, "%d %s %d = %d\n", */\
    /*           GET_INT_VALUE(a), #op, GET_INT_VALUE(b), */ \
    /*	    GET_INT_VALUE(a) op GET_INT_VALUE(b)); */ \
    result = res(GET_INT_VALUE(a) op GET_INT_VALUE(b));\
    C_RETURN(mkR(result, mkTNm(t, mknm(result), mkSR())));\
}

#define PRIM_INT_OP2_NZ(fun,op,res,mknm)\
C_HEADER(fun)\
{\
    NodePtr result;\
    CTrace* t; NodePtr nt,a,b;\
    nt = C_GETARG1(1);\
    a = C_GETARG1(2);\
    b = C_GETARG1(3);\
    IND_REMOVE(nt);\
    IND_REMOVE(a);\
    IND_REMOVE(b);\
    t = (CTrace*)nt;\
    a = GET_POINTER_ARG1(a, 1);\
    b = GET_POINTER_ARG1(b, 1);\
    IND_REMOVE(a);\
    IND_REMOVE(b);\
    /*fprintf(stderr, "Doing %d %s %d\n", GET_INT_VALUE(a), #op, GET_INT_VALUE(b)); */\
    if (GET_INT_VALUE(b) == 0) {\
        hat_exit("Division by zero.", t, 5);\
    }\
    result = res(GET_INT_VALUE(a) op GET_INT_VALUE(b));\
    C_RETURN(mkR(result, mkTNm(t, mknm(result), mkSR())));\
}

/* Eq Int */
PRIM_INT_OP2(_tprim_IntEq,==,mkBool,mkNmBool)
PRIM_INT_OP2(_tprim_IntNEq,!=,mkBool,mkNmBool)

/* Num Int */
PRIM_INT_OP2(_tprim_IntPlus,+,mkInt,mkNmInt)
PRIM_INT_OP2(_tprim_IntMinus,-,mkInt,mkNmInt)
PRIM_INT_OP2(_tprim_IntTimes,*,mkInt,mkNmInt)
/*PRIM_INT_OP1(primIntSignum,abs,mkInt)*/
PRIM_INT_OP1(_tprim_IntAbs,abs,mkInt,mkNmInt)
PRIM_INT_OP1(_tprim_IntNegate,-,mkInt,mkNmInt)

/* Integral Int */
PRIM_INT_OP2_NZ(_tprim_IntQuot,/,mkInt,mkNmInt)
PRIM_INT_OP2_NZ(_tprim_IntRem,%,mkInt,mkNmInt)

/* Ord Int */
PRIM_INT_OP2(_tprim_IntLT,<,mkBool,mkNmBool)
PRIM_INT_OP2(_tprim_IntLE,<=,mkBool,mkNmBool)
PRIM_INT_OP2(_tprim_IntGE,>=,mkBool,mkNmBool)
PRIM_INT_OP2(_tprim_IntGT,>,mkBool,mkNmBool)

/* Enum Char */
PRIM_INT_OP1(_tprim_CharToEnum,,mkChar,mkNmChar)
PRIM_INT_OP1(_tprim_CharFromEnum,,mkInt,mkNmInt)

#if 0
/* An older, incorrect, implementation of Integers */
/* Num Integer -- Not correct!!! */
PRIM_INT_OP2(_tprim_IntegerLt,<,mkBool,mkNmBool)
PRIM_INT_OP2(_tprim_IntegerPlus,+,mkInt,mkNmInt)
PRIM_INT_OP2(_tprim_IntegerMinus,-,mkInt,mkNmInt)
PRIM_INT_OP2(_tprim_IntegerTimes,*,mkInt,mkNmInt)
PRIM_INT_OP1(_tprim_IntegerNegate,-,mkInt,mkNmInt)
PRIM_INT_OP2(_tprim_IntegerGt,>,mkBool,mkNmBool)
PRIM_INT_OP2(_tprim_IntegerGe,>=,mkBool,mkNmBool)
PRIM_INT_OP2(_tprim_IntegerLe,<=,mkBool,mkNmBool)
/*PRIM_NOT_IMPLEMENTED(_tprim_IntegerQuotRem)*/
PRIM_INT_OP2(_tprim_NEqInteger,!=,mkBool,mkNmBool)
PRIM_INT_OP2(_tprim_EqInteger,==,mkBool,mkNmBool)
#endif

C_HEADER(_tprim_IntSignum)
{
    int v;
    NodePtr result;
    CTrace* t = (CTrace*)C_GETARG1(1);
    NodePtr a = shortCircuitSelectors(C_GETARG1(2));
    a = shortCircuitSelectors(GET_POINTER_ARG1(a, 1));
    v = GET_INT_VALUE(a);
    result = mkInt(v < 0 ? -1 : 1);
    /*fprintf(stderr, "Now in primIntSignum arg=%d\n", v);*/
    C_RETURN(mkR(result, mkTNm(t, mkNmInt(result), mkSR())));
}

#if 0
/* Never used */
C_HEADER(_tprim_FromInteger)
{
    /*prGraph(C_GETARG1(1), 3, 3);*/
  fprintf(stderr, "_fromInteger not yet implemented.\n");
  exit(1);
}
#endif

#if 0
#ifdef PROFILE
static SInfo IntFromIntegerProfInfo = { "Builtin","Builtin.primIntFromInteger","Prelude.Int"};
#endif

/* No longer used */
C_HEADER(_tprim_xxxIntFromInteger)
{
  Int tag,size,i;
  NodePtr nodeptr,res;
  /*fprintf(stderr, "primIntFromInteger\n");*/
  C_CHECK(SIZE_INT);
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  tag = *nodeptr;
  size = CONINFO_LARGESIZES(tag);
  if(!size) {
    res = GET_INT(0);
  } else {
    res = C_ALLOC(SIZE_INT);
    fprintf(stderr, "intFromInteger: %d\n", GET_INT_VALUE(nodeptr));
    MK_INT(res,GET_INT_VALUE(nodeptr));
#ifdef PROFILE
    INIT_PROFINFO(res,&IntFromIntegerProfInfo)
#endif
  }
  C_RETURN(res);
}
#endif

#ifdef PROFILE
static SInfo nodeProfInfo = { "Builtin","Builtin.primIntFromInteger","Prelude.Int"};
#endif

/* This is the real one. */
C_HEADER(_tprim_IntFromInteger)
{
  Int tag,size,result;
  NodePtr res, i;
  CTrace* t = (CTrace*)C_GETARG1(1);
  NodePtr nodeptr = C_GETARG1(2);

  IND_REMOVE(nodeptr);
  nodeptr = GET_POINTER_ARG1(nodeptr, 1);
  IND_REMOVE(nodeptr);
  C_CHECK(SIZE_INT);
  
  tag = *nodeptr;
  size = CONINFO_LARGESIZES(tag);
  result = size ? GET_INT_VALUE(nodeptr) : 0;
      
  i = mkInt(result);
  C_RETURN(mkR(i, mkTNm(t, mkNmInt(i), mkSR())));
}

#define PRIM_NOT_IMPLEMENTED(fun)\
C_HEADER(_tprim_##fun)\
{\
    fprintf(stderr, "%s: not yet implemented\n", #fun); \
    /*startDbg(C_GETARG1(1), FALSE);*/ \
    exit(3); \
}

#if 0
PRIM_NOT_IMPLEMENTED(DoubleFromInteger)
PRIM_NOT_IMPLEMENTED(DoubleSignum)
PRIM_NOT_IMPLEMENTED(DoubleAbs)
PRIM_NOT_IMPLEMENTED(DoubleNegate)
PRIM_NOT_IMPLEMENTED(DoubleTimes)
PRIM_NOT_IMPLEMENTED(DoubleMinus)
PRIM_NOT_IMPLEMENTED(DoublePlus)
PRIM_NOT_IMPLEMENTED(FloatFromInteger)
PRIM_NOT_IMPLEMENTED(FloatSignum)
PRIM_NOT_IMPLEMENTED(FloatAbs)
PRIM_NOT_IMPLEMENTED(FloatNegate)
PRIM_NOT_IMPLEMENTED(FloatTimes)
PRIM_NOT_IMPLEMENTED(FloatMinus)
PRIM_NOT_IMPLEMENTED(FloatPlus)
PRIM_NOT_IMPLEMENTED(FractionalDoubleDivide)
PRIM_NOT_IMPLEMENTED(FractionalFloatDivide)
/*PRIM_NOT_IMPLEMENTED(IntegerNegate)*/
/*PRIM_NOT_IMPLEMENTED(IntegerTimes)*/
/*PRIM_NOT_IMPLEMENTED(IntegerMinus)*/
/*PRIM_NOT_IMPLEMENTED(IntegerPlus)*/
/*PRIM_NOT_IMPLEMENTED(IntegerGt)*/
/*PRIM_NOT_IMPLEMENTED(IntegerGe)*/
/*PRIM_NOT_IMPLEMENTED(IntegerLt)*/
/*PRIM_NOT_IMPLEMENTED(IntegerLe)*/
PRIM_NOT_IMPLEMENTED(IntegerQuotRem)
/*PRIM_NOT_IMPLEMENTED(NEqInteger)*/
/*PRIM_NOT_IMPLEMENTED(EqInteger)*/
PRIM_NOT_IMPLEMENTED(NEqFloat)
PRIM_NOT_IMPLEMENTED(EqFloat)
PRIM_NOT_IMPLEMENTED(NEqDouble)
PRIM_NOT_IMPLEMENTED(EqDouble)
PRIM_NOT_IMPLEMENTED(EncodeDouble)
PRIM_NOT_IMPLEMENTED(DecodeDouble)
PRIM_NOT_IMPLEMENTED(EncodeFloat)
PRIM_NOT_IMPLEMENTED(DecodeFloat)
#endif
