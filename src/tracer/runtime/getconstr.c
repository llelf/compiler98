#include <stdio.h>
#include <string.h>
#include "haskell2c.h"
#include "bytecode.h"
#include "getconstr.h"

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

/* cEvaluating :: E a -> Bool */
extern void prGraph(NodePtr nodeptr,Int flags,Int d);
char *profName(UInt *p);
NodePtr shortCircuitSelectors(NodePtr node);

NodePtr mkhString(char *s)
{
  NodePtr np;
  char *sp;

  for (sp = s; *sp != '\0'; sp++);
  np = mkNil();
  for (; --sp >= s;)
    np = mkCons(mkChar(*sp), np);
  
  return np;
}


NodePtr mkR(NodePtr v, NodePtr t)
{
  NodePtr n = C_ALLOC(1+EXTRA+2);
  n[0] = CONSTRR(0, 2, 0);
#ifdef PROFILE
  INIT_PROFINFO(n, &dummyProfInfo)
#endif
  n[EXTRA+1] = (Node)v;
  n[EXTRA+2] = (Node)t;
  return n;
}

NodePtr mkTNil()
{
  NodePtr n = C_ALLOC(1+EXTRA);
  n[0] = (Node)mkNil();
#ifdef PROFILE
  INIT_PROFINFO(n, &dummyProfInfo)
#endif
  return n;
}

NodePtr mkTAp(NodePtr t, NodePtr ts, NodePtr sr)
{
  NodePtr n = C_ALLOC(1+EXTRA+3);
  n[0] = CONSTR(TagAp, 3, 0);
#ifdef PROFILE
  INIT_PROFINFO(n, &dummyProfInfo)
#endif
  n[EXTRA+1] = (Node)t;
  n[EXTRA+2] = (Node)ts;
  n[EXTRA+3] = (Node)sr;
  return n;
}

NodePtr mkTNm(NodePtr t, NodePtr nm, NodePtr sr)
{
  NodePtr n = C_ALLOC(1+EXTRA+3);
  n[0] = CONSTR(TagNm, 3, 0);
#ifdef PROFILE
  INIT_PROFINFO(n, &dummyProfInfo)
#endif
  n[EXTRA+1] = (Node)t;
  n[EXTRA+2] = (Node)nm;
  n[EXTRA+3] = (Node)sr;
  return n;
}

NodePtr mkTInd(NodePtr t1, NodePtr t2)
{
  NodePtr n = C_ALLOC(1+EXTRA+2);
  n[0] = CONSTR(TagInd, 2, 0);
#ifdef PROFILE
  INIT_PROFINFO(n, &dummyProfInfo)
#endif
  n[EXTRA+1] = (Node)t1;
  n[EXTRA+2] = (Node)t2;
  return n;
}

NodePtr mkRString(NodePtr sr, NodePtr t, NodePtr str)
{
  NodePtr n, nt, l, c, ch;
  IND_REMOVE(str);
  if (CONINFO_SIZE(*str) > 0) {
      l = mkRString(sr, t, GET_POINTER_ARG1(str, 2));
      ch = GET_POINTER_ARG1(str, 1);
      IND_REMOVE(ch);
      c = mkR(ch, mkTNm(t, mkNmChar(mkChar(GET_CHAR_VALUE(ch))), sr));
      return mkR(mkCons(c, l), 
		 mkTAp(t, mkCons(mkTNm(t, mkNmCons(), sr),
				 mkCons(GET_POINTER_ARG1(c, 2),
					mkCons(GET_POINTER_ARG1(l, 2),
					       mkNil()))), sr));
  } else {
      return mkR(mkNil(), mkTNm(t, mkNmNil(), sr));
  }
}

C_HEADER(stringConst)
{
  NodePtr sr, t, str, res;
  char *sp;

  sr = C_GETARG1(1);
  IND_REMOVE(sr);
  t = C_GETARG1(2);
  IND_REMOVE(t);
  str = C_GETARG1(3);

  C_RETURN(mkRString(sr, t, str));
}

#define PRIM_INT_OP1(fun,op,res,mknm)\
C_HEADER(fun)\
{\
    NodePtr result;\
    NodePtr t = C_GETARG1(1);\
    NodePtr a = C_GETARG1(2);\
    IND_REMOVE(a);\
    a = GET_POINTER_ARG1(a, 1);\
    IND_REMOVE(a);\
    result = res(op(GET_INT_VALUE(a)));\
    C_RETURN(mkR(result, mkTNm(t, mknm(result), mkSR())));\
}

#define PRIM_INT_OP2(fun,op,res,mknm)\
C_HEADER(fun)\
{\
    NodePtr result;\
    NodePtr t = C_GETARG1(1);\
    NodePtr a = C_GETARG1(2);\
    NodePtr b = C_GETARG1(3);\
    IND_REMOVE(a);\
    IND_REMOVE(b);\
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
    NodePtr t = C_GETARG1(1);\
    NodePtr a = C_GETARG1(2);\
    NodePtr b = C_GETARG1(3);\
    extern int terminated;\
    IND_REMOVE(a);\
    IND_REMOVE(b);\
    a = GET_POINTER_ARG1(a, 1);\
    b = GET_POINTER_ARG1(b, 1);\
    IND_REMOVE(a);\
    IND_REMOVE(b);\
    /*fprintf(stderr, "Doing %d %s %d\n", GET_INT_VALUE(a), #op, GET_INT_VALUE(b)); */\
    if (GET_INT_VALUE(b) == 0) {\
	fprintf(stderr, "Division by zero\n");\
	terminated = TRUE;\
	startDbg(t, FALSE);\
	exit(1);\
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

C_HEADER(_tprim_IntSignum)
{
    int v;
    NodePtr result;
    NodePtr t = shortCircuitSelectors(C_GETARG1(1));
    NodePtr a = shortCircuitSelectors(C_GETARG1(2));
    a = shortCircuitSelectors(GET_POINTER_ARG1(a, 1));
    v = GET_INT_VALUE(a);
    result = mkInt(v < 0 ? -1 : 1);
/*    fprintf(stderr, "Now in primIntSignum arg=%d\n", v);*/
    C_RETURN(mkR(result, mkTNm(t, mkNmInt(result), mkSR())));
}

C_HEADER(_tprim_FromInteger)
{
    /*prGraph(C_GETARG1(1), 3, 3);*/
  fprintf(stderr, "_fromInteger not yet implemented.\n");
  exit(1);
}

#ifdef PROFILE
static SInfo FromEnumProfInfo = { "Builtin","Builtin.prim_fromEnum","Prelude._fromEnum"};
#endif

C_HEADER(_tprim_FromEnum)
{
    NodePtr result;
    
    NodePtr t = C_GETARG1(1);
    NodePtr a = C_GETARG1(2);

    /* fprintf(stderr, "prim_fromEnum called\n");*/
    IND_REMOVE(a);
    a = GET_POINTER_ARG1(a, 1);
    IND_REMOVE(a);
    result = C_ALLOC(SIZE_INT);
    MK_INT(result, CONINFO_NUMBER(*a));
    C_RETURN(mkR(result, mkTNm(t, mkNmInt(result), mkSR())));
}

int fromEnumC (NodePtr e)
{
    int result; NodePtr a;
    a = GET_POINTER_ARG1(e, 1);
    result = CONINFO_NUMBER(*a);
    fprintf(stderr, "_fromEnumC %d\n",result);
    return result;
}

#ifdef PROFILE
static SInfo ToEnumProfInfo = { "Builtin","Builtin.prim_toEnum","Prelude._toEnum"};
#endif

C_HEADER(_tprim_ToEnum)
{
    NodePtr result;
    
    NodePtr t = C_GETARG1(1);
    NodePtr a = C_GETARG1(2);
    int i;

    /* fprintf(stderr, "prim_toEnum called\n");*/
    IND_REMOVE(a);
    a = GET_POINTER_ARG1(a, 1);
    IND_REMOVE(a);
    i = GET_INT_VALUE(a);
    result = C_ALLOC(1+EXTRA);
    result[0] = CONSTR(i,0,0);
    C_RETURN(mkR(result, mkTNm(t, mkNm(i), mkSR())));
}

NodePtr toEnumC (int i)
{
    NodePtr result;
    fprintf(stderr, "_toEnumC %d\n",i);
    result = C_ALLOC(1+EXTRA);
    result[0] = CONSTR(i,0,0);
    return result;
}

#ifdef PROFILE
static SInfo IntFromIntegerProfInfo = { "Builtin","Builtin.primIntFromInteger","Prelude.Int"};
#endif

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

#ifdef PROFILE
static SInfo nodeProfInfo = { "Builtin","Builtin.primIntFromInteger","Prelude.Int"};
#endif

C_HEADER(_tprim_IntFromInteger)
{
  Int tag,size,result;
  NodePtr res, i;
  NodePtr t = C_GETARG1(1);
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
    startDbg(C_GETARG1(1), FALSE); \
    exit(3); \
}

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

C_HEADER(cSeq)
{
  NodePtr nodeptr, np;

  /*fprintf(stderr, "cSeq used\n");*/
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  nodeptr = GET_POINTER_ARG1(nodeptr,1);

  /*prGraph(nodeptr, 3, 3);*/
  C_RETURN(nodeptr);
}

C_HEADER(_tprim_seq)
{
  NodePtr nodeptr;
  extern int reductions, traceBreak;

  nodeptr = C_GETARG1(3);
  IND_REMOVE(nodeptr);
  C_RETURN(nodeptr);
}

C_HEADER(_tprim_packString)
{
  NodePtr sptr, s, ch, res;
  int len = 0, swords;
  char *sp;
  sptr = C_GETARG1(2); /* sptr is a fully evaluated string */
  IND_REMOVE(sptr);
  s = GET_POINTER_ARG1(sptr, 1);	/* select list [v] from (R [v] t) */
  IND_REMOVE(s);
  while (CONINFO_SIZE(*s) == 2) {	/* calculate length of list */
    s = GET_POINTER_ARG1(s, 2);		/* select wrapped tail of list */
    IND_REMOVE(s);
    len++;
    s = GET_POINTER_ARG1(s, 1);		/* select real tail from (R v t) */
    IND_REMOVE(s);
  }
  swords = (1+len+3)/((sizeof(Node)/sizeof(char)));
  res = C_ALLOC(3+EXTRA+1+EXTRA+swords);
  res[0] = CONSTRR(0, 2, 0);
  res[EXTRA+1] = (Node)&res[EXTRA+3];
  res[EXTRA+2] = (Node)mkTNm(C_GETARG1(1), mkNmWithArg(NTCString, (Node*)&res[EXTRA+3]), mkSR());
#ifdef PROFILE
  INIT_PROFINFO(res, &dummyProfInfo)
#endif
  res[EXTRA+3] = CONSTRW(swords, 0);
#ifdef PROFILE
  INIT_PROFINFO(res[3+EXTRA], &dummyProfInfo)
#endif
  sp = (char *)&res[3+EXTRA+1+EXTRA];

  s = GET_POINTER_ARG1(sptr, 1);
  IND_REMOVE(s);
  while (CONINFO_SIZE(*s) == 2) {
    ch = GET_POINTER_ARG1(s, 1);
    IND_REMOVE(ch);
    ch = GET_POINTER_ARG1(ch, 1);
    IND_REMOVE(ch);
    *sp++ = GET_CHAR_VALUE(ch);
    /* fprintf(stderr, "char='%c'\n", GET_CHAR_VALUE(ch)); */
    s = GET_POINTER_ARG1(s, 2);
    IND_REMOVE(s);
    s = GET_POINTER_ARG1(s, 1);
    IND_REMOVE(s);
  }
  *sp = '\0';
  /* fprintf(stderr, "packString: '%s'\n", &res[3+EXTRA+1+EXTRA]);*/
  C_RETURN(res);
}

C_HEADER(_tprim_unpackPS)
{
  NodePtr src, res, rp, t;
  char *sp;
  int len = 0;  char c;
  t   = C_GETARG1(1);		/* t is the trail for (unpackPS p) */
  src = C_GETARG1(2);		/* src is a wrapped PackedString */
  IND_REMOVE(src);
  src = GET_POINTER_ARG1(src, 1);	/* select v from (R v t) */
  IND_REMOVE(src);
  sp = (char *)&src[3+EXTRA+1+EXTRA];
  res = mkhString(sp);		/* build ordinary [Char] */
#if 0
  c = *sp++;
  res = mkCons(mkChar(c),0);
  rp = &res[EXTRA+2];
  while (*sp!='\0') {		/* build ordinary [Char] */
    c = *sp++;
    *rp = (Node)mkCons(mkChar(c),0);
    rp = &((NodePtr)(*rp))[EXTRA+2];
  }
  *rp = (Node)mkNil();
#endif

  res = mkRString(0,t,res);	/* then wrap it at the end */
  C_RETURN(res);
}

NodePtr mkNmWithArg(int tag, NodePtr x)
{
    NodePtr n = C_ALLOC(1+EXTRA+1);
    n[0] = CONSTR(tag, 1, 0);
#ifdef PROFILE
    INIT_PROFINFO(n, &dummyProfInfo)
#endif
    n[EXTRA+1] = (Node)x;
    return n;
}

NodePtr mkNm(int tag)
{
    NodePtr n = C_ALLOC(1+EXTRA);
    n[0] = CONSTR(tag, 0, 0);
#ifdef PROFILE
    INIT_PROFINFO(n, &dummyProfInfo)
#endif
    return n;
}

#if 0
C_HEADER(cGetConstr)
{
  NodePtr nodeptr, np;
  Info *info;
  int i;
  static char str[16];
  char *sp;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
#if 0
  fprintf(stderr, "getConstr\n");
  prGraph(nodeptr, 1, 1); fprintf(stderr, "\n");
#endif
 
#if 0
  nodeptr = GET_POINTER_ARG1(nodeptr,1);
  IND_REMOVE(nodeptr);
#endif

  if((GET_TAG(nodeptr) == CON_TAG)) {
    info = GET_INFO(nodeptr);
    if (strcmp(info->sinfo->constructor, "Prelude.Int") == 0) {
      int n = GET_INT_VALUE(nodeptr);
      sprintf(str, "%d", n);
      np = mkhString(str);
    } else if (strcmp(info->sinfo->constructor, "Prelude.Char") == 0) {
      char ch = GET_CHAR_VALUE(nodeptr);
      str[0] = str[2] = '\'';
      str[3] = '\0';
      str[2] = ch;
      np = mkhString(str);
    } else if (strcmp(info->sinfo->constructor, "Prelude.2") == 0) {
      np = mkhString(",");
    } else {
      np = mkhString(info->sinfo->constructor);
    }
    C_RETURN(np);
#if 0
    fprintf(stderr, "size:\t%d\npsize:\t%d\ncnr:\t%d\n", 
	    CONINFO_SIZE(*(int*)nodeptr), CONINFO_PSIZE(*(int*)nodeptr), CONINFO_NUMBER(*(int*)nodeptr));
    fprintf(stderr, "module:\t\t%s\nproducer:\t%s\nconstructor:\t%s\n", 
	    info->sinfo->module, info->sinfo->producer, info->sinfo->constructor);
    nodeptr = mkTrue();
#endif
  } else {
    fprintf(stderr, "cGetConstr: node not a constructor (tag=%d)\n", GET_TAG(nodeptr));
    exit(1);
  }
}

extern char PC_Prelude__46Char[];
extern char PC_Prelude__46Int[];

C_HEADER(cGetConstrNm)
{
  NodePtr nodeptr;
  Info *info;
  char *constr;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
#if 0
  fprintf(stderr, "getConstrNm\n");
  prGraph(nodeptr, 1, 1); fprintf(stderr, "\n");
#endif
 
  if((GET_TAG(nodeptr) == CON_TAG)) {
    info = GET_INFO(nodeptr);
    if (!info->sinfo || ((constr = info->sinfo->constructor) == PC_Prelude__46Int)) {   
	C_RETURN(mkNmInt(GET_INT_VALUE(nodeptr)));
    } else if (constr == PC_Prelude__46Char) {   
	C_RETURN(mkNmChar(GET_CHAR_VALUE(nodeptr)));
    } else if (strcmp(constr, "Prelude.2") == 0) {
	C_RETURN(mkNmTuple());
    } else {
	/*fprintf(stderr, "constr = %s\n", info->sinfo->constructor);*/
	/*prGraph(nodeptr, 3, 3); fprintf(stderr, "\n");*/
	C_RETURN(mkNmFun((int)info->sinfo->constructor));
    }
  } else {
    fprintf(stderr, "cGetConstrNm: node not a constructor (tag=%d)\n", GET_TAG(nodeptr));
    exit(1);
  }
}

C_HEADER(cGetFunNm)
{
  NodePtr nodeptr;
  Info *info;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  {
  Cinfo cinfo = GET_CINFO(nodeptr);
  Finfo finfo = CINFO_FINFO(cinfo);
  UInt *constptr = FINFO_CONST(finfo);
  
#if 0
  fprintf(stderr, "getFunNm <%s>\n", profName(constptr));
  prGraph(nodeptr, 1, 1); fprintf(stderr, "\n");
#endif
  C_RETURN(mkNmFun((int)profName(constptr)));
  }
}

C_HEADER(cContains)
{
  NodePtr nodeptr, np;
  Info *info;
  int i, size;

  /*fprintf(stderr, "Entering contain...\n"); */
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  nodeptr = GET_POINTER_ARG1(nodeptr,1);
  IND_REMOVE(nodeptr);

  if((GET_TAG(nodeptr) == CON_TAG)) {
    info = GET_INFO(nodeptr);
    
    /*prGraph(nodeptr, 1, 1); fprintf(stderr, "\n");*/
    size = CONINFO_SIZE(*(int*)nodeptr);
#if 0
    fprintf(stderr, "size:\t%d\npsize:\t%d\ncnr:\t%d\n", 
	    CONINFO_SIZE(*(int*)nodeptr), 
	    CONINFO_PSIZE(*(int*)nodeptr), 
	    CONINFO_NUMBER(*(int*)nodeptr));
    fprintf(stderr, "module:\t\t%s\nproducer:\t%s\nconstructor:\t%s\n", 
	    info->sinfo->module, info->sinfo->producer, info->sinfo->constructor);
#endif
    np = mkNil();
    for(i = size; i > 0; i--) {
	    np = mkCons(GET_POINTER_ARG1(nodeptr, i), np);
	    /*fprintf(stderr, "Got element %d\n", size - i + 1);*/
    }

    C_RETURN(np);
  } else {
    fprintf(stderr, "cGetConstr: node not a constructor (tag=%d)\n", GET_TAG(nodeptr));
    exit(1);
  }
}
#endif

void _tprim_error()
{
    char s[] = "x";
    NodePtr nodeptr, t;
    extern int terminated;

    fprintf(stderr, "Error: ");
    nodeptr = C_GETARG1(1);
    IND_REMOVE(nodeptr);

    t = C_GETARG1(2);
    IND_REMOVE(t);
    t = GET_POINTER_ARG1(t,1);
    IND_REMOVE(t);
    while (CONINFO_SIZE(*t) > 0) {
	NodePtr c = GET_POINTER_ARG1(t,1);
        IND_REMOVE(c);
	c = GET_POINTER_ARG1(c,1);
	IND_REMOVE(c);
	s[0] = GET_CHAR_VALUE(c);
	fprintf(stderr, s);
	t = GET_POINTER_ARG1(t,2);
        IND_REMOVE(t);
	t = GET_POINTER_ARG1(t,1);
	IND_REMOVE(t);
    }
    fprintf(stderr, "\n");
    terminated = TRUE;
    startDbg(nodeptr, FALSE);
    exit(0);
}

extern int cTrusted(NodePtr t, NodePtr tf);

C_HEADER(cPointerEquality)
{
  NodePtr nodeptr1, nodeptr2;

  nodeptr1 = C_GETARG1(1);
  IND_REMOVE(nodeptr1);
  nodeptr1 = GET_POINTER_ARG1(nodeptr1,1);
  IND_REMOVE(nodeptr1);
  /*prGraph(nodeptr1, 0, 1); fprintf(stderr, "\n");
  printf("n1 = 0x%x\n", nodeptr1);*/

  nodeptr2 = C_GETARG1(2);
  IND_REMOVE(nodeptr2);
  nodeptr2 = GET_POINTER_ARG1(nodeptr2,1);
  IND_REMOVE(nodeptr2);
  /*prGraph(nodeptr2, 0, 1); fprintf(stderr, "\n");
  printf("n2 = 0x%x\n", nodeptr2);*/

  C_RETURN(mkBool((nodeptr1 == nodeptr2) || cTrusted(nodeptr1, nodeptr2)));
}

extern Node FN_Main_46con[];

#define SELECTOR_INS  1
#define SELECTOR_ARG  3

NodePtr
shortCircuitSelectors(NodePtr node)
{
    NodePtr nodeptr, np, app, arg, start;
    Cinfo cinfo;
    Finfo finfo;
    int tag, pos, size;

#ifdef DEBUG_SCS
    fprintf(stderr, "scs: enter\nscs: ");
    prGraph(node, 0, 1); fprintf(stderr, "\n");
#endif /* DEBUG_SCS */
    IND_REMOVE(node);
    start = node;
    while (((tag = GET_TAG(node)) == VAP_TAG0) || (tag == VAP_TAG1)) {
	if (ZAPPED(node))
	    return node;
#ifdef DEBUG_SCS
	fprintf(stderr, "scs: found a vap\n");
#endif /* DEBUG_SCS */
	/* The node is a VAP, check if it's a selector */
	cinfo = GET_CINFO(node);
	size = (int)CINFO_SIZE(cinfo);
	finfo = CINFO_FINFO(cinfo);
#ifdef DEBUG_SCS
	fprintf(stderr, "scs: need=%d instr=%d\n", CINFO_NEED(cinfo), (FINFO_CODE(finfo))[SELECTOR_INS]);
#endif /* DEBUG_SCS */
	if(!CINFO_NEED(cinfo) &&                                  /* Fully saturated */
	   ((FINFO_CODE(finfo))[SELECTOR_INS] == SELECTOR_EVAL))
	{  /* Selector */
#ifdef DEBUG_SCS
	    fprintf(stderr, "scs: it was a saturated selector\n");
#endif /* DEBUG_SCS */
	    app = node;
	    arg = GET_POINTER_ARG1(app, 1);
#ifdef DEBUG_SCS
	    fprintf(stderr, "scs: recursive call\n");
#endif /* DEBUG_SCS */
	    arg = shortCircuitSelectors(arg);
	    if (GET_TAG(arg) == CON_TAG) {
#ifdef DEBUG_SCS
		fprintf(stderr, "scs: found a con, now pick out the selected component\n");
#endif /* DEBUG_SCS */
		pos = FINFO_CODE(GET_FINFO(app))[SELECTOR_ARG];
		node = GET_POINTER_ARG1(arg,pos);  /* Get part .. */
		IND_REMOVE(node);
	    }  else {
#ifdef DEBUG_SCS
		fprintf(stderr, "scs: it was not a con\n");		
		fprintf(stderr, "scs: returning the original node\nscs: ");		
		prGraph(arg, 0, 1); fprintf(stderr, "\n");
#endif /* DEBUG_SCS */
		return start;
	    }
	} else {
#ifdef DEBUG_SCS
	    fprintf(stderr, "scs: it was not a saturated selector\n");		
	    fprintf(stderr, "scs: returning the original node\nscs: ");		
	    prGraph(node, 0, 1); fprintf(stderr, "\n");
#endif /* DEBUG_SCS */
	    return start;	
	}
    }
    return node;
}

extern Node C0_Prelude_46_95Evaluating[];
extern Node C0_Prelude_46_95Evaluated[];
extern Node C0_Prelude_46_95Closure[];

#define mkEvaluating() ((NodePtr)C0_Prelude_46_95Evaluating)
#define mkEvaluated() ((NodePtr)C0_Prelude_46_95Evaluated)
#define mkClosure() ((NodePtr)C0_Prelude_46_95Closure)

C_HEADER(cCheckEvaluation)
{
  NodePtr nodeptr, np, app, arg;
  int i, size, pos;
  int tag;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  nodeptr = GET_POINTER_ARG1(nodeptr,1);
  IND_REMOVE(nodeptr);

  nodeptr = shortCircuitSelectors(nodeptr);

  if (ZAPPED(nodeptr)) {
      C_RETURN(mkEvaluating());
  } else if ((GET_TAG(nodeptr) == CON_TAG)) {
      C_RETURN(mkEvaluated());
  } else
      C_RETURN(mkClosure());
}

char *profName(UInt *p)
{
#if PROFILE
  return (char *)(p[-1]);
#else
  return "";
#endif
}
    
