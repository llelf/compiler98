#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <setjmp.h>

#include "cinterface.h"
/* #include "node.h"     -- already included in cinterface.h */
/* #include "runtime.h"  -- already included in node.h */
/* #include "bytecode.h" -- already included in node.h via newmacros.h */
#include "mutlib.h"
#include "mark.h"

extern int exit_code;
#ifdef __CYGWIN32__
extern jmp_buf exit_mutator;
#else
extern sigjmp_buf exit_mutator;
#endif

NodePtr  Hp;
NodePtr *Sp;
NodePtr *Fp;
CodePtr  ip;	/*PH*/

#if 0
#define INSTR(x)  fprintf(stderr,"eval: %s\n",x)
#else
#define INSTR(x)
#endif

#if TRACE
NodePtr stopHP;
CodePtr stopIP;
int inscount;
int stopInscount;
#endif


#ifdef PROFILE

static SInfo apply1ProfInfo = { "Runtime","<APPLY>","<APPLY.VAP>"};
static SInfo apply2ProfInfo = { "Runtime","<APPLY>","<APPLY.$>"};
static SInfo apply3ProfInfo = { "Runtime","<APPLY>","<APPLY.CAP>"};
static SInfo evalProfInfo = { "Runtime","<EVAL>","<EVAL.BlackHole>"};
static SInfo int1ProfInfo = { "Runtime","<unary_int_op>","Prelude.Int"};
static SInfo int2ProfInfo = { "Runtime","<binary_int_op>","Prelude.Int"};
static SInfo float1ProfInfo = { "Runtime","<unary_float_op>","Prelude.Float"};
static SInfo float2ProfInfo = { "Runtime","<binary_float_op>","Prelude.Float"};
static SInfo double1ProfInfo = { "Runtime","<unary_double_op>","Prelude.Double"};
static SInfo double2ProfInfo = { "Runtime","<binary_double_op>","Prelude.Double"};
static SInfo fromEnumProfInfo = { "Runtime","<fromEnum>","Prelude.Int"};
static SInfo toEnumProfInfo = { "Runtime","<toEnum>","<Enum>"};
static SInfo string1ProfInfo = { "Runtime","<STRING>","Prelude.Int"};
static SInfo string2ProfInfo = { "Runtime","<STRING>","<String_VAP>"};
static SInfo string3ProfInfo = { "Runtime","<STRING>","Prelude.:"};

#endif

#if TRACE
#define DUMP_NODE(n) prGraph(n,0xff,1); fprintf(stderr,"\n");
#else
#define DUMP_NODE(n) fprintf(stderr," %08x at %08x\n",n[0],n);
#endif

#if PARANOID

#define ASSERT_W(i,n) \
  if(CONSTR_INT != *(n)) { \
    fprintf(stderr,"Ip = %08x: Expected Int but got ",i);   \
    DUMP_NODE(n); \
  }

#define ASSERT_F(i,n) \
  if(CONSTR_FLOAT != *(n)) { \
    fprintf(stderr,"Ip = %08x: Expected Float but got ",i); \
    DUMP_NODE(n); \
  }

#define ASSERT_D(i,n) \
  if(CONSTR_DOUBLE != *(n)) { \
    fprintf(stderr,"Ip = %08x: Expected Double but got ",i); \
    DUMP_NODE(n); \
  }

#else

#define ASSERT_W(i,n)
#define ASSERT_F(i,n)
#define ASSERT_D(i,n)

#endif


void run(NodePtr toplevel)
{
  NodePtr *sp,  *fp,  hp;
  NodePtr vapptr;
  NodePtr nodeptr;
/*CodePtr ip;		-- now global -PH */
  NodePtr *constptr;

#ifdef TPROF	/*PH*/
  int cancel_enter;
  int cancelarr[] = {131,125,126,33};
  int **enterPtr;
  enterPtr = ((int**) malloc(sizeof(int**)));
#endif

  sp = Sp;
  fp = Fp;
  hp = Hp;

#ifdef TPROF
  ip = (CodePtr)(LEAVE+NS);    /* +NS   (DAVID) PH */
#else
  ip = (CodePtr)(LEAVE);    /* +NS   (DAVID) */
#endif
  vapptr = toplevel;

  goto EvalTOS;

  for(;;) {
  NextInst:

#if TRACE
    if(traceSp&1) prStack(sp,fp,vapptr,constptr,traceFlag,traceDepth);
    if(traceIp&1) prByteIns(ip);

    inscount++;
    if(stopInscount && inscount >= stopInscount) {
      fprintf(stderr,"inscount = %08d\n",inscount);
    }

    if(ip == stopIP) {
      fprintf(stderr,"Stop ip = %08x\n",ip);
    }

    if(hp == stopHP) {
      fprintf(stderr,"Stop hp = %08x\n",hp);
    }
#endif
#if INSCOUNT
    if(insCount)
      countIns(ip);
#endif
#ifdef TPROF	/*PH*/
    if (tprof) { /* Checking that we _really_ want to count an enter */
      if (cancel_enter)
      if (*ip==cancelarr[cancel_enter-1])
        cancel_enter++;
      else
        cancel_enter=0;
    }
#endif

    switch (*ip++) {
    case NEEDHEAP_I32: { HEAP_CHECK_VAP(32); } break;
    case NEEDHEAP_P1:  { Int i = *ip++;      HEAP_CHECK_VAP(i); } break;
    case NEEDHEAP_P2:  { Int i = HEAPOFFSET(ip[0]) + (HEAPOFFSET(ip[1])<<8); ip+=2; HEAP_CHECK_VAP(i);} break;
      /* !!! Need stack !!! */
    case NEEDSTACK_I16:  { HEAP_CHECK_VAP(16); } break;
    case NEEDSTACK_P1:  { Int i = *ip++;      HEAP_CHECK_VAP(i); } break;
    case NEEDSTACK_P2: { Int i = HEAPOFFSET(ip[0]) + (HEAPOFFSET(ip[1])<<8); ip+=2; HEAP_CHECK_VAP(i); } break;
      
    case JUMP:  ip += HEAPOFFSET(ip[0]) + (HEAPOFFSET(ip[1])<<8); break;
    case JUMPFALSE:	/* DAVID */
      { nodeptr = *sp++; IND_REMOVE(nodeptr);
	UPDATE_PROFINFO(nodeptr)
	if (GET_BOOL_VALUE(nodeptr) )
	  ip += 2;
	else
	  ip += HEAPOFFSET(ip[0]) + (HEAPOFFSET(ip[1])<<8);
      } break;
    case NOP :
      fprintf(stderr,"Executed NOP at %08x\n",ip);
      break;
#if 0 /* ----------------------------- DAVID -------------- */
    case MATCHCON:
      nodeptr = sp[0];
      IND_REMOVE(nodeptr);
      UPDATE_PROFINFO(nodeptr)
      switch(GET_LARGETAG(nodeptr)) { /* !!! Can be improved when compiler doesn't test tuples !!! */
      case CON_DATA | CON_TAG:
      case CON_CDATA | CON_TAG:
	nodeptr = (NodePtr) GET_CONSTR(nodeptr);	
	break;
      case CON_PTRS | CON_TAG:
      case CON_WORDS | CON_TAG:
	nodeptr = 0;
	break;
      default:
	fprintf(stderr,"Trying to get tag from unevaluated node in MATCHCON at %08x!\n",ip-1);
	fprintf(stderr,"Node is:\n");
	DUMP_NODE(nodeptr);
	exit(-1);
	break;
      }
      break;
    case MATCHINT:
      nodeptr = sp[0];
      IND_REMOVE(nodeptr);
      UPDATE_PROFINFO(nodeptr)
      nodeptr = (NodePtr) GET_INT_VALUE(nodeptr);
      break;
    case PRIMITIVE:
      { Primitive fun;
	int gc = 0;
	ip = (CodePtr) ALIGNPTR(ip);
	fun = *(Primitive*)ip;
	ip += sizeof(Primitive);
	CALL_C(fun);
      } break;
    case JUMPS_T:
      ip = (CodePtr) ALIGNPTR2(ip);
      ip +=  *((JumpItem *)ip + (Int)nodeptr);
      break;
    case JUMPS_L:
      ip = (CodePtr)ALIGNPTR4(ip);
      { JumpTable *t = (JumpTable*) ip;
	JumpTable def = *t++;
	JumpItem len =  def.constr;
	Int c = (Int)nodeptr;
	while (len--) {
	  JumpTable x = *t++;
	  if (c == (Int)((signed short)x.constr)) {
	    ip += x.offset;
	    goto mjump_confound;
	  }
	}
	ip += def.offset;
      }
    mjump_confound:
      break;
#endif        /* -------------------- DAVID ---------------------- */
    case PRIMITIVE:
      { Primitive fun;
	int gc = 0;
	ip = (CodePtr) ALIGNPTR(ip);
	fun = *(Primitive*)ip;
	ip += sizeof(Primitive);
#ifdef TPROF	/*PH*/
        if (tprof)
          tprofEnterGreencard((CodePtr*)FINFO_CODE(GET_FINFO(vapptr)),
                              (char *)constptr[-1]);
        CALL_C(fun);
        if (tprof) tprofExitGreencard();
#else
	CALL_C(fun);
#endif
      } break;


    case ZAP_ARG_I1:   vapptr[EXTRA +1    ] = (Node)ZAP_ARG_NODE;  break;
    case ZAP_ARG_I2:   vapptr[EXTRA +2    ] = (Node)ZAP_ARG_NODE;  break;
    case ZAP_ARG_I3:   vapptr[EXTRA +3    ] = (Node)ZAP_ARG_NODE;  break;
    case ZAP_ARG:      vapptr[EXTRA +HEAPOFFSET(ip[0])] = (Node)ZAP_ARG_NODE; ip+=1; break;
    case ZAP_STACK_P1: sp[HEAPOFFSET(ip[0])           ] = ZAP_STACK_NODE;     ip+=1; break;
    case ZAP_STACK_P2: sp[HEAPOFFSET(ip[0])+(HEAPOFFSET(ip[1])<<8)] = ZAP_STACK_NODE;     ip+=2; break;
      
    case PUSH_CADR_N2: *--sp = (NodePtr)&constptr[-HEAPOFFSET(ip[0])-(HEAPOFFSET(ip[1])<<8)]; ip+=2; break;
    case PUSH_CADR_N1: *--sp = (NodePtr)&constptr[-HEAPOFFSET(ip[0])];            ip+=1; break;
    case PUSH_CADR_P1: *--sp = (NodePtr)&constptr[ HEAPOFFSET(ip[0])];            ip+=1; break;
    case PUSH_CADR_P2: *--sp = (NodePtr)&constptr[ HEAPOFFSET(ip[0])+(HEAPOFFSET(ip[1])<<8)]; ip+=2; break;
    case PUSH_CVAL_N2: *--sp = (NodePtr) constptr[-HEAPOFFSET(ip[0])-(HEAPOFFSET(ip[1])<<8)]; ip+=2; break;
    case PUSH_CVAL_N1: *--sp = (NodePtr) constptr[-HEAPOFFSET(ip[0])];            ip+=1; break;
    case PUSH_CVAL_P1: *--sp = (NodePtr) constptr[ HEAPOFFSET(ip[0])];            ip+=1; break;  
    case PUSH_CVAL_P2: *--sp = (NodePtr) constptr[ HEAPOFFSET(ip[0])+(HEAPOFFSET(ip[1])<<8)]; ip+=2; break;
    case PUSH_INT_N1:  *--sp =  GET_INT(-HEAPOFFSET(ip[0]));            ip+=1; break;
    case PUSH_INT_P1:  *--sp =  GET_INT( HEAPOFFSET(ip[0]));            ip+=1; break;
    case PUSH_CHAR_N1: *--sp =  GET_CHAR(-HEAPOFFSET(ip[0]));           ip+=1; break;
    case PUSH_CHAR_P1: *--sp =  GET_CHAR( HEAPOFFSET(ip[0]));           ip+=1; break;
    case PUSH_ARG_I1:  UPDATE_PROFINFO(vapptr);
                       nodeptr =GET_POINTER_ARG1(vapptr,1);
                       IND_REMOVE(nodeptr); 
                       *--sp = nodeptr; break;
    case PUSH_ARG_I2:  UPDATE_PROFINFO(vapptr);
                       nodeptr =GET_POINTER_ARG1(vapptr,2);
                       IND_REMOVE(nodeptr); 
                       *--sp = nodeptr; break;
    case PUSH_ARG_I3:  UPDATE_PROFINFO(vapptr);
                       nodeptr =GET_POINTER_ARG1(vapptr,3);
                       IND_REMOVE(nodeptr); 
                       *--sp = nodeptr; break;

    case PUSH_ARG:     UPDATE_PROFINFO(vapptr);
                       nodeptr =GET_POINTER_ARG1(vapptr,ip[0]);          ip+=1;
                       IND_REMOVE(nodeptr); 
                       *--sp = nodeptr; break;
    case PUSH_HEAP:    *--sp = hp; break;
    case PUSH_I1:      nodeptr = sp[1];                *--sp = nodeptr;        break;
    case PUSH_P1:      nodeptr = sp[HEAPOFFSET(ip[0])];            *--sp = nodeptr; ip+=1; break;
    case PUSH_P2:      nodeptr = sp[HEAPOFFSET(ip[0])+(HEAPOFFSET(ip[1])<<8)]; *--sp = nodeptr; ip+=2; break;
      
    case POP_I1:       sp += 1;                        break;
    case POP_P1:       sp += HEAPOFFSET(ip[0]);             ip+=1; break;
    case POP_P2:       sp += HEAPOFFSET(ip[0])+(HEAPOFFSET(ip[1])<<8) ; ip+=2; break;
    case SLIDE_P1:     nodeptr = sp[0]; sp += HEAPOFFSET(ip[0]);             sp[0] = nodeptr; ip+=1; break;
    case SLIDE_P2:     nodeptr = sp[0]; sp += HEAPOFFSET(ip[0])+(HEAPOFFSET(ip[1])<<8) ; sp[0] = nodeptr; ip+=2; break;
    case SELECT:
      { Int index = *ip++;
	nodeptr = *sp;
	IND_REMOVE(nodeptr);
	UPDATE_PROFINFO(nodeptr)
	*sp = (NodePtr) GET_POINTER_ARG1(nodeptr,index);
      } break;

    case UNPACK:
#if PARANOID
      { int i;
	nodeptr = sp[0];
	IND_REMOVE(nodeptr);
	switch(GET_LARGETAG(nodeptr)) {
	case CON_DATA | CON_TAG:
	case CON_CDATA | CON_TAG:
	  i = CONINFO_SIZE(GET_CONINFO(nodeptr));
	  break;
	case CON_PTRS | CON_TAG:
	case CON_WORDS | CON_TAG:
	  i = CONINFO_LARGESIZES(GET_CONINFO(nodeptr));
	  break;
	default:
	  fprintf(stderr,"Trying to get tag from unevaluated node in MATCHCON at %08x!\n",ip-1);
	  fprintf(stderr,"Node is:\n");
#if TRACE
	  prGraph(nodeptr,0xff,1);
	  fprintf(stderr,"\n");
#else
	  fprintf(stderr," %08x at %08x\n",nodeptr[0],nodeptr);
#endif
	  exit(-1);
	  break; 
	}
	if(i!=ip[0]) {
	  fprintf(stderr,"Trying to do UNPACK %d on a node with %d arguments at %08x!\n",ip[0],i,ip-1);
	  fprintf(stderr,"Node is:\n");
#if TRACE
	  prGraph(nodeptr,0xff,1);
	  fprintf(stderr,"\n");
#else
	  fprintf(stderr," %08x at %08x\n",nodeptr[0],nodeptr);
#endif
	  exit(-1);
	}
      }
#endif
      { Int arity = *ip++;
	nodeptr = *sp++;
	IND_REMOVE(nodeptr);
	UPDATE_PROFINFO(nodeptr)
	while (arity) {
	  *--sp = (NodePtr) GET_POINTER_ARG1(nodeptr,arity--);
	}
      } break;
      
      
    case APPLY:
    INSTR("apply");
    { int need,size,args = *ip++;
      Cinfo cinfo;
      NodePtr vap;

      nodeptr = *sp++;
      IND_REMOVE(nodeptr);
      UPDATE_PROFINFO(nodeptr)

      cinfo = GET_CINFO(nodeptr);

#ifdef PARANOID
      {
	  int c = (GET_LARGETAG(nodeptr));
	  switch(c) {
	  case CON_DATA | CON_TAG:
	  case CON_CDATA | CON_TAG:
	      fprintf(stderr, "Strange: con in apply:\n");
#if TRACE
	      prGraph(nodeptr, 3, 3);
#endif
	      fprintf(stderr, "\n");
	      /*startDbg(GET_POINTER_ARG1(nodeptr, 2));*/
	      exit(-1);
	  }
       }
#endif /*0*/
#if 1
      if(GET_TAG(nodeptr)&VAP_TAG && !CINFO_NEED(cinfo)) {   /* Probably not needed */
        fprintf(stderr,"VAP in Apply?\n");
        vap = nodeptr;
        goto build_apply;
      }
#endif

      need = CINFO_NEED(cinfo);
      size = CINFO_SIZE(cinfo);
      nodeptr = nodeptr+1+EXTRA;  /* Skip tag (and optional profile info) */
      if(need <= args) {
	INIT_PROFINFO(hp,&apply1ProfInfo)
	vap = hp;
        *hp++ = (Node)((UInt)2*need+(UInt)cinfo)+(UInt)VAP_TAG;
	hp += EXTRA; 
        while(size-->0)
          *hp++ = *nodeptr++;
        args -= need;
        while(need--)
          *hp++ = (Node)*sp++;
      build_apply:
        while(args--) {
	  INIT_PROFINFO(hp,&apply2ProfInfo)
          *hp++ = (Node)(C_VAPTAG(PRIM_APPLY));
	  hp += EXTRA;
          *hp ++ = (Node) vap;
          vap = &hp[-2-EXTRA];
          *hp++ = (Node)*sp++;
        }
      } else { /* need > args */
	INIT_PROFINFO(hp,&apply3ProfInfo)
        vap = hp;
        *hp++ = (Node)(2*(UInt)args+(UInt)VAP_TAG+(UInt)cinfo);
	hp +=EXTRA;
        while(size-->0)
          *hp++ = *nodeptr++;
        while(args-->0)
          *hp++ = (Node)*sp++;
      }
      *--sp = vap;
    } break;

  /* DON'T Fall trough to evaluate TOS. We might wan't to do RETURN_EVAL !!! */

    case SELECTOR_EVAL:   /* == PUSH_ARG 1, EVAL  has it's own opcode to signal that this is a selector function (gc need to know) */
      UPDATE_PROFINFO(vapptr);
      nodeptr =GET_POINTER_ARG1(vapptr,1);
      IND_REMOVE(nodeptr);
      *--sp = nodeptr;
      /* Fall through to EVAL */
    case EVAL:
      INSTR("evalToS");
    EvalTOS:
      { nodeptr = sp[0];
	IND_REMOVE(nodeptr);
	UPDATE_PROFINFO(nodeptr)
	  sp[0] = nodeptr;
	if(GET_TAG(nodeptr) & VAP_TAG && !CINFO_NEED(GET_CINFO(nodeptr))) {
	  if (ZAPPED(nodeptr)) {
#ifdef DBGTRANS
            extern void dbg_blackhole();
	    dbg_blackhole();
#if 0 /*def PROFILE*/
	    prGraph(nodeptr, 15, 15);
#endif
#else
            extern Node CF_DbgIface_46blackhole[];
            extern Node FN_DbgIface_46fatal[];

	    fp = sp = spStart; /* Clear the stack */
	    /* There better be space left now. */

	    MK_VAP2(hp,C_VAPTAG(FN_DbgIface_46fatal),CON_NIL,(Node)CF_DbgIface_46blackhole);
		     /* Build a call to blackhole, in the future use function name */
	    INIT_PROFINFO(hp,&evalProfInfo)
	    *--sp = hp;
	    hp += SIZE_VAP2;

#if 0
	    MK_VAP1(hp,C_VAPTAG(BLACKHOLE),CON_NIL); /* Build a call to blackhole, in the future use function name */
	    INIT_PROFINFO(hp,&evalProfInfo)
	    *--sp = hp;
	    hp += SIZE_VAP1;
#endif
	    if (hp >= (NodePtr)sp) {
	      fprintf(stderr, "Black hole detected and no heap space available.\n");
	      exit(-1);
	    }
	    nodeptr = sp[0];
#endif
	  }
	
	  ZAP(nodeptr);

	  PUSH_STATE;
	  vapptr   = nodeptr;
	  constptr = VAP_CONST(vapptr);
#if TRACE
	  if(traceFlag & TRACE_EVAL) {
	    fprintf(stderr,"<ENTER %s>\n",(char *)constptr[-1]);
	  }
#endif

	  ip       = FINFO_CODE(GET_FINFO(vapptr));
#ifdef TPROF	/*PH*/
          if(tprof) {
            *enterPtr = (int*)(FINFO_ENTERPTR(GET_FINFO(vapptr)));
            tprofRecordEnter((CodePtr*)ip, (char*)constptr[-1], enterPtr);
            cancel_enter=1;
          }
#endif
	}
#ifdef TPROF	/*PH*/
        if (cancel_enter==5) {
          tprofUnrecordEnter();
          cancel_enter=0;
        }
#endif
      } break;

 case RETURN:
      INSTR("return");
#if TRACE
      if(traceFlag & TRACE_RETURN)
	fprintf(stderr,"<RETURN %s>\n",(char *)constptr[-1]);
#endif
    nodeptr = *sp++;
    UPDATE_VAP(nodeptr);
    POP_STATEVP;
    break;
  case RETURN_EVAL:
      INSTR("returneval");
#if TRACE
      if(traceFlag & TRACE_RETURN)
	fprintf(stderr,"<RETURN(e) %s>\n",(char *)constptr[-1]);
#endif
    nodeptr = *sp++;
    UPDATE_VAP(nodeptr);
    POP_STATEVP;
    goto EvalTOS;

#ifdef PROFILE
  case HEAP_CREATE: { BInfo binfo; binfo.all = 0; binfo.parts.created = year;  *hp++ = (Node)binfo.all; break;}
  case HEAP_SPACE:  *hp++ = 0; break;
#endif

  case HEAP_OFF_N2: *hp = (Node) (hp-HEAPOFFSET(ip[0])-(HEAPOFFSET(ip[1])<<8));   hp++; ip+=2; break;
  case HEAP_OFF_N1: *hp = (Node) (hp-HEAPOFFSET(ip[0]));              hp++; ip+=1; break;
  case HEAP_OFF_P1: *hp = (Node) (hp+HEAPOFFSET(ip[0]));              hp++; ip+=1; break;
  case HEAP_OFF_P2: *hp = (Node) (hp+HEAPOFFSET(ip[0])+(HEAPOFFSET(ip[1])<<8));   hp++; ip+=2; break;

  case HEAP_CADR_N2: *hp++ = (Node)&constptr[-HEAPOFFSET(ip[0])-(HEAPOFFSET(ip[1])<<8)]; ip+=2; break;
  case HEAP_CADR_N1: *hp++ = (Node)&constptr[-HEAPOFFSET(ip[0])];            ip+=1; break;
  case HEAP_CADR_P1: *hp++ = (Node)&constptr[ HEAPOFFSET(ip[0])];            ip+=1; break;
  case HEAP_CADR_P2: *hp++ = (Node)&constptr[ HEAPOFFSET(ip[0])+(HEAPOFFSET(ip[1])<<8)]; ip+=2; break;
  case HEAP_CVAL_N2: *hp++ = (Node) constptr[-HEAPOFFSET(ip[0])-(HEAPOFFSET(ip[1])<<8)]; ip+=2; break;
  case HEAP_CVAL_N1: *hp++ = (Node) constptr[-HEAPOFFSET(ip[0])];            ip+=1; break;
  case HEAP_CVAL_IN3:*hp++ = (Node) constptr[    -3];                   break;  
  case HEAP_CVAL_I3: *hp++ = (Node) constptr[     3];                   break;  
  case HEAP_CVAL_I4: *hp++ = (Node) constptr[     4];                   break;  
  case HEAP_CVAL_I5: *hp++ = (Node) constptr[     5];                   break;  
  case HEAP_CVAL_P1: *hp++ = (Node) constptr[ HEAPOFFSET(ip[0])];            ip+=1; break;  
  case HEAP_CVAL_P2: *hp++ = (Node) constptr[ HEAPOFFSET(ip[0])+(HEAPOFFSET(ip[1])<<8)]; ip+=2; break;
  case HEAP_INT_N1:  *hp++ = (Node) GET_INT(-HEAPOFFSET(ip[0])) ;            ip+=1; break;
  case HEAP_INT_P1:  *hp++ = (Node) GET_INT( HEAPOFFSET(ip[0])) ;            ip+=1; break;
  case HEAP_CHAR_N1: *hp++ = (Node) GET_CHAR(-HEAPOFFSET(ip[0])) ;           ip+=1; break;
  case HEAP_CHAR_P1: *hp++ = (Node) GET_CHAR( HEAPOFFSET(ip[0])) ;           ip+=1; break;
  case HEAP_ARG:     UPDATE_PROFINFO(vapptr);
                     *hp++ = (Node) GET_POINTER_ARG1(vapptr,ip[0]);         ip+=1; break;
  case HEAP_I1:      nodeptr = sp[   1 ];            *hp++ = (Node)nodeptr;        break;
  case HEAP_I2:      nodeptr = sp[   2 ];            *hp++ = (Node)nodeptr;        break;
  case HEAP_P1:      nodeptr = sp[HEAPOFFSET(ip[0])];            *hp++ = (Node)nodeptr; ip+=1; break;
  case HEAP_P2:      nodeptr = sp[HEAPOFFSET(ip[0])+(HEAPOFFSET(ip[1])<<8)]; *hp++ = (Node)nodeptr; ip+=1; break;


#define PRIM_OP2_INT(op) \
    { NodePtr nodeptr = *sp++; \
	Int a,b;                 \
	IND_REMOVE(nodeptr); ASSERT_W(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = GET_INT_VALUE(nodeptr); \
	nodeptr = *sp++;         \
	IND_REMOVE(nodeptr); ASSERT_W(ip,nodeptr) UPDATE_PROFINFO(nodeptr) b = GET_INT_VALUE(nodeptr); \
	MK_INT(hp, a op b); \
	INIT_PROFINFO(hp,&int2ProfInfo) \
	*--sp = hp; hp += SIZE_INT; \
    } break

  case ADD_W: PRIM_OP2_INT(+);
  case SUB_W: PRIM_OP2_INT(-);
  case MUL_W: PRIM_OP2_INT(*);
  case QUOT:  PRIM_OP2_INT(/);
  case REM:   PRIM_OP2_INT(%);

 case NEG_W:
    { NodePtr nodeptr = *sp++;
      Int a;
      IND_REMOVE(nodeptr); ASSERT_W(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = GET_INT_VALUE(nodeptr);
      MK_INT(hp, -a); 
      INIT_PROFINFO(hp,&int1ProfInfo) 
      *--sp = hp; hp += SIZE_INT; 
    } break;

 case ABS_W:
    { NodePtr nodeptr = *sp++;
      Int a;
      IND_REMOVE(nodeptr); ASSERT_W(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = GET_INT_VALUE(nodeptr);
      if (a < 0) {
	nodeptr = hp;
        MK_INT(hp, -a); 
	INIT_PROFINFO(hp,&int1ProfInfo) 
	hp += SIZE_INT;
      }
      *--sp = nodeptr;
    } break;

 case SIGNUM_W:
    { NodePtr nodeptr = *sp++;
      Int a;
      IND_REMOVE(nodeptr); ASSERT_W(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = GET_INT_VALUE(nodeptr);
      if (a < 0) {
	nodeptr = GET_INT(-1);
      } else if (a == 0) {
	nodeptr = GET_INT(0);
      } else  {
	nodeptr = GET_INT(1);
      }
      *--sp = nodeptr;
    } break;
      

#define PRIM_CMP2_INT(op) \
  { NodePtr nodeptr = *sp++; \
    Int a,b;                 \
    IND_REMOVE(nodeptr); ASSERT_W(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = GET_INT_VALUE(nodeptr); \
    nodeptr = *sp++;         \
    IND_REMOVE(nodeptr); ASSERT_W(ip,nodeptr) UPDATE_PROFINFO(nodeptr) b = GET_INT_VALUE(nodeptr); \
    *--sp = GET_BOOL(a op b); \
  } break

  case EQ_W: PRIM_CMP2_INT(==);
  case NE_W: PRIM_CMP2_INT(!=);
  case LT_W: PRIM_CMP2_INT(<);
  case LE_W: PRIM_CMP2_INT(<=);
  case GT_W: PRIM_CMP2_INT(>);
  case GE_W: PRIM_CMP2_INT(>=);

#ifndef __alpha /* Float is Double on alpha */
#define PRIM_OP2_FLOAT(op) \
    { NodePtr nodeptr = *sp++; \
	float a,b;                 \
	IND_REMOVE(nodeptr); ASSERT_F(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = get_float_value(nodeptr); \
	nodeptr = *sp++;         \
	IND_REMOVE(nodeptr); ASSERT_F(ip,nodeptr) UPDATE_PROFINFO(nodeptr) b = get_float_value(nodeptr); \
	mk_float(hp, a op b); \
	INIT_PROFINFO(hp,&float2ProfInfo) \
	*--sp = hp; hp += SIZE_FLOAT; \
    } break

  case ADD_F: PRIM_OP2_FLOAT(+);
  case SUB_F: PRIM_OP2_FLOAT(-);
  case MUL_F: PRIM_OP2_FLOAT(*);
  case SLASH_F: PRIM_OP2_FLOAT(/);

#define PRIM_OP1_FLOAT(op) \
    { NodePtr nodeptr = *sp++; \
	float a;                 \
	IND_REMOVE(nodeptr); ASSERT_F(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = get_float_value(nodeptr); \
	mk_float(hp, op); \
	INIT_PROFINFO(hp,&float1ProfInfo) \
	*--sp = hp; hp += SIZE_FLOAT; \
    } break

  case NEG_F: PRIM_OP1_FLOAT(-a);
  case ABS_F: PRIM_OP1_FLOAT((a<0?-a:a));
  case SIGNUM_F: PRIM_OP1_FLOAT((a<0?-1.0:(a==0?0.0:1.0)));
  case EXP_F: PRIM_OP1_FLOAT((float)exp((double)a));
  case LOG_F:PRIM_OP1_FLOAT((float)log((double)a));
  case SQRT_F:PRIM_OP1_FLOAT((float)sqrt((double)a));
  case SIN_F:PRIM_OP1_FLOAT((float)sin((double)a));
  case COS_F:PRIM_OP1_FLOAT((float)cos((double)a));
  case TAN_F:PRIM_OP1_FLOAT((float)tan((double)a));
  case ASIN_F:PRIM_OP1_FLOAT((float)asin((double)a));
  case ACOS_F:PRIM_OP1_FLOAT((float)acos((double)a));
  case ATAN_F:PRIM_OP1_FLOAT((float)atan((double)a));

#define PRIM_CMP2_FLOAT(op) \
  { NodePtr nodeptr = *sp++; \
    float a,b;                 \
    IND_REMOVE(nodeptr); ASSERT_F(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = get_float_value(nodeptr); \
    nodeptr = *sp++;         \
    IND_REMOVE(nodeptr); ASSERT_F(ip,nodeptr) UPDATE_PROFINFO(nodeptr) b = get_float_value(nodeptr); \
    *--sp = GET_BOOL(a op b); \
  } break

  case EQ_F: PRIM_CMP2_FLOAT(==);
  case NE_F: PRIM_CMP2_FLOAT(!=);
  case LT_F: PRIM_CMP2_FLOAT(<);
  case LE_F: PRIM_CMP2_FLOAT(<=);
  case GT_F: PRIM_CMP2_FLOAT(>);
  case GE_F: PRIM_CMP2_FLOAT(>=);

#endif /* __alpha */

#define PRIM_OP2_DOUBLE(op) \
    { NodePtr nodeptr = *sp++; \
	double a,b;                 \
	IND_REMOVE(nodeptr); ASSERT_D(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = get_double_value(nodeptr); \
	nodeptr = *sp++;         \
	IND_REMOVE(nodeptr); ASSERT_D(ip,nodeptr) UPDATE_PROFINFO(nodeptr) b = get_double_value(nodeptr); \
	mk_double(hp, a op b); \
	INIT_PROFINFO(hp,&double2ProfInfo) \
	*--sp = hp; hp += SIZE_DOUBLE; \
    } break

  case ADD_D: PRIM_OP2_DOUBLE(+);
  case SUB_D: PRIM_OP2_DOUBLE(-);
  case MUL_D: PRIM_OP2_DOUBLE(*);
  case SLASH_D: PRIM_OP2_DOUBLE(/);

#define PRIM_OP1_DOUBLE(op) \
    { NodePtr nodeptr = *sp++; \
	double a;                 \
	IND_REMOVE(nodeptr); ASSERT_D(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = get_double_value(nodeptr); \
	mk_double(hp, op); \
	INIT_PROFINFO(hp,&double1ProfInfo) \
	*--sp = hp; hp += SIZE_DOUBLE; \
    } break

  case NEG_D: PRIM_OP1_DOUBLE(-a);
  case ABS_D: PRIM_OP1_DOUBLE((a<0?-a:a));
  case SIGNUM_D: PRIM_OP1_DOUBLE((a<0?-1.0:(a==0?0.0:1.0)));
  case EXP_D: PRIM_OP1_DOUBLE(exp(a));
  case LOG_D:PRIM_OP1_DOUBLE(log(a));
  case SQRT_D:PRIM_OP1_DOUBLE(sqrt(a));
  case SIN_D:PRIM_OP1_DOUBLE(sin(a));
  case COS_D:PRIM_OP1_DOUBLE(cos(a));
  case TAN_D:PRIM_OP1_DOUBLE(tan(a));
  case ASIN_D:PRIM_OP1_DOUBLE(asin(a));
  case ACOS_D:PRIM_OP1_DOUBLE(acos(a));
  case ATAN_D:PRIM_OP1_DOUBLE(atan(a));

#define PRIM_CMP2_DOUBLE(op) \
  { NodePtr nodeptr = *sp++; \
    double a,b;                 \
    IND_REMOVE(nodeptr); ASSERT_D(ip,nodeptr) UPDATE_PROFINFO(nodeptr) a = get_double_value(nodeptr); \
    nodeptr = *sp++;         \
    IND_REMOVE(nodeptr); ASSERT_D(ip,nodeptr) UPDATE_PROFINFO(nodeptr) b = get_double_value(nodeptr); \
    *--sp = GET_BOOL(a op b); \
  } break

  case EQ_D: PRIM_CMP2_DOUBLE(==);
  case NE_D: PRIM_CMP2_DOUBLE(!=);
  case LT_D: PRIM_CMP2_DOUBLE(<);
  case LE_D: PRIM_CMP2_DOUBLE(<=);
  case GT_D: PRIM_CMP2_DOUBLE(>);
  case GE_D: PRIM_CMP2_DOUBLE(>=);

  case ORD:
    { UInt tag;
      nodeptr = sp[0];
      IND_REMOVE(nodeptr);
      UPDATE_PROFINFO(nodeptr)
      tag = (UInt)GET_CONSTR(nodeptr);
      MK_INT(hp,tag);
      INIT_PROFINFO(hp,&fromEnumProfInfo)
      sp[0] = hp;
      hp+= SIZE_INT;
    } break;
 case CHR:
  { UInt tag;
    nodeptr = sp[0];
    IND_REMOVE(nodeptr);
    UPDATE_PROFINFO(nodeptr)
    tag = (UInt)GET_INT_VALUE(nodeptr);
    MK_ENUM(hp,tag);
    INIT_PROFINFO(hp,&toEnumProfInfo)
    sp[0] = hp;
    hp+= SIZE_ENUM;
  } break;


  case STRING:
   { char *str;
     nodeptr = *sp++;
     IND_REMOVE(nodeptr);
     UPDATE_PROFINFO(nodeptr)
     str = (char *)GET_INT_VALUE(nodeptr);
     if (*str) {
       if(*str == '\\')
	 str++;
       MK_INT(hp,(UInt)(str+1));
       INIT_PROFINFO(hp,&string1ProfInfo)
       nodeptr = hp;
       hp += SIZE_INT;
       
       MK_VAP1(hp
              ,(Node)(C_VAPTAG(PRIM_STRING))
              ,(Node)nodeptr);
       INIT_PROFINFO(hp,&string2ProfInfo)
       nodeptr = hp;
       hp += SIZE_VAP1;

       MK_CONS(hp
              ,(Node)GET_INT(0xff&(Int)(*str))
              ,(Node)nodeptr);
       INIT_PROFINFO(hp,&string3ProfInfo)

       nodeptr = hp;
       hp += SIZE_CONS;

     } else {
       nodeptr = GET_NIL();
     }
     *--sp = nodeptr;
   } break;

    case HGETC:
      { 
	int c;
	Arg *a;

	nodeptr = *sp;
	IND_REMOVE(nodeptr);
	UPDATE_PROFINFO(nodeptr)
	a = cdataArg((CData *)(GET_INT_VALUE(nodeptr)));
#ifdef PROFILE
	if(replay) {
	  if(255==(c=getc(inputFILE)))
	    if(0==(c=getc(inputFILE))) c = -1;
	} else
#endif
	  c = getc(a->fp);
#ifdef PROFILE
	if(record) {
	  if(c==EOF) {
	    putc(255,inputFILE);
	    putc(0,inputFILE);
	  } if (c==255) {
	    putc(255,inputFILE);
	    putc(255,inputFILE);
	  } else
	    putc(c,inputFILE);
	}
#endif
	*sp = GET_CHAR(c);  /* Note EOF == -1 == negative character,      */
			     /* but it's OK characters are ints anyway,   */
                             /* and the table includes -1.                */
      } break;
    case HPUTC:
      {
	char c;
	Arg *a;

	nodeptr = *sp++;
	IND_REMOVE(nodeptr);
	UPDATE_PROFINFO(nodeptr)
	a = cdataArg((CData *)GET_INT_VALUE(nodeptr));
	nodeptr = *sp;
        IND_REMOVE(nodeptr);
        UPDATE_PROFINFO(nodeptr)
        c = GET_CHAR_VALUE(nodeptr);
#ifdef PROFILE
        if(!replay)
#endif
	  putc(c,a->fp);
	*sp = HPUTC_OK;
      } break;
    case EXIT:
      INSTR("exit");
      goto mutator_end;
      break;

    case ENDCODE:
      INSTR("endcode");
      fprintf(stderr,"Tried to evaluate beyond end of function.\n");
      fprintf(stderr,"Instruction pointer at %lx\n",(UInt)&ip[-1]);
      goto mutator_end;
      break;

      case TABLESWITCH :	/* DAVID */
          nodeptr = sp[0];
          IND_REMOVE(nodeptr);
          UPDATE_PROFINFO(nodeptr)
          switch(GET_LARGETAG(nodeptr)) {
          case CON_DATA  | CON_TAG :
          case CON_CDATA | CON_TAG :
              nodeptr = (NodePtr) GET_CONSTR(nodeptr);
              break;
        case CON_PTRS  | CON_TAG :
        case CON_WORDS | CON_TAG :
              nodeptr = 0;
              break;
          default :
              fprintf(stderr,"Trying to get tag from unevaluated node in MATCHCON at %08x!\n",ip-1);
              fprintf(stderr,"Node is:\n");
              DUMP_NODE(nodeptr);
              exit(-1);
              break;
          }
        ip  = (CodePtr) ALIGNPTR2(ip+1);
          ip += *(((short*) ip) + (int) nodeptr);
          break;

      case LOOKUPSWITCH :	/* DAVID */
        { int    sz = *ip;
          short* t;

          nodeptr = sp[0];
          IND_REMOVE(nodeptr);
          UPDATE_PROFINFO(nodeptr)
          nodeptr = (NodePtr) GET_INT_VALUE(nodeptr);

          ip = (CodePtr) ALIGNPTR2(ip+1);

          for (t = (short*) ip; sz > 0; t = t + 2, sz--) {
              if (((int) nodeptr) == *t) {
                  ip += *(t+1);
                  goto NextInst;
              }
          }
          ip += *t;
        }
        break;

    default:
      fprintf(stderr,"Unknown instruction %d at %lx\n",ip[-1], (UInt)&ip[-1]);
      exit(-1);
    }
  }
 mutator_end:
  Hp = hp;
  Sp = sp;
  Fp = fp;
  return;
}


#if PROFILE

int sizeofNode(Node tag) {
  switch (EXT_TAG(tag)) {
  case VAP_TAG0:
  case VAP_TAG1:
    { Cinfo cinfo = EXT_CINFO(tag);
      int size = (int)CINFO_SIZE(cinfo);
      return size+1;
    }
  case CON_TAG:
    { if (tag == CONSTR(42,0,0)) 
	return 1;
      else {
	fprintf(stderr,"CON_TAG in sizeofNode!\n");
	exit(-1);
	return 0;
      }
    }
  case IND_TAG:
    { fprintf(stderr,"IND_TAG in sizeofNode!\n");
      exit(-1);
      return 0;
    }
  }
}

#endif
