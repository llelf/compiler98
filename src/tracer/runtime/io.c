#include "ui.h"
#include "fileformat.h"

extern int traceGcStat;
extern int traceBreak;

FileOffset outputContext;


/* _tprim_setOutputContext primitive 3 :: Trace -> E (R a) -> R a */
C_HEADER(_tprim_setOutputContext)
{
        CTrace *t; NodePtr arg;

        arg = C_GETARG1(1);
	IND_REMOVE(arg);
        t = (CTrace*)arg;
	outputContext = t->ptr;

        arg = C_GETARG1(2);
	IND_REMOVE(arg);
	arg = GET_POINTER_ARG1(arg,1);
	C_RETURN(arg);
}

#if 0
/* obsolete */
C_HEADER(mycPutChar)
{
    NodePtr nodeptr, ch, t;

    nodeptr = C_GETARG1(1);
    IND_REMOVE(nodeptr);
    ch = GET_POINTER_ARG1(nodeptr, 1);
    IND_REMOVE(ch);
    t = GET_POINTER_ARG1(nodeptr, 2);
    IND_REMOVE(t);
    otInsert(GET_CHAR_VALUE(ch), t);
    putc(GET_CHAR_VALUE(ch), stdout);
    fflush(stdout);
    C_RETURN(mkUnit());
}
#endif

/* _tprim_chPutChar :: Trace -> R Handle -> R Char -> R (Either IOError ()) */
C_HEADER(_tprim_chPutChar)
{
    NodePtr handleR, np, ch, result;
    CTrace *t, *unit_t;
    FileDesc *a;
    char c;

    t = (CTrace*)C_GETARG1(1);
    handleR = C_GETARG1(2);
    IND_REMOVE(handleR);
    np = GET_POINTER_ARG1(handleR, 1);
    a = derefForeignObj((ForeignObj *)GET_INT_VALUE(np));
    np = C_GETARG1(3);
    IND_REMOVE(np);
    ch = GET_POINTER_ARG1(np, 1);
    IND_REMOVE(ch);
    c = GET_CHAR_VALUE(ch);
    putc(c, a->fp);		/* real output */
    putc(c, HatOutput);		/* copy of output */
    fwrite(&outputContext, sizeof(FileOffset), 1, HatBridge);
				/* link trace to output */

/* For heap-based tracing, we did this: */
/*  otInsert(GET_CHAR_VALUE(ch), outputContext); */
/* Originally: */
/*  otInsert(GET_CHAR_VALUE(ch), t); */
    
    unit_t = mkTNm(t, mkNmUnit(), mkSR());

    C_RETURN (mkR(mkRight(mkR(mkUnit(), unit_t)),
		  mkTAp1(t, mkTNm(t, mkNmRight(), mkSR()), 
			    unit_t,
                            mkSR())));
}

void
chPutChar (FileDesc* a, char c)
{
    putc(c, a->fp);		/* real output */
    putc(c, HatOutput);		/* copy of output */
    fwrite(&outputContext, sizeof(FileOffset), 1, HatBridge);
				/* link trace to output */
    return;
}

C_HEADER(_tprim_chGetChar)
{
    NodePtr handleR, np, result;
    CTrace* t;
    FileDesc *a;
    int ch;

    t = (CTrace*)C_GETARG1(1);
    handleR = C_GETARG1(2);
    IND_REMOVE(handleR);
    np = GET_POINTER_ARG1(handleR, 1);
    a = derefForeignObj((ForeignObj *)GET_INT_VALUE(np));
    ch = getc(a->fp);
    result = mkChar(ch);
    C_RETURN(mkR(result, mkTNm(t, mkNmChar(result), mkSR())));
}

int
_chGetChar (FileDesc* a)
{
    return getc(a->fp);
}

#if 0
-- This was the beginnings of a primitive, faster, implementation
-- of string I/O.  I never completed it.   (malcolm)

/* _tprim_chPutStr :: Trace -> R Handle -> R PackedString -> R (Either IOError ()) */
C_HEADER(_tprim_chPutStr)
{
    NodePtr handleR, np, ch, result, t, unit_t;
    FileDesc *a;

    t = C_GETARG1(1);
    handleR = C_GETARG1(2);
    IND_REMOVE(handleR);
    np = GET_POINTER_ARG1(handleR, 1);
    a = derefForeignObj((ForeignObj *)GET_INT_VALUE(np));
    np = C_GETARG1(3);
    IND_REMOVE(np);
    ch = GET_POINTER_ARG1(np, 1);
    IND_REMOVE(ch);
 /* putc(GET_CHAR_VALUE(ch), a->fp); 	-- needs fixing to PackedString */
 /* fflush(a->fp); /* Not really necessary... */
#if 0
    fprintf(stderr, "\t = 0x%x\n", t);
    prGraph(t, 1, 1);
    fprintf(stderr, "\n", t);
#endif
 /* otInsert(GET_CHAR_VALUE(ch), outputContext); */
    
    unit_t = mkTNm(t, mkNmUnit(), mkSR());

    C_RETURN (mkR(mkRight(mkR(mkUnit(), unit_t)),
		  mkTAp(t, mkCons(mkTNm(t, mkNmRight(), mkSR()), 
				  mkCons(unit_t, mkNil())), mkSR())));
}
#endif


C_HEADER(_tprim_HClose)
{
    NodePtr handleR, np, ch, result;
    CTrace *t, *unit_t;
    FileDesc *a;

    t = (CTrace*)C_GETARG1(1);
    handleR = C_GETARG1(2);
    IND_REMOVE(handleR);
    np = GET_POINTER_ARG1(handleR, 1);
    a = derefForeignObj((ForeignObj *)GET_INT_VALUE(np));
    np = C_GETARG1(3);
    IND_REMOVE(np);
    fclose(a->fp);
    
    unit_t = mkTNm(t, mkNmUnit(), mkSR());

    C_RETURN (mkR(mkRight(mkR(mkUnit(), unit_t)),
		  mkTAp1(t, mkTNm(t, mkNmRight(), mkSR()), 
			    unit_t,
                            mkSR())));
}

#define CAPPLY 0

#ifdef CAPPLY

extern Node FN_Prelude_46_95apply2[];
#define APPLY2 ((Node)FN_Prelude_46_95apply2)

NodePtr
apn(int n, Node papm)
{
    int i;
    NodePtr sr = C_GETARG1(1);
    NodePtr t = C_GETARG1(2);
    NodePtr rf = C_GETARG1(3);
    NodePtr arg1 = C_GETARG1(4);
    NodePtr call = Hp, result;
    NodePtr f;

    IND_REMOVE(rf);
    f = GET_POINTER_ARG1(rf, 1);

    call[0] = C_VAPTAG(APPLY2);
    INIT_PROFINFO(call,  &dummyProfInfo)
    call[EXTRA+1] = (Node)f;
    call[EXTRA+2] = (Node)t; /* Should build a new t here */
    call[EXTRA+3] = (Node)arg1;
    Hp += EXTRA+4;
    result = Hp;
    result[0] = C_VAPTAG(papm);
    INIT_PROFINFO(result,  &dummyProfInfo)
    result[EXTRA+1] = (Node)sr;
    result[EXTRA+2] = (Node)t; /* Should build a new t here */
    result[EXTRA+3] = (Node)call;
    for (i = 0; i < n-1; i++) {
	result[EXTRA+4+i] = (Node)C_GETARG1(5+i);
    }
    Hp += EXTRA+n+3;
/*    fprintf(stderr, "in ap%d\n", n);*/
    return result;
}


#define APFUN(n,m) \
extern Node FN_Prelude_46pap##m[];\
C_HEADER(ap##n) \
{\
    C_RETURN(apn(n, (Node)FN_Prelude_46pap##m)); \
}

APFUN(2,1)
APFUN(3,2)
APFUN(4,3)
APFUN(5,4)
APFUN(6,5)
APFUN(7,6)
APFUN(8,7)
APFUN(9,8)
APFUN(10,9)

#endif

#include <setjmp.h>
#ifdef __CYGWIN32__
extern jmp_buf exit_mutator;
#else
extern sigjmp_buf exit_mutator;
#endif
extern int exit_code;

C_HEADER(_tprim_cExitWith)
{
    NodePtr t, c;

    updateSatBs();
    updateSatCs();

    t = C_GETARG1(1);
    c = C_GETARG1(2);

    IND_REMOVE(t);
    IND_REMOVE(c);
    switch (CONINFO_NUMBER(*c)) {
    case TagExitSuccess:
	exit_code = 0;
	fprintf(stderr, "\nProgram exited normally.\n");
	break;
    case TagExitFailure:
	c = shortCircuitSelectors(GET_POINTER_ARG1(c,1));
	exit_code = GET_INT_VALUE(c);
	fprintf(stderr, "Program exited with error code %d\n", exit_code);
	break;
    default:
	fprintf(stderr, "exitWith: Strange error code:\n");
	/*prGraph(c, 3, 3);*/
	fprintf(stderr, "\n");
	break;
    }
    terminated = TRUE;
    startDbg(t, TRUE);
#ifdef __CYGWIN32__
    longjmp(exit_mutator, 1);
#else
    siglongjmp(exit_mutator,1);
#endif

    haskellEnd();	/* dead code now ? */
    exit(0);
}


C_HEADER(cEnter)
{
    NodePtr nodeptr;
    char *mod = NULL, *name;
    int defpos, pri;

    reductions++;
    if ((traceGcStat > 0) && ((reductions % traceGcStat) == 0)) {
	extern NodePtr hpLowLimit;
	extern FILE *traceGcFd;
	C_GC(1);
	/* 2+EXTRA == GCEXTRA as defined in runtime/Kernel/mark.h */
	fprintf(traceGcFd, "%d %d\n", reductions, Hp-&hpLowLimit[2+EXTRA]); 
    }

    dbg_last_trace = C_GETARG1(2);
    IND_REMOVE(dbg_last_trace);


    if ((traceBreak > 0) && (reductions > traceBreak))
	interrupted++;

    if (interrupted) {
        CNmType* nt;
        CTrace* t;
        FileOffset fo;
	fprintf(stderr, "Program execution interrupted\n");
        updateSatBs();
        updateSatCs();
        nt = primNTCString("Program interrupted (^C).");
        fo = nt->ptr;
        fseek(HatFile,8+sizeof(FileOffset),SEEK_SET);
        fwrite(&fo, sizeof(FileOffset), 1, HatFile);
        t = (CTrace*)dbg_last_trace;
        fo = t->ptr;
        fseek(HatFile,8,SEEK_SET);
        fwrite(&fo, sizeof(FileOffset), 1, HatFile);
        haskellEnd();
        exit(0);
    }
    if (trace_enter) {
	showSymbol(C_GETARG1(1), &mod, &name, &defpos, &pri);
	fprintf(stderr, "Entering %s.%s\n", mod, name);
    }
    if (trace_bpregexp) {
	if (mod == NULL)
	    showSymbol(C_GETARG1(1), &mod, &name, &defpos, &pri);
	if (re_exec(name)) {
	    startDbg(C_GETARG1(2), FALSE);
	}
    }
    if (mod != NULL) {
	free(mod);
	free(name);
    }
    if (trace_singlestep) 
	startDbg(C_GETARG1(2), FALSE);

    nodeptr = C_GETARG1(3);
    IND_REMOVE(nodeptr);
    C_RETURN(GET_POINTER_ARG1(nodeptr,1));
}

