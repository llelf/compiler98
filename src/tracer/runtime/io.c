#ifdef BIG_ENDIAN
#undef BIG_ENDIAN
#endif

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <assert.h>
/* #include <netinet/in.h> */
/* #include <netdb.h> */
#include "haskell2c.h"
#include "bytecode.h"
#include "getconstr.h"
#include "nodecache.h"
#include "outputtrace.h"

#define DEFAULT_DEPTH 5
#define DBGPORT 6710
#define DBGMAXTRIES 32

/*char PM_Prelude[] = "Prelude";*/
char PM_List[] = "List";
char Unimplemented[] = "!!!Unimplemented!!!";

int terminated = FALSE;
int reductions = 0;
int redTT = 0, redTS = 0, redST = 0, redSS = 0;
int trace_enter = 0;
int trace_bpregexp = 0;
int trace_singlestep = 0;
int interrupted = 0;

extern int traceGcStat;
extern int traceBreak;

/*extern void prGraph(NodePtr nodeptr,Int flags,Int d);*/

NodePtr dbg_last_trace = NULL;
NodePtr outputContext = NULL;

NodePtr shortCircuitSelectors(NodePtr node);

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

/* _tprim_setOutputContext primitive 3 :: Trace -> E (R a) -> R a */
C_HEADER(_tprim_setOutputContext)
{
	NodePtr arg = C_GETARG1(2);
	outputContext = C_GETARG1(1);
	IND_REMOVE(arg);
	arg = GET_POINTER_ARG1(arg,1);
	C_RETURN(arg);
}

/* _tprim_chPutChar :: Trace -> R Handle -> R Char -> R (Either IOError ()) */
C_HEADER(_tprim_chPutChar)
{
    NodePtr handleR, np, ch, result, t, unit_t;
    Arg *a;

    t = C_GETARG1(1);
    handleR = C_GETARG1(2);
    IND_REMOVE(handleR);
    np = GET_POINTER_ARG1(handleR, 1);
    a = cdataArg((CData *)GET_INT_VALUE(np));
    np = C_GETARG1(3);
    IND_REMOVE(np);
    ch = GET_POINTER_ARG1(np, 1);
    IND_REMOVE(ch);
    putc(GET_CHAR_VALUE(ch), a->fp);
    fflush(a->fp); /* Not really necessary... */
#if 0
    fprintf(stderr, "\t = 0x%x\n", t);
    prGraph(t, 1, 1);
    fprintf(stderr, "\n", t);
#endif
    otInsert(GET_CHAR_VALUE(ch), outputContext);
/*    otInsert(GET_CHAR_VALUE(ch), t);*/
    
    unit_t = mkTNm(t, mkNmUnit(), mkSR());

    C_RETURN (mkR(mkRight(mkR(mkUnit(), unit_t)),
		  mkTAp(t, mkCons(mkTNm(t, mkNmRight(), mkSR()), 
				  mkCons(unit_t, mkNil())), mkSR())));
}

C_HEADER(_tprim_chGetChar)
{
    NodePtr handleR, np, result, t;
    Arg *a;
    int ch;

    t = C_GETARG1(1);
    handleR = C_GETARG1(2);
    IND_REMOVE(handleR);
    np = GET_POINTER_ARG1(handleR, 1);
    a = cdataArg((CData *)GET_INT_VALUE(np));
    ch = getc(a->fp);
    result = mkChar(ch);
    C_RETURN(mkR(result, mkTNm(t, mkNmChar(result), mkSR())));
}

C_HEADER(_tprim_HClose)
{
    NodePtr handleR, np, ch, result, t, unit_t;
    Arg *a;

    t = C_GETARG1(1);
    handleR = C_GETARG1(2);
    IND_REMOVE(handleR);
    np = GET_POINTER_ARG1(handleR, 1);
    a = cdataArg((CData *)GET_INT_VALUE(np));
    np = C_GETARG1(3);
    IND_REMOVE(np);
    fclose(a->fp);
    
    unit_t = mkTNm(t, mkNmUnit(), mkSR());

    C_RETURN (mkR(mkRight(mkR(mkUnit(), unit_t)),
		  mkTAp(t, mkCons(mkTNm(t, mkNmRight(), mkSR()), 
				  mkCons(unit_t, mkNil())), mkSR())));
}

C_HEADER(fatal)
{
  fprintf(stderr, "No match in pattern.\n");
  terminated = TRUE;
  startDbg(C_GETARG1(1), FALSE);
  exit(0);
}

#define CAPPLY 0

#ifdef CAPPLY

extern Node FN_Prelude_46_95apply2[];
#define APPLY2 ((Node)FN_Prelude_46_95apply2)

NodePtr apn(int n, Node papm)
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

void dbg_blackhole()
{
  fprintf(stderr, "Black hole detected.\n");
  terminated = TRUE;
  startDbg(dbg_last_trace, FALSE);
  exit(0);
}

C_HEADER(_tprim_cExitWith)
{
    NodePtr t, c;

    t = C_GETARG1(1);
    c = C_GETARG1(2);

    IND_REMOVE(t);
    IND_REMOVE(c);
    switch (CONINFO_NUMBER(*c)) {
    case TagExitSuccess:
	fprintf(stderr, "\nProgram exited normally.\n");
	break;
    case TagExitFailure:
	c = shortCircuitSelectors(GET_POINTER_ARG1(c,1));
	fprintf(stderr, "Program exited with error code %d\n", GET_INT_VALUE(c));
	break;
    default:
	fprintf(stderr, "exitWith: Strange error code:\n");
	/*prGraph(c, 3, 3);*/
	fprintf(stderr, "\n");
	break;
    }
    terminated = TRUE;
    startDbg(t, TRUE);
    exit(0);
}

/*_primIntFromInteger primitive 1 :: Integer -> Int */

C_HEADER(_primIntFromInteger)
{
    fprintf(stderr, "In _primIntFromInteger...\n");
    /*prGraph(C_GETARG1(1), 3, 3);*/
    exit(1);
}

int 
checkEvaluation(NodePtr nodeptr, NodePtr *bot)
{
  nodeptr = shortCircuitSelectors(nodeptr);

  if ((GET_TAG(nodeptr) == CON_TAG)) {
      return EVALUATED;
  } else {
      while (1) {
	  if(GET_TAG(nodeptr) & VAP_TAG) {
	      if (ZAPPED(nodeptr)) {
		  *bot = nodeptr;
		  return EVALUATING;
	      } else
		  nodeptr = shortCircuitSelectors(GET_POINTER_ARG1(nodeptr,1));
	  } else
	      return CLOSURE;
      }

  }
}

int refnr = 1;

FILE *
connectToDebugger()
{
    int fdesc;
    char *fdesc_str = getenv("TRACE_FD");
    int i, port = DBGPORT;
    int s, len;
#if defined(__linux) || defined(__NetBSD__)
    struct sockaddr addr;
#else
    struct sockaddr_in addr;
#endif
    struct hostent *host;

    if (fdesc_str == NULL) {
	/* Should fork and spawn the interface here! */
	i = 0;
	while (i < DBGMAXTRIES && listen(s = in_bind_ne(port+i, SOCK_STREAM), 5))
	    i++;
	if (i == DBGMAXTRIES) {
	    fprintf(stderr, "Couldn't establish listen socket\n");
	    exit(1);
	}
	fflush(stdout);
        if (1)
          fprintf(stderr, "%d reductions (%d TT, %d TS, %d ST, %d SS)\n", reductions, redTT, redTS, redST, redSS);
        fprintf(stderr, "Waiting for connection from trace browser on port %d...\n", port+i);
        len=sizeof(addr);
	if ((fdesc = accept(s, &addr, &len)) == -1) {
	    fprintf(stderr, "Couldn't accept connection\n");
	    exit(1);
	}
	close(s);
#if 0
	switch (fork()) {
	case -1:
	    fprintf(stderr, "Couldn't fork\n");
	    exit(1);
	case 0: /* Child */
	    sprintf(&fd_str[0], "TRACE_FD=%d", sockfd);
	    putenv(fd_str);
	    execv(argv[0], &argv[0]);
	    quit("Couldn't exec demo program!");
	    break;
	default:
	    /*fprintf(stderr, "Closing parent version of socket fd.\n");*/
	    close(sockfd);
	    break;
	}
#endif
    } else {
	/*fdesc = sockopen("indy105.cs.york.ac.uk", DBGPORT, SOCK_STREAM);*/
	fdesc = atoi(fdesc_str);
    }
    return fdopen(fdesc, "rb+");
#if 0
    if (fdesc < 0)
	return 0;
    else
	return fdopen(fdesc, "rb+");
#endif
}

CData *sockcdata = NULL;

/* cGetDbgSocket :: () -> Socket */
C_HEADER(cGetDbgSocket)
{
	if (sockcdata == NULL) { /* Socket not yet opened */
		FILE *fp;
		
		if (fp = connectToDebugger()) {
			Arg a;
			
			a.fp = fp;
			a.bm = _IOFBF;
			a.size = -1;
			a.gc = gcFile;

			sockcdata = allocCData(a);
			/* fprintf(stderr, "Debug socket successfully opened!\n");*/
		} else {
			fprintf(stderr, "Couldn't fdopen dbg socket!\n");
			exit(-1);
		}
	}
	C_RETURN(mkCInt((int)sockcdata));
}

void ToDebugger(FILE *sock, char *s)
{
    int fn = fileno(sock);
    int n = write(fn, s, strlen(s));
    if (n != strlen(s)) {
       fprintf(stderr, "Couldn't write %s, errno=%d\n", s, errno);
       perror("Error: ");
       exit(1);
    }
#if 0
    fprintf(stderr, "ToDebugger: %s\n", s);
#endif
}

showNode(NodePtr p)
{
#ifdef PROFILE
    Info *info = GET_INFO(p);
    
    fprintf(stdout, "m: %s p: %s c: %s\n", 
	    info->sinfo->module, 
	    info->sinfo->producer,
	    info->sinfo->constructor);
#endif
}

void showHaskellString(FILE *sock, NodePtr t)
{
    char s[] = "x";

    IND_REMOVE(t);
    while (CONINFO_SIZE(*t) > 0) {
	NodePtr c = GET_POINTER_ARG1(t,1);
        IND_REMOVE(c);
	s[0] = GET_CHAR_VALUE(c);
	ToDebugger(sock, s);
	t = GET_POINTER_ARG1(t,2);
        IND_REMOVE(t);
    }
}

/*	This definition does not match the one in PreludeDebug.hs !!!!
data NmType =
     NTInt
   | NTBool
   | NTChar
   | NTCons
   | NTNil
   | NTTuple
   | NTString 
   | NTPString
   | NTFun
   | NTBuiltin Int deriving (Eq, Show)
                ^--- A pointer to the profile code
   */


char *showProfString(char *s)
{
    static char str[128];
    char *fn = s;
	
    while (*fn && *fn != ':') {
	while (*fn && *fn != '.' && *fn != ':')
	    fn++;
	if (*fn && *fn == '.') {
	    fn++;
	    s = fn;
	}
    }
	
    if (*fn == ':' && !*(fn+1))  /* Must be : (cons) */
	fn++;
    
    if (s == fn) /* must be a . (composition) */
	s--;

    strncpy(&str[0], &s[0], fn-s);
    str[fn-s] = '\0';
    if (str[0] == '\0') {
	fprintf(stderr, "\nStrange node in showProfString\n");
	str[0] = '_';
	str[1] = '\0';
    }
    if (str[0] == '(') {
	bcopy(&str[0], &str[1], strlen(str)+1);
	str[0] = '"';
	strcat(&str[0], "\"");
    }
    return strdup(str);
}

void showSymbol(NodePtr t, char **pmodule, char **pname, int *pdefpos, int *ppri)
{
    int constr, ch, len;
    static char str[128];
    char *fn, *s;
    NodePtr c, np;

    *pdefpos = -1;
    *ppri = 3; /* Default priority, infixl 9 */
    IND_REMOVE(t);
    constr = CONINFO_NUMBER(*t);
    switch (constr) {
    case NTChar:
	np = GET_POINTER_ARG1(t, 1);
	IND_REMOVE(np);
	ch = GET_INT_VALUE(np);
	if (ch > 31 && ch < 256 && ch != '\'')
	    sprintf(&str[0], "'%c'", ch);
	else if (ch == '\n')
	    sprintf(&str[0], "'\\n'");
	else if (ch == '\t')
	    sprintf(&str[0], "'\\t'");
	else if (ch == -1) 
	    sprintf(&str[0], "'\\e'");
	else 
	    sprintf(&str[0], "'\\0%X'", ch);
	*pmodule = strdup("Prelude");
	*pname = strdup(str);
	break;
    case NTInt:
	np = GET_POINTER_ARG1(t, 1);
	IND_REMOVE(np);
	sprintf(&str[0], "%d", GET_INT_VALUE(np));
	*pmodule = strdup("Prelude");
	*pname = strdup(str);
	break;
    case NTInteger:
	np = GET_POINTER_ARG1(t, 1);
	IND_REMOVE(np);
        if (CONINFO_LARGESIZEU(*np) == 1)
	  sprintf(&str[0], "%d", GET_INT_VALUE(np));
        else
	  sprintf(&str[0], "<Integer>");
	*pmodule = strdup("Prelude");
	*pname = strdup(str);
	break;
#if 0
    case NTFun:
	return showProfString((char *)GET_INT_VALUE(t));
    case TagNTBuiltin:
	fn = (char *)GET_INT_VALUE(t);
	while (*fn && *fn != '.')
	    fn++;
	if (!*fn && (np=shortCircuitSelectors(GET_POINTER_ARG1(t, 1)))) { 
            /* Assume it is a CAP0. Not sure if this is always true. */
	    Cinfo cinfo = GET_CINFO(np);
	    Finfo finfo = CINFO_FINFO(cinfo);
	    UInt *constptr = FINFO_CONST(finfo);
	    return showProfString((char *)profName(constptr));
	} else {
	    if (!*fn) {
		fprintf(stderr, "\nStrange node in showSymbol(TagNTBuiltin)\n");	
		return(strdup("_"));
	    } else
		return strdup(++fn);
	}
#endif
    case NTTrusted:
    case NTId:
	*pmodule = strdup((char *)GET_POINTER_ARG1(t, 1));
	*pname =  strdup((char *)GET_POINTER_ARG1(t, 3)); /*&t[1+EXTRA+2]);*/
	*pdefpos = GET_VALUE_ARG1(t, 2);
	*ppri = GET_VALUE_ARG1(t, 4);
	break;
#if 0
    case NTConstr:
	/* Not used at the moment -- NTId is used both for vars and constrs */
	*pmodule = strdup((char *)GET_POINTER_ARG1(t, 1));
	*pname =  strdup((char *)&t[1+EXTRA+1]);
	break;
#endif
    case NTCase:
	*pmodule = strdup("Prelude");
	*pname = strdup("case");
	break;
    case NTIf:
	*pmodule = strdup("Prelude");
	*pname = strdup("if");
	break;
    case NTGuard:
	*pmodule = strdup("Prelude");
	*pname = strdup("guard");
	break;
    case NTLambda:
	*pmodule = strdup("Prelude");
	*pname = strdup("\\");
	break;
    case NTDummy:
	*pmodule = strdup("Unknown");
	*pname = strdup("prim");
	break;
    case NTCString:
	*pmodule = strdup("Unknown");
	np = GET_POINTER_ARG1(t, 1);
	*pname = malloc(len+3);
	sprintf(*pname, "'%s'", (char *)&np[1+EXTRA]);
	break;
    case NTTuple:
	*pmodule = strdup("Prelude");
	*pname = strdup(",");
	break;
    default:
      {
#ifdef PROFILE
	Info *info = GET_INFO(t);
	showNode(t);
#endif
	fprintf(stderr, "\nStrange node in showSymbol at 0x%x(0x%x)\n", t, *t);
	/*prGraph(t, 3, 3);*/
	exit(1);
      }
    }
}

void dumpSR(FILE *sock, NodePtr t)
{
    static char str[128];
    int rowcol, row, col;
    IND_REMOVE(t);
    if((GET_TAG(t) == CON_TAG)) {
	Coninfo cinfo = GET_CONINFO(t);
#if 0
	if (CONINFO_NUMBER(cinfo) == 1) { /* SR2 */
	    rowcol = GET_INT_VALUE(GET_POINTER_ARG1(t, 2));
	    t = GET_POINTER_ARG1(t, 1);
	    {
		Cinfo cinfo = GET_CINFO(t);
		Finfo finfo = CINFO_FINFO(cinfo);
		UInt *constptr = FINFO_CONST(finfo);
		char *modname = (char*)profName(FINFO_CONST(finfo)) + 2;
		char *s;
		sprintf(&str[0], "\"%s", modname);
		s = strchr(str, ':');
		sprintf(s, "\" %d ", rowcol);
		ToDebugger(sock, str);
	    }
	}
#endif
	if (CONINFO_NUMBER(cinfo) == 2) { /* SR3 */
	    rowcol = (int)t[1+EXTRA];
	    sprintf(&str[0], "\"%s.hs\" %d ", (char *)GET_POINTER_ARG1(t, 2), rowcol);
	    ToDebugger(sock, str);	    
	} else if (CONINFO_NUMBER(cinfo) == 0) { /* SR */
	    ToDebugger(sock, "* ");
	} else {
	    /*prGraph(t, 3, 3);*/
	    fprintf(stderr, "\ncnr = %d\n", CONINFO_NUMBER(cinfo));
	    ToDebugger(sock, "* ");
	}
    } else {
	fprintf(stderr, "Bad SR\n");
	/*prGraph(t, 3, 3); */
	fprintf(stderr, "\n");
	ToDebugger(sock, "* ");
    }
}

/*
data Node =
     Ap Trace [Trace] SR
   | Nm Trace NmType SR
   | Ind Trace Trace
   | Root
   | Sat Trace Trace
   | Pruned
*/
char TAGSTR[][10] = {"Ap", "Nm", "Ind", "Root", "Sat", "Pruned"};

void showTrace(NodePtr t, int level)
{
    NodePtr n;
    int constr, ref;
    NodePtr *pbot;
    int *pind;

    if (level-- == 0) {
	    fprintf(stderr, "<>");
	    return;
    }

    t = shortCircuitSelectors(t);
    constr = CONINFO_NUMBER(*t) & 0xf;
    switch (constr) {
    case TagAp: {
	    NodePtr ts;
	    int tag, first = 1;
	    fprintf(stderr, "(A ");
	    showTrace(GET_POINTER_ARG1(t, 1), level);
	    fprintf(stderr, " (");
	    ts = GET_POINTER_ARG1(t, 2);
	    IND_REMOVE(ts);
	    tag = CONINFO_NUMBER(*ts);
	    while (CONINFO_PSIZE(*ts) == 2) {
	       if (first++ > 1)
		       fprintf(stderr, " ");
	       showTrace(GET_POINTER_ARG1(ts, 1), level);
	       ts = shortCircuitSelectors(GET_POINTER_ARG1(ts, 2));
	   }
	   fprintf(stderr, ")");
	   break; }
    case TagNm: {
	    char *mod, *name;
	    int defpos, pri;    
	    fprintf(stderr, "(N ");
	    showSymbol(GET_POINTER_ARG1(t, 2), &mod, &name, &defpos, &pri);
	    fprintf(stderr, "%s)", name);
	    break;}
    case TagInd:
	    fprintf(stderr, "(I ");
	    showTrace(GET_POINTER_ARG1(t, 1), level);
	    fprintf(stderr, ")");
	    break;
    case TagRoot:
	    fprintf(stderr, "R");
	    break;
    case TagSat:
	    fprintf(stderr, "(S ");
	    showTrace(GET_POINTER_ARG1(t, 1), level);
	    fprintf(stderr, " ");
	    n = shortCircuitSelectors(GET_POINTER_ARG1(t, 2));
	    switch (checkEvaluation(n, pbot)) {
	    case EVALUATED:
		    showTrace(n, level);
		    t = n;
		break;
	    case EVALUATING:
		    fprintf(stderr, "B");
		    break;
	    case CLOSURE:
		    fprintf(stderr, "_");
		    break;
	    }
	    fprintf(stderr, ")");
	    break;
    case TagPruned:
	    fprintf(stderr, "P");
	    break;
    case TagHidden:
	    fprintf(stderr, "(H ");
	    showTrace(GET_POINTER_ARG1(t, 1), level);
	    fprintf(stderr, ")");
	    break;
    default:
	    fprintf(stderr, "showTrace: strange tag %d\n", constr);
    }
}

int checkForCaseIfGuard(NodePtr t) /* t must be a TagAp */
{
    NodePtr ts, np;
    assert(CONINFO_NUMBER(*t) == TagAp);
    ts = GET_POINTER_ARG1(t, 2); /* Get arg list */
    IND_REMOVE(ts);
    assert(CONINFO_PSIZE(*ts) == 2); /* Is it a cons? */
    np = GET_POINTER_ARG1(ts, 1); /* Get the head */
    IND_REMOVE(np);
    if (CONINFO_NUMBER(*np) == TagNm) { /* Is it a name? */
	  np = GET_POINTER_ARG1(np, 2);
	  switch (CONINFO_NUMBER(*np)) {
	  case NTCase: return NTCase;
	  case NTIf: return NTIf;
	  case NTGuard: return NTGuard;
	  }
    }
    return 0;
}

#define MASK_K                  (0x00003f0)
#define CONINFO_DIST(p) 	(((p)>>8)&0x3f)
#define INF_AGE			63

#define DEBUG_FT_NOT

NodePtr followTrace(NodePtr t, NodePtr *pbot, int *pind)
{
    NodePtr n;
    int constr, ref;
    *pbot = NULL;
    *pind = 0;
#ifdef DEBUG_FT
    fprintf(stderr, "\n.");
#endif    
    while (1) {
#ifdef DEBUG_FT
	fprintf(stderr, ".");
#endif    
	t = shortCircuitSelectors(t);
	constr = CONINFO_NUMBER(*t);
	if (constr & MASK_K) {
	    
	    if ((CONINFO_DIST(*t) != INF_AGE) || 
		((constr & ~MASK_K) != TagNm)) {
		/* A name should have K == INF_AGE */
		fprintf(stderr, "Pruned-bit not removed (d=%d)!!!\n", CONINFO_DIST(*t));
	    }
	    constr &= ~MASK_K;
	    t[0] &= ~(MASK_K << 4);
	}
	switch (constr) {
	case TagInd:
#ifdef DEBUG_FT
	    fprintf(stderr, " # (%d, %d)", ncFind(t), ncFind(GET_POINTER_ARG1(t, 1)));
#endif	    
	    if (*pind == 0) {
		ref = abs(ncFind(t));
		if (ref == 0)
		    ref = ncInsert(t, FALSE);
		*pind = ref;
	    }
	    t = GET_POINTER_ARG1(t, 2);
	    break;
	case TagSat:
	    n = shortCircuitSelectors(GET_POINTER_ARG1(t, 2));
#ifdef DEBUG_FT
	    fprintf(stderr, " S -> (%d, %d)", ncFind(t), ncFind(GET_POINTER_ARG1(t, 1)));
#endif	    
	    switch (checkEvaluation(n, pbot)) {
	    case EVALUATED:
		t = n;
		break;
	    case EVALUATING:
#if 0  
		fprintf(stderr, "Evaluating!!!!\n");
#endif
		return GET_POINTER_ARG1(t, 1);
	    case CLOSURE:
#if 0
		fprintf(stderr, "Evaluated!!!\n");
		prGraph(n, 15, 15);
#endif
		t = GET_POINTER_ARG1(t, 1);
		break;
	    }
	    break;
	case TagAp:
	case TagRoot:
	case TagPruned:
	case TagNm:
	case TagHidden:
#ifdef DEBUG_FT
	    fprintf(stderr, " RAPN %d\n", ncFind(t));
#endif	    
	    return t;
	default:
	    fprintf(stderr, "followTrace: strange node (tag=%d).\n", constr);
	    showNode(t);
	    /*prGraph(t, 1, 1); fprintf(stderr, "\n");	    */
	    exit(1);
       }
    }
}

void getRef(FILE *sock, NodePtr t)
{
    int ref, constr;
    NodePtr bot;
    static char str[16];
#if 0
    t = followTrace(t, &bot);
    if (bot != NULL) {
	fprintf(stderr, "getRef: impossible bottom...\n");
#ifdef PROFILE
	prGraph(t, 5, 5);
	fprintf(stderr, "\n");
#endif
    }
#endif
    ref = ncFind(t);
    constr = CONINFO_NUMBER(*t);
    if ((GET_TAG(t) == CON_TAG) && (constr == TagRoot))
	ref = 0;
    else if (ref == 0)
	ref = ncInsert(t, FALSE);
    else if (ref < 0)
	ref = abs(ref);
    sprintf(&str[0], " %d ", ref);
    ToDebugger(sock, str);
}

static NodePtr charListInProgress = NULL;

void charListP(NodePtr t)
{
    NodePtr old = t, bot = NULL, np, ts;
    int ind;

    while (1) {
	switch (CONINFO_NUMBER(*t)) {
	case TagAp:
		ts = GET_POINTER_ARG1(t, 2); /* Get arg list */
		IND_REMOVE(ts);
		if (CONINFO_PSIZE(*ts) != 2) return; /* Check if empty */
		np = GET_POINTER_ARG1(ts, 1); /* Get the head (the cons) */
		IND_REMOVE(np);
		np = followTrace(np, &bot, &ind);
		if (bot != NULL) return;
		if (CONINFO_NUMBER(*np) != TagNm) return;
		np = GET_POINTER_ARG1(np, 2);
		switch (CONINFO_NUMBER(*np)) {
		case NTTrusted:
		case NTId:
			if (strcmp((char*)GET_POINTER_ARG1(np, 3), ":") != 0)
				return;
			break;
		default: return;
		}
		ts = GET_POINTER_ARG1(ts, 2); /* Get the tail */
		if (CONINFO_PSIZE(*ts) != 2) return; /* Check if empty */
		np = GET_POINTER_ARG1(ts, 1); /* Get the head (the char) */
		IND_REMOVE(np);
		np = followTrace(np, &bot, &ind);
		if (bot != NULL) return;
		if (CONINFO_NUMBER(*np) != TagNm) return;
		np = GET_POINTER_ARG1(np, 2);
		if (CONINFO_NUMBER(*np) != NTChar) return;
		ts = GET_POINTER_ARG1(ts, 2); /* Get the tail */
		if (CONINFO_PSIZE(*ts) != 2) return; /* Check if empty */
		old = t;
		t = followTrace(GET_POINTER_ARG1(ts, 1), &bot, &ind);
		if (bot != NULL) return;	       
		break;
	case TagNm:
		np = GET_POINTER_ARG1(t, 2);
		switch (CONINFO_NUMBER(*np)) {
		case NTTrusted:
		case NTId:
		    if (strcmp((char*)GET_POINTER_ARG1(np, 3), "[]") == 0)
			charListInProgress = old;
		}
		return;
	default:
		return;
	}
    }
}

void dump(FILE *sock, int level, NodePtr t) 
{
   int constr, tag, first = 1, ref, ind;
   NodePtr nt, n, ts, bot;
   static char str[128];
   char *s;

   t = followTrace(t, &bot, &ind);

   if (bot != NULL) {
       ref = ncFind(bot);
       if (ref > 0) {
	   sprintf(&str[0], "(R %d %d)", ref, ind);
           ToDebugger(sock, str);
       } else {
	   ref = ncInsert(bot, TRUE);
	   sprintf(&str[0], "(B %d ", abs(ref));
	   ToDebugger(sock, str);
	   getRef(sock, t);
	   ToDebugger(sock, ")");
       }
       return;
   }

   ref = ncFind(t);
   if ((charListInProgress == NULL) && (--level < 0)) {
       ToDebugger(sock, "(D ");
       getRef(sock, t);
       ToDebugger(sock, ")");
       return;
   }

   constr = CONINFO_NUMBER(*t);
   switch (constr) {
   case TagAp:
       if (level == 0) {
	   ToDebugger(sock, "(D ");
	   getRef(sock, t);
	   ToDebugger(sock, ")");
	   charListInProgress = NULL;
	   return;
       }
       if (ref > 0) {
	   sprintf(&str[0], "(R %d %d)", abs(ref), ind);
           ToDebugger(sock, str);
	   charListInProgress = NULL;
       } else {
	   NodePtr np;
	   int nmType = 0;
	   if (ref < 0)
	       ncHist(t, abs(ref));
	   else
	       ref = ncInsert(t, TRUE);
	   
	   nmType = checkForCaseIfGuard(t);

	   ts = GET_POINTER_ARG1(t, 2); /* Get arg list */
	   IND_REMOVE(ts);

	   if (nmType == 0) { /* Ordinary application */
	       if (!charListInProgress) {
		   charListP(t);
	       }
	       sprintf(&str[0], "(A%s %d ", charListInProgress==NULL?"":"s", abs(ref));
	       if (charListInProgress == t)  
		   charListInProgress = NULL; /* We're done with the string */

	       ToDebugger(sock, str);
	       dumpSR(sock, GET_POINTER_ARG1(t,3));
	       ToDebugger(sock, "(");
	       while (CONINFO_PSIZE(*ts) == 2) {
		   if (first++ > 1)
		       ToDebugger(sock, " ");
		   dump(sock, level, GET_POINTER_ARG1(ts, 1));
		   ts = shortCircuitSelectors(GET_POINTER_ARG1(ts, 2));
	       }
	       ToDebugger(sock, ")");
	       getRef(sock,  GET_POINTER_ARG1(t, 1));
	       sprintf(&str[0], " %d)", ind);
	       ToDebugger(sock, str);
	   } else {
	       sprintf(&str[0], "(C %d %d ", nmType==NTIf?0:nmType==NTCase?1:2, abs(ref));
	       ToDebugger(sock, str);
	       dumpSR(sock, GET_POINTER_ARG1(t,3));
	       assert(CONINFO_PSIZE(*ts) == 2);
	       ts = shortCircuitSelectors(GET_POINTER_ARG1(ts, 2));
	       dump(sock, level, GET_POINTER_ARG1(ts, 1));
	       dump(sock, level, GET_POINTER_ARG1(t, 1));
	       sprintf(&str[0], " %d)", ind);
	       ToDebugger(sock, str);	       
	   }
       }
       break;
   case TagNm:
       if (ref > 0) {
	   sprintf(&str[0], "(R %d %d)", ref, ind); 
           ToDebugger(sock, str);
      } else {
	   char *mod, *name;
	   int defpos, pri;
	   if (ref < 0) 
	       ncHist(t, abs(ref));
	   else
	       ref = ncInsert(t, TRUE);
	   sprintf(&str[0], "(N %d ", abs(ref));
           ToDebugger(sock, str);
	   dumpSR(sock, GET_POINTER_ARG1(t, 3));
	   showSymbol(GET_POINTER_ARG1(t, 2), &mod, &name, &defpos, &pri);
	   sprintf(&str[0], "%s.hs", mod);
	   ToDebugger(sock, str);
	   ToDebugger(sock, " ");
	   if (name[0] == '\0')
	       ToDebugger(sock, "\"()\"");
	   else
	       ToDebugger(sock, name);
	   sprintf(&str[0], " %d %d ", defpos, pri);
	   ToDebugger(sock, str);
	   free(mod);
	   free(name);
	   getRef(sock,  GET_POINTER_ARG1(t, 1));
	   sprintf(&str[0], " %d)", ind);
	   ToDebugger(sock, str);
       }
       break;
   case TagInd:
       fprintf(stderr, "dump: TagInd\n");
       exit(1);
       break;
   case TagPruned:
       ToDebugger(sock, "P ");
       break;
   case TagRoot:
       /* Shouldn't happen */
       fprintf(stderr, "TagRoot in dump. Hmmm.\n");
       ToDebugger(sock, "P ");
       break; /* Should exit !!! */
   case TagSat:
       fprintf(stderr, "TagSat in dump. Hmmm.\n");
       showNode(t);
       /*prGraph(t, 1, 1); fprintf(stderr, "\n");*/
       exit(1);
   case TagHidden:
       ref = ncFind(t);
       if (ref > 0) {
	   sprintf(&str[0], "(R %d %d)", ref, ind);
           ToDebugger(sock, str);
       } else {
	   ref = ncInsert(t, TRUE);
	   sprintf(&str[0], "(H %d ", abs(ref));
	   ToDebugger(sock, str);
	   t = GET_POINTER_ARG1(t, 1);
	   IND_REMOVE(t);
	   getRef(sock, t);
	   ToDebugger(sock, ")");
       }
       break;
   default:
       fprintf(stderr, "dump: strange node.\n");
       showNode(t);
       /*prGraph(t, 1, 1); fprintf(stderr, "\n");*/
       exit(1);
       break;
   }
}

void dumperr(NodePtr t)
{
    dump(stderr, 3, t);
}

void getOutputRef(FILE *sock, NodePtr t)
{
    int ref, constr;
    NodePtr bot;
    static char str[16];
    ref = ncFind(t);
    constr = CONINFO_NUMBER(*t);
    if ((GET_TAG(t) == CON_TAG) && (constr == TagRoot))
	ref = 0;
    else {
	    if (ref == 0)
		    ref = ncInsert(t, FALSE);
	    else if (ref < 0)
		    ref = abs(ref);
	    if ((GET_TAG(t) == CON_TAG) && (constr == TagHidden)) {
		    t = GET_POINTER_ARG1(t, 1);
		    IND_REMOVE(t);
		    ref = ncFind(t);
		    if (ref == 0)
			    ref = ncInsert(t, FALSE);
		    else if (ref < 0)
			    ref = abs(ref);
	    }	    
    }

    sprintf(&str[0], " %d ", ref);
    ToDebugger(sock, str);
}

void dumpOutput(FILE *sock, int ch, NodePtr t) 
{
   int constr, ref, ind;
   NodePtr c, np, bot;
   static char str[128];

   t = followTrace(t, &bot, &ind);
   if (bot != NULL) {
       fprintf(stderr, "dumpOutput: character trace is bottom!\n");
       exit(1);
   }

   /* Transfer the character */
   sprintf(&str[0], " %d ", ch);
   ToDebugger(sock, str);

   /* Transfer its trace */
   getOutputRef(sock, t);

#if 0
   constr = CONINFO_NUMBER(*t);
   switch (constr) {
   case TagNm:
       if (ref == 0) 
	   ref = ncInsert(t, FALSE);
       ref = abs(ref);
       c = GET_POINTER_ARG1(t, 2);
       IND_REMOVE(c);
       if (CONINFO_NUMBER(*c) == NTChar) {
	   np = GET_POINTER_ARG1(c, 1);
	   sprintf(&str[0], "%d ", GET_INT_VALUE(np));
	   ToDebugger(sock, str);
	   if (ind > 0) {
	       sprintf(&str[0], " %d ", ind);
	       ToDebugger(sock, str);
	   } else 
	       getRef(sock,  GET_POINTER_ARG1(t, 1));
	   /*fprintf(stderr, "Sending char '%c'\n", GET_INT_VALUE(c));*/
       } else {
	   fprintf(stderr, "dumpOutput: not a char\n");
	   sprintf(&str[0], "64 0 ");
	   ToDebugger(sock, str);
#if 0
	   prGraph(c, 3, 3);
	   fprintf(stderr, "\n");
	   prGraph(t, 3, 3);
	   fprintf(stderr, "\n");	   
	   exit(1);
#endif
       }
       break;
   default:
       fprintf(stderr, "dumpOutput: not an Nm\n");
       sprintf(&str[0], "63 0 ");
       ToDebugger(sock, str);
       break;
   }
#endif
}

int isRoot(NodePtr t)
{
    t = shortCircuitSelectors(t);
    return CONINFO_NUMBER(*t) == TagRoot;
}

void dumpTrace(FILE *sock, NodePtr t)
{
    /*ToDebugger(sock, "(");*/
    /*dumpChain(sock, 50, 10, t);*/
    charListInProgress = NULL;
    dump(sock, DEFAULT_DEPTH, t);
    ToDebugger(sock, "\n");
    fflush(sock);
}

NodePtr listIndex(int n, NodePtr t)
{
    /*fprintf(stderr, "listIndex\n");*/
    t = shortCircuitSelectors(t);
    while (n-- > 0) {
	if (CONINFO_PSIZE(*t) == 2) {
	    t = GET_POINTER_ARG1(t, 2);
	    t = shortCircuitSelectors(t);
	} else {
	    fprintf(stderr, "index out of range in listIndex\n");
	    exit(1);
	}
    }
    return GET_POINTER_ARG1(t, 1);
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


    if ((traceBreak > 0) && (reductions > traceBreak))
	interrupted++;

    if (interrupted) {
	fprintf(stderr, "Program execution interrupted\n");
	startDbg(dbg_last_trace, FALSE);
	interrupted = 0;
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

void
DisplayName(char *s, NodePtr nm)
{
    char *mod = NULL, *name;
    int defpos, pri;
    showSymbol(nm, &mod, &name, &defpos, &pri);
    fprintf(stderr, "(%s)%s.%s", s, mod, name);
}

#if 0
#define displayName(x, y) DisplayName(x, y)
#define displayString(s) fprintf(stderr, s)
#else
#define displayName(x, y)
#define displayString(s) 
#endif

int trustFun(NodePtr t) 
{
   char *module, *name;
   int defpos;
   NodePtr nm;

   displayString("trustFun:\n");
   while (1) {
       int cnr;
       displayString("(loop)\n");       
       IND_REMOVE(t);
       switch (CONINFO_NUMBER(*t)) {
       case TagAp:
	   t = GET_POINTER_ARG1(t, 2);
	   IND_REMOVE(t);
	   t = GET_POINTER_ARG1(t, 1);
	   displayString("TagAp\n");
	   /*return FALSE;*/ /*  Wrong!!!!!!!! */
	   break;
       case TagNm:
	   displayString("TagNm\n");
	   nm = t;
	   t = GET_POINTER_ARG1(t, 2);
	   IND_REMOVE(t);
	   switch (CONINFO_NUMBER(*t)) {
	   case NTTrusted:
	       displayName("Trusted",t);
	       return TRUE;
	   case NTId:
	       displayName("Id", t);
	       return FALSE;
	   case NTLambda:
	       displayName("Lambda", t);
	       /*t = GET_POINTER_ARG1(t, 1);*/
	       return FALSE;
	       break;
	   case NTCase:
	       displayName("Case", t);
#if 1
	       t = GET_POINTER_ARG1(nm, 1);
#else
	       return TRUE;
#endif
	       break;
	   case NTIf:
	       displayName("If", t);
#if 1
	       t = GET_POINTER_ARG1(nm, 1);
#else
	       return TRUE;
#endif
	       break;
	   case NTGuard:
	       displayName("Guard", t);
#if 1
	       t = GET_POINTER_ARG1(nm, 1);
#else
	       return TRUE;
#endif
	       break;
	   case NTDummy:
	       displayName("Dummy", t);
	       return FALSE;
	   default:
	       fprintf(stderr, "(%d) ", CONINFO_NUMBER(*t));
	       fprintf(stderr, "!");
	       displayName("!", t);
	       break;
	   }
	   break;
       case TagSat:
	   displayString("TagSat\n");
	   t = GET_POINTER_ARG1(t, 1);
	   break;
       case TagInd:
	   displayString("TagInd\n");
	   t = GET_POINTER_ARG1(t, 2);
	   break;
       case TagRoot:
	   displayString("TagRoot\n");
	   /*fprintf(stderr, "<root> ");*/
	   return FALSE;
       case TagPruned:
	   displayString("TagPruned\n");
	   /* Not entirely sure this is correct */
	   return FALSE;
       case TagHidden:
	   displayString("TagHidden\n");
	   return TRUE;
       default:
	   fprintf(stderr, "trustFun: strange node, tag=%d (masked %d).\n", CONINFO_NUMBER(*t), CONINFO_NUMBER(*t) & ~MASK_K);
	   fprintf(stderr, "\nwrong-t = 0x%x ", t);
#ifdef TRACE
	   prGraph(t, 1, 1); fprintf(stderr, "\n");
#endif
	   exit(1);
       }
   }
}

C_HEADER(trustedName)
{
	NodePtr t = C_GETARG1(1);
	IND_REMOVE(t);
	switch (CONINFO_NUMBER(*t)) {
	case NTTrusted:
		C_RETURN(mkTrue());
	case NTId:
		C_RETURN(mkFalse());
	case NTLambda:
		C_RETURN(mkTrue());
	case NTCase:
		C_RETURN(mkTrue());
	case NTDummy:
		C_RETURN(mkFalse());
	default:
		fprintf(stderr, "trustedName: strange name tag %d.\n", CONINFO_NUMBER(*t));
		
		break;
	}
}

C_HEADER(trustedFun)
{
    NodePtr t = C_GETARG1(1);
    IND_REMOVE(t);
    dump(stderr, 5, t);
    C_RETURN(trustFun(C_GETARG1(1)) ? mkTrue() : mkFalse());
}

C_HEADER(trusted)
{
    int trust1, trust2;
    
    /*dbg_last_trace = C_GETARG1(2);*/

    displayString("F: ");
    trust2 = trustFun(C_GETARG1(2));
    /*reductions++;*/
    if (trust2) {
        displayString("   C: ");
	trust1 = trustFun(C_GETARG1(1));
        displayString("\n");
	if (trust1) {
	    redTT++;
	    C_RETURN(mkTrue());
	} else {
	    redST++;
	    C_RETURN(mkFalse());
	}
    } else {
        displayString("\n");
	    redSS++;
	    C_RETURN(mkFalse());
#if 0
	if (trust2) {
	    redST++;
	    C_RETURN(mkFalse());
	} else {
	    redSS++;
	    C_RETURN(mkFalse());
	}
#endif
    }
}

int
cTrusted(NodePtr t, NodePtr tf)
{
    return trustFun(tf) && trustFun(t);
}

C_HEADER(trust)
{
  NodePtr np = C_GETARG1(1);
  IND_REMOVE(np);
  if (trustFun(np)) {
	  /*fprintf(stderr, "Trusting something\n");*/
    C_RETURN(mkTrue());
  } else {
	  /*fprintf(stderr, "Not trusting something\n");*/
    C_RETURN(mkFalse());
  }
}

#define MAX_LINE_SIZE 1024
#ifndef PRELUDE_PATH
#define PRELUDE_PATH PRELSRCDIR
#endif

char *paths[] = {0, 0, 0}; /* Filled in by init routine */
char *prelude_path = NULL;

char *preldirs[] = 
    {"Prelude", "PreludeIO", "PreludeList", "PreludeText", "Ratio", 
     "System", "Text", "Array", "Char", "Complex", "Directory", "IO", 
     "Interrupt", "Ix", "List", "LowB", "Maybe", "Monad", "NonStd", 
     "PackedString", NULL};

void sendFile(FILE *sock, char *filename)
{
    FILE *file = NULL;
    char *ch;
    char path[128];
    char s[MAX_LINE_SIZE];
    int i;

    if ((ch = strchr(filename, '\n')) != NULL)
	*ch = '\0';

    if ((ch = strchr(filename, '\r')) != NULL)
	*ch = '\0';

    i = 0;
    while (file == NULL && paths[i] != NULL) {
	sprintf(&path[0], "%s/%s", paths[i], filename);
	file = fopen(path, "r");
	i++;
    }
    i = 0;
    while (file == NULL && preldirs[i] != NULL) {
	sprintf(&path[0], "%s/%s/%s", prelude_path, preldirs[i], filename);
	file = fopen(path, "r");
	i++;
    }
    if (file == NULL) {
	/* Better way of giving an error message here, please!!! */
	sprintf(&path[0], "Couldn't open source file %s.\n", filename);
	ToDebugger(sock, path);
	ToDebugger(sock, "<EOF>\n");
	return;
    }
    while (fgets(s, MAX_LINE_SIZE, file) != NULL)
	ToDebugger(sock, s);
    ToDebugger(sock, "<EOF>\n");
}

int getline(FILE *sock, char *s)
{
    if (fgets(s, MAX_LINE_SIZE, sock) == NULL) {
	if (terminated) {
	    fprintf(stderr, "Socket closed. Exiting.\n");
	    exit(1);
	}
	return FALSE;
    }
    /* fprintf(stderr, "got: %s\n", s); */
    return TRUE;
}

void loop(FILE *sock, NodePtr t)
{
    NodePtr nt;
    int refnr;
    char str[MAX_LINE_SIZE];
    int done = FALSE;
#if 0
    t = shortCircuitSelectors(t);
    t = GET_POINTER_ARG1(t, 1);
#endif
#if 0
    dumpTrace(sock, t);
#endif
    while (!done) {
	if (!getline(sock, &str[0]))
	    break;
	/*fprintf(stderr, "command: %s\n", str);*/
	switch (str[0]) {
	case 'G':
	    if (!getline(sock, &str[0]))
		done = TRUE;
	    else {
		sscanf(str, "%d", &refnr);
		nt = ncRef(refnr);
		dumpTrace(sock, nt);
	    }
	    break;
	case 'I':
	    if (!getline(sock, &str[0]))
		done = TRUE;
	    else {
		    /*fprintf(stderr, "Icommand: %s\n", str);*/
		sscanf(str, "%d", &refnr);
		nt = ncRef(refnr);
		/* Must be an indirection!!! */
		if (CONINFO_NUMBER(*nt) != TagInd) {
		    fprintf(stderr, "Expected TagInd, got %s\n",
			    TAGSTR[CONINFO_NUMBER(*nt)]);
		    exit(1);
		}
		nt = GET_POINTER_ARG1(nt, 1);
		dumpTrace(sock, nt);
	    }
	    break;
	case 'R':
	    if (!getline(sock, &str[0]))
		done = TRUE;
	    else {
		sscanf(str, "%d", &refnr);
		nt = ncRef(refnr);
		dump(sock, DEFAULT_DEPTH, nt);
		ToDebugger(sock, "\n");
	    }
	    break;
	case 'N':
	    if (!getline(sock, &str[0]))
		done = TRUE;
	    else {
		if (isRoot(nt)) {
		    ToDebugger(sock, "_\n");
		} else {
		    dump(sock, DEFAULT_DEPTH, nt);
		    ToDebugger(sock, "\n");
		}
	    }
	    break;	    
	case 'F':
	    if (!getline(sock, &str[0]))
		done = TRUE;
	    else {
		sendFile(sock, str);
	    }
	    break;
	}
    }
}

extern timer gcTime,totalTime,runTime;

void displayTime()
{
    double tt;
    extern NodePtr Hp;
    
    finishGc(Hp,1);
  
    timerStop(&totalTime);
    timerStop(&runTime);
    tt = (double)totalTime.l/(double)HZ;
    if(totalTime.h)
	fprintf(stderr,"32 bit timer not enough! Total time wrapped around %d times.\n",totalTime.h);
    fprintf(stderr,"Total time = %7.2f\n",tt);
    if(gcTime.h)
	fprintf(stderr,"32 bit timer not enough! Gc time wrapped around %d times.\n",gcTime.h);
    fprintf(stderr,"Gc time    = %7.2f\n",(double)gcTime.l/(double)HZ);
    if(runTime.h)
	fprintf(stderr,"32 bit timer not enough! Run time wrapped around %d times.\n",runTime.h);
    fprintf(stderr,"Run time   = %7.2f\n",(double)runTime.l/(double)HZ);
}


typedef struct _IdEntry {
    int constr;
#ifdef PROFILE
    int profInfo[EXTRA];
#endif
    char *srcmod;
    int srcpos;
    char *name;
    int pri;
} IdEntry;

typedef struct _ModInfo {
    char *srcfile;
    IdEntry *idtable;
    struct _ModInfo **modinfo;
    char *modname;
} ModInfo;

showDbgInfo(ModInfo *modInfo)
{
    IdEntry *identry;
    int r, c, i;
 
#if 1
    fprintf(stderr, "%s(%s)\n", modInfo->srcfile, modInfo->modname);
#endif
    identry = modInfo->idtable;
    while (identry->constr != 0) {
	r = identry->srcpos / 10000;
	c = identry->srcpos % 10000;
#if 1
	fprintf(stderr, "  %c %s %d:%d\n",
		CONINFO_NUMBER(identry->constr) == NTId ? 'S' : 'T',
		identry->name, r, c);
#endif
	identry++;
    }
    i = 0;
    while (modInfo->modinfo[i] != NULL) {
	showDbgInfo(modInfo->modinfo[i++]);
    }
}

int zero = 0;
#define NULL_ID_TABLE (IdEntry*)&zero
#define NULL_MOD_TABLE (ModInfo **)&zero

ModInfo NMOD__Apply1 = {"_Apply1", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD__Apply2 = {"_Apply2", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD__Apply3 = {"_Apply3", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD__Apply4 = {"_Apply4", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD_PreludeBuiltin = {"PreludeBuiltin", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD_PreludeDebug = {"PreludeDebug", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD__EqInteger = {"_EqInteger", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};

extern ModInfo *MODULE_Main;
extern ModInfo NMOD_Prelude;

#define TRUST CONSTR(NTTrusted, 3, 3)
#define SUSPECT CONSTR(NTId, 3, 3)

ModInfo *findModule(char *modname, ModInfo *modinfo)
{
    ModInfo *found;
    int i;

    if (modinfo == NULL) {
      fprintf(stderr, "findModule: Reached null when looking for %s\n", modname);
      exit(1);
    }
#if 0
    fprintf(stderr, "Looking for %s in %s\n", modname, modinfo->srcfile);
#endif
    if (strcmp(modinfo->srcfile, modname) == 0)
	return modinfo;
    i = 0;
    if (modinfo->modinfo == NULL) {
      fprintf(stderr, "findModule: modinfo->modinfo == NULL !!!");
      exit(1);
    }
    while (modinfo->modinfo[i] != NULL) {
	found = findModule(modname, modinfo->modinfo[i]);
	if (found != NULL)
	    return found;
	i++;
    }
    return NULL;
}

int isConstr(char *name) 
{
  int ch = name[0];
  return !((ch >= 'a' && ch <= 'z') || ch == '_');
}

void changeTrustedness(ModInfo *modInfo, char *fun, int constr)
{
   IdEntry *identry = modInfo->idtable;
   int changed = 0;

   fprintf(stderr, "Changing trustedness for %s (%s)\n", modInfo->modname, 
	   fun == NULL ? "all functions" : fun);
   while (identry->constr != 0) {
     fprintf(stderr, "Module: %s\n", identry->srcmod);
     fprintf(stderr, "Checking %s\n", identry->name);
     if ((fun == NULL) || (strcmp(fun, identry->name) == 0)) {
       changed++;
       if (!(constr == TRUST && isConstr(identry->name)))
	 identry->constr = constr;
     }
     identry++;
   }
   if ((fun != NULL) && (changed == 0)) {
       fprintf(stderr, "Couldn't find %s in %s\n", fun, modInfo->srcfile);
   }
}

void changeTrustednessRecursively(ModInfo *modInfo, int constr)
{
    int i;

#if 0
    fprintf(stderr, "%s(%s)\n", modInfo->srcfile, modInfo->modname);
#endif
    changeTrustedness(modInfo, NULL, constr);
    i = 0;
    while (modInfo->modinfo[i] != NULL) {
	changeTrustednessRecursively(modInfo->modinfo[i++], constr);
    }
}

void trustModule(char *mod, char *fun, int recursively)
{
    ModInfo *modInfo = findModule(mod, MODULE_Main);
    
    if (modInfo == NULL)
        modInfo = findModule(mod, &NMOD_Prelude);

    if (modInfo != NULL) {
        if (recursively) {
	    changeTrustednessRecursively(modInfo, TRUST);
	} else {
	    changeTrustedness(modInfo, fun, TRUST);
        }
    } else {
	fprintf(stderr, "trustModule: Cannot find module %s\n", mod);
    }
}

void suspectModule(char *mod, char *fun, int recursively)
{
    ModInfo *modInfo = findModule(mod, MODULE_Main);
    if (modInfo == NULL) {
        modInfo = findModule(mod, &NMOD_Prelude);
    }
    if (modInfo != NULL) {
        if (recursively) {
	    changeTrustednessRecursively(modInfo, SUSPECT);
	} else {
	    changeTrustedness(modInfo, fun, SUSPECT);
        }
    } else {
	fprintf(stderr, "suspectModule: Cannot find module %s\n", mod);
    }
}

int startDbg(NodePtr nodeptr, int exitok)
{
    FILE *sock;
    int i, ch;
    NodePtr np, n;
    char str[16], *s;
    ModInfo *modInfo = MODULE_Main;
    extern int traceQuit;
    extern int traceStat;
    extern int traceShow;

    /* Reset signal handlers */
    signal(SIGINT, SIG_DFL);
    signal(SIGQUIT, SIG_DFL);

    if (0) {
        showDbgInfo(modInfo);
	showDbgInfo(&NMOD_Prelude);
    }
    if (traceStat) {
	displayTime();
	fprintf(stderr, "%d reductions\n", reductions);
    }
    if (traceQuit) {
	fprintf(stderr, "%d reductions (%d TT, %d TS, %d ST, %d SS)\n", 
		reductions, redTT, redTS, redST, redSS);
    } else { 
	paths[0] = getcwd(NULL, 160);
	paths[1] = getenv("TRACE_SOURCEPATH");
	s = getenv("TRACE_PRELUDEPATH");
	prelude_path = s != NULL ? s : PRELUDE_PATH;

	ncInit();
	if (!(sock = connectToDebugger())) {
	    fprintf(stderr, "Couldn't connect to redex trail browser\n");
	    exit(1);
	}
	fprintf(stderr, "Haskell redex trail browser connected\n");

	if (exitok) {
	    otElement *ote;
	    ToDebugger(sock, "Output\n");
	    sprintf(&str[0], "%d\n", otSize());
	    ToDebugger(sock, str);
	    for (i = 1; i <= otSize(); i++) {
		ote = otRef(i);
		/* fprintf(stderr, "Sending char %d '%c'\n", i, ote->ch); */
		dumpOutput(sock, ote->ch, ote->trace);
	    }
	    ToDebugger(sock, "\n");
	} else {
	    ToDebugger(sock, "Error\n");
	    dumpTrace(sock, nodeptr);
	}
	/*nodeptr = otRef(1);*/
	/*IND_REMOVE(nodeptr);*/
	loop(sock, nodeptr);
	fclose(sock);
    }
}

void sigquit_handler()
{
    fprintf(stderr, "%d reductions (%d TT, %d TS, %d ST, %d SS)\n", 
	    reductions, redTT, redTS, redST, redSS);
    signal(SIGQUIT, sigquit_handler);
}

void ctrl_C_handler()
{
    if (interrupted) {
	fprintf(stderr, "%d reductions (%d TT, %d TS, %d ST, %d SS)\n", 
		reductions, redTT, redTS, redST, redSS);
	exit(1);
    } else {
	interrupted++;
	signal(SIGINT, ctrl_C_handler);
    }
}

C_HEADER(cInitializeDebugger)
{
    NodePtr nodeptr;
    char *regexp;

    add_user_gc(otMark, otFlip);
    signal(SIGQUIT, sigquit_handler);
    signal(SIGINT, ctrl_C_handler);

    trace_enter = getenv("TRACE_ENTER") != NULL;

    if ((regexp = getenv("TRACE_BPREGEXP")) != NULL) { 
	char *errmsg = (char *)re_comp(regexp);
	if (errmsg != NULL) {
	    fprintf(stderr, "Bad regular expression in TRACE_BPREGEXP: %s", 
		    errmsg);
	    exit(1);
	}
	trace_bpregexp = TRUE;
    }
    
    trace_singlestep = getenv("TRACE_STEP") != NULL;

#if 0
    fprintf(stderr, "Initializing... enter=%d bp=%d step=%d\n", 
	    trace_enter, trace_bpregexp, trace_singlestep);
#endif
    nodeptr = C_GETARG1(1);
    IND_REMOVE(nodeptr);
    C_RETURN(GET_POINTER_ARG1(nodeptr,1));
}

/* cConnectToServer :: E Trace -> () */
C_HEADER(cConnectToServer)
{
    int constr;
    NodePtr nodeptr;
    FILE *sock;

    if (!(sock = connectToDebugger())) {
	fprintf(stderr, "Couldn't connect to redex trail browser\n");
	exit(1);
    }
    ncInit();
    otInit();
    /*fprintf(stderr, "setvbuf: %d\n", setvbuf(sock, NULL, _IONBF, BUFSIZ));*/

    fprintf(stderr, "Haskell redex trail browser connected...\n");
    nodeptr = C_GETARG1(1);
    IND_REMOVE(nodeptr);
    /* constr = CONINFO_NUMBER(*nodeptr);*/
    /* fprintf(stderr, "constr = %d(np=0x%x)\n", constr, *nodeptr);*/
    ToDebugger(sock, "Output\n");
    loop(sock, nodeptr);
    C_RETURN(mkUnit());
}

