#include "ui.h"
#include "ident.h"
#include "fileformat.h"


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
extern int exit_code;

#define	MAX_LINE_SIZE	1024
#define	MAX_SYM_SIZE	128

C_HEADER(fatal)
{
  CNmType* nt;
  CTrace* t;
  FileOffset fo;
  fprintf(stderr, "No match in pattern.\n");
  terminated = TRUE;
  updateSatBs();
  updateSatCs();
  nt = primNTCString("No match in pattern.");
  fo = nt->ptr;
  fseek(HatFile,8+sizeof(FileOffset),SEEK_SET);
  fwrite(&fo, sizeof(FileOffset), 1, HatFile);
  t = (CTrace*)C_GETARG1(1);
  fo = t->ptr;
  fseek(HatFile,8,SEEK_SET);
  fwrite(&fo, sizeof(FileOffset), 1, HatFile);
  exit_code = 1;
  haskellEnd();
  exit(1);
}

void
dbg_blackhole()
{
  CNmType* nt;
  CTrace* t;
  FileOffset fo;
  fprintf(stderr, "Black hole detected.\n");
  terminated = TRUE;
  updateSatBs();
  updateSatCs();
  nt = primNTCString("Black hole detected.");
  fo = nt->ptr;
  fseek(HatFile,8+sizeof(FileOffset),SEEK_SET);
  fwrite(&fo, sizeof(FileOffset), 1, HatFile);
  t = (CTrace*)dbg_last_trace;
  fo = t->ptr;
  fseek(HatFile,8,SEEK_SET);
  fwrite(&fo, sizeof(FileOffset), 1, HatFile);
  exit_code = 2;
  haskellEnd();
  exit(2);
}


/**
 * Check if the node pointed to by nodeptr is evaluated.
 * Returns:
 *    EVALUATED, if nodeptr points to an evaluated node.
 *    EVALUATING, if nodeptr points to a node that is under evaluation.
 *	When the program has finished, no nodes should be under evaluation.
 *	The interpretation is therefore that the node value is bottom. In
 *	this case, the bot arg is filled in with a pointer to the bottom
 *	value.
 *    CLOSURE, if nodeptr points to an unevaluated value (that is not
 *	under evaluation).
 **/
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

/* int refnr = 1; */
/* (not required) */

/**
 * Wait for a Redex Trace Browser to connect on a predefined port.
 * Return a file stream connected to the browser.
 **/
FILE *
waitForBrowserConnection()
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
	i = 0;
	while (i < DBGMAXTRIES && 
	       listen(s = in_bind_ne(port+i, SOCK_STREAM), 5))
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
    } else {
	fdesc = atoi(fdesc_str);
    }
    return fdopen(fdesc, "rb+");
}

#if 0
/* this variable and function are believed obsolete */
ForeignObj *sockcdata = NULL;

/* cGetDbgSocket :: () -> Socket */
C_HEADER(cGetDbgSocket)
{
    if (sockcdata == NULL) { /* Socket not yet opened */
	FILE *fp;
		
	if (fp = waitForBrowserConnection()) {
	    FileDesc a;

	    a.fp = fp;
	    a.bm = _IOFBF;
	    a.size = -1;

	    sockcdata = allocForeignObj(&a,gcFile,gcNone);
	    /* fprintf(stderr, "Debug socket successfully opened!\n");*/
	} else {
	    fprintf(stderr, "Couldn't fdopen dbg socket!\n");
	    exit(-1);
	}
    }
    C_RETURN(mkCInt((int)sockcdata));
}
#endif

/**
 * Send a string to the browser.
 **/
void 
ToBrowser(FILE *sock, char *s)
{
    int fn = fileno(sock);
    int n = write(fn, s, strlen(s));
    if (n != strlen(s)) {
       fprintf(stderr, "Couldn't write %s, errno=%d\n", s, errno);
       perror("Error: ");
       exit(1);
    }
  /*fprintf(stderr,"%s",s);	-- See what's happening on the socket */
}

/**
 * If profiling, some extra information is available. This function prints
 * that information. Used when debugging the tracer.
 **/
void
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

/**
 * Get information about a node, and fill in variables with source
 * reference and priority (for infix operators) information.
 **/
void 
showSymbol(NodePtr t, char **pmodule, char **pname, int *pdefpos, int *ppri)
{
    int constr, ch, len;
    static char str[MAX_SYM_SIZE];
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
        if (CONINFO_LARGESIZEU(*np) == 0)
          sprintf(&str[0], "0");
        else if (CONINFO_LARGESIZEU(*np) == 1)
          if (CONINFO_LARGEEXTRA(*np))
	    sprintf(&str[0], "-%d", GET_INT_VALUE(np));
          else
	    sprintf(&str[0], "%d", GET_INT_VALUE(np));
        else
	  sprintf(&str[0], "<Integer>");
	*pmodule = strdup("Prelude");
	*pname = strdup(str);
	break;
    case NTFloat:
	np = GET_POINTER_ARG1(t, 1);
	IND_REMOVE(np);
	sprintf(&str[0], "%.6f", (double)get_float_value(np));
	*pmodule = strdup("Prelude");
	*pname = strdup(str);
	break;
    case NTDouble:
	np = GET_POINTER_ARG1(t, 1);
	IND_REMOVE(np);
	sprintf(&str[0], "%.15f", get_double_value(np));
	*pmodule = strdup("Prelude");
	*pname = strdup(str);
	break;
    case NTTrusted:
    case NTId:
	*pmodule = strdup(((ModInfo *)GET_POINTER_ARG1(t, 1))->modname);
	*pname =  strdup((char *)GET_POINTER_ARG1(t, 3)); /*&t[1+EXTRA+2]);*/
	*pdefpos = GET_VALUE_ARG1(t, 2);
	*ppri = GET_VALUE_ARG1(t, 4);
	break;
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
	*pname = strdup("ß");
	break;
    case NTContainer:
	*pmodule = strdup("Unknown");
	*pname = strdup("?");
	break;
    case NTCString:
	*pmodule = strdup("Unknown");
/*	{ Coninfo ci;
          np = GET_POINTER_ARG1(t, 1);
          ci = GET_CONINFO(np);
          len = CONINFO_LARGESIZEU(ci)*sizeof(Node) - CONINFO_LARGEEXTRA(ci);
	  *pname = malloc(3+len);
	  sprintf(*pname, "'%s'", (char *)&np[1+EXTRA]);
        }
*/
        np = GET_POINTER_ARG1(t, 1);
	sprintf(&str[0],"<%d>",GET_INT_VALUE(np));
	*pname = strdup(str);
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

/**
 * Dump a source reference.
 * Source references come in two flavours.
 * An SR3 contains a file name and a position in that file.
 * An SR is just a place-holder constructor.
 **/
void
dumpSR(FILE *sock, NodePtr t)
{
    static char str[128];
    int rowcol, row, col;
    if (t) IND_REMOVE(t);
    if (t && (GET_TAG(t)==CON_TAG)) {
	Coninfo cinfo = GET_CONINFO(t);
	if (CONINFO_NUMBER(cinfo) == 2) { /* SR3 */
            ModInfo *mod;
            mod = (ModInfo*)GET_POINTER_ARG1(t,2);
	    rowcol = (int)t[1+EXTRA];
	    sprintf(&str[0], "\"%s\" %d ", (char *)mod->srcfile, rowcol);
	    ToBrowser(sock, str);	    
	} else if (CONINFO_NUMBER(cinfo) == 0) { /* SR */
	    ToBrowser(sock, "* ");
	} else {
	    fprintf(stderr, "\ncnr = %d\n", CONINFO_NUMBER(cinfo));
	    ToBrowser(sock, "* ");
	}
    } else {
	fprintf(stderr, "Bad SR\n");
	/*prGraph(t, 3, 3); */
	fprintf(stderr, "\n");
	ToBrowser(sock, "* ");
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

/* Show the entire trace structure.  Warning: potentially very large! */
/* Used only for debugging the RTS itself. */
void
showTrace(NodePtr t, int level)
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

int
checkForCaseIfGuard(NodePtr t) /* t must be a TagAp */
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

#define DEBUG_FT_NOT

NodePtr
followTrace(NodePtr t, NodePtr *pbot, int *pind)
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

void
getRef(FILE *sock, NodePtr t)
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
    ToBrowser(sock, str);
}

static NodePtr charListInProgress = NULL;

void
charListP(NodePtr t)
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

void
dump(FILE *sock, int level, NodePtr t) 
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
           ToBrowser(sock, str);
       } else {
	   ref = ncInsert(bot, TRUE);
	   sprintf(&str[0], "(B %d ", abs(ref));
	   ToBrowser(sock, str);
	   getRef(sock, t);
	   ToBrowser(sock, ")");
       }
       return;
   }

   ref = ncFind(t);
   if ((charListInProgress == NULL) && (--level < 0)) {
       ToBrowser(sock, "(D ");
       getRef(sock, t);
       ToBrowser(sock, ")");
       return;
   }

   constr = CONINFO_NUMBER(*t);
   switch (constr) {
   case TagAp:
       if (level == 0) {
	   ToBrowser(sock, "(D ");
	   getRef(sock, t);
	   ToBrowser(sock, ")");
	   charListInProgress = NULL;
	   return;
       }
       if (ref > 0) {
	   sprintf(&str[0], "(R %d %d)", abs(ref), ind);
           ToBrowser(sock, str);
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

	       ToBrowser(sock, str);
	       dumpSR(sock, GET_POINTER_ARG1(t,3));
	       ToBrowser(sock, "(");
	       while (CONINFO_PSIZE(*ts) == 2) {
		   if (first++ > 1)
		       ToBrowser(sock, " ");
		   dump(sock, level, GET_POINTER_ARG1(ts, 1));
		   ts = shortCircuitSelectors(GET_POINTER_ARG1(ts, 2));
	       }
	       ToBrowser(sock, ")");
	       getRef(sock,  GET_POINTER_ARG1(t, 1));
	       sprintf(&str[0], " %d)", ind);
	       ToBrowser(sock, str);
	   } else {
	       sprintf(&str[0], "(C %d %d ", nmType==NTIf?0:nmType==NTCase?1:2, abs(ref));
	       ToBrowser(sock, str);
	       dumpSR(sock, GET_POINTER_ARG1(t,3));
	       assert(CONINFO_PSIZE(*ts) == 2);
	       ts = shortCircuitSelectors(GET_POINTER_ARG1(ts, 2));
	       dump(sock, level, GET_POINTER_ARG1(ts, 1));
	       dump(sock, level, GET_POINTER_ARG1(t, 1));
	       sprintf(&str[0], " %d)", ind);
	       ToBrowser(sock, str);	       
	   }
       }
       break;
   case TagNm:
       if (ref > 0) {
	   sprintf(&str[0], "(R %d %d)", ref, ind); 
           ToBrowser(sock, str);
      } else {
	   char *mod, *name;
	   int defpos, pri;
	   if (ref < 0) 
	       ncHist(t, abs(ref));
	   else
	       ref = ncInsert(t, TRUE);
	   sprintf(&str[0], "(N %d ", abs(ref));
           ToBrowser(sock, str);
	   dumpSR(sock, GET_POINTER_ARG1(t, 3));
	   showSymbol(GET_POINTER_ARG1(t, 2), &mod, &name, &defpos, &pri);
	   sprintf(&str[0], "%s", mod);
	   ToBrowser(sock, str);
	   ToBrowser(sock, " ");
	   if (name[0] == '\0')
	       ToBrowser(sock, "\"()\"");
	   else
	       ToBrowser(sock, name);
	   sprintf(&str[0], " %d %d ", defpos, pri);
	   ToBrowser(sock, str);
	   free(mod);
	   free(name);
	   getRef(sock,  GET_POINTER_ARG1(t, 1));
	   sprintf(&str[0], " %d)", ind);
	   ToBrowser(sock, str);
       }
       break;
   case TagInd:
       fprintf(stderr, "dump: TagInd\n");
       exit(1);
       break;
   case TagPruned:
       ToBrowser(sock, "P ");
       break;
   case TagRoot:
       /* Shouldn't happen */
       fprintf(stderr, "TagRoot in dump. Hmmm.\n");
       ToBrowser(sock, "P ");
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
           ToBrowser(sock, str);
       } else {
	   ref = ncInsert(t, TRUE);
	   sprintf(&str[0], "(H %d ", abs(ref));
	   ToBrowser(sock, str);
	   t = GET_POINTER_ARG1(t, 1);
	   IND_REMOVE(t);
	   getRef(sock, t);
	   ToBrowser(sock, ")");
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

void
dumperr(NodePtr t)
{
    dump(stderr, 3, t);
}

void
getOutputRef(FILE *sock, NodePtr t)
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
    ToBrowser(sock, str);
}

void
dumpOutput(FILE *sock, int ch, NodePtr t) 
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
   ToBrowser(sock, str);

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
	   ToBrowser(sock, str);
	   if (ind > 0) {
	       sprintf(&str[0], " %d ", ind);
	       ToBrowser(sock, str);
	   } else 
	       getRef(sock,  GET_POINTER_ARG1(t, 1));
	   /*fprintf(stderr, "Sending char '%c'\n", GET_INT_VALUE(c));*/
       } else {
	   fprintf(stderr, "dumpOutput: not a char\n");
	   sprintf(&str[0], "64 0 ");
	   ToBrowser(sock, str);
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
       ToBrowser(sock, str);
       break;
   }
#endif
}

int
isRoot(NodePtr t)
{
    t = shortCircuitSelectors(t);
    return CONINFO_NUMBER(*t) == TagRoot;
}

void
dumpTrace(FILE *sock, NodePtr t)
{
    /*ToBrowser(sock, "(");*/
    /*dumpChain(sock, 50, 10, t);*/
    charListInProgress = NULL;
    dump(sock, DEFAULT_DEPTH, t);
    ToBrowser(sock, "\n");
    fflush(sock);
}

NodePtr
listIndex(int n, NodePtr t)
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



#ifndef PRELUDE_PATH
#define PRELUDE_PATH PRELSRCDIR
#endif

char *paths[] = {0, 0, 0}; /* Filled in by init routine */
char *prelude_path = NULL;

char *preldirs[] = 
    {"Prelude", "PreludeIO", "PreludeList", "PreludeText", "Ratio", 
     "System", "Text", "Array", "Char", "Complex", "Directory", "IO", 
     "Interrupt", "Ix", "List", "LowT", "Maybe", "Monad", "NonStd", 
     "PackedString", NULL};

void
sendFile(FILE *sock, char *filename)
{
    FILE *file = NULL;
    char *ch;
    char path[MAX_SYM_SIZE];
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
	ToBrowser(sock, path);
	ToBrowser(sock, "<EOF>\n");
	return;
    }
    while (fgets(s, MAX_LINE_SIZE, file) != NULL)
	ToBrowser(sock, s);
    ToBrowser(sock, "<EOF>\n");
}

int
getline(FILE *sock, char *s)
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

void
loop(FILE *sock, NodePtr t)
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
		ToBrowser(sock, "\n");
	    }
	    break;
	case 'N':
	    if (!getline(sock, &str[0]))
		done = TRUE;
	    else {
		if (isRoot(nt)) {
		    ToBrowser(sock, "_\n");
		} else {
		    dump(sock, DEFAULT_DEPTH, nt);
		    ToBrowser(sock, "\n");
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

void
displayTime()
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



int
startDbg(NodePtr nodeptr, int exitok)
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

#if 0
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

        if (!exitok) {
            stackTrace(nodeptr);
        }

	if (!(sock = waitForBrowserConnection())) {
	    fprintf(stderr, "Couldn't connect to redex trail browser\n");
	    exit(1);
	}
	fprintf(stderr, "Haskell redex trail browser connected\n");

	if (exitok) {
	    otElement *ote;
	    ToBrowser(sock, "Output\n");
	    sprintf(&str[0], "%d\n", otSize());
	    ToBrowser(sock, str);
	    for (i = 1; i <= otSize(); i++) {
		ote = otRef(i);
		/* fprintf(stderr, "Sending char %d '%c'\n", i, ote->ch); */
		dumpOutput(sock, ote->ch, ote->trace);
	    }
	    ToBrowser(sock, "\n");
	} else {
	    ToBrowser(sock, "Error\n");
	    dumpTrace(sock, nodeptr);
	}
	/*nodeptr = otRef(1);*/
	/*IND_REMOVE(nodeptr);*/
	loop(sock, nodeptr);
	fclose(sock);
    }
#endif
}

void
sigquit_handler()
{
    fprintf(stderr, "%d reductions (%d TT, %d TS, %d ST, %d SS)\n", 
	    reductions, redTT, redTS, redST, redSS);
    signal(SIGQUIT, sigquit_handler);
}

void
ctrl_C_handler()
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

  /*add_user_gc(otMark, otFlip);*/
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

    if (!(sock = waitForBrowserConnection())) {
	fprintf(stderr, "Couldn't connect to redex trail browser\n");
	exit(1);
    }
    ncInit();
  /*otInit();*/
    /*fprintf(stderr, "setvbuf: %d\n", setvbuf(sock, NULL, _IONBF, BUFSIZ));*/

    fprintf(stderr, "Haskell redex trail browser connected...\n");
    nodeptr = C_GETARG1(1);
    IND_REMOVE(nodeptr);
    /* constr = CONINFO_NUMBER(*nodeptr);*/
    /* fprintf(stderr, "constr = %d(np=0x%x)\n", constr, *nodeptr);*/
    ToBrowser(sock, "Output\n");
    loop(sock, nodeptr);
    C_RETURN(mkUnit());
}


/***** Code not currently used *****/

#if 0

char *
showProfString(char *s)
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

::::::::::::(from showSymbol)
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

:::::::::: from dumpSR
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
		ToBrowser(sock, str);
	    }
	}
#endif


#endif
