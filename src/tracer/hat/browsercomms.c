#include "browsercomms.h"

#define	MAX_LINE_SIZE	1024
#define	MAX_SYM_SIZE	128


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
 * Dump a source reference.
 * An SR contains a file name and a position in that file.
 * A zero pointer is a place-holder constructor for "no SR".
 **/
void
dumpSRToBrowser(FILE *sock, FileOffset t)
{
    static char str[256];
    SrcRef *sr;

    sr = readSRAt(t);
    if (sr) {
      sprintf(str,"\"%s\" %d ",sr->srcname,(sr->line*10000+sr->column));
      ToBrowser(sock, str);	    
      free(sr);
    } else {
      ToBrowser(sock, "* ");
    }
}



/* We have a TAp in hand, and just want to know whether it is an application
 * of case, if, or guard.  Returns 0==no, value=yes.
 */
int
checkForCaseIfGuard(FileOffset fo)
{
    char c;
    FileOffset fn;
    Ident *id;

    freadAt(fo,&c,sizeof(char),1,HatFile);
    if (c!=0x0) {
      fprintf(stderr,"%s: expected a TAp descriptor at position 0x%x\n"
                    ,"checkCaseIfGuard",fo);
      exit(1);
    }
    fread(&c,sizeof(char),1,HatFile);		/* next char is arity */
  /*fprintf(stderr,"checkForCaseIfGuard: got arity %d\n",c);*/
  /*if (c!=2) return 0;				   which must be 2 */
    (void)readFO();				/* skip parent trace */
    fn  = readFO();				/* get fn trace ptr */
    freadAt(fn,&c,sizeof(char),1,HatFile);
    if (c!=TNm) return 0;			/* only interested in a TNm */
    (void)readFO(); 				/* skip parent trace */
    fn  = readFO();				/* get nmtype ptr */
    freadAt(fn,&c,sizeof(char),1,HatFile);	/* then nmtype tag */
  /*fprintf(stderr,"checkForCase: got tag 0x%x\n",c);*/
    switch (c&0x1f) {
      case NTCase:
      case NTIf:
      case NTGuard:
		return (c&0x1f); break;
      default:	return 0; break;
    }
}


/* followTrace() takes a trace pointer and skips over any TInd or TSat
 * nodes, returning the outermost "real" trace.  If it does encounter a
 * Sat or Indirection, the pbot and/or pind pointers are filled in as
 * extra return values.
 */
FileOffset
followTrace(FileOffset fo, FileOffset *pbot, int *pind)
{
    char c; int ref;
    FileOffset arg1, arg2;

    /* defaults */
    *pbot = (FileOffset)0;	/* the file pointer itself */
    *pind = 0;			/* a nodecache'd reference */

    while (1) {
      if (!fo) return fo;	/* when trace is Root */
      freadAt(fo,&c,sizeof(char),1,HatFile);
      switch (c) {
        case TInd:
            arg1=readFO();	/* fst of indirection */
            arg2=readFO();	/* snd of indirection */
	    if (*pind == 0) {
		ref = abs(ncFind(fo));
		if (ref == 0)
		    ref = ncInsert(fo, FALSE);
		*pind = ref;  /* pind points to indirection */
	    }
	    fo = arg2;
	    break;

	case TSatA:
	case TSatC:
            fo = readFO();
            break;
	case TSatB:
            *pbot = fo;		/* not sure about this line */
            fo = readFO();
            return fo;
            break;

	case TAp:
	case TNm:
	case THidden:
	    return fo;
	default:
	    fprintf(stderr, "followTrace: expected trace tag, got 0x%x).\n",c);
	    exit(1);
      }
    }
}

/* dumpRefToBrowser() sends a nodecache reference for a trace to the browser.
 * If the nodecache doesn't already contain the trace, we insert it, but
 * the trace is /not/ sent yet, just the reference.  This allows
 * recursive structures to be sent without problems.
 */
int
storeRef (FileOffset fo)
{
  if (fo==0)
    return 0;
  else {
    int ref = ncFind(fo);
    if (ref==0)
      return ncInsert(fo, FALSE);
    else if (ref<0)
      return abs(ref);
  }
}

void
dumpRefToBrowser(FILE *sock, FileOffset fo)
{
    int ref;
    static char str[16];

    ref = storeRef(fo);
    sprintf(str, " %d ", ref);
    ToBrowser(sock, str);
}

void
dumpOutputRefToBrowser(FILE *sock, FileOffset fo)
{
    int ref; char c;
    static char str[16];

    ref = storeRef(fo);
    freadAt(fo,&c,sizeof(char),1,HatFile);
    if (c==THidden) {
      fo = readFO();
      ref = storeRef(fo);
    }
    sprintf(str, " %d ", ref);
    ToBrowser(sock, str);
}


/* Character lists should be globbed up into whole strings in the browser.
 * This routine sets the global variable charListInProgress to ensure that
 * we know we are in the middle of a list.
 */
static FileOffset charListInProgress = 0;

void
charListP(FileOffset fo)
{
    FileOffset old=fo, bot=0, fn, arg1, arg2;
    int ind;
    char c;
    Ident *id;

    while (1) {
      freadAt(fo,&c,sizeof(char),1,HatFile);
      switch (c) {
	case TAp:
	  fread(&c,sizeof(char),1,HatFile);		/* get arity */
	  if (c!=2) return;				/* must be 2 */
	  (void)readFO();				/* skip parent */
	  fn  = readFO();				/* get fn trace */
	  arg1= readFO();				/* get arg1 trace */
	  arg2= readFO();				/* get arg2 trace */

	  fn = followTrace(fn, &bot, &ind);
	  if (bot != 0) return;			/* fn must be fully evaluated */
	  freadAt(fn,&c,sizeof(char),1,HatFile);
	  if (c!=TNm) return;				/* must be a TNm */
          (void)readFO();				/* skip parent */
          fn  = readFO();				/* get NmType ptr */
	  id  = readNmTypeAt(fn);
	  if (strcmp(id->idname,":")) return;		/* must be cons */

	  arg1 = followTrace(arg1, &bot, &ind);
	  if (bot != 0) return;
	  freadAt(arg1,&c,sizeof(char),1,HatFile);
	  if (c!=TNm) return;				/* must be a TNm */
          (void) readFO();				/* skip parent */
          arg1 = readFO();				/* get NmType ptr */
	  freadAt(arg1,&c,sizeof(char),1,HatFile);
	  if ((c&0x1f)!=NTChar) return;		   	/* must be a char */

	  old = fo;
	  fo = followTrace(arg2, &bot, &ind);		/* follow list tail */
	  if (bot != 0) return;
	  break;

	case TNm:
          (void)readFO();				/* skip parent */
          fn  = readFO();				/* get NmType ptr */
	  id  = readNmTypeAt(fn);
	  if (!strcmp(id->idname,"[]"))	 		/* must be nil */
	      charListInProgress = old;			/* to set flag */
	  return;
	default:
	  return;
      }
    }
}

void
dumpNodeToBrowser(FILE *sock, int level, FileOffset fo) 
{
  char c;
  int ref, ind;
  FileOffset bot;
  static char str[128];
  char *s;

  fo = followTrace(fo, &bot, &ind);

  if (!fo) {	/* found Root */
    fprintf(stderr, "found trace Root in dumpNodeToBrowser. Hmmm.\n");
    ToBrowser(sock, "P ");
    return;
  }

  if (bot != 0) {	/* found a bottom */
    ref = ncFind(bot);
    if (ref > 0) {
      sprintf(&str[0], "(R %d %d)", ref, ind);
      ToBrowser(sock, str);
    } else {
      ref = ncInsert(bot, TRUE);
      sprintf(&str[0], "(B %d ", abs(ref));
      ToBrowser(sock, str);
      dumpRefToBrowser(sock, fo);
      ToBrowser(sock, ")");
    }
    return;
  }

  ref = ncFind(fo);
  if ((charListInProgress == 0) && (--level < 0)) {
    ToBrowser(sock, "(D ");
    dumpRefToBrowser(sock, fo);
    ToBrowser(sock, ")");
    return;
  }

  freadAt(fo,&c,sizeof(char),1,HatFile);
  switch (c) {
    case TAp:
      if (level == 0) {
	 ToBrowser(sock, "(D ");
	 dumpRefToBrowser(sock, fo);
	 ToBrowser(sock, ")");
	 charListInProgress = 0;
	 return;
      }
      if (ref > 0) {
        sprintf(str, "(R %d %d)", abs(ref), ind);
        ToBrowser(sock, str);
	charListInProgress = 0;
      } else {
	int nmType=0, i=0;
	FileOffset parent, foExprs[20], foSR;

	fread(&c,sizeof(char),1,HatFile);		/* get arity */
	parent = readFO();				/* get parent */
	for (i=0; i<=c; i++) {				/* get fn + args */
	  foExprs[i] = readFO();
	}
	foSR = readFO();				/* get SR ptr */

	if (ref < 0)					/* update sent bit */
	  ncHist(abs(ref));				/* in node cache */
	else
	  ref = ncInsert(fo, TRUE);
	   
	nmType = checkForCaseIfGuard(fo);
      /*fprintf(stderr,"checkForCaseIfGuard: %s\n",(nmType?"yes":"no"));*/
	if (nmType == 0) { /* Ordinary application */
	  if (!charListInProgress) {
	    charListP(fo);
	  }
	  sprintf(str,"(A%s %d ",charListInProgress==0?"":"s",abs(ref));
	  if (charListInProgress == fo)  
	    charListInProgress = 0; /* We're done with the string */

	  ToBrowser(sock, str);
	  dumpSRToBrowser(sock, foSR);

	  ToBrowser(sock, "(");
	  dumpNodeToBrowser(sock, level, foExprs[0]);
	  for (i=1; i<=c; i++) {
	    ToBrowser(sock, " ");
	    dumpNodeToBrowser(sock, level, foExprs[i]);
	  }
	  ToBrowser(sock, ")");
	  dumpRefToBrowser(sock, parent);
	  sprintf(str, " %d)", ind);
	  ToBrowser(sock, str);

	} else { /* case/if/guard */
	  sprintf(str,"(C %d %d ",nmType==NTIf?0:nmType==NTCase?1:2,abs(ref));
	  ToBrowser(sock, str);
	  dumpSRToBrowser(sock, foSR);
	  dumpNodeToBrowser(sock, level, foExprs[1]);
	  dumpNodeToBrowser(sock, level, parent);
	  sprintf(str, " %d)", ind);
	  ToBrowser(sock, str);	       
        }
      } break;

    case TNm:
      if (ref > 0) {
        sprintf(str, "(R %d %d)", ref, ind); 
        ToBrowser(sock, str);
      } else {
	char *mod, *name;
	int defpos, pri;
	FileOffset parent, nmtype, sr;
        Ident *id;

	if (ref<0) 
	  ncHist(abs(ref));
	else
	  ref = ncInsert(fo, TRUE);

        parent = readFO();
        nmtype = readFO();
        sr     = readFO();

	sprintf(str, "(N %d ", abs(ref));
        ToBrowser(sock, str);
	dumpSRToBrowser(sock, sr);
	id = readNmTypeAt(nmtype);
	sprintf(str, "%s", id->srcname);
	ToBrowser(sock, str);
	ToBrowser(sock, " ");
	if (id->idname[0] == '\0')
	  ToBrowser(sock, "\"()\"");
	else
	  ToBrowser(sock, id->idname);
	sprintf(str," %d %d ",(id->defnline*10000+id->defncolumn),id->priority);
        ToBrowser(sock, str);
	free(id->idname);
	free(id->modname);
	free(id->srcname);
	free(id);
	dumpRefToBrowser(sock, parent);
	sprintf(str, " %d)", ind);
	ToBrowser(sock, str);
      } break;

    case THidden:
      ref = ncFind(fo);
      if (ref > 0) {
        sprintf(str, "(R %d %d)", ref, ind);
        ToBrowser(sock, str);
      } else {
        ref = ncInsert(fo, TRUE);
	sprintf(str, "(H %d ", abs(ref));
	ToBrowser(sock, str);
	fo = readFO();
	dumpRefToBrowser(sock, fo);
	ToBrowser(sock, ")");
      } break;

   default:
       fprintf(stderr, "dumpNodeToBrowser: unexpected 0x%x tag.\n",c);
       exit(1);
       break;
   }
}




void
dumpOutputToBrowser(FILE *sock) 
{
  int ind;  char c;
  FileOffset fo, bot;
  static char str[128];

  c = fgetc(OutputFile);
  while (c!=-1) {
    fread(&fo,sizeof(FileOffset),1,BridgeFile);
    fo = ntohl(fo);
    fo = followTrace(fo, &bot, &ind);
    if (bot != 0) {
      fprintf(stderr, "dumpOutput: character trace is bottom!\n");
      exit(1);
    }

    /* Transfer the character */
    sprintf(str, " %d ", c);
    ToBrowser(sock, str);
    /* Transfer its trace */
    dumpOutputRefToBrowser(sock, fo);

    c = fgetc(OutputFile);
  }
}

int
isRoot(FileOffset fo)
{
    return (fo==0);
}

void
dumpTraceToBrowser(FILE *sock, FileOffset fo)
{
    charListInProgress = 0;
    dumpNodeToBrowser(sock, DEFAULT_DEPTH, fo);
    ToBrowser(sock, "\n");
    fflush(sock);
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
    ToBrowser(sock, "\n<EOF>\n");
}

int
getline(FILE *sock, char *s)
{
    if (fgets(s, MAX_LINE_SIZE, sock) == NULL) {
	return FALSE;
    }
    return TRUE;
}

/* loop() is the main read-eval-print loop for "commands" from the browser.
 */
void
loop(FILE *sock, FileOffset fo)
{
    int refnr; char c;
    FileOffset next;
    char str[MAX_LINE_SIZE];
    int done = FALSE;

    while (!done) {
      if (!getline(sock, str))
        break;
      c = str[0];
      if (!getline(sock, &str[0]))
	done = TRUE;

      switch (c) {
	case 'G':
		sscanf(str, "%d", &refnr);
		next = ncRef(refnr);
		dumpTraceToBrowser(sock, next);
	        break;

	case 'I':
		sscanf(str, "%d", &refnr);
		next = ncRef(refnr);
		/* Must be an indirection!!! */
		freadAt(next,&c,sizeof(char),1,HatFile);
		if (c != TInd) {
		    fprintf(stderr, "Expected TInd, got 0x%x\n",c);
		    exit(1);
		}
		next = readFO();
		dumpTraceToBrowser(sock, next);
	        break;

	case 'R':
		sscanf(str, "%d", &refnr);
		next = ncRef(refnr);
		dumpNodeToBrowser(sock, DEFAULT_DEPTH, next);
		ToBrowser(sock, "\n");
	        break;

	case 'N':
		if (!next) {	/* Root */
		    ToBrowser(sock, "_\n");
		} else {
		    dumpNodeToBrowser(sock, DEFAULT_DEPTH, next);
		    ToBrowser(sock, "\n");
		}
	        break;	    

	case 'F':
		sendFile(sock, str);
	        break;
	}
    }
}


int
main (int argc, char** argv)
{
    FILE *sock;
    int i;
    char str[16], *s;
    FileOffset fo;

    initialise(argc,argv);

    paths[0] = getcwd(NULL, 160);
    paths[1] = getenv("TRACE_SOURCEPATH");
    s = getenv("TRACE_PRELUDEPATH");
    prelude_path = s != NULL ? s : PRELUDE_PATH;

    ncInit();

    if (!(sock = waitForBrowserConnection())) {
      fprintf(stderr, "Couldn't connect to redex trail browser\n");
      exit(1);
    }
    fprintf(stderr, "Haskell redex trail browser connected\n");

    if (errorRoot&&!ignoreErrors) {
      ToBrowser(sock, "Error\n");
      dumpTraceToBrowser(sock, errorRoot);
      loop(sock, errorRoot);
    } else {
      ToBrowser(sock, "Output\n");
      sprintf(str, "%d\n", outputsize);
      ToBrowser(sock, str);
      dumpOutputToBrowser(sock);
      ToBrowser(sock, "\n");
      loop(sock, 0);
    }

    fclose(sock);
    finalise();
}

