/* hat-check: reads hat trace files, checking at least the basic format
 * other checks and information can be requested by options
 * Colin Runciman, University of York
 * Original version February 2001
 *   text output as default, with -v option for added verification check
 * 21 March 2001
 *   added -s option for statistics, made text an option (-a)
 * 28 March 2001
 *   added -r option to check proportion of nodes reachable
 * 6 April 2001
 *   -v extended to check for inappropriate zero pointers
 *   -n option added to request textual dump of single node
 *   when both -a & -r set show reachability of each node in text lines
 * 8 May 2001
 *   allow (but ignore) bits to mark SATs with no APP
 *   accept progname with or without .hat
 */

/* #include <unistd.h> */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <signal.h>

#define TR 0

#define APP 0
#define NAM 1
#define IND 2
#define HIDDEN 3
#define SATA 4
#define SATB 5
#define SATC 6

#define SATNOAPP 8

#define MD 1

#define SUSPECT 0
#define TRUSTED 1

#define NT 2

#define INT 0
#define CHAR 1
#define INTEGER 2
#define RATIONAL 3
#define FLOAT 4
#define DOUBLE 5
#define IDENTIFIER 6
#define CONSTRUCTOR 7
#define TUPLE 8
#define FUN 9
#define CASE 10
#define LAMBDA 11
#define DUMMY 12
#define CSTRING 13
#define IF 14
#define GUARD 15
#define CONTAINER 16
#define TOPIDENTIFIER 22

#define SR 3

#define HEADER 6
#define INVALID 7
#define BEYOND 8

/* Main driver and routines to providing basic interface to archive file.
 */

int f;               /* file descriptor for archive */
#define BUFSIZE 2048
char buf[BUFSIZE];   /* input buffer */
int n;               /* buf[0..n-1] filled */
int boff;            /* if n>0, boff in 0..n-1 and buf[boff] is current */
unsigned long foff;  /* if n>0, this is offset in f of buf[0] */
unsigned long nextoffset;

int vmode = 0;       /* verify -- check tag-types of pointer destinations */
int smode = 0;       /* statistics -- counts and space usage for node types */
int amode = 0;       /* ascii -- show archive in a `readable' text format */
int nmode = 0;       /* node -- show node at given offset in text format */
int rmode = 0;       /* reachable -- show how many nodes are reachable */
int xmode = 0;       /* exit mode -- cleanup after signal to halt */

unsigned filesize = 0; /* used in precondition for seeks ... */
struct stat statbuf;   /* ... to catch seek beyond EOF */

#define FILENAMESIZE 200
char filename[FILENAMESIZE];

/* byte buffer for use in reachability mark-phase */
#define BUFFERSIZE 100000
char buffer[BUFFERSIZE];

/* signal handler -- only installed for -r */
void restoretags(int signum) {
  fprintf(stderr, "hat-check cleaning up -- please wait\n");
  amode = 0; rmode = 0; smode = 0; vmode = 0;
  xmode = 1;
  lseek(f,0L,0);
  n = read(f, buf, BUFSIZE);
  boff = 0;
  foff = 0L;
  header();
  nodes();
  exit(1);
}

main (int argc, char *argv[])
{
  int i = 0;
  while (++i < argc-1) {
    if (strcmp(argv[i], "-v") == 0) {
      vmode = 1;
    } else if (strcmp(argv[i], "-s") == 0) {
      smode = 1;
    } else if (strcmp(argv[i], "-a") == 0) {
      amode = 1;
    } else if (strcmp(argv[i], "-n") == 0) {
      nmode = 1;
      sscanf(argv[++i], "0x%x", &nextoffset);      
    } else if (strcmp(argv[i], "-r") == 0) {
      smode = 1;
      rmode = 1;
    } else {
      badusage();
    }
  }
  if (i > argc-1) badusage();
  strcpy(filename, argv[i]);
  if (!strends(".hat", filename)) strcat(filename, ".hat");
  stat(filename, &statbuf);
  filesize = statbuf.st_size;
  f = open(filename, (rmode ? 2 : 0));
  if (f==-1) {
    fprintf(stderr, "cannot open trace file %s\n",filename);
    exit(1);
  }
  if (nmode) {
    if (nextoffset >= filesize) {
      fprintf(stderr, "-n 0x%x is beyond end of trace file\n", nextoffset);
      exit(1);
    }
    amode = 1; rmode = 0; smode = 0;
  } else {
    nextoffset = 0L;
  }
  if (rmode) {
    signal(SIGINT, restoretags);
    signal(SIGQUIT, restoretags);
    markfromheader(buffer);
    strcat(filename, ".bridge");
    markfromoutput(filename,buffer);
    lseek(f,0L,0);
  }
  lseek(f,nextoffset,0);
  n = read(f, buf, BUFSIZE);
  boff = 0;
  foff = nextoffset;
  if (nmode) {
    nextnode();
  } else {
    if (smode) initstats();
    header();
    nodes();
    if (amode && smode) putchar('\n');
    if (smode) reportstats();
  }
}

strends(char *e, char *s) {
  int d = strlen(s) - strlen(e);
  return d>=0 && strcmp(s+d, e)==0;
}

badusage() {
  fprintf(stderr,"usage: hat-check [-a] [-n <hexnode>][-r] [-s] [-v] prog-name\n");
  fprintf(stderr,"\t-a\tprint ascii text version of hat file\n");
  fprintf(stderr,"\t-n\tprint text for specified node only (disables -r, -s)\n");
  fprintf(stderr,"\t-r\tprint statistics about reachable nodes (implies -s)\n");
  fprintf(stderr,"\t-s\tprint statistics about frequency and size of nodes\n");
  fprintf(stderr,"\t-v\tverify tag types of pointer destinations\n");
  exit(1);
}

unsigned int count[4];       /* indexed by TR, MD, NT, SR */
unsigned long space[4];      /* ditto */
unsigned long headspace;     /* space oocupied by header */
unsigned int trcount[7];     /* indexed by APP, NAM, ... , SATC */
unsigned long trspace[7];    /* ditto */

unsigned int reachcount[4];
unsigned int reachtrcount[7];

initstats() {
  int k;
  for (k=0; k<4; k++) {
    count[k] = 0;
    reachcount[k] = 0;
    space[k] = 0L;
  }
  for (k=0; k<7; k++) {
    trcount[k] = 0;
    reachtrcount[k] = 0;
    trspace[k] = 0L;
  }
}

float pc(unsigned long i, unsigned long j) {
  return (float)((i*100.0)/j);
}

reportstats() {
  unsigned int grandcount = 1;
  unsigned int grandreachcount = 1;
  unsigned long grandspace = headspace;
  int k;
  for (k=0; k<4; k++) {
    grandcount += count[k];
    grandreachcount += reachcount[k];
    grandspace += space[k];
  }
  if (rmode) printf("%7s", "% Reach");
  printf("%10s   %-20s%12s%10s\n\n",
    "Number", "Description", "Bytes", "% Space");
  if (rmode) printf("%7.1f", pc(1,1));
  printf("%10u   %-20s%12u%10.1f\n",
    1, "header", headspace, pc(headspace,grandspace));
  if (rmode) printf("%7.1f", pc(reachtrcount[APP], trcount[APP]));
  printf("%10u   %-20s%12u%10.1f\n",
    trcount[APP], "TR application nodes", trspace[APP],
    pc(trspace[APP],grandspace));
  if (rmode) printf("%7.1f", pc(reachtrcount[NAM], trcount[NAM]));
  printf("%10u   %-20s%12u%10.1f\n",
    trcount[NAM], "TR name nodes", trspace[NAM], pc(trspace[NAM],grandspace));
  if (rmode) printf("%7.1f", pc(reachtrcount[IND], trcount[IND]));
  printf("%10u   %-20s%12u%10.1f\n",
    trcount[IND], "TR indirection nodes", trspace[IND], pc(trspace[IND],grandspace));
  if (rmode) printf("%7.1f", pc(reachtrcount[HIDDEN], trcount[HIDDEN]));
  printf("%10u   %-20s%12u%10.1f\n",
    trcount[HIDDEN], "TR hidden nodes", trspace[HIDDEN], pc(trspace[HIDDEN],grandspace));
  if (rmode) printf("%7.1f", pc(reachtrcount[SATA], trcount[SATA]));
  printf("%10u   %-20s%12u%10.1f\n",
    trcount[SATA], "TR type A SAT nodes", trspace[SATA], pc(trspace[SATA],grandspace));
  if (rmode) printf("%7.1f", pc(reachtrcount[SATB], trcount[SATB]));
  printf("%10u   %-20s%12u%10.1f\n",
    trcount[SATB], "TR type B SAT nodes", trspace[SATB], pc(trspace[SATB],grandspace));
  if (rmode) printf("%7.1f", pc(reachtrcount[SATC], trcount[SATC]));
  printf("%10u   %-20s%12u%10.1f\n",
    trcount[SATC], "TR type C SAT nodes", trspace[SATC], pc(trspace[SATC],grandspace));
  if (rmode) printf("%7.1f", pc(reachcount[MD], count[MD]));
  printf("%10u   %-20s%12u%10.1f\n",
    count[MD], "MD nodes", space[MD], pc(space[MD],grandspace));
  if (rmode) printf("%7.1f", pc(reachcount[NT], count[NT]));
  printf("%10u   %-20s%12u%10.1f\n",
    count[NT], "NT nodes", space[NT], pc(space[NT],grandspace));
  if (rmode) printf("%7.1f", pc(reachcount[SR], count[SR]));
  printf("%10u   %-20s%12u%10.1f\n",
    count[SR], "SR nodes", space[SR], pc(space[SR],grandspace));
  { int w;
    putchar('\n');
    if (rmode) for (w=0; w<7; w++) putchar(' ');
    for (w=0; w<55; w++) putchar(w<13 ? ' ' : '-');
    putchar('\n');
  }
  if (rmode) printf("%7.1f", pc(grandreachcount,grandcount));
  printf("%10u   %-20s%12u%10.1f\n",
    grandcount, "whole trace file", grandspace, pc(grandspace,grandspace));
}

unsigned long byteoffset() {
  return foff + boff;
}

char nextbyte() {
  char c;
  if (n==0) { fprintf(stderr, "unexpected end of trace file"); exit(1); }
  c = buf[boff++];
  if (boff==n) {
    foff += n;
    n = read(f,buf,BUFSIZE);
    boff = 0;
  }
  return c;
}

int more() {
  return (n>0);
}
  
/* Routines to extract values encoded as one or more bytes.
 */

#define STRINGMAX 30
char stringbuf[STRINGMAX+1];

char *readstring() {
  int n = 0;
  char c;
  do {
    c = nextbyte();
    stringbuf[n++] = c;
  } while (c!='\0' && n<STRINGMAX);
  stringbuf[n] = '\0';
  return stringbuf;
}

typedef union {char byte[4];
               unsigned long ptrval;
	       long intval;
	       float floatval;} fourbytes;

fourbytes readfourbytes() {
  fourbytes slot;
  slot.byte[0] = nextbyte();
  slot.byte[1] = nextbyte();
  slot.byte[2] = nextbyte();
  slot.byte[3] = nextbyte();
  slot.ptrval = ntohl(slot.ptrval);
  return slot;
}

#define POSNMAX 30
char posnbuf[POSNMAX+1];

char *readposn() {
  unsigned long posn = readfourbytes().ptrval;
  sprintf(posnbuf, "line %u, column %u", posn/10000, posn%10000);
  return posnbuf;
}

unsigned long readpointer() {
  return readfourbytes().ptrval;
}

char readchar() {
  return nextbyte();
}

int readint() {
  return readfourbytes().intval;
}

#define INTEGERMAX 30
char integerbuf[INTEGERMAX+1];

char *readinteger() {
  int n = (signed char)(nextbyte());
  if (n<0) n=-n; // negative sign of value is signalled by negative number of bytes
  while (n-- > 0) (void)(readfourbytes());
  return "<integer>";  /* DUMMY FOR NOW */
}

#define RATMAX (2*INTEGERMAX+2)
char ratbuf[RATMAX+1];

char *readrational() {
  strcpy(ratbuf, readinteger());
  strcat(ratbuf, ":%");
  strcat(ratbuf, readinteger());
  return ratbuf;
}

float readfloat() {
  return readfourbytes().floatval;
}

double readdouble() {
  (void)(readfourbytes());
  (void)(readfourbytes());
  return 0.0;  /* DUMMY FOR NOW */ 
}

int hi3(char b) {
  return (int)(b>>5);
}

int lo5(char b) {
  return (int)(b&037);
}

int readarity() {
  return (int)(nextbyte());
}

#define FIXPRIMAX 8
char fixpribuf[FIXPRIMAX+1];

char *readfixpri() {
  int b = (int)(nextbyte());
  switch (b % 4) {
  case 0:
    sprintf(fixpribuf, "infix %d", b/4);
    break;
  case 1:
    sprintf(fixpribuf, "infixr %d", b/4);
    break;
  case 2:
    sprintf(fixpribuf, "infixl %d", b/4);
    break;
  case 3:
    sprintf(fixpribuf, "");
    break;
  }
  return fixpribuf;
}

char *tag2str(int tag) {
  switch (tag) {
  case TR: return "TR";
  case MD: return "MD";
  case NT: return "NT";
  case SR: return "SR";
  case HEADER: return "HEADER";
  case INVALID: return "INVALID";
  case BEYOND: return "beyond end of file";
  default: return "UNKNOWN";
  }
}   

int tagat(unsigned long offset) {
  char byte[1];
  int i;
  if (offset <= filesize) {
    lseek(f, offset, 0);
    i = read(f, byte, 1);
    lseek(f, foff + n, 0);
    if (rmode) cleartag(byte);
    return (i==1 ? hi3(byte[0]) : INVALID);
  } else return BEYOND;
}

void newtagat(char *t, unsigned long offset) {
  lseek(f, offset, 0);
  write(f, t, 1);
  lseek(f, foff + n, 0);
}

#define NONZERO 0
#define MAYBEZERO 1
   
void dopointer(int okzero,
               int requiretag, unsigned long requireoffset,
               int contexttag, unsigned long contextoffset) {
  if (vmode && !okzero && requireoffset == 0) {
      fprintf(stderr, "bad zero pointer in %s 0x%x\n",
	    tag2str(contexttag), contextoffset);     
  }
  if (vmode && requireoffset>0) {
    int t = tagat(requireoffset);
    if (t != requiretag) {
      fprintf(stderr, "tag at 0x%x is %s, not %s as %s at 0x%x implies\n",
            requireoffset, tag2str(t), tag2str(requiretag),
	    tag2str(contexttag), contextoffset);
    }
  }
  if (amode) printf("(%s 0x%x)", tag2str(requiretag), requireoffset);
}
  
#define ismarked(b) ((b)&0x80)

marktag(char *b) {
  *b |= 0x80;
}

cleartag(char *b) {
  *b &= 0x7F;
}

/* reading, checking and/or writing header and node information
 */
 
header() {
  { char *s = readstring(); if (amode) printf("%s", s); }
  if (amode) printf("\nEntry point: ");
  dopointer(MAYBEZERO, TR, readpointer(), HEADER, 0L);
  if (amode) printf("\nError message: ");
  dopointer(MAYBEZERO, NT, readpointer(), HEADER, 0L);
  if (amode) printf("\n");
  headspace = byteoffset();
}
  
nodes() {
  nextoffset = byteoffset();
  while (more()) {
    nextnode();
  }
}

nextnode() {
  unsigned long offset = nextoffset;
  char b = nextbyte();
  int marked;
  if (rmode || xmode) {
    marked = ismarked(b);
    if (marked) {
      cleartag(&b);
      newtagat(&b, offset);
    }
    if (amode) printf("%s", (marked ? "=> " : "   "));
  }
  {
    int k = hi3(b);
    if (k > SR) {
      fprintf(stderr, "strange high-bits tag %d at byte offset 0x%x\n",
                      k, offset);
      exit(1);
    } else if (smode) {
      count[k]++;
      if (rmode && marked) reachcount[k]++;
    }
    switch (k) {
    case TR: {
      int trk = lo5(b);
      if (trk & SATNOAPP) trk -= SATNOAPP;
      if (trk > SATC) {
        fprintf(stderr, "strange low-bits tag %d in TR 0x%x\n",
	        trk, offset);
        exit(1);
      } else if (smode) {
        trcount[trk]++;
	if (rmode && marked) reachtrcount[trk]++;
      }
      if (amode) printf("TR 0x%x: ", offset);
      switch (trk) {
      case APP:
        { int arity = readarity();
	  if (amode) printf(" Application %d \t",arity);
	  for (; arity-- > -2;)
	    dopointer(NONZERO, TR, readpointer(), TR, offset);
	}
	dopointer(MAYBEZERO, SR, readpointer(), TR, offset);
	break;
      case NAM:
        if (amode) printf(" Name          \t");
        dopointer(MAYBEZERO, TR, readpointer(), TR, offset);
	dopointer(NONZERO,   NT, readpointer(), TR, offset);
	dopointer(MAYBEZERO, SR, readpointer(), TR, offset);
	break;
      case IND:
        if (amode) printf(" Indirection   \t");
	dopointer(NONZERO,   TR, readpointer(), TR, offset);
	dopointer(MAYBEZERO, TR, readpointer(), TR, offset);
	break;
      case HIDDEN:
        if (amode) printf(" Hidden        \t");
	dopointer(NONZERO,   TR, readpointer(), TR, offset);
	break;
      case SATA:
        if (amode) printf(" SAT(A)        \t");
	dopointer(NONZERO,   TR, readpointer(), TR, offset);
	break;
      case SATB:
        if (amode) printf(" SAT(B)        \t");
	dopointer(NONZERO,   TR, readpointer(), TR, offset);
	break;
      case SATC:
        if (amode) printf(" SAT(C)        \t");
	dopointer(NONZERO,   TR, readpointer(), TR, offset);
	break;
      }
      if (smode) trspace[trk] += byteoffset() - offset;
      break;
    }
    case MD:
      if (amode) printf("MD 0x%x:  ", offset);
      switch (lo5(b)) {
          case SUSPECT: if (amode) printf("module (suspect)\t"); break;
          case TRUSTED: if (amode) printf("module (trusted)\t"); break;
          default:
	    fprintf(stderr, "strange low-bits tag %d in MD 0x%x\n",
	            lo5(b), offset);
	    exit(1);
      }
      { char *s = readstring(); if (amode) printf("%s\t", s); }
      { char *s = readstring(); if (amode) printf("\"%s\"", s); }
      break;
    case NT:
      if (amode) printf("NT 0x%x:  ", offset);
      switch (lo5(b)) {
      case INT:
        { int i = readint(); if (amode) printf("INT %d", i); }
	break;
      case CHAR:
        { char c = readchar(); if (amode) printf("CHAR '%c'", c); }
	break;
      case INTEGER:
        { char *i = readinteger(); if (amode) printf("INTEGER %s", i); }
	break;       
      case RATIONAL:
        { char *r = readrational(); if (amode) printf("RATIONAL %s", r); }
	break;
      case FLOAT:
        { float f = readfloat(); if (amode) printf("FLOAT %g", f); }
	break;
      case DOUBLE:
        { double d = readdouble(); if (amode) printf("DOUBLE %g", d); }
	break;
      case IDENTIFIER:
        { char *s = readstring(); if (amode) printf("identifier    \t%s ", s); }
	{ unsigned long modinfo = readpointer();
	  char *fp = readfixpri();
	  if (*fp!='\0' && amode) printf("%s ", fp);
	  dopointer(NONZERO, MD, modinfo, NT, offset);
	  { char *p = readposn(); if (amode) printf(" %s", p); }
        }
	break; 
      case TOPIDENTIFIER:
        { char *s = readstring(); if (amode) printf("toplevel ident\t%s ", s); }
	{ unsigned long modinfo = readpointer();
	  char *fp = readfixpri();
	  if (*fp!='\0' && amode) printf("%s ", fp);
	  dopointer(NONZERO, MD, modinfo, NT, offset);
	  { char *p = readposn(); if (amode) printf(" %s", p); }
        }
	break; 
      case CONSTRUCTOR:
        { char *s = readstring(); if (amode) printf("constructor   \t%s ", s); }
	{ unsigned long modinfo = readpointer();
	  char *fp = readfixpri();
	  if (*fp!='\0' && amode) printf("%s ", fp);
	  dopointer(NONZERO, MD, modinfo, NT, offset);
	  { char *p = readposn(); if (amode) printf(" %s", p); }
        }
	break; 
      case TUPLE:
	if (amode) printf("TUPLE");
	break;
      case FUN:
	if (amode) printf("FUN");
	break;
      case CASE:
	if (amode) printf("CASE");
	break;
      case LAMBDA:
	if (amode) printf("LAMBDA");
	break;
      case DUMMY:
	if (amode) printf("DUMMY");
	break;
      case CSTRING:
	{char *s = readstring(); if (amode) printf("CSTRING \"%s\"", s); }
	break;
      case IF:
	if (amode) printf("IF");
	break;
      case GUARD:
	if (amode) printf("GUARD");
	break;
      case CONTAINER:
	if (amode) printf("CONTAINER");
	break;
      default:
        fprintf(stderr, "strange low-bits tag %d in NT 0x%x\n",
	                lo5(b), offset);
        exit(1);
      }
      break;
    case SR:
      if (amode) printf("SR 0x%x:  Source reference  \t", offset);
      dopointer(NONZERO, MD, readpointer(), SR, offset);
      { char *p = readposn(); if (amode) printf(" %s", p); }
      break;
    }
    if (amode) printf("\n");
    nextoffset = byteoffset();
    if (smode) space[k] += nextoffset - offset;
  }
}

/* Traverse the trail structure starting from output and error
 * roots, marking all reachable nodes.
 */

fourbytes getfourbytes(char *buf) {
  fourbytes slot;
  slot.byte[0] = *(buf++);
  slot.byte[1] = *(buf++);
  slot.byte[2] = *(buf++);
  slot.byte[3] = *(buf++);
  slot.ptrval = ntohl(slot.ptrval);
  return slot;
}

unsigned long getpointer(char *buf) {
  return getfourbytes(buf).ptrval;
}

/* reading, checking and/or writing header and node information
 */
 
markfromheader(char *buf) {
  lseek(f,0L,0);
  do {
    read(f,buf,1);
  } while (*buf!='\0');
  read(f,buf,8);
  markfrom(getpointer(buf+4),buf+4);
  markfrom(getpointer(buf),  buf);
}

markfromoutput(char *bridgefile, char *buf) {
  int bridge = open(bridgefile, 0);
  if (bridge==-1) return;
  for (;;) {
    int n = read(bridge,buf,4);
    if (n<4) return;
    markfrom(getpointer(buf),buf);
  }  
}

/* mark all the nodes in the hat file that are reachable
 * from the given root -- setting the highest bit in the
 * tag byte
 */
markfrom(unsigned long root, char *buf) {
  if (root > 0 && root < filesize) {
    /* First read the tag byte.  If it is marked, return.
     * If it is not marked, then mark it now.
     */
    lseek(f,root,0);
    read(f,buf,1);
    if (ismarked(*buf)) return;
    marktag(buf);
    lseek(f,root,0);
    write(f,buf,1);
    cleartag(buf);
    /* Examine the tag to determine the kind of node.
     * Read pointers from the node into buf, then
     * markfrom these pointers recursively.  The buffer is
     * overwritten where possible to minimise the risk of overflow:
     * for this reason, pointers are recursively traced in
     * reverse order.
     */
    {
      int k = hi3(*buf);
      if (k > SR) {
	fprintf(stderr, "strange high-bits tag %d at 0x%x\n",
                	k, root);
	exit(1);
      }
      switch (k) {
      case TR: {
	int trk = lo5(*buf);
	if (trk & SATNOAPP) trk -= SATNOAPP;
	if (trk > SATC) {
          fprintf(stderr, "strange low-bits tag %d in TR 0x%x\n",
	          trk, root);
          exit(1);
	}
	switch (trk) {
	case APP:
          read(f,buf,1);
	  { int arity = *buf;
            arity += 3;
	    read(f,buf,arity*4);
	    for (;arity-->0;)
	      markfrom(getpointer(buf+arity*4), buf+arity*4);         }
	  break;
	case NAM:
	  read(f,buf,12);
	  markfrom(getpointer(buf+8), buf+8);
	  markfrom(getpointer(buf+4), buf+4);
          markfrom(getpointer(buf),   buf);
	  break;
	case IND:
	  read(f,buf,8);
	  markfrom(getpointer(buf+4), buf+4);
	  markfrom(getpointer(buf),   buf);
	  break;
	case HIDDEN:
	case SATA:
	case SATB:
	case SATC:
	  read(f,buf,4);
	  markfrom(getpointer(buf), buf);
	  break;
	}
	break;
      }
      case MD:
	break;
      case NT: {
	int ntk = lo5(*buf);
	if (ntk > CONTAINER) {
          fprintf(stderr, "strange low-bits tag %d in NT 0x%x\n",
	          ntk, root);
          exit(1);
	}
	switch (ntk) {
	case IDENTIFIER:
	case TOPIDENTIFIER:
	case CONSTRUCTOR:
	  do {
	    read(f,buf,1);
	  } while (*buf!='\0');
	  read(f,buf,4);
	  markfrom(getpointer(buf), buf);
	  break;
	default:
	  break;
	}
	break;
      }
      case SR:
	break;
      }
    }
  }
}

  
  

