/* hat-check: reads hat trace files, checking at least the basic format
 * other checks and information can be requested by options
 * Colin Runciman, University of York
 * Original version February 2001
 *   text output as default, with -v option for added verification check
 * 21 March 2001
 *   added -s option for statistics, made text an option (-a)
 */

/* #include <unistd.h> */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

#define TR 0

#define APP 0
#define NAM 1
#define IND 2
#define HIDDEN 3
#define SATA 4
#define SATB 5
#define SATC 6

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

int vmode = 0;       /* verify -- check tag-types of pointer destinations */
int smode = 0;       /* statistics -- counts and space usage for node types */
int amode = 0;       /* ascii -- show archive in a `readable' text format */

unsigned filesize = 0; /* used in precondition for seeks ... */
struct stat statbuf;   /* ... to catch seek beyond EOF */

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
    } else {
      badusage();
    }
  }
  if (i > argc-1) badusage();
  stat(argv[i], &statbuf);
  filesize = statbuf.st_size;
  f = open(argv[i], 0);
  if (f==-1) { fprintf(stderr, "cannot open trace file"); exit(1); }
  n = read(f, buf, BUFSIZE);
  boff = 0;
  foff = 0L;
  if (smode) initstats();
  header();
  nodes();
  if (amode && smode) putchar('\n');
  if (smode) reportstats();
}


badusage() {
  fprintf(stderr,"usage: hat-check [-a] [-s] [-v] file-name\n");
  fprintf(stderr,"\t-a\tprint ascii text version of hat file\n");
  fprintf(stderr,"\t-s\tprint statistics about frequency and size of nodes\n");
  fprintf(stderr,"\t-v\tverify tag types of pointer destinations\n");
  exit(1);
}

unsigned int count[4];       /* indexed by TR, MD, NT, SR */
unsigned long space[4];      /* ditto */
unsigned long headspace;     /* space oocupied by header */
unsigned int trcount[7];     /* indexed by APP, NAM, ... , SATC */
unsigned long trspace[7];    /* ditto */

initstats() {
  int k;
  for (k=0; k<4; k++) {
    count[k] = 0;
    space[k] = 0L;
  }
  for (k=0; k<7; k++) {
    trcount[k] = 0;
    trcount[k] = 0L;
  }
}

float pc(unsigned long i, unsigned long j) {
  return (float)((i*100.0)/j);
}

reportstats() {
  unsigned int grandcount = 0;
  unsigned long grandspace = 0L;
  unsigned long allspace;
  int k;
  for (k=0; k<4; k++) {
    grandcount += count[k];
    grandspace += space[k];
  }
  allspace = headspace + grandspace;
  printf("%10s\t%-20s\t%10s\t%8s\n\n",
    "Number", "Description", "Bytes", "% Space");
  printf("%10u\t%-20s\t%10u\n\n",
    1, "header", headspace);
  printf("%10u\t%-20s\t%10u\t%8.1f\n",
    trcount[APP], "TR application nodes", trspace[APP], pc(trspace[APP],allspace));
  printf("%10u\t%-20s\t%10u\t%8.1f\n",
    trcount[NAM], "TR name nodes", trspace[NAM], pc(trspace[NAM],allspace));
  printf("%10u\t%-20s\t%10u\t%8.1f\n",
    trcount[IND], "TR indirection nodes", trspace[IND], pc(trspace[IND],allspace));
  printf("%10u\t%-20s\t%10u\t%8.1f\n",
    trcount[HIDDEN], "TR hidden nodes", trspace[HIDDEN], pc(trspace[HIDDEN],allspace));
  printf("%10u\t%-20s\t%10u\t%8.1f\n",
    trcount[SATA], "TR type A SAT nodes", trspace[SATA], pc(trspace[SATA],allspace));
  printf("%10u\t%-20s\t%10u\t%8.1f\n",
    trcount[SATB], "TR type B SAT nodes", trspace[SATB], pc(trspace[SATB],allspace));
  printf("%10u\t%-20s\t%10u\t%8.1f\n",
    trcount[SATC], "TR type C SAT nodes", trspace[SATC], pc(trspace[SATC],allspace));
  printf("%10u\t%-20s\t%10u\t%8.1f\n",
    count[MD], "MD nodes", space[MD], pc(space[MD],allspace));
  printf("%10u\t%-20s\t%10u\t%8.1f\n",
    count[NT], "NT nodes", space[NT], pc(space[NT],allspace));
  printf("%10u\t%-20s\t%10u\t%8.1f\n",
    count[SR], "SR nodes", space[SR], pc(space[SR],allspace));
  { int w;
    putchar('\n');
    for (w=0; w<64; w++) putchar(w<16 ? ' ' : '-');
    putchar('\n');
  }
  printf("%10s\t%-20s\t%10u\t%8.1f\n",
    "", "whole trace file", allspace, pc(allspace,allspace));
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
  int n = (int)(nextbyte());
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
    return (i==1 ? hi3(byte[0]) : INVALID);
  } else return BEYOND;
}
  
void dopointer(int requiretag, unsigned long requireoffset,
               int contexttag, unsigned long contextoffset) {
  if (vmode && requireoffset>0) {
    int t = tagat(requireoffset);
    if (t != requiretag) {
      fprintf(stderr, "tag at %u is %s, not %s as %s at %u implies\n",
            requireoffset, tag2str(t), tag2str(requiretag),
	    tag2str(contexttag), contextoffset);
      /*exit(1);*/
    }
  }
  if (amode) printf("(%s 0x%x)", tag2str(requiretag), requireoffset);
}
  
/* reading, checking and/or writing header and node information
 */
 
header() {
  { char *s = readstring(); if (amode) printf("%s", s); }
  if (amode) printf("\nEntry point: ");
  dopointer(TR, readpointer(), HEADER, 0L);
  if (amode) printf("\nError message: ");
  dopointer(NT, readpointer(), HEADER, 0L);
  if (amode) printf("\n");
  headspace = byteoffset();
}
  
nodes() {
  unsigned long offset;
  unsigned long nextoffset;
  nextoffset = byteoffset();
  while (more()) {
    char b = nextbyte();
    int k = hi3(b);
    offset = nextoffset;
    if (k > SR) {
      fprintf(stderr, "strange high-bits tag %d at byte offset %u\n",
                      k, offset);
      exit(1);
    } else {
      count[k]++;
    }
    switch (k) {
    case TR: {
      int trk = lo5(b);
      if (trk > SATC) {
        fprintf(stderr, "strange low-bits tag %d in TR %u\n",
	        trk, offset);
        exit(1);
      } else {
        trcount[trk]++;
      }
      if (amode) printf("TR 0x%x: ", offset);
      switch (trk) {
      case APP:
        { int arity = readarity();
	  if (amode) printf(" Application %d \t",arity);
	  for (; arity-- > -2;)
	    dopointer(TR, readpointer(), TR, offset);
	}
	dopointer(SR, readpointer(), TR, offset);
	break;
      case NAM:
        if (amode) printf(" Name          \t");
        dopointer(TR, readpointer(), TR, offset);
	dopointer(NT, readpointer(), TR, offset);
	dopointer(SR, readpointer(), TR, offset);
	break;
      case IND:
        if (amode) printf(" Indirection   \t");
	dopointer(TR, readpointer(), TR, offset);
	dopointer(TR, readpointer(), TR, offset);
	break;
      case HIDDEN:
        if (amode) printf(" Hidden        \t");
	dopointer(TR, readpointer(), TR, offset);
	break;
      case SATA:
        if (amode) printf(" SAT(A)        \t");
	dopointer(TR, readpointer(), TR, offset);
	break;
      case SATB:
        if (amode) printf(" SAT(B)        \t");
	dopointer(TR, readpointer(), TR, offset);
	break;
      case SATC:
        if (amode) printf(" SAT(C)        \t");
	dopointer(TR, readpointer(), TR, offset);
	break;
      }
      trspace[trk] += byteoffset() - offset;
      break;
    }
    case MD:
      if (amode) printf("MD 0x%x:  ", offset);
      switch (lo5(b)) {
          case SUSPECT: if (amode) printf("module (suspect)\t"); break;
          case TRUSTED: if (amode) printf("module (trusted)\t"); break;
          default:
	    fprintf(stderr, "strange low-bits tag %d in MD %u",
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
        { char *s = readstring(); if (amode) printf("identifier\t\t%s ", s); }
	{ unsigned long modinfo = readpointer();
	  char *fp = readfixpri();
	  if (*fp!='\0' && amode) printf("%s ", fp);
	  dopointer(MD, modinfo, NT, offset);
	  { char *p = readposn(); if (amode) printf(" %s", p); }
        }
	break; 
      case CONSTRUCTOR:
        { char *s = readstring(); if (amode) printf("constructor\t\t%s", s); }
	{ unsigned long modinfo = readpointer();
	  char *fp = readfixpri();
	  if (*fp!='\0' && amode) printf("%s ", fp);
	  dopointer(MD, modinfo, NT, offset);
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
        fprintf(stderr, "strange low-bits tag %d in NT %u\n",
	                lo5(b), offset);
        exit(1);
      }
      break;
    case SR:
      if (amode) printf("SR 0x%x:  Source reference\t", offset);
      dopointer(MD, readpointer(), SR, offset);
      { char *p = readposn(); if (amode) printf(" %s", p); }
      break;
    }
    if (amode) printf("\n");
    nextoffset = byteoffset();
    space[k] += nextoffset - offset;
  }
}

