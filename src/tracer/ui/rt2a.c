/* rt2a: expands redex trail archives to a (more) readable ASCII format
 * Colin Runciman, University of York, February 2001
 */

/* #include <unistd.h> */
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

/* Main driver and routines to providing basic interface to archive file.
 */

int f;               /* file descriptor for archive */
#define BUFSIZE 2048
char buf[BUFSIZE];   /* input buffer */
int n;               /* buf[0..n-1] filled */
int boff;            /* if n>0, boff in 0..n-1 and buf[boff] is current */
unsigned long foff;  /* if n>0, this is offset in f of buf[0] */

int vmode = 0;

main (int argc, char *argv[])
{
  switch (argc) {
  case 2: 
    f = open(argv[1], 0);
    break;
  case 3: 
    if (strcmp(argv[1], "-v") == 0) {
      vmode = 1;
      f = open(argv[2], 0);
      break;
    }
  default:
    fprintf(stderr,"usage: rt2a [-v] file-name\n");
    exit(1);
  }
  if (f==-1) {
    fprintf(stderr, "cannot open trace file %s\n",argv[1]);
    exit(1);
  }
  n = read(f, buf, BUFSIZE);
  boff = 0;
  foff = 0L;
  header();
  nodes();
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
  default: return "INVALID";
  }
}   

int tagat(unsigned long offset) {
  char byte[1];
  int i;
  lseek(f, offset, 0);
  i = read(f, byte, 1);
  lseek(f, foff + n, 0);
  return (i==1 ? hi3(byte[0]) : INVALID);
}
  
void dopointer(int requiretag, unsigned long requireoffset,
               int contexttag, unsigned long contextoffset) {
  if (vmode && requireoffset>0) {
    int t = tagat(requireoffset);
    if (t != requiretag) {
      fprintf(stderr, "tag at %u is %s, not %s as %s at %u implies\n",
            requireoffset, tag2str(t), tag2str(requiretag),
	    tag2str(contexttag), contextoffset);
      exit(1);
    }
  }
  printf("(%s %u)", tag2str(requiretag), requireoffset);
}
  
/* reading, checking and/or writing header and node information
 */
 
header() {
  printf("%s", readstring());
  printf("\nEntry point: ");
  dopointer(TR, readpointer(), HEADER, 0L);
  printf("\nError message: ");
  dopointer(NT, readpointer(), HEADER, 0L);
  printf("\n");
}
  
nodes() {
  while (more()) {
    unsigned long offset = byteoffset();
    char b = nextbyte();
    switch (hi3(b)) {
    case TR:
      printf("TR %u: ", offset);
      switch (lo5(b)) {
      case APP:
        { int arity = readarity();
	  printf("Application\t\t%d",arity);
	  for (; arity-- > -2;)
	    dopointer(TR, readpointer(), TR, offset);
	}
	dopointer(SR, readpointer(), TR, offset);
	break;
      case NAM:
        printf(" Name\t\t\t");
        dopointer(TR, readpointer(), TR, offset);
	dopointer(NT, readpointer(), TR, offset);
	dopointer(SR, readpointer(), TR, offset);
	break;
      case IND:
        printf(" Indirection\t\t");
	dopointer(TR, readpointer(), TR, offset);
	dopointer(TR, readpointer(), TR, offset);
	break;
      case HIDDEN:
        printf(" Hidden\t\t\t");
	dopointer(TR, readpointer(), TR, offset);
	break;
      case SATA:
        printf(" SAT(A)\t\t");
	dopointer(TR, readpointer(), TR, offset);
	break;
      case SATB:
        printf("SAT(B)\t\t");
	dopointer(TR, readpointer(), TR, offset);
	break;
      case SATC:
        printf("SAT(C)\t\t");
	dopointer(TR, readpointer(), TR, offset);
	break;
      default:
        fprintf(stderr, "strange low-bits tag %d in TR %u\n",
	        lo5(b), offset);
        exit(1);
      }
      break;
    case MD:
      printf("MD %u:  ", offset);
      switch (lo5(b)) {
          case SUSPECT: printf("module (suspect)\t"); break;
          case TRUSTED: printf("module (trusted)\t"); break;
          default: printf("WRONG\t\t\t"); break;
      }
      printf("%s\t", readstring());
      printf("\"%s\"", readstring());
      break;
    case NT:
      printf("NT %u:  ", offset);
      switch (lo5(b)) {
      case INT:
        printf("INT %d", readint());
	break;
      case CHAR:
        printf("CHAR '%c'", readchar());
	break;
      case INTEGER:
        printf("INTEGER %s", readinteger());
	break;       
      case RATIONAL:
        printf("RATIONAL %s", readrational());
	break;
      case FLOAT:
        printf("FLOAT %g", readfloat());
	break;
      case DOUBLE:
        printf("DOUBLE %g", readdouble());
	break;
      case IDENTIFIER:
        printf("identifier\t\t%s ", readstring());
	{ unsigned long modinfo = readpointer();
	  char *fp = readfixpri();
	  if (*fp!='\0') printf("%s ", fp);
	  dopointer(MD, modinfo, NT, offset);
	  printf(" %s", readposn());
        }
	break; 
      case CONSTRUCTOR:
        printf("constructor\t\t%s", readstring());
	{ unsigned long modinfo = readpointer();
	  char *fp = readfixpri();
	  if (*fp!='\0') printf("%s ", fp);
	  dopointer(MD, modinfo, NT, offset);
	  printf(" %s", readposn());
        }
	break; 
      case TUPLE:
	printf("TUPLE");
	break;
      case FUN:
	printf("FUN");
	break;
      case CASE:
	printf("CASE");
	break;
      case LAMBDA:
	printf("LAMBDA");
	break;
      case DUMMY:
	printf("DUMMY");
	break;
      case CSTRING:
	printf("CSTRING \"%s\"", readstring());
	break;
      case IF:
	printf("IF");
	break;
      case GUARD:
	printf("GUARD");
	break;
      case CONTAINER:
	printf("CONTAINER");
	break;
      default:
        fprintf(stderr, "strange low-bits tag %d in NT %u\n",
	                lo5(b), offset);
        exit(1);
      }
      break;
    case SR:
      printf("SR %u:  Source reference\t", offset);
      dopointer(MD, readpointer(), SR, offset);
      printf(" %s", readposn());
      break;
    default:
      fprintf(stderr, "strange high-bits tag %d at byte offset %u\n",
                      hi3(b), offset);
      exit(1);
    }
    printf("\n");
  }
}

