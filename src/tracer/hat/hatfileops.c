/**************************************************************************/
/* hatfileops.c: general operations on hat files                          */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include "Expressions.h"
#include "hatfileops.h"
#include "FunTable.h"

/* routines providing interface to the hat archive file. */

int f;                  /* file descriptor for archive */
#define MAXBUFSIZE 16384
int bufsize=MAXBUFSIZE; /* buffersize */
char buf[MAXBUFSIZE];   /* input buffer */
unsigned int buf_n;     /* buf[0..buf_n-1] filled */
unsigned int boff;      /* if buf_n>0, boff in 0..buf_n-1 and buf[boff] is current */
unsigned long foff;     /* if buf_n>0, this is offset in f of buf[0] */

unsigned long _filesize = 0;

struct stat hatStatBuf;

typedef union {char byte[4];
               unsigned long ptrval;
	       long intval;
	       float floatval;} fourbytes;

int checkParameters(char* str,char* allowed) {
  if (*str!='-') return 1; // bad parameter syntax
  str++;
  if (*str=='\0') return 1; // nothing specified!
  while (*str!='\0') {
    if (strchr(allowed,*str)==NULL)
      return 2; // unsupported parameter
    str++;
  }
  return 0; // string is ok
}

/* make proper file extension, if missing */
char* filename(char* name) {
  if (strstr(name,".hat")!=NULL) return newStr(name);
  else {
    char* newstr=(char*) malloc(strlen(name)+5,sizeof(char));
    strcpy(newstr,name);
    strcat(newstr,".hat");
    return newstr;
  }
}

int getline(char s[], int max) {
  int c,i;
  fflush(stdout);
  c=getchar();
  for (i=0;(i<max-1) && (c!=EOF) && (c!='\n');i++) {
    if (c==-1) {
      i--;
    } else {
      s[i]=c;
    }
    c=getchar();
  }
  s[i]=0;
  return i;
}

/* open file for reading, save in internal file descriptor */
int openfile(char* name) {
  name = filename(name);
  stat(name, &hatStatBuf);
  _filesize = hatStatBuf.st_size;
  f = open(name, 0);
  freeStr(name);
  buf_n = 0; // set buffer pointers appropriately. buf_n=0 buffer currently empty
  boff = 0;  // at position 0 in buffer
  foff = 0;  // at position 0 in file
  return (f!=-1);
}

/* close file in internal file descriptor */
void closefile() {
  close(f);
}

unsigned long filesize() {return _filesize;}

/* set new file position */
void seek(unsigned long ofs) {
  if ((ofs>=foff)&&(ofs<foff+buf_n)) { // new position is within the buffer!
    boff=ofs-foff;   // only reset the buffer position!
  } else {
    boff=0;   // set internal pointers correctly
    foff=ofs;
    buf_n=0;      // buf_n=0, nothing in memory. read from file on next read!
    lseek(f,ofs,0); // do seek in file
  }
}

/* check for more data in file */
int more() {
  if (boff<buf_n) {
    return 1; /* more data in buffer! */
  } else {
    foff+=buf_n; // read next block
    boff-=buf_n;
    buf_n=read(f,buf,bufsize);
    return (buf_n>boff);  // more data?
  }
}

/* return position in file */
unsigned long byteoffset() {
  return foff + boff;
}

/* Routines to extract values encoded as one or more bytes. */

/* read one byte from file */
char nextbyte() {
  char c;
  if (boff>=buf_n) {  // need to read a new block
    foff += buf_n;
    boff -= buf_n;    // reduce boff appropriately
    buf_n = read(f,buf,bufsize);
    //if (buf_n==0) { fprintf(stderr, "unexpected end of trace file\n"); exit(1); }
    
  }
  c =  buf[boff++];
  return c;
}

char seenextbyte() {
  char c = nextbyte();
  boff--;    // backup by one byte
  return c;
}

/* just skip one byte in file - don't care to read it */
void skipbyte() { boff++; }

#define STRINGMAX 255
char stringbuf[STRINGMAX+1];

char *readstring() {
  int n = 0;
  char c;
  do {
    c = nextbyte();
    stringbuf[n++] = c;
  } while (c!='\0' && n<STRINGMAX); // read until buffer full or string's end
  stringbuf[n] = '\0';
  while (c!='\0') {c = nextbyte();} // oops, string is even longer. find its end
  return stringbuf;
}

/* skip one string in file */
void skipstring() {
  do {
    if (boff>=buf_n) {
      foff +=buf_n;
      boff -=buf_n;
      buf_n = read(f,buf,bufsize);
    }
  } while (buf[boff++]!='\0');
}

fourbytes readfourbytes() {
  unsigned int b=boff+4;
  fourbytes slot1,slot;
  if (b<=buf_n) { // next 4 bytes are in memory
    memcpy((char*) &slot1.ptrval,(char*) &buf[boff],4);
    boff=b;
    slot1.ptrval = ntohl(slot1.ptrval); // return next 4 bytes, increase boff
    return slot1;
  } else { // next 4 bytes not in memory, read bytewise
    slot.byte[0] = nextbyte();
    slot.byte[1] = nextbyte();
    slot.byte[2] = nextbyte();
    slot.byte[3] = nextbyte();
    slot.ptrval = ntohl(slot.ptrval);
    return slot;
  }
}

/* skip number of bytes in buffer
 */
void skipbytes(int bytes) { boff+=bytes; }

#define POSNMAX 30
char posnbuf[POSNMAX+1];

unsigned long readpointer() {
  return readfourbytes().ptrval;
}

void skippointer() {skipbytes(4);}

int readint() {
  return readfourbytes().intval;
}

#define INTEGERMAX 30
char integerbuf[INTEGERMAX+1];

int readinteger() { // int for now!
  int n = (int)(nextbyte());
  int i = n;
  if (n==0) return 0;
  if (n==1) return readint();
  while (i-- > 0) (void)(readfourbytes());
  return 0; // dummy value for INTEGER!  
}

void skipinteger() {
  int n = (signed char) (nextbyte());
  if (n<0) n=-n;
  skipbytes(n*4);
}

#define RATMAX (2*INTEGERMAX+2)
char ratbuf[RATMAX+1];

void skiprational() {
  skipinteger();
  skipinteger();
}

typedef union {
  double d;
  fourbytes a[2];
} eightbytes;

/* double readdouble() {
  eightbytes v;
  v.a[0] = readfourbytes();
  v.a[1] = readfourbytes();
  v.a[1].ptrval = htonl(v.a[1].ptrval);
  v.a[0].ptrval = htonl(v.a[0].ptrval);
  return v.d;
  }*/

int hi3(char b) {
  return (int)(b>>5);
}

int lo5(char b) {
  return (int)(b&037);
}

int readarity() {
  return (int)(nextbyte());
}

int readfixpri() {
  return (int)(nextbyte());
}

int tagat(unsigned long offset) {
  char byte[1];
  int i;
  if (offset <= _filesize) {
    lseek(f, offset, 0);
    i = read(f, byte, 1);
    lseek(f, foff + buf_n, 0);
    return (i==1 ? hi3(byte[0]) : INVALID);
  } else return BEYOND;
}

/* reading, checking and/or writing header and node information */
int testheader() {
  char *version;
  version = readstring();
  if (strcmp(version,"Hat v01")!=0) {
    fprintf(stderr,"ERROR: File is not a hat file or version is not supported.\nAborted.\n\n");
    return 0;
  } else {
    skipbytes(8);
    return 1;
  }
}

int isSAT(unsigned long fileoffset) {
  char c;
  unsigned long old = byteoffset();
  
  seek(fileoffset);
  c = seenextbyte();
  seek(old);

  return ((c==TRSATC)||(c==TRSATB)||(c==TRSATA));
}

void skipNode(char nodeType) {
  switch (hi3(nodeType)) {
  case TR:
    switch (lo5(nodeType)) {
    case APP: // Application
      {
	int arity = readarity();
	skipbytes(4*(1+1+arity+1));
      }
      break;
    case NAM: // Name
      skipbytes(4*3);
      break;
    case IND: //  Indirection
      skipbytes(4*2);
      break;
    case HIDDEN: // Hidden
    case SATA:
    case SATB:
    case SATC:
    case SATCIS:
    case SATBIS:
    case SATAIS:
      skippointer();
      break;
    default:
      fprintf(stderr, "strange low-bits tag %d in TR %u\n",
	      lo5(nodeType), byteoffset()-1);
      exit(1);
    }
    break;
  case MD: // Module / trusted or untrusted
    skipstring();
    skipstring();
    break;
  case NT:
    switch (lo5(nodeType)) {
    case INT:
      skipbytes(4);
      break;
    case CHAR:
      skipbytes(1);
      break;
    case INTEGER:
      skipinteger();
      break;       
    case RATIONAL:
      skiprational();
      break;
    case FLOAT:
      skipbytes(4);
      break;
    case DOUBLE:
      skipbytes(8);
      break;
    case IDENTIFIER:
      skipstring();  // simply read string
      skipbytes(4+1+4);
      break; 
    case CONSTRUCTOR:
      skipstring();
      skipbytes(4+1+4);
      break; 
    case TUPLE:
    case FUN:
    case CASE:
    case LAMBDA:
    case DUMMY:
    case CSTRING:
    case IF:
    case GUARD:
    case CONTAINER:
      break;
    default:
      fprintf(stderr, "strange low-bits tag %d in NT %u\n",
	      lo5(nodeType), byteoffset()-1);
      exit(1);
    }
    break;
  case SR:
    skipbytes(4+4);
    break;
  default:
    fprintf(stderr, "strange high-bits tag %d at byte offset %u\n",
	    hi3(nodeType), byteoffset()-1);
    exit(1);
  }
}

/* follow the trace along SATs, indirections and TRNAMEs */
unsigned long followTrace(unsigned long fileoffset) {
  char nodeType;
  unsigned long p;
  
  while (1) {
    //printf("following Trace... %u\n",fileoffset);
    seek(fileoffset);
    nodeType = nextbyte();
    //printf("node type: %i\n",b);
    switch (nodeType) {
    case TRNAM:
    case TRIND: //  Indirection
      skippointer();
      fileoffset=readpointer();
      break;
    case TRSATCIS:
    case TRSATC:
      fileoffset=readpointer(); // follow link...
      break;
    default:
      seek(fileoffset); // backup one byte
      return fileoffset;
    }
  }
}

/* follow the trace along HIDDEN traces */
unsigned long followHidden(unsigned long fileoffset) {
  char nodeType;
  unsigned long p;
  
  while (1) {
    seek(fileoffset);
    nodeType = nextbyte();
    if (nodeType==TRHIDDEN) {
      fileoffset=readpointer(); // follow link...
    } else {
      seek(fileoffset); // backup one byte
      return fileoffset;
    }
  }
}

/* follow the trace along SATs */
unsigned long followSATs(unsigned long fileoffset) {
  char nodeType;
  unsigned long p;
  
  while (1) {
    seek(fileoffset);
    nodeType = nextbyte();
    switch (nodeType) {
    case TRSATCIS:
    case TRSATC:
      fileoffset=readpointer(); // follow link...
      break;
    default:
      seek(fileoffset); // backup one byte
      return fileoffset;
    }
  }
}

/* find the SAT belonging to an application */
/* return value 0: no SAT found, otherwise: offset for SAT */
unsigned long findAppSAT(unsigned long fileoffset) {
  char nodeType;
  unsigned long p,satc;

  while (1) {
    //printf("searching for App SAT... %u\n",fileoffset);
    seek(fileoffset);
    nodeType = getNodeType();
    //printf("node type: %i\n",nodeType);
    switch (nodeType) {
    case TRAPP: // Application
      p = getTrace();
      nextNode();
      satc = byteoffset();
      if (isSAT(satc)) { // success! found the SATC!
 	return satc;
      }
      else fileoffset = p; // follow parent!
      break;
    case TRNAM:   // for finding CAFs. SATc should be behind the TRNAM
      p=getTrace();
      //if (p==0) { // found CAF!
      nextNode();
      satc = byteoffset();
      if (isSAT(satc)) return satc;
      //}
      fileoffset=p;
      break;
    default: {
	unsigned long newfileoffset=followTrace(fileoffset);
	if (newfileoffset == fileoffset) {
	  if (isSAT(newfileoffset)) return fileoffset;
	  else return 0;
	} else
	  fileoffset = newfileoffset;
      }
    }
  }
}

/* building expression at given offset from hat file */
ExprNode* buildExprRek(unsigned long fileoffset,int verbose) {
//#define DebugbuildExpr
  char b;
  unsigned long p;
  ExprNode* exp=NULL;
  char *s;

  while (fileoffset!=0) {
#ifdef DebugbuildExpr
    printf("building expression... %u\n",fileoffset);
#endif
    fileoffset=followTrace(fileoffset); // follow the trace along all SATs and indirections
    b = getNodeType();
#ifdef DebugbuildExpr
    printf("node type: %i\n",b);
#endif
    switch (b) {
    case TRAPP: // Application
      { 
	int i=0,arity;
	AppNode* apn;
	ExprNode* fun;
	unsigned long localoffset,functionOffset;
	arity=getAppArity();
	apn=newAppNode(arity);

	exp = newExprNode(TRAPP);
	exp->v.appval = apn;

	functionOffset = getFunTrace();
#ifdef DebugbuildExpr
	printf("Found application of arity: %i\n",arity);
#endif
	while (i++<arity) {  // now read all argument pointers into memory
	  setAppNodeArg(apn,i-1,(ExprNode*) getAppArgument(i-1)); // do something nasty
	  // abuse pointers for storing the fileoffsets temporarily
	}
	// build function
	setAppNodeFun(apn,fun=buildExprRek(functionOffset,verbose));
	i=0;
	while (i++<arity) {
#ifdef DebugbuildExpr
	  printf("building argument %i\n",i);
#endif
	  // exchange arguments containing fileoffsets against the built expressions
	  setAppNodeArg(apn,i-1,buildExprRek((unsigned long) getAppNodeArg(apn,i-1),
					     verbose));
	}
	return exp;
	}
    case NTIDENTIFIER:
    case NTCONSTRUCTOR: {
      int infix,infixprio;
      exp = newExprNode(b);
      s=getName();
      infix=getInfixPrio();
      infixprio = infix / 4;
      infix = infix % 4;
      if (strcmp(s,",")==0) {
	s=newStr(",");
	infix=0;
      } else {
	if (infix==3) infixprio=32768;
	s=newStr(s);  // read name of identifier/constructor
      }
      exp->v.identval = newIdentNode(s,infix,infixprio);
      return exp;
    }
    case TRNAM: // Name
      fileoffset=getNmType(); // read NmType -> follow this link to build 
      break;
    case TRIND: //  Indirection
      fileoffset=getValueTrace(); // follow this link for prettyPrint
      break;
    case NTINT:
      exp = newExprNode(b);
      exp->v.intval = getIntValue();
      return exp;
    case NTCHAR:
      exp = newExprNode(b);
      exp->v.charval = getCharValue();
      return exp;
    case NTDOUBLE:
      exp = newExprNode(b);
      exp->v.doubleval = (double*) malloc(1,sizeof(double));
      *(exp->v.doubleval) = getDoubleValue();
      return exp;
    case NTRATIONAL:
    case NTINTEGER:
      exp = newExprNode(b);
      exp->v.intval = getIntegerValue();
      return exp;
    case NTFLOAT:
      exp = newExprNode(b);
      exp->v.floatval = getFloatValue();
      return exp;
    case NTTUPLE:
    case NTFUN:
    case NTCASE:
    case NTLAMBDA:
    case NTDUMMY:
    case NTCSTRING:
    case NTIF:
    case NTGUARD:
    case NTCONTAINER:
      return newExprNode(b);
    case TRHIDDEN:
      if (verbose) {
	exp = newExprNode(MESSAGE);
	exp->v.message = newStr("\253HIDDEN\273");
	return exp;
      } else return NULL;
    case TRSATAIS:
    case TRSATA: // unevaluated expression
      if (verbose) {
	p = getTrace();
	exp = newExprNode(TRSATA);
	exp->v.expr = buildExprRek(p,verbose);
	return exp;
      } else
	return NULL;
    case TRSATBIS:
    case TRSATB:
      exp = newExprNode(b);
      exp->v.intval = getTrace();
      return exp;
    default:
      fprintf(stderr, "strange tag %d in %u\n",
	      b, byteoffset()-1);
      exit(1);
    }
  }
  return NULL;
}

/* build expression at given offset in file */
ExprNode* buildExpr(unsigned long fileoffset,int buildUnevaldepth) {
  return buildExprRek(fileoffset,buildUnevaldepth);
}

/*********************************************************************/

/* show location in source file of this application/symbol */
void showLocation(unsigned long fileoffset) {
  char nodeType;
  char *s;
  unsigned long old = byteoffset(),posn;
  while (1) {
    seek(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case SRCREF:
      fileoffset = getModInfo();
      s = getPosnStr();
      showLocation(fileoffset);
      printf(", %s\n",s);
      seek(old);
      return;
    case MDSUSPECT:
    case MDTRUSTED:
      s = getName();

      printf("module \"%s\", ",s);
      s = getSrcName();
      printf("file \"%s\"",s);
      seek(old);
      return;
    case TRAPP:
      {
	fileoffset = getSrcRef();
	break;
      }
    case TRNAM:
      fileoffset = getSrcRef();
      break;
    default:
      seek(old);
      return;
    }
  }
}

/* show location where function was defined */
void showFunLocation(unsigned long fileoffset) {
  char nodeType;
  char *s;
  unsigned long old = byteoffset(),posn;
  while (1) {
    fileoffset = followSATs(fileoffset);
    seek(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case NTIDENTIFIER:
    case NTCONSTRUCTOR:
      fileoffset = getTrace();
      s = getPosnStr();
      showLocation(fileoffset);
      printf(", %s\n",s);
      seek(old);
      return;
    case SRCREF:
      fileoffset = getModInfo();
      s = getPosnStr();
      showLocation(fileoffset);
      printf(", %s\n",s);
      seek(old);
      return;
    case MDSUSPECT:
    case MDTRUSTED:
      s = getName();

      printf("module \"%s\", ",s);
      s = getSrcName();
      printf("file \"%s\"",s);
      seek(old);
      return;
    case TRAPP:
      {
	fileoffset = getFunTrace();
	break;
      }
    case TRNAM:
      fileoffset = getNmType();
      break;
    default:
      printf("no location at offset: %u\n",fileoffset);
      seek(old);
      return;
    }
  }
}

void showNode(unsigned long fileoffset,int verboseMode) {
  char *appstr;
  ExprNode* exp;

  exp = buildExpr(fileoffset,verboseMode);
  appstr = prettyPrintExpr(exp,1);
  printf(appstr);
  freeExpr(exp);
  freeStr(appstr);
}

//#define showAppNode
unsigned long showAppAndResult(unsigned long fileoffset,int verboseMode) {
  char *appstr;
  char *resstr;
  ExprNode* exp;
  unsigned long satc = 0;

  exp = buildExpr(fileoffset,verboseMode);
  appstr = prettyPrintExpr(exp,1);
  freeExpr(exp);

  satc = findAppSAT(fileoffset);  // find SATC for the application!

  if (satc!=0) {
#ifdef showAppNode
    printf("(%u): ",fileoffset);
#endif
    exp = buildExpr(byteoffset(),verboseMode);
    resstr = prettyPrintExpr(exp,1); // don't show any level of unevaluated functions
    freeExpr(exp);
    printf(appstr);
    printf(" = %s\n",resstr); // print value of SAT!

    freeStr(resstr);
  }
  freeStr(appstr);
  return satc;
}

int isTrusted(unsigned long srcref) {
  char nodeType;
  unsigned long old = byteoffset();
  while (1) {
    seek(srcref);
    nodeType=getNodeType();
    switch(nodeType) {
    case TRAPP:
      srcref=getFunTrace();
      break;
    case TRSATA:
    case TRSATB:
    case TRSATC:
    case TRSATCIS:
    case TRSATBIS:
    case TRSATAIS:
    case SRCREF:
      srcref = getTrace();
      break;
    case MDSUSPECT:
      seek(old);
      return 0;
    case MDTRUSTED:
      seek(old);
      return 1;
    case TRNAM:{
      unsigned long nmtype;
      nmtype=getNmType(); // follow nmType by default
      srcref=getSrcRef(); // use srcref if nmType is lambda
      seek(nmtype);
      if (getNodeType()!=NTLAMBDA) srcref=nmtype;
      break;
    }
    case NTGUARD:
    case NTIF:  // IFs are trusted. They do the right thing...
    case NTCASE: // CASEs are trusted, same reason...
    case TUPLE:
    case NTCONSTRUCTOR: // constructors are "trusted"! => its applications are ok!
      seek(old);
      return 1;
    case NTIDENTIFIER:
      srcref=getModInfo(); // follow module info
      break;
    default:
      seek(old);
      return 0;
    }
  }
}

int isCAF(unsigned long fileoffset) {
  char nodeType,i;
  unsigned long old = byteoffset();
  seek(fileoffset);
  nodeType=getNodeType();
  seek(old);
  switch(nodeType) {
  case TRNAM:
    return 1;
  default:
    return 0;
  }
}

int isTopLevel(unsigned long srcref) {
  char nodeType,i;
  unsigned long old = byteoffset();
  while (1) {
    seek(srcref);
    nodeType=getNodeType();
    switch(nodeType) {
    case SRCREF:
      i=(getPosn() % 10000)==1;
      seek(old);
      return i;
    case TRSATA:
    case TRSATB:
    case TRSATC:
    case TRSATAIS:
    case TRSATBIS:
    case TRSATCIS:
      srcref=getTrace();
      break;
    case TRNAM:
      srcref=getNmType(); // follow nmType
      break;
    case NTIDENTIFIER:
      i=(getPosn() % 10000)==1;
      seek(old);
      return i;
    case TRAPP:
      srcref = getFunTrace();
      break;
    case NTCASE:
    case NTIF:
    case NTGUARD:
    case NTLAMBDA:
    case NTCONTAINER:
    case NTTUPLE:
    case NTDUMMY:
    case NTFUN:
    case NTCSTRING:
      return 0; // not toplevel (necessary for hat-observe funA in funA!)
    default:
      seek(old);
      return 1;
    }
  }
}

int isDescendantOf(unsigned long fileoffset,unsigned long parent) {
  char nodeType;
  unsigned long old = byteoffset();

  if (parent==0) return 0;
  while (fileoffset!=0) {
    seek(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case TRHIDDEN:
      //case TRSATA:
      //case TRSATB:
    case TRSATC:
    case TRSATCIS:
      //case TRSATBIS:
      //case TRSATAIS:
      fileoffset=getTrace();
      break;
    case TRNAM:
    case TRAPP:{
      unsigned long newoffs;
      newoffs = getTrace();
      if (leftmostOutermost(fileoffset)==parent) {
	seek(old);
	return 1;
      }
      fileoffset = newoffs;
      break;
    }
    default:
      seek(old);
      return 0;
    }
  }
  seek(old);
  return 0;
}

int isDirectDescendantOf(unsigned long fileoffset,unsigned long parent) {
  char nodeType;
  unsigned long old = byteoffset();

  if (parent==0) return 0;
  while (fileoffset!=0) {
    seek(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case TRHIDDEN:
    case TRSATA:
    case TRSATB:
    case TRSATC:
    case TRSATCIS:
    case TRSATBIS:
    case TRSATAIS:
      fileoffset=getTrace();
      break;
    case TRAPP:{
      unsigned long newoffs,lmo;
      newoffs = getTrace();
      if ((lmo=leftmostOutermost(fileoffset))==parent) {
	seek(old);
	return 1;
      }
      if (isTopLevel(fileoffset)) {
	seek(old);
	return 0;
      }
      fileoffset = newoffs;
      break;
    }
    default:
      seek(old);
      return 0;
    }
  }
  seek(old);
  return 0;
}

int isChildOf(unsigned long fileoffset,unsigned long parent) {
  char nodeType;
  unsigned long old = byteoffset();

  if (parent==0) return 0;
  while (fileoffset!=0) {
    seek(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case TRHIDDEN:
    case TRSATA:
    case TRSATB:
    case TRSATC:
    case TRSATCIS:
    case TRSATBIS:
    case TRSATAIS:
      fileoffset=getTrace();
      break;
    case TRNAM:
    case TRAPP:{
      fileoffset = getTrace();
      if (fileoffset==parent) {
	seek(old);
	return 1;
      }
      if (isTopLevel(fileoffset)) {
	seek(old);
	return 0;
      }
      break;
    }
    default:
      seek(old);
      return 0;
    }
  }
  seek(old);
  return 0;
}

unsigned long leftmostOutermost(unsigned long fileoffset) {
  char nodeType;
  while (1) {
    seek(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case TRSATA:
    case TRSATB:
    case TRSATBIS:
    case TRSATAIS:
      return 0;
    case TRSATC:
    case TRSATCIS:
      fileoffset=getTrace();
      break;
    case TRNAM:
      fileoffset=getNmType(); // follow nmType
      break;
    case NTIDENTIFIER:
      return fileoffset;
    case TRAPP:
      fileoffset = getFunTrace();
      break;
    default:
      return 0;
    }
  }
}


/* set beginning of internal buffer to current position in file */
void resetFilepointer() {
  foff += boff;    // make position in buffer new file offset
  boff = 0;
  lseek(f,foff,0); // set filepointer
  buf_n = read(f,buf,bufsize); // read data
}

/* make sure, next <bytes> are in buffer */
void prepareBuffer(int bytes) {
  if (boff+bytes>=buf_n) {
    resetFilepointer();
  }
}

int getAppArity() {
  char c;
  prepareBuffer(1);
  return buf[boff+1];
}

filepointer getTrace() {
  unsigned int lbuf;
  filepointer fp;

  prepareBuffer(5); // need 5 bytes after current position
  lbuf = boff;

  if (nextbyte()==TRAPP) {
    skipbyte(); // skip one byte in applications
  }
  fp = readpointer();
  boff = lbuf;
  return fp;
}

filepointer getNmType() {
  unsigned int lbuf;
  filepointer fp;

  prepareBuffer(8);
  lbuf = boff;

  skipbytes(5);
  fp = readpointer();

  boff = lbuf;
  return fp;  
}

filepointer getSrcRef() {
  int arity;
  unsigned int lbuf;
  filepointer fp;

  prepareBuffer(4);
  lbuf = boff;

  switch(nextbyte()) {
  case TRAPP:
    arity = nextbyte();
    boff = lbuf;
    prepareBuffer((arity+3)*4+1);
    lbuf = boff;
    skipbytes((arity+2)*4+2);
    fp = readpointer();
    break;
  case TRNAM:
    prepareBuffer(12);
    skipbytes(8);
    fp = readpointer();
    break;
  }
  
  boff = lbuf;
  return fp;
}

filepointer getAppArgument(int i) {
  unsigned int lbuf;
  filepointer fp;
  prepareBuffer((i+3)*4+1);
  lbuf = boff;
  
  skipbytes((i+2)*4+2);
  fp = readpointer();

  boff = lbuf;
  return fp;
}

filepointer getFunTrace() {
  unsigned int lbuf;
  filepointer fp;
  prepareBuffer(9);
  lbuf = boff;

  skipbytes(6);
  fp = readpointer();

  boff = lbuf;
  return fp;
  
}

char getCharValue() {
  unsigned int lbuf;
  char c;

  prepareBuffer(2);
  lbuf = boff;

  skipbyte();
  c=seenextbyte();
  
  boff=lbuf;
  return c;
}

int getIntValue() {
  int i;
  unsigned int lbuf;

  prepareBuffer(4);
  lbuf = boff;

  skipbyte();
  i = readfourbytes().intval;

  boff=lbuf;
  return i;
}


int getIntegerValue() { // int for now!
  int n;
  int i;
  int res=0;
  unsigned int lbuf;

  prepareBuffer(5);
  lbuf = boff;

  skipbyte();
  n = (signed char) nextbyte();
  i = n;
  
  boff = lbuf;
  if (n==0) return 0;
  prepareBuffer(2+(n*4));
  lbuf = boff;
  skipbytes(2);
  
  // ATTENTION: |n| is number of bytes - mind the sign!
  //            n==0: value is 0!

  if (n==1) res = readint();else
    if (n==-1) res = - readint();
  // missing implementation for integers with more than 4 bytes!
  //while (i-- > 0) (void)(readfourbytes());

  boff = lbuf;
  return res;
}

char *getRationalValue() {
  filepointer fp;

  fp = foff;
  skipbyte();
  sprintf(ratbuf, "%i:%% %i",readinteger(),readinteger());
  foff = fp;
  buf_n=0;
  return ratbuf;
}

float getFloatValue() {
  unsigned int lbuf;
  fourbytes a;

  prepareBuffer(5);
  lbuf = boff;

  skipbyte();
  a = readfourbytes();
  a.ptrval = htonl(a.ptrval);
  
  boff = lbuf;
  return a.floatval;
}

double getDoubleValue() {
  unsigned int lbuf;
  eightbytes v;
  prepareBuffer(8);
  lbuf = boff;

  v.a[0] = readfourbytes();
  v.a[1] = readfourbytes();
  v.a[1].ptrval = htonl(v.a[1].ptrval);
  v.a[0].ptrval = htonl(v.a[0].ptrval);

  boff = lbuf;
  return v.d;
}

char getNodeType() {
  return seenextbyte();
}

void nextNode() {
  skipNode(nextbyte());
}

char* getName() { // get module, constructor or identifier name
  filepointer fp=foff;
  unsigned int lbuf=boff;
  char* s;
  
  skipbyte();
  s = readstring();
  
  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp; // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return s;
}

int getInfixPrio() {
  filepointer fp=foff;
  unsigned int lbuf=boff;
  char c;

  skipstring();
  skippointer();
  c = seenextbyte();

  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp;             // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return c;
}

filepointer getValueTrace() {
  unsigned int lbuf;
  filepointer trace;
  prepareBuffer(8);
  skipbytes(5);
  trace = readpointer();
  
  lbuf = boff;
  boff = lbuf;
  return trace;
}

char* getSrcName() {
  filepointer fp=foff;
  unsigned int lbuf=boff;
  char* s;
  
  skipbyte();
  skipstring();
  s = readstring();
  
  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp; // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return s;
}

filepointer getModInfo() {
  filepointer fp=foff;
  unsigned int lbuf=boff;
  filepointer res;
  
  if (nextbyte()!=SRCREF) {
    skipstring();  // skip string for NTIdentifier and NTConstructor
  }
  res = readpointer();
  
  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp; // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return res;
}

unsigned long getPosn() {
  filepointer fp=foff;
  unsigned int lbuf=boff;
  filepointer res;
  
  if (nextbyte()==SRCREF) {
    skipbytes(4);
  } else {
    skipstring(); // for NTCONST, NTIDENT
    skipbytes(5);
  }
  res = readpointer();
  
  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp; // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return res;
}

char* getPosnStr() {
  unsigned long posn = getPosn();
  
  sprintf(posnbuf, "line %u, column %u", posn/10000, posn%10000);
  return posnbuf;
}


