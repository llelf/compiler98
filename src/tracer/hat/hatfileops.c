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
int bufsize=MAXBUFSIZE; /* buffersize can be reduced when only seeking through the file! */
char buf[MAXBUFSIZE];   /* input buffer */
unsigned int buf_n;     /* buf[0..buf_n-1] filled */
unsigned int boff;      /* if buf_n>0, boff in 0..buf_n-1 and buf[boff] is current */
unsigned long foff;     /* if buf_n>0, this is offset in f of buf[0] */

unsigned _filesize = 0;

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

/* reset the buffer size. useful for different scanning modes.
   large buffer for sequential scan through the file, small values
   for seeking only */
void resetbuffersize(int size) {
  if ((size>100)&&(size<=MAXBUFSIZE)) bufsize=size;
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

/* skip one string in file
   don't bother about its value
*/
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

char *readposn() {
  unsigned long posn = readfourbytes().ptrval;
  sprintf(posnbuf, "line %u, column %u", posn/10000, posn%10000);
  return posnbuf;
}

void skipposn() { skipbytes(4); }

unsigned long readpointer() {
  return readfourbytes().ptrval;
}

void skippointer() {skipbytes(4);}

char readchar() {
  return nextbyte();
}

int readint() {
  return readfourbytes().intval;
}
void skipint() { skipbytes(4); }

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
  int n = (int) (nextbyte());
  skipbytes(n*4);
}

#define RATMAX (2*INTEGERMAX+2)
char ratbuf[RATMAX+1];

char *readrational() {
  sprintf(ratbuf, "%i:%% %i",readinteger(),readinteger());
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
    skippointer();
    skippointer();
    return 1;
  }
}

int isSAT() {
  char c = seenextbyte();
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
      skipint();
      break;
    case CHAR:
      readchar();
      break;
    case INTEGER:
      skipinteger();
      break;       
    case RATIONAL:
      readrational();
      break;
    case FLOAT:
      readfloat();
      break;
    case DOUBLE:
      readdouble();
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
      fileoffset=readpointer(); // follow this link for prettyPrint
      break;
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
  unsigned long p;

  while (1) {
    //printf("searching for App SAT... %u\n",fileoffset);
    seek(fileoffset);
    nodeType = nextbyte();
    //printf("node type: %i\n",nodeType);
    switch (nodeType) {
    case TRAPP: // Application
      {
	int arity = readarity();
	p = readpointer();  // fileoffset of App-trace
	skippointer();  // fileoffset of Function-trace
	skipbytes(4*(arity+1));
      }
      if (isSAT()) { // success! found the SATC!
 	return byteoffset();
      }
      else fileoffset = p; // follow parent!
      break;
    case TRNAM:   // for finding CAFs. SATc should be behind the TRNAM
      p=readpointer();
      skipbytes(4*2);
      if (p==0) { // found CAF!
	if (isSAT()) return byteoffset();
      }
      fileoffset=p;
      break;
    default: {
	unsigned long newfileoffset=followTrace(fileoffset);
	if (newfileoffset == fileoffset) {
	  if (isSAT()) return fileoffset;
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
    b = nextbyte();
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
	arity=readarity();
	apn=newAppNode(arity);

	exp = newExprNode(TRAPP);
	exp->v.appval = apn;

	skippointer();  // fileoffset of App-trace
	functionOffset = readpointer();
#ifdef DebugbuildExpr
	printf("Found application of arity: %i\n",arity);
#endif
	while (i++<arity) {  // now read all argument pointers into memory
	  setAppNodeArg(apn,i-1,(ExprNode*) readpointer()); // do something nasty
	  // abuse pointers for storing the fileoffsets temporarily
	}
	localoffset = byteoffset();
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
	seek(localoffset);   // back to correct position
	return exp;
	}
    case NTIDENTIFIER:
    case NTCONSTRUCTOR: {
      int infix,infixprio;
      exp = newExprNode(b);
      s=readstring();
      skippointer();            // skip modinfo
      infix=nextbyte();
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
      skippointer();
      fileoffset=readpointer(); // read NmType -> follow this link to build 
      // skippointer();
      break;
    case TRIND: //  Indirection
      skippointer();
      fileoffset=readpointer(); // follow this link for prettyPrint
      break;
    case NTINT:
      exp = newExprNode(b);
      exp->v.intval = readint();
      return exp;
    case NTCHAR:
      exp = newExprNode(b);
      exp->v.charval = readchar();
      return exp;
    case NTDOUBLE:
    case NTRATIONAL:
    case NTINTEGER:
      exp = newExprNode(b);
      exp->v.intval = readinteger();
      return exp;
    case NTFLOAT:
      exp = newExprNode(b);
      exp->v.floatval = readfloat();
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
    case TRSATA: // unevaluated expression
      if (verbose) {
	p = readpointer();
	exp = newExprNode(TRSATA);
	exp->v.expr = buildExprRek(p,verbose);
	return exp;
      } else
	return NULL;
    case TRSATB:
      exp = newExprNode(b);
      exp->v.intval = readpointer();
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
/* list datastructure, keeping a list of file offsets                */
/*                                                                   */
/*********************************************************************/

NodeList* newList() {
  int sz = sizeof(NodeList);
  NodeList* e = (NodeList*) calloc(1,sz); // sets both pointers to NULL!
}

void appendToList(NodeList *nl,unsigned long foffset) {
  int sz = sizeof(NodeElement);
  NodePtr e = (NodePtr) calloc(1,sz);
  if (e==NULL) {
    fprintf(stderr,"Tried to reserve %i bytes of memory.\n",sz);
    fprintf(stderr,"ERROR: No more space in heap!\n\n");
    exit(1);
  }
  e->fileoffset = foffset;
  if (nl->last==NULL) {
    nl->first = e;
    nl->last = e;
  } else {
    nl->last->next = e;
    nl->last = e;
  }
}

void addBeforeList(NodeList *nl,unsigned long foffset) {
  int sz = sizeof(NodeElement);
  NodePtr e = (NodePtr) calloc(1,sz);
  if (e==NULL) {
    fprintf(stderr,"Tried to reserve %i bytes of memory.\n",sz);
    fprintf(stderr,"ERROR: No more space in heap!\n\n");
    exit(1);
  }
  e->fileoffset = foffset;
  e->next=nl->first;
  nl->first = e;
  if (nl->last==NULL) nl->last=e;
}

void insertInList(NodeList *nl,unsigned long foffset) {
  int sz = sizeof(NodeElement);
  NodePtr l,e = (NodePtr) calloc(1,sz);
  if (e==NULL) {
    fprintf(stderr,"Tried to reserve %i bytes of memory.\n",sz);
    fprintf(stderr,"ERROR: No more space in heap!\n\n");
    exit(1);
  }
  e->fileoffset = foffset;
  l=nl->first;
  if ((l==NULL)||(l->fileoffset>=foffset)) {
    e->fileoffset=foffset;
    e->next = nl->first;
    nl->first = e;
    if (nl->last==NULL) nl->last = e;
    return;
  }
  while ((l->next!=NULL)&&(l->next->fileoffset<foffset)) l=l->next;
  e->next=l->next;
  l->next = e;
  if (e->next==NULL) nl->last=e;
}

int isInList(NodeList *nl,unsigned long foffset) {
  NodePtr e;
  if (nl->first==NULL) return 0; // list empty! => not in list!
  if ((foffset<nl->first->fileoffset)||(foffset>nl->last->fileoffset))
    return 0;  // foffset without range of stored values => not in list!
  e = nl->first;
  while ((e!=NULL)&&(e->fileoffset!=foffset)) e=e->next;
  return (e!=NULL);
}

void showList(NodeList *nl) {
  NodePtr e;
  e = nl->first;
  if (e==NULL) printf("EMPTY\n"); else
    {
      while (e!=NULL) {
	printf("element: %u\n",e->fileoffset);
	e=e->next;
      }
    }
}

unsigned long listLength(NodeList *nl) {
  NodePtr e;
  unsigned long l = 0;
  e = nl->first;
  while (e!=NULL) {
    e=e->next;
    l++;
  }
  return l;
}

void freeList(NodeList *nl) {
  NodePtr e,f;
  e = nl->first;
  while (e!=NULL) {
    f=e;
    e=e->next;
    free(f);
  }
  nl->first=NULL;
  nl->last=NULL;
}

/*********************************************************************/

/* show location in source file of this application/symbol */
void showLocation(unsigned long fileoffset) {
  char nodeType;
  char *s;
  unsigned long old = byteoffset(),posn;
  while (1) {
    seek(fileoffset);
    nodeType=nextbyte();
    switch(nodeType) {
    case SRCREF:
      fileoffset = readpointer();
      s = readposn();
      showLocation(fileoffset);
      printf(", %s\n",s);
      seek(old);
      return;
    case MDSUSPECT:
    case MDTRUSTED:
      s = readstring();

      printf("module \"%s\", ",s);
      s = readstring();
      printf("file \"%s\"",s);
      seek(old);
      return;
    case TRAPP:
      {
	int arity=nextbyte();
	skipbytes(4*(1+1+arity));
	fileoffset = readpointer();
	break;
      }
    case TRNAM:
      skipbytes(8);
      fileoffset = readpointer();
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
    nodeType=nextbyte();
    switch(nodeType) {
    case NTIDENTIFIER:
    case NTCONSTRUCTOR:
      skipstring(); // skip one string and handle like a SRCREF
      fileoffset = readpointer();
      skipbyte();
      s = readposn();
      showLocation(fileoffset);
      printf(", %s\n",s);
      seek(old);
      return;
    case SRCREF:
      fileoffset = readpointer();
      s = readposn();
      showLocation(fileoffset);
      printf(", %s\n",s);
      seek(old);
      return;
    case MDSUSPECT:
    case MDTRUSTED:
      s = readstring();

      printf("module \"%s\", ",s);
      s = readstring();
      printf("file \"%s\"",s);
      seek(old);
      return;
    case TRAPP:
      {
	//int arity=nextbyte();
	skipbytes(1+4);
	fileoffset = readpointer();
	break;
      }
    case TRNAM:
      skipbytes(4); // follow the NT
      fileoffset = readpointer();
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

  if (isSAT()) {
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

void showPretty(NodeList *nl,int verboseMode) {
  NodePtr e;
  FunTable* results = newFunTable();
  e = nl->first;

  if (e==NULL) printf("FUNCTION TABLE EMPTY\n"); else
    {
      unsigned long lngth = listLength(nl);
      unsigned long satc,lsz = 0,built = 0;
      while (e!=NULL) {
	satc=findAppSAT(e->fileoffset);  // find SATC for the application!
	if (isSAT()) {
	  ExprNode* r=buildExpr(satc,verboseMode);
	  ExprNode* a=buildExpr(e->fileoffset,verboseMode);
	  addToFunTable(results,a,r);
	}
	e=e->next;
      }
    }
  fflush(stderr);
  showFunTable(results);
  freeFunTable(results);
}

int isTrusted(unsigned long srcref) {
  char nodeType;
  unsigned long old = byteoffset();
  while (1) {
    seek(srcref);
    nodeType=nextbyte();
    switch(nodeType) {
    case TRAPP:
      skipbytes(5);
      srcref=readpointer();
      break;
    case TRSATA:
    case TRSATB:
    case TRSATC:
    case SRCREF:
      srcref = readpointer();
      break;
    case MDSUSPECT:
      seek(old);
      return 0;
    case MDTRUSTED:
      seek(old);
      return 1;
    case TRNAM:{
      unsigned long nmtype;
      skippointer();
      nmtype=readpointer(); // follow nmType by default
      srcref=readpointer(); // use srcref if nmType is lambda
      seek(nmtype);
      if (seenextbyte()!=NTLAMBDA) srcref=nmtype;
      break;
    }
    case NTGUARD:
    case NTIF:  // IFs are trusted. They do the right thing...
    case NTCASE: // CASEs are trusted, same reason...
    case NTCONSTRUCTOR: // constructors are "trusted"! => its applications are ok!
      seek(old);
      return 1;
    case NTIDENTIFIER:
      skipstring();
      srcref=readpointer(); // follow module info
      break;
    default:
      seek(old);
      return 0;
    }
  }
}

int isTopLevel(unsigned long srcref) {
  char nodeType,i;
  unsigned long old = byteoffset();
  while (1) {
    //fprintf(stderr,"%u ",srcref);
    seek(srcref);
    nodeType=nextbyte();
    switch(nodeType) {
    case SRCREF:
      skippointer;
      i=(readfourbytes().ptrval % 10000)==1;
      seek(old);
      //fprintf(stderr,"returning %i ",i);
      return i;
    case TRSATA:
    case TRSATB:
    case TRSATC:
      srcref=readpointer();
      break;
    case TRNAM:
      skippointer();
      srcref=readpointer(); // follow nmType
      break;
    case NTIDENTIFIER:
      skipstring();
      skipbytes(4+1);
      i=(readfourbytes().ptrval % 10000)==1;
      seek(old);
      //fprintf(stderr,"returning %i ",i);
      return i;
    case TRAPP:
      skipbytes(1+4);
      srcref = readpointer();
      break;
    default:
      seek(old);
      //fprintf(stderr,"returning 1 (unknown type) ");
      return 1;
    }
  }
}

unsigned long leftmostOutermost(unsigned long fileoffset) {
  char nodeType;
  while (1) {
    seek(fileoffset);
    nodeType=nextbyte();
    switch(nodeType) {
    case TRSATA:
    case TRSATB:
      return 0;
    case TRSATC:
      fileoffset=readpointer();
      break;
    case TRNAM:
      skippointer();
      fileoffset=readpointer(); // follow nmType
      break;
    case NTIDENTIFIER:
      return fileoffset;
    case TRAPP:
      skipbytes(1+4);
      fileoffset = readpointer();
      break;
    default:
      return 0;
    }
  }
}





















