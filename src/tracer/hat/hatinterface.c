/**************************************************************************/
/* hatinterface.c: general operations on hat files                        */
/*                                                                        */
/* Thorsten Brehm, 11/2001                                                */
/**************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include "hatinterface.h"
#include "hatgeneral.h"

/* routines providing interface to the hat archive file. */
#define MAXBUFSIZE 16384

int f;                  // file descriptor for archive

int bufsize=MAXBUFSIZE; // buffersize 
char* buf;              // input buffer
unsigned int buf_n;     // buf[0..buf_n-1] filled
unsigned int boff;      // if buf_n>0, boff in 0..buf_n-1 and buf[boff] is current
unsigned long foff;     // if buf_n>0, this is offset in f of buf[0]

typedef struct {int dummy;} hiddenHatFile;
typedef struct {int dummy;} hiddenFilePointer;
typedef unsigned long intFile;

typedef struct {
  int f;                  /* file descriptor for archive */
  int bufsize;
  char *buf;              /* input buffer */
  unsigned int buf_n;     /* buf[0..buf_n-1] filled */
  unsigned int boff;      /* if buf_n>0, boff in 0..buf_n-1 and buf[boff] is current */
  unsigned long foff;     /* if buf_n>0, this is offset in f of buf[0] */
  unsigned long filesize;
  struct stat statBuf;
  char* filename;
  unsigned long pprogress;
} filehandler;

int hatHandleCount = 0;
HatFile currentHandle=(HatFile) -1;
filehandler* hatHandle;

typedef union {char byte[4];
               unsigned long ptrval;
	       long intval;
	       float floatval;} fourbytes;

void _saveHandle(HatFile h) {
  if ((int) h<0) return;
  hatHandle[(int) h].f=f;
  hatHandle[(int) h].bufsize=bufsize;
  hatHandle[(int) h].buf = buf;
  hatHandle[(int) h].buf_n = buf_n;
  hatHandle[(int) h].boff = boff;
  hatHandle[(int) h].foff = foff;
}

void _loadHandle(HatFile h) {
  if ((int) h<0) return;
  f= hatHandle[(int) h].f;
  bufsize = hatHandle[(int) h].bufsize;
  buf = hatHandle[(int) h].buf;
  buf_n = hatHandle[(int) h].buf_n;
  boff = hatHandle[(int) h].boff;
  foff = hatHandle[(int) h].foff;
}

void _hatSwitchToHandle(HatFile h) {
  if (h==currentHandle) return;
  if ((int) h>=hatHandleCount) {
    fprintf(stderr,"ERROR: Trying to access an invalid handle in hatinterface!\n");
  }
  _saveHandle(currentHandle);
  currentHandle = h;
  _loadHandle(currentHandle);
}

HatFile hatNewHandle() {
  filehandler* newHandles = (filehandler*) calloc(++hatHandleCount,sizeof(filehandler));
  if (hatHandleCount>1)
    memcpy(newHandles,hatHandle,sizeof(filehandler)*(hatHandleCount-1));
  free(hatHandle);
  (int) hatHandle = newHandles;
  hatHandle[hatHandleCount-1].bufsize = MAXBUFSIZE;
  hatHandle[hatHandleCount-1].buf = (char*) calloc(MAXBUFSIZE,1);
  hatHandle[hatHandleCount-1].filename = NULL;
  return (HatFile) (hatHandleCount-1);
}

/* make proper file extension, if missing */
char* hatFileExtension(char* name) {
  if (strstr(name,".hat")!=NULL) return newStr(name);
  else {
    char* newstr=(char*) malloc((strlen(name)+5)*sizeof(char));
    strcpy(newstr,name);
    strcat(newstr,".hat");
    return newstr;
  }
}

unsigned long hatFileSize(HatFile h) {return hatHandle[(int) h].filesize;}

/* set new file position */
void hatSeekNode(HatFile h,filepointer ofs) {
  _hatSwitchToHandle(h);
  if (((intFile) ofs>=foff)&&((intFile) ofs<foff+buf_n)) { // new position is within the buffer!
    boff=((intFile) ofs)-foff;   // only reset the buffer position!
  } else {
    boff=0;   // set internal pointers correctly
    foff=(intFile) ofs;
    buf_n=0;      // buf_n=0, nothing in memory. read from file on next read!
    lseek(f,(intFile) ofs,0); // do seek in file
  }
}

/* check for EOF */
int hatSeqEOF(HatFile h,filepointer nodenumber) {
  hatSeekNode(h,nodenumber);
  if (boff<buf_n) {
    return 0; /* more data in buffer! */
  } else {
    foff+=buf_n; // read next block
    boff-=buf_n;
    buf_n=read(f,buf,bufsize);
    return (buf_n<=boff);  // more data?
  }
}

/* return position in file */
filepointer hatNodeNumber(HatFile h) {
  _hatSwitchToHandle(h);
  return (filepointer) (foff + boff);
}

/* Routines to extract values encoded as one or more bytes. */

// #define countPageMiss

#ifdef countPageMiss
int bufferMiss = 0;
#endif

/* read one byte from file */
char nextbyte() {
  char c;
  if (boff>=buf_n) {  // need to read a new block
    foff += buf_n;
    boff -= buf_n;    // reduce boff appropriately
    buf_n = read(f,buf,bufsize);
#ifdef countPageMiss
    bufferMiss++;
#endif
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

#define POSNMAX 50
char posnbuf[POSNMAX+1];

filepointer readpointer() {
  return (filepointer) readfourbytes().ptrval;
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

filepointer hatSeqFirst(HatFile h) {
  hatSeekNode(h,0);
  skipstring();
  skipbytes(8);
  return (filepointer) (foff + boff);
}

BOOL isSAT(HatFile handle,filepointer fileoffset) {
  char c;
  filepointer old = hatNodeNumber(handle);
  
  c = getNodeType(handle,fileoffset);
  hatSeekNode(currentHandle,old);

  return ((c==HatSATC)||(c==HatSATB)||(c==HatSATA));
}

BOOL _isNotIsolatedSAT(HatFile handle,filepointer fileoffset) {
  char c;
  filepointer old = hatNodeNumber(handle);
  
  c = seenextbyte();
  hatSeekNode(currentHandle,old);

  return ((c==HatSATC)||(c==HatSATB)||(c==HatSATA));
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
	fprintf(stderr, "skipNode: strange low-bits tag %d in TR 0x%x\n",
		lo5(nodeType), (int) (hatNodeNumber(currentHandle))-1);
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
    case TOPIDENTIFIER:
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
    case IF:
    case GUARD:
    case CONTAINER:
	break;
    case CSTRING:
	skipstring();
	break;
    default:
	fprintf(stderr, "skipNode: strange low-bits tag %d in NT 0x%x\n",
		lo5(nodeType), (int) (hatNodeNumber(currentHandle))-1);
	exit(1);
    }
    break;
  case SR:
      skipbytes(4+4);
      break;
  default:
      fprintf(stderr, "skipNode: strange high-bits tag %d at byte offset 0x%x\n",
	      hi3(nodeType), (int) (hatNodeNumber(currentHandle))-1);
      exit(1);
  }
}

/* follow the trace along SATCs, projections and Constants */
filepointer hatFollowTrace(HatFile handle,filepointer fileoffset) {
  char nodeType;
  while (1) {
    //printf("following Trace... 0x%x\n",fileoffset);
    nodeType = getNodeType(handle,fileoffset);
    //printf("node type: %i\n",b);
    switch (nodeType) {
    case HatConstant:
      fileoffset=getAtom();break;
    case HatProjection: //  Indirection
    case HatSATC:
      fileoffset=getProjValue();
      break;
    default:
      return fileoffset;
    }
  }
}

/* follow the trace along HIDDEN traces */
filepointer hatFollowHidden(HatFile handle,filepointer fileoffset) {
  char nodeType;
  
  while (1) {
    nodeType = getNodeType(handle,fileoffset);
    if (nodeType==HatHidden) {
      fileoffset=getParent(); // follow parent...
    } else {
      return fileoffset;
    }
  }
}

/* follow a trail of SATCs */
filepointer hatFollowSATCs(HatFile handle,filepointer fileoffset) {
  char nodeType;
  
  while (1) {
    nodeType = getNodeType(handle,fileoffset);
    switch (nodeType) {
    case HatSATC:
      fileoffset=getProjValue(); // follow link...
      break;
    default:
      return fileoffset;
    }
  }
}

/* find the SAT belonging to a redex (node must be application or name) */
/* return value 0: no SAT found, otherwise: offset for SAT */
filepointer getResult() {
  char nodeType;
  HatFile handle = currentHandle;
  filepointer p,satc,fileoffset = hatNodeNumber(handle);

  while (fileoffset!=0) {
    hatSeekNode(handle,fileoffset);
    nodeType =  seenextbyte(); // identify isolated SATs! (getNodeType doesn't)
    switch (nodeType) {
    case HatApplication: // Application
      p = getParent();
      satc = hatSeqNext(handle,fileoffset);
      if (_isNotIsolatedSAT(handle,satc)) { // success! found the SATC!
 	return satc;
      }
      if ((getNodeType(handle,satc)==HatApplication)&& // does an App Node follow?
	  (getAppFun()==fileoffset)) { // Attention: following App node references this one
	return 0; // no result available! It's just a partial application
      }
      fileoffset = p; // follow parent!
      break;
    case HatConstant:   // for finding CAFs. SATc should be behind the HatConstant
      p=getParent();
      satc = hatSeqNext(handle,fileoffset);
      if (_isNotIsolatedSAT(handle,satc)) return satc;
      return 0; // for HatConstants: SATC must be immediately after the node, do not
      // search its parents!
    default: {
	filepointer newfileoffset=hatFollowTrace(handle,fileoffset);
	if (newfileoffset == fileoffset) {
	  if (isSAT(handle,newfileoffset)) return fileoffset;
	  else return 0;
	} else
	  fileoffset = newfileoffset;
      }
    }
  }
  return 0;
}

filepointer hatResult(HatFile handle,filepointer fileoffset) {
  switch(getNodeType(handle,fileoffset)) {
   case HatConstant:
   case HatApplication:return getResult();
  default:return InvalidFilePointer;
  }
}

/* show location in source file of this application/symbol */
char* hatLocationStr(HatFile handle,filepointer fileoffset) {
  char nodeType;
  char *s,*tmp;
  filepointer old = hatNodeNumber(handle);
  while (1) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case HatSrcRef:
      fileoffset = getModInfo();
      s = getPosnStr();
      tmp = hatLocationStr(handle,fileoffset);
      replaceStr(&tmp,tmp,", ",s);
      // printf(", %s\n",s);
      hatSeekNode(handle,old);
      return tmp;
    case HatModule:
      tmp = getName();
      tmp=catStr("module \"",tmp,"\", ");
      //printf("module \"%s\", ",s);
      s = getModuleSrcName();
      replaceStr(&tmp,tmp,"file \"",s);
      replaceStr(&tmp,tmp,"\"","");
      //printf("file \"%s\"",s);
      hatSeekNode(handle,old);
      return tmp;
    case HatApplication:
      {
	//fileoffset = getSrcRef();
	fileoffset = getAppFun();
	break;
      }
    case HatConstant:
      //fileoffset = getSrcRef();
      fileoffset = getAtom();
      break;
    case HatIdentifier:
      fileoffset = getModInfo();
      s = getPosnStr();
      tmp = hatLocationStr(handle,fileoffset);
      replaceStr(&tmp,tmp,", ",s);
      //printf(", %s\n",s);
      hatSeekNode(handle,old);
      return tmp;
    default:
      hatSeekNode(handle,old);
      return newStr("");
    }
  }
}

/* show location where function was defined */
char* hatFunLocationStr(HatFile handle,filepointer fileoffset) {
  char nodeType;
  char *s,*tmp;
  filepointer old = hatNodeNumber(handle);
  while (1) {
    fileoffset = hatFollowSATCs(handle,fileoffset);
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case HatIdentifier:
    case HatConstructor:
      fileoffset = getParent();
      s = getPosnStr();
      tmp = hatLocationStr(handle,fileoffset);
      replaceStr(&tmp,tmp,", ",s);
      //printf(", %s\n",s);
      hatSeekNode(handle,old);
      return tmp;
    case SRCREF:
      fileoffset = getModInfo();
      s = getPosnStr();
      tmp = hatLocationStr(handle,fileoffset);
      replaceStr(&tmp,tmp,", ",s);
      //printf(", %s\n",s);
      hatSeekNode(handle,old);
      return tmp;
    case MDSUSPECT:
    case MDTRUSTED:
      s = getName();

      //printf("module \"%s\", ",s);
      s=catStr("module \"",s,"\", ");
      tmp = getModuleSrcName();
      tmp=catStr(tmp,"file \"",s);
      replaceStr(&tmp,tmp,"\"","");
      freeStr(s);
      //printf("file \"%s\"",s);
      hatSeekNode(handle,old);
      return tmp;
    case HatApplication:
      {
	fileoffset = getAppFun();
	break;
      }
    case HatConstant:
      fileoffset = getAtom();
      break;
    default:
      //printf("no location at offset: 0x%x\n",fileoffset);
      hatSeekNode(handle,old);
      return newStr("<no location>");
    }
  }
}

BOOL isTrusted(HatFile handle,filepointer srcref) {
  char nodeType;
  filepointer old = hatNodeNumber(handle);
  while (1) {
    nodeType=getNodeType(handle,srcref);
    switch(nodeType) {
    case HatApplication:
      srcref=getAppFun();
      break;
    case HatSATA:
    case HatSATB:
    case HatSATC:
      srcref = getProjValue();
      break;
    case HatSrcRef:
      srcref = getParent();
      break;
    case HatModule:{
      char trusted = getModuleTrusted();
      hatSeekNode(handle,old);
      return trusted;
    }
    case HatConstant:{
      filepointer nmtype;
      nmtype=getAtom(); // follow nmType by default
      srcref=getSrcRef();   // use srcref if nmType is lambda
      if (getNodeType(handle,nmtype)!=HatLambda) srcref=nmtype;
      break;
    }
    case HatGuard:
    case HatIf:   // IFs are trusted. They do the right thing...
    case HatCase: // CASEs are trusted, same reason...
    case HatTuple:
    case HatConstructor: // constructors are "trusted"! => its applications are ok!
      hatSeekNode(handle,old);
      return 1;
    case HatIdentifier:
      srcref=getModInfo(); // follow module info
      break;
    default:
      hatSeekNode(handle,old);
      return 0;
    }
  }
}

int isCAF(HatFile handle,filepointer fileoffset) {
  char nodeType;
  filepointer old = hatNodeNumber(handle);
  nodeType=getNodeType(handle,fileoffset);
  hatSeekNode(handle,old);
  switch(nodeType) {
  case HatConstant:
    return 1;
  default:
    return 0;
  }
}

BOOL _internalIsLHSModule(HatFile handle,filepointer modinfo) {
  if (modinfo==0) return 0;
  if (getNodeType(handle,modinfo)==HatModule) {
    char *s2,*s=getModuleSrcName();
    if (s==NULL) return 0;
    while ((*s!=0)&&(*s!='.')) s++; // strrchr DOES NOT WORK! A bug!?
    if (*s=='.') {                  // so we do our own comparison...
      s++;
      if (strcmp(s,"lhs")==0) return 1;
    } else return 0;
  }
  return 0;
}

BOOL getTopLevelFlag() {
  return (seenextbyte()==NTTOPIDENTIFIER);
}

int isTopLevel(HatFile handle,filepointer srcref) {
  char nodeType,i;
  filepointer old = hatNodeNumber(handle);
  while (1) {
    nodeType=getNodeType(handle,srcref);
    switch(nodeType) {
    case SRCREF:
      i=getPosnColumn();
      i=((i<=1)||((i<=3)&&(_internalIsLHSModule(handle,getModInfo()))));
      hatSeekNode(handle,old);
      return i;
    case HatSATA:
    case HatSATB:
    case HatSATC:
      srcref=getProjValue();
      break;
    case HatConstant:
      srcref=getAtom(); // follow nmType
      break;
    case HatIdentifier:
//    i=getPosnColumn();
//    i=((i<=1)||((i<=3)&&(_internalIsLHSModule(handle,getModInfo()))));
//    hatSeekNode(handle,old);
//    return i;
      return getTopLevelFlag();
    case HatApplication:
      srcref = getAppFun();
      break;
    case HatCase:
    case HatIf:
    case HatGuard:
    case HatLambda:
    case HatContainer:
    case HatTuple:
    case HatDummy:
    case HatFun:
    case HatCString:
    case HatHidden: // neccessary for isChild
      return 0; // not toplevel (necessary for hat-observe funA in funA!)
    default:
      hatSeekNode(handle,old);
      return 1;
    }
  }
}

filepointer hatOutermostSymbol(HatFile handle,filepointer fileoffset) {
  char nodeType;
  while (1) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case HatSATA:
    case HatSATB:
      return 0;
    case HatSATC:
      fileoffset=getProjValue();
      break;
    case HatConstant:
      fileoffset=getAtom(); // follow nmType
      break;
    case HatIdentifier:
    case HatConstructor:
      return fileoffset;
    case HatApplication:
      fileoffset = getAppFun();
      break;
    default:
      return 0;
    }
  }
}

// return leftmost NAME (not identifier/constructor as hatOutermost)
filepointer hatOutermostName(HatFile handle,filepointer fileoffset) {
  char nodeType;
  while (1) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case HatSATA:
    case HatSATB:
      return 0;
    case HatSATC:
      fileoffset=getProjValue();
      break;
    case HatConstant:
      return fileoffset;
      break;
    case HatIdentifier:
    case HatConstructor:
      return fileoffset;
    case HatApplication:
      fileoffset = getAppFun();
      break;
    default:
      return 0;
    }
  }
}

filepointer hatTopAncestor(HatFile handle,filepointer fileoffset) {
  filepointer prev = 0;
  filepointer old = hatNodeNumber(handle);
  char nodeType;

  while (fileoffset!=0) {
    prev = fileoffset;
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case HatSATC:
    case HatProjection:
      fileoffset = getProjValue();
      break;
    case HatHidden:
    case HatConstant:
    case HatApplication:
      fileoffset = getParent();
      break;
    default:
      hatSeekNode(handle,old);
      return 0;
    }
  }
  hatSeekNode(handle,old);
  return prev;
}

filepointer hatMainCAF(HatFile h) {
  filepointer currentOffset,satc,srcref;
  char nodeType;

  currentOffset = hatSeqFirst(h);
  while (!hatSeqEOF(h,currentOffset)) {
    nodeType = getNodeType(h,currentOffset);
    switch (nodeType) {
    case HatConstant: // Name
      if (getParent()==0) { // is parent 0?
	srcref = getSrcRef();
	satc = hatSeqNext(h,currentOffset);
	if ((srcref!=0)&&(_isNotIsolatedSAT(h,satc))) {  // SATC behind HatConstant?
	  // found a CAF!
	  //if (isTrusted(h,srcref)) printf("isTrusted\n");
	  //if (isTopLevel(h,currentOffset)==0) printf("main is not top-level!\n");
	  if ((isTrusted(h,srcref)==0)&&
	      (isTopLevel(h,currentOffset))) {
	    filepointer lmo = hatOutermostSymbol(h,currentOffset);
	    if ((lmo!=0)&&(getNodeType(h,lmo)==HatIdentifier)&&
		(strcmp(getName(),"main")==0)) {
	      return currentOffset;
	    }
	    currentOffset = satc;
	  } else currentOffset = satc;
	} else currentOffset = satc;
      } else currentOffset = hatSeqNext(h,currentOffset);
      break;
    default:
      currentOffset = hatSeqNext(h,currentOffset);
      break;
    }
  }
  return 0;
}

/* set beginning of internal buffer to current position in file */
void resetFilepointer() {
  foff += boff;    // make position in buffer new file offset
  boff = 0;
  lseek(f,foff,0); // set filepointer
  buf_n = read(f,buf,bufsize); // read data
#ifdef countPageMiss
    bufferMiss++;
#endif
}

/* make sure, next <bytes> are in buffer */
void prepareBuffer(int bytes) {
  if (boff+bytes>=buf_n) {
    resetFilepointer();
  }
}

char getAppArity() {
  prepareBuffer(1);
  return buf[boff+1];
}

filepointer getParent() {
  unsigned int lbuf;
  filepointer fp;

  prepareBuffer(5); // need 5 bytes after current position
  lbuf = boff;

  if (nextbyte()==HatApplication) {
    skipbyte(); // skip one byte in applications (arity)
  }
  fp = readpointer();
  boff = lbuf;
  return fp;
}

filepointer getAtom() {
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
  case HatApplication:
    arity = nextbyte();
    boff = lbuf;
    prepareBuffer((arity+3)*4+1);
    lbuf = boff;
    skipbytes((arity+2)*4+2);
    fp = readpointer();
    break;
  case HatConstant:
    prepareBuffer(12);
    skipbytes(8);
    fp = readpointer();
    break;
  default:
    fprintf(stderr,"ERROR in getSrcRef: node is not of type Application or Name!\n");
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

filepointer getAppFun() {
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

char* getStringValue() {return getName();}

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
  //printf("integer length %i\n",n);
  i = n;
  if (i<0) i=-i;
  
  boff = lbuf;
  if (n==0) return 0;
  prepareBuffer(2+(i*4));
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

#define RATMAX (2*INTEGERMAX+2)
char ratbuf[RATMAX+1];
// rationals not yet used: rational values are still represented as a constructor
// application to two integer arguments!
char *getRationalValue() {
  unsigned long fp;

  fp = foff;
  skipbyte();
  sprintf(ratbuf, "%i :%% %i",readinteger(),readinteger());
  foff = fp;
  buf_n=0;
  return newStr(ratbuf);
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

  skipbyte();
  v.a[0] = readfourbytes();
  v.a[1] = readfourbytes();
  v.a[1].ptrval = htonl(v.a[1].ptrval);
  v.a[0].ptrval = htonl(v.a[0].ptrval);

  boff = lbuf;
  return v.d;
}

char getNodeType(HatFile h,filepointer nodenumber) {
  char nodeType;
  hatSeekNode(h,nodenumber);
  nodeType=seenextbyte();
  if ((nodeType&240)==0) nodeType=nodeType & 247; // mask isolated SATs
  if (nodeType==NTTOPIDENTIFIER) return HatIdentifier;
  return (nodeType == MDTRUSTED ? HatModule : nodeType);
}

filepointer hatSeqNext(HatFile h,filepointer nodenumber) {
  hatSeekNode(h,nodenumber);
  skipNode(nextbyte());
  return (filepointer) (foff + boff);
}

char* getName() { // get module, constructor or identifier name
  unsigned long fp=foff;
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

char _getInfix() {
  unsigned long fp=foff;
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

char getInfixType() {
  return (_getInfix() % 4);
}

char getInfixPrio() {
  return (_getInfix() / 4);
}

filepointer getProjValue() {
  unsigned int lbuf;
  filepointer trace;
  prepareBuffer(8);  
  lbuf = boff;
  if (nextbyte()==HatProjection) skippointer();
  trace = readpointer();
  
  boff = lbuf;
  return trace;
}

char* getModuleSrcName() {
  unsigned long fp=foff;
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
  unsigned long fp=foff;
  unsigned int lbuf=boff;
  filepointer res;
  
  if (nextbyte()!=HatSrcRef) {
    skipstring();  // skip string for HatIdentifier and HatConstructor
  }
  res = readpointer();
  
  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp; // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return res;
}

BOOL getModuleTrusted() {
  return (seenextbyte()==MDTRUSTED);
}

unsigned long _getPosn() {
  unsigned long fp=foff;
  unsigned int lbuf=boff;
  unsigned long res;
  
  if (nextbyte()==SRCREF) {
    skipbytes(4);
  } else {
    skipstring(); // for HatConstructor, HatIdentifier
    skipbytes(5);
  }
  res = (unsigned long) readpointer();
  
  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp; // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return res;
}

int getPosnColumn() {
  return (_getPosn() % 10000);
}

int getPosnRow() {
  return (_getPosn() / 10000);
}

char* getPosnStr() {
  unsigned long posn = _getPosn();
  
  sprintf(posnbuf, "line %u, column %u", posn/10000, posn%10000);
  return posnbuf;
}

char* hatVersionNumber() {
#ifdef VERSION
  return newStr(VERSION);
#else
  return newStr("???");
#endif
}

filepointer hatErrorRedex(HatFile h) {    // return the error entry point, if
                                          // evaluation failed, otherwise 0 returned
  filepointer p,old = hatNodeNumber(h);
  hatSeekNode(h,0);
  skipstring(); // skip version info string
  p = readpointer(); // get entry point for trace
  hatSeekNode(h,old);
  return p;
}

char* hatErrorMessage(HatFile h) {  // return the error message, otherwise NULL
  char* errorMessage=NULL;
  filepointer p,old = hatNodeNumber(h);
  hatSeekNode(h,0);
  skipstring();  // skip version info string
  skippointer(); // skip entry point for trace
  p = readpointer(); // get NmType CString for error message
  if ((p!=0)&&(getNodeType(h,p)==HatCString)) {
    errorMessage = getStringValue();
  }
  hatSeekNode(h,old);
  return errorMessage;
}

/* reading, checking and/or writing header and node information */
int hatTestHeader(HatFile h) {
  char *version;
  hatSeekNode(h,0);
  version = readstring();
  if (strncmp(version,"Hat",3)) {
    fprintf(stderr,"(error): file does not appear to be a Hat archive.\n");
    fprintf(stderr,"    Quitting.\n");
    return 0;
  }
  if (strncmp(version+3,VERSION,4)) {
    fprintf(stderr,"(warning): file appears to be a Hat archive in format %s\n"
                  ,version+3);
    fprintf(stderr,"   but this tool deals with format version %s\n",VERSION);
    fprintf(stderr,"   I'm continuing, but there may be unexpected errors.\n");
  }
  skipbytes(8);
  return 1;
}

/* open file for reading, save in internal file descriptor */
HatFile hatOpenFile(char* name) {
  int f;
  name = hatFileExtension(name);
  f = open(name, 0);
  if (f!=-1) {
    int handle = (int) hatNewHandle();
    stat(name, &(hatHandle[(int) handle].statBuf));
    hatHandle[handle].filesize = hatHandle[handle].statBuf.st_size;
    hatHandle[handle].pprogress = hatHandle[handle].statBuf.st_size/1000;
    if (hatHandle[handle].pprogress==0) hatHandle[handle].statBuf.st_size=1;
    hatHandle[handle].filename = name;
    hatHandle[handle].buf_n = 0; // set buffer pointers appropriately. buf_n=0 buffer currently empty
    hatHandle[handle].boff = 0;  // at position 0 in buffer
    hatHandle[handle].foff = 0;  // at position 0 in file
    hatHandle[handle].f = f;
    currentHandle = (HatFile) handle;
    _loadHandle((HatFile) handle);
    if (hatTestHeader((HatFile) handle)==0) {
      hatCloseFile((HatFile) handle);
      return HatFileBadVersion;
    }
    return (HatFile) handle;
  } else
    return HatFileNotFound;
}

/* close file in internal file descriptor */
void hatCloseFile(HatFile h) {
  //printf("Closing file %u\n",handle);
  close(hatHandle[(int) h].f);
  free(hatHandle[(int) h].buf);
  freeStr(hatHandle[(int) h].filename);
  if (h==currentHandle) {
    currentHandle = HatFileNotFound;
  }
}

char* hatFileName(HatFile h) {
  return hatHandle[(int) h].filename;
}

int perProgress(HatFile h,filepointer f) {
  return ((unsigned long) f)/10/hatHandle[(int) h].pprogress;
}
