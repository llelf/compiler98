/**************************************************************************/
/* hatinterface.c: general operations on hat files                        */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
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
} filehandler;

int hatHandleCount = 0,currentHandle=-1;
filehandler* hatHandle;

typedef union {char byte[4];
               unsigned long ptrval;
	       long intval;
	       float floatval;} fourbytes;

void _saveHandle(HatFile h) {
  if (h<0) return;
  hatHandle[h].f=f;
  hatHandle[h].bufsize=bufsize;
  hatHandle[h].buf = buf;
  hatHandle[h].buf_n = buf_n;
  hatHandle[h].boff = boff;
  hatHandle[h].foff = foff;
}

void _loadHandle(HatFile h) {
  if (h<0) return;
  f= hatHandle[h].f;
  bufsize = hatHandle[h].bufsize;
  buf = hatHandle[h].buf;
  buf_n = hatHandle[h].buf_n;
  boff = hatHandle[h].boff;
  foff = hatHandle[h].foff;
}

void _hatSwitchToHandle(HatFile h) {
  if (h==currentHandle) return;
  if (h>=hatHandleCount) {
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
  hatHandle = newHandles;
  hatHandle[hatHandleCount-1].bufsize = MAXBUFSIZE;
  hatHandle[hatHandleCount-1].buf = (char*) calloc(MAXBUFSIZE,1);
  hatHandle[hatHandleCount-1].filename = NULL;
  return hatHandleCount-1;
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

unsigned long hatFileSize(HatFile h) {return hatHandle[h].filesize;}

/* set new file position */
void hatSeekNode(HatFile h,filepointer ofs) {
  _hatSwitchToHandle(h);
  if ((ofs>=foff)&&(ofs<foff+buf_n)) { // new position is within the buffer!
    boff=ofs-foff;   // only reset the buffer position!
  } else {
    boff=0;   // set internal pointers correctly
    foff=ofs;
    buf_n=0;      // buf_n=0, nothing in memory. read from file on next read!
    lseek(f,ofs,0); // do seek in file
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
  return foff + boff;
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
  return foff + boff;
}

BOOL isSAT(HatFile handle,filepointer fileoffset) {
  char c;
  filepointer old = hatNodeNumber(handle);
  
  c = getNodeType(handle,fileoffset);
  hatSeekNode(currentHandle,old);

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
	fprintf(stderr, "skipNode: strange low-bits tag %d in TR 0x%x\n",
		lo5(nodeType), hatNodeNumber(currentHandle)-1);
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
		lo5(nodeType), hatNodeNumber(currentHandle)-1);
	exit(1);
    }
    break;
  case SR:
      skipbytes(4+4);
      break;
  default:
      fprintf(stderr, "skipNode: strange high-bits tag %d at byte offset 0x%x\n",
	      hi3(nodeType), hatNodeNumber(currentHandle)-1);
      exit(1);
  }
}

/* follow the trace along SATs, indirections and TRNAMEs */
filepointer hatFollowTrace(HatFile handle,filepointer fileoffset) {
  char nodeType;
  
  while (1) {
    //printf("following Trace... 0x%x\n",fileoffset);
    nodeType = getNodeType(handle,fileoffset);
    //printf("node type: %i\n",b);
    switch (nodeType) {
    case TRNAM:
      fileoffset=getNameType();break;
    case TRIND: //  Indirection
    case TRSATCIS:
    case TRSATC:
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
    if (nodeType==TRHIDDEN) {
      fileoffset=getParent(); // follow parent...
    } else {
      return fileoffset;
    }
  }
}

/* follow the trace along SATs */
filepointer hatFollowSATs(HatFile handle,filepointer fileoffset) {
  char nodeType;
  
  while (1) {
    nodeType = getNodeType(handle,fileoffset);
    switch (nodeType) {
      //case TRSATCIS:
    case TRSATC:
      fileoffset=getProjValue(); // follow link...
      break;
    default:
      return fileoffset;
    }
  }
}

/* find the SAT belonging to an application */
/* return value 0: no SAT found, otherwise: offset for SAT */
filepointer getResult(HatFile handle,filepointer fileoffset) {
  char nodeType;
  filepointer p,satc;

  while (fileoffset!=0) {
    hatSeekNode(handle,fileoffset);
    nodeType =  seenextbyte(); // identify isolated SATs! (getNodeType doesn't)
    switch (nodeType) {
    case TRAPP: // Application
      p = getParent();
      satc = hatSeqNext(handle,fileoffset);
      if (isSAT(handle,satc)) { // success! found the SATC!
 	return satc;
      }
      if ((getNodeType(handle,satc)==HatApplication)&& // does an App Node follow?
	  (getAppFun()==fileoffset)) { // Attention: following App node references this one
	return 0; // no result available! It's just a partial application
      }
      fileoffset = p; // follow parent!
      break;
    case TRNAM:   // for finding CAFs. SATc should be behind the TRNAM
      p=getParent();
      satc = hatSeqNext(handle,fileoffset);
      if (isSAT(handle,satc)) return satc;
      return 0; // for TRNAMs: SATC must be immediately after the node, do not
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

/* show location in source file of this application/symbol */
char* hatLocationStr(HatFile handle,filepointer fileoffset) {
  char nodeType;
  char *s,*tmp;
  filepointer old = hatNodeNumber(handle);
  while (1) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case SRCREF:
      fileoffset = getModInfo();
      s = getPosnStr();
      tmp = hatLocationStr(handle,fileoffset);
      replaceStr(&tmp,tmp,", ",s);
      // printf(", %s\n",s);
      hatSeekNode(handle,old);
      return tmp;
    case MDSUSPECT:
    case MDTRUSTED:
      tmp = getName();
      tmp=catStr("module \"",tmp,"\", ");
      //printf("module \"%s\", ",s);
      s = getModuleSrcName();
      replaceStr(&tmp,tmp,"file \"",s);
      replaceStr(&tmp,tmp,"\"","");
      //printf("file \"%s\"",s);
      hatSeekNode(handle,old);
      return tmp;
    case TRAPP:
      {
	//fileoffset = getSrcRef();
	fileoffset = getAppFun();
	break;
      }
    case TRNAM:
      //fileoffset = getSrcRef();
      fileoffset = getNameType();
      break;
    case NTIDENTIFIER:
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
    fileoffset = hatFollowSATs(handle,fileoffset);
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case NTIDENTIFIER:
    case NTCONSTRUCTOR:
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
    case TRAPP:
      {
	fileoffset = getAppFun();
	break;
      }
    case TRNAM:
      fileoffset = getNameType();
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
    case HatName:{
      filepointer nmtype;
      nmtype=getNameType(); // follow nmType by default
      srcref=getSrcRef();   // use srcref if nmType is lambda
      if (getNodeType(handle,nmtype)!=NTLAMBDA) srcref=nmtype;
      break;
    }
    case NTGUARD:
    case NTIF:   // IFs are trusted. They do the right thing...
    case NTCASE: // CASEs are trusted, same reason...
    case TUPLE:
    case NTCONSTRUCTOR: // constructors are "trusted"! => its applications are ok!
      hatSeekNode(handle,old);
      return 1;
    case NTIDENTIFIER:
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
  case TRNAM:
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

BOOL isNTToplevel() {
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
    case TRSATA:
    case TRSATB:
    case TRSATC:
    case TRSATAIS:
    case TRSATBIS:
    case TRSATCIS:
      srcref=getProjValue();
      break;
    case TRNAM:
      srcref=getNameType(); // follow nmType
      break;
    case NTIDENTIFIER:
//    i=getPosnColumn();
//    i=((i<=1)||((i<=3)&&(_internalIsLHSModule(handle,getModInfo()))));
//    hatSeekNode(handle,old);
//    return i;
      return isNTToplevel();
    case TRAPP:
      srcref = getAppFun();
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
    case TRHIDDEN: // neccessary for isChild
      return 0; // not toplevel (necessary for hat-observe funA in funA!)
    default:
      hatSeekNode(handle,old);
      return 1;
    }
  }
}

filepointer hatLMO(HatFile handle,filepointer fileoffset) {
  char nodeType;
  while (1) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case TRSATA:
    case TRSATB:
    case TRSATBIS:
    case TRSATAIS:
      return 0;
    case TRSATC:
    case TRSATCIS:
      fileoffset=getProjValue();
      break;
    case TRNAM:
      fileoffset=getNameType(); // follow nmType
      break;
    case NTIDENTIFIER:
    case NTCONSTRUCTOR:
      return fileoffset;
    case TRAPP:
      fileoffset = getAppFun();
      break;
    default:
      return 0;
    }
  }
}

// return leftmost NAME (not identifier/constructor as hatLMO)
filepointer hatLMOName(HatFile handle,filepointer fileoffset) {
  char nodeType;
  while (1) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case TRSATA:
    case TRSATB:
    case TRSATBIS:
    case TRSATAIS:
      return 0;
    case TRSATC:
    case TRSATCIS:
      fileoffset=getProjValue();
      break;
    case TRNAM:
      return fileoffset;
      break;
    case NTIDENTIFIER:
    case NTCONSTRUCTOR:
      return fileoffset;
    case TRAPP:
      fileoffset = getAppFun();
      break;
    default:
      return 0;
    }
  }
}

filepointer hatInitialCAF(HatFile handle,filepointer fileoffset) {
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
    case TRNAM:
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

int isDescendantOf(HatFile handle,filepointer fileoffset,filepointer parent) {
  char nodeType;
  filepointer old = hatNodeNumber(handle);

  if (parent==0) return 0; 

  if (getNodeType(handle,fileoffset)==HatApplication) {
    fileoffset = hatLMOName(handle,fileoffset);
    if (fileoffset!=0) {
      getNodeType(handle,fileoffset);
      fileoffset=getParent();
    }
  }
  if (fileoffset==parent) return 1;

  while (fileoffset!=0) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case HatHidden:
    case HatProjection:
      fileoffset=getParent();
      break;
    case HatSATC:
      fileoffset=getProjValue();
      break;
    case HatName:
      if (getNameType()==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      fileoffset = getParent();
      break;
    case HatApplication:{
      filepointer newoffs;
      newoffs = getAppFun(); //Parent();
      if (hatLMO(handle,fileoffset)==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      getNodeType(handle,newoffs);
      fileoffset = getParent();
      break;
    }
    default:
      hatSeekNode(handle,old);
      return 0;
    }
  }
  hatSeekNode(handle,old);
  return 0;
}

int isDirectDescendantOf(HatFile handle,filepointer fileoffset,filepointer parent) {
  char nodeType;
  filepointer old = hatNodeNumber(handle);
  int debug=0;

  if (parent==0) return 0;
  
  if (getNodeType(handle,fileoffset)==HatApplication) {
    fileoffset = hatLMOName(handle,fileoffset);
    if (fileoffset!=0) {
      getNodeType(handle,fileoffset);
      fileoffset=getParent();
    }
  }
  if (fileoffset==parent) return 1;

  while (fileoffset!=0) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case HatHidden:
      fileoffset=getParent();
      break;
    case HatSATA:
    case HatSATB:
    case HatSATC:
      fileoffset=getProjValue();
      break;
    case HatProjection:
      fileoffset=getParent();
      break;
    case HatName:
      if (getNameType()==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      fileoffset = getParent();
      break;
    case HatApplication:{
      filepointer newoffs;
      newoffs = getAppFun();
      if (hatLMO(handle,fileoffset)==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      if (isTopLevel(handle,fileoffset)) {
	hatSeekNode(handle,old);
	return 0;
      }
      getNodeType(handle,newoffs);
      fileoffset = getParent();
      break;
    }
    default:
      hatSeekNode(handle,old);
      return 0;
    }
  }
  hatSeekNode(handle,old);
  return 0;
}

filepointer hatMainCAF(HatFile h) {
  filepointer currentOffset,satc,srcref;
  char nodeType;

  currentOffset = hatSeqFirst(h);
  while (!hatSeqEOF(h,currentOffset)) {
    nodeType = getNodeType(h,currentOffset);
    switch (nodeType) {
    case TRNAM: // Name
      if (getParent()==0) { // is parent 0?
	srcref = getSrcRef();
	satc = hatSeqNext(h,currentOffset);
	if ((srcref!=0)&&(isSAT(h,satc))) {  // SATC behind TRNAM?
	  // found a CAF!
	  //if (isTrusted(h,srcref)) printf("isTrusted\n");
	  //if (isTopLevel(h,currentOffset)==0) printf("main is not top-level!\n");
	  if ((isTrusted(h,srcref)==0)&&
	      (isTopLevel(h,currentOffset))) {
	    filepointer lmo = hatLMO(h,currentOffset);
	    if ((lmo!=0)&&(getNodeType(h,lmo)==NTIDENTIFIER)&&
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

  if (nextbyte()==TRAPP) {
    skipbyte(); // skip one byte in applications (arity)
  }
  fp = readpointer();
  boff = lbuf;
  return fp;
}

filepointer getNameType() {
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
  filepointer fp;

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
  if ((nodeType&240)==0) nodeType=nodeType & 247;
  if (nodeType==NTTOPIDENTIFIER) return NTIDENTIFIER;
  return (nodeType == MDTRUSTED ? HatModule : nodeType);
}

filepointer hatSeqNext(HatFile h,filepointer nodenumber) {
  hatSeekNode(h,nodenumber);
  skipNode(nextbyte());
  return foff + boff;
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

char _getInfix() {
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

BOOL getModuleTrusted() {
  return (seenextbyte()==MDTRUSTED);
}

unsigned long _getPosn() {
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

filepointer hatErrorPoint(HatFile h) {    // return the error entry point, if
                                          // evaluation failed, otherwise 0 returned
  filepointer p,old = hatNodeNumber(h);
  hatSeekNode(h,0);
  skipstring(); // skip version info string
  p = readpointer(); // get entry point for trace
  hatSeekNode(h,old);
  return p;
}

char* hatErrorText (HatFile h) {  // return the error message, otherwise NULL
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
  if (strcmp(version,"Hat v01")!=0) {
    fprintf(stderr,"ERROR: File is not a hat file or version is not supported.\nAborted.\n\n");
    return 0;
  } else {
    skipbytes(8);
    return 1;
  }
}

/* open file for reading, save in internal file descriptor */
HatFile hatOpenFile(char* name) {
  int f;
  name = hatFileExtension(name);
  f = open(name, 0);
  if (f!=-1) {
    int handle = hatNewHandle();
    stat(name, &(hatHandle[handle].statBuf));
    hatHandle[handle].filesize = hatHandle[handle].statBuf.st_size;
    hatHandle[handle].filename = name;
    hatHandle[handle].buf_n = 0; // set buffer pointers appropriately. buf_n=0 buffer currently empty
    hatHandle[handle].boff = 0;  // at position 0 in buffer
    hatHandle[handle].foff = 0;  // at position 0 in file
    hatHandle[handle].f = f;
    currentHandle = handle;
    _loadHandle(handle);
    if (hatTestHeader(handle)==0) {
      hatCloseFile(handle);
      return -2;
    }
    return handle;
  } else
    return -1;
}

/* close file in internal file descriptor */
void hatCloseFile(HatFile h) {
  //printf("Closing file %u\n",handle);
  close(hatHandle[h].f);
  free(hatHandle[h].buf);
  freeStr(hatHandle[h].filename);
  if (h==currentHandle) {
    currentHandle = -1;
  }
}

char* hatFileName(HatFile h) {
  return hatHandle[h].filename;
}
