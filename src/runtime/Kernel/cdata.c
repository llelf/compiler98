#include <stdlib.h>
#include <stdio.h>

/* #include "runtime.h"	-- now included from cinterface.h below */
#include "stableptr.h"	/* MW 19991213, needed for Haskell finalizers */
#include "cinterface.h"	/* MW 19991213, needed for Haskell finalizers */
extern void deferGC (StablePtr finalise);	/* prototype (ditto) */

#define MAX_CDATA 1024

static CData cdata[MAX_CDATA];

CData cdata_stdin;
CData cdata_stdout;
CData cdata_stderr;

void initCData(void)
{
  int i;
  for(i=0; i<MAX_CDATA; i++) {
    cdata[i].used = 0;
    cdata[i].arg.gc = NULL;
  }
  cdata_stdin.used = 1; 
  cdata_stdin.arg.fp = stdin; 
  cdata_stdin.arg.bm = _IOLBF; 
  cdata_stdin.arg.size = -1; 
  cdata_stdin.arg.gc = gcNone;
  cdata_stdout.used = 1; 
  cdata_stdout.arg.fp = stdout;
  cdata_stdout.arg.bm = _IOLBF;
  cdata_stdout.arg.size = -1;
  cdata_stdout.arg.gc = gcNone;
  cdata_stderr.used = 1; 
  cdata_stderr.arg.fp = stderr;
  cdata_stderr.arg.bm = _IOLBF;
  cdata_stderr.arg.size = -1;
  cdata_stderr.arg.gc = gcNone;
}

CData* allocCData(Arg arg)
{
  int i;
  for(i=0; i<MAX_CDATA; i++) {
    if(!cdata[i].used) {
      cdata[i].used = 1;
      cdata[i].arg = arg;
      return &cdata[i];
    }
  }
  fprintf(stderr,"Warning: allocation limit exceeded for ForeignObj\n");
  return 0;
}

void freeCData(CData *cd)
{
  cd->arg.gc(cd);
  cd->used = 0;
}

Arg *cdataArg(CData *cd)
{
  return &cd->arg;
}

void clearCData(void)
{
  int i;
  for(i=0; i<MAX_CDATA; i++)
    cdata[i].used = 0;
}

void markCData(CData *cd)
{
  cd->used++;
}

void gcCData(void)
{
  int i;
  for(i=0; i<MAX_CDATA; i++)
    if(cdata[i].used == 0 && cdata[i].arg.gc) {
      cdata[i].arg.gc(&cdata[i]);  /* Call first-stage garbage collector */
      cdata[i].arg.gc = NULL;
    } 
}

void gcFile(CData *cd)		/* This is a possible first-stage GC */
{
  Arg *a = cdataArg(cd);
  fclose(a->fp);
}

void gcSocket(CData *cd)	/* This is another possible first-stage GC */
{
  Arg *a = cdataArg(cd);
  close(a->fdesc);
}

void gcCVal(CData *cd)		/* This is another possible first-stage GC */
{				/* (added by MW) */
  Arg *a = cdataArg(cd);
  if (a->gcc)
    a->gcc(a->cval);		/* Call the second-stage garbage collector */
  a->cval = NULL;		/* and ensure we don't keep dead values */
  a->gcc  = NULL;
}

void gcHVal(CData *cd)		/* This is another possible first-stage GC */
{				/* (added by MW) */
  Arg *a = cdataArg(cd);
  if (a->gcc)
    deferGC(a->gcc);		/* Call the second-stage garbage collector */
  a->cval = NULL;		/* and ensure we don't keep dead values */
  a->gcc  = NULL;
}


void gcNone(CData *cd)		/* This is another possible first-stage GC */
{				/* Altered by MW */
  Arg *a = cdataArg(cd);
  a->cval = NULL;		/* Just ensure we don't keep dead values */
  a->gcc  = NULL;
}


/* ********************* */
/* ForeignObj/Addr stuff */
/* ********************* */

CData *buildForeignObj (void *addr, NodePtr finalise)
{
  Arg a;
  a.cval = (void*)addr;
  a.gc   = &gcHVal;
  a.gcc  = (gccval)mkStablePtr(finalise);
  return allocCData(a);
}

/* makeForeignObj primitive 2 :: Addr -> () -> IO ForeignObj */
/* -- Note, we assume that the finaliser already has `unsafePerformIO' */
/*    wrapped around it, so its type is really (). */
C_HEADER(primForeignObj)
{
  NodePtr nodeptr, finalise;
  void *addr;
  CData *fo;
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  addr = (void*)GET_INT_VALUE(nodeptr);
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  finalise = nodeptr;

  fo = buildForeignObj(addr,finalise);
  nodeptr = mkRight(mkCInt((Int)fo));
  C_RETURN(nodeptr);
}

static StablePtr pending[MAX_CDATA];	/* queue for pending finalisers */
static int       pendingIdx=0;

void deferGC (StablePtr finalise)
{
  if (++pendingIdx >= MAX_CDATA) {
    fprintf(stderr,"Warning: mismatch in limits for ForeignObjs and finalisers.\n");
    exit(1);
  }
  pending[pendingIdx] = finalise;	/* put finaliser in the queue	*/
}

void runDeferredGCs (void)
{
  int i;
  NodePtr n;
  CodePtr IP=Ip;		/* save global instruction pointer */

  for (i=1; i<=pendingIdx; i++) {/* traverse the queue */
    n = stableRef(pending[i]);
    C_PUSH(n);
    C_EVALTOS(n); 		/* .. run each finaliser, discarding result */
    C_POP();
    stableRelease(pending[i]);	/* .. then permit GC of finaliser itself */
    pending[i] = NULL;
  }
  Ip=IP;			/* restore global instruction pointer */
  pendingIdx = 0;		/* finally, reset the queue */
}


#if 0
/* *********************************************** */
/* ForeignObj stuff that was removed from FFI spec */
/* *********************************************** */
void  noGC      (void *ptr)  { return; }
long  addrToInt (void *addr) { return (long)addr; }
void *intToAddr (long i)     { return (void*)i; }
void *hs_coerce (void *v)    { return v; }

/* foreign import makeForeignObj           :: Addr -> Addr -> IO ForeignObj */
CData *makeForeignObj (void *addr, void *finalise)
{
  Arg a;
  a.cval = (void*)addr;
  a.gc   = &gcCVal;
  a.gcc  = (gccval)finalise;
  return allocCData(a);
}

/* foreign import addrToForeignObj         :: Addr ->         IO ForeignObj */
CData *addrToForeignObj (void *addr)
{
  Arg a;
  a.cval = (void*)addr;
  a.gc   = &gcNone;
  return allocCData(a);
}

/* foreign import addForeignFinalizer      :: ForeignObj -> Addr -> IO () */
void addForeignFinalizer (CData *cd, void *finalise)
{
  Arg *a = cdataArg(cd);
  a->gc  = &gcCVal;
  a->gcc = (gccval)finalise;
}

/* foreign import foreignObjToAddr         :: ForeignObj   -> IO Addr */
void *foreignObjToAddr (CData *cd)
{
  Arg *a = cdataArg(cd);
  return a->cval;
}

/* foreign import writeForeignAddr          :: ForeignObj -> Addr -> IO () */
void writeForeignAddr (CData *cd, void *addr)
{
  Arg *a = cdataArg(cd);
  a->cval = addr;
}

#endif
