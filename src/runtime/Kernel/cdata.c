#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "mk.h"
/* #include "runtime.h"	-- now included from cinterface.h below */
#include "stableptr.h"	/* MW 19991213, needed for Haskell finalizers */
#include "cinterface.h"	/* MW 19991213, needed for Haskell finalizers */
extern void deferGC (StablePtr finalise);	/* prototype (ditto) */

#define MAX_FOREIGNOBJ 1024

static ForeignObj foreign[MAX_FOREIGNOBJ];

/*static FileDesc fd_stdin;*/
/*static FileDesc fd_stdout;*/
/*static FileDesc fd_stderr;*/
ForeignObj fo_stdin;
ForeignObj fo_stdout;
ForeignObj fo_stderr;

/* The following are functions *not* visible to the Haskell world. */

void initForeignObjs(void)
{
  int i;
  FileDesc *fd_stdin, *fd_stdout, *fd_stderr;

  for(i=0; i<MAX_FOREIGNOBJ; i++) {
    foreign[i].used = 0;
    foreign[i].cval = NULL;
    foreign[i].gc   = NULL;
    foreign[i].gcf  = NULL;
  }
  fd_stdin = (FileDesc*)malloc(sizeof(FileDesc));
  fd_stdin->fp = stdin; 
  fd_stdin->bm = _IOLBF; 
  fd_stdin->size = -1; 
  fd_stdin->path = strdup("<stdin>"); 
    fo_stdin.used = 1; 
    fo_stdin.cval = (void*)fd_stdin; 
    fo_stdin.gcf  = gcNone;
  fd_stdout = (FileDesc*)malloc(sizeof(FileDesc));
  fd_stdout->fp = stdout;
  fd_stdout->bm = _IOLBF;
  fd_stdout->size = -1;
  fd_stdout->path = strdup("<stdout>"); 
    fo_stdout.used = 1; 
    fo_stdout.cval = (void*)fd_stdout;
    fo_stdout.gcf  = gcNone;
  fd_stderr = (FileDesc*)malloc(sizeof(FileDesc));
  fd_stderr->fp = stderr;
  fd_stderr->bm = _IOLBF;
  fd_stderr->size = -1;
  fd_stderr->path = strdup("<stderr>"); 
    fo_stderr.used = 1; 
    fo_stderr.cval = (void*)fd_stderr;
    fo_stderr.gcf  = gcNone;
}

ForeignObj* allocForeignObj(void* arg, gcCval finalCV, gcFO finalFO)
{
  int i;
  for(i=0; i<MAX_FOREIGNOBJ; i++) {
    if(!foreign[i].used) {
      foreign[i].used = 1;
      foreign[i].cval = arg;
      foreign[i].gc   = finalCV;
      foreign[i].gcf  = finalFO;
      /*printf("allocForeignObj: allocated %d (gcCval %x, gcFO %x)\n",i,finalCV,finalFO);*/
      return &foreign[i];
    }
  }
  fprintf(stderr,"Warning: allocation limit exceeded for ForeignObj\n");
  return 0;
}

void freeForeignObj(ForeignObj *cd)
{
  /*printf("freeForeignObj: releasing %d\n",((int)cd-(int)foreign)/sizeof(ForeignObj));*/
  if (cd->gcf)
    cd->gcf(cd);
  else
    fprintf(stderr,"Warning: freeForeignObj called on already-free ForeignObj");
  cd->used = 0;
  cd->gcf  = NULL;
}

void *derefForeignObj(ForeignObj *cd)
{
  return cd->cval;
}

void clearForeignObjs(void)
{
  int i;
  for(i=0; i<MAX_FOREIGNOBJ; i++)
    foreign[i].used = 0;
}

void markForeignObj(ForeignObj *cd)
{
  cd->used++;
}

void gcForeignObjs(void)
{
  int i;
  for(i=0; i<MAX_FOREIGNOBJ; i++)
    /*if(foreign[i].used == 0) {
        printf("gcForeignObjs: could reclaim %d (gcFO %x)\n",i,foreign[i].gcf);
      } */
    if(foreign[i].used == 0 && foreign[i].gcf) {
    /*printf("gcForeignObjs: reclaiming %d\n",i);*/
      foreign[i].gcf(&foreign[i]);  /* Call first-stage garbage collector */
      foreign[i].gcf  = NULL;
    } 
}

void gcNow(ForeignObj *cd)	/* This is a possible first-stage GC */
{
  if (cd->gc)
    cd->gc(cd->cval);		/* Call the second-stage garbage collector */
  cd->cval = NULL;		/* and ensure we don't keep dead values */
  cd->gc   = NULL;
}
void gcLater(ForeignObj *cd)	/* This is another possible first-stage GC */
{
  if (cd->gc)
    deferGC(cd->gc);		/* Call the second-stage garbage collector */
  cd->cval = NULL;		/* and ensure we don't keep dead values */
  cd->gc  = NULL;
}
void gcNone(ForeignObj *cd)	/* This is another possible first-stage GC */
{
  cd->cval = NULL;		/* Just ensure we don't keep dead values */
  cd->gc   = NULL;
}

void gcFile(void *c)	/* This is a possible second-stage GC */
{
  FileDesc *a = (FileDesc*)c;
#ifdef PROFILE
  if(!replay)
#endif
    fclose(a->fp);
  /*if (a->path) free(a->path);*/
  /*free(a);	-- free'ing causes a seg-fault! don't know why */
}
void gcSocket(void *c)	/* This is another possible second-stage GC */
{
  FileDesc *a = (FileDesc*)c;
  close(a->fdesc);
  /*if (a->path) free(a->path);*/
  /*free(a);	-- free'ing causes a seg-fault! don't know why */
}


/* ********************* */
/* ForeignObj/Addr stuff */
/* ********************* */

#if 0
/* The following function *is* visible to the Haskell world. */
/* makeForeignObj primitive 2 :: Addr -> () -> IO ForeignObj */
/* -- Note, we assume that the finaliser already has `unsafePerformIO' */
/*    wrapped around it, so its type is really (). */
C_HEADER(primForeignObj)
{
  NodePtr nodeptr, finalise;
  void *addr;
  ForeignObj *fo;
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  addr = (void*)GET_INT_VALUE(nodeptr);
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  finalise = nodeptr;

  fo = allocForeignObj(addr, (gcCval)mkStablePtr(finalise), gcLater);
  nodeptr = (NodePtr)mkRight(mkCInt((Int)fo));
  C_RETURN(nodeptr);
}
#endif

/* The following FFI function *is* visible to the Haskell world.         */
/* -- Note, we assume that the finaliser already has `unsafePerformIO'   */
/*      wrapped around it, so its type is really (), not even IO ().     */
/*      Furthermore, it is wrapped in a box so that the primitive call   */
/*      (which evaluates all args to WHNF) does not execute it straight  */
/*      away!                                                            */
/* -- Note also that, normally one *cannot* return a ForeignObj complete */
/*      to the Haskell world.  nhc98 does allow it, but this is the only */
/*      occasion where it actually makes sense.                          */

/* foreign import makeForeignObjC :: Addr -> _E a -> IO ForeignObj       */
void *primForeignObjC (void *addr, NodePtr fbox)
{
  ForeignObj *fo;
  NodePtr finalise;
  finalise = GET_POINTER_ARG1(fbox,1);
  fo = allocForeignObj(addr, (gcCval)makeStablePtr(finalise), gcLater);
  return mkCInt((int)fo);
}

/* 'addrToHandle' is very tricky!  The Addr *must* be a pointer to   */
/* a ForeignObj that has already been allocated in C-land.  This     */
/* function just changes the types around for Haskell-land.          */
void *
addrToHandle (void* addr)
{
  return mkCInt((int)addr);
}

/* The following function is also visible to the Haskell world.       */
/* It _must_ be a primitive, not a foreign import, because the latter */
/* mechanism would dereference the ForeignObj argument to become just */
/* an Addr, which is the opposite of what we want here.               */

/* reallyFreeForeignObj primitive 1 :: ForeignObj -> IO ()            */
C_HEADER(reallyFreeForeignObj)
{
  NodePtr nodeptr;
  ForeignObj *fo;
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  fo = (void*)GET_INT_VALUE(nodeptr);

  /*printf("reallyFreeForeignObj: releasing %d\n",((int)fo-(int)foreign)/sizeof(ForeignObj));*/
  freeForeignObj(fo);
  C_RETURN(mkUnit());
}

static StablePtr pending[MAX_FOREIGNOBJ];  /* queue for pending finalisers */
static int       pendingIdx=0;

void deferGC (StablePtr finalise)
{
  if (++pendingIdx >= MAX_FOREIGNOBJ) {
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
  NodePtr *SP=Sp;		/*                   stack pointer */
  NodePtr *FP=Fp;		/*                   frame pointer */

  static int alreadyRunning=0;	/* need lock in case a finaliser triggers GC! */

  if (alreadyRunning) {
    fprintf(stderr,"Warning: running ForeignObj finalisers has triggered another GC!\n");
    return;
  } else alreadyRunning=1;	/* grab mutex lock before entering */

  fprintf(stderr,"runDeferredGCs: %d finalisers to process\n",pendingIdx);

  for (i=1; i<=pendingIdx; i++) {/* traverse the queue */
    n = derefStablePtr(pending[i]);
    C_PUSH(n);
    C_EVALTOS(n); 		/* .. run each finaliser, discarding result */
    C_POP();
    freeStablePtr(pending[i]);	/* .. then permit GC of finaliser itself */
    pending[i] = NULL;
  }
  Ip=IP;			/* restore global instruction pointer */
  Sp=SP;			/*                      stack pointer */
  Fp=FP;			/*                      frame pointer */
  pendingIdx = 0;		/* finally, reset the queue */
  alreadyRunning = 0;		/* and release the mutex lock */
}

