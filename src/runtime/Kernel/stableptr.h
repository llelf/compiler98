#ifndef _STABLEPTR_H_
#define _STABLEPTR_H_

typedef void* StablePtr;

extern  StablePtr      makeStablePtr  (unsigned long*);
extern  unsigned long *derefStablePtr (StablePtr);
extern  void           freeStablePtr  (StablePtr);
extern  void           copyStablePtr  (StablePtr,StablePtr);

/* for backwards compatibility: */
typedef StablePtr HaskellRef;
#define stableRef(x)		derefStablePtr(x)
#define stableInsert(x)		makeStablePtr(x)
#define stableRelease(x)	freeStablePtr(x)
#define stableCopy(x,y)		copyStablePtr(x)

#endif
