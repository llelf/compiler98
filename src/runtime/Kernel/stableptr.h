#ifndef _STABLEPTR_H_
#define _STABLEPTR_H_

typedef void* StablePtr;

extern  StablePtr      stableInsert (unsigned long*);
extern  unsigned long *stableRef (StablePtr);
extern  void           stableRelease (StablePtr);
extern  void           stableCopy (StablePtr,StablePtr);

/* for backward compatibility */
typedef StablePtr HaskellRef;

#endif
