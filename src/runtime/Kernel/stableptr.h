#if 1
typedef int HaskellRef;

extern  HaskellRef     stableInsert (unsigned long*);
extern  unsigned long *stableRef (HaskellRef);
extern  void           stableRelease (HaskellRef);
#endif
