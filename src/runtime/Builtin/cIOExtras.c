/* basic unsafe utilities, defined in IOExtras */
#include "cinterface.h"

void   performGC         ()                     { C_GC(0); }
void*  unsafePtrEq       (void* a, void* b)     { return mkBool(a==b); }

