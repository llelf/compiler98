#include "newmacros.h"

#ifdef PROFILE
unsigned prof_Unimplemented[] = {0};
#define STUB(x) \
unsigned x[] = { \
  CONSTRW(1,0) \
, useLabel(prof_Unimplemented) \
, 4, 2, 3, 1 \
};
#else
#define STUB(x) \
unsigned x[] = { \
  CONSTRW(1,0) \
, 1 \
};
#endif

STUB(PM_IO)
STUB(PM_System)
STUB(PM_Text)
STUB(PM_Char)

STUB(CF_Prelude_46Show_46Prelude_46IOError)
STUB(PM__95Driver)
STUB(FN_DbgIface_46fatal)
STUB(CF_DbgIface_46blackhole)

