/* mkForeign() added by MW */

#include "mk.h"

NodePtr mkForeign (void *x, gccval f)
{
    Arg a;
    CData *cdata;
    a.cval = x;
    a.size = -1;
    a.gc   = gcCVal;
    a.gcc  = f;
    cdata  = allocCData(a);
    return mkCInt((Int)cdata);
}

