/* mkForeign() added by MW */

#include "mk.h"

NodePtr mkForeign (void *x, gcCval final)
{
    ForeignObj *fo;
    fo = allocForeignObj(x,final,gcNow);
    return mkCInt((Int)fo);
}

