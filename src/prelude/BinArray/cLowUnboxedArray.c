#include "cLowUnboxedArray.h"
#include "localmalloc.h"

void finaliseUBA (UBA uba) {
  free(uba->block);
  free(uba);
}
