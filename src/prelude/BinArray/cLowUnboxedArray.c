#include "cLowUnboxedArray.h"
#include <malloc.h>

void finaliseUBA (UBA uba) {
  free(uba->block);
  free(uba);
}
