#include <stdio.h>


void *alloca (unsigned size)
{
  fprintf(stderr,"Falilure alloca %d called:-(\n",size);
  abort(-1);
}
