#include "mk.h"

int sizePackedString(int length)
{
  int size = (length + sizeof(Node)) / sizeof(Node);
  return (1+EXTRA+size);
}


NodePtr allocPackedString(int length)
{
  NodePtr n;
  int size = (length + sizeof(Node)) / sizeof(Node);
  int extra = size * sizeof(Node) - (length+1);

  n = C_ALLOC(1+EXTRA+size);
  *n = CONSTRW(size,extra);
  INIT_PROFINFO(n,&dummyProfInfo)
  return n;
}

void copyPackedString(int length, NodePtr dst, char *src)
{
  if (src==(char*)0)
    memcpy((char*)&dst[1+EXTRA],"\0",1);
  else
    memcpy((char*)&dst[1+EXTRA],src,length+1);
}

NodePtr mkPackedString(int length, char *str)
{
  NodePtr n = allocPackedString(length);
  copyPackedString(length,n,str);
  return n;
}

char *getPackedString(NodePtr n)
{
  return (char*)&n[1+EXTRA];
}
