/**************************************************************************/
/* hashtable.h: definition of a hashtable over long integer values        */
/* The hashtable only saves one key, no value can be associated           */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/
#include <stdio.h>
#include "hashtable.h"

HashTable* newHashTable(unsigned long size) {
  HashTable* h = (HashTable*) malloc(1,sizeof(HashTable));
  h->hashArray = (HashElement**) calloc(size,sizeof(HashElement*));
  h->size = size;
  h->count = 0;
}

HashElement* newHashElement(unsigned long value) {
  HashElement* e = (HashElement*) malloc(1,sizeof(HashElement));
  e->value = value;
  e->next = NULL;
}

void addToHashTable(HashTable* h,unsigned long value) {
  unsigned long hash = value % h->size;
  HashElement* e = newHashElement(value);
  e->next = h->hashArray[hash];
  h->hashArray[hash]=e;
  h->count++;
}

//#define showHashStatistic
int isInHashTable(HashTable* h,unsigned long value) {
  unsigned long hash = value % h->size;
  HashElement *e = h->hashArray[hash];

#ifdef showHashStatistic
  int c=0;
  while ((e!=NULL)&&(e->value!=value)) {e=e->next;c++;}
  if (c>5) printf("needed: %i, size: %i\n",c,h->count);
#else
  while ((e!=NULL)&&(e->value!=value)) e=e->next;
#endif
  return (e!=NULL);
}








