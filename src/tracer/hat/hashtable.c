/**************************************************************************/
/* hashtable.h: definition of a hashtable over long integer values        */
/* The hashtable only saves one key, no value can be associated           */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "hashtable.h"

HashTable* newHashTable(unsigned long size) {
  HashTable* h = (HashTable*) malloc(sizeof(HashTable));
  h->hashArray = (HashElement**) calloc(size,sizeof(HashElement*));
  h->size = size;
  h->count = 0;
  return h;
}

void freeHashTable(HashTable* h) {
  free(h->hashArray);
  free(h);
}

HashElement* newHashElement(unsigned long value) {
  HashElement* e = (HashElement*) malloc(sizeof(HashElement));
  e->value = value;
  e->next = NULL;
}

void freeHashElement(HashElement* e) {
  free(e);
}

void addToHashTable(HashTable* h,unsigned long value) {
  unsigned long hash = value % h->size;
  HashElement* e = newHashElement(value);
  e->next = h->hashArray[hash];
  h->hashArray[hash]=e;
  h->count++;
}

void removeFromHashTable(HashTable* h,unsigned long value) {
  unsigned long hash = value % h->size;
  HashElement* e = h->hashArray[hash];
  HashElement* l = NULL;
  while (e!=NULL) {
    if (e->value==value) {
      if (l==NULL) {
	h->hashArray[hash]=e->next;
	e->next = NULL;
	freeHashElement(e);
	e = h->hashArray[hash];
      } else {
	l->next=e->next;
	e->next = NULL;
	freeHashElement(e);
	e=l->next;
      }
    } else {
      l=e;
      e=e->next;
    }
  }
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








