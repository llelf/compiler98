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
  HashTable* h = (HashTable*) calloc(1,sizeof(HashTable));
  if (h!=NULL) {
    h->hashArray = (HashElement**) calloc(size,sizeof(HashElement*));
    if (h->hashArray!=NULL) {
      h->size = size;
      h->rehashAt = h->size-(h->size/10); // rehash at 90%
      h->incrementSize = size;
      h->count = 0;
      h->outOfMemory=0;
    } else {
      free(h);
      h=NULL;
    }
  }
  return h;
}

void freeHashTable(HashTable* h) {
  free(h->hashArray);
  free(h);
}

HashElement* newHashElement(filepointer value) {
  HashElement* e = (HashElement*) malloc(sizeof(HashElement));
  e->value = value;
  e->next = NULL;
  return e;
}

void freeHashElement(HashElement* e) {
  free(e);
}

void rehash(HashTable* h) {
  if (h->outOfMemory==0) {
    HashTable* h2 = newHashTable(h->size+h->incrementSize);
    HashElement *e,*l;
    unsigned long sz,hash,i=0;
    
    if (h2==NULL) {
      h->outOfMemory=1;
    } else {
      sz=h2->size;
      h2->incrementSize=h->incrementSize; // make increment of new table same as old
      while (i<h->size) {
	l = h->hashArray[i];   // get list from old hash table
	h->hashArray[i]=NULL;
	while (l!=NULL) {      // process every entry from old list 
	  e = l;               // process first list element
	  l = l->next;         // get the tail of the list
	  hash = ((unsigned long) e->value) % sz; // hash value for entry to be processed
	  e->next = h2->hashArray[hash]; // append entries to new list
	  h2->hashArray[hash]=e; // set list in new hash table
	}
	i++;
      }
      // swap old and new hash table (to keep pointers to old table valid!)
      l=(HashElement*) h->hashArray;
      h->hashArray = h2->hashArray; // move new hashArray to old hashTable
      h2->hashArray= (HashElement**)l; // set temporary hashTable to old hashArray
      h2->size = h->size;           // set temporary hashTable size to old size
      h->size = sz;                 // set hash table size to new size
      h->rehashAt = h->size-(h->size/10); // set new rehash size: 90%
      freeHashTable(h2); // get rid of temporary table, now holding old hashArray
    }
  }
}

void addToHashTable(HashTable* h,filepointer value) {
  unsigned long hash = ((unsigned long) value) % h->size;
  HashElement* e = newHashElement(value);
  e->next = h->hashArray[hash];
  h->hashArray[hash]=e;
  if (h->count++>h->rehashAt) rehash(h);
}

void removeFromHashTable(HashTable* h,filepointer value) {
  unsigned long hash = ((unsigned long) value) % h->size;
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
int isInHashTable(HashTable* h,filepointer value) {
  unsigned long hash = ((unsigned long) value) % h->size;
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








