/**************************************************************************/
/* hashtable.h: definition of a hashtable over long integer values        */
/* The hashtable only saves one key, no value can be associated           */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/
#include "hatinterface.h"

typedef struct hashelement *HashPtr;

typedef struct hashelement {
  filepointer value;
  HashPtr next; // Array of all arguments
} HashElement;

typedef struct {
  HashPtr* hashArray;
  unsigned long size;
  unsigned long rehashAt;
  unsigned long incrementSize;
  unsigned long count;
  int outOfMemory;
} HashTable;

HashTable*   newHashTable(unsigned long size);   /* return empty table of size */
void         freeHashTable(HashTable* h);
void         addToHashTable(HashTable* h,filepointer value); /* add element to table */
void         removeFromHashTable(HashTable* h,filepointer value);

/* check for element in hash table */
/* returns 1, if value is in the table */
/*         0 otherwise */
int          isInHashTable(HashTable* h,filepointer value); /* check for element */








