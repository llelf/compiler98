/**************************************************************************/
/* hashtable.h: definition of a hashtable over long integer values        */
/* The hashtable only saves one key, no value can be associated           */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

typedef struct hashelement *HashPtr;

typedef struct hashelement {
  unsigned long value;
  HashPtr next; // Array of all arguments
} HashElement;

typedef struct {
  HashPtr* hashArray;
  unsigned long size;
  unsigned long count;
} HashTable;

HashTable*   newHashTable(unsigned long size);   /* return empty table of size */
void addToHashTable(HashTable* h,unsigned long value); /* add element to table */

/* check for element in hash table */
/* returns 1, if value is in the table */
/*         0 otherwise */
int isInHashTable(HashTable* h,unsigned long value); /* check for element */








