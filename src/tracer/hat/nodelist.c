/**************************************************************************/
/* nodelist.c                                                             */
/* simple list operations to manage sorted lists of fileoffsets           */
/*                                                                        */
/* Thorsten Brehm, 5/2001                                                 */
/**************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "Expressions.h"
#include "hatinterface.h"
#include "FunTable.h"
#include "nodelist.h"

/* return an empty list */
NodeList* newList() {
  return (NodeList*) calloc(1, sizeof(NodeList)); // sets both pointers to NULL!
}

/* append to lists end */
void appendToList(NodeList *nl,unsigned long foffset) {
  hNodePtr e = (hNodePtr) calloc(1, sizeof(NodeElement));
  if (e==NULL) {
    fprintf(stderr,"ERROR: No more space in heap!\n\n");
    exit(1);
  }
  e->fileoffset = foffset;
  if (nl->last==NULL) {
    nl->first = e;
    nl->last = e;
  } else {
    nl->last->next = e;
    nl->last = e;
  }
}

/* insert an element at lists beginning */
void addBeforeList(NodeList *nl,unsigned long foffset) {
  hNodePtr e = (hNodePtr) calloc(1,sizeof(NodeElement));
  if (e==NULL) {
    fprintf(stderr,"ERROR: No more space in heap!\n\n");
    exit(1);
  }
  e->fileoffset = foffset;
  e->next=nl->first;
  nl->first = e;
  if (nl->last==NULL) nl->last=e;
}

/* insert element appropriately within the list */
void insertInList(NodeList *nl,unsigned long foffset) {
  hNodePtr l,e = (hNodePtr) calloc(1,sizeof(NodeElement));
  if (e==NULL) {
    fprintf(stderr,"ERROR: No more space in heap!\n\n");
    exit(1);
  }
  e->fileoffset = foffset;
  l=nl->first;
  if ((l==NULL)||(l->fileoffset>=foffset)) {
    e->fileoffset=foffset;
    e->next = nl->first;
    nl->first = e;
    if (nl->last==NULL) nl->last = e;
    return;
  }
  while ((l->next!=NULL)&&(l->next->fileoffset<foffset)) l=l->next;
  e->next=l->next;
  l->next = e;
  if (e->next==NULL) nl->last=e;
}

/* check for element in list */
int isInList(NodeList *nl,unsigned long foffset) {
  hNodePtr e;
  if (nl->first==NULL) return 0; // list empty! => not in list!
  if ((foffset<nl->first->fileoffset)||(foffset>nl->last->fileoffset))
    return 0;  // foffset without range of stored values => not in list!
  e = nl->first;
  while ((e!=NULL)&&(e->fileoffset!=foffset)) e=e->next;
  return (e!=NULL);
}


/* show values in list */
void showList(NodeList *nl) {
  hNodePtr e;
  e = nl->first;
  if (e==NULL) printf("EMPTY\n"); else
    {
      while (e!=NULL) {
	printf("element: %u\n",e->fileoffset);
	e=e->next;
      }
    }
}

/* return the number of elements in the list */
unsigned long listLength(NodeList *nl) {
  hNodePtr e;
  unsigned long l = 0;
  e = nl->first;
  while (e!=NULL) {
    e=e->next;
    l++;
  }
  return l;
}

/* free the entire list */
void freeList(NodeList *nl) {
  hNodePtr e,f;
  e = nl->first;
  while (e!=NULL) {
    f=e;
    e=e->next;
    free(f);
  }
  nl->first=NULL;
  nl->last=NULL;
}

/* show pretty printing of all nodes (and their results) in the list */
void showPretty(HatFile handle,NodeList *nl,int verboseMode,unsigned int precision) {
  hNodePtr e;
  FunTable results = newFunTable();
  e = nl->first;

  if (e==NULL) printf("FUNCTION TABLE EMPTY\n"); else
    {
      unsigned long satc;
      while (e!=NULL) {
	satc=getResult(handle,e->fileoffset);  // find SATC for the application!
	if (isSAT(handle,satc)) {
	  ExprNode* r=buildExpr(handle,satc,verboseMode,precision);
	  ExprNode* a=buildExpr(handle,e->fileoffset,verboseMode,precision);
	  addToFunTable(results,a,r,e->fileoffset);
	}
	e=e->next;
      }
    }
  fflush(stderr);
  showFunTable(results);
  freeFunTable(results);
}
