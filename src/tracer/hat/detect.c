/**************************************************************************/
/* detect.c: module for algorithmic debugging of traces                   */
/*                                                                        */
/* Thorsten Brehm, 5/2001                                                 */
/**************************************************************************/

#include <stdio.h>
#include <string.h>
#include "Expressions.h"
#include "hatinterface.h"
#include "FunTable.h"
#include "nodelist.h"
#include "hashtable.h"

#define HASH_TABLE_SIZE 3000
/*
typedef struct {
  HashTable* htable;
  filepointer parent;
  filepointer currentOffset;
  int found;
  int finished;
  HatFile handle;
} _EDTQuery;


EDTQuery newObserveQuery(HatFile handle,
			 filepointer parent) {
  _EDTQuery* newQ = (_EDTQuery*) calloc(1,sizeof(_EDTQuery));
  hatSwitchToHandle(handle);
  newQ->handle = handle;
  newQ->htable = newHashTable(80000);
  newQ->parent = parent;
  newQ->currentOffset = identifierNode;
  if (topIdentifierNode>identifierNode) newQ->currentOffset = topIdentifierNode;
  newQ->fsz = hatFileSize()/1000;
  if (newQ->fsz==0) newQ->fsz = 1;
  if (newQ->fsz<20000) newQ->lsz=200;
  return ((EDTQuery) newQ);
}

void freeEDTQuery(EDTQuery query) {
  freeHashTable(((_EDTQuery*) query)->htable);
  ((_EDTQuery*) query)->htable = NULL;
  free(query);
}

filepointer nextEDTQueryNode(EDTQuery query) {
  unsigned long p,currentOffset;
  char nodeType;
  int arity;
  HashTable* htable = ((_EDTQuery*) query)->htable;
  unsigned long identifierNode = ((_EDTQuery*) query)->identifierNode;
  unsigned long topIdentifierNode = ((_EDTQuery*) query)-> topIdentifierNode;
  int recursiveMode = ((_EDTQuery*) query)->recursiveMode;

*/

// checks, whether nodenumber is a child of parent. It is a child of parent,
// if the nodenumber's parentTrace equals parent or if its parent is a non-toplevel
// node which in turn is a child of parent (or so on recursively)
int isChildOf(HatFile handle,filepointer nodenumber,filepointer parent) {
  char nodeType;
  filepointer old = hatNodeNumber(handle);

  if (parent==0) return 0;
  while (nodenumber!=0) {
    nodeType=getNodeType(handle,nodenumber);
    switch(nodeType) {
    case TRHIDDEN:
    case TRSATA:
    case TRSATB:
    case TRSATC:
    case TRSATCIS:
    case TRSATBIS:
    case TRSATAIS:
      nodenumber=getParent();
      if (nodenumber==parent) {
	return 1;
      }
      break;
    case TRNAM:
    case TRAPP:{
      nodenumber = getParent();
      if (nodenumber==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      if (isTopLevel(handle,nodenumber)) {
	hatSeekNode(handle,old);
	return 0;
      }
      break;
    }
    default:
      hatSeekNode(handle,old);
      return 0;
    }
  }
  hatSeekNode(handle,old);
  return 0;
}

//#define DebuggetChildrenFor
void getChildrenFor(HatFile handle,
		    NodeList* nl,unsigned long parentTrace,unsigned long current,
		    HashTable* hash) {
  char nodeType;
  unsigned long satc=0,result,orig_current=current;
  int question_old=0;
  {
    if (isInHashTable(hash,current)) return;
    if (current==0) return;
    addToHashTable(hash,current);

    result=getResult(handle,current);

    nodeType = getNodeType(handle,current);
#ifdef DebuggetChildrenFor
    printf("nodeType at %u is %i, searching %u, resulting %u\n",current,nodeType,
	   parentTrace,result);
#endif
    switch (nodeType) {
    case TRAPP:
      {
	unsigned long srcref,p,funTrace,appTrace;
	int arity,isChild;
	arity     = getAppArity();
	appTrace  = getParent();            // fileoffset of App-trace
	funTrace  = getAppFun();            // function-trace
	srcref    = getSrcRef();            // get srcref
	appTrace  = hatFollowHidden(handle,
				    appTrace); // follow along hidden to find parent
	
	isChild   = isChildOf(handle,current,parentTrace);
	if ((appTrace==parentTrace)&&(isChild==0)) {
	  printf("That's odd: %u %u %u\n",current,parentTrace,appTrace);
	  printf("AppTrace: %u\n",getParent());
	  printf("AppTrace: %u\n",getParent());
	  //isChild = 1;
	}

	if (isChild==0) { //(appTrace!=parentTrace) { // if it's not a child itself
	  getChildrenFor(handle,nl,parentTrace,appTrace,hash);
	}
	if (isChild) { //(appTrace==parentTrace) {
	  int i=0;
	  while (i++<arity) {
#ifdef DebuggetChildrenFor
	    printf("checking arg %i of %u\n",i,current);
#endif
	    hatSeekNode(handle,current);
	    p = getAppArgument(i-1);
	    getChildrenFor(handle,nl,parentTrace,p,hash);
	  }
	}
	
	if ((isChild)||(appTrace==0)) {
	  //((appTrace==parentTrace)||(appTrace==0)) {
#ifdef DebuggetChildrenFor
	  printf("APP at %u is child!\n",current);
#endif
	  satc=hatFollowSATs(handle,result);
	  { 
	    int trusted = isTrusted(handle,funTrace);
	    int toplevel = isTopLevel(handle,funTrace);
	    int isOk=1;
	    if ((trusted==0)&&(toplevel)) {
#ifdef DebuggetChildrenFor
	      printf("Function at %u is not trusted.\n",funTrace);
	      printf("Toplevel: %i\n",toplevel);
#endif
	      if ((satc!=current)&&(!isInList(nl,current))) {
		appendToList(nl,current);
	      }
	    } else {
	      if (satc!=current)
		getChildrenFor(handle,nl,current,satc,hash);
	    }
	  }
	}
      }
      removeFromHashTable(hash,orig_current);
      return;
    case TRNAM: {
      unsigned long p = getParent();
      unsigned long srcref = getSrcRef();
      unsigned long newcurrent;

      if ((p==parentTrace)||(p==0)) {
	if (p==0) {  // CAF found
	  unsigned long lmo = hatLMO(handle,current);
	  if (lmo!=0) {
	    if (isTopLevel(handle,current)&&(isTrusted(handle,srcref)==0)&&
		(!isInList(nl,current))) 
	      appendToList(nl,current);
	  }
	}
	removeFromHashTable(hash,orig_current);
	return;
      }
      else {
        newcurrent = hatFollowSATs(handle,p);
	if (newcurrent==current) {
	  removeFromHashTable(hash,orig_current);
	  return;
	}
	else current = newcurrent;
      }
    }
    getChildrenFor(handle,nl,parentTrace,current,hash);
    removeFromHashTable(hash,orig_current);
    return;
    case TRIND:
      current = getParent();
      getChildrenFor(handle,nl,parentTrace,current,hash);
      removeFromHashTable(hash,orig_current);
      return;
    case TRHIDDEN:
    case TRSATA: // not evaluated expression
    case TRSATAIS:
    case TRSATB:
    case TRSATBIS:
      current = getParent();
      getChildrenFor(handle,nl,parentTrace,current,hash);
      removeFromHashTable(hash,orig_current);
      return;
    default: {
	unsigned long newcurrent = hatFollowSATs(handle,current);
	if (newcurrent==current) {
	  removeFromHashTable(hash,orig_current);
	  return;
	}
	else current = newcurrent;
      }
      getChildrenFor(handle,nl,parentTrace,current,hash);
      removeFromHashTable(hash,orig_current);
      return;
    }
  }
  removeFromHashTable(hash,orig_current);
  return;
}

int getEDTchildren(HatFile handle,filepointer parentTrace,int **childrenArray) {
  unsigned long current = getResult(handle,parentTrace);
  HashTable* hash = newHashTable(HASH_TABLE_SIZE);
  NodeList* results=newList();
  int l;

  getChildrenFor(handle,results,parentTrace,current,hash);
  l = listLength(results);
  //printf("New!\n");
  {
    int i=0;
    NodeElement *e = results->first;
    *childrenArray = (int*) calloc(l+1,sizeof(long));
    while (i<l) {
      (*childrenArray)[i]=e->fileoffset;
      i++;
      e=e->next;
    }
    (*childrenArray)[i]=(-1);
    //printf("detected %i\n",i);
  }
  freeList(results);
  freeHashTable(hash);
  return l;
}

void freeArray(int *array) {
  //printf("Freeing %u...\n",array);
  if (array!=NULL) free(array);
  array = NULL;
  //printf("Free!\n");
}
