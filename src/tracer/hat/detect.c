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
int isChildOf(unsigned long nodenumber,unsigned long parent) {
  char nodeType;
  unsigned long old = hatNodeNumber();

  if (parent==0) return 0;
  while (nodenumber!=0) {
    hatSeekNode(nodenumber);
    nodeType=getNodeType();
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
	hatSeekNode(old);
	return 1;
      }
      if (isTopLevel(nodenumber)) {
	hatSeekNode(old);
	return 0;
      }
      break;
    }
    default:
      hatSeekNode(old);
      return 0;
    }
  }
  hatSeekNode(old);
  return 0;
}

//#define DebuggetChildrenFor
void getChildrenFor(NodeList* nl,unsigned long parentTrace,unsigned long current,
		    HashTable* hash) {
  char nodeType;
  unsigned long satc=0,result,orig_current=current;
  int question_old=0;
  {
    if (isInHashTable(hash,current)) return;
    if (current==0) return;
    addToHashTable(hash,current);

    result=getResult(current);

    hatSeekNode(current);
    nodeType = getNodeType();
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
	appTrace  = getParent();             // fileoffset of App-trace
	funTrace  = getFunTrace();          // function-trace
	srcref    = getSrcRef();            // get srcref
	appTrace  = followHidden(appTrace); // follow along hidden to find parent
	
	isChild   = isChildOf(current,parentTrace);
	if ((appTrace==parentTrace)&&(isChild==0)) {
	  printf("That's odd: %u %u %u\n",current,parentTrace,appTrace);
	  printf("AppTrace: %u\n",getParent());
	  printf("AppTrace: %u\n",getParent());
	  //isChild = 1;
	}

	if (isChild==0) { //(appTrace!=parentTrace) { // if it's not a child itself
	  getChildrenFor(nl,parentTrace,appTrace,hash);
	}
	if (isChild) { //(appTrace==parentTrace) {
	  int i=0;
	  while (i++<arity) {
#ifdef DebuggetChildrenFor
	    printf("checking arg %i of %u\n",i,current);
#endif
	    hatSeekNode(current);
	    p = getAppArgument(i-1);
	    getChildrenFor(nl,parentTrace,p,hash);
	  }
	}
	
	if ((isChild)||(appTrace==0)) {
	  //((appTrace==parentTrace)||(appTrace==0)) {
#ifdef DebuggetChildrenFor
	  printf("APP at %u is child!\n",current);
#endif
	  satc=followSATs(result);
	  { 
	    int trusted = isTrusted(funTrace);
	    int toplevel = isTopLevel(funTrace);
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
		getChildrenFor(nl,current,satc,hash);
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
	  unsigned long lmo = leftmostOutermost(current);
	  if (lmo!=0) {
	    if (isTopLevel(current)&&(isTrusted(srcref)==0)&&(!isInList(nl,current))) 
	      appendToList(nl,current);
	  }
	}
	removeFromHashTable(hash,orig_current);
	return;
      }
      else {
        newcurrent = followSATs(p);
	if (newcurrent==current) {
	  removeFromHashTable(hash,orig_current);
	  return;
	}
	else current = newcurrent;
      }
    }
    getChildrenFor(nl,parentTrace,current,hash);
    removeFromHashTable(hash,orig_current);
    return;
    case TRIND:
      current = getParent();
      getChildrenFor(nl,parentTrace,current,hash);
      removeFromHashTable(hash,orig_current);
      return;
    case TRHIDDEN:
    case TRSATA: // not evaluated expression
    case TRSATAIS:
    case TRSATB:
    case TRSATBIS:
      current = getParent();
      getChildrenFor(nl,parentTrace,current,hash);
      removeFromHashTable(hash,orig_current);
      return;
    default: {
	unsigned long newcurrent = followSATs(current);
	if (newcurrent==current) {
	  removeFromHashTable(hash,orig_current);
	  return;
	}
	else current = newcurrent;
      }
      getChildrenFor(nl,parentTrace,current,hash);
      removeFromHashTable(hash,orig_current);
      return;
    }
  }
  removeFromHashTable(hash,orig_current);
  return;
}

int getEDTchildren(unsigned long parentTrace,int **childrenArray) {
  unsigned long current = getResult(parentTrace);
  HashTable* hash = newHashTable(HASH_TABLE_SIZE);
  NodeList* results=newList();
  int l;

  getChildrenFor(results,parentTrace,current,hash);
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
