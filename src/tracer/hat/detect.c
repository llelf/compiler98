/**************************************************************************/
/* detect.c: module for algorithmic debugging of traces                   */
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
#include "hashtable.h"
#include "detect.h"

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

filepointer getEDTroot(HatFile handle) {
  return hatMainCAF(handle);
}

int hasAncestor(HatFile handle,filepointer nodenumber,filepointer parent) {
  char nodeType;
  int distance = 0;
  filepointer old = hatNodeNumber(handle);

  while (1) {
    distance++;
    if (nodenumber==0) {hatSeekNode(handle,old);return 0;}
    if (nodenumber==parent) {hatSeekNode(handle,old);return distance;}
    getNodeType(handle,nodenumber);
    nodenumber = getParent();
  }
}

// leftmost outermost application/name, without passing any SATCs
// nodes "behind" the SATC were calculated separately, and (may) have nothing
// to do with the current considered EDT node 
filepointer hatLMOdirect(HatFile handle,filepointer fileoffset) {
  char nodeType;
  filepointer last=0;

  while (1) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case TRSATA:
    case TRSATB:
    case TRSATBIS:
    case TRSATAIS:
      return 0;
    case TRSATC:
    case TRSATCIS:
      hatSeekNode(handle,last);
      return last;
    case TRNAM:
      return fileoffset;
      break;
    case NTIDENTIFIER:
    case NTCONSTRUCTOR:
      return fileoffset;
    case TRAPP:
      last = fileoffset;
      fileoffset = getAppFun();
      break;
    default:
      return 0;
    }
  }
}

filepointer getHatParent(HatFile handle,filepointer nodenumber,filepointer parent) {
  int d1,d2,ok=0;
  filepointer p1;
  char nodeType;

  nodeType=getNodeType(handle,nodenumber);
  //printf("realparent1: 0x%x\n",nodenumber);
  p1 = getParent();

  if (nodeType==HatApplication) {
    filepointer fun = getAppFun();
    filepointer n;
    if (!isSAT(handle,fun)) {
      getNodeType(handle,fun); // hatFollowSATs(handle,fun));
      n = hatLMOdirect(handle,fun);         // in getChildrenRek!
      if (n!=0) {
	getNodeType(handle,n);
	n = getParent();
	if (n!=p1) {
	  d2=hasAncestor(handle,n,parent);
	  if (d2>0) {
	    d1=hasAncestor(handle,p1,parent);
	    if (d2>d1) {
	      nodenumber=n;
	      ok=1;
	    }
	  }
	} else {nodenumber=n;ok=1;}
      }
    }
  }
  if ((nodenumber!=0)&&(ok==0)) {
    nodenumber=hatFollowHidden(handle,p1);
  }
  return nodenumber;
}

int isChildOf(HatFile handle,filepointer nodenumber,filepointer parent) {
  char nodeType;
  filepointer old = hatNodeNumber(handle);

  if (parent==0) return 0;
  while (nodenumber!=0) {
    nodeType=getNodeType(handle,nodenumber);
    switch(nodeType) {
    case HatHidden:
    case HatSATA:
    case HatSATB:
    case HatSATC:
      nodenumber=getParent();
      if (nodenumber==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      break;
    case HatName:
    case HatApplication:{
      nodenumber = getHatParent(handle,nodenumber,parent);
      if (nodenumber==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      if (isTopLevel(handle,nodenumber)&&(getResult(handle,nodenumber)!=0)) {
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

//#define DebuggedtChildrenFor

#ifdef DebuggedtChildrenFor
int debugLines = 0;
#endif

void getChildrenForRek(HatFile handle,
		       NodeList* nl,filepointer parentTrace,filepointer current,
		       HashTable* hash,filepointer initialCAF) {
  char nodeType;
  filepointer satc=0,result,orig_current=current;

  //exit(1);
  {
    if (current==parentTrace) return; // GUARD expressions: parent is an argument at the
    // same time: avoid loops!
    if (current==initialCAF) return;  // never consider the CAF which resulted in the
    // evaluation of the edtparent! This is not a child, but the top-most parent!
    if (isInHashTable(hash,current)) return;
    if (current==0) return;
    addToHashTable(hash,current);

    result=getResult(handle,current);

    nodeType = getNodeType(handle,current);
#ifdef DebuggedtChildrenFor
    printf("nodeType at 0x%x is %i, searching 0x%x, resulting 0x%x\n",current,nodeType,
	   parentTrace,result);
    debugLines++;
    if (debugLines > 20) {
      char c;
      fflush(stdout);
      c=getchar();
      while ((c!=EOF)&&(c!='\n')) c=getchar();
      debugLines = 0;
    }
#endif
    switch (nodeType) {
    case HatApplication:
      {
	unsigned long srcref,p,funTrace,appTrace;
	int arity,isChild,isIForGUARD;
	arity     = getAppArity();
	//appTrace  = getParent();            // fileoffset of App-trace
	funTrace  = getAppFun();            // function-trace
	srcref    = getSrcRef();            // get srcref
	//appTrace  = hatFollowHidden(handle,
	//			    appTrace); // follow along hidden to find parent
	
	appTrace = getHatParent(handle,current,parentTrace);
	/*	if (getNodeType(handle,hatFollowSATs(handle,funTrace))!=HatApplication) {
	  // note: this test should match the test in isChildOf
	  // if fun is not an application, take the parent of the leftmost node
	  appTrace = hatLMOName(handle,funTrace);
	  if (appTrace!=0) {
	    if (getNodeType(handle,appTrace)==HatName) {
	      appTrace = getParent();
	    }
	    else appTrace=0;
	  }
	} else {
	  // fun is an application: we'll investigate this application node later!
	  appTrace = hatFollowHidden(handle,appTrace);
	}
	*/
	if (getNodeType(handle,hatFollowSATs(handle,funTrace))==HatApplication) {
	  // higher order result of fun is applied additional arguments
	  
	}
	switch (getNodeType(handle,funTrace)) {
	case HatIf:
	case HatGuard:isIForGUARD = 1;arity=1; // reduce arity to 1 (2nd value is parent!)
	default:isIForGUARD = 0;
	}

	isChild   = isChildOf(handle,current,parentTrace);
	/* if ((appTrace==parentTrace)&&(isChild==0)) {
	  printf("That's odd: 0x%x 0x%x 0x%x\n",current,parentTrace,appTrace);
	  printf("AppTrace: 0x%x\n",getParent());
	  printf("AppTrace: 0x%x\n",getParent());
	  }*/
#ifdef DebuggedtChildrenFor
	if (isChild) {
	  printf("APP at 0x%x is child of 0x%x\n",current,parentTrace);debugLines++;
	}
#endif
	if ((isChild==0)||(isTopLevel(handle,appTrace)==0)) { // isIForGUARD)) {
	  // isChild check is not enough, 'cause then and else clauses 
	  // of IF's (and GUARDs) are at the parent...
	  // (isChild==0) { //(appTrace!=parentTrace) { // if it's not a child itself
#ifdef DebuggedtChildrenFor
	  printf("checking parent of 0x%x\n",current);debugLines++;
#endif
	  getChildrenForRek(handle,nl,parentTrace,appTrace,hash,initialCAF);
	}

	satc=hatFollowSATs(handle,result);
	if ((satc!=0)&&(getNodeType(handle,satc)==HatSATA)) isChild=0; // forget this one, if
	// its result is a SATA (unevaluated!) nothing to ask for...

	if (isChild) { //(appTrace==parentTrace) {
	  int i=0;

	  // check evaluation of (partial) function
	  getChildrenForRek(handle,nl,parentTrace,funTrace,hash,initialCAF);

	  // check all its arguments
	  while (i++<arity) {
#ifdef DebuggedtChildrenFor
	    printf("checking arg %i of 0x%x\n",i,current);debugLines++;
#endif
	    hatSeekNode(handle,current);
	    p = getAppArgument(i-1);
	    getChildrenForRek(handle,nl,parentTrace,p,hash,initialCAF);
	  }
	}
	
	if ((isChild)||(appTrace==0)) {
	  //((appTrace==parentTrace)||(appTrace==0)) {
#ifdef DebuggedtChildrenFor
	  //printf("APP at 0x%x is child!\n",current);debugLines++;
	  printf("back at APP at 0x%x\n",current);debugLines++;
#endif
	  // satc=hatFollowSATs(handle,result); done already!
	  { 
	    int trusted = isTrusted(handle,funTrace);
	    int toplevel = isTopLevel(handle,funTrace);
	    if ((trusted==0)&&(toplevel)) {
#ifdef DebuggedtChildrenFor
	      printf("Function at 0x%x is not trusted.\n",funTrace);debugLines++;
	      printf("Toplevel: %i\n",toplevel);debugLines++;
#endif
	      if ((satc!=0)&&(satc!=current)&&(!isInList(nl,current))) {
		insertInList(nl,current);
	      }
	    } else {
	      if (satc!=current)
		getChildrenForRek(handle,nl,current,satc,hash,initialCAF);
	    }
	  }
	}
      }
      //removeFromHashTable(hash,orig_current);
      return;
    case HatName: {
      unsigned long p = getParent();
      unsigned long srcref = getSrcRef();
      unsigned long newcurrent;

      if ((p==parentTrace)||(p==0)) {
	if (p==0) {  // CAF found
	  unsigned long lmo = hatLMO(handle,current);
	  if (lmo!=0) {
	    if (isTopLevel(handle,current)&&(isTrusted(handle,srcref)==0)&&
		(!isInList(nl,current))) {
	      filepointer sat2,satc = getResult(handle,current);
	      if ((satc!=0)&&((sat2=hatFollowSATs(handle,satc))!=current)) {
		insertInList(nl,current);
	      }
	    }
	  }
	}
	//removeFromHashTable(hash,orig_current);
	return;
      }
      else {
        newcurrent = hatFollowSATs(handle,p);
	if (newcurrent==current) {
	  //removeFromHashTable(hash,orig_current);
	  return;
	}
	else current = newcurrent;
      }
    }
    getChildrenForRek(handle,nl,parentTrace,current,hash,initialCAF);
    //removeFromHashTable(hash,orig_current);
    return;
    case HatProjection:
      current = getParent();
      getChildrenForRek(handle,nl,parentTrace,current,hash,initialCAF);
      //removeFromHashTable(hash,orig_current);
      return;
    case HatHidden:
    case HatSATA: // not evaluated expression
    case HatSATB:
      current = getParent();
      getChildrenForRek(handle,nl,parentTrace,current,hash,initialCAF);
      //removeFromHashTable(hash,orig_current);
      return;
    default: {
	unsigned long newcurrent = hatFollowSATs(handle,current);
	if (newcurrent==current) {
	  //removeFromHashTable(hash,orig_current);
	  return;
	}
	else current = newcurrent;
      }
      getChildrenForRek(handle,nl,parentTrace,current,hash,initialCAF);
      //removeFromHashTable(hash,orig_current);
      return;
    }
  }
  //removeFromHashTable(hash,orig_current);
  return;
}

void getChildrenFor(HatFile handle,
		    NodeList* nl,filepointer edtnode) {
  filepointer result = getResult(handle,edtnode);
  if (edtnode) { // does any result exist?
    HashTable* hash=newHashTable(HASH_TABLE_SIZE);
    filepointer topmost,current=edtnode;
    while (current!=0) { // find topmost CAF
      topmost = current;
      getNodeType(handle,current);
      current = getParent();
    }
    // initialise hash table: save lots of time by disallowing to search
    // all parents of the node to be searched!
    current = edtnode;
    while (current!=0) { // add all parents to hash table: do not search them
      current = getHatParent(handle,current,topmost);
      addToHashTable(hash,current);
    }
    getChildrenForRek(handle,nl,edtnode,result,hash,hatInitialCAF(handle,edtnode));
    freeHashTable(hash);
  }
}

// interface function to Haskell: return children as an array rather than a list
int getEDTchildren(HatFile handle,filepointer parentTrace,int **childrenArray) {
  NodeList* results=newList();
  int l;

  getChildrenFor(handle,results,parentTrace);
  l = listLength(results);
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
  }
  freeList(results);
  return l;
}

void freeArray(int *array) {
  if (array!=NULL) free(array);
  array = NULL;
}
