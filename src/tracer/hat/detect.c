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

// leftmost outermost application/name, without passing any SATCs
// nodes "behind" the SATC were calculated separately, and (may) have nothing
// to do with the current considered EDT node 
filepointer hatOutermostdirect(HatFile handle,filepointer fileoffset) {
  char nodeType;
  filepointer last=0;

  while (1) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case HatSATA:
    case HatSATB:
      return 0;
    case HatSATC:
      hatSeekNode(handle,last);
      return last;
    case HatConstant:
      return fileoffset;
      break;
    case HatIdentifier:
    case HatConstructor:
      return fileoffset;
    case HatApplication:
      last = fileoffset;
      fileoffset = getAppFun();
      break;
    default:
      return 0;
    }
  }
}

filepointer getHatParent(HatFile handle,filepointer nodenumber,
			 filepointer parent) {
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
      getNodeType(handle,fun); // hatFollowSATCs(handle,fun));
      n = hatOutermostdirect(handle,fun);         // in getChildrenRek!
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

int isEDTChild(HatFile handle,filepointer nodenumber,filepointer parent) {
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
    case HatConstant:
    case HatApplication:{
      nodenumber = getHatParent(handle,nodenumber,parent);
      if (nodenumber==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      if (isTopLevelOrTrusted(handle,nodenumber)) {//&&(hatResult(handle,nodenumber)!=0)) {
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

typedef struct hSearchPtr* SearchList;

typedef struct hSearchPtr {
  filepointer parentTrace;
  filepointer current;
  SearchList next;
} _SearchList;

typedef struct {
  HatFile handle;
  HashTable* hash;
  filepointer initialCAF;
  int found;
  int finished;
  SearchList searchList;
} _EDTQuery;

SearchList newSearch(filepointer parentTrace,filepointer current) {
  SearchList l = (SearchList) calloc(1, sizeof(_SearchList));
  if (l==NULL) {
    printf("ERROR: No available heap memory in detect.c!\n");
    exit(1);
  }
  l->parentTrace = parentTrace;
  l->current = current;
  return l;
}

void addSearchElement(SearchList* init,SearchList *l,
		      filepointer parentTrace,
		      filepointer current) {
  if (current!=parentTrace) {
    if ((*l)==NULL) {
      (*init)=newSearch(parentTrace,current);
      (*l)=(*init);
    } else {
      (*l)->next=newSearch(parentTrace,current);
      (*l)=(*l)->next;
    }
  }
}

void addFoundElement(SearchList* init,SearchList *l,filepointer foundnode) {
  if ((*l)==NULL) {
    (*init)=newSearch(foundnode,foundnode);
    (*l)=(*init);
  } else {
    (*l)->next=newSearch(foundnode,foundnode);
    (*l)=(*l)->next;
  }
}

SearchList concatLists(SearchList first,SearchList last,SearchList second) {
  if (last!=NULL) {
    last->next = second;
    return first;
  } else {
    return second;
  }
}

void freeSearchList(SearchList l) {
  SearchList r;
  while (l!=NULL) {
    r = l;
    l=l->next;
    r->next=NULL;
    free(r);
  }
}

EDTQuery newEDTQuery(HatFile handle,filepointer edtnode) {
  filepointer topmost,current=edtnode;
  _EDTQuery* newQ = (_EDTQuery*) calloc(1,sizeof(_EDTQuery));
  newQ->handle = handle;
  newQ->hash = newHashTable(HASH_TABLE_SIZE);
  while (current!=0) { // find topmost CAF
    topmost = current;
    getNodeType(handle,current);
    current = getParent();
  }
  // initialise hash table: save lots of time by disallowing to search
  // all parents of the node!
  current = edtnode;
  while (current!=0) { // add all parents to hash table: do not search them
    current = getHatParent(handle,current,topmost);
    addToHashTable(newQ->hash,current);
  }
  newQ->initialCAF = hatTopAncestor(handle,edtnode);
  newQ->searchList=newSearch(edtnode,hatResult(handle,edtnode));
  return ((EDTQuery) newQ);
}

void freeEDTQuery(EDTQuery query) {
  if (((_EDTQuery*) query)->hash) {
    freeHashTable(((_EDTQuery*) query)->hash);
    ((_EDTQuery*) query)->hash = NULL;
  }
  if (((_EDTQuery*) query)->searchList) {
    freeSearchList(((_EDTQuery*) query)->searchList);
    ((_EDTQuery*) query)->searchList = NULL;
  }
  free(query);
}

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

// #define DebuggedtChildrenFor

#ifdef DebuggedtChildrenFor
int debugLines = 0;
#endif

filepointer getChildrenForRek(_EDTQuery* query,
			      filepointer parentTrace,filepointer current);
filepointer nextEDTQueryNode(EDTQuery q) {
  _EDTQuery* query = (_EDTQuery*) q;
  SearchList sl,searchList = NULL,currentList = NULL;
  HatFile handle=query->handle;
  filepointer parentTrace,current;
  if ((query->finished)||(query->searchList==NULL)) {
    if (!query->finished) {
      freeHashTable(((_EDTQuery*) query)->hash);
      query->hash = NULL;
      query->finished=1;
    }
    return InvalidFilePointer;
  }
  sl = query->searchList; // next element to be searched
  parentTrace = sl->parentTrace;
  current = sl->current;
  query->searchList = sl->next; // remove element from searchList
  sl->next = NULL;
  freeSearchList(sl); // remove element from heap
  if (parentTrace==current) { // special coding for found edt children
    return current;
  }
  return getChildrenForRek(query,parentTrace,current);
}

filepointer getChildrenForRek(_EDTQuery* query,
			      filepointer parentTrace,filepointer current) {
  char nodeType;
  filepointer result,satc=0,orig_current=current;
  SearchList sl,searchList = NULL,currentList = NULL;
  HatFile handle=query->handle;
  {
    // GUARD expressions: parent is an argument at the
    if (current==parentTrace) return nextEDTQueryNode((EDTQuery) query);

    // never consider the CAF which resulted in the
    if (current==query->initialCAF) return nextEDTQueryNode((EDTQuery) query);

    // evaluation of the edtparent! This is not a child, but the top-most parent!
    if (isInHashTable(query->hash,current))
      return nextEDTQueryNode((EDTQuery) query);
    if (current==0) return nextEDTQueryNode((EDTQuery) query);
    addToHashTable(query->hash,current);

    nodeType = getNodeType(handle,current);
#ifdef DebuggedtChildrenFor
    printf("nodeType at 0x%x is %i, searching 0x%x\n",current,nodeType,
	   parentTrace);
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
	filepointer srcref,p,funTrace,appTrace;
	int arity,isChild,isIForGUARD;
	arity     = getAppArity();
	funTrace  = getAppFun();            // function-trace
	srcref    = getSrcRef();            // get srcref
	appTrace  = getHatParent(handle,current,parentTrace);
	if (getNodeType(handle,hatFollowSATCs(handle,funTrace))==HatApplication) {
	  // higher order result of fun is applied additional arguments
	  
	}
	switch (getNodeType(handle,funTrace)) {
	case HatIf:
	case HatGuard:isIForGUARD = 1;arity=1; // reduce arity to 1 (2nd value is parent!)
	default:isIForGUARD = 0;
	}

	isChild   = isEDTChild(handle,current,parentTrace);
#ifdef DebuggedtChildrenFor
	if (isChild) {
	  printf("APP at 0x%x is child of 0x%x\n",current,parentTrace);debugLines++;
	}
#endif
	if ((isChild==0)||(isTopLevel(handle,appTrace)==0)) { // isIForGUARD)) {
#ifdef DebuggedtChildrenFor
	  printf("checking parent of 0x%x\n",current);debugLines++;
#endif
	  // # getChildrenForRek(handle,nl,parentTrace,appTrace,hash,initialCAF);
	  addSearchElement(&searchList,&currentList,parentTrace,appTrace);
	}

	if (isChild) {
	  result=hatResult(handle,current); // only determine hatResult when really necessary
	  satc=hatFollowSATCs(handle,result);
	  if ((satc!=0)&&(getNodeType(handle,satc)==HatSATA))
	    isChild=0; // forget this one, if
	}
	// its result is a SATA (unevaluated!) nothing to ask for...

	if (isChild) { //(appTrace==parentTrace) {
	  int i=0;
	  
	  // check evaluation of (partial) function
	  // # getChildrenForRek(handle,nl,parentTrace,funTrace,hash,initialCAF);
	  addSearchElement(&searchList,&currentList,parentTrace,funTrace);
	  // check all its arguments
	  while (i++<arity) {
#ifdef DebuggedtChildrenFor
	    printf("checking arg %i of 0x%x\n",i,current);debugLines++;
#endif
	    hatSeekNode(handle,current);
	    p = getAppArgument(i-1);
	    // # getChildrenForRek(handle,nl,parentTrace,p,hash,initialCAF);
	    addSearchElement(&searchList,&currentList,parentTrace,p);
	  }
	}
	
	if ((isChild)||(appTrace==0)) {
	  //((appTrace==parentTrace)||(appTrace==0)) {
#ifdef DebuggedtChildrenFor
	  printf("back at APP at 0x%x\n",current);debugLines++;
#endif
	  { 
	    int trusted = isTrusted(handle,funTrace);
	    int toplevel = isTopLevel(handle,funTrace);
	    if ((trusted==0)&&(toplevel)) {
#ifdef DebuggedtChildrenFor
	      printf("Function at 0x%x is not trusted.\n",funTrace);debugLines++;
	      printf("Toplevel: %i\n",toplevel);debugLines++;
#endif
	      if (satc==0) { // only determine hatResult when really necessary!
		result=hatResult(handle,current);
		satc=hatFollowSATCs(handle,result);
	      }
	      if ((satc!=0)&&(satc!=current)) { //&&(!isInList(nl,current))) {
		addFoundElement(&searchList,&currentList,current); // save this
		// node as an EDT child (but consider later!)
	      }
	    } else {
	      if (satc==0) { // only determine hatResult when really necessary!
		result=hatResult(handle,current);
		satc=hatFollowSATCs(handle,result);
	      }
	      if (satc!=current)
		// #getChildrenForRek(handle,nl,current,satc,hash,initialCAF);
		addSearchElement(&searchList,&currentList,current,satc);
	    }
	  }
	}
      }
      query->searchList = concatLists(searchList,currentList,
				      query->searchList);
      return nextEDTQueryNode((EDTQuery) query); // no node found: search next
    case HatConstant: {
      filepointer p = getParent();
      filepointer srcref = getSrcRef();
      filepointer newcurrent;

      if ((p==parentTrace)||(p==0)) {
	if (p==0) {  // CAF found
	  filepointer lmo = hatOutermostSymbol(handle,current);
	  if (lmo!=0) {
	    if (isTopLevel(handle,current)&&(isTrusted(handle,srcref)==0)) {
	      // &&(!isInList(nl,current))) {
	      filepointer sat2,satc = hatResult(handle,current);
	      if ((satc!=0)&&((sat2=hatFollowSATCs(handle,satc))!=current)) {
		return current; // return this node!
	      }
	    }
	  }
	}
	return nextEDTQueryNode((EDTQuery) query);
      }
      else {
        newcurrent = hatFollowSATCs(handle,p);
	if (newcurrent==current) {
	  return nextEDTQueryNode((EDTQuery) query);
	}
	else current = newcurrent;
      }
    }
    // # getChildrenForRek(handle,nl,parentTrace,current,hash,initialCAF);
    addSearchElement(&searchList,&currentList,parentTrace,current);
    query->searchList = concatLists(searchList,currentList,
				    query->searchList);
    return nextEDTQueryNode((EDTQuery) query);
    case HatProjection:
      current = getParent();
      return getChildrenForRek(query,parentTrace,current);
    case HatHidden:
    case HatSATA: // not evaluated expression
    case HatSATB:
      current = getParent();
      return getChildrenForRek(query,parentTrace,current);
    default: {
	filepointer newcurrent = hatFollowSATCs(handle,current);
	if (newcurrent==current) {
	  return nextEDTQueryNode((EDTQuery) query);
	}
	else current = newcurrent;
      }
      return getChildrenForRek(query,parentTrace,current);
    }
  }
  return nextEDTQueryNode((EDTQuery) query);
}

