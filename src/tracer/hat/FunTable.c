/**************************************************************************/
/* FunTable.c                                                             */
/* Stores a function table. For each function application the list of     */
/* arguments is saved. Applications may be compared, only the most        */
/* general application is stored.                                         */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include "Expressions.h"
#include "hatinterface.h"
#include "FunTable.h"
#include "hatgeneral.h"

//#define doStatistics

typedef struct lnode* FunTablePtr;

typedef struct lnode {
  ExprNode* funAppl;
  ExprNode* res;
  filepointer fileoffset;
  FunTablePtr next;
} _FunTable;

FunTable newFunTable() {
  FunTable l = (FunTable) calloc(1,sizeof(_FunTable));
  return l;
}

void freeFunTable(FunTable ftable) {
  _FunTable *e,*l = (_FunTable*) ftable;
  while (l!=NULL) {
    e=l;
    l=l->next;
    freeExpr(e->funAppl);
    freeExpr(e->res);
    free(e);
  }
}

int FunTableLength(FunTable ftable) {
  int c=0;
  _FunTable* l=((_FunTable*) ftable)->next;
  while (l!=NULL) {
    c++;
    l=l->next;
  }
  return c;
}

unsigned long thesame = 0,smaller = 0, moregeneral=0,uncomparable=0;
void addToFunTable(FunTable ftable,ExprNode* funAppl,ExprNode* res,filepointer fileoffset) {
  _FunTable* p;
  _FunTable* e = (_FunTable*) newFunTable();
  _FunTable* l = (_FunTable*) ftable;

  if (e==NULL) {
    fprintf(stderr,"Tried to reserve memory.\n");
    fprintf(stderr,"ERROR: No more space in heap!\n\n");
    exit(1);
  }
  /* {
    char* appstr = prettyPrintExpr(funAppl,1);
    char* resstr = prettyPrintExpr(res,1);
    printf(appstr);
    printf(" = %s\n",resstr);
    freeStr(resstr);freeStr(appstr);
    }*/
  e->funAppl = funAppl;
  e->fileoffset = fileoffset;
  e->res = res;
  e->next = NULL;
  if (l->next==NULL) { // first element!
    e->next = l->next;  // add element to list (first element is a dummy!)
    l->next = e;
  } else {
    int c1=0,c2=0;
    p=l;
    while (p->next!=NULL) { // while incomparable and not end of list
      c1 = compareExpr(p->next->funAppl,funAppl);
      //printf("c1: %i\n",c1);
      if (c1!=2) { // c1=!2 => comparable!
	c2 = compareExpr(p->next->res,res);
	if (c2!=2) {
	  if ((c1<=0)&&(c2>=0)) { // same or less general as in memory! 
	    // remark: for result it's just the other way round! remember more specific
	    // value!
	    freeFunTable((FunTable) e);
	    if ((c1==0)&&(c2==0)) thesame++;else smaller++;
	    return;
	  }
	  if ((c1>=0)&&(c2<=0)) {  // new entry is more general!
	    _FunTable* t = p->next;
	    p->next = p->next->next;
	    t->next = NULL;
	    freeFunTable((FunTable) t);
	    moregeneral++;
	  } else
	    p=p->next;
	} else {p=p->next;uncomparable++;}
      } else {p=p->next;uncomparable++;}
    }
    if (p->next==NULL) { // add entry to table!
      p->next = e;
    } else freeFunTable((FunTable) e);
  }
}

unsigned long showFunTable_internal(FunTable ftable,int mode) {
  char* appstr;
  char* resstr;
  unsigned long c=0;
  char buf[5];
  _FunTable* l = (_FunTable*) ftable;

  if ((l==NULL)||(l->next==NULL)) return 0; else
    { 
      l=l->next;
      while (l!=NULL) {
	c++;
	// printf("#%i: ",c);
	appstr = prettyPrintExpr(l->funAppl,1);
	resstr = prettyPrintExpr(l->res,1);
	printf(appstr);
	printf(" = %s\n",resstr);
	freeStr(resstr);
	l=l->next;
	if ((mode)&&(c % 20==0)) {
	  printf("<press RETURN to continue>");
	  getline(buf,5);
	}
      }
#ifdef doStatistics      
      fprintf(stderr,"found %u unique applications.\n",c);
      fprintf(stderr,"statistics: thesame %u,smaller %u, moregeneral %u, uncomparable %u\n",
	      thesame,smaller,moregeneral,uncomparable);
#endif
      return c;
    }
}

unsigned long showFunTable(FunTable l) {
  return showFunTable_internal(l,0);
}

FunTable _FunTablecurrent,_FunTablelast=NULL;
int _FunTablePos;

void getFunTableEntry(FunTable ftable,long i,
		      filepointer* fp,
		      ExprNode** appl,
		      ExprNode** res) {
  _FunTable* l;
  if ((_FunTablelast == ftable)&&(i>=_FunTablePos)) {
    int tmp;
    l=(_FunTable*) _FunTablecurrent; // remember previous position
    tmp = i;
    i = i-_FunTablePos; // remaining steps to be done
    _FunTablePos = tmp;
  } else {
    l=((_FunTable*) ftable)->next;
    _FunTablePos = i;
  }
  while ((l!=NULL)&&(i-->0)) l=l->next;
  if (l==NULL) {
    _FunTablelast = NULL;
    (*fp) = 0;
    (*appl) = NULL;
    (*res) = NULL;
    return;
  }
  _FunTablelast = ftable;
  _FunTablecurrent = (FunTable) l;
  (*fp) = l->fileoffset;
  (*appl) = l->funAppl;
  (*res) = l->res;
  return;
}

int isInFunTable(FunTable ftable,ExprNode* funAppl,ExprNode* res) {
  int c1=0,c2=0;
  _FunTable* p;
  p = (_FunTable*) ftable;
  while (((_FunTable*) p)->next!=NULL) { // while incomparable and not end of list
    c1 = compareExpr(p->next->funAppl,funAppl);
    if (c1!=2) { // c1=!2 => comparable!
      c2 = compareExpr(p->next->res,res);
      if (c2!=2) {
	if ((c1<=0)&&(c2<=0)) { // same or less general as in memory!
	  return 1; // already in memory!
	}
      }
    }
    p=p->next;
  }
  return 0; // not im memory yet!
}

void checkArities(FunTable ftable) {
  int arity,maxarity=0;
  _FunTable *h,*p;

  p=(_FunTable*) ftable;
  while (p->next!=NULL) {
    arity = getExprArity(p->next->funAppl);
    if (arity>maxarity) maxarity = arity;
    if (arity<maxarity) {
      h=p->next;
      p->next = p->next->next;
      h->next=NULL;
      freeFunTable((FunTable) h);
    } else p=p->next;
  }
  p=(_FunTable*) ftable;
  while (p->next!=NULL) { // now make sure to clear all smaller arities
    if (getExprArity(p->next->funAppl)<maxarity) {
      h=p->next;
      p->next =  p->next->next;
      h->next = NULL;
      freeFunTable((FunTable) h);
    } else p=p->next;
  }
}












