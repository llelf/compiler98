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
#include "hatfileops.h"
#include "FunTable.h"

//#define doStatistics

FunTable* newFunTable() {
  FunTable* l = (FunTable*) calloc(1,sizeof(FunTable));
}

void freeFunTable(FunTable* e) {
  FunTable* l;
  l=e;
  while (l!=NULL) {
    e=l;
    l=l->next;
    freeExpr(e->funAppl);
    freeExpr(e->res);
    free(e);
  }
}

int FunTableLength(FunTable* l) {
  int c=0;
  l=l->next;
  while (l!=NULL) {
    c++;
    l=l->next;
  }
  return c;
}

unsigned long thesame = 0,smaller = 0, moregeneral=0,uncomparable=0;
void addToFunTable(FunTable* l,ExprNode* funAppl,ExprNode* res,unsigned long fileoffset) {
  FunTable* p;
  FunTable* e = newFunTable();

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
	    freeFunTable(e);
	    if ((c1==0)&&(c2==0)) thesame++;else smaller++;
	    return;
	  }
	  if ((c1>=0)&&(c2<=0)) {  // new entry is more general!
	    FunTable* t = p->next;
	    p->next = p->next->next;
	    t->next = NULL;
	    freeFunTable(t);
	    moregeneral++;
	  } else
	    p=p->next;
	} else {p=p->next;uncomparable++;}
      } else {p=p->next;uncomparable++;}
    }
    if (p->next==NULL) { // add entry to table!
      p->next = e;
    } else freeFunTable(e);
  }
}

void showFunTable_internal(FunTable* l,int mode) {
  char* appstr;
  char* resstr;
  unsigned long c=0;
  char buf[5];
  if ((l==NULL)||(l->next==NULL)) printf("FUNCTION TABLE EMPTY\n"); else
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
    }
}

void showFunTable(FunTable* l) {
  showFunTable_internal(l,0);
}

unsigned long getFunTableFileOffs(FunTable*l,long i) {
  l=l->next;
  while ((l!=NULL)&&(i-->0)) l=l->next;
  if (l==NULL) return 0;
  return l->fileoffset;
}

int isInFunTable(FunTable* p,ExprNode* funAppl,ExprNode* res) {
  int c1=0,c2=0;
  while (p->next!=NULL) { // while incomparable and not end of list
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

void checkArities(FunTable* ftable) {
  int arity,maxarity=0;
  FunTable *h,*p;

  p=ftable;
  while (p->next!=NULL) {
    arity = getExprArity(p->next->funAppl);
    if (arity>maxarity) maxarity = arity;
    if (arity<maxarity) {
      h=p->next;
      p->next = p->next->next;
      h->next=NULL;
      freeFunTable(h);
    } else p=p->next;
  }
  p=ftable;
  while (p->next!=NULL) { // now make sure to clear all smaller arities
    if (getExprArity(p->next->funAppl)<maxarity) {
      h=p->next;
      p->next =  p->next->next;
      h->next = NULL;
      freeFunTable(h);
    } else p=p->next;
  }
}












