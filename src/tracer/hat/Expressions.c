/**************************************************************************/
/* Expressions.c: module for basic operations on expressions in memory    */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "hatfile.h"
#include "Expressions.h"
#include "hatgeneral.h"
#include "hashtable.h"

AppNode* newAppNode(int arity) {
  AppNode* a = (AppNode*) calloc(1,sizeof(AppNode));
  a->args = (ExprPtr*) calloc(arity,sizeof(ExprNode));
  a->arity = arity;
  return a;
}

void freeAppNode(AppNode* a) {
  if (a==NULL) return;
  if (a->arity>0) free(a->args);
  free(a);
}

void setAppNodeFun(AppNode* a,ExprNode* e) {
  a->fun = e;
}

ExprNode* getAppNodeFun(AppNode* a) {
  return a->fun;
}

/* arguments numbered 0..arity-1 */
void setAppNodeArg(AppNode* a,int i,ExprNode* e) {
  /*if (i>=a->arity) {
    fprintf(stderr,"ERROR: index out of arity range...\n");
    exit(1);
  }*/
  a->args[i] = e;
}

ExprNode* getAppNodeArg(AppNode* a,int i) {
  /*if (i>=a->arity) {
    fprintf(stderr,"ERROR: index out of arity range...\n");
    exit(1);
  }*/
  return a->args[i];
}

IdentNode* newIdentNode(char* name,int infix,int prio) {
  IdentNode* id = (IdentNode*) malloc(sizeof(IdentNode));
  id->name = name;
  id->infixtype = infix;
  id->infixpriority = prio;
  return id;
}

void freeIdentNode(IdentNode* id) {
  free(id);
}

ExprNode* newExprNode(int type) {
  ExprNode* e = (ExprNode*) calloc(1,sizeof(ExprNode));
  e->type = type;
  return e;
}

/* free a single node */
void freeExprNode(ExprNode* e) {
  if (e!=NULL) free(e);
}

/* free the entire structure */
void freeExpr(ExprNode* e) {
  int i;
  if (e!=NULL) {
    switch(e->type) {
    case HatApplication:
      i=0;
      while (i++<e->v.appval->arity)
	freeExpr(getAppNodeArg(e->v.appval,i-1));
      freeAppNode(e->v.appval);
      break;
    case HatIdentifier:
    case HatConstructor:
      freeIdentNode(e->v.identval);
      break;
    case HatSATA:
      freeExpr(e->v.expr);
      break;
    case HatDouble:
      free(e->v.doubleval);
      break;
    case MESSAGE:
      freeStr(e->v.message);
      break;
    }
    freeExprNode(e);
  }
}

int getExprArity(ExprNode* e) {
  if (e==NULL) return 0;
  switch(e->type) {
  case HatApplication:
    return getExprArity(e->v.appval->fun)+e->v.appval->arity;
  case HatSATA:
    return getExprArity(e->v.expr);
  default:
    return 0;
  }
}

int getExprInfixPrio(ExprNode* e) {
  if (e==NULL) return 0;
  switch(e->type) {
  case HatApplication:
    return getExprInfixPrio(e->v.appval->fun);
  case HatSATA:
    return getExprInfixPrio(e->v.expr);
  case HatIdentifier:
  case HatConstructor:
    return e->v.identval->infixpriority;
  default:
    return 0;
  }
}


#define CUT_MESSAGE "<CUT>"
#define CYCLE_MESSAGE "<CYCLE>"

#define MAXCYCLES 255

char* cycleID(int i) {
  int j=0;
  char s[5];
  do {
    s[j++] = 'a'+(i % 26); // allow characters a-z (0-25)
    i = i/26;
  } while ((i!=0)&&(j<5));
  s[j]=0; // set strings sentinel
  return newStr(s);
}

char* addCycle(filepointer* cycles,filepointer newoffset) {
  int i=0;
  while ((i<MAXCYCLES)&&(cycles[i]!=0)&&(cycles[i]!=newoffset)) i++;
  if (cycles[i]==0) cycles[i]=newoffset;
  return cycleID(i);
}

char* isCycle(filepointer* cycles,filepointer offset) {
  int i=0;
  while ((i<MAXCYCLES)&&(cycles[i]!=0)&&(cycles[i]!=offset)) i++;
  if (cycles[i]==offset) {
    return cycleID(i);
  }
  else return NULL;
}

/* building expression at given offset from hat file */
ExprNode* buildExprRek(HatFile handle,filepointer fileoffset,int verbose,
		       unsigned int precision,HashTable* hash,filepointer *cycles) {
//#define DebugbuildExpr
  char b;
  unsigned long p;
  ExprNode* exp=NULL;
  char *s;

  if (precision==0) {
    exp = newExprNode(MESSAGE);
    exp->v.message = newStr(CUT_MESSAGE);
    return exp; // nesting reached the given boundary!
  }

  while (fileoffset!=0) {
#ifdef DebugbuildExpr
    printf("building expression... 0x%x\n",fileoffset);
#endif
    fileoffset=hatFollowTrace(handle,fileoffset); // follow the trace along all SATs and indirections
    b = getNodeType(handle,fileoffset);
#ifdef DebugbuildExpr
    printf("node type: %i\n",b);
#endif
    switch (b) {
    case HatApplication: // Application
      {
	int i=0,arity;
	AppNode* apn;
	ExprNode* fun;
	filepointer functionOffset;

	if (isInHashTable(hash,fileoffset)) { // detected a cycle?
	  exp = newExprNode(MESSAGE);
	  exp->v.message = addCycle(cycles,fileoffset); // get id for this cycle
	  return exp; // cycle detected!
	}
	addToHashTable(hash,fileoffset);

	arity=getAppArity();
	apn=newAppNode(arity);

	exp = newExprNode(TRAPP);
	exp->v.appval = apn;

	functionOffset = getAppFun();
#ifdef DebugbuildExpr
	printf("Found application of arity: %i\n",arity);
#endif
	while (i++<arity) {  // now read all argument pointers into memory
	  setAppNodeArg(apn,i-1,(ExprNode*) getAppArgument(i-1)); // do something nasty
	  // abuse pointers for storing the fileoffsets temporarily
	}
	// build function
	setAppNodeFun(apn,fun=buildExprRek(handle,functionOffset,verbose,precision-1,
					   hash,cycles));
	/* special workaround for IO(_) follows: show operation behind HIDDEN node,
	   to give atleast some information about the kind of IO performed */
	if ((fun)&&(fun->type==MESSAGE)&&(strcmp(fun->v.message,CUT_MESSAGE)==0)) {
	  removeFromHashTable(hash,fileoffset);
	  return fun; // function was cut off -> whole application is cut off!
	} else
	if ((fun)&&(fun->type==NTCONSTRUCTOR)&&(fun->v.identval)) {
	  IdentNode* id = fun->v.identval;
	  if ((id)&&(id->name)&&(strcmp(id->name,"IO")==0)&&(fun->v.appval->arity>0)) {
	    filepointer hidden = hatFollowSATs(handle,
					       (filepointer) getAppNodeArg(apn,0));
	    if (getNodeType(handle,hidden)==TRHIDDEN) {
	      setAppNodeArg(apn,0,(ExprNode*) (getParent()));
	    }
	  }
	}
	/* workaround for IO(_) ends */
	i=0;
	while (i++<arity) {
#ifdef DebugbuildExpr
	  printf("building argument %i\n",i);
#endif
	  // exchange arguments containing fileoffsets against the built expressions
	  setAppNodeArg(apn,i-1,buildExprRek(handle,
					     (unsigned long) getAppNodeArg(apn,i-1),
					     verbose,precision-1,hash,cycles));
	}
	apn->cycle = isCycle(cycles,fileoffset);
	removeFromHashTable(hash,fileoffset);
	return exp;
	}
    case HatIdentifier:
    case HatConstructor: {
      int infix,infixprio;
      exp = newExprNode(b);
      s=getName();
      infixprio=getInfixPrio();
      infix = getInfixType();
      if ((*s)==',') {
	s=newStr(",");
	infix=0;
      } else {
	if (infix==HatNOINFIX) infixprio=32768;
	s=newStr(s);  // read name of identifier/constructor
      }
      exp->v.identval = newIdentNode(s,infix,infixprio);
      return exp;
    }
    case HatName: // Name
      fileoffset=getNameType(); // read NmType -> follow this link to build 
      break;
    case HatProjection: //  Indirection
      fileoffset=getProjTrace(); // follow this link for prettyPrint
      break;
    case HatInt:
      exp = newExprNode(b);
      exp->v.intval = getIntValue();
      return exp;
    case HatChar:
      exp = newExprNode(b);
      exp->v.charval = getCharValue();
      return exp;
    case HatDouble:
      exp = newExprNode(b);
      exp->v.doubleval = (double*) malloc(sizeof(double));
      *(exp->v.doubleval) = getDoubleValue();
      return exp;
    case HatRational:
    case HatInteger:
      exp = newExprNode(b);
      exp->v.intval = getIntegerValue();
      return exp;
    case HatFloat:
      exp = newExprNode(b);
      exp->v.floatval = getFloatValue();
      return exp;
    case HatTuple:
    case HatFun:
    case HatCase:
    case HatLambda:
    case HatDummy:
    case HatCString:
    case HatIf:
    case HatGuard:
    case HatContainer:
      return newExprNode(b);
    case HatHidden:
      if (verbose) {
	exp = newExprNode(MESSAGE);
	exp->v.message = newStr("\253HIDDEN\273");
	return exp;
      } else {
	return NULL;
      }
    case HatSATA: // unevaluated expression
      if (verbose) {
	p = getParent();
	exp = newExprNode(b);
	exp->v.expr = buildExprRek(handle,p,verbose,precision,hash,cycles);
	return exp;
      } else {
	return NULL;
      }
    case HatSATB:
      exp = newExprNode(b);
      exp->v.intval = getParent();
      return exp;
    default:
      fprintf(stderr, "(buildExprRek) strange tag %d in 0x%x, handle: %u\n",
	      b,fileoffset,handle);
      return NULL; //exit(1);
    }
  }
  return NULL;
}

/* build expression at given offset in file */
ExprNode* buildExpr(HatFile handle,filepointer fileoffset,int verbose,
		    unsigned int precision) {
  HashTable* hash = newHashTable(8000);
  filepointer *cycles = calloc(MAXCYCLES,sizeof(filepointer));
  return buildExprRek(handle,fileoffset,verbose,precision,hash,cycles);
  freeHashTable(hash);
  free(cycles);
}

/*********************************************************************/


/*********************************************************************/
/* pretty printing routines for expressions                          */
/*                                                                   */
/*********************************************************************/

/* return a pretty print of char c in a new string */
char* prettyChar(char c) {
  char buf[10];
  if (c=='\\') {return newStr("\\\\");}
  if (c=='\n') {return newStr("\\n");}
  if (c=='\r') {return newStr("\\r");}
  if (c=='\t') {return newStr("\\t");}
  if (c=='\b') {return newStr("\\b");}
  if ((int)c<=31) {
    int i=(int) c;
    sprintf(buf,"\\%i%i%i",i / 64,(i % 64)/8,(i % 8));
  }
  else sprintf(buf,"%c",c);
  return newStr(buf);
}


/* return a string value of the expressions, or NULL if it's not a string */
char* getStringExpr(ExprNode* exp) {
// #define DebugStringExpr  // enable this switch to get debug info
  char *s1,*s2;
  ExprNode* first;

  if (exp==NULL) return NULL;
#ifdef DebugStringExpr
  printf("inside getStringExpr\n");
  printf("if: %i\n",exp->type);
#endif
  if ((exp->type==TRAPP)&&(exp->v.appval->fun!=NULL)&&
      (exp->v.appval->fun->type==NTCONSTRUCTOR)
      &&(strcmp(exp->v.appval->fun->v.identval->name,":")==0)) {
    // ok, so far, it's a list...
#ifdef DebugStringExpr
    printf("APP:  %i\n",exp->v.appval->fun->type);
    printf("name: %s\n",exp->v.appval->fun->v.identval->name);
    printf("getting AppNodeArg\n");
#endif
    first = getAppNodeArg(exp->v.appval,0);
#ifdef DebugStringExpr
    printf("type...\n");if (first!=NULL) printf("is: %u\n",first->type);
#endif
    if ((first!=NULL)&&(first->type==NTCHAR)) {
      s1 = prettyChar(first->v.charval);
    } else 
      return NULL;
#ifdef DebugStringExpr
    printf("rek to getStringExpr\n");
#endif
    s2 = getStringExpr(getAppNodeArg(exp->v.appval,1)); // check whether right argument is string
    if (s2!=NULL) {
      replaceStr(&s1,s1,s2,NULL);
      freeStr(s2);
      return s1;
    }
    freeStr(s1);
    return NULL;
  } else
    if ((exp->type==NTCONSTRUCTOR)&&(strcmp(exp->v.identval->name,"[]")==0)) {
#ifdef DebugStringExpr
      printf("found []...\n");
#endif
      return newStr("");
    }
    else
      if ((exp->type==MESSAGE)&&(strcmp(exp->v.message,CUT_MESSAGE)==0)) {
	return newStr("..."); // avoid showing strings as lists,
	//                       just because they were cut off
      }
      else {
	return NULL;
      }
}

char minibuf[256];

/* pretty printing routine, called recursively */
char* printRekExpr(ExprNode* exp,int verbose,unsigned int precision,int topInfixprio) {
  //#define DebugPrintExpr  // enable switch to get debug info
  char* s1;
  char* s2;
  if (exp==NULL) return newStr("_");
  if (precision==0) return newStr("<CUT>");
#ifdef DebugPrintExpr
  printf("exp->type %i\n",exp->type);
#endif
  switch (exp->type) {
  case HatApplication:
    {
      AppNode* apn=exp->v.appval;
      int infix=3,infixprio=32768;
      int i=0,spacing=1,nospace=0,tuple=0;
#ifdef DebugPrintExpr
      printf("application of arity %i\n",apn->arity);
      printf("address is: %u\n",(unsigned long) exp);
#endif
      s1 = getStringExpr(exp);
#ifdef DebugPrintExpr
      printf("got string expr..\n");
#endif
      if (s1!=NULL) {
	if (strcmp(s1,"")==0) {
	  freeStr(s1);
	  return newStr("[]");
	} else {
	  replaceStr(&s1,"\"",s1,"\"");
	  return s1;
	}
      }
#ifdef DebugPrintExpr
      printf("before switch..\n");
      if (apn->fun!=NULL) printf("switch: %i\n",apn->fun->type);
#endif
      if (apn->fun==NULL) {
	s1=newStr("_");
	infix=3;
      }
      else
	switch (apn->fun->type) {
	case HatIdentifier:
	case HatConstructor:
	  s1 = newStr(apn->fun->v.identval->name);
	  infix = apn->fun->v.identval->infixtype;
	  infixprio = apn->fun->v.identval->infixpriority;
	  if ((*s1)==',') { // first char is ',' => tuple (2-tuple,...13-tuple)
	    tuple=1;
	    topInfixprio=32768; // force brackets for a tuple
	    replaceStr(&s1,"",NULL,NULL);
	  }
	  break;
	default: {
	    char last=' ';
	    int len=0;
	    // note: functionDepth is not increased in this recursion!
	    s1=printRekExpr(apn->fun,verbose,precision-1,0);
	    infixprio = getExprInfixPrio(apn->fun);
	    len = strlen(s1);
	    if ((s1!=NULL)&&(len>0)) last = s1[len-1];
	    if (((last>='*')&&(last<='.'))||((last>=':')&&(last<='>'))) nospace=1; 
	    // for some infix operators: no spaces!
#ifdef DebugPrintExpr
	    printf("succ in printRekExpr after default\n");
	    printf(s1);
#endif
	  }
	}
#ifdef DebugPrintExpr
      printf("app: %s\n",s1);
#endif
      if ((infix<3)&&(strlen(s1)==1)&&((int)(*s1)<65)) spacing=0;

      for (; i++<apn->arity;) {
	ExprNode* e;
	int iprio;
#ifdef DebugPrintExpr
	printf("prRekExpr for arity %i of %i\n",i,apn->arity);
#endif
	e=getAppNodeArg(apn,i-1);
#ifdef DebugPrintExpr
	printf("got AppNodeArg\n");
#endif
	iprio = infixprio;
	if ((infix==0)||((infix==1)&&(i==1))) iprio = infixprio+1; // right associative
	else if ((infix==2)&&(i>1)) iprio = infixprio+1; // left associative
	s2=printRekExpr(getAppNodeArg(apn,i-1),verbose,precision-1,iprio);
#ifdef DebugPrintExpr
	printf("succ in prRekExpr for arity %i\n",i);
	printf("arg %i: %s\n",i,s2);
	printf("for %i %s\n",apn->arity,s2);
#endif
	if (nospace) {
	  nospace=0;
	  replaceStr(&s1,s1,s2,NULL); // no spacing!
	} else
	  if (tuple) {
	    if (i==1) replaceStr(&s1,s2,NULL,NULL);else
	      replaceStr(&s1,s1,",",s2);
	  } else
	  if ((infix<3)&&(i==1)) {
	    if (isalnum(*s1)) replaceStr(&s1,"`",s1,"`");
	    if (spacing)
	      replaceStr(&s1,s2," ",s1);     // write first argument before the function
	    else replaceStr(&s1,s2,s1,NULL);
	  } else {
	    if ((!spacing)&&(i==2)) replaceStr(&s1,s1,s2,NULL);else
	      replaceStr(&s1,s1," ",s2);     // append new argument
	  }
	freeStr(s2);
#ifdef DebugPrintExpr
	printf("app so far: %s\n",s1);
#endif
      }
      if (((infixprio==32768)&&(apn->arity>0)&&(topInfixprio>0))
	  ||(topInfixprio>infixprio)||(topInfixprio==32768)||(apn->cycle!=NULL)) {
	replaceStr(&s1,"(",s1,")");
	if (apn->cycle!=NULL) {
	  char s[100];
	  sprintf(s,"(%s where %s = ",apn->cycle,apn->cycle);
	  replaceStr(&s1,s,s1,")");
	}
#ifdef DebugPrintExpr
	printf("out1...\n");
#endif
	return s1;
      } else {
#ifdef DebugPrintExpr
	printf("out2...\n");
#endif
	return s1;
      }
    }
  case HatIdentifier:
  case HatConstructor:
    return newStr(exp->v.identval->name);
  case HatInteger:
  case HatRational:
  case HatInt:
    sprintf(minibuf,"%i",exp->v.intval);
    return newStr(minibuf);
  case HatChar: {
    char* s1 = prettyChar(exp->v.charval);
    replaceStr(&s1,"'",s1,"'");
    return s1;
  }
  case HatFloat:
    sprintf(minibuf,"%f",exp->v.floatval);
    return newStr(minibuf);
  case HatDouble:
    sprintf(minibuf,"%f",*(exp->v.doubleval));
    //sprintf(minibuf,"DoUbLe");
    return newStr(minibuf);
  case HatTuple:
    return newStr("TUPLE");
  case HatFun:
    return newStr("");
  case HatCase:
    return newStr("CASE");
  case HatLambda:
    return newStr("LAMBDA");
  case HatDummy:
    return newStr("DUMMY");
  case HatCString:
    return newStr("CSTRING");
  case HatIf:
    return newStr("IF");
  case HatGuard:
    return newStr("GUARD");
  case HatContainer:
    return newStr("CONTAINER");
  case MESSAGE:
    if ((verbose)||(strcmp(exp->v.message,"\253HIDDEN\273")!=0)) {
      return newStr(exp->v.message);
    } else
      return newStr("_");
  case HatSATA:
    if (verbose) return printRekExpr(exp->v.expr,verbose,precision-1,topInfixprio);
    else return newStr("_");
  case HatSATB:
    return newStr("_|_");
  default:
    fprintf(stderr, "strange type in expression syntax tree %i\n",
	    exp->type);
    exit(1);
  }
}

/* pretty printing routine for an expression */
/* verboseMode=1 will show unevaluated parts of the expression */
char* prettyPrintExpr(ExprNode* exp,unsigned int precision,int verboseMode) {
  return printRekExpr(exp,verboseMode,precision,0);
}


/*********************************************************************/
/* comparing routine for expressions                                 */
/*                                                                   */
/*********************************************************************/

/* compares two expressions.
   result: 0 if same,
   -1 if e1<e2, (e1 less defined than e2)
   +1 if e1>e2,
   2 if expressions incomparable
*/
int compareExpr(ExprNode* e1, ExprNode* e2) {
  int c,cmp=0;

  if ((e1==NULL)&&(e2==NULL)) return 0;
  if (e1==NULL) return -1;
  if (e2==NULL) return 1;
  if (e1->type != e2->type) return 2;
  switch(e1->type) {
  case HatApplication:
    {
      int i=0;
      if (compareExpr(e1->v.appval->fun,e2->v.appval->fun)!=0) return 2;
      if (e1->v.appval->arity!=e2->v.appval->arity) return 2; // very, very strange indeed...
      while (i++<e1->v.appval->arity) {
	c=compareExpr(getAppNodeArg(e1->v.appval,i-1),getAppNodeArg(e2->v.appval,i-1));
	if ((c==2)||((c==-1)&&(cmp==1))||((c==1)&&(cmp==-1))) return 2;
	if (c!=0) cmp=c;
      }
      return cmp;
    }
  case HatIdentifier:
  case HatConstructor:
    if (strcmp(e1->v.identval->name,e2->v.identval->name)==0) return 0;
    else return 2;
  case HatInteger:
  case HatRational:
  case HatInt:
    if (e1->v.intval==e2->v.intval) return 0;
    else return 2;
  case HatChar: {
    if (e1->v.charval==e2->v.charval) return 0;
    else return 2;
  }
  case HatFloat:
    if (e1->v.floatval==e2->v.floatval) return 0;
    else return 2;
  case HatDouble:
    if (*(e1->v.doubleval)==*(e2->v.doubleval)) return 0;
    else return 2;
  case HatSATA: // both are unevaluated!
  case HatSATB: // both are bottom!
    return 0;
  default:
    return 2;
  }
}

int getColumnWidth(char* column) {
  char* s;
  if (column==NULL) return 0;
  s=strchr(column,'\n');
  if (s==NULL) return strlen(column);
  return (((unsigned long) s)-((unsigned long) column));
}

char* newCentreStr(char* str,int width) {
  char* s = (char*) malloc((width+1)*sizeof(char));
  int len = strlen(str);
  int l;
  memset(s,' ',width);
  s[width]=0; // set terminator
  l=(width-len);
  if (l<=0) return newStr(str);
  l=l/2;
  strncpy(&(s[l]),str,len);
  return s;
}

#define SPACER "  "
#define ENDLN "\n"

char* addColumns(char* column1,char* column2) {
  char* result=newStr("");
  char *s1,*s2,*empty,*c1,*c2;
  int n1,n2,w1,w2;
  c1=column1;
  c2=column2;
  if ((column1==NULL)&&(column2==NULL)) return NULL;
  if (column1==NULL) return newStr(column2);
  if (column2==NULL) return newStr(column1);
  w1=getColumnWidth(column1);
  w2=getColumnWidth(column2);
  replaceStr(&result,result,newCentreStr("+",w1),SPACER);
  replaceStr(&result,result,newCentreStr("+",w2),ENDLN);
  s1=result;
  n1=0;n2=0;
  while ((*s1)!='+') {((unsigned long) s1)++;n1++;}
  ((unsigned long) s1)++;
  while ((*s1)!='+') {(*s1)='-';((unsigned long) s1)++;n2++;}
  //result[n1+(n2/2)]='+';

  //replaceStr(&result,newCentreStr(" ",w2),ENDLN,result);
  //replaceStr(&result,newCentreStr(" ",w1),SPACER,result);
  replaceStr(&result,result,newCentreStr("|",w1),SPACER);
  replaceStr(&result,result,newCentreStr("|",w2),ENDLN);
  while ((column1!=NULL)&&(*column1!=0)&&(column2!=NULL)&&(*column2!=0)) {
    s1=strchr(column1,'\n');
    s2=strchr(column2,'\n');
    //if ((s1!=NULL)&&(((unsigned long) s1)==((unsigned long) column1)+1)) {
    //  column1=s1;
    //}
    if (s1!=NULL) {
      n1=((unsigned long) s1)-((unsigned long) column1);
      *s1=0;
      s1++;
    } else n1 = strlen(column1);
    if (s2!=NULL) {
      n2=((unsigned long) s2)-((unsigned long) column2);
      *s2=0;
      s2++;
    } else n2 = strlen(column2);
    replaceStr(&result,result,column1,SPACER);
    replaceStr(&result,result,column2,ENDLN);
    column1 = s1;
    column2 = s2;
  }
  if ((column1!=NULL)&&(*column1==0)) column1=NULL;
  if ((column2!=NULL)&&(*column2==0)) column2=NULL;
  if ((column1!=NULL)||(column2!=NULL)) {
    int w,cflag,n;
    char* column;
    if (column1!=NULL) {column=column1;w=w2;cflag=1;} else {w=w1;column=column2;cflag=2;}
    empty=(char*) malloc((w+1)*sizeof(char));
    memset(empty,' ',w);
    empty[w]=0; // set terminator
    while (column!=NULL) {
      s1=strchr(column,'\n');
      if (s1!=NULL) {
	n=((unsigned long) s1)-((unsigned long) column);
	*s1=0;
	s1++;
      } else
	n1 = strlen(column);
      if (strcmp(column,"")!=0) {
	if (cflag==1) {
	  replaceStr(&result,result,column,SPACER);
	  replaceStr(&result,result,empty,ENDLN);
	}
	else {
	  replaceStr(&result,result,empty,SPACER);
	  replaceStr(&result,result,column,ENDLN);
	}
      }
      column=s1;
    }
    freeStr(empty);
    
  }
  freeStr(c1);
  freeStr(c2);
  return result;
}

/* pretty printing routine, called recursively */
char* treePrint(ExprNode* exp,int verbose,int topInfixprio) {
  //#define DebugTreePrint  // enable switch to get debug info
  char* s1;
  char* s2;
  char* fun;
  if (exp==NULL) return newStr("_");
#ifdef DebugTreePrint
  printf("exp->type %i\n",exp->type);
#endif
  switch (exp->type) {
  case HatApplication:
    {
      AppNode* apn=exp->v.appval;
      int infix=3,infixprio=32768;
      int i=0,spacing=1,nospace=0;
#ifdef DebugTreePrint
      printf("application of arity %i\n",apn->arity);
      printf("address is: %u\n",(unsigned long) exp);
#endif
      s1 = getStringExpr(exp);
#ifdef DebugTreePrint
      printf("got string expr..\n");
#endif
      if (s1!=NULL) {
	if (strcmp(s1,"")==0) {
	  freeStr(s1);
	  return newStr("[]");
	} else {
	  replaceStr(&s1,"\"",s1,"\"");
	  return s1;
	}
      }
#ifdef DebugTreePrint
      printf("before switch..\n");
      if (apn->fun!=NULL) printf("switch: %i\n",apn->fun->type);
#endif
      s1=NULL;
      if (apn->fun==NULL) {
	fun=newStr("_");
	infix=3;
      }
      else
	switch (apn->fun->type) {
	case NTIDENTIFIER:
	case NTCONSTRUCTOR:
	  fun = newStr(apn->fun->v.identval->name);
	  infix = apn->fun->v.identval->infixtype;
	  infixprio = apn->fun->v.identval->infixpriority;
          if ((*fun)==',') { //tuple found
	    topInfixprio=32768; // force brackets for a tuple
	  }
	  break;
	default: {
	    char last=' ';
	    int len=0;
	    // note: functionDepth is not increased in this recursion!
	    fun=treePrint(apn->fun,verbose,0);
	    if (fun!=NULL) {
	      unsigned long len=strlen(fun);
	      s1=strchr(fun,'\n');
	      if (s1!=NULL) {
		*s1 = 0; // cut string fun at newline
		s1++;
		if (((unsigned long) s1)>=len+((unsigned long) fun)) s1=NULL;
		if (s1!=NULL) {
		  char* p=fun;
		  s1=newStr(s1);
		  fun=newStr(fun);
		  freeStr(p);
		}
	      }
	    }
	    infixprio = getExprInfixPrio(apn->fun);
	    len = strlen(fun);
	    if ((fun!=NULL)&&(len>0)) last = fun[len-1];
	    if (((last>='*')&&(last<='.'))||((last>=':')&&(last<='>'))) nospace=1; 
	    // for some infix operators: no spaces!
#ifdef DebugTreePrint
	    printf("succ in treePrint after default\n");
	    printf(fun);
#endif
	  }
	}
#ifdef DebugTreePrint
      printf("app: %s\n",fun);
#endif
      if ((infix<3)&&(strlen(fun)==1)&&((int)(*fun)<65)) spacing=0;

      for (; i++<apn->arity;) {
	ExprNode* e;
	int iprio;
#ifdef DebugTreePrint
	printf("treePrint for arity %i of %i\n",i,apn->arity);
#endif
	e=getAppNodeArg(apn,i-1);
#ifdef DebugTreePrint
	printf("got AppNodeArg\n");
#endif
	iprio = infixprio;
	if ((infix==0)||((infix==1)&&(i==1))) iprio = infixprio+1; // right associative
	else if ((infix==2)&&(i>1)) iprio = infixprio+1; // left associative
	//printf("rek call %i\n",i);
	s2=treePrint(getAppNodeArg(apn,i-1),verbose,iprio);
	//printf("got rek call %i\n",i);
#ifdef DebugTreePrint
	printf("succ in treePrint for arity %i\n",i);
	printf("arg %i: ->%s<-\n",i,s2);
	printf("for %i\n",apn->arity);
#endif
	//printf("adding:->%s<-->%s<-\n",s1,s2);
	if (s1==NULL) s1=s2;
	else
	  s1 = addColumns(s1,s2); // add columns and free them!
	//printf("resulting:->%s<-\n",s1);
#ifdef DebugTreePrint
	printf("app so far:->%s<-\n",s1);
#endif
      }
      //printf("finished\n");
      fun=newCentreStr(fun,getColumnWidth(s1));
      replaceStr(&s1,fun,"\n",s1);
      freeStr(fun);
      //printf("endresult: %s\n",s1);
      return s1;
      /*
      if (((infixprio==32768)&&(apn->arity>0)&&(topInfixprio>0))
	  ||(topInfixprio>infixprio)||(topInfixprio==32768)) {
	replaceStr(&s1,"(",s1,")");
#ifdef DebugTreePrint
	printf("out1...\n");
#endif
	return s1;
      } else {
#ifdef DebugTreePrint
	printf("out2...\n");
#endif
        return s1;
	}*/
    }
  case NTIDENTIFIER:
    //if (functionDepth>=showEvalUpToDepth) return newStr("_");
  case NTCONSTRUCTOR:
    return newStr(exp->v.identval->name);
  case NTDOUBLE:
    sprintf(minibuf,"%f",*(exp->v.doubleval));
    return newStr(minibuf);
  case NTINTEGER:
  case NTRATIONAL:
  case NTINT:
    sprintf(minibuf,"%i",exp->v.intval);
    return newStr(minibuf);
  case NTCHAR: {
    char* s1 = prettyChar(exp->v.charval);
    replaceStr(&s1,"'",s1,"'");
    return s1;
  }
  case NTFLOAT:
    sprintf(minibuf,"%f",exp->v.floatval);
    return newStr(minibuf);
  case NTTUPLE:
    return newStr("TUPLE");
  case NTFUN:
    return newStr("");
  case NTCASE:
    return newStr("CASE");
  case NTLAMBDA:
    return newStr("LAMBDA");
  case NTDUMMY:
    return newStr("DUMMY");
  case NTCSTRING:
    return newStr("CSTRING");
  case NTIF:
    return newStr("IF");
  case NTGUARD:
    return newStr("GUARD");
  case NTCONTAINER:
    return newStr("CONTAINER");
  case MESSAGE:
    if (verbose) {
      return newStr(exp->v.message);
    } else
      return newStr("_");
  case TRSATA:
    if (verbose) return treePrint(exp->v.expr,verbose,topInfixprio);
    else return newStr("_");
  case TRSATB:
    return newStr("_|_");
  default:
    fprintf(stderr, "strange type in expression syntax tree %i\n",
	    exp->type);
    exit(1);
  }
}

void showNode(HatFile handle,filepointer fileoffset,int verboseMode,
	      unsigned int precision) {
  char *appstr;
  ExprNode* exp;

  exp = buildExpr(handle,fileoffset,verboseMode,precision<100 ? 100:2*precision);
  appstr = prettyPrintExpr(exp,precision,1);
  printf(appstr);
  freeExpr(exp);
  freeStr(appstr);
}

//#define showAppNode
unsigned long showAppAndResult(HatFile handle,filepointer fileoffset,int verboseMode,
			       unsigned int precision) {
  char *appstr;
  char *resstr;
  ExprNode* exp;
  unsigned long satc = 0;

  exp = buildExpr(handle,fileoffset,verboseMode,precision<100 ? 100:2*precision);
  appstr = prettyPrintExpr(exp,precision,1);
  freeExpr(exp);

  satc = getResult(handle,fileoffset);  // find SATC for the application!

  if (satc!=0) {
#ifdef showAppNode
    printf("(0x%x): ",fileoffset);
#endif
    exp = buildExpr(handle,satc,verboseMode,precision<100 ? 100:2*precision);
    resstr = prettyPrintExpr(exp,precision,1); // don't show any level of unevaluated functions
    freeExpr(exp);
    printf(appstr);
    printf(" = %s\n",resstr); // print value of SAT!

    freeStr(resstr);
  }
  freeStr(appstr);
  return satc;
}



