/**************************************************************************/
/* hat-detect: algorithmic debugging for traces in a hat redex trace file */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "Expressions.h"
#include "hatinterface.h"
#include "FunTable.h"
#include "nodelist.h"
#include "hashtable.h"
#include "observe.h"
#include "hatgeneral.h"
#include "detect.h"
#include "menu.h"

#define HASH_TABLE_SIZE 3000

void checkmainCAF(HatFile handle);
int askNodeList(HatFile handle,int question,NodeList* results,
		int isTopSession,HashTable* hash);

char*     traceFileName=NULL;
NodeList* userTrustedList = NULL; // list of trusted functions
FunTable memorizedFunsYes = NULL;
FunTable memorizedFunsNo = NULL;
NodeList* CAFList = NULL;
unsigned int precision = 30;
int filehandle;

int main (int argc, char *argv[])
{
  if (argc!=2) {
    fprintf(stderr,"\nusage: hat-detect file-name\n");
    fprintf(stderr,"       algorithmic debugging on a hat redex trace file\n\n");
    exit(1);
  }
  traceFileName = hatFilename(argv[1]);
  if ((filehandle=hatOpenFile(traceFileName))==-1) {
    fprintf(stderr, "cannot open trace file %s\n\n",traceFileName);
    exit(1);
  }
  if (filehandle==-2) {
    fprintf(stderr, "format of file unknwon/not supported %s\n\n",traceFileName);
    exit(1);
  }
  userTrustedList = newList();
  CAFList = newList();
  memorizedFunsYes = newFunTable();
  memorizedFunsNo = newFunTable();
  checkmainCAF(filehandle);
  hatCloseFile(filehandle);
  return 0;
}

int getline(char s[], int max) {
  int c,i;
  fflush(stdout);
  c=getchar();
  for (i=0;(i<max-1) && (c!=EOF) && (c!='\n');i++) {
    if (c==-1) {
      i--;
    } else {
      s[i]=c;
    }
    c=getchar();
  }
  s[i]=0;
  return i;
}

void quit() {
  printf("\n\nOk, goodbye!\n");
  exit(0);
}

/* yes = 1, no = 0 */
void memorizeAnswer(ExprNode* app,ExprNode* res,int answer) {
  if (answer==0)
    addToFunTable(memorizedFunsNo,app,res,0);
  else 
    addToFunTable(memorizedFunsYes,app,res,0);
}

int getMemorizedAnswer(ExprNode* app,ExprNode* res) {
  int no = 0; // isInFunTable(memorizedFunsNo,app,res); no sense in memorizing no's!
  // memorized no's must lead to a cycle, if they're actually used!
  int yes = isInFunTable(memorizedFunsYes,app,res);
  if ((!no)&&(yes)) return 1;
  if ((no)&&(!yes)) return 0;
  return -1; // unknown!
}

void clearMemorized() {
  freeFunTable(memorizedFunsNo);
  freeFunTable(memorizedFunsYes);
  memorizedFunsYes = newFunTable();
  memorizedFunsNo = newFunTable();
}

void untrustAll() {
  freeList(userTrustedList);
}

void userTrustsFunction(unsigned long fileoffset) {
  insertInList(userTrustedList,fileoffset);
}

int isUserTrusted(unsigned long fileoffset) {
  return isInList(userTrustedList,fileoffset);
}

void clearCAFList() {
  if (CAFList!=NULL) freeList(CAFList);else
    CAFList = newList();
}

void showReduction(HatFile handle,filepointer application,
		   filepointer result,int verbose) {
  showNode(handle,application,verbose,precision);
  printf(" = ");
  showNode(handle,result,verbose,precision);
  printf("\n");
}

/***********************************************************************/
/* observing function applications                                     */
/***********************************************************************/

FunTable currentFTable;

char* getFunTableStr(int i) {
  char *appstr,*resstr;
  ExprNode *app,*res;
  filepointer nodenumber;
  
  getFunTableEntry(currentFTable,i,&nodenumber,&app,&res);
  if (nodenumber==0) return newStr("");
  appstr = prettyPrintExpr(app,1);
  resstr = prettyPrintExpr(res,1);
  replaceStr(&appstr,appstr," = ",resstr);
  freeStr(resstr);
  return appstr;
}

long showFunTablePaged(FunTable l) {
  currentFTable=l;
  return menu("choose any equation and press <RETURN> or <Q> to cancel",
	      FunTableLength(l),&getFunTableStr);  
}

/***********************************************************************/

int verboseMode = 0;
int memorizeMode = 1;

/* returns 1 for yes, 0 for no, 2 for ? */
int askForApp(HatFile handle,int *question,unsigned long appofs,
	      unsigned long resofs,int reconsider) {
  char answer[51];
  int c,err,retval=-1;
  ExprNode *appNode,*resNode;
  char *pp1,*pp2;
  char isCAF=0;
  unsigned long lmost = hatLMO(handle,appofs);

  if (isUserTrusted(lmost)) return 1;
  isCAF = (resofs == 0);
  if (isCAF) resofs = getResult(handle,appofs);

  appNode = buildExpr(handle,appofs,verboseMode,precision);
  resNode = buildExpr(handle,resofs,verboseMode,precision);

  if (memorizeMode) {
    c=getMemorizedAnswer(appNode,resNode);
    if ((c==0)||(c==1)) retval=c; // return the memorized answer, if known
  }
  (*question)++;
  
  while (retval==-1) {
    if (reconsider) printf("\nreconsider:");
    printf("\n%i> ",*question);
    pp1 = prettyPrintExpr(appNode,verboseMode);
    printf("%s = ",pp1);
    pp2 = prettyPrintExpr(resNode,verboseMode);
    printf("%s",pp2);
    if (strlen(pp1)+strlen(pp2)>70) printf("\n");
    freeStr(pp2);freeStr(pp1);
    err=0;
    if (reconsider) printf("   (Y/N): ");
    else printf("   (Y/?/N): ");
    if (getline(answer,50)>=1) {
      c=toupper(answer[0]);
      if (strchr("YARNQT?HVUMGOC*0123456789+-",c)==NULL) { // was answer a valid character?
	printf("   Sorry. Please answer the question with 'Yes' or 'No' or press 'H' for help!\n");
      } else {
	switch(c) {
	case '*':
	  if (answer[1]=='*') {
	    printf("debug info: %u %u\n",appofs,resofs);
	  }
	  break;
	case 'O': 
	  {
	    unsigned long newAdr,selected;
	    FunTable results;
	    ObserveQuery query;
	    if (strlen(answer)>2) {
	      int rek=0;
	      char* ans = answer+2;
	      if ((strlen(ans)>2)&&(*ans=='-')&&(*(ans+1)=='r')) {
		rek = 1;
		ans=ans+3;
	      }
	      printf("looking for %s\n",ans);
	      query = newObserveQueryIdent(filehandle,
					   ans,
					   NULL,rek,1);
	    } else {
	      query = newObserveQuery(filehandle,lmost,0,0,1);
	    }
	    results = observeUnique(query,verboseMode,precision);
	    selected=showFunTablePaged(results);
	    if (selected>=0) {
	      ExprNode* dummy;
	      getFunTableEntry(results,selected,&newAdr,&dummy,&dummy);
	    } else newAdr=0;
	    freeObserveQuery(query);
	    freeFunTable(results);

	    if ((newAdr>0)&&(newAdr!=appofs)) {
	      char s[10];
	      printf("Start detect session for ");
	      showAppAndResult(handle,newAdr,verboseMode,precision);
	      printf("(Y/N): ");
	      getline(s,10);
	      if (toupper(s[0])=='Y') {
		NodeList* nl=newList();
		printf("New detect session for: ");
		showAppAndResult(handle,newAdr,verboseMode,precision);
		printf("\n");
		appendToList(nl,newAdr);
		{
		  HashTable* hash=newHashTable(HASH_TABLE_SIZE);
		  askNodeList(handle,0,nl,1,hash);
		  freeHashTable(hash);
		}
		printf("\nResuming previous session.\n");
	      }
	    }
	  }
	break;
	case 'Y':
	  if (memorizeMode) {
	    memorizeAnswer(appNode,resNode,1);
	    return 1;
	  } else retval = 1;
	  break;
	case '?':
	   if (!reconsider) {
	     retval=2; // maybe...
	     break;
	   }
	   if (reconsider) {
	     printf("   Sorry, you need to decide now. Answer 'Y' or 'N'.");
	   }
	   break;
	case 'N':
	  if (memorizeMode) {
	    memorizeAnswer(appNode,resNode,0);
	    return 0;
	  } else retval = 0;
	  break;
	case 'Q':
	  quit();
	  break;
	case 'T':
	  userTrustsFunction(lmost);
	  printf("   Ok. \"");
	  showNode(handle,lmost,verboseMode,precision);
	  printf("\" will be trusted from now on.\n");
	  retval = 1; // answer is yes
	  break;
	case 'U':
	  untrustAll();
	  printf("   Ok. All functions are untrusted again.\n");
	  break;
	case 'A':
	case 'R':
	  {
	    char* treeP;
	    if (c=='A') treeP = treePrint(appNode,verboseMode,0);
	    else treeP = treePrint(resNode,verboseMode,0);
	    printf(treeP);
	    freeStr(treeP);
	  }
	  break;
	case '+':
	case '-':
	  {
	    long step = atol(answer);
	    long prec = precision;
	    printf("%i\n",step);
	    if (step==0) {
	      if (answer[0]=='+') step=1;else step=-1;
	    }
	    if ((step<0)&&(prec+step<2)&&(prec>2)) {
	      step = -(prec-2);
	    }
	    if ((step>0)||(prec+step>1)) {
	      precision = precision+step;
	      freeExpr(appNode);
	      freeExpr(resNode);
	      appNode = buildExpr(handle,appofs,verboseMode,precision);
	      resNode = buildExpr(handle,resofs,verboseMode,precision);
	      printf("precision now %i",precision);
	    } else printf("Precision already at minimum level.\n");
	  }
	  break;
	case 'V':
	  verboseMode = 1-verboseMode;
	  printf("   verbose mode is now ");
	  if (verboseMode) printf("ON\n"); else printf("OFF\n");
	  freeExpr(appNode);
	  freeExpr(resNode);
	  appNode = buildExpr(handle,appofs,verboseMode,precision);
	  resNode = buildExpr(handle,resofs,verboseMode,precision);
	  break;
	case 'M':
	  memorizeMode = 1-memorizeMode;
	  printf("   memorize mode is now ");
	  if (memorizeMode) printf("ON\n"); else printf("OFF\n");
	  break;
	case 'C':
	  printf("   all memorized answers cleared.\n");
	  clearMemorized();
	  break;
	case '0':
	case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':
	case 'G':
	  if (*question==1) {
	    printf("Cannot go back. Already at question 1.\n");
	  } else {
	    int j,i=1;
	    if (toupper(answer[0])=='G') { // go command
	      if ((strlen(answer)>1)&&
		  ((toupper(answer[1])=='O')||(toupper(answer[1])==' '))) i++;
	    } else i=0; // only number answered
	    j = atol(i+answer);
	    if ((j<1)||(j>*question)) {
	      if (j<1) printf("Bad number given.\n");
	      else printf("Can only go back to questions 1 to %i.\n",*question);
	    } else {
	      retval=10*j;
	      clearMemorized();
	      untrustAll();
	      clearCAFList();
	    }
	  }
	  break;
	case 'H':
	  printf("\nhat-detect - HELP\n\n");
	  printf("Keyboard commands\n\n");
	  printf("     'Yes'      or 'y' if the equation is ok.\n");
	  printf("     'No'       or 'n' if it isn't.\n");
	  if (!reconsider)
	    printf("     '?'               if you are unsure whether it is or is not.\n");
	  printf("\n     'Trust'    or 't' if the function shall now be trusted.\n");
	  printf("     'Untrust'  or 'u' to untrust all functions again.\n");
	  printf("\n     'Memorize' or 'm' to toggle the memorize mode.\n");
	  printf("     'Clear'    or 'c' to clear all memorized answers.\n");
	  printf("     'Verbose'  or 'v' to toggle verbose mode.\n");
	  printf("     'Go <n>'   or '<n>' to go back to question <n>.\n");
	  printf("     'Observe'  or 'o' to observe all applications of the current function.\n");
	  printf("\n     '+[n]'     or '-[n]' to increase or decrease the output precision [by n].\n");
	  printf("\n     'Quit'     or 'q' to leave the tool.\n");
	  break;
	}
      }
    }
  }
  freeExpr(appNode);
  freeExpr(resNode);
  return retval;
}

int askNodeList(HatFile handle,int question,NodeList* results,
		int isTopSession,HashTable* hash) {
  unsigned long satc;
  int success,askAgain,question_old,first_question;
  NodeList* children=NULL;
  char s[2];
  NodeElement* e;
  first_question = question;
  do {
    question=first_question;
    success=0;
    e=results->first;
    while ((success==0)&&(e!=NULL)) {
      int answer;
      askAgain = 0;
      question_old = question;
      if (isInList(CAFList,e->fileoffset)==0) {
	answer=askForApp(handle,&question,e->fileoffset,0,0);
	if (isCAF(handle,e->fileoffset)) {
	  insertInList(CAFList,e->fileoffset);
	}
      } else answer=1;
      if ((answer!=1)&&(answer<10)){
	satc=getResult(handle,e->fileoffset);
	children = newList();
	{
	  HashTable* hash=newHashTable(HASH_TABLE_SIZE);
	  getChildrenFor(handle,children,e->fileoffset,satc,hash);
	  freeHashTable(hash);
	}
	success = askNodeList(handle,question,children,0,hash);
	if (success>=10) {answer=success;success=0;}
	if (answer==2) {
	question = question_old;
	answer = askForApp(handle,&question,e->fileoffset,0,1);
	}
	if (answer==0) {
	  char *tmp;
	  printf("\nError located!\nBug found in: ");
	  showReduction(handle,e->fileoffset,getResult(handle,e->fileoffset),
			verboseMode);
	  tmp = hatLocationStr(handle,e->fileoffset);
	  printf(tmp);
	  freeStr(tmp);
	  printf("\n\n");
	  printf("\nPress 'q' to quit, any other key to go back to question %i:",
		 question_old+1);
	  getline(s,2);
	  if (toupper(s[0])=='Q') quit();
	  clearMemorized();
	  untrustAll();
	  clearCAFList();
	  askAgain = 1;
	  question = question_old;
	}
      }
      if (answer>=10) {
	if (answer / 10 > first_question) {
	  e=results->first;
	  question = first_question;
	  while (question+1<answer /10) {
	    if (e->next!=NULL) e=e->next;
	    question++;
	  }
	  askAgain=1;
	} else {
	  freeList(results);
	  return answer;
	}
      }
      if (!askAgain) {
	e=e->next;
      }
    }
    if ((isTopSession)&&(success==0)) {
      printf("Ok, no bug found!\n");
    }
    if (isTopSession==2) { // it's the main session!
      printf("\nPress 'q' to quit, any other key to go back to question 1: ");
      getline(s,2);
      clearMemorized();
      untrustAll();
      clearCAFList();
    }
  } while ((isTopSession==2)&&(toupper(s[0])!='Q'));
  freeList(results);  
  return success;
}

void checkmainCAF(HatFile handle) {
  HashTable* hash=newHashTable(HASH_TABLE_SIZE);
  NodeList* results=newList();
  addBeforeList(results,hatMainCAF(handle));
  askNodeList(handle,0,results,2,hash);
  freeHashTable(hash);
  quit();
}




