/**************************************************************************/
/* hat-detect: algorithmic debugging for traces in a hat redex trace file */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "Expressions.h"
#include "hatfileops.h"
#include "FunTable.h"

/* optional switch */
#define showNodeInfo // show additional information about file offsets

void checkCAFs();

char*     traceFileName=NULL;
NodeList* userTrustedList = NULL; // list of trusted functions
FunTable* memorizedFunsYes = NULL;
FunTable* memorizedFunsNo = NULL;

main (int argc, char *argv[])
{
  if (argc!=2) {
    fprintf(stderr,"\nusage: hat-detect file-name\n");
    fprintf(stderr,"       algorithmic debugging on a hat redex trace file\n\n");
    exit(1);
  }
  traceFileName = filename(argv[1]);
  if (!openfile(traceFileName)) {
    fprintf(stderr, "cannot open trace file %s\n\n",traceFileName);
    exit(1);
  }
  if (testheader()) {
    userTrustedList = newList();
    memorizedFunsYes = newFunTable();
    memorizedFunsNo = newFunTable();
    checkCAFs();

  }
  closefile();
}

void quit() {
  printf("\n\nOk, Goodbye!\n");
  exit(0);
}

int getline(char s[], int max) {
  int c,i;
  for (i=0;(i<max-1) && ((c=getchar())!=EOF) && (c!='\n');i++)
    s[i]=c;
  s[i]=0;
  return i;
}

/* yes = 1, no = 0 */
void memorizeAnswer(ExprNode* app,ExprNode* res,int answer) {
  if (answer==0)
    addToFunTable(memorizedFunsNo,app,res);
  else 
    addToFunTable(memorizedFunsYes,app,res);
}

int getMemorizedAnswer(ExprNode* app,ExprNode* res) {
  int no = isInFunTable(memorizedFunsNo,app,res);
  int yes = isInFunTable(memorizedFunsYes,app,res);
  if ((!no)&&(yes)) return 1;
  if ((no)&&(!yes)) return 0;
  return -1; // unknown!
}

void userUntrust() {
  freeList(userTrustedList);
}

void userTrustsFunction(unsigned long fileoffset) {
  printf("Now trusting function at %u\n",fileoffset);
  insertInList(userTrustedList,fileoffset);
}

int isUserTrusted(unsigned long fileoffset) {
  return isInList(userTrustedList,fileoffset);
}

void showReduction(unsigned long application,unsigned long result,int verbose) {
  showNode(application,verbose);
  printf(" = ");
  showNode(result,verbose);
  printf("\n");
}

int verboseMode = 0;
int memorizeMode = 0;

/* returns 1 for yes, 0 for no, 2 for ? */
int askForApp(int *question,unsigned long appofs,unsigned long resofs,int reconsider) {
  int success=0;
  char answer[11];
  int c,err,retval=-1;
  ExprNode *appNode,*resNode;
  char* pp;
  char isCAF=0;
  unsigned long lmost = leftmostOutermost(appofs);

  if (isUserTrusted(lmost)) return 1;
  isCAF = (resofs == 0);
  if (isCAF) resofs = findAppSAT(appofs);

  appNode = buildExpr(appofs,verboseMode);
  resNode = buildExpr(resofs,verboseMode);

  if (memorizeMode) {
    c=getMemorizedAnswer(appNode,resNode);
    if ((c==0)||(c==1)) retval=c; // return the memorized answer, if known
  }
  (*question)++;
  while (retval==-1) {
#ifdef showNodeInfo
    if (reconsider) {
      if (isCAF) printf("\n%i. Now reconsider the following CAF (at %u).\n",
			     *question,appofs);
      else
      printf("\n%i. Now reconsider the following equation (at %u/%u).\n",
	     *question,appofs,resofs);
    }
    else {
      if (isCAF) printf("\n%i. Consider the following CAF (at %u).\n",*question,appofs);
      else
	printf("\n%i. Consider the following equation (at %u/%u).\n",*question,appofs,
	       resofs);
    }
#else
    if (reconsider) {
      if (isCAF) printf("\n%i. Now reconsider the following CAF.\n",*question);
      else printf("\n%i. Now reconsider the following equation.\n",*question);
    }
    else {
      if (isCAF)
	printf("\n%i. Consider the following CAF.\n",*question);
      else
	printf("\n%i. Consider the following equation.\n",*question);
    }
#endif 
    pp = prettyPrintExpr(appNode,verboseMode);
    printf("%s = ",pp);
    freeStr(pp);
    pp = prettyPrintExpr(resNode,verboseMode);
    printf("%s\n",pp);
    freeStr(pp);
    err=0;
    if (reconsider) printf("Does it seem ok? (Y/N or Help): ");
    else printf("Does it seem ok? (Y/?/N or Help): ");
    if (getline(answer,10)>=1) {
      c=toupper(answer[0]);
      if (strchr("YNQT?HVUMAR",c)==NULL) { // was answer a valid character?
	printf("Sorry. Please answer the question with 'Yes' or 'No' (Y/N)!\n");
      } else {
	if (c=='Y') {
	  if (memorizeMode) {
	    memorizeAnswer(appNode,resNode,1);
	    return 1;
	  } else retval = 1;
	}
	if ((!reconsider)&&(c=='?')) return 2; // maybe...
	if ((reconsider)&&(c=='?')) {
	  printf("Sorry, you need to decide now. Answer 'Y' or 'N'.");
	}
	if (c=='N') {
	  if (memorizeMode) {
	    memorizeAnswer(appNode,resNode,0);
	    return 0;
	  } else retval = 0;
	}
	if (c=='Q') quit();
	if (c=='T') {
	  userTrustsFunction(lmost);
	  printf("Ok. Function will be trusted from now on.\n");
	  retval = 1; // answer is yes
	}
	if (c=='U') {
	  userUntrust();
	  printf("Ok. All functions are untrusted again.\n");
	}
	if ((c=='A')||(c=='R')) {
	  char* treeP;
	  if (c=='A') treeP = treePrint(appNode,verboseMode,0);
	  else treeP = treePrint(resNode,verboseMode,0);
	  printf(treeP);
	  freeStr(treeP);
	}
	if (c=='V') {
	  verboseMode = 1-verboseMode;
	  printf("verbose mode is now ");
	  if (verboseMode) printf("ON\n"); else printf("OFF\n");
	  freeExpr(appNode);
	  freeExpr(resNode);
	  appNode = buildExpr(appofs,verboseMode);
	  resNode = buildExpr(resofs,verboseMode);
	}
	if (c=='M') {
	  memorizeMode = 1-memorizeMode;
	  printf("memorize mode is now ");
	  if (memorizeMode) printf("ON\n"); else printf("OFF\n");
	}
	if (c=='H') {
	  printf("\nhat-detect - HELP\n\n");
	  printf("Keyboard commands\n\n");
	  printf("     'Yes'      or 'y', if the equation is ok.\n");
	  printf("     'No'       or 'n', if it isn't.\n");
	  if (!reconsider)
	    printf("     '?',               if you are unsure whether it is or is not.\n");
	  printf("\n     'Trust'    or 't', if the function shall now be trusted.\n");
	  printf("     'Untrust'  or 'u', to untrust all functions again.\n");
	  printf("\n     'Memorize' or 'm', to toggle the memorize mode.\n");
	  printf("     'Verbose'  or 'v' to toggle verbose mode.\n");
	  printf("\n     'Quit'     or 'q' to leave the tool.\n");
	}
      }
    }
  }
  freeExpr(appNode);
  freeExpr(resNode);
  return retval;
}

int readCAFsFromFile(NodeList* nl) {
  int n,f;
  unsigned long b;
  char* caffilename=catStr(traceFileName,".caf",NULL);

  f = open(caffilename,0);
  if (f==-1) return 0; // no file with CAFs!
  n=read(f,&b,4);
  if ((n!=4)||(ntohl(b)!=hatStatBuf.st_mtime)) {
    close(f);
    return 0; // hat file is new!
  }
  n=read(f,&b,4);
  while (n==4) {
    appendToList(nl,ntohl(b));
    n=read(f,&b,4);
  }
  close(f);
  return 1;
}

void writeCAFsToFile(NodeList* nl) {
  NodeElement* e;
  int n,f;
  unsigned long b;
  char* caffilename=catStr(traceFileName,".caf",NULL);

  f = creat(caffilename,0666);
  if (f==-1) return;
  b=htonl(hatStatBuf.st_mtime);
  n=write(f,&b,4);
  e=nl->first;
  while (e!=NULL) {
    b=htonl(e->fileoffset);
    write(f,&b,4);
    e=e->next;
  }
  close(f);
}

void findCAFs(NodeList* nl) {
  unsigned long currentOffset,srcref,fsz,lsz=0;
  char nodeType;
  int bigFileMode = 0;
  
  if (readCAFsFromFile(nl)==1) {
    return; // ok, read simply read them from disk!
  }

  fsz = filesize()/1000;
  if (fsz<20000) lsz=200;else {
    bigFileMode=1;
    fprintf(stderr,"Searching entire hat file for CAFs.\n");
    fprintf(stderr,"Please be patient, this might take a while...\n");
    fprintf(stderr,"  0%%");
    fflush(stderr); // force printing to screen, even though LF is still missing...
  }
  if (fsz==0) fsz=1;
    
  while (more()) {
    currentOffset = byteoffset();

    if ((bigFileMode)&&((currentOffset/10)/fsz>lsz)) {
      lsz = (currentOffset/10)/fsz;
      fprintf(stderr,"\b\b\b\b%3u%%",lsz);
      fflush(stderr);
    }

    nodeType = nextbyte();
    switch (nodeType) {
    case TRNAM: // Name
      if (readpointer()==0) { // is parent 0?
	skippointer();

	srcref = readpointer();
	if ((srcref!=0)&&(isSAT())) {  // SATC behind TRNAM?
	  // found a CAF!
	  if ((isSAT())&&(isTrusted(srcref)==0)&&
	      (isTopLevel(currentOffset))) {
	    addBeforeList(nl,currentOffset);
	  }
	}
      }
      seek(currentOffset+1+4*3);
      break;
    default:
      skipNode(nodeType);
      break;
    }
  }
  if (bigFileMode) {
    fprintf(stderr,"\nWriting CAFs to file for next session...\n");
  }
  writeCAFsToFile(nl);
  if (bigFileMode)  {
    fprintf(stderr,"\b\b\b\b");
    fflush(stderr);
  }
}

//#define DebugCheckAppsFor

int checkAppsFor(int *question,unsigned long parentTrace,unsigned long current,
		  unsigned long result) {
  char nodeType;
  unsigned long satc=0;
  int question_old=0;
  while (1) {
    if (current==0) return 0;

    result=findAppSAT(current); // no satc passed along

    seek(current);
    nodeType = nextbyte();
#ifdef DebugCheckAppsFor
    printf("nodeType at %u is %i, searching %u, resulting %u\n",current,nodeType,
	   parentTrace,result);
#endif
    switch (nodeType) {
    case TRAPP:
      {
	unsigned long srcref,p,funTrace,appTrace;
	int arity = readarity();
	appTrace = readpointer();         // fileoffset of App-trace
	funTrace = readpointer();         // skip function-trace
	skipbytes(4*(arity));
	srcref = readpointer(); // read srcref
	appTrace = followHidden(appTrace); // follow along hidden to find parent

	if ((appTrace!=parentTrace)&&(checkAppsFor(question,parentTrace,appTrace,result))) {
	  // check for bug in parent
	  return 1;
	} else {
	  if (appTrace==parentTrace) {
	    int i=0;
	    while (i++<arity) {
#ifdef DebugCheckAppsFor
	      printf("checking arg %i of %u\n",i,current);
#endif
	      seek(current+2+4+i*4);
	      p = readpointer();
	      if (checkAppsFor(question,parentTrace,p,p)==1) {
		return 1;
	      }
	    }
	  }
	}

	if ((appTrace==parentTrace)) {
#ifdef DebugCheckAppsFor
	  printf("APP at %u is child!\n",current);
#endif
	  satc=followSATs(result);
	  { 
	    int trusted = isTrusted(funTrace);
	    int toplevel = isTopLevel(funTrace);
	    int isOk=1;
	    if (trusted==0) {
#ifdef DebugCheckAppsFor
	      printf("Function at %u is not trusted.\n",funTrace);
	      printf("Toplevel: %i\n",toplevel);
#endif
	      if (satc==current) return 0;
	    }

	    question_old = *question;
	    while (1) {
		*question = question_old;
		if ((trusted==0)&&(toplevel)) {
		  isOk = askForApp(question,current,satc,0);
		  if (isOk==1) return 0;
		}
		if (checkAppsFor(question,current,satc,result)==0) {
		  // no bug in these children
		  if (isOk==2) {
		    *question = question_old;
		    isOk = askForApp(question,current,satc,1);
		  }
		  if (isOk==0) {
		    // user said, there is a bug! So it's here!
		    char s[2];
		    printf("\nBug located! (at %u)\nIn equation:\n",current);
		    showReduction(current,satc,1);
		    printf("in ");
		    showFunLocation(current);
		    printf("\nPress 'q' to quit, any other key to go back to question %i:",
			   question_old+1);
		    getline(s,2);
		    if ((s[0]=='q')||(s[0]=='Q')) quit();
		  } else return 0;
		} else { // ok, bug was found in one of its children
		  return 1;
		}
	      }
	  } // of if (isSAT())
	}
	seek(current+2+4+arity*4);
      }

      return 0;
    case TRNAM: {
      unsigned long p = readpointer();
      unsigned long newcurrent;
      if (p==parentTrace) return 0; // caf found
      else {
        newcurrent = followSATs(p);
	if (newcurrent==current) return 0;
	else current = newcurrent;
      }
    }
    break;
    case TRIND:
      current = readpointer();
      break;
    case TRHIDDEN:
    case TRSATA: // not evaluated expression
    case TRSATB:
      current = readpointer();
      break;
    default: {
	unsigned long newcurrent = followSATs(current);
	if (newcurrent==current) return 0;
	else current = newcurrent;
      }
    break;
    }
  }
  return 0;
}

void checkCAFs() {
  unsigned long satc;
  int question = 0,success=0,askAgain;
  char s[2];
  NodeElement* e;
  NodeList* results=newList();
  findCAFs(results);
  e=results->first;
  while ((success==0)&&(e!=NULL)) {
    int answer,question_old;
    askAgain = 0;
    question_old = question;
    answer=askForApp(&question,e->fileoffset,0,0);
    if (answer!=1) {
      satc=findAppSAT(e->fileoffset);
      if (checkAppsFor(&question,e->fileoffset,satc,satc)==0) {
	if (answer==2) {
	  question = question_old;
	  answer = askForApp(&question,e->fileoffset,0,1);
	}
	if (answer==0) {
	  printf("Error located!\nBug found in the CAF: \"");
	  showNode(e->fileoffset,1);
	  printf("\"\nin ");
	  showLocation(e->fileoffset);
	  printf("\n");
	  printf("\nPress 'q' to quit, any other key to go back to question %i:",
		 question_old);
	  getline(s,2);
	  if (toupper(s[0])=='Q') quit();
	  askAgain = 1;
	  question = question_old-1;
	}
      } else success=1;
    }
    if (!askAgain) e=e->next;
  }
  if (success==0) {
    printf("Ok, no bug found!\n");
  }
  freeList(results);
}
