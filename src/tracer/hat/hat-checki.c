/* hat-interactive: nice tool for traversing the hat file structure
 */

/* #include <unistd.h> */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include "Expressions.h"
#include "hatinterface.h"
#include "hatgeneral.h"

/* Main driver and routines to providing basic interface to archive file.
 */

void interactive();
char* fname;
int verboseMode = 0;
unsigned int precision = 100;
HatFile handle;

int main (int argc, char *argv[])
{
  if (argc!=2) {
    fprintf(stderr,"\nusage: hat-checki file-name\n");
    exit(1);
  }
  fname = hatFileExtension(argv[1]);
  if ((handle=hatOpenFile(fname))==-1) {
    fprintf(stderr, "cannot open trace file %s\n\n",argv[1]);
    exit(1);
  }
  if (handle==-2) {
    fprintf(stderr, "format of file unknwon/not supported %s\n\n",fname);
    exit(1);
  }
  
  interactive(0);

  hatCloseFile(handle);

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

/*
void showOutput() {
  unsigned long p =  
    p <- readpointer bridge
    c <- readchar output
    putStr (prettyPrintChar c)
    outputForSamePointer output bridge p
    putStrLn ""
    return p  -- returns first pointer

outputForSamePointer :: Handle -> Handle -> Int -> IO()
outputForSamePointer output bridge p =
 do
   m <- more output
   if m == False then
     putStr ("\t -> "++(show p))
    else
      do
        p2 <- readpointer bridge
        c <- readchar output
        if (p == p2) then
           putStr (prettyPrintChar c) >>
           outputForSamePointer output bridge p
         else
           putStr ("\t -> "++(show p)++"\n"++(prettyPrintChar c)) >>
           outputForSamePointer output bridge p2

void checkOutput() {
  char str[255];
  int output,bridge;

  sprintf(str,"%s%s",fname,".output");
  output = open(str, 0);
  sprintf(str,"%s%s",fname,".bridge");
  bridge = open(str, 0);
  if (isEOF(output)) printf("Program had no output\n\n");
  else {
    printf("output text\t -> address\n");
    showOutput(output,bridge);
    printf("\n");
  }
  close(output);
  close(bridge);
}*/

int isCmd(char* s,char* s1,char* s2) {
  if (strcmp(s,s1)==0) return 1;
  return (strcmp(s,s2)==0);
}

#define FIXPRIMAX 8
char fixpribuf[FIXPRIMAX+1];

char *getfixpriStr() {
  switch (getInfixType()) {
  case HatINFIX:
    sprintf(fixpribuf, "infix %d", getInfixPrio());
    break;
  case HatINFIXR:
    sprintf(fixpribuf, "infixr %d", getInfixPrio());
    break;
  case HatINFIXL:
    sprintf(fixpribuf, "infixl %d", getInfixPrio());
    break;
  case HatNOINFIX:
    sprintf(fixpribuf, "");
    break;
  }
  return fixpribuf;
}

int lhi3(char b) {
  return (int)(b>>5);
}

int llo5(char b) {
  return (int)(b&037);
}

filepointer printNode(unsigned long offset) {
  char b,showAble=0;
  unsigned long next;
  b = getNodeType(handle,offset);
  printf("%i ",b);
  switch (lhi3(b)) {
  case TR:
    printf("TR 0x%x: ", offset);
    switch (llo5(b)) {
    case APP:
      { 
	int i=0;
	int arity = getAppArity();
	showAble = 1;
	printf("Application: ");
	printf("AppTrace 0x%x, ",getParent());
	printf("AppFun 0x%x, ",getAppFun());
	printf("Arguments [");
	while (i++<arity) {
	  printf("TR 0x%x",getAppArgument(i-1));
	  if (i<arity) printf(",");
	}
	printf("], SRCREF 0x%x",getSrcRef());
	break;
      }
    case NAM:
      showAble = 1;
      printf(" Name: ");
      printf("TR 0x%x, ", getParent());
      printf("NT 0x%x, ", getNameType());
      printf("SR 0x%x ", getSrcRef());
      break;
    case IND:
      printf(" Indirection: ");
      printf("TR 0x%x, ", getParent());
      printf("TR 0x%x ", getProjValue());
      break;
    case HIDDEN:
      showAble = 1;
      printf(" Hidden: ");
      printf("TR 0x%x", getParent());
      break;
    case SATA:
      showAble = 1;
      printf(" SAT(A): ");
      printf("TR 0x%x", getParent());
      break;
    case SATAIS:
      showAble = 1;
      printf(" isolated SAT(A): ");
      printf("TR 0x%x", getParent());
      break;
    case SATB:
      showAble = 1;
      printf(" SAT(B): ");
      printf("TR 0x%x\t", getParent());
      break;
    case SATBIS:
      showAble = 1;
      printf(" isolated SAT(B): ");
      printf("TR 0x%x\t", getParent());
      break;
    case SATC:
      showAble = 1;
      printf(" SAT(C): ");
      printf("TR 0x%x", getParent());
      break;
    case SATCIS:
      showAble = 1;
      printf(" isolated SAT(C): ");
      printf("TR 0x%x", getParent());
      break;
    default:
      printf("strange low-bits tag %d in TR 0x%x\n",
	     llo5(b), offset);
    }
    break;
  case MD:
    printf("MD 0x%x: ", offset);
    switch (llo5(b)) {
    case SUSPECT: printf("module (suspect), "); break;
    case TRUSTED: printf("module (trusted), "); break;
    default: printf("WRONG, "); break;
    }
    printf("%s, ", getName());
    printf("\"%s\"", getModuleSrcName());
    break;
  case NT:
    printf("NT 0x%x:  ", offset);
    switch (llo5(b)) {
    case INT:
      printf("INT %d", getIntValue());
      break;
    case CHAR:
      printf("CHAR '%c'", getCharValue());
      break;
    case INTEGER:
      printf("INTEGER %i", getIntegerValue());
      break;       
    case RATIONAL:
      printf("RATIONAL %s", getRationalValue());
      break;
    case FLOAT:
      printf("FLOAT %f", getFloatValue());
      break;
    case DOUBLE:
      printf("DOUBLE %f", getDoubleValue());
      break;
    case IDENTIFIER:
    case TOPIDENTIFIER:
      printf("identifier: %s ", getName());
      {
	unsigned long modinfo = getModInfo();
	char *fp = getfixpriStr();
	if (*fp!='\0') printf("%s ", fp);
	printf("MD 0x%x, ", modinfo);
	printf(" %s", getPosnStr());
      }
      break; 
    case CONSTRUCTOR:
      printf("constructor\t\t%s ", getName());
      { 
	unsigned long modinfo = getModInfo();
	char *fp = getfixpriStr();
	if (*fp!='\0') printf("%s ", fp);
	printf("MD 0x%x, ", modinfo);
	printf(" %s", getPosnStr());
      }
      break; 
    case TUPLE:
      printf("TUPLE");
      break;
    case FUN:
      printf("FUN");
      break;
    case CASE:
      printf("CASE");
      break;
    case LAMBDA:
      printf("LAMBDA");
      break;
    case DUMMY:
      printf("DUMMY");
      break;
    case CSTRING:
      printf("CSTRING \"%s\"", getName());
      break;
    case IF:
      printf("IF");
      break;
    case GUARD:
      printf("GUARD");
      break;
    case CONTAINER:
      printf("CONTAINER");
      break;
    default:
      printf("strange low-bits tag %d in NT 0x%x\n",
	     llo5(b), offset);
    }
    break;
  case SR:
    printf("SR 0x%x:  Source reference\t", offset);
    printf("MD 0x%x\t", getModInfo());
    printf(" %s", getPosnStr());
    break;
  default:
    printf("strange high-bits tag %d at byte offset 0x%x\n",
	   lhi3(b), offset);
  }
  printf("\n");
  next = hatSeqNext(handle,offset);

  if (showAble) {
    filepointer satc=0;
    char *appstr;
    filepointer lmo;
    ExprNode* exp;
    char xx;

    exp = buildExpr(handle,offset,verboseMode,precision);
    appstr = prettyPrintExpr(exp,precision,1);
    
    lmo = hatLMO(handle,offset);
    if ((lmo!=0) && ((xx=getNodeType(handle,lmo))==HatIdentifier
                     || xx==HatTopIdentifier)) {
     satc = getResult(handle,hatFollowSATs(handle,offset));
     if ((isSAT(handle,satc))&&(satc!=offset)) {
       printf("corresponding SAT at: 0x%x\n\n",satc);
     }
    }
    if (satc) {
      printf("reduction: %s = ",appstr);
      freeStr(appstr);
      exp = buildExpr(handle,satc,verboseMode,precision);
      appstr = prettyPrintExpr(exp,precision,1);
      printf("%s\n",appstr);
    } else {
      printf("\nredex: %s\n",appstr);
    }
    freeStr(appstr);
  }
  return next;
}

void interactive(filepointer current) {
  char command[255];
  int toplevel=0;
  unsigned long adr,next;

  toplevel = (current==0);
  if (current == 0) current = hatNodeNumber(handle);
  while (1) {
    next = 0;
    if (current!=0) next=printNode(current);
    printf("\nenter <address>, (b)ack, (n)ext=0x%x or (f)ollow SATs, (q)uit: ",next);
    getline(command,80);
    if (isCmd(command,"back","b")) {
      if (toplevel) printf("Sorry. Already at beginning. Can't go back!\n");
      else return;
    }  else
      if (isCmd(command,"quit","q")) {
	printf("Ok. Goodbye. Thank you for using hat-check-interactive! :)\n\n");
	exit(0);
      } else {
	if (isCmd(command,"next","n")) {
	  if (next!=0) {
	    interactive(next);
	  }
	} else {
	  if (isCmd(command,"follow","f")) {
	    adr = hatFollowSATs(handle,current);
	  } else {
	    adr = atol(command);
	    if (adr==0) {
	      sscanf(command, "0x%x", &adr);
	    }
	  }
	  if (adr!=0) {
	    if (current==0) current=adr;else
	      interactive(adr);
	  } else {
	    if (isCmd(command,"verbose","v")) {
	      verboseMode=1-verboseMode;
	      printf("verbose mode is now ");
	      if (verboseMode) printf("ON\n"); else printf("OFF\n");
	    } else
	      printf("Please enter 'b','n','q' or an address!\n");
	  }
	}
      }	
  }
}
