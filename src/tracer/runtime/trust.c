#include "ui.h"

void
DisplayName(char *s, NodePtr nm)
{
    char *mod = NULL, *name;
    int defpos, pri;
    showSymbol(nm, &mod, &name, &defpos, &pri);
    fprintf(stderr, "(%s)%s.%s", s, mod, name);
}

#if 0
#define displayName(x, y) DisplayName(x, y)
#define displayString(s) fprintf(stderr, s)
#else
#define displayName(x, y)
#define displayString(s) 
#endif

int
trustFun(NodePtr t) 
{
   char *module, *name;
   int defpos;
   NodePtr nm;

   displayString("trustFun:\n");
   while (1) {
       int cnr;
       displayString("(loop)\n");       
       IND_REMOVE(t);
       switch (CONINFO_NUMBER(*t)) {
       case TagAp:
	   t = GET_POINTER_ARG1(t, 2);
	   IND_REMOVE(t);
	   t = GET_POINTER_ARG1(t, 1);
	   displayString("TagAp\n");
	   /*return FALSE;*/ /*  Wrong!!!!!!!! */
	   break;
       case TagNm:
	   displayString("TagNm\n");
	   nm = t;
	   t = GET_POINTER_ARG1(t, 2);
	   IND_REMOVE(t);
	   switch (CONINFO_NUMBER(*t)) {
	   case NTTrusted:
	       displayName("Trusted",t);
	       return TRUE;
	   case NTId:
	       displayName("Id", t);
	       return FALSE;
	   case NTLambda:
	       displayName("Lambda", t);
	       /*t = GET_POINTER_ARG1(t, 1);*/
	       return FALSE;
	       break;
	   case NTCase:
	       displayName("Case", t);
#if 1
	       t = GET_POINTER_ARG1(nm, 1);
#else
	       return TRUE;
#endif
	       break;
	   case NTIf:
	       displayName("If", t);
#if 1
	       t = GET_POINTER_ARG1(nm, 1);
#else
	       return TRUE;
#endif
	       break;
	   case NTGuard:
	       displayName("Guard", t);
#if 1
	       t = GET_POINTER_ARG1(nm, 1);
#else
	       return TRUE;
#endif
	       break;
	   case NTDummy:
	       displayName("Dummy", t);
	       return FALSE;
	   default:
	       fprintf(stderr, "(%d) ", CONINFO_NUMBER(*t));
	       fprintf(stderr, "!");
	       displayName("!", t);
	       break;
	   }
	   break;
       case TagSat:
	   displayString("TagSat\n");
	   t = GET_POINTER_ARG1(t, 1);
	   break;
       case TagInd:
	   displayString("TagInd\n");
	   t = GET_POINTER_ARG1(t, 2);
	   break;
       case TagRoot:
	   displayString("TagRoot\n");
	   /*fprintf(stderr, "<root> ");*/
	   return FALSE;
       case TagPruned:
	   displayString("TagPruned\n");
	   /* Not entirely sure this is correct */
	   return FALSE;
       case TagHidden:
	   displayString("TagHidden\n");
	   return TRUE;
       default:
	   fprintf(stderr, "trustFun: strange node, tag=%d (masked %d).\n", CONINFO_NUMBER(*t), CONINFO_NUMBER(*t) & ~MASK_K);
	   fprintf(stderr, "\nwrong-t = 0x%x ", t);
#ifdef TRACE
	   prGraph(t, 1, 1); fprintf(stderr, "\n");
#endif
	   exit(1);
       }
   }
}

C_HEADER(trustedName)
{
	NodePtr t = C_GETARG1(1);
	IND_REMOVE(t);
	switch (CONINFO_NUMBER(*t)) {
	case NTTrusted:
		C_RETURN(mkTrue());
	case NTId:
		C_RETURN(mkFalse());
	case NTLambda:
		C_RETURN(mkTrue());
	case NTCase:
		C_RETURN(mkTrue());
	case NTDummy:
		C_RETURN(mkFalse());
	default:
		fprintf(stderr, "trustedName: strange name tag %d.\n", CONINFO_NUMBER(*t));
		
		break;
	}
}

C_HEADER(trustedFun)
{
    NodePtr t = C_GETARG1(1);
    IND_REMOVE(t);
    dump(stderr, 5, t);
    C_RETURN(trustFun(C_GETARG1(1)) ? mkTrue() : mkFalse());
}

C_HEADER(trusted)
{
    int trust1, trust2;
    
    /*dbg_last_trace = C_GETARG1(2);*/

    displayString("F: ");
    trust2 = trustFun(C_GETARG1(2));
    /*reductions++;*/
    if (trust2) {
        displayString("   C: ");
	trust1 = trustFun(C_GETARG1(1));
        displayString("\n");
	if (trust1) {
	    redTT++;
	    C_RETURN(mkTrue());
	} else {
	    redST++;
	    C_RETURN(mkFalse());
	}
    } else {
        displayString("\n");
	    redSS++;
	    C_RETURN(mkFalse());
#if 0
	if (trust2) {
	    redST++;
	    C_RETURN(mkFalse());
	} else {
	    redSS++;
	    C_RETURN(mkFalse());
	}
#endif
    }
}

int
cTrusted(NodePtr t, NodePtr tf)
{
    return trustFun(tf) && trustFun(t);
}

C_HEADER(trust)
{
  NodePtr np = C_GETARG1(1);
  IND_REMOVE(np);
  if (trustFun(np)) {
	  /*fprintf(stderr, "Trusting something\n");*/
    C_RETURN(mkTrue());
  } else {
	  /*fprintf(stderr, "Not trusting something\n");*/
    C_RETURN(mkFalse());
  }
}

