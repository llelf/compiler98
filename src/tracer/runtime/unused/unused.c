typedef struct {
    char *module;
    int line_col;
} SourceRef;

char module[] = "MyModule";

SourceRef sourceRefs[] =
{{module, 120003}, {module, 120045}}; 

NodePtr mkSR(int srnr)
{
    NodePtr n = C_ALLOC(1+EXTRA+1);
    n[0] = CONSTR(42, 1, 1);
    INIT_PROFINFO(n, &dummyProfInfo)
    n[EXTRA+1] = (int)&sourceRefs[srnr];
    return n;
}

C_HEADER(cFakeSR)
{
    C_RETURN(mkSR(1));
}

#if 0
void dumpChain(FILE *sock, int clevel, int nlevel, NodePtr t) 
{
   int constr, tag, first = 1;
   NodePtr nt, n, ts;

   if (--clevel < 0) {
       ToDebugger(sock, "C ");
       return;
   }

   IND_REMOVE(t);
   constr = CONINFO_NUMBER(*t);
   switch (constr) {
   case TagAp:
       dump(sock, nlevel, t);
       dumpChain(sock, clevel, nlevel, GET_POINTER_ARG1(t,1));
       break;
   case TagNm:
       dump(sock, nlevel, t);
       dumpChain(sock, clevel, nlevel, GET_POINTER_ARG1(t,1));
       break;
   case TagInd:
       dumpChain(sock, clevel+1, nlevel, GET_POINTER_ARG1(t, 1));
       break;
   case TagRoot:
       break;
   case TagSat:
       n = shortCircuitSelectors(GET_POINTER_ARG1(t, 2));
       switch (checkEvaluation(n)) {
       case EVALUATED:
	   switch (CONINFO_NUMBER(*n)) {
	   case TagInd:
	       dump(sock, nlevel+1, n);
	       dumpChain(sock, clevel+1, nlevel, GET_POINTER_ARG1(t, 1));
	       break;
	   default:
	       dumpChain(sock, clevel+1, nlevel, n);
	       break;
	   }
	   break;
       case EVALUATING:
	   ToDebugger(sock, "B ");
	   break;
       case CLOSURE:
	   dumpChain(sock, clevel, nlevel+1, GET_POINTER_ARG1(t, 1));
	   break;
       }
       break;
   }
}
#endif

NodePtr nodeTrace(NodePtr t);

NodePtr pick(int n, NodePtr t)
{
    int constr;
    NodePtr np;

    while (1) {
	t = shortCircuitSelectors(t);
	constr = CONINFO_NUMBER(*t);
	/*fprintf(stderr, "pickloop %s %d\n", TAGSTR[constr], n);*/
	if (constr == TagSat) {
	    np = shortCircuitSelectors(GET_POINTER_ARG1(t, 2));
	    switch (checkEvaluation(np)) {
	    case EVALUATED:
		switch (CONINFO_NUMBER(*np)) {
		case TagInd:
		    if (n == 0)
			return GET_POINTER_ARG1(np, 1);
		    else {
			t = GET_POINTER_ARG1(t, 1);
			n--;
		    }
		    break;
		default:
		    t = np;
		    break;
		}
		break;
	    case EVALUATING:
		fprintf(stderr, "pick can not handle bottom yet.\n");
		exit(1);
	    case CLOSURE:
		t = GET_POINTER_ARG1(t, 1);
		break;
	    }	    
	} else if (n == 0)
	    return t;
	else {
	    switch (CONINFO_NUMBER(*t)) {
	    case TagAp:
	    case TagNm:
		t = GET_POINTER_ARG1(t, 1);
		n--;
		break;
	    case TagInd:
		t = GET_POINTER_ARG1(t, 1);
		break;
	    default:
		fprintf(stderr, "Strange node in pick\n");
		prGraph(t, 15, 15);
		exit(1);
	    }
	}
    }
}

NodePtr pickSubNode(int n, NodePtr t)
{
    NodePtr np;
    /*fprintf(stderr, "pickSubNode\n");*/
    t = shortCircuitSelectors(t);
    switch(CONINFO_NUMBER(*t)) {
    case TagAp:
	return listIndex(n, GET_POINTER_ARG1(t, 2));
	break;
    case TagSat:
	np = shortCircuitSelectors(GET_POINTER_ARG1(t, 2));
	switch (checkEvaluation(np)) {
	case EVALUATED:
	    switch (CONINFO_NUMBER(*np)) {
	    case TagInd:
		if (n == 0)
		    return GET_POINTER_ARG1(np, 1);
		else
		    return pickSubNode(n-1, GET_POINTER_ARG1(t, 1));
	    default:
		return pickSubNode(n, np);
	    }
	case EVALUATING:
	    fprintf(stderr, "pickSubNode can not handle bottom yet.\n");
	    exit(1);
	case CLOSURE:
	    return pickSubNode(n, GET_POINTER_ARG1(t, 1));
	}	    
	break;
    default:
	fprintf(stderr, "Strange node in pickSubNode\n");
	prGraph(t, 3, 3);
	exit(1);
    }
}

NodePtr pickFromChain(char *cs, NodePtr t, int traceFlag);

NodePtr pickNode(char *cs, NodePtr t)
{
    int n;
    char *s;
    NodePtr nt;
    /*fprintf(stderr, "pickNode <%s>\n", cs);*/
    t = shortCircuitSelectors(t);
    while (*cs == ' ')
	cs++;
    if (*cs == '0') 
	return t;
    if ((*cs >= '0') && (*cs <= '9')) {
	s = cs;
	while (*s && (*s != ' '))
	    s++;
	sscanf(cs, "%d", &n);
	cs = s;
	nt = pickSubNode(n-1, t);
	while (*cs == ' ')
	    cs++;
	if (*cs == 'X')
	    return nt;
	else
	    return pickNode(cs, nt);
    } else
	return pickFromChain(cs, t, TRUE);
}

NodePtr pickFromChain(char *cs, NodePtr t, int traceFlag)
{
    char ch;
    char *s;
    int n;
    
    /* fprintf(stderr, "pickFromChain <%s>\n", cs); */
    while (*cs == ' ') 
	cs++;
    if (*cs == 'D') {
	cs++;
	s = cs;
	while (*s && (*s != ' '))
	    s++;
	if (sscanf(cs, "%d", &n) != 1) {
	    fprintf(stderr, "Couldn't read number in pickFromChain: %s\n", cs);
	    exit(1);
	}
	t = pick(n, t);
	if (traceFlag)
	    return pickNode(s, nodeTrace(t));
	else
	    return pickNode(s, t);
    } else {
	fprintf(stderr, "Strange char in pickFromChain: %s\n", cs);
	exit(1);
    }
}

NodePtr nodeTrace(NodePtr t)
{
    NodePtr n;

    t = shortCircuitSelectors(t);
    switch (CONINFO_NUMBER(*t)) {
    case TagAp:
    case TagNm:
    case TagInd:
	return GET_POINTER_ARG1(t, 1);
    case TagSat:
	n = shortCircuitSelectors(GET_POINTER_ARG1(t, 2));
	switch (checkEvaluation(n)) {
	case EVALUATED:
	    switch (CONINFO_NUMBER(*n)) {
	    case TagInd:
		return GET_POINTER_ARG1(n, 1);
	    default:
		return nodeTrace(n);
	    }
	case EVALUATING:
	    fprintf(stderr, "nodeTrace: can not handle bottom yet.\n");
	    exit(1);
	case CLOSURE:
	    return GET_POINTER_ARG1(t, 1);
	}
    default:
	fprintf(stderr, "Strange node in nodeTrace\n");
	prGraph(t, 15, 15);
	exit(1);
    }    
}

