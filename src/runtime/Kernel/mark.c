#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#if !defined(__arm) && !defined(__hppa__) && !defined(__CYGWIN32__)
#include <malloc.h>
#endif
#include "node.h"
/* #include "newmacros.h"  -- already included in node.h */
/* #include "runtime.h"    -- already included in node.h */
/* #include "bytecode.h"   /* Need value for GA1EVAL */
/*                         -- already included in node.h via newmacros.h */
#include "mark.h"
#include "mutlib.h"

GcConst oldCaf = GcEnd;
GcConst newCaf = GcEnd;

void addCaf2(UInt cons)
{
  GcConst cptr = (GcConst)cons;
  if(!cptr->next) {
    cptr->next = newCaf;
    newCaf = cptr;
  }
}

void addCaf(Finfo finfo)
{
  addCaf2((UInt)FINFO_CONST(finfo));
}


UInt marked(NodePtr node)   /* True if marked, false otherwise,  mark the node */
{
  UInt off = node - hpLowLimit;
  UInt mask = 1l << (off & WORDMASK);

  off >>= WORDSHIFT;
  if(bitTable[off] & mask)
    return 1;
  bitTable[off] |= mask;
  return 0;
}


UInt ifmarked(NodePtr node)   /* True if marked, false otherwise */
{
  UInt off = node - hpLowLimit;
  UInt mask = 1l << (off & WORDMASK);
  off >>= WORDSHIFT;
  if(bitTable[off] & mask)
    return 1;
  return 0;
}

void unmarked(NodePtr node)   /* Clear mark for node */
{
  UInt off = node - hpLowLimit;
  UInt mask = 1l << (off & WORDMASK);
  off >>= WORDSHIFT;
  bitTable[off] &= ~mask;
}

void markClear(void)
{
  NodePtr p;
  for(p = bitTable; p < hpEnd; *p++ = 0)
    ;
  hpEnd[-1] = ~0;
  marked(hpLowLimit);
}

#ifdef PROFILE
static Retainer *profileRetainer;
static int maxRetainerStack;
static int posRetainerStack;
static Retainer **RetainerStack;

#define STARTRETAINERSTACK 256

void pushRetainerStack(Retainer *retainer,NodePtr node)
{
  if(posRetainerStack >= maxRetainerStack) {
    if(RetainerStack) {
      maxRetainerStack *=2;
      RetainerStack = realloc(RetainerStack,sizeof(Retainer *) * maxRetainerStack);
    } else {
      maxRetainerStack = STARTRETAINERSTACK;
      RetainerStack = malloc(sizeof(Retainer *) * maxRetainerStack);
    }
    if(!RetainerStack) {
      fprintf(stderr,"pushRetainerStack run out of memory!\n");
      exit(-1);
    }
  }
  RetainerStack[posRetainerStack++] = retainer;
}

Retainer *popRetainerStack(NodePtr node)
{
  Retainer *retainer = profileRetainer;
  if(posRetainerStack) {
    retainer = RetainerStack[--posRetainerStack];
  } else
    fprintf(stderr,"popRetainerStack on empty stack ignored!\n");

  return retainer;
}

#endif

NodePtr ind_remove(NodePtr np)
{
    NodePtr op;
    int n = 0;

    while ((*np & 0x2) == 0) {
	n++;
	op = np;
	np = (NodePtr)*np;
	if (np == NULL) {
	    fprintf(stderr, "Hmm np = NULL, op = 0x%x after %d loops\n",
		    op, n);
	    return np;
	    /* **np;*/
	}
    }
    return np;
}

#ifdef DBGTRANS
NodePtr shortCircuitSelectors(NodePtr node);
#else
#define shortCircuitSelectors(x)	x
#endif

#ifdef DBGTRANS
extern int traceShow;
int sats_count, sats_pruned;

#define INF_AGE 0x3f

#define CONINFO_DIST(p) 	(((p)>>8)&INF_AGE)
#define CONINFO_SHIFT_DIST(k)	(k<<8)
#define MASK_K 			(INF_AGE<<8)
#define CONINFO_SET_DIST(n, k) n = (n & ~MASK_K) | CONINFO_SHIFT_DIST(k)

#define MASK_TNUMBER 		(0xf)

void pruneSATs(NodePtr nodeptr)
{
    Cinfo cinfo;
    Coninfo coninfo;
    Int i, size, psize;

/*    if (!OUTSIDE(nodeptr) && !marked(nodeptr)) {*/
	switch (GET_LARGETAG(nodeptr)) {
	case CON_DATA|VAP_TAG0: 
	case CON_PTRS|VAP_TAG0: 
	case CON_CDATA|VAP_TAG0: 
	case CON_WORDS|VAP_TAG0:
	case CON_DATA|VAP_TAG1: 
	case CON_PTRS|VAP_TAG1: 
	case CON_CDATA|VAP_TAG1: 
	case CON_WORDS|VAP_TAG1:
	    if (!OUTSIDE(nodeptr) && !marked(nodeptr)) {
		cinfo = GET_CINFO(nodeptr);
		size  = CINFO_SIZE(cinfo);
		for(i = 0; i < size; i++) {
		    pruneSATs(GET_POINTER_ARG1(nodeptr, i+1));
		}
	    }
	    break;
	case CON_DATA|CON_TAG:
#define Sat 4
	case CON_CDATA|CON_TAG:
	    coninfo = GET_CONINFO(nodeptr);
	    size  = CONINFO_SIZE(coninfo);
	    psize = CONINFO_PSIZE(coninfo);
	    if (CONINFO_IS_TRACE(coninfo) && 
		((CONINFO_NUMBER(coninfo) & MASK_TNUMBER) == Sat)) {
		/*NodePtr np = ind_remove((NodePtr)nodeptr[EXTRA+2]);*/
		NodePtr np = shortCircuitSelectors((NodePtr)nodeptr[EXTRA+2]);
		sats_count++;
		if (GET_TAG(np) & VAP_TAG) {
		    pruneSATs((NodePtr)nodeptr[EXTRA+1]);
		    pruneSATs((NodePtr)nodeptr[EXTRA+2]);
		} else { 
		    /* The second part is evaluated, short-cirtuit the SAT */
		    nodeptr[0] = (Node)np;
		    sats_pruned++;
		    pruneSATs(np);
		}
	    } else {
		if (!OUTSIDE(nodeptr) && !marked(nodeptr)) {
		    for(i = 0; i < psize; i++) {
			pruneSATs((NodePtr)nodeptr[EXTRA+i+1]);
		    }
		}
	    }
	    break;
	case CON_PTRS|CON_TAG:
	    if (!OUTSIDE(nodeptr) && !marked(nodeptr)) {
		coninfo = GET_CONINFO(nodeptr);
		psize  = CONINFO_LARGESIZES(coninfo);
		for(i = 0; i < psize; i++) {
		    pruneSATs((NodePtr)GET_POINTER_ARG1(nodeptr, i+1));
		}
		/* Ignore pointer-less args */
	    }
	    break;
	case CON_WORDS|CON_TAG:
	    /* ignore pointer-less constructors */
	    break;
	default: /*  IND_TAG: */
	    pruneSATs(GET_IND_ADDRESS(nodeptr));
	    break;
	}
/*    }*/
}

void check_eval(char *s, NodePtr nodeptr)
{
    if (traceShow)
	switch (GET_LARGETAG(nodeptr)) {
	case CON_DATA|VAP_TAG0: 
	case CON_PTRS|VAP_TAG0: 
	case CON_CDATA|VAP_TAG0: 
	case CON_WORDS|VAP_TAG0:
	case CON_DATA|VAP_TAG1: 
	case CON_PTRS|VAP_TAG1: 
	case CON_CDATA|VAP_TAG1: 
	case CON_WORDS|VAP_TAG1:
	    fprintf(stderr, "%s", s);
	}
}

void pruneNode(int k, NodePtr nodeptr);

void pruneTrace(int k, NodePtr nodeptr)
{
    extern Node pruned[];
    extern int traceNoSat;
    extern int traceNoSatF;
    extern int traceShow;
    extern int traceStat;
    extern int traceK;
    extern int tAp, tNm, tInd, tRoot, tSat, tPruned, tHidden, tUnknown;
    int old_k;
    NodePtr np;
    Coninfo coninfo;

    /*fprintf(stderr, "pruneTrace: tag=0x%x\n", GET_LARGETAG(nodeptr));*/
    if (!(GET_LARGETAG(nodeptr) == (CON_DATA|CON_TAG))) {
/*	fprintf(stderr, "nodeptr = 0x%x(0x%x)\n", nodeptr, *nodeptr);*/
/*	fprintf(stderr, "!");*/
/*	prGraph(nodeptr, 3, 3);*/
/*	fprintf(stderr, "\n");*/
/*	pruneNode(k, nodeptr);*/
/*	exit(1);*/
	return;
    }
 
    /* Have we been here before with "longer time to live"? */
    old_k = CONINFO_DIST(GET_CONINFO(nodeptr));
    /*fprintf(stderr, "old_k = %d  k = %d\n", old_k, k);*/
    if (old_k >= k) 
	return;
    
    switch (CONINFO_NUMBER(GET_CONINFO(nodeptr)) & MASK_TNUMBER) {
    case 0: /* Application */
	if (traceStat) fprintf(stderr, "A"); 
        
	CONINFO_SET_DIST(nodeptr[0], k);
	if (k > 0) {
	    NodePtr list;
	    np = (NodePtr)nodeptr[EXTRA+1];
	    /*np = ind_remove(np);*/
	    np = shortCircuitSelectors(np);
	    check_eval("Ap(f)", np);
	    pruneTrace(k-1, np);
#if 1
	    list = ind_remove((NodePtr)nodeptr[EXTRA+2]);
	    while (CONINFO_PSIZE(*list) == 2) {
		/*np = ind_remove((NodePtr)list[EXTRA+1]);*/
		np = shortCircuitSelectors((NodePtr)list[EXTRA+1]);
		check_eval("Ap(arg)", np);
		pruneTrace(k-1, np);
		list = ind_remove((NodePtr)list[EXTRA+2]);
	    }
	    if (CONINFO_PSIZE(*list) != 0) {
		fprintf(stderr, "Hmm. List element not cons or nil... (%d)\n",
			CONINFO_PSIZE(*list));
	    }
#endif
	}
	tAp++;
	break;
    case 1: /* Name */
	if (traceStat) fprintf(stderr, "N"); 
	/* Never prune a name */
	CONINFO_SET_DIST(nodeptr[0], INF_AGE);
	np = (NodePtr)nodeptr[EXTRA+1];
	/*np = ind_remove(np);*/
	np = shortCircuitSelectors(np);
       	check_eval("Name", np);
	pruneTrace(k, np);
	tNm++;
	break;
    case 2: /* Indirection */
	if (traceStat) fprintf(stderr, "I"); 
	CONINFO_SET_DIST(nodeptr[0], k);
 	np = (NodePtr)nodeptr[EXTRA+2];
 	np = shortCircuitSelectors(np);/**/
 	pruneTrace(k, np);
 	np = (NodePtr)nodeptr[EXTRA+1];
 	np = shortCircuitSelectors(np);
 	pruneTrace(k, np);	
	tInd++;
	break;
    case 3: /* Root */
	if (traceStat) fprintf(stderr, "O"); 
	CONINFO_SET_DIST(nodeptr[0], k);
	/* No need to do anything here */
	tRoot++;
	break;
    case 4: /* Sat */
	if (traceStat) fprintf(stderr, "S"); 
	CONINFO_SET_DIST(nodeptr[0], k);
	if (k > 0) {
	    np = (NodePtr)nodeptr[EXTRA+2];
	    /*np = ind_remove(np);*/
	    np = shortCircuitSelectors(np);
	    if (!(GET_TAG(np) & VAP_TAG)) {  /*CONINFO_IS_TRACE(*np)) */
		/* Overwrite the node with an indirection to the first trace */
		/* i.e., short-circuit the Sat node */
		np = (NodePtr)nodeptr[EXTRA+2];
		np = shortCircuitSelectors(np);
		nodeptr[0] = (Node)np;
		check_eval("Sat(l1)", np);
		pruneTrace(k, np);
	    } else {
		/* Have to prune the vap ???*/
		pruneNode(traceK, np);
		np = (NodePtr)nodeptr[EXTRA+1];
		np = shortCircuitSelectors(np);
		check_eval("Sat(l2)", np);
		pruneTrace(k, np);
	    }
	}
	tSat++;
	break;
    case 5: /* Pruned */
	if (traceStat) fprintf(stderr, "P"); 
	CONINFO_SET_DIST(nodeptr[0], k);
	tPruned++;
	break;
     case 6: /* Hidden */
	if (traceStat) fprintf(stderr, "H"); 
	CONINFO_SET_DIST(nodeptr[0], k);
	np = (NodePtr)nodeptr[EXTRA+1];
 	np = shortCircuitSelectors(np);
 	pruneTrace(k, np);
 	tHidden++;
 	break;
    default: /* Unknown (should not happen) */
	if (traceShow) fprintf(stderr, "?"); 
	fprintf(stderr, "Unknown trace node: node = 0x%x, tag = %d\n", 
		nodeptr, CONINFO_NUMBER(GET_CONINFO(nodeptr)));
	tUnknown++;
	break;
    }
}

int tnodes = 0;

void pruneNode(int k, NodePtr nodeptr)
{
    Cinfo cinfo;
    Coninfo coninfo;
    Int i, size, psize;
    extern int traceK, tR;

    /*if (!OUTSIDE(nodeptr) && !marked(nodeptr)) {*/
	switch (GET_LARGETAG(nodeptr)) {
	case CON_DATA|VAP_TAG0: 
	case CON_PTRS|VAP_TAG0: 
	case CON_CDATA|VAP_TAG0: 
	case CON_WORDS|VAP_TAG0:
	case CON_DATA|VAP_TAG1: 
	case CON_PTRS|VAP_TAG1: 
	case CON_CDATA|VAP_TAG1: 
	case CON_WORDS|VAP_TAG1:
	    if (!OUTSIDE(nodeptr) && !marked(nodeptr)) {
		cinfo = GET_CINFO(nodeptr);
		size  = CINFO_SIZE(cinfo);
		for(i = 0; i < size; i++) {
		    pruneNode(k, GET_POINTER_ARG1(nodeptr, i+1));
		}
	    }
	    break;
	case CON_DATA|CON_TAG:
	case CON_CDATA|CON_TAG:
	    coninfo = GET_CONINFO(nodeptr);
	    size  = CONINFO_SIZE(coninfo);
	    psize = CONINFO_PSIZE(coninfo);
	    if (CONINFO_IS_R(coninfo)) {
		/*NodePtr np = ind_remove((NodePtr)nodeptr[EXTRA+2]);*/
		NodePtr np = shortCircuitSelectors((NodePtr)nodeptr[EXTRA+2]);
		pruneTrace(k, np);
		pruneNode(k, (NodePtr)nodeptr[EXTRA+1]);
		tR++;
		tnodes++;
	    } else if (CONINFO_IS_TRACE(coninfo)) {
		pruneTrace(k, nodeptr);
	    } else {
		if (!OUTSIDE(nodeptr) && !marked(nodeptr)) {
		    for(i = 0; i < psize; i++) {
			pruneNode(k, (NodePtr)nodeptr[EXTRA+i+1]);
		    }
		}
	    }
	    break;
	case CON_PTRS|CON_TAG:
	    if (!OUTSIDE(nodeptr) && !marked(nodeptr)) {
		coninfo = GET_CONINFO(nodeptr);
		psize  = CONINFO_LARGESIZES(coninfo);
		for(i = 0; i < psize; i++) {
		    pruneNode(k, (NodePtr)GET_POINTER_ARG1(nodeptr, i+1));
		}
		/* Ignore pointer-less args */
	    }
	    break;
	case CON_WORDS|CON_TAG:
	    /* ignore pointer-less constructors */
	    break;
	default: /*  IND_TAG: */
	    pruneNode(k, GET_IND_ADDRESS(nodeptr));
	    break;
	}
/*    }*/
}

void prunePrunedNode(NodePtr nodeptr)
{
    Cinfo cinfo;
    Coninfo coninfo;
    Int i, size, psize;
    extern int traceK;

    if (!OUTSIDE(nodeptr) && !marked(nodeptr) || OUTSIDE(nodeptr)) {
	switch (GET_LARGETAG(nodeptr)) {
	case CON_DATA|VAP_TAG0: 
	case CON_PTRS|VAP_TAG0: 
	case CON_CDATA|VAP_TAG0: 
	case CON_WORDS|VAP_TAG0:
	case CON_DATA|VAP_TAG1: 
	case CON_PTRS|VAP_TAG1: 
	case CON_CDATA|VAP_TAG1: 
	case CON_WORDS|VAP_TAG1:
	    cinfo = GET_CINFO(nodeptr);
	    size  = CINFO_SIZE(cinfo);
	    for(i = 0; i < size; i++) {
		prunePrunedNode(GET_POINTER_ARG1(nodeptr, i+1));
	    }
	    break;
	case CON_DATA|CON_TAG:
	case CON_CDATA|CON_TAG:
	    coninfo = GET_CONINFO(nodeptr);
	    size  = CONINFO_SIZE(coninfo);
	    psize = CONINFO_PSIZE(coninfo);
	    if (CONINFO_IS_TRACE(coninfo)) {
		extern Node pruned[];
		extern int traceNoR;

		if (traceNoR) fprintf(stderr, "k = %d\n", CONINFO_DIST(coninfo));
		if(!CONINFO_DIST(coninfo)) {
		    extern int nPruned;
		    Int i;
		    /*fprintf(stderr, "  pruned constr: %d\n", CONINFO_NUMBER(coninfo));*/
		    for (i = 0; i <= EXTRA; i++)
			nodeptr[i] = (Node)pruned[i];
		    nPruned++;
		} else {
		    nodeptr[0] &= ~MASK_K; 
		}
	    }
	    for(i = 0; i < psize; i++) {
		prunePrunedNode((NodePtr)GET_POINTER_ARG1(nodeptr, i+1));
	    }
	    break;
	case CON_PTRS|CON_TAG:
	    coninfo = GET_CONINFO(nodeptr);
	    psize  = CONINFO_LARGESIZES(coninfo);
	    for(i = 0; i < psize; i++) {
		prunePrunedNode((NodePtr)GET_POINTER_ARG1(nodeptr, i+1));
	    }
	    /* Ignore pointer-less args */
	    break;
	case CON_WORDS|CON_TAG:
	    /* ignore pointer-less constructors */
	    break;
	default: /*  IND_TAG: */
	    prunePrunedNode(GET_IND_ADDRESS(nodeptr));
	    break;
	}
    }
}

void pruneOne(NodePtr np)
{
    extern int traceK, tracePruneSATs;
    if (tracePruneSATs > 0)
 	pruneSATs(np);
    else
 	pruneNode(traceK, np);
}
 
void pruneStackAndOutput(NodePtr *sp)
{
#if 0
  NodePtr *sptr;
  extern void otMap(void (*f)(NodePtr));

  for(sptr = sp; sptr < spStart; ) {
    NodePtr *fp;
    sptr++;                /* skip ip */
    fp = (NodePtr *)*sptr++; /* Fetch fp */

    while(sptr != fp) {
 	pruneOne(*sptr);
	sptr++;
    }
  }
  otMap(pruneOne);
#endif
}

void prunePrunedStackAndOutput(NodePtr *sp)
{
#if 0
  NodePtr *sptr;
  extern void otMap(void (*f)(NodePtr));

  for(sptr = sp; sptr < spStart; ) {
    NodePtr *fp;
    sptr++;                /* skip ip */
    fp = (NodePtr *)*sptr++; /* Fetch fp */

    while(sptr != fp) {
      prunePrunedNode(*sptr);
      sptr++;
    }
  }
  otMap(prunePrunedNode);
#endif
}

void pruneCafs()
{
  extern int traceK, tracePruneSATs;

  while(newCaf != GcEnd) {
      GcConst cptr = newCaf;
      Int size = cptr->sizeArity;
      Int i,arity = SNDHW(size);
      size = FSTHW(size);

      newCaf = cptr->next;
      cptr->next = oldCaf;
      oldCaf = cptr;

      {
	  NodePtr *nptr = &cptr->ptr[0];
	  if(GET_TAG(nptr) == IND_TAG)
	      if (tracePruneSATs > 0)
		  pruneSATs(*nptr);
	      else
		  pruneNode(traceK, *nptr);
	  i = EXTRA+1;
	  size += EXTRA+1;
      }

      for(; i<size; i++) {
	  NodePtr nptr = cptr->ptr[i];
	  switch(EXT_TAG(nptr)) {
	  case VAP_TAG0: 
	  case VAP_TAG1:
	  { Cinfo cinfo = EXT_CINFO(nptr);
	  addCaf(CINFO_FINFO(cinfo));
	  } break;
	  case CON_TAG:
	      fprintf(stderr,"CON_TAG in pruneCafs(1) cptr = %8lx sizeArity = %08x i = %2d %8lx:%08lx\n",(UInt)cptr,cptr->sizeArity,i,(UInt)&cptr->ptr[i],(UInt)nptr);
	      exit(-1);
	  case IND_TAG:
	      if (tracePruneSATs > 0)
		  pruneSATs(cptr->ptr[i]);
	      else
		  pruneNode(traceK, cptr->ptr[i]);
	      break;
	  }
      }
  }
}

void prunePrunedCafs()
{
  while(newCaf != GcEnd) {
      GcConst cptr = newCaf;
      Int size = cptr->sizeArity;
      Int i,arity = SNDHW(size);
      size = FSTHW(size);

      newCaf = cptr->next;
      cptr->next = oldCaf;
      oldCaf = cptr;

      {
	  NodePtr *nptr = &cptr->ptr[0];
	  if(GET_TAG(nptr) == IND_TAG)
	      prunePrunedNode(*nptr);
	  i = EXTRA+1;
	  size += EXTRA+1;
      }

      for(; i<size; i++) {
	  NodePtr nptr = cptr->ptr[i];
	  switch(EXT_TAG(nptr)) {
	  case VAP_TAG0: 
	  case VAP_TAG1:
	  { Cinfo cinfo = EXT_CINFO(nptr);
	  addCaf(CINFO_FINFO(cinfo));
	  } break;
	  case CON_TAG:
	      fprintf(stderr,"CON_TAG in pruneCafs(1) cptr = %8lx sizeArity = %08x i = %2d %8lx:%08lx\n",(UInt)cptr,cptr->sizeArity,i,(UInt)&cptr->ptr[i],(UInt)nptr);
	      exit(-1);
	  case IND_TAG:
	      prunePrunedNode(cptr->ptr[i]);
	      break;
	  }
      }
  }
}

tracePrune(NodePtr *sp)
{
    extern int traceK, tracePruneSATs;
    
    tnodes = 0;
    markClear();
    if (tracePruneSATs > 0)
	sats_count = sats_pruned = 0;
    pruneStackAndOutput(sp);
    pruneCafs();
    if (traceK >= 0) {
	markClear();
	prunePrunedStackAndOutput(sp);
	prunePrunedCafs();
    } else {
	if (traceShow) {
	    fprintf(stderr, "\n%d SATs, %d pruned\n", sats_count, sats_pruned);
	}
    }
}

#if 0
      if (CONINFO_IS_R(*node)) {
	  extern Node pruned[];
	  extern int traceNoR;
	  extern int traceShow;
	  extern int tR;
	  extern int traceK;

	  if (traceShow) fprintf(stderr, "R");
	  tR++;
	  if (traceK >= 0) {
	      if (traceK == 0) {
		  node[EXTRA+2] = (Node)&pruned[0];
	      } else {
		  NodePtr np = (NodePtr)node[EXTRA+2];
		  np = ind_remove(np);
		  if (np == NULL) {
		      node[EXTRA+2] = (Node)&pruned[0];
		  } else {
		      /*np = (Node)pruneTrace(traceK-1, np);*/
		      /*node[EXTRA+2] = (Node)pruneTrace(traceK-1, np);*/
		  }
	      }
	  }
      } else if (CONINFO_IS_TRACE(*node)) {
	  extern int traceK;
	  /*if (traceK >= 0)
	      pruneTrace(traceK, node);*/
      } else {
	  extern int traceShow;

	  if (traceShow) 
	      fprintf(stderr, ".");
      }
#endif
#endif

extern NodePtr prevLow,prevHigh;
Int debug = 0;

NodePtr mark(NodePtr *inode)
{
  NodePtr node = *inode;
  NodePtr pptr = &hpLowLimit[1];
  NodePtr newpptr;

  hpLowLimit[1] = 0;

  EDB(if(debug) {fprintf(stderr,"\nmark %lx:",(UInt)node); fflush(stderr);})
  Q(node,"mark")
  SQ("mark")

  WHEN_DYNAMIC(posRetainerStack = 0;)

 InspectNode:
  EDB(if(debug) {fprintf(stderr,"Inspect %lx:",(UInt)node); fflush(stderr);})
  if(node > hpEnd) {
    fprintf(stderr,"InspectNode %lx > hpEnd %lx\n",(UInt)node,(UInt)hpEnd);
    exit(-1);
  }
  { UInt tag;
    IND_REMOVE(node);
    tag = EXT_LARGETAG(*node);
    if(OUTSIDE(node)) {
      if(tag & VAP_TAG) {
	Cinfo cinfo;
	EDB(if(debug) {fprintf(stderr,"VAP(0):"); fflush(stderr);})
	  cinfo = GET_CINFO(node);
	addCaf(CINFO_FINFO(cinfo));
      }
      goto NextNode;
    }


#ifdef DYNAMIC
    if (pactive && useUnique && GET_INFO(node)->unique == 0) {
      GET_INFO(node)->unique = ++unique;
    }
#endif

    if(marked(node)) {
#ifdef DYNAMIC
      if(pactive && ((profile | filter) & PROFILE_RETAINER)
         && !memberAdr(profileRetainer->keep,profileRetainer->member[0],(Retainer *)GET_INFO(node)->rinfo)) {
        pushRemarkStack(profileRetainer->keep,profileRetainer->member[0],node,0);
      }
#endif
      goto NextNode;
    }
    WHEN_DYNAMIC(GET_INFO(node)->rinfo = profileRetainer;)
    Q(node,"inspect")
    switch(tag) {
    case CON_DATA|VAP_TAG0: case CON_PTRS|VAP_TAG0: case CON_CDATA|VAP_TAG0: case CON_WORDS|VAP_TAG0:
    case CON_DATA|VAP_TAG1: case CON_PTRS|VAP_TAG1: case CON_CDATA|VAP_TAG1: case CON_WORDS|VAP_TAG1:
      EDB(if(debug) {fprintf(stderr,"VAP/CAP:"); fflush(stderr);})
      { Cinfo cinfo = GET_CINFO(node);
	Int size = (Int)CINFO_SIZE(cinfo);
        Finfo finfo = CINFO_FINFO(cinfo);

/* !!! SPACE FOR WADLER HERE !!! */

#if 1
/*
 * Check if it is a selector with an evaluated argument.
 * Note : The constructor might have its pointer reversed already.
 *           Don't fix selector, no space savings are possible in any case.
 *        Selector on Selecor on Constructor should be optimised.
 *           Do Pointer reversing down the list as long as all applicatins are selectors
 *           and no application is marked. If we find a constructor then fix all applications
 *           otherwise just restore everything.
 *        All marked applications on the way down must be unmarked on the way up otherwise garbage
 *        might survive.
 *
 * 0: NEEDSTACK_I16
 * 1: SELECTOR_EVAL
 * 2: SELECT pos
 * 4: RETURN_EVAL
 *
 */

#define SELECTOR_INS  1
#define SELECTOR_ARG  3

#if 1
#define WHEN_WADLER(x)
#else
#define WHEN_WADLER(x) x
#endif

	if(!ZAPPED(node) &&                                       /* Don't do wadler on things under evaluation */
	   !CINFO_NEED(cinfo) &&                                  /* All arguments must be available, ie one */
	   (FINFO_CODE(finfo))[SELECTOR_INS] == SELECTOR_EVAL) {  /* and it must be a selector */
	  NodePtr app = node;
	  NodePtr arg = GET_POINTER_ARG1(node,1);
	  IND_REMOVE(arg);
	  WHEN_WADLER(fprintf(stderr,"START  node  %08lx:%08lx %08lx\n",node,node[0],node[EXTRA+1]);)
	  WHEN_WADLER(fprintf(stderr,"WADLER 1     %08lx %08lx\n",app,arg);)
	  while((GET_TAG(arg) & VAP_TAG) &&     /* An application ... */
		!ZAPPED(arg) &&                 /* not under evaluation ... */
		((VAP_CODE(arg))[SELECTOR_INS] == SELECTOR_EVAL)) {  /* ... that is a selector */
	    if(INSIDE(arg) && !ifmarked(arg)) { /* ... and it wasn't marked */
	      NodePtr tmp = (NodePtr)arg[EXTRA+1];      /* Pointer reversal and one step down */
	      WHEN_WADLER(fprintf(stderr,"WADLER   2    %08lx %08lx\n",app,arg);)
	      marked(arg);
	      arg[EXTRA+1]=(Node)app;
	      app = arg;
	      arg = tmp;
	      IND_REMOVE(arg);
	    } else
	      goto restore;	      
	  }
	  WHEN_WADLER(fprintf(stderr,"WADLER 3     %08lx %08lx\n",app,arg);)
	  /*
	   *  arg is either an unmarked constructor or something else
	   */

	  while(GET_TAG(arg) == CON_TAG && (INSIDE(arg) && !ifmarked(arg))) {
	    Int pos;
	    WHEN_WADLER(fprintf(stderr,"WADLER   4   %08lx %08lx(%08lx)\n",app,arg,*arg);)
	    WHEN_WADLER(fprintf(stderr,"WADLER   4b [%08lx %08lx]\n",hpLowLimit,bitTable);)
	    pos = FINFO_CODE(GET_FINFO(app))[SELECTOR_ARG];
	    WHEN_WADLER(fprintf(stderr,"WADLER   4c  pos = %08lx\n",pos);)
#if PARANOID
	    switch(GET_LARGETAG(arg)) {
	    case CON_DATA  | CON_TAG :
	    case CON_CDATA | CON_TAG :
	      WHEN_WADLER(fprintf(stderr,"WADLER   4d size = %08lx\n",CONINFO_SIZE(*arg));)
	      WHEN_WADLER({Int i; for(i=1; i<= CONINFO_SIZE(*arg);i++) { fprintf(stderr,"%08lx:%08lx\n",&(arg[EXTRA+i]),arg[EXTRA+i]);}});
	      if(CONINFO_SIZE(*arg)<pos) {
		fprintf(stderr,"pos (%d) > size (%d)  %08lx:%08lx\n",pos,CONINFO_SIZE(*arg),arg,*arg);
		exit(-1);
	      } break;
	    case CON_PTRS  | CON_TAG :
	    case CON_WORDS | CON_TAG :
	      WHEN_WADLER(fprintf(stderr,"WADLER   4e size = %08lx\n",CONINFO_LARGESIZEU(*arg));)
	      WHEN_WADLER({Int i; for(i=1; i<= CONINFO_LARGESIZEU(*arg);i++) { fprintf(stderr,"%08lx:%08lx\n",&(arg[EXTRA+i]),arg[EXTRA+i]);}});
	      if(CONINFO_LARGESIZEU(*arg)<pos) {
		fprintf(stderr,"pos (%d) > size (%d)  %08lx:%08lx\n",pos,CONINFO_LARGESIZEU(*arg),arg,*arg);
		exit(-1);
	      } break;
            default: fprintf(stderr,"Shit happend! app = %08lx arg = %08lx\n",app,arg);
	             exit(-1);
	    }
	      
#endif
	    WHEN_WADLER(fprintf(stderr,"WADLER   4f arg = %08lx\n",arg);)
	    arg = GET_POINTER_ARG1(arg,pos);  /* Get part .. */
	    WHEN_WADLER(fprintf(stderr,"WADLER   4g arg = %08lx\n",arg);)
	    IND_REMOVE(arg);
	    WHEN_WADLER(fprintf(stderr,"WADLER   4h arg = %08lx\n",arg);)

	    WHEN_WADLER(fprintf(stderr,"WADLER   4i app = %08lx:%08lx\n",app,*app);)
#ifdef PROFILE
	    SAVE_PROFINFO(app);
	    app[1] = (Node)-(Int)(app+SIZE_VAP1);            /* .. insert padding cell */ 
            WHEN_WADLER(fprintf(stderr,"WADLER   4j app[1] = %08lx\n",(Node)-(Int)(app+SIZE_VAP1));)
#endif
	    app[0] = (Node)arg;                                 /* .. overwrite Selector */
    	    unmarked(app);              /* .. and unmark, no marked indirection nodes. */
	    if(app == node) {            /* All done ..  */
	      WHEN_WADLER(fprintf(stderr,"END(1) node  %08lx:%08lx %08lx\n",node,node[0],node[EXTRA+1]);)
	      goto InspectNode;
	    }
	    app = (NodePtr)app[EXTRA+1];      /* .. otherwise back up one step */
	  }
	  WHEN_WADLER(fprintf(stderr,"WADLER 5     %08lx %08lx\n",app,arg);)
	  /*
           * arg is not a constructor or it is already marked
	   */
	restore:
	  node[EXTRA+1] = (Node)0;                    /* End conition */
	  do {
	    NodePtr tmp = (NodePtr)app[EXTRA+1];  /* Get back pointer */
	    WHEN_WADLER(fprintf(stderr,"WADLER   6   %08lx %08lx\n",app,arg);)
	    app[EXTRA+1] = (Node)arg;
	    arg = app;
	    unmarked(arg);
	    app = tmp;
	  } while (app);
	  marked(arg);  /* Not as efficient as it should be */
	  WHEN_WADLER(fprintf(stderr,"WADLER 7     %08lx %08lx\n",app,arg);)
	  WHEN_WADLER(fprintf(stderr,"END(2) node  %08lx:%08lx %08lx\n",node,node[0],node[EXTRA+1]);)
	}
#endif

/*********************************/

        addCaf(finfo);
        WHEN_DYNAMIC(if(pactive) GET_INFO(node)->rinfo = profileRetainer;)
        if(size) {
	  WHEN_PROFILE(marked(node+EXTRA);) /* Mark last word in info */
          newpptr = node+size+EXTRA;
	  EDB(if(debug) {fprintf(stderr,"node = %lx newpptr = %lx:",(UInt)node,(UInt)newpptr); fflush(stderr);})
#ifdef DYNAMIC
          if(pactive && ((profile | filter) & PROFILE_RETAINER)) {
            extern int countAp;
            NodePtr *consttable = FINFO_CONST(finfo);
            char *string = ((char **)consttable)[-1];
            pushRetainerStack(profileRetainer,node);
            if(countAp || *string != '@')
              profileRetainer = findRetainer(0,keepFunction(string),string);
          }
#endif
          goto PushNode;
        }
      } goto NextNode;
    case CON_CDATA|CON_TAG:
      EDB(if(debug) {fprintf(stderr,"CON CDATA:"); fflush(stderr);})
      { Coninfo coninfo = GET_CONINFO(node);
        Int psize  = (Int)CONINFO_PSIZE(coninfo);
        WHEN_DYNAMIC(if(pactive) GET_INFO(node)->rinfo = profileRetainer;)
        EDB(if(debug) {fprintf(stderr,"psize = %d:",psize); fflush(stderr);})
        markForeignObj((ForeignObj *)*(node+psize+1+EXTRA));
	WHEN_PROFILE(marked(node+EXTRA);) /* Mark last word in info */
        if(psize) {
          newpptr = node+psize+EXTRA;
          EDB(if(debug) {fprintf(stderr,"node = %lx newpptr = %lx:",(UInt)node,(UInt)newpptr); fflush(stderr);})
          goto PushNode;
        }
      } goto NextNode;
    case CON_DATA|CON_TAG:
      EDB(if(debug) {fprintf(stderr,"CON DATA:"); fflush(stderr);})
      { Coninfo coninfo = GET_CONINFO(node);
        Int psize  = (Int)CONINFO_PSIZE(coninfo);

        WHEN_DYNAMIC(if(pactive) GET_INFO(node)->rinfo = profileRetainer;)
        if(psize) {
	  WHEN_PROFILE(marked(node+EXTRA);) /* Mark last word in info */
          newpptr = node+psize+EXTRA;
          EDB(if(debug) {fprintf(stderr,"node = %lx newpptr = %lx:",(UInt)node,(UInt)newpptr); fflush(stderr);})
          goto PushNode;
        }
      } goto NextNode;
    case CON_PTRS|CON_TAG:
      EDB(if(debug) {fprintf(stderr,"CON PTRS:"); fflush(stderr);})
      { Coninfo coninfo = GET_CONINFO(node);
        Int psize  = (Int)CONINFO_LARGESIZEU(coninfo);
        WHEN_DYNAMIC(if(pactive) GET_INFO(node)->rinfo = profileRetainer;)
        EDB(if(debug) {fprintf(stderr,"psize = %d:",psize); fflush(stderr);})
        if(psize) {
	  WHEN_PROFILE(marked(node+EXTRA);) /* Mark last word in info */
          newpptr = node+psize+EXTRA;
          EDB(if(debug) {fprintf(stderr,"node = %lx newpptr = %lx:",(UInt)node,(UInt)newpptr); fflush(stderr);})
          goto PushNode;
        }
      } goto NextNode;
    case CON_WORDS|CON_TAG:
      EDB(if(debug) {fprintf(stderr,"CON WORDS:"); fflush(stderr);})

/* Check if it is a known integer */
      { Int i;
        extern Node ints[];
        if(*node == CONSTRW(1,0) && (i=node[EXTRA+1]) >= -10 && i <= 255) { /* Table int (includes all characters) */
          unmarked(node);
          node = (NodePtr)&ints[TABLE_SIZE_INT*(i)];
          goto NextNode;
        }
      }

#ifdef DYNAMIC
      { Coninfo coninfo = GET_CONINFO(node);
        Int size  = (Int)CONINFO_LARGESIZEU(coninfo);
        if(pactive) GET_INFO(node)->rinfo = profileRetainer;
        EDB(if(debug) {fprintf(stderr,"size = %d:",size); fflush(stderr);})
      } goto NextNode;
#endif
      break;
    default:
      fprintf(stderr,"IND_TAG in mark! (1)\n");
      exit(-1);
    }
  }

 NextNode:
  EDB(if(debug) {fprintf(stderr,"Nextnode %lx:",(UInt)node); fflush(stderr);})
  { Node tmp = *pptr;
    *pptr-- = (Node)node;

    if(ifmarked(pptr)) {         /* PopNode: */
      EDB(if(debug) {fprintf(stderr,"PopNode %lx:",(UInt)node); fflush(stderr);})
      if(tmp) {
        node = -EXTRA+(NodePtr)pptr; /* skip over info */
        pptr = (NodePtr)tmp;
#ifdef DYNAMIC
	if(pactive && ((profile | filter) & PROFILE_RETAINER)  && (GET_TAG(node) & VAP_TAG)) {
	  profileRetainer = popRetainerStack(node);
	}
#endif
        goto NextNode;
      } else {
        *inode = node;
        EDB(if(debug) {fprintf(stderr,"return1 %lx\n",(UInt)node); fflush(stderr);})
        return node;
      }
    } else {
      node = (NodePtr)*pptr;
      *pptr = tmp;
      goto InspectNode;
    }
  }

 PushNode:
  EDB(if(debug) {fprintf(stderr,"PushNode %lx:",(UInt)node); fflush(stderr);})
  node = (NodePtr)*newpptr;
  *newpptr = (Node)pptr;
  pptr = newpptr;
  goto InspectNode;
}

void markCaf(void)
{
  while(newCaf != GcEnd) {
    GcConst cptr = newCaf;
    Int size = cptr->sizeArity;
    Int i,arity = SNDHW(size);
    size = FSTHW(size);

#ifdef DYNAMIC
    if((profile | filter) & PROFILE_RETAINER) {
      char *function = (char *)(newCaf[-1].ptr[0]);   /* ((char **)newCaf)[-1]; didn't work */
      profileRetainer = findRetainer(0,keepFunction(function),function);
    }
#endif
    newCaf = cptr->next;
    cptr->next = oldCaf;
    oldCaf = cptr;

    {
      NodePtr *nptr = &cptr->ptr[0];
      if(GET_TAG(nptr) == IND_TAG)
	mark(nptr);
      i = EXTRA+1;
      size += EXTRA+1;
    }

    for(; i<size; i++) {
      NodePtr nptr = cptr->ptr[i];
      switch(EXT_TAG(nptr)) {
      case VAP_TAG0: case VAP_TAG1:
	{ Cinfo cinfo = EXT_CINFO(nptr);
	  addCaf(CINFO_FINFO(cinfo));
	} break;
      case CON_TAG:
	fprintf(stderr,"CON_TAG in markCaf(1) cptr = %8lx sizeArity = %08lx i = %2d %8lx:%08lx\n",(UInt)cptr,cptr->sizeArity,i,(UInt)&cptr->ptr[i],(UInt)nptr);
	exit(-1);
      case IND_TAG:
	mark(&cptr->ptr[i]);
	break;
      }
    }
  }
}

/*

         |         |                   /-----------\
         |     ----+-------------->--->|VAP Node   |
if scc   :sccptr'  :             /     \-----------/
         |fp'      |            /
fp,sp -> |ip'      |  vapptr --/
         -----------

 */

void markStack(NodePtr *sp)
{
  NodePtr *sptr;
  for(sptr = sp; sptr < spStart; ) {
    NodePtr *fp;
    sptr++;                /* skip ip */
    fp = (NodePtr *)*sptr++; /* Fetch fp */

#ifdef DYNAMIC
      if(pactive && ((profile|filter)&PROFILE_RETAINER)) {
	NodePtr *cp = VAP_CONST(fp[2]);  /* fp[0] == ip, fp[1] == fp' */
	char *function = ((char **)cp)[-1];
	profileRetainer = findRetainer(0,keepFunction(function),function);
      }
#endif

    EDB(if(bellGc>1) { fprintf(stderr,"."); fflush(stderr); })

    while(sptr != fp) {
      EDB(if(debug) {fprintf(stderr,"\n  %4x: ",(UInt)sptr); printGraf(*sptr,3,10);fprintf(stderr,"\n"); })
      mark(sptr);
      sptr++;
    }
  }
}
