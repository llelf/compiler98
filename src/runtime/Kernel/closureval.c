#include "haskell2c.h"
#include "newmacros.h"

#ifdef PROFILE
static SInfo apply1ProfInfo = { "Runtime","buildClosure","<APPLY.VAP>"};
static SInfo apply2ProfInfo = { "Runtime","buildClosure","<APPLY.$>"};
static SInfo apply3ProfInfo = { "Runtime","buildClosure","<APPLY.CAP>"};
#endif

extern  HaskellRef       stableInsert (NodePtr);
extern  NodePtr   stableRef (HaskellRef);
#define getNode() stableRef(*block++)

HaskellRef buildClosure (int args, HaskellRef* block)
{
    int need, size;
    Cinfo cinfo;
    NodePtr vap, nodeptr;
 
    if (args<0) {
        fprintf(stderr,"buildClosure() called with negative argument\n");
        exit(1);
    }

    C_CHECK(2*(args+1));

    nodeptr = getNode();
    IND_REMOVE(nodeptr);
    UPDATE_PROFINFO(nodeptr)

    cinfo = GET_CINFO(nodeptr);
 
    {
        int c = (GET_LARGETAG(nodeptr));
        switch(c) {
        case CON_DATA | CON_TAG:
        case CON_CDATA | CON_TAG:
            fprintf(stderr, "Strange: con in apply:\n");
#if TRACE
            prGraph(nodeptr, 3, 3);
#endif
            fprintf(stderr, "\n");
            /*startDbg(GET_POINTER_ARG1(nodeptr, 2));*/
            exit(-1);
        }
    }
#if 1
    if(GET_TAG(nodeptr)&VAP_TAG && !CINFO_NEED(cinfo)) {  /* Probably not needed */
        fprintf(stderr,"VAP in Apply?\n");
        vap = nodeptr;
        goto build_apply;
    }
#endif
 
    need = CINFO_NEED(cinfo);
    size = CINFO_SIZE(cinfo);
    nodeptr = nodeptr+1+EXTRA;  /* Skip tag (and optional profile info) */
    if(need <= args) {
        INIT_PROFINFO(Hp,&apply1ProfInfo)
        vap = Hp;
        *Hp++ = (Node)((UInt)2*need+(UInt)cinfo)+(UInt)VAP_TAG;
        Hp += EXTRA; 
        while(size-->0)
            *Hp++ = *nodeptr++;
        args -= need;
        while(need--)
            *Hp++ = (Node)getNode();
    build_apply:
        while(args--) {
            INIT_PROFINFO(Hp,&apply2ProfInfo)
            *Hp++ = (Node)(C_VAPTAG(PRIM_APPLY));
            Hp += EXTRA;
            *Hp ++ = (Node) vap;
            vap = &Hp[-2-EXTRA];
            *Hp++ = (Node)getNode();
        }
    } else { /* need > args */
        INIT_PROFINFO(Hp,&apply3ProfInfo)
        vap = Hp;
        *Hp++ = (Node)(2*(UInt)args+(UInt)VAP_TAG+(UInt)cinfo);
        Hp +=EXTRA;
        while(size-->0)
            *Hp++ = *nodeptr++;
        while(args-->0)
            *Hp++ = (Node)getNode();
    }
    return stableInsert(vap);
}

void eval(HaskellRef x)
{
  CodePtr IP=Ip;		/* save global instruction pointer */
  C_PUSH(stableRef(x));
  C_EVALTOS(stableRef(x));
  C_POP();
  Ip=IP;			/* restore instruction pointer */
}

HaskellRef makeInt (int x) { return stableInsert(mkInt(x)); }
int unmakeInt (HaskellRef x)
{
  NodePtr n = stableRef(x);
  IND_REMOVE(n);
  return GET_INT_VALUE(n);
}

HaskellRef makeChar (char x) { return stableInsert(mkChar(x)); }
char unmakeChar (HaskellRef x)
{
  NodePtr n = stableRef(x);
  IND_REMOVE(n);
  return GET_CHAR_VALUE(n);
}

HaskellRef makeBool (int x) { return stableInsert(mkBool(x)); }
int unmakeBool (HaskellRef x)
{
  NodePtr n = stableRef(x);
  IND_REMOVE(n);
  return GET_BOOL_VALUE(n);
}

/* ***********************************************************
HaskellRef makeFloat (float x) { return stableInsert(mkFloat(x)); }
float unmakeFloat (HaskellRef x)
{
  NodePtr n = stableRef(x);
  IND_REMOVE(n);
  return GET_FLOAT_VALUE(n);
}

HaskellRef makeDouble (double x) { return stableInsert(mkDouble(x)); }
double unmakeDouble (HaskellRef x)
{
  NodePtr n = stableRef(x);
  IND_REMOVE(n);
  return GET_DOUBLE_VALUE(n);
}

HaskellRef makePackedString (char* x) { return stableInsert(mkString(x)); }
char* unmakePackedString (HaskellRef x)
{
  NodePtr n = stableRef(x);
  IND_REMOVE(n);
  return GET_STRING_VALUE(n);
}
   *********************************************************** */
