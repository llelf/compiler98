#include "ui.h"

/* Show a single expression from the trace.  Return the parent trace of
 * this whole expression.
 */

void
stackSR(NodePtr t)
{
    int rowcol, row, col;
    fprintf(stderr,"\t\t(called at ");
    if (t) IND_REMOVE(t);
    if (t && (GET_TAG(t)==CON_TAG)) {
        Coninfo cinfo = GET_CONINFO(t);
        if (CONINFO_NUMBER(cinfo) == 2) { /* SR3 */
            rowcol = (int)t[1+EXTRA];
            row = rowcol / 10000;
            col = rowcol % 10000;
            fprintf(stderr, "line-%d/col-%d of \"%s.hs\"",
				 row, col, (char *)GET_POINTER_ARG1(t, 2));
        } else if (CONINFO_NUMBER(cinfo) == 0) { /* SR */
            fprintf(stderr, "dynamic site");
        } else {
            fprintf(stderr, "* %d", CONINFO_NUMBER(cinfo));
        }
    } else {
        fprintf(stderr, "unknown");
        /*prGraph(t, 3, 3); */
    }
    fprintf(stderr,")");
}

/* Pass in the trace expression to be printed.  Get back its parent.
 * Also fill in a variable indicating the expression's source reference.
 */
NodePtr
stackExpr(NodePtr t, int level, NodePtr *sr)
{
    NodePtr n, parent=NULL;
    int constr, ref;
    NodePtr *pbot;
    int *pind;

    if (level-- == 0) {
            fprintf(stderr, "<>");
            return;
    }
    /* fprintf(stderr,"í"); */

    t = shortCircuitSelectors(t);
    constr = CONINFO_NUMBER(*t) & 0xf;
    switch (constr) {
    case TagAp: {
            NodePtr ts;
            int tag, first = 1;
            parent = GET_POINTER_ARG1(t, 1);
            ts = GET_POINTER_ARG1(t, 2);
            IND_REMOVE(ts);
            tag = CONINFO_NUMBER(*ts);
            /* fprintf(stderr,"ä"); */
            fprintf(stderr,"(");
            while (CONINFO_PSIZE(*ts) == 2) {
               if (first++ > 1)
                       fprintf(stderr, " ");
               stackExpr (GET_POINTER_ARG1(ts, 1), level, sr);
               ts = shortCircuitSelectors(GET_POINTER_ARG1(ts, 2));
            }
            fprintf(stderr,")");
            *sr = GET_POINTER_ARG1(t, 3);
            break; }
    case TagNm: {
            char *mod, *name;
            int defpos, pri;    
            /* fprintf(stderr,"ñ"); */
            showSymbol(GET_POINTER_ARG1(t, 2), &mod, &name, &defpos, &pri);
            fprintf(stderr, "%s", name);
            break;}
    case TagInd:
            /* fprintf(stderr,"I"); */
            parent = stackExpr(GET_POINTER_ARG1(t, 1), level, sr);
            break;
    case TagRoot:
            /* fprintf(stderr,"R"); */
            fprintf(stderr, "<root>");
            break;
    case TagSat:
            /* fprintf(stderr,"ß"); */
            n = GET_POINTER_ARG1(t, 1);
            parent = stackExpr(n, level, sr);
#if 0
            parent = GET_POINTER_ARG1(t, 1);
            n = shortCircuitSelectors(GET_POINTER_ARG1(t, 2));
            switch (checkEvaluation(n, pbot)) {
            case EVALUATED:
                    stackExpr(n, level, sr);
                    t = n;
                break;
            case EVALUATING:
                    fprintf(stderr, "_L");
                    break;
            case CLOSURE:
                    fprintf(stderr, "_");
                    break;
            }
#endif
            break;
    case TagPruned:
            fprintf(stderr, "[X]");
            break;
    case TagHidden:
            fprintf(stderr, "[/]");
            parent = GET_POINTER_ARG1(t, 1);
            break;
    default:
            fprintf(stderr, "stackExpr: strange tag %d\n", constr);
    }
    /* fprintf(stderr,"ò"); */
    return parent;
}

void
stackTrace(NodePtr t)
{
    NodePtr parent, sr;
    fprintf(stderr, "\nVirtual stack trace:\n    ");
    parent = stackExpr(t, DEFAULT_DEPTH, &sr);
    stackSR(sr);
    while (parent) {
        fprintf(stderr, "\n    ");
        parent = stackExpr(parent, DEFAULT_DEPTH,&sr);
        stackSR(sr);
    }
    fprintf(stderr,"\n\n");
}

