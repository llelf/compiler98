#include <errno.h>
#include "haskell2c.h"

/* primIntAnd :: Int -> Int -> Int */
C_HEADER(primIntAnd) {
    unsigned x,y;
    NodePtr n = C_GETARG1(1);
    IND_REMOVE(n);
    x = (unsigned)GET_INT_VALUE(n);

    n = C_GETARG1(2);
    IND_REMOVE(n);
    y = (unsigned)GET_INT_VALUE(n);

    C_RETURN(nhc_mkInt(x&y));
}

/* primIntOr :: Int -> Int -> Int */
C_HEADER(primIntOr) {
    unsigned x,y;
    NodePtr n = C_GETARG1(1);
    IND_REMOVE(n);
    x = (unsigned)GET_INT_VALUE(n);

    n = C_GETARG1(2);
    IND_REMOVE(n);
    y = (unsigned)GET_INT_VALUE(n);

    C_RETURN(nhc_mkInt(x|y));
}

/* primIntXor :: Int -> Int -> Int */
C_HEADER(primIntXor) {
    unsigned x,y;
    NodePtr n = C_GETARG1(1);
    IND_REMOVE(n);
    x = (unsigned)GET_INT_VALUE(n);

    n = C_GETARG1(2);
    IND_REMOVE(n);
    y = (unsigned)GET_INT_VALUE(n);

    C_RETURN(nhc_mkInt(x^y));
}

/* primIntCompl :: Int -> Int */
C_HEADER(primIntCompl) {
    unsigned x;
    NodePtr n = C_GETARG1(1);
    IND_REMOVE(n);
    x = (unsigned)GET_INT_VALUE(n);

    C_RETURN(nhc_mkInt(~x));
}

/* primIntLsh :: Int -> Int -> Int */
C_HEADER(primIntLsh) {
    int s; unsigned i;
    NodePtr n = C_GETARG1(1);
    IND_REMOVE(n);
    i = (unsigned)GET_INT_VALUE(n);

    n = C_GETARG1(2);
    IND_REMOVE(n);
    s = GET_INT_VALUE(n);

    C_RETURN(nhc_mkInt(i<<s));
}

/* primIntRsh :: Int -> Int -> Int */
C_HEADER(primIntRsh) {
    int s; unsigned i;
    NodePtr n = C_GETARG1(1);
    IND_REMOVE(n);
    i = (unsigned)GET_INT_VALUE(n);

    n = C_GETARG1(2);
    IND_REMOVE(n);
    s = GET_INT_VALUE(n);

    C_RETURN(nhc_mkInt(i>>s));
}

