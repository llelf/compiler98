#include <stdio.h>
#include "fileformat.h"

/* Remaining problems include (at least) the following:
 *   . All Integer values are faked to zero for now.
 *   . Hence all Rational values are also dummy.
 *   . Floats and Doubles are written to the file without regard for endianness.
 *   . SATs are never overwritten - the B and C variants create new things
 *     in the file.
 */

FileOffset
primTAp1 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset sr)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x01,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
primTAp2 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset sr)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x02,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
primTAp3 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset sr)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x03,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
primTAp4 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset sr)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x04,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
primTAp5 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset sr)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ5, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    return fo;
}






FileOffset
primTNm (FileOffset tnm, CNmType nm, FileOffset sr)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | TNm),HatFile);
    fwrite(&tnm,       sizeof(FileOffset), 1, HatFile);
    fwrite(&(nm->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,        sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
primTInd (FileOffset t1, FileOffset t2)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | TInd),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
    fwrite(&t2, sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
primTHidden (FileOffset t1)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | THidden),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
primTSatA (FileOffset t1)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | TSatA),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
    return fo;
}


/* This implementation of SatB is wrong - it creates a new node rather than
 * overwriting the old one.
 */

FileOffset
primTSatB (FileOffset t1)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | TSatB),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
    return fo;
}

/* This implementation of SatC is wrong - it creates a new node rather than
 * overwriting the old one.
 */

FileOffset
primTSatC (FileOffset torig,FileOffset teval)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((Trace<<5) | TSatC),HatFile);
    fwrite(&teval, sizeof(FileOffset), 1, HatFile);
    return fo;
}



/* Auxiliary function to turn a simple FileOffset into a struct CNmType.
 * We store the kind of the Nm object (currently not needed),
 * and also its trustedness (needed).
 */

CNmType
mkCNmType (int type, FileOffset fo, int trustedness)
{
    CNmType nm;
    nm = (CNmType) C_ALLOC(1+EXTRA+2);
    nm->constr = CONSTRW(type,2);
    INIT_PROFINFO((void*)nm,&dummyProfInfo)
    nm->ptr = fo;
    nm->trust = trustedness;
    return nm;
}






CNmType
primNTInt (int i)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTInt),HatFile);
    i = htonl(i);
    fwrite(&i, sizeof(int), 1, HatFile);
    return mkCNmType(NTInt,fo,False);
}

CNmType
primNTChar (char c)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTChar),HatFile);
    fwrite(&c, sizeof(char), 1, HatFile);
    return mkCNmType(NTChar,fo,False);
}

CNmType
primNTInteger (NodePtr i)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTInteger),HatFile);
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    return mkCNmType(NTInteger,fo,False);
}

CNmType
primNTRational (NodePtr i, NodePtr j)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTRational),HatFile);
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    return mkCNmType(NTRational,fo,False);
}

CNmType
primNTFloat (float f)
{
    FileOffset fo;
    CNmType nm;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTFloat),HatFile);
    fwrite(&f, sizeof(float), 1, HatFile);	/* ignore endian problems */
    return mkCNmType(NTFloat,fo,False);
}

CNmType
primNTDouble (double d)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTDouble),HatFile);
    fwrite(&d, sizeof(double), 1, HatFile);	/* ignore endian problems */
    return mkCNmType(NTDouble,fo,False);
}

CNmType
primNTId (IdEntry *id)
{
    if (id->fileoffset) {
        return mkCNmType(NTId,id->fileoffset,id->srcmod->trusted);
    } else {
        FileOffset fo;
        int i;
        fo = ftell(HatFile);
        fo = htonl(fo);
        fputc(((NmType<<5) | NTId),HatFile);
        fprintf(HatFile,"%s",id->name);
        fputc(0x0,HatFile);
        i = 0;
        fwrite(&(id->srcmod->fileoffset), sizeof(FileOffset), 1, HatFile);
        fputc(id->pri,HatFile);
        i = htonl(id->srcpos);
        fwrite(&i, sizeof(int), 1, HatFile);
        id->fileoffset = fo;
        return mkCNmType(NTId,fo,id->srcmod->trusted);
    }
}

/* probably never used? */
CNmType
primNTConstr (IdEntry *id)
{
    if (id->fileoffset) {
        return mkCNmType(NTConstr,id->fileoffset,id->srcmod->trusted);
    } else {
        FileOffset fo;
        int i;
        fo = ftell(HatFile);
        fo = htonl(fo);
        fputc(((NmType<<5) | NTConstr),HatFile);
        fprintf(HatFile,"%s",id->name);
        fputc(0x0,HatFile);
        i = 0;
        fwrite(&(id->srcmod->fileoffset), sizeof(FileOffset), 1, HatFile);
        fputc(id->pri,HatFile);
        i = htonl(id->srcpos);
        fwrite(&i, sizeof(int), 1, HatFile);
        id->fileoffset = fo;
        return mkCNmType(NTConstr,fo,id->srcmod->trusted);
    }
}

CNmType
primNTTuple ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTTuple),HatFile);
    return mkCNmType(NTTuple,fo,False);
}

CNmType
primNTFun ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTFun),HatFile);
    return mkCNmType(NTFun,fo,False);
}

CNmType
primNTCase ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTCase),HatFile);
    return mkCNmType(NTCase,fo,False);
}

CNmType
primNTLambda ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTLambda),HatFile);
    return mkCNmType(NTLambda,fo,False);
}

CNmType
primNTDummy ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTDummy),HatFile);
    return mkCNmType(NTDummy,fo,False);
}

CNmType
primNTCString (char *s)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTCString),HatFile);
    fprintf(HatFile,"%s",s);
    fputc(0x0,HatFile);
    return mkCNmType(NTCString,fo,False);
}

CNmType
primNTIf ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTIf),HatFile);
    return mkCNmType(NTIf,fo,False);
}

CNmType
primNTGuard ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTGuard),HatFile);
    return mkCNmType(NTGuard,fo,False);
}

CNmType
primNTContainer ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTContainer),HatFile);
    return mkCNmType(NTContainer,fo,False);
}


int
primTrustedFun (CNmType nm)
{
    return nm->trust;
}



FileOffset
primSR0 ()
{
    return (FileOffset)0;
}

FileOffset
primSR3 (SrcRef *sr)
{
    if (sr->fileoffset) {
        return sr->fileoffset;
    } else {
        FileOffset fo;
        int i = 0;
        fo = ftell(HatFile);
        fo = htonl(fo);
        fputc((SR<<5),HatFile);
        fwrite(&(sr->modinfo->fileoffset), sizeof(FileOffset), 1, HatFile);
        i = htonl(sr->posn);
        fwrite(&i, sizeof(int), 1, HatFile);
        sr->fileoffset = fo;
        return fo;
    }
}

