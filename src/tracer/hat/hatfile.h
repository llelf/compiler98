/**************************************************************************/
/* hatfile.h                                                              */
/* definition of constants within the hat file                            */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

#ifndef HATFILE  // only import this file once

#define HATFILE

#define TR 0

#define APP 0
#define TRAPP 0
#define NAM 1
#define TRNAM 1
#define IND 2
#define TRIND 2
#define HIDDEN 3
#define TRHIDDEN 3
#define SATA 4
#define TRSATA 4
#define SATB 5
#define TRSATB 5
#define SATC 6
#define TRSATC 6
#define TRSATAIS 12
#define TRSATBIS 13
#define TRSATCIS 14
#define SATAIS 12
#define SATBIS 13
#define SATCIS 14

#define MD 1
#define SUSPECT 0
#define MDSUSPECT 32
#define TRUSTED 1
#define MDTRUSTED 33

#define NT 2

#define INT 0
#define NTINT 64
#define CHAR 1
#define NTCHAR 65
#define INTEGER 2
#define NTINTEGER 66
#define RATIONAL 3
#define NTRATIONAL 67 
#define FLOAT 4
#define NTFLOAT 68
#define DOUBLE 5
#define NTDOUBLE 69
#define IDENTIFIER 6
#define NTIDENTIFIER 70
#define CONSTRUCTOR 7
#define NTCONSTRUCTOR 71
#define TUPLE 8
#define NTTUPLE 72
#define FUN 9
#define NTFUN 73
#define CASE 10
#define NTCASE 74
#define LAMBDA 11
#define NTLAMBDA 75
#define DUMMY 12
#define NTDUMMY 76
#define CSTRING 13
#define NTCSTRING 77
#define IF 14
#define NTIF 78
#define GUARD 15
#define NTGUARD 79
#define CONTAINER 16
#define NTCONTAINER 80
#define TOPIDENTIFIER 22
#define NTTOPIDENTIFIER 86

#define SR 3
#define SRCREF 96

#define HEADER 6
#define INVALID 7
#define BEYOND 8

#define MESSAGE 255  // local definition for expressions only


#define HatINFIX   0
#define HatINFIXR  1
#define HatINFIXL  2
#define HatNOINFIX 3


#define HatApplication 0
#define HatName 1
#define HatProjection 2
#define HatHidden 3
#define HatSATA 4
#define HatSATB 5
#define HatSATC 6

#define HatModule 32

#define HatInt 64
#define HatChar 65
#define HatInteger 66
#define HatRational 67 
#define HatFloat 68
#define HatDouble 69
#define HatIdentifier 70
#define HatConstructor 71
#define HatTuple 72
#define HatFun 73
#define HatCase 74
#define HatLambda 75
#define HatDummy 76
#define HatCString 77
#define HatIf 78
#define HatGuard 79
#define HatContainer 80

#define HatSrcRef 96

#endif
