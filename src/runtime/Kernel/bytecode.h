
#ifndef _BYTECODE_H
#define _BYTECODE_H

/* bytecode */

#define NEEDHEAP_P1      1
#define NEEDHEAP_P2      2


#define JUMP            3
#define JUMPFALSE       4	/* DAVID */

#define NOP             5

/* #define MATCHCON        6	   DAVID */
/* #define MATCHINT        7	   DAVID */
/* #define JUMPS_T         8	   DAVID */
/* #define JUMPS_L         9	   DAVID */

#define PUSH_CADR_N2   10
#define PUSH_CADR_N1   11
#define PUSH_CADR_P1   12
#define PUSH_CADR_P2   13
#define PUSH_CVAL_N2   14
#define PUSH_CVAL_N1   15
#define PUSH_CVAL_P1   16
#define PUSH_CVAL_P2   17
#define PUSH_INT_N2    18
#define PUSH_INT_N1    19
#define PUSH_INT_P1    20
#define PUSH_INT_P2    21
#define PUSH_ARG       22
#define PUSH_P1        23
#define PUSH_P2        24

#define POP_P1         25
#define POP_P2         26
#define SLIDE_P1       27    
#define SLIDE_P2       28
#define UNPACK         29

#define APPLY          30
#define EVAL           31

#define RETURN         32
#define RETURN_EVAL    33

#define HEAP_CADR_N2   34
#define HEAP_CADR_N1   35
#define HEAP_CADR_P1   36
#define HEAP_CADR_P2   37
#define HEAP_CVAL_N2   38
#define HEAP_CVAL_N1   39
#define HEAP_CVAL_P1   40
#define HEAP_CVAL_P2   41
#define HEAP_INT_N2    42
#define HEAP_INT_N1    43
#define HEAP_INT_P1    44
#define HEAP_INT_P2    45
#define HEAP_ARG       46
#define HEAP_P1        47
#define HEAP_P2        48

#define ADD_W          49
#define ADD_F          50
#define ADD_D          51
#define SUB_W          52
#define SUB_F          53
#define SUB_D          54
#define MUL_W          55
#define MUL_F          56
#define MUL_D          57
#define ABS_W          58
#define ABS_F          59
#define ABS_D          60
#define SIGNUM_W       61 
#define SIGNUM_F       62
#define SIGNUM_D       63
#define EXP_F          64
#define EXP_D          65
#define LOG_F          66
#define LOG_D          67
#define SQRT_F         68
#define SQRT_D         69
#define SIN_F          70
#define SIN_D          71
#define COS_F          72
#define COS_D          73
#define TAN_F          74
#define TAN_D          75
#define ASIN_F         76
#define ASIN_D         77
#define ACOS_F         78
#define ACOS_D         79
#define ATAN_F         80
#define ATAN_D         81
#define SLASH_F        82
#define SLASH_D        83
#define EQ_W           84
#define EQ_F           85
#define EQ_D           86
#define NE_W           87
#define NE_F           88
#define NE_D           89
#define LT_W           90
#define LT_F           91
#define LT_D           92
#define LE_W           93
#define LE_F           94
#define LE_D           95
#define GT_W           96
#define GT_F           97
#define GT_D           98
#define GE_W           99
#define GE_F          100
#define GE_D          101
#define NEG_W         102
#define NEG_F         103
#define NEG_D         104

#define QUOT          105
#define REM           106
#define AND           107
#define OR            108
#define NOT           109
#define ORD           110
#define CHR           111
#define SEQ           112
#define STRING        113

#define PRIMITIVE     114
#define PUSH_HEAP     115
#define EXIT          116

#define NEEDSTACK_P1  117
#define NEEDSTACK_P2  118

#define HEAP_OFF_N2  119
#define HEAP_OFF_N1  120
#define HEAP_OFF_P1  121
#define HEAP_OFF_P2  122

#define HEAP_CREATE  123
#define HEAP_SPACE   124

#define SELECTOR_EVAL 125
#define SELECT        126

#define ZAP_ARG       127
#define ZAP_STACK_P1  128
#define ZAP_STACK_P2  129



#define NEEDHEAP_I32      130
#define NEEDSTACK_I16     131

#define PUSH_I1           132
#define POP_I1            133

#define PUSH_ARG_I1       134
#define PUSH_ARG_I2       135
#define PUSH_ARG_I3       136

#define ZAP_ARG_I1        137
#define ZAP_ARG_I2        138
#define ZAP_ARG_I3        139

#define HEAP_CVAL_I3      140
#define HEAP_CVAL_I4      141
#define HEAP_CVAL_I5      142

#define HEAP_CVAL_IN3     143

#define HEAP_I1           144
#define HEAP_I2           145

#define HPUTC             146
#define HGETC             147

#define PUSH_CHAR_N1	  148
#define PUSH_CHAR_P1	  149
#define HEAP_CHAR_N1	  150
#define HEAP_CHAR_P1	  151

#define TABLESWITCH       190	/* DAVID */
#define LOOKUPSWITCH      191	/* DAVID */

#define ENDCODE           199	/* DAVID */

#endif
