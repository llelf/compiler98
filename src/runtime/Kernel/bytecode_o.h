#ifndef _BYTECODE_O_H_
#define _BYTECODE_O_H_

/* This file defines an obsolete form of the bytecode, used by the
 * non-ANSI-C (assembler) backend of the compiler.  This obsolete form
 * can maintained in sync with the "real" bytecode (if the "real" bytecode
 * changes) by running the file  script/mangler.
 */

#define DUMMY_FOR_ZERO 0
#define NEEDHEAP_P1 1
#define NEEDHEAP_P2 2
#define JUMP 3
#define JUMPFALSE 4
#define NOP 5
#define PUSH_CADR_N2 6
#define PUSH_CADR_N1 7
#define PUSH_CADR_P1 8
#define PUSH_CADR_P2 9
#define PUSH_CVAL_N2 10
#define PUSH_CVAL_N1 11
#define PUSH_CVAL_P1 12
#define PUSH_CVAL_P2 13
#define PUSH_INT_N2 14
#define PUSH_INT_N1 15
#define PUSH_INT_P1 16
#define PUSH_INT_P2 17
#define PUSH_ARG 18
#define PUSH_P1 19
#define PUSH_P2 20
#define POP_P1 21
#define POP_P2 22
#define SLIDE_P1 23
#define SLIDE_P2 24
#define UNPACK 25
#define APPLY 26
#define EVAL 27
#define RETURN 28
#define RETURN_EVAL 29
#define HEAP_CADR_N2 30
#define HEAP_CADR_N1 31
#define HEAP_CADR_P1 32
#define HEAP_CADR_P2 33
#define HEAP_CVAL_N2 34
#define HEAP_CVAL_N1 35
#define HEAP_CVAL_P1 36
#define HEAP_CVAL_P2 37
#define HEAP_INT_N2 38
#define HEAP_INT_N1 39
#define HEAP_INT_P1 40
#define HEAP_INT_P2 41
#define HEAP_ARG 42
#define HEAP_ARG_ARG 43
#define HEAP_ARG_ARG_RET_EVAL 44
#define HEAP_P1 45
#define HEAP_P2 46
#define ADD_W 47
#define ADD_F 48
#define ADD_D 49
#define SUB_W 50
#define SUB_F 51
#define SUB_D 52
#define MUL_W 53
#define MUL_F 54
#define MUL_D 55
#define ABS_W 56
#define ABS_F 57
#define ABS_D 58
#define SIGNUM_W 59
#define SIGNUM_F 60
#define SIGNUM_D 61
#define EXP_F 62
#define EXP_D 63
#define LOG_F 64
#define LOG_D 65
#define SQRT_F 66
#define SQRT_D 67
#define SIN_F 68
#define SIN_D 69
#define COS_F 70
#define COS_D 71
#define TAN_F 72
#define TAN_D 73
#define ASIN_F 74
#define ASIN_D 75
#define ACOS_F 76
#define ACOS_D 77
#define ATAN_F 78
#define ATAN_D 79
#define SLASH_F 80
#define SLASH_D 81
#define EQ_W 82
#define EQ_F 83
#define EQ_D 84
#define NE_W 85
#define NE_F 86
#define NE_D 87
#define LT_W 88
#define LT_F 89
#define LT_D 90
#define LE_W 91
#define LE_F 92
#define LE_D 93
#define GT_W 94
#define GT_F 95
#define GT_D 96
#define GE_W 97
#define GE_F 98
#define GE_D 99
#define NEG_W 100
#define NEG_F 101
#define NEG_D 102
#define QUOT 103
#define REM 104
#define AND 105
#define OR 106
#define NOT 107
#define ORD 108
#define CHR 109
#define SEQ 110
#define STRING 111
#define PRIMITIVE 112
#define PUSH_HEAP 113
#define EXIT 114
#define NEEDSTACK_P1 115
#define NEEDSTACK_P2 116
#define HEAP_OFF_N2 117
#define HEAP_OFF_N1 118
#define HEAP_OFF_P1 119
#define HEAP_OFF_P2 120
#define HEAP_CREATE 121
#define HEAP_SPACE 122
#define SELECTOR_EVAL 123
#define SELECT 124
#define ZAP_ARG 125
#define ZAP_STACK_P1 126
#define ZAP_STACK_P2 127
#define NEEDHEAP_I32 128
#define NEEDSTACK_I16 129
#define PUSH_I1 130
#define POP_I1 131
#define PUSH_ARG_I1 132
#define PUSH_ARG_I2 133
#define PUSH_ARG_I3 134
#define ZAP_ARG_I1 135
#define ZAP_ARG_I2 136
#define ZAP_ARG_I3 137
#define HEAP_CVAL_I3 138
#define HEAP_CVAL_I4 139
#define HEAP_CVAL_I5 140
#define HEAP_CVAL_IN3 141
#define HEAP_I1 142
#define HEAP_I2 143
#define HPUTC 144
#define HGETC 145
#define HGETS 146
#define PUSH_CHAR_N1 147
#define PUSH_CHAR_P1 148
#define HEAP_CHAR_N1 149
#define HEAP_CHAR_P1 150
#define TABLESWITCH 151
#define LOOKUPSWITCH 152
#define MKIORETURN 153
#define PUSH_ZAP_ARG_I1 154
#define PUSH_ZAP_ARG_I2 155
#define PUSH_ZAP_ARG_I3 156
#define PUSH_ZAP_ARG 157
#define ENDCODE 158

#endif
