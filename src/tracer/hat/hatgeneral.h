/**************************************************************************/
/* hatgeneral.h:                                                          */
/* General library for all kinds of helpful functions used by hat tools   */
/*                                                                        */
/* Thorsten Brehm, 6/2001                                                 */
/**************************************************************************/

/* basic functions for allocating and freeing strings on heap */ 

/* reserve space on heap for given string - and copy */
char*      newStr(char* str);

/* append strings and reserve space for the result string */
char*      catStr (char* s1, char* s2, char* s3);

/* replace string in s with the concatenation of s1,s2 and s3 */
void       replaceStr(char** s,char* s1,char *s2,char* s3);

/* free memory space */
void       freeStr(char* s);
