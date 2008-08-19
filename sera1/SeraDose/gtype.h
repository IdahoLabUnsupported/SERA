/* ============================= gtype.h ===========================

   Include file for general purpose types and constants.

   ================================================================= */

#ifndef __GTYPE

#define  FALSE       0
#define  TRUE   ~FALSE

#define RET_NO_ERR         0
#define RET_WARN          -1
#define RET_FAIL          -2
#define RET_ERR           -1

#define PATH_LEN         128

#define RET_NO_ERROR       0
#define RET_OK             0
#define RET_WARN          -1
#define RET_FAIL          -2


typedef int BOOLEAN;
typedef unsigned char UCHAR;
typedef unsigned short USHORT;


typedef struct complex_str
{
   float r,i;
} COMPLEX_T;

#endif
