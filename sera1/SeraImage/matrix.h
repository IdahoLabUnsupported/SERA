/* =========================================================================
 *
 * Matrix support routines for image registration.
 *
 * History: Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

/*
 * defines for registration process
 */

#define MBASE_TYPE float
#define MBASE_TINY 1.0E-06
#define MBASE_NEAR_ZERO   1.0E-04

/*
 * Prototypes
 */

void CopyMatrix (MBASE_TYPE *, int, int, MBASE_TYPE *);
void DumpMatrix (char *, MBASE_TYPE *, int, int);
MBASE_TYPE *AllocateMatrix (int, int);
void FreeMatrix (MBASE_TYPE *);

int MatrixInvert (MBASE_TYPE *, int, MBASE_TYPE *);
int MatrixInvertGJ (MBASE_TYPE *, int, MBASE_TYPE *);
void Transpose (MBASE_TYPE *, int, int);

int LUDecomp (MBASE_TYPE *, int, int *, int *);
int LUBacksub (MBASE_TYPE *, int, MBASE_TYPE *);
void IdentityMatrix (MBASE_TYPE *, int);
void MatrixMult (MBASE_TYPE *, int, int, 
   MBASE_TYPE *, int, MBASE_TYPE *);
