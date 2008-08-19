
/* ============================================================> matrix.c
 *
 * Matrix algebra support routines.
 *
 * =======================================================================
 */

   
/* =======================================================================
 *
 * Invert matrix M, of size NxN and return the result in inverse.
 *
 * =======================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include "matrix.h"
#include <math.h>

/*
void main ()
{
   MBASE_TYPE   T[2][2], inverseT[2][2], det;
   MBASE_TYPE   *Z1, *Z2, *Z3, *ZY, *B, *T2;
   MBASE_TYPE   Y[4] = {2, -1, 2, 4};
   MBASE_TYPE   Z[8] = {3, 3, 4, 8, 6, 5, 7, 7};
   int          N = 4;

   Z1 = AllocateMatrix (2, 2);
   MatrixInvertGJ (Y, 2, Z1);
   DumpMatrix ("Y", Y, 2, 2);
   DumpMatrix ("Inverse", Z1, 2, 2);

   Z1 = AllocateMatrix (N, 2);
   CopyMatrix (Z1, N, 2, Z);
   Transpose (Z1, N, 2);

   Z2 = AllocateMatrix (2, 2);
   MatrixMult (Z1, 2, N, Z, 2, Z2);

   Z3 = AllocateMatrix (2, 2);
   MatrixInvertGJ (Z2, 2, Z3);

   ZY = AllocateMatrix (2, 2);
   MatrixMult (Z1, 2, N, Y, 2, ZY);

   T2 = AllocateMatrix (2, 2);
   MatrixMult (Z3, 2, 2, ZY, 2, T2);

   printf ("T[0][0] = %.2f\n", T2[0]);
   printf ("T[0][1] = %.2f\n", T2[1]);
   printf ("T[1][0] = %.2f\n", T2[2]);
   printf ("T[1][1] = %.2f\n", T2[3]);

   FreeMatrix (Z1);
   FreeMatrix (Z2);
   FreeMatrix (Z3);
   FreeMatrix (ZY);
   FreeMatrix (T2);

}
*/

int MatrixInvert
(
   MBASE_TYPE    *mat,
   int           N,
   MBASE_TYPE    *inverse
)
{
   int         row, col, ct, pos_neg, *index;
   MBASE_TYPE  *Y;

/*
   index = (int *) malloc (N * sizeof (int));
   if (index == NULL)
      return (-1);
*/
   Y = AllocateMatrix (N, N); 
   if (Y == NULL)
      return (-1);

   /*
    * Find the inverse by using LU decomposition and back substitution.
    */

   CopyMatrix (inverse, N, N, mat);
   LUDecomp (inverse, N, index, &pos_neg);
   
   IdentityMatrix (Y, N);
   for (ct = 0; ct < N; ct ++)
      LUBacksub (inverse, N, Y);
   Transpose (Y, N, N);


   FreeMatrix (Y);
   return (0);
}

void Transpose
(
   MBASE_TYPE     *mat,
   int            N,
   int            M
)
{
   int            row, col;
   MBASE_TYPE     *temp;

   temp = (MBASE_TYPE *) malloc (N * M * sizeof (MBASE_TYPE));
   for (row = 0; row < N; row ++)
      for (col = 0; col < M; col ++)
         temp [col * N + row] = mat [row * M + col];

   memcpy (mat, temp, M * N * sizeof (MBASE_TYPE));

   free (temp);

   return;
}

void IdentityMatrix 
(
   MBASE_TYPE     *mat,
   int            N
)
{
   int        row, col;

   for (row = 0; row < N; row ++)
   {
      for (col = 0; col < N; col ++)
         mat [row * N + col] = 0.0;
       mat [row * N + row] = 1.0;
   }

   return;
}

   
/* ======================================================================
 *
 * Given mat of size NxN, perform LU Decomposition with pivoting.  Return
 * the index of the pivots, the LU matrix and pos_neg which is +1
 * for an even number of exchanges and -1 for an odd number (this is
 * used in determining the premultiplier for the determinant.
 *
 * This algorithm is an implementation of Crout's Algorithm, as shown
 * in Numerical Recipes, page 31.
 *
 * ======================================================================
 */

int LUDecomp
(
   MBASE_TYPE     *mat, 
   int            N, 
   int            *index, 
   int            *pos_neg
)
{
   MBASE_TYPE    row_max, *scale, m, term;
   int           idx_max, row, col, ct;

   /*
    * Simple LU Decomp.  Copy mat to LU and then proceed.
    */

   for (col = 0; col < N; col ++)
   {
      for (row = 0; row <= col; row ++)
      {
         term = (MBASE_TYPE) 0.0;
         for (ct = 0; ct < row; ct ++)
            term += mat [row * N + ct] * mat [ct * N + col];
         mat [row * N + col] -= term;
      }

      for (row = col+1; row < N; row ++)
      {
         term = (MBASE_TYPE) 0.0;
         for (ct = 0; ct < col; ct ++)
            term += mat [row * N + ct] * mat [ct * N + col];
         *(mat + row * N + col) = 
            ((MBASE_TYPE) 1.0) / 
               mat [col * N + col] * mat [row * N + col] - term;
      }
   }

   return (0);

}


int LUBacksub
(
   MBASE_TYPE     *mat,
   int            N,
   MBASE_TYPE     *B
)
{
   MBASE_TYPE     term;
   int            row, col;

   /*
    * Do the forward substitution.
    */

   for (row = 0; row < N; row ++)
   {
      term = B [row];
printf ("B[%d] = %.3f,", row, term);
      for (col = 0; col < row - 1; col ++)
      {
         term = term - mat [row * N + col] * B [col]; 
printf ("  %.3f,", term);
      }
      B [row] = term;
printf ("  %.3f\n ", term);
   }


   /*
    * Do the back substitution.
    */

   for (row = N-1; row >= 0; row --)
   {
      term = B [row];
printf ("B[%d] = %.3f, ", row, term);
      for (col = row + 1; col < N; col ++)
      {
         term = term - mat [row * N + col] * B [col]; 
printf ("  %.3f,", term);
      }
      B [row] = term / mat [row * N + row];
printf ("  %.3f\n\n",term);
   }

   return (0);
}
   
     
void MatrixMult 
(
   MBASE_TYPE    *mat1, 
   int           N1, 
   int           N2, 
   MBASE_TYPE    *mat2, 
   int           N3,
   MBASE_TYPE    *result
)
{
   int         row, col, row2, col2;
   MBASE_TYPE  term;

   for (row = 0; row < N1; row ++)
      for (col2 = 0; col2 < N3; col2 ++)
      {
         term = (MBASE_TYPE) 0.0; 
         for (col = 0; col < N2; col ++)
               term += mat1 [row * N2 + col] * mat2 [col * N3 + col2];
         result [row * N3 + col2] = term;
      }

   return;
}

int MatrixInvertGJ
(
   MBASE_TYPE    *mat,
   int           N,
   MBASE_TYPE    *inverse
)
{
   int          row, col, ct, pivot_row, *pivot;
   MBASE_TYPE   *p1, *tmat, factor;


   /*
    * Store the original in a temp so that it is not overwritten
    * by the operations.
    */

   tmat = AllocateMatrix (N, N);
   if (tmat == NULL)
      return (-1);

   CopyMatrix (tmat, N, N, mat);

   /*
    * The inverse matrix starts as the identity matrix.
    */

   IdentityMatrix (inverse, N);

   /*
    * We need a place to store the pivot rows.  Initialize to
    * zero indicating that the row hasn't been used to pivot.
    */

   pivot = (int *) malloc (N * sizeof (int));
   if (pivot == NULL)
      return (-1);

   for (ct = 0; ct < N; ct ++)
      pivot [ct] = 0;        

   /*
    * Do the reduction column by column.
    */

   pivot_row = -1;   /* not yet set */
   for (col = 0; col < N; col ++)
   {
      /* 
       * Find a non-zero pivot row in the current column.  If none found
       * this matrix has no inverse.
       */

      for (row = 0; row < N; row ++)
         if ((fabs ((double) tmat [row * N + col]) > (double) MBASE_NEAR_ZERO) &&
             (pivot [row] == 0))
         {
            pivot_row = row;
            pivot [row] = 1;
            break;
         }

      if (pivot_row < 0)
      {
         free (pivot);
         FreeMatrix (tmat);
         return (-1);
      }

      factor = tmat [pivot_row * N + col];

      /*
       * Divide everything in the row by factor so the pivot is 1.
       */

      for (ct = 0; ct < N; ct ++)
      {
         tmat [pivot_row * N + ct] /= factor;
         inverse [pivot_row * N + ct] /= factor;
      } 

      /*
       * Reduce all the other rows
       */

      for (row = 0; row < N; row ++)
      {
         factor = tmat[row * N + col];
         if (row != pivot_row)
            for (ct = 0; ct < N; ct ++)
            {
               tmat [row * N + ct] -= factor * tmat [pivot_row * N + ct];
               inverse [row* N + ct] -= factor * inverse [pivot_row * N + ct];
            }
      }     
   }
 
   free (pivot);
   FreeMatrix (tmat);

   return (0);
}

MBASE_TYPE *AllocateMatrix
(
   int      M,
   int      N
)
{
   float   *matrix;
   int     ct;


   
   matrix = (MBASE_TYPE *) calloc (M * N,  sizeof (MBASE_TYPE *));
   if (matrix == NULL)
      return (NULL);

   return (matrix);
}

void FreeMatrix
(
   MBASE_TYPE   *matrix
)
{
   int     ct;

   free (matrix);

}

void CopyMatrix
(
   MBASE_TYPE   *target,
   int          M,
   int          N,
   MBASE_TYPE   *source
)
{
   int     row, col;

   memcpy (target, source, M * N * sizeof (MBASE_TYPE));
   return;
}

void DumpMatrix
(
   char          *name,
   MBASE_TYPE    *mat,
   int           M,
   int           N
)
{
   int           row, col;

   printf ("\n%s\n", name);
   for (row = 0; row < M; row ++)
   {
      for (col = 0; col < M; col ++)
         printf ("%.3f  ", mat [row * N + col]);
      printf ("\n");
   }

   return;
}
      
