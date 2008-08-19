/* =========================================================> contours_calc.c
 *
 * Syntax: ContourCalculate (floyd_data_ptr *data, 
 *                           contours_data_type *cntr
 *                           int which_dose
 *                          )
 *     
 * Calculate contours for data, given the parameters in cntr and the
 * dose type in which_dose. Store the mesh of points in cntr. 
 *
 * Compile options -   DEBUG - prints out debugging information, including
 *                             mesh points.
 * 
 * Gary Harkin
 * 
 * ============================================================================
 */

#include <stdio.h>
#include <math.h>
#include <strings.h>
#include <errno.h>



#include "contours_calc.h"
#include "global.h"

#define  UP        0
#define  DOWN      1
#define  RIGHT     2
#define  LEFT      3

#define POS_SENSE 0
#define NEG_SENSE 1

#define LEFT_HAND_MOVE   0
#define RIGHT_HAND_MOVE  1

/*
 * Global data structures used to temporarily store data.
 */

mesh_type    mt /*, *seg*/;

/*
 * Prototypes 
 */

int CntrTraceBlocks (input_data_type *, char *, int, int, 
      contour_data_type *, mesh_type *); 
int CntrTraceStart (char *, int, int, float, int *, int *, int, int *);
int CntrPickMovedir (char, int);
char *CntrBlockType (input_data_type *, int, int, contour_data_type *);
int CntrNextBlock (int, int, char, int *);
int CntrMoveToBlock (int, int, int, int *, int *);
int CntrCalcKnot (input_data_type *, int, int, int, int, int, float,
       mesh_type *);
int CntrTrace (input_data_type *, char *, int, int, int, int, int, 
      float, mesh_type *);
int CntrTest5_10 (char *, int, int);

int CntrPositiveMove (int);
int CntrNegativeMove (int);
int CntrSenseMove (int, int);
int CntrWhatIsSense (int, int);
void CntrChoiceMove (int, int, int *, int *);

int CntrMoveDir (char, int);
int CntrReverseDir (int);
void CntrPrint (contour_data_type *, int);
void CntrPrintBlockTypes ( char *, int, int);

int ContoursCalculate
(
   floyd_data_ptr     fdata,
   contour_data_type  *contours,
   int                ncntr,
   int                which_dose
)
{
   int                    nrow, ncol, ct;
   floyd_data_ptr         fp;
   input_data_type        data, *dp;
   float                  *dx, *dy, *dz, *dv;
   char                   str [80];
   
   
   /*
    * The size of the data array is given by NUM_X_POINTS and NUM_Y_POINTS
    * which are global data values (in global.h).  To avoid potential,
    * problems, local variables will be used throughout.
    */

   nrow = NUM_X_POINTS;
   ncol = NUM_Y_POINTS;

   /*
    * Allocate space and convert the floyd_data_ptr input data to the
    * internal form needed.  This includes selecting only the data
    * specified by which_dose.
    */
    

   data . x = (float *) MT_malloc (nrow * ncol * sizeof (float));
   data . y = (float *) MT_malloc (nrow * ncol * sizeof (float));
   data . z = (float *) MT_malloc (nrow * ncol * sizeof (float));
   data . v = (float *) MT_malloc (nrow * ncol * sizeof (float));
   fp = fdata;
   dx = data . x;
   dy = data . y;
   dz = data . z;
   dv = data . v;
   

   
   for (ct = 0; ct < nrow * ncol; ct ++)
   {                  
      *(dx++) = /*fp[ct].x;*/   fp -> x;
      *(dy++) = /*fp[ct].y;*/   fp -> y;
      *(dz++) = /*fp[ct].z;*/   fp -> z;
      
      switch (which_dose)
      {
         case 0:      /* Boron Dose */
            *dv ++ = fp -> boronDose;
            break;

         case 1:      /* Gamma Dose */
            *dv ++ = fp -> gammaDose;
            break;

         case 2:      /* Nitrogen Dose */
            *dv ++ = fp -> nitrogenDose;
            break;

         case 3:      /* Fast Dose */
            *dv ++ = fp -> fastDose;
            break;

         case 4:      /* Group 1 Fluence */
            *dv ++ = fp -> group1Fluence;
            break;

         case 5:      /* Group 2 Fluence */
            *dv ++ = fp -> group2Fluence;
            break;

         case 6:      /* Thermal Fluence */
            *dv ++ = fp -> thermalFluence;
            break;

         case 7:      /* Other Dose */
            *dv ++ = fp -> otherDose;
            break;

         case 8:      /* Total Dose */
            *dv ++ = fp -> totalDose;
            break;

         default:      /* Nothing we know about */
            CntrError ("Illegal dose type requested", "ContoursCalculate");
            break;

      }
      fp ++;
   }

   /*
    * Note that the contour data structure must already be initialized
    * with the number of contours, the values to be used for each contour,
    * the color, the label properties and so on.
    *
    * Go calculate the contour mesh values.
    */

   if (CntrCalc (&data, nrow, ncol, contours, ncntr) < 0)
   {
      CntrError ("Aborting", "CntrMain");
      exit (0);
   }

   MT_free(data . x);
   MT_free(data . y);
   MT_free(data . z);
   MT_free(data . v);

/*
#ifdef DEBUG
   CntrPrint (contours, ncntr);
#endif
*/

}

int CntrCalc
(
   input_data_type     *data, 
   int                 nrow, 
   int                 ncol, 
   contour_data_type   *contours,
   int                 ncntr
)
{
   char                   *bdata;
   contour_data_type      *cp;
   int                    ct;
   mesh_type             *seg;

   /*
    * Algorithm based on ideas from "3-D Surface Contours" by Schroeder and
    * Lorensen, Dr. Dobbs Journal, July, 1996.
    *
    * This alorithm assumes a uniform grid of points.  First, analyze the
    * data and get the get the block types from the data.  Then, calculate
    * the crossing points and create a list of points to be used as the
    * fixed points for a spline curve fit.
    */

   /*
    * Create static arrays for temporarily storing data until sizes
    * are known.  These are used in CntrTraceBlocks, but don't need to
    * be reallocated and freed each time.
    */

   mt . me_x = (float *) MT_malloc ((nrow * ncol / 4) * sizeof (float));
   mt . me_y = (float *) MT_malloc ((nrow * ncol / 4) * sizeof (float));
   seg = (mesh_type *) MT_malloc (nrow * 2 * sizeof (mesh_type));

   if ((mt . me_x == NULL) || (mt . me_y == NULL) || (seg == NULL))
   {
      CntrError ("Could not allocate temporary data structures", "CntrCalc");
      return (-1);
   }

   cp = contours;
   for (ct = 0; ct < ncntr; ct++)
   {

#ifdef DEBUG
printf ("\nContour value = %.2f\n", cp -> cd_value);
#endif
      if ((bdata = CntrBlockType (data, nrow, ncol, cp)) == NULL)
      {
         MT_free ((void *)mt . me_x);
         MT_free ((void *)mt . me_y);
         MT_free ((void *)seg);
#ifdef DEBUG_L2
CntrError ("CntrBlockType failed", "CntrCalc");
#endif
         return (-1);
      }
#ifdef DEBUG
CntrPrintBlockTypes (bdata, nrow, ncol);
#endif

      if (CntrTraceBlocks (data, bdata, nrow, ncol, cp, seg) < 0)
      {
         MT_free ((void *)bdata);
         MT_free ((void *)mt . me_x);
         MT_free ((void *)mt . me_y);
#ifdef DEBUG_L2
CntrError ("CntrTraceBlocks failed", "CntrCalc");
#endif
         return (-1);
      }

      MT_free ((void *)bdata);
      cp ++;
   }
   
   MT_free ((void *)mt . me_x);
   MT_free ((void *)mt . me_y);
   MT_free ((void *)seg);

   return (0);
}

char *CntrBlockType 
(
   input_data_type    *data,
   int                nrow,
   int                ncol,
   contour_data_type  *cntr
)
{
   char                *bdata, *bdp;
   int                 row, col, ct;
   float               value;
   float               *ul, *ll, *ur, *lr;  /* upper left, lower left, .. */
   contour_data_type   *cp;

   /*
    * Allocate an array to hold the block types.
    */

   bdata = (char *) MT_malloc ((nrow - 1) * (ncol - 1));

   /*
    * Assume row major order.  Set up pointers to the four corners of a
    * grid block.  Test to determine what arrangement of the four corners
    * is present (see the article) and set bdata to be the mask.  Although
    * only 4 bits are needed for the block type, a fifth bit is used later
    * so a full 8 bits is used, which wastes some space.
    *
    * In the following, 0 marks vertics above the contour value
    *
    * Case  0      1      2      3      4      5      6      7
    *     .---.  .---.  .---.  .---.  .--xo  .-x-o  .-x-o  .-x-o
    *     |   |  x   |  |   x  x   x  |   x  x   x  |   |  x   |
    *     .---.  o-x-.  .-x-o  o---o  .---.  o-x-.  .-x-o  o---o
    *
    * Case  8      9      10     11     12     13     14     15 
    *     o-x-.  o-x-.  o-x-.  o-x-.  o---o  o---o  o---o  o---o
    *     x   |  |   |  x   x  |   x  x   x  |   x  x   |  |   |
    *     .---.  o-x-.  .-x-o  o---o  .---.  o-x-.  .-x-o  o---o
    *
    */   
   
   /*
    * Set up a pointer to the bdata mask array and to the four corners
    * of the block.
    */

   bdp = bdata;
   ul = data -> v;
   ll = ul + ncol;
   ur = ul + 1;
   lr = ll + 1;


   value = cntr -> cd_value;
   for (row = 0; row < nrow - 1; row ++)
   {
      for (col = 0; col < ncol - 1; col ++)
      {
         *bdp = 0;
         if (*ll > value)
            *bdp |= 1;
         if (*lr > value)
            *bdp |= 2;
         if (*ur > value)
            *bdp |= 4;
         if (*ul > value)
            *bdp |= 8;

#ifdef DEBUG
printf ("     %d %d - %f %f %f %f %f -- case = %d\n", 
             row, col, *ul, *ur, *ll, *lr, value, *bdp);
#endif

         /*
          * 5 and 10 are special cases that have two possible configurations.
          * The fifth bit is used to indicate that they are as yet, not
          * determined.  When determined, the fifth bit indicates that
          * the slopes are positive (0) or negative (1).  For the time being,
          * leave as zero.
          */

         /*
          * Increment to the next block.
          */

         bdp ++;
         ul ++;
         ur ++;
         ll ++;
         lr ++;
      }
 
      /*
       * End of row, add another increment to wrap around to the next row,
       * since the loop goes to ncol minus one.
       */

      ul ++;
      ur ++;
      ll ++;
      lr ++;

   }

   return (bdata);
}

int CntrTraceBlocks
(
   input_data_type     *data,
   char                *bdata,
   int                 nrow,
   int                 ncol,
   contour_data_type   *cntr,
   mesh_type           *seg
)
{
   int                result, srow, scol, movedir, tmove, stype;
   char               btype;
   float              value;
   mesh_type   *sp;

   /*
    * Find starting points for this value until all separate contours
    * are accounted for.
    */

   sp = seg;
   cntr -> cd_nseg = 0;
   value = cntr -> cd_value;
   srow = 0;
   scol = 0;
   stype = 0;
   while (1)
   {
      stype = CntrTraceStart (bdata, nrow, ncol, value, &srow, &scol, stype,
        &movedir);

      if (stype < 0)
      {

         /*
          * Now move everything from seg to cntr, now that the size is known.
          */

         if (cntr -> cd_nseg > 0)
         {
            cntr -> cd_mesh = (mesh_type *) MT_malloc (cntr -> cd_nseg *
                  sizeof (mesh_type));
            if (cntr -> cd_mesh == NULL)
            {
               CntrError ("Could not allocate contour space", 
                     "CntrTraceBlocks");
               return (-1);
            }
   
            memcpy (cntr -> cd_mesh, seg, 
                  cntr -> cd_nseg * sizeof (mesh_type));
         }

         return (0);
      }

      /*
       * Calculate the intersection with the starting grid line.
       */

      btype = *(bdata + srow * (ncol - 1) + scol);
      tmove = CntrReverseDir (movedir);
      tmove = CntrMoveDir (btype, tmove);
      mt . me_npt = 0;
      CntrCalcKnot (data, nrow, ncol, tmove, srow, scol, value, &mt);
   
      /*
       * Trace the contour.  Either is closes on itself, or both ends terminate
       * at boundaries.  There are no other legal possibilities.  From the
       * starting block, there are two possible directions.  If the contour
       * closes, only one must be traced, otherwise, both must be.  Pick the
       * first one arbitrarily.
       */

#ifdef DEBUG
printf ("Tracing contour = %.2f\n", value);
#endif

      *(bdata + srow * (ncol - 1) + scol) |= 0x20;   /* mark as visited */
      result = CntrTrace (data, bdata, nrow, ncol, srow, scol, movedir, 
            value, &mt);
      if (result == -2)
      {
#ifdef DEBUG_L2
CntrError ("CntrTrace failed", "CntrTraceBlocks");
#endif
         return (-1);
      }

      if ((result == -1) && (stype == 4))
      {
         CntrError ("Interior contour terminates at boundary", 
               "CntrTraceBlocks");
         /* return (-1); */
         /* return (0); */
      }

      /*
       * mt now contains the knot vector values.  Allocate space in
       * the contour descriptor and copy the knot vector.
       */

      sp -> me_npt = mt . me_npt;
      cntr -> cd_nseg ++;

      sp -> me_x = (float *) MT_malloc (mt . me_npt * sizeof (float));
      if (sp -> me_x == NULL)
      {
         CntrError ("Could not allocate knot vector space", "CntrTraceBlocks");
         return (-1);
      }
      memcpy (sp -> me_x, mt . me_x, mt . me_npt * sizeof (float));

      sp -> me_y = (float *) MT_malloc (mt . me_npt * sizeof (float));
      if (sp -> me_y == NULL)
      {
         CntrError ("Could not allocate knot vector space", "CntrTraceBlocks");
         return (-1);
      }
      memcpy (sp -> me_y, mt . me_y, mt . me_npt * sizeof (float));
      
      sp ++;
   }

   /*
    * Now move everything from seg to cntr, now that the size is known.
    */

   /*cntr->cd_mesh = ( mesh_type * ) MT_malloc ( cntr->cd_nseg * sizeof(mesh_type) );

   if (cntr -> cd_mesh == NULL)
   {
      CntrError ("Could not allocate contour space", "CntrTraceBlocks");
      return (-1);
   }

   memcpy (cntr -> cd_mesh, seg, cntr -> cd_nseg * sizeof (mesh_type));

   return (0);*/
}

/* ============================================================> CntrTrace
 *
 * Recursive routine to trace the countour. 
 *
 * Return:     -2  = error
 *             -1  = boundary reached
 *             1,2,3,4 = direction of last move
 *             For the initial call, the direction of last move is
 *             useless, but it is necessary to handle the recursive
 *             tracing of cases 5 and 10.
 *              
 *
 * Case 5 and 10 have intersections on all four sides, and therefore,
 * have two internal line segments, but the direction is not decidable
 * from just the information in the single block. In this phase, 
 * make a determination and set the fifth bit to indicate the chosen
 * slope.
 *
 * The difference is that one slope will give a single contour going
 * through the block in different directions and the other will give
 * two distinct contour lines (a saddle point).  The one that is right
 * may not ever be decidable, since the values inside the block are
 * unknown.  The solution is to try to guess the correct direction
 * based on surrounding blocks (not effective) or to simply guess and
 * if no anomalies (contours that cross) are created, do nothing.  The
 * visual error is slight and as pointed out, the data resolution
 * is not great enough to decide what is right anyway.  So here's the
 * strategy.
 *
 * Trying to create a single continuous contour eliminates possible
 * anomolies, so try to find the solution that gives a single contour.
 *
 *
 * =========================================================================
 */

int CntrTrace
(
   input_data_type   *data,
   char              *bdata,
   int               nrow,
   int               ncol,
   int               srow,
   int               scol,
   int               smove,
   float             value,
   mesh_type  *mt
)
{
   int       row, col, move, tmove, lastmove;
   int       sense;
   char      *bdp;

   /*
    * The mechanics of the trace are simple moving from block to block,
    * except for the case 5 and 10 blocks.
    */

   lastmove = -1;
   row = srow;
   col = scol;
   move = smove;
   if (mt != NULL)
      CntrCalcKnot (data, nrow, ncol, move, row, col, value, mt);
   
   do
   {

#ifdef DEBUG
printf ("     Move %d at row = %d col = %d\n", move, row, col);
#endif
      /*
       * Make the desired move.  Then check to see if the new location is
       * an indeterminate case (5 or 10).  If so, handle the recursive
       * trace of the subcontour.
       */
      
      if (CntrMoveToBlock (nrow, ncol, move, &row, &col) < 0)
      {
#ifdef DEBUG_L2
CntrError ("CntrMoveToBlock failed", "CntrTrace");
#endif
         return (-1);
      }
      if ((row == srow) && (col == scol))
         return (move);

      bdp = bdata + row * (ncol - 1) + col;

      /*
       * Test to see if this is a case 5 or 10.  If so, the routine
       * to trace indeterminate cases. It returns -2 for an error,
       * or sense of the direction to move.
       */

      if (((*bdp & 0x0F) == 5) || ((*bdp & 0x0F) == 10))
      {
         tmove = CntrTest5_10 (bdp, move, lastmove);
         if (tmove < 0)
         {
#ifdef DEBUG_L2
CntrError ("CntrNextBlock failed", "CntrTrace");
#endif
            return (-2);
         }
      }
      else
      {
         /* 
          * Things other than 5 or 10 - decide the next move.
          */

         if (CntrNextBlock (nrow, ncol, *bdp, &move) < 0)
         {
#ifdef DEBUG_L2
CntrError ("CntrNextBlock failed", "CntrTrace");
#endif
            return (-2);
         }
      }
 

      /*
       * Mark the node visited and find the intersection point for the 
       * exit move.
       */

      lastmove = move;
      *bdp |= 0x20;  
      if (mt != NULL)
         CntrCalcKnot (data, nrow, ncol, move, row, col, value, mt);

   }  while (1);
}

int CntrTest5_10 
(
   char     *bdp, 
   int      move, 
   int      lastmove
)
{
   int      tmove, sense;

   /* 
    * For 5 and 10 blocks there are three possibilities.
    *
    *    1. If its already been visited, use the previously
    *       selected direction.
    *    2. Not visited, so choose the same slope sense 
    *       as the move that got us here.
    *    3. If not 1 and there is no previous move, choose
    *       the left-hand strategy.  Go in the left-hand
    *       direction.
    */

   /* 
    * Already visited case 
    */

   if ((*bdp & 0x20) == 0)
   {
      /*
       * Move bit, 0 = positive direction, 1 = negative.
       */

      if ((*bdp & 0x10) == 0)
         tmove = CntrPositiveMove (move);
      else
         tmove = CntrNegativeMove (move);
   }
   else
   {
      /* 
       * Not yet visited case.
       */

      if (lastmove >= 0)
      {
         sense = CntrWhatIsSense (lastmove, move);
         if (sense < 0)    /* Choose arbitrarily */
         {
            CntrChoiceMove (move, LEFT_HAND_MOVE, &tmove, &sense);
            sense = CntrWhatIsSense (lastmove, move);
            if (sense = 0)
            {
               CntrError ("Internal error on sense choice", "CntrTrace");
               return (-2);
            }
         }
      }
      else
      {         
         /* 
          * First pass through the loop, so no last move.  Use
          * left-hand sense.
          */

           CntrChoiceMove (move, LEFT_HAND_MOVE, &tmove, &sense);
           sense = CntrWhatIsSense (lastmove, move);
           if (sense = 0)
           {
              CntrError ("Internal error on sense choice", "CntrTrace");
              return (-2);
           }
      }

      /*
       * 5 and 10 blocks may be visited again, so set the sense of the
       * direction sense used (positive or negative slope).
       */

      if (sense == 0)
         *bdp |= 0x10;

   }  /* end not previously visited */
            
   return (tmove);
}

 
int CntrMoveToBlock 
(
   int                nrow, 
   int                ncol, 
   int                move,
   int                *row, 
   int                *col
)
{
   /*
    * Make the changes in row and column to execute the desired move.
    * Return -1 if a boundary is reached, 0 otherwise.
    */

   switch (move)
   {
      case 0:    /* UP */
         (*row) --;
         break;

      case 1:    /* DOWN */
         (*row) ++;
         break;

      case 2:    /* RIGHT */
         (*col) ++;
         break;

      case 3:    /* LEFT */
         (*col) --;
         break;

   }
   if ((*row < 0) || (*row > nrow - 2) ||
       (*col < 0) || (*col > ncol - 2))
      return (-1);

   return (0);
}

int CntrCalcKnot
(
   input_data_type    *data,
   int                nrow, 
   int                ncol, 
   int                move,
   int                row, 
   int                col,
   float              value,
   mesh_type          *mt
)
{
   long               ur, ul, lr, ll, k;
   float              x1, x2, y1, y2, v1, v2, prop;

   /*
    * Calculate the intersection in the direction the move is being made.
    * ur, ul, lr, ll and k are the offsets to the x and y locations
    * in the data, and (k) the offset in the knot vectors.
    */

   ul = row * ncol + col;
   ur = ul + 1;
   ll = ul + ncol;
   lr = ll + 1;
   k = mt -> me_npt; 
   

   switch (move)
   {
      case 0:    /* UP */
         v1 = *(data -> v + ul);
         v2 = *(data -> v + ur);
         prop = (value - v1) / (v2 - v1);

         x1 = *(data -> x + ul);
         x2 = *(data -> x + ur);

         y1 = *(data -> y + ul);
         y2 = *(data -> y + ur);
         
         break;

      case 1:    /* DOWN */
         v1 = *(data -> v + ll);
         v2 = *(data -> v + lr);
         prop = (value - v1) / (v2 - v1);

         x1 = *(data -> x + ll);
         x2 = *(data -> x + lr);

         y1 = *(data -> y + ll);
         y2 = *(data -> y + lr);
         
         break;

      case 2:    /* RIGHT */
         v1 = *(data -> v + ur);
         v2 = *(data -> v + lr);
         prop = (value - v1) / (v2 - v1);

         x1 = *(data -> x + ur);
         x2 = *(data -> x + lr);

         y1 = *(data -> y + ur);
         y2 = *(data -> y + lr);
         
         break;

      case 3:    /* LEFT */
         v1 = *(data -> v + ul);
         v2 = *(data -> v + ll);
         prop = (value - v1) / (v2 - v1);

         x1 = *(data -> x + ul);
         x2 = *(data -> x + ll);

         y1 = *(data -> y + ul);
         y2 = *(data -> y + ll);
         
         break;

   }

  *(mt -> me_x + k) = x1 + prop * (x2 - x1);
  *(mt -> me_y + k) = y1 + prop * (y2 - y1);
   mt -> me_npt ++; 

#ifdef DEBUG
printf ("     %d - %d %d - %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f %ld\n", 
             move, row, col, v1, v2, x1, x2, y1, y2, value,
         *(mt -> me_x + k), *(mt -> me_y + k), k);
#endif

   return (0);
}

int CntrNextBlock 
(
   int       nrow,
   int       ncol,
   char      btype,    
   int       *move
)
{
   int       outmove;

   /*
    * Based on the btype of the block at row and col, determine the next 
    * move and return the results in row and col. Return -2 for an illogical
    * situation.
    */


   outmove = CntrMoveDir (btype, *move);
   if (outmove == -2)
   {
#ifdef DEBUG_L2
CntrError ("Illogical situation", "CntrNextBlock");
#endif
      return (-2);
   }
   if (outmove == -1)
      return (0);

   *move = outmove;

   return (0);    /* normal move */
}

int CntrTraceStart 
(
   char       *bdata, 
   int        nrow, 
   int        ncol, 
   float      value, 
   int        *row, 
   int        *col,
   int        smode,
   int        *move
)
{
   char       *bdp;

   /*
    * Find a block with an edge in it.  But don't use a 5 or 10 case block
    * since it has 4 intersections.  Note that the block has to be "unvisited",
    * that is, not already on a contour line.  The strategy is to look around
    * the outside edge for boundary terminations and trace those first, and
    * then to look for closed contours.  row and col insure that each
    * search starts where the last one ended. smode keeps track of the
    * current mode.
    *   0 = top row
    *   1 = right side
    *   2 = bot
    *   3 = left side
    *   4 = interior 
    */

   
/*
   if (*row == 0)
      smode = 0;
   else
      if (*row == (nrow - 2))
         smode = 2;
      else
         if (*col == 0)
            smode = 3;
         else
            if (*col == (ncol - 2))
               smode = 1;
            else
               smode = 4;
*/
   do
   {
      bdp = bdata + *row * (ncol - 1) + *col;
      if ((*bdp != 0) && (*bdp != 15) &&
          (*bdp != 5) && (*bdp != 10) &&
          ((*bdp & 0x20) == 0))
         if ((*move = CntrPickMovedir (*bdp, smode)) >= 0)
            return (smode);
            

      /*
       * Pick the next block.
       */

      if (smode == 0)
      {
         (*col) ++;
         if (*col == (ncol - 1))
         {
            smode = 1;
            *row = 0;
            *col = ncol - 2;
         }
      }
      
      if (smode == 1)
      {
         (*row) ++;
         if (*row == (nrow - 2))
         {
            smode = 2;
            *row = nrow - 2;
            *col = ncol - 1;
         }
      }
      
      if (smode == 2)
      {
         (*col) --;
         if (*col < 0)
         {
            smode = 3;
            *row = 0;
            *col = 0;
         }
      }
      
      if (smode == 3)
      {
         (*row) ++;
         if (*row == (nrow - 2))
         {
            smode = 4;
            *row = 1;
            *col = 0;
         }
      }
   
      if (smode == 4)
      {
         (*col) ++;
         if (*col == (ncol - 2))
         {
            *col = 1;
            (*row) ++;
            if (*row == (nrow - 2))
               return (-1);
         }
      }

   }  while (1);
      
   /*return (smode);*/
}

int CntrPickMovedir
(
   char     btype,
   int      mode
)
{

   /*
    * Choose the direction for the trace to move.  For mode 0-3, there is
    * only one depending on the mode and the block type.  For mode 4, there
    * are two, choose one arbitrarily.
    */

   switch (mode)
   {
      case 0:
         switch (btype)
         {   
            case 4:
            case 11:
               return (RIGHT);
            case 6:
            case 9:
               return (DOWN);
            case 7:
            case 8:
               return (LEFT);
            default:
               return (-1);
         }
      case 1:
         switch (btype)
         {   
            case 2:
            case 13:
               return (DOWN);
            case 3:
            case 12:
               return (LEFT);
            case 4:
            case 11:
               return (UP);
            default:
               return (-1);
         }
      case 2:
         switch (btype)
         {   
            case 1:
            case 14:
               return (LEFT);
            case 2:
            case 13:
               return (RIGHT);
            case 6:
            case 9:
               return (UP);
            default:
               return (-1);
         }
      case 3:
         switch (btype)
         {   
            case 1:
            case 14:
               return (DOWN);
            case 3:
            case 12:
               return (RIGHT);
            case 7:
            case 8:
               return (UP);
            default:
               return (-1);
         }
      case 4:
         switch (btype)
         {   
            case 1:
            case 14:
               return (LEFT);
            case 2:
            case 13:
               return (DOWN);
            case 3:
            case 12:
               return (RIGHT);
            case 4:
            case 11:
               return (RIGHT);
            case 6:
            case 9:
               return (DOWN);
            case 7:
            case 8:
               return (UP);

            default:
               return (-1);
         }
   }
}
      

int CntrCalcPoints 
(
   input_data_type     *data,
   int                 nrow,
   int                 ncol,
   char                *bdata,
   contour_data_type   *cdata
)
{
   return (0);
}



void CntrError 
(
   char     *msg,
   char     *rtn
)
{   
   printf ("Error: %s\n", msg);
   printf ("at %s\n", rtn);
 
   return;
}

/*
 * Utility to determine the move to take out of a block given the type of
 * block and the direction used to move in.  Note that this can also be
 * used in other ways if the logic is understood.  For example, if 
 * outgoing direction is given, reversing the returned direction will
 * give the direction used to move in.
 *
 * Return value is the direction.
 */

int CntrMoveDir
(
   char     btype,
   int      inmove
)
{
   int      move;

   /*
    * Based on the btype of the block at row and col, determine the next 
    * move and return the results in row and col. Return -2 for an illogical
    * situation.
    */


   switch (btype & 0x0F)
   {
      case 0:
      case 15:
         return (-2);   /* Nowhere to go - not logical */
     
      case 1:
      case 14:
         if (inmove == UP)
            move = LEFT;
         else
            move = DOWN;
         break;
     
      case 2:
      case 13:
         if (inmove == UP)
            move = RIGHT;
         else
            move = DOWN;
         break;
     
      case 3:
      case 12:
         if (inmove == RIGHT)
            move = RIGHT;
         else
            move = LEFT;
         break;
     
      case 4:
      case 11:
         if (inmove == DOWN)
            move = RIGHT;
         else
            move = UP;
         break;
     
      case 5:
      case 10:
         /*
          * Indeterminate, unless the block has been visited.
          * Then use the direction that has been set.
          */

          if ((btype & 0x20) == 0)
             return (-1);
          switch (inmove)
          {
             case 0:
                if ((btype & 0x10) == 0)
                   move = LEFT;
                else
                   move = RIGHT;
                break;
             case 1:
                if ((btype & 0x10) == 0)
                   move = RIGHT;
                else
                   move = LEFT;
                break;
             case 2:
                if ((btype & 0x10) == 0)
                   move = UP;
                else
                   move = DOWN;
                break;
             case 3:
                if ((btype & 0x10) == 0)
                   move = DOWN;
                else
                   move = UP;
                break;
          }
          break;
     
      case 6:
      case 9:
         if (inmove == DOWN)
            move = DOWN;
         else
            move = UP;
         break;
     
      case 7:
      case 8:
         if (inmove == RIGHT)
            move = UP;
         else
            move = LEFT;
         break;
   }
   return (move);
}

int CntrReverseDir
(
   int     dir
)
{
   switch (dir)
   {
      case 0:
         return (1);
      case 1:
         return (0);
      case 2:
         return (3);
      case 3:
         return (2);
   }
}

int CntrPositiveMove 
(
   int move
)
{
   /* 
    * For a given move, what is the following move that gives
    * a positive slope.
    */

   switch (move)
   {
      case UP:
         return (RIGHT);
         break;
      case DOWN:
         return (LEFT);
         break;
      case LEFT:
         return (UP);
         break;
      case RIGHT:
         return (DOWN);
         break;
   }
}

int CntrNegativeMove 
(
   int move
)
{
   int tmove;

   /* 
    * For a given move, what is the following move that gives
    * a negative slope.
    */

   switch (move)
   {
      case UP:
         return (LEFT);
         break;
      case DOWN:
         return (RIGHT);
         break;
      case LEFT:
         return (DOWN);
         break;
      case RIGHT:
         return (UP);
         break;
   }
}

int CntrSenseMove 
(
   int   move, 
   int   sense
)
{
   /*
    * For a given move and sense, what new move will provide the
    * correct sense.
    */

   if (sense == 0)
      return (CntrPositiveMove (move));
   else
      return (CntrNegativeMove (move));

}

int CntrWhatIsSense 
(
   int     move, 
   int     lastmove
)
{
   switch (lastmove)
   {
      case UP:
         switch (lastmove)
         {
            case UP:
               return (-1);
            case DOWN:
               return (-1);   /* impossible case */
            case LEFT:
               return (NEG_SENSE);
            case RIGHT:
               return (POS_SENSE);
         }
      case DOWN:
         switch (lastmove)
         {
            case UP:
               return (-1);   /* impossible case */
            case DOWN:
               return (-1);   
            case LEFT:
               return (POS_SENSE);
            case RIGHT:
               return (NEG_SENSE);
         }
         
      case LEFT:
         switch (lastmove)
         {
            case UP:
               return (NEG_SENSE);
            case DOWN:
               return (POS_SENSE);   
            case LEFT:
               return (-1);
            case RIGHT:
               return (-1);   /* impossible case */
         }
         
      case RIGHT:
         switch (lastmove)
         {
            case UP:
               return (POS_SENSE);
            case DOWN:
               return (NEG_SENSE);   
            case LEFT:
               return (-1);   /* impossible case */
            case RIGHT:
               return (-1);
         }
         
   }
}

void CntrChoiceMove 
(
   int     move, 
   int     type,
   int     *tmove, 
   int     *sense
)
{
   /* 
    * Given the incoming direction and the handedness of the move,
    * derive the exit direction.
    */

   if (type = LEFT_HAND_MOVE)
      switch (move)
      {
         case UP:
            *tmove = LEFT;
            *sense = NEG_SENSE; 
         case DOWN:
            *tmove = RIGHT;
            *sense = NEG_SENSE;
         case LEFT:
            *tmove = DOWN;
            *sense = POS_SENSE;
         case RIGHT:
            *tmove = UP;
            *sense = POS_SENSE;
      }
   else
      switch (move)
      {
         case UP:
            *tmove = RIGHT;
            *sense = POS_SENSE;
         case DOWN:
            *tmove = LEFT;
            *sense = POS_SENSE;
         case LEFT:
            *tmove = UP;
            *sense = NEG_SENSE;
         case RIGHT:
            *tmove = DOWN;
            *sense = NEG_SENSE;
      }
}


void CntrPrint 
(
   contour_data_type     *cntr,
   int                   ncntr
)
{
   contour_data_type   *cp;
   mesh_type    *mtt;
   float               *fpx, *fpy;
   int                 nseg, kct, sct, ct;

   
   cp = cntr;
   for (ct = 0; ct < ncntr; ct ++)
   {
      mtt = cp -> cd_mesh;
      for (sct = 0; sct < cp -> cd_nseg; sct ++)
      {
         fpx = mtt -> me_x;
         fpy = mtt -> me_y;
         for (kct = 0; kct < mtt -> me_npt; kct ++)
         {
            printf ("%.2f %.2f\n", *fpx ++, *fpy ++);
         }
         printf ("\n");
         mtt ++;
      }
      cp ++;
   }

   return;
}

void CntrPrintBlockTypes 
(
   char    *bdata, 
   int     nrow, 
   int     ncol
)
{
   int     row, col;

   printf ("Block Type Output\n");
   for (row = 0; row < nrow - 1; row ++)
   {
      for (col = 0; col < ncol - 1; col ++)
      {
         printf ("%2d ", (int) *bdata);
         bdata ++;
      }
      printf ("\n");
   }
      
   return;
}
