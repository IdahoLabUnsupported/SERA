/*
 * $Id: brl_oslo.c,v 1.1 1998/01/18 14:23:35 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: brl_oslo.c,v $
 * Revision 1.1  1998/01/18 14:23:35  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:46  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:37  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.1.1.1  1996/02/12  23:29:14  babcock
 * Imported sources
 *
 * Revision 1.2  1995/04/17  19:25:41  astrakan
 * Working version, going to stop keeping around subsurface patches.
 *
 * Revision 1.1  1995/03/20  23:27:06  astrakan
 * Initial revision
 *
 * Revision 1.3  1995/03/01  22:19:03  astrakan
 * Before switch back to paul's subdivision_routines.c.
 *
 *
 */


/* These routines will refine an ARL NURB knot vector (and therefore the */
/* accompanying surface) by a specified factor. */ 


#include <stdio.h>
#include <stdlib.h>



/*
 * Data structure definitions.
 */
#include "nurb.h"
#include "surface_structure_defs.h"



/*
 * Prototypes for any called functions.
 */
#include "brl_oslo.h"

/******************************************************************************
 * Routine
 * -------
 * brl_oslo 
 * BNCT Project
 * Written by John Evans, 
 ******************************************************************************
 * Purpose
 * -------
 * Uses oslo algorithm on srf_ptr->srf to refine the control net.  Only
 * the u parametric direction is actually refined, due to a quirk in the
 * nurb splitting routine in the arl nurb library, which can split a
 * FLOATing nurb into two invalid surfaces if the number of knots is low
 * enough.  So what we do is refine the knots in the u parametric
 * direction (the direction which will be FLOATing), and then use that new
 * knot vector to refine the nurb surface to the point that we don't have
 * to worry about getting invalid subsurfaces when we split the nurbs.
 *
 * The v knot vector is NOT refined, because it is assumed to be CLOSED.
 * There are no problems with splitting a surface in the direction of a
 * CLOSED knot vector.
 ******************************************************************************
 * Parameters
 * ----------
 * srf_ptr:  Points to surface type.
 * refinement_factor:    This integer tells how to refine the knot
 *           vectors.  If, for example, the original vector is
 *
 *           0 0 0 1 2 3 3 3
 * 
 *           and the refinement_factor = 3, then the knot vector will be
 *           transformed to
 * 
 *           0 0 0 3 6 9 9 9 (for all practical purposes, identical to
 *                            original)
 *
 *           and then into
 *
 *           "0 0 0" 1 2 "3" 4 5 "6" 7 8 "9 9 9"
 ******************************************************************************/
void brl_oslo ( surface_type *srf_ptr, 
		int refinement_factor )
{
  struct knot_vector *new_kv;          /* New knot vector.  */
  struct snurb *new_snurb;             /* New surface constructed out of */
				       /* the refinement. */

  int i;                               /* Index variable for looping */
				       /* through the knot vector. */



/*
 * Refine the u knot vector.
 */
  new_kv = refine_closed_kv ( &srf_ptr->srf->u_knots, 
                               refinement_factor-1, 
			       srf_ptr->srf->order[0] );

  new_snurb = (struct snurb *) rt_nurb_s_refine ( srf_ptr->srf,
						  RT_NURB_SPLIT_ROW, 
						  new_kv );
  /*
   * Free up old space and link in new surface.
   */
  rt_nurb_free_snurb ( srf_ptr->srf );
  srf_ptr->srf = new_snurb;


/*
 * The rest of the code for this procedure used to refine the v knot
 * vector, but has been commented out for now, since I think it to be 
 * unnecessary.  The subdivision routine seems to ALWAYS work ok for the
 * CLOSED parametric direction.
 */

  /*
   * Change old kv to keep unit spacing in new kv.
   */
/*
  for (i = 0;  i < srf_ptr->srf->v_knots.k_size;  ++i)
    srf_ptr->srf->v_knots.knots[i] *= (refinement_factor);

  rt_nurb_kvknot ( new_kv,
		   srf_ptr->srf->order[1], 
		   srf_ptr->srf->v_knots.knots[0],
		   srf_ptr->srf->v_knots.knots[srf_ptr->srf->v_knots.k_size-1],
		   refinement_factor * ( srf_ptr->srf->v_knots.k_size 
			     - 2*srf_ptr->srf->order[1] + 1 ) -1 );
 
			     */

  /* 
   * Construct the new surface due to knot vector refinement.
   */
/*
  new_snurb = (struct snurb *) rt_nurb_s_refine ( srf_ptr->srf, 
						  RT_NURB_SPLIT_COL, 
      						  new_kv );
						  */

  /*
   * Free up old space and link in new surface.
   */
/*
  rt_nurb_free_snurb ( srf_ptr->srf );
  srf_ptr->srf = new_snurb;
  */


}










/******************************************************************************
 * Routine
 * -------
 * refine_closed_kv
 * BNCT Project
 * Written by John Evans, 
 ******************************************************************************
 * Purpose
 * -------
 * Use this to create a new refined closed knot vector from an old.
 *
 * If old_kv = 0 1 2 3 4 5 6 7 and factor = 2,
 * it will first be transformed into
 *
 * 0 3 6 9 12 15 18 21 
 * 
 * and then into
 * 
 * 0 1 2 3 4 5 6 7 ... 18 19 20 21
 *
 * So two new knots get added into each knot inverval.
 *
 *
 * An equivalent refinement would be
 *
 * 0 0.333 0.667 1.0 1.333 1.667 2.0 ... 6.667 7.0
 *
 * but we lose the nicety of unit knot spacing that way.
 *
 ******************************************************************************
 * Parameters
 * ----------
 * old_kv : old knot vector to be refined, only in valid parameter range
 * factor : # of new knots in each valid parameter range interval
 * k      : order of nurb
 ******************************************************************************/
struct knot_vector *refine_closed_kv ( struct knot_vector *old_kv, 
				       int factor,
				       int k)
{

  struct knot_vector *new_kv;         /* new kv to be allocated               */
  int n = old_kv->k_size - k,         /* number of vertices for the kv        */
      m = n + (n-k+1)*factor,         /* number of new vertices of the kv     */
      num_new_knots = m + k,          /* total number of new knots            */
      i, j;                           /* loop indices */

  float old_kv_interval,                     /* old_kv_interval between old knots */ 
      new_kv_interval;                       /* old_kv_interval between new knots */


  /*
   * Increase spacing in the old knot vector to keep unit spacing in
   * final kv.
   */
  for (i = 0; i < old_kv->k_size; ++i)
    old_kv->knots[i] *= (factor+1);


  /*
   * Allocate space for a new knot vector. 
   */
  new_kv = (struct knot_vector *) malloc ( sizeof( struct knot_vector ) );
					
  new_kv->k_size = num_new_knots;

  new_kv->knots = (fastf_t *) malloc ( sizeof( fastf_t) * num_new_knots );




  /* 
   *copy in the lowermost and uppermost "order" knots 
   */
  for (i = 0; i < k; ++i) {
    new_kv->knots[ i ] = old_kv->knots[ i ];
    new_kv->knots[ num_new_knots -1 - i ] = old_kv->knots [n+k-1-i];
  }


  /* 
   * Fill in the new valid parameter range with the proper new
   * increments.  This is probably more complex than it needs to be, but
   * this method is very general and I believe it works for non-uniform
   * knot vectors.
   */
  for (i = 0;  i < n-k+1;  ++i) {
    old_kv_interval = old_kv->knots[ k-1 + i + 1 ] - old_kv->knots[ k-1 + i ];
    new_kv_interval = old_kv_interval / (factor + 1);

    for (j = 0; j < factor + 1; ++j)
      new_kv->knots[ k-1 + i*(factor+1) + j ] = old_kv->knots[ k-1 + i ]
	  + j*new_kv_interval;

  }


  return new_kv;

}


