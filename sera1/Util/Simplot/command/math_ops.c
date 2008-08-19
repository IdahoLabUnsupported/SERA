#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "col_struct.h"
#include "math_ops.h"

#define EPSILON_PLUS   1E-17

extern int x_log, y_log;

double sqrt (double);



/*
 * axes_extent_compute
 * 
 * This procedure computes the xmin, xmax, ymin, and ymax extents of the axes 
 * to be plotted by xmgr.  Writes those extents to the xmgr output file.
 *
 * Parameters
 * outfile:  obvious
 * data:  pointer to structure containing column data
 */
void axes_extent_compute ( FILE *outfile, col_struct *data )
{
	float x_min, x_max, temp_x_min, temp_x_max,
	      temp_y_min, temp_y_max,
	      y_min, y_max;
        float temp;

	int i;

	/* set default values for y_min and max */
	y_min = 1e+25;
	y_max = -1e25;

	/* Only need to consider xmgr field for x_min and x_max  */
	temp_x_min = col_min ( data->xmgr_x_coord, data->current_size, x_log );
	temp_x_max = col_max ( data->xmgr_x_coord, data->current_size, x_log );

        x_max = ceil ( temp_x_max );
        if ( fabs(temp_x_max) > 0.0 ) {
           temp = floor ( log10( fabs(temp_x_max)) );
           x_max = pow(10.0,temp)*( ceil(temp_x_max/pow(10.0,temp)) );
        }
  
        x_min = floor ( temp_x_min );
        if ( fabs(temp_x_min) > 0.0 ) {
           x_min = pow(10.0,temp)*( floor(temp_x_min/pow(10.0,temp)) );
        }

	fprintf (outfile, "@    world xmin %f\n",  x_min);
	fprintf (outfile, "@    world xmax %f\n",  x_max);  
	fprintf (outfile, "@    xaxis ticklabel format decimal\n");
	fprintf (outfile, "@    xaxis tick major %f\n", pow(10.0,temp));
	fprintf (outfile, "@    xaxis tick minor %f\n", pow(10.0,temp-1));

	if (data->total_dose_enabled) {
		temp_y_min = col_min ( data->total_dose, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->total_dose, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->b_10_enabled) {
		temp_y_min = col_min ( data->b_10, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->b_10, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->gamma_enabled) {
		temp_y_min = col_min ( data->gamma, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->gamma, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->n_14_enabled) {
		temp_y_min = col_min ( data->n_14, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->n_14, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->fast_enabled) {
		temp_y_min = col_min ( data->fast, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->fast, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->ultra_fast_enabled) {
		temp_y_min = col_min ( data->ultra_fast, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->ultra_fast, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->gp1_fluence_enabled) {
		temp_y_min = col_min ( data->gp1_fluence, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->gp1_fluence, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->gp2_fluence_enabled) {
		temp_y_min = col_min ( data->gp2_fluence, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->gp2_fluence, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->thermal_fluence_enabled) {
		temp_y_min = col_min ( data->thermal_fluence, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
	 	temp_y_max = col_max ( data->thermal_fluence, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->gamma_prod_enabled) {
		temp_y_min = col_min ( data->gamma_prod, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->gamma_prod, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->ultra_gamma_prod_enabled) {
		temp_y_min = col_min ( data->ultra_gamma_prod, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->ultra_gamma_prod, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->rr1_enabled) {
		temp_y_min = col_min ( data->rr1, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->rr1, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (data->rr2_enabled) {
		temp_y_min = col_min ( data->rr2, data->current_size, y_log );
		y_min = min ( y_min, temp_y_min );
		temp_y_max = col_max ( data->rr2, data->current_size, y_log );
		y_max = max ( y_max, temp_y_max );
	}

	if (y_log) {

	  /* set up y_min to be next lower power of 10, y_max to be 
	     next higher power of 10 */
	  if (y_min > 0)  {
	    if (y_min > 1) {
	      for (temp_y_min = 1.0; temp_y_min < y_min; temp_y_min *= 10.0);
	      temp_y_min /= 10.0;
	    }
	    else
	      for (temp_y_min = 1.0; temp_y_min > y_min; temp_y_min /= 10.0);
	  }
	  
	  else {
	    if (y_min < -1.0)
	      for (temp_y_min = -1.0; temp_y_min > y_min; temp_y_min *= 10.0);
	    else {
	      for (temp_y_min = -1.0; temp_y_min < y_min; temp_y_min /= 10.0);
	      temp_y_min *= 10.0;
	    }
	  }
	  
	  y_min = temp_y_min;
	  
	  if (y_max > 0)  {
	    if (y_max > 1) 
	      for (temp_y_max = 1.0; temp_y_max < y_max; temp_y_max *= 10.0);
	    else {
	      for (temp_y_max = 1.0; temp_y_max > y_max; temp_y_max /= 10.0);
	      temp_y_max *= 10.0;
	    }
	  }
	  
	  else {
	    if (y_max < -1.0) {
	      for (temp_y_max = -1.0; temp_y_max > y_max; temp_y_max *= 10.0);
	      temp_y_max /= 10.0;
	    }
	    else 
	      for (temp_y_max = -1.0; temp_y_max < y_max; temp_y_max /= 10.0);
	  }
	  
	  y_max = temp_y_max;
	  
	  fprintf (outfile, "@    world ymin %7.2e\n", y_min);
	  fprintf (outfile, "@    world ymax %7.2e\n", y_max);
	  fprintf (outfile, "@with g0\n");
	  fprintf (outfile, "@g0 type logy\n");
	  fprintf (outfile, "@    yaxis ticklabel format power\n");
	  
	}
	
	else {
	  
	  /* Expand y-axis to next nearest whole number times power of 10 *
           * down for min, up for max                                     */

	  temp_y_max = ceil ( y_max );
          if ( fabs(temp_y_max) > 0.0 ) {
             temp = floor ( log10( fabs(temp_y_max)) );
             y_max = pow(10.0,temp)*( ceil(temp_y_max/pow(10.0,temp)) );
          }
	  
	  temp_y_min = floor ( y_min );
          if ( fabs(temp_y_min) > 0.0 ) {
             y_min = pow(10.0,temp)*( floor(temp_y_min/pow(10.0,temp)) );
          }

	  fprintf (outfile, "@    world ymin %.2f\n", y_min);
	  fprintf (outfile, "@    world ymax %.2f\n", y_max);
	  fprintf (outfile, "@    yaxis ticklabel format decimal\n");
	  fprintf (outfile, "@    yaxis tick major %f\n", pow(10.0,temp));
	  fprintf (outfile, "@    yaxis tick minor %f\n", pow(10.0,temp-1));
	  
	}
	

} 










/*
 * col_min
 *
 * This procedure returns the minimum value in an array of floats. 
 *
 * Parameters:  The array (col) and its size (size).
 */
float col_min (float *col, int size, int logarithmic)
{
	int i;
	float min;

	if (logarithmic) {
		min = 1e+25;
		for (i = 0; i <= size; ++i)
			if ( (col[i] < min) && (col[i] > EPSILON_PLUS) )
				min = col[i];
	}

	else {
		min = col[0];
		for (i = 0; i <= size; ++i) 
			if ( col[i] < min )
				min = col[i];
     	}
	
	return min;
 
 }









/*
 * col_max
 *
 * This procedure returns the maximum value in an array of floats.
 *
 * Parameters:  The array (col) and its size (size).
 */
float col_max (float *col, int size, int logarithmic)
{
	int i;
	float max = col[0];

	for (i = 0; i <= size; ++i)
		if ( col[i] > max )
			max = col[i];

	return max;
}









/*
 * This prodecure computes the minimum of two numbers.
 */
 float min (float a, float b)
 {
	if (a <= b)
		return a;
	else
		return b;
}









/*
 * This procedure computes the maximum of two numbers.
 */
 float max (float a, float b)
 {
	if (a >= b)
		return a;
	else
		return b;
}









/*
 * compute_x_coord
 * 
 * This procedure computes the x coordinate that is to be written out to the .xmgr file.
 *
 * Paramters
 * data:  pointer to structure containing column data
 */

void compute_x_coord (col_struct *data)
{
	int i;

    float xdist, ydist, zdist;                   /* distances from first point */
    float dist_sq;

    for (i = 0; i <= data->current_size; ++i) {
	xdist = data->x[i] - data->x[0];
    	ydist = data->y[i] - data->y[0];
	zdist = data->z[i] - data->z[0];
	
	dist_sq = xdist*xdist + ydist*ydist + zdist*zdist;

	data->xmgr_x_coord[i] = sqrt ( (double) dist_sq );

	}

}









/*
 * renorm_dose_comps
 *
 * This procedure controls the dose component renormalization, for all
 * dose and fluence components in data.  Norm_flux allows the fluences
 * to be normalized to n/cm**2/s.
 *
 */
void renorm_dose_comps (col_struct *data, float norm, float bconc )
{
     float norm_flux;

     norm_flux = norm/60.0;

     adjust_total (data->total_dose, data->b_10, data->current_size, norm, bconc);
     renorm_boron (data->b_10, data->current_size, norm, bconc);
     renorm_dose (data->gamma, data->current_size, norm);
     renorm_dose (data->n_14, data->current_size, norm);
     renorm_dose (data->fast, data->current_size, norm);
     renorm_dose (data->ultra_fast, data->current_size, norm);
     renorm_dose (data->gp1_fluence, data->current_size, norm);
     renorm_dose (data->gp2_fluence, data->current_size, norm);
     renorm_dose (data->thermal_fluence, data->current_size, norm);
     renorm_dose (data->gamma_prod, data->current_size, norm);
     renorm_dose (data->ultra_gamma_prod, data->current_size, norm);
     renorm_dose (data->rr1, data->current_size, norm);
     renorm_dose (data->rr2, data->current_size, norm);

}









/*
 * adjust_total
 *
 * This procedure adjusts the total dose to include the renormalized boron dose,
 * including the input boron concentration.  Also normalizes the total dose.
 *
 */

void adjust_total (float *total_dose, float *b_10, int size, float norm, float bconc)
{
     int i;

     for (i=0; i <= size; ++i) {
         total_dose[i] -= b_10[i];
         total_dose[i] += b_10[i] * bconc;
         total_dose[i] *= norm;
     }

}









/*
 * renorm_boron
 *
 * This procedure renormalizes the boron dose, using the input boron concentration and
 * the normalization factor.
 *
 */
void renorm_boron (float *b_10, int size, float norm, float bconc)
{
     int i;

     for (i=0; i <= size; ++i) {
         b_10[i] *= bconc * norm;
     }

}









/*
 * renorm_dose
 *
 * This procedure renormalizes the dose and fluence components, using the input
 * normalization factor.
 *
 */
void renorm_dose (float *dose, int size, float norm)
{
     int i;

     for (i=0; i <= size; ++i) {
         dose[i] *= norm;
     }

}










/*
 * nearest_whole
 *
 * Finds nearest whole number to num in direction given by num_diff
 *
 */
float nearest_whole (float num, int num_diff)
{
      float anum;
      int inum;

      inum = num;
      if ( num-inum > 0 ) {
         anum = inum + num_diff;
         return anum;
      }
      else {
         anum = inum;
         return anum;
      }

}

