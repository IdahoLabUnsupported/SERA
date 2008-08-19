#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "dose_vol_struct.h"
#include "memory.h"

/*
 * This routine initially allocates room for storing columns of size 
 * 20.
 */
void initialize (dose_vol_struct *data)
{
	data->max_size = 20;
	data->current_size = 0;

	data->histogram_bin = (bin *) malloc (data->max_size * sizeof (bin));

	data->total_dose = (float *) malloc (data->max_size * sizeof (float));
	data->total_dose_sum = (float *) malloc (data->max_size * sizeof (float));
	data->total_dose_enabled = 0;

	data->b10_dose = (float *) malloc (data->max_size * sizeof (float));
	data->b10_dose_sum = (float *) malloc (data->max_size * sizeof (float));
	data->b10_dose_enabled = 0;

	data->gamma_dose = (float *) malloc (data->max_size * sizeof (float));
	data->gamma_dose_sum = (float *) malloc (data->max_size * sizeof (float));
	data->gamma_dose_enabled = 0;

	data->n14_dose = (float *) malloc (data->max_size * sizeof (float));
	data->n14_dose_sum = (float *) malloc (data->max_size * sizeof (float));
	data->n14_dose_enabled = 0;

	data->fast_dose = (float *) malloc (data->max_size * sizeof (float));
	data->fast_dose_sum = (float *) malloc (data->max_size * sizeof (float));
	data->fast_dose_enabled = 0;

	data->fast_flux = (float *) malloc (data->max_size * sizeof (float));
	data->fast_flux_sum = (float *) malloc (data->max_size * sizeof (float));
	data->fast_flux_enabled = 0;

	data->thermal_flux = (float *) malloc (data->max_size * sizeof (float));
	data->thermal_flux_sum = (float *) malloc (data->max_size * sizeof (float));
	data->thermal_flux_enabled = 0;

	data->ultra_fast_dose = (float *) malloc (data->max_size * sizeof (float));
	data->ultra_fast_dose_sum = (float *) malloc (data->max_size * sizeof (float));
	data->ultra_fast_dose_enabled = 0;

		
}










/*
 * This procedure reallocates space for columns as nessary.  Doubles
 * as necessary.
 */
void mem_reallocate (dose_vol_struct *data)
{


	data->max_size *=2;

	data->histogram_bin = realloc (data->histogram_bin, 
					data->max_size * sizeof(bin));


	data->total_dose = realloc (data->total_dose, 
					data->max_size * sizeof(float));
	data->total_dose_sum = realloc (data->total_dose_sum, 
					data->max_size * sizeof(float));

	data->b10_dose = realloc (data->b10_dose, 
					data->max_size * sizeof(float));
	data->b10_dose_sum = realloc (data->b10_dose_sum, 
					data->max_size * sizeof(float));

	data->gamma_dose = realloc (data->gamma_dose, 
					data->max_size * sizeof(float));
	data->gamma_dose_sum = realloc (data->gamma_dose_sum, 
					data->max_size * sizeof(float));

	data->n14_dose = realloc (data->n14_dose, 
					data->max_size * sizeof(float));
	data->n14_dose_sum = realloc (data->n14_dose_sum, 
					data->max_size * sizeof(float));

	data->fast_dose = realloc (data->fast_dose, 
					data->max_size * sizeof(float));
	data->fast_dose_sum = realloc (data->fast_dose_sum, 
					data->max_size * sizeof(float));

	data->fast_flux = realloc (data->fast_flux, 
					data->max_size * sizeof(float));
	data->fast_flux_sum = realloc (data->fast_flux_sum, 
					data->max_size * sizeof(float));

	data->thermal_flux = realloc (data->thermal_flux, 
					data->max_size * sizeof(float));
	data->thermal_flux_sum = realloc (data->thermal_flux_sum, 
					data->max_size * sizeof(float));

	data->ultra_fast_dose = realloc (data->ultra_fast_dose, 
					data->max_size * sizeof(float));
	data->ultra_fast_dose_sum = realloc (data->ultra_fast_dose_sum, 
					data->max_size * sizeof(float));

	
}
