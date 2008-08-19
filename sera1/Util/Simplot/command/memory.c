 #include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "col_struct.h"
#include "memory.h"

/*
 * This routine initially allocates room for storing columns of size 
 * 200.
 */
void initialize (col_struct *data)
{
	data->max_size = 200;
	data->current_size = 0;

	data->x = (float *) malloc (data->max_size * sizeof (float));
	data->y = (float *) malloc (data->max_size * sizeof (float));
	data->z = (float *) malloc (data->max_size * sizeof (float));
	data->total_dose = (float *) malloc (data->max_size * sizeof (float));
	data->b_10 = (float *) malloc (data->max_size * sizeof (float));
	data->gamma = (float *) malloc (data->max_size * sizeof (float));
	data->n_14 = (float *) malloc (data->max_size * sizeof (float));
	data->fast = (float *) malloc (data->max_size * sizeof (float));
	data->ultra_fast = (float *) malloc (data->max_size * sizeof (float));
	data->gp1_fluence = (float *) malloc (data->max_size * sizeof (float));
	data->gp2_fluence = (float *) malloc (data->max_size * sizeof (float));
	data->thermal_fluence = (float *) malloc (data->max_size * sizeof (float));
	data->gamma_prod = (float *) malloc (data->max_size * sizeof (float));
	data->ultra_gamma_prod = (float *) malloc (data->max_size * sizeof (float));
	data->rr1 = (float *) malloc (data->max_size * sizeof (float));
	data->rr2 = (float *) malloc (data->max_size * sizeof (float));
	data->xmgr_x_coord = (float *) malloc (data->max_size * sizeof (float));

	data->total_dose_enabled = 0;
	data->b_10_enabled = 0;
	data->gamma_enabled = 0;
	data->n_14_enabled = 0;
	data->fast_enabled = 0;
	data->ultra_fast_enabled = 0;
	data->gp1_fluence_enabled = 0;
	data->gp2_fluence_enabled = 0;
	data->thermal_fluence_enabled = 0;
	data->gamma_prod_enabled = 0;
	data->ultra_gamma_prod_enabled = 0;
	data->rr1_enabled = 0;
	data->rr2_enabled = 0;


		
}










/*
 * This procedure reallocates space for columns as nessary.  Doubles
 * as necessary.
 */
void mem_reallocate (col_struct *data)
{
	float *temp;
	int    old_size = data->max_size * sizeof(char);


	data->max_size *=2;

	data->x = realloc (data->x, data->max_size * sizeof(float));
	data->y = realloc (data->y, data->max_size * sizeof(float));
	data->z = realloc (data->z, data->max_size * sizeof(float));
	data->total_dose = realloc (data->total_dose, 
					data->max_size * sizeof(float));
	data->b_10 = realloc (data->b_10, 
					data->max_size * sizeof(float));
	data->gamma = realloc (data->gamma, 
					data->max_size * sizeof(float));
	data->n_14 = realloc (data->n_14, 
					data->max_size * sizeof(float));
	data->fast = realloc (data->fast, 
					data->max_size * sizeof(float));
	data->ultra_fast = realloc (data->ultra_fast, 
					data->max_size * sizeof(float));
	data->gp1_fluence = realloc (data->gp1_fluence, 
					data->max_size * sizeof(float));
	data->gp2_fluence = realloc (data->gp2_fluence, 
					data->max_size * sizeof(float));
	data->thermal_fluence = realloc (data->thermal_fluence, 
					data->max_size * sizeof(float));
	data->gamma_prod = realloc (data->gamma_prod, 
					data->max_size * sizeof(float));
	data->ultra_gamma_prod = realloc (data->ultra_gamma_prod, 
					data->max_size * sizeof(float));
	data->xmgr_x_coord = realloc (data->xmgr_x_coord,
					data->max_size * sizeof(float));
	
}
