typedef struct {
	int    max_size;           /* max number of entries */
	int    current_size;       /* current # of entries   */

	float *x, *y, *z;

	int    logarithmic;        /* flag to be set if y-axis scale should be logarithmic */

	float *total_dose;         /* to be allocated at runtime */
	int    total_dose_enabled; /* if yes, then plot this column */
	
	float *b_10;  
	int    b_10_enabled;

	float *gamma;
	int    gamma_enabled;

	float *n_14;
	int    n_14_enabled;

	float *fast;
	int    fast_enabled;

	float *gp1_fluence;
	int    gp1_fluence_enabled;

	float *gp2_fluence;
	int    gp2_fluence_enabled;

	float *thermal_fluence;
	int    thermal_fluence_enabled;

	float *gamma_prod;
	int    gamma_prod_enabled;

	float *xmgr_x_coord;

} col_struct;

	
