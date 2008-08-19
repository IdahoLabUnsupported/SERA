/* This structure contains the left and right boundaries of a bin
 * in a histogram.
 */
typedef struct {
  int bin_left;
  int bin_right;
} bin;


/* This structure contains all the data necessary to compute
 * the dose/volume plot.
 */
typedef struct {
	int    max_size;           /* max number of entries */
	int    current_size;       /* current # of entries   */

	bin   *histogram_bin;

	float *total_dose;         /* to be allocated at runtime */
	float *total_dose_sum;
	int    total_dose_enabled; /* if yes, then plot this column */
	
	float *b10_dose;  
	float *b10_dose_sum;
	int    b10_dose_enabled;

	float *gamma_dose;
	float *gamma_dose_sum;
	int    gamma_dose_enabled;

	float *n14_dose;
	float *n14_dose_sum;
	int    n14_dose_enabled;

	float *fast_dose;
	float *fast_dose_sum;
	int    fast_dose_enabled;

	float *fast_flux;
	float *fast_flux_sum;
	int    fast_flux_enabled;

	float *thermal_flux;
	float *thermal_flux_sum;
	int    thermal_flux_enabled;

	float *ultra_fast_dose;
	float *ultra_fast_dose_sum;
	int    ultra_fast_dose_enabled;

} dose_vol_struct;

	
