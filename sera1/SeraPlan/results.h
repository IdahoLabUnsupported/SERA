#define MAX_XY          40
#define MAX_RESOLUTION 256

/*
 *  Structures to store the edit results
 */

/*
 *  Points are just single data points;
 */
typedef struct {
   double  *data;
} point_res;

/*
 *  Lines are sets of MAX_RESOLUTION data points
 */
typedef struct {
   double  *data;
} line_res;

/*
 *  Boxes are sets of MAX_NNED data points
 *  Use this array to store data for all DV_histogram edits
 */
typedef struct {
   double  *data[MAX_REGIONS+MAX_BOXES];
   double  *dose_max[MAX_REGIONS+MAX_BOXES];
   double  *max_loc[MAX_REGIONS+MAX_BOXES];
   double  *dose_min[MAX_REGIONS+MAX_BOXES];
   double  *min_loc[MAX_REGIONS+MAX_BOXES];
   double  *dose_mean[MAX_REGIONS+MAX_BOXES];
   double  *dose_ref[MAX_REGIONS+MAX_BOXES];
   float   *dv_bins[MAX_REGIONS+MAX_BOXES];
   double  *dose_avg[MAX_REGIONS+MAX_BOXES];
} box_res;

/*
 *  Surfaces (both kinds) are sets of MAX_RESOLUTION x MAX_RESOLUTION data points
 */
typedef struct {
   double  *data[MAX_CONTOURS];
} surf_res;


typedef struct {
   double     *ref_loc, *ref_dose;
   point_res  *points;
   line_res   *lines;
   box_res    *boxes;
   surf_res   *surfaces;
} edit_results_struct;
