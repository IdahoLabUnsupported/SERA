#define NUM_RBE 7
#define MAX_BODIES 25
#define MAX_REGIONS 256

#define MAX_POINTS 20
#define MAX_LINES 10
#define MAX_BOXES 10
#define MAX_CONTOURS 4

/*
 *  Structure for body list in box edit structure
 */
typedef struct {
   int   num_bodies;
   char  bodies[MAX_REGIONS][MAX_FILE];
} body_struct;

/*
 *  Structure for point edit data
 */
typedef struct {

   int     saved[MAX_POINTS], num_points;
   double  *points;

} point_edit_struct;


/*
 *  Structure for line edit data
 */
typedef struct {

   int     saved[MAX_LINES], num_lines;
   double  *delta, *line_starts, *line_ends;

} line_edit_struct;


/*
 *  Structure for box edit data
 */
typedef struct {

   int            saved[MAX_BOXES], num_boxes;
   body_struct    *bodlist;

} box_edit_struct;


/*
 *  Structure for contour edit data
 */
typedef struct {

   int     saved[MAX_CONTOURS], num_contours;
   char    *files[MAX_CONTOURS];
   double  *points, *vector1, *vector2;

} contour_struct;


/*
 *  Structure for the editing data
 */
typedef struct {

   int perf_edits;

   char dose_file[MAX_FILE];
   char plan_name[MAX_ID], patient_name[MAX_ID];
   char save_dir[MAX_FILE];

   double blood_b10;
   double ref_pt[3];
   double ref_b10;
   double ref_rbe[NUM_RBE];

   body_struct ref_regions;

   int n_avg, n_bin, upper_dv;
   int ref_opt, ref_dose_opt;
   int calc_edits;

   point_edit_struct *points;

   line_edit_struct  *lines;

   box_edit_struct   *boxes;

   contour_struct    *contours;

} edit_data_struct;
