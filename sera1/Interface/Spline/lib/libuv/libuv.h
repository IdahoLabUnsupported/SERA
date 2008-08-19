/* This version of libuv.h frozen on 4-29-98 for Spline directory
 */

#ifndef LIBUV_H
#define LIBUV_H

/* Make a region type rather than forcing us to use a char */
typedef unsigned char region_t;

typedef struct _dose_info_valid_t {
  char boron_cf;
  char gamma_rbe;
  char nitrogen_rbe;
  char recoil_rbe;
  char hydrogen_rbe;
  char other_rbe;
  char tissue_to_blood;
  char maximum_dose;
  char constraint_dose;
} dose_info_valid_t;

typedef struct _dose_info_t {
  float boron_cf;
  float gamma_rbe;
  float nitrogen_rbe;
  float recoil_rbe;
  float hydrogen_rbe;
  float other_rbe;
  float tissue_to_blood;
  float maximum_dose;
  float constraint_dose;
  dose_info_valid_t valid;
} dose_info_t;

typedef struct _fiducial_marker_t {
  struct _fiducial_marker_type * next;
} fiducial_marker_t;

typedef struct _constraint_marker_t {
  dose_info_t dose;
  struct _constraint_marker_type * next;
} constraint_marker_t;

/* This structure will be a substructure of the
 * geom_info_t and should be initialized to all
 * 'false' (0) and set to 'true' as values are set
 */
typedef struct _geom_info_valid_t {
  char imagerowaxis;
  char imagecolumnaxis;
  char imagesliceaxis;
  char dimensionality;
  char sliceorientation;
  char imageslices;
  char imagecolumns;
  char imagerows;
  char pixelsizeslices;
  char isaxismin;
  char pixelsizecolumns;
  char pixelsizerows;
  char rlaxismin;
  char paaxismin;
  char regionnum[256];
  char mean_intensity[256];
  char color_red[256];
  char color_green[256];
  char color_blue[256];
  char bodyname[256];
  char matname[256];
  char dose[256];
  char parent_body[256];
  char bboxpaaxismin[256];
  char bboxpaaxismax[256];
  char bboxrlaxismin[256];
  char bboxrlaxismax[256];
  char bboxisaxismin[256];
  char bboxisaxismax[256];
  char volume[256];
  char uvval[256];
  char vol_arr;
  char rlaxismax;
  char paaxismax;
  char isaxismax;
  char inv_pixelsizeslices;
  char inv_pixelsizecolumns;
  char inv_pixelsizerows;
  char num_fiducial_markers;
  char num_constraint_markers;
  char fiducial_markers_list;
  char constraint_markers_list;
} geom_info_valid_t;

typedef struct _geom_info_t {
  char imagerowaxis[256];
  char imagecolumnaxis[256];
  char imagesliceaxis[256];
  char dimensionality[256];   /* mm, cm, etc. */
  char sliceorientation[256];
  int imageslices;            /* total number of slices             */
  int imagecolumns;           /* width (x), in pixels, of a slice   */
  int imagerows;              /* height (y), in pixels, of a slice  */
  float pixelsizeslices;      /* distance between slices in arbitrary units
			       * that will just be consistent in all requests
			       * for anything relating to length
			       * -- Note:  same as thickness (z) of a slice
			       */
  /* NOTE:  AS OF 3/12, GRAPHICS GEMS (247) does NOT recommend using
   * the CENTERS.  Instead, STARTs should all be OUTERMOST EDGES of
   * corresponding voxels
   */
  float isaxismin;        /* (EDGE) of first slice in image set */
  float pixelsizecolumns;
  float pixelsizerows;
  float rlaxismin;
  float paaxismin;
  
  int regionnum[256];
  int mean_intensity[256];
  int color_red[256];
  int color_green[256];
  int color_blue[256];
  char bodyname[256][256];
  
  char matname[256][256];
  dose_info_t dose[256];
  char parent_body[256][256];
  float bboxpaaxismin[256];
  float bboxpaaxismax[256];
  float bboxrlaxismin[256];
  float bboxrlaxismax[256];
  float bboxisaxismin[256];
  float bboxisaxismax[256];
  float volume[256];          /* in current units */

  /* The following are _derived_ quantities not in the .uvh */
  int uvval[256];
  unsigned char *vol_arr;     /* ptr to all the volume information     */
  float rlaxismax;            /* Parallels to the mins                 */
  float paaxismax;
  float isaxismax;
  float inv_pixelsizeslices;  /* Inverses for quick divides            */
  float inv_pixelsizecolumns;
  float inv_pixelsizerows;
  int num_fiducial_markers;
  int num_constraint_markers;
  fiducial_marker_t * fiducial_markers_list;
  constraint_marker_t * constraint_markers_list;

  /* Use this to keep track of what's valid, what's not */
  geom_info_valid_t valid;
} geom_info_t;

/* Prototypes */
void read_uvh_minimal(geom_info_t *, char *);
void read_uvh(geom_info_t *, char *);
void read_uv(geom_info_t *, char *);
void write_uvh(geom_info_t *, char *);
void write_uv(geom_info_t *, char *);
void initialize_empty_slices(geom_info_t *);
void free_geom(geom_info_t * geom_ptr);
geom_info_t * get_geometry_ptr(void);
void get_geometry(geom_info_t *, char *);
void make_regions(geom_info_t *);
void cp_to_vox(geom_info_t *, int, float *, int, char *, float);
void process_resource_file(geom_info_t *, int *, float *);

/*** Cory's ***/
void process_files_for_univels(char *fname);

/* Never know if Fortran wants fcns to end in "__", "_", or "" */
#ifdef TWO_UNDERSCORE
#define IRE_NAME init_ray_environ__
#define IG_NAME interrogate_geometry__
#else
#ifdef ONE_UNDERSCORE
#define IRE_NAME init_ray_environ_
#define IG_NAME interrogate_geometry_
#else
#define IRE_NAME init_ray_environ
#define IG_NAME interrogate_geometry
#endif
#endif

#define INEL_BUFF_SIZE 256

void IRE_NAME (int *, double *, char[], char[], int, int);

void IG_NAME (double *, double *, double *,
	      int *, int *, int *);

#endif
