#ifndef LIBUV_H
#define LIBUV_H

#define LIBUV_MAX_STRING   256   /* maximum length of strings in libuv */
#define LIBUV_MAX_REGIONS  256   /* the maximum allowed number of regions in libuv */

#define NUM_KEYWORD_LABELS 30
#define LCASE_KEYWORDS {\
    "begin",\
    "regionnum",\
    "matname",\
    "color_red",\
    "color_green",\
    "color_blue",\
    "mean_intensity",\
    "boron_cf",\
    "gamma_rbe",\
    "nitrogen_rbe",\
    "nitrogen_dens",\
    "recoil_rbe",\
    "hydrogen_rbe",\
    "hydrogen_dens",\
    "other_rbe",\
    "ultrafast_rbe",\
    "carbon_dens",\
    "oxygen_dens",\
    "tissue_to_blood",\
    "maximum_dose",\
    "constraint_dose",\
    "editable",\
    "parent_body",\
    "bboxrlaxismin",\
    "bboxrlaxismax",\
    "bboxpaaxismin",\
    "bboxpaaxismax",\
    "bboxisaxismin",\
    "bboxisaxismax",\
    "volume"}
enum keyword_labels {
    e_begin,
    e_regionnum,
    e_matname,
    e_color_red,
    e_color_green,
    e_color_blue,
    e_mean_intensity,
    e_boron_cf,
    e_gamma_rbe,
    e_nitrogen_rbe,
    e_nitrogen_dens,
    e_recoil_rbe,
    e_hydrogen_rbe,
    e_hydrogen_dens,
    e_other_rbe,
    e_ultrafast_rbe,
    e_carbon_dens,
    e_oxygen_dens,
    e_tissue_to_blood,
    e_maximum_dose,
    e_constraint_dose,
    e_editable,
    e_parent_body,
    e_bboxrlaxismin,
    e_bboxrlaxismax,
    e_bboxpaaxismin,
    e_bboxpaaxismax,
    e_bboxisaxismin,
    e_bboxisaxismax,
    e_volume
};

/* Make a region type rather than forcing us to use a char */
typedef unsigned char region_t;

typedef struct _dose_info_valid_t {
  char bodyname;
  char matname;
  char boron_cf;
  char gamma_rbe;
  char nitrogen_rbe;
  char nitrogen_dens;
  char recoil_rbe;
  char hydrogen_rbe;
  char hydrogen_dens;
  char other_rbe;
  char ultrafast_rbe;
  char carbon_dens;
  char oxygen_dens;
  char tissue_to_blood;
  char maximum_dose;
  char constraint_dose;
  char editable;
} dose_info_valid_t;

typedef struct _dose_info_t {
  char bodyname[LIBUV_MAX_STRING];
  char matname [LIBUV_MAX_STRING];
  float boron_cf;
  float gamma_rbe;
  float nitrogen_rbe;
  float nitrogen_dens;
  float recoil_rbe;
  float hydrogen_rbe;
  float hydrogen_dens;
  float other_rbe;
  float ultrafast_rbe;
  float carbon_dens;
  float oxygen_dens;
  float tissue_to_blood;
  float maximum_dose;
  float constraint_dose;
  int editable;
  dose_info_valid_t valid;
  struct _dose_info_t * next;
} dose_info_t;

typedef enum {FIDUCIAL,
	      CONSTRAINT
} MARKER_KIND;

/* This isn't the best.  Rather than copy arrays back and forth
 * between libuv and ImageTools, define a few extra things here and
 * just let libuv and ImageTools "share" the list
 */
typedef struct _marker_type {
  MARKER_KIND marker_kind; /* FIDUCIAL or CONSTRAINT */
  char * name;/* anterior, etc. */
  char bodyname[LIBUV_MAX_STRING]; /* to look up dose info, constraint only */
  int index;  /* index of the image it's on */
  int x, y;   /* x, y coords of it's center on the image */
  float wcf_x, wcf_y, wcf_z;
  struct _marker_type * next;
  char is_used;
  dose_info_t dose;
  void * pvt_ptr;
} marker_type;

/* This structure will be a substructure of the
 * geom_info_t and should be initialized to all
 * 'false' (0) and set to 'true' as values are set
 */
typedef struct _geom_info_valid_t {
  char imagerowaxis;
  char imagecolumnaxis;
  char imagesliceaxis;
  char bodydatatitle;        /* added 1-12-98 mbr */
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
  char regionnum[LIBUV_MAX_REGIONS];
  char mean_intensity[LIBUV_MAX_REGIONS];
  char color_red[LIBUV_MAX_REGIONS];
  char color_green[LIBUV_MAX_REGIONS];
  char color_blue[LIBUV_MAX_REGIONS];
  char bodyname[LIBUV_MAX_REGIONS];
  char dose[LIBUV_MAX_REGIONS];
  char parent_body[LIBUV_MAX_REGIONS];
  char bboxpaaxismin[LIBUV_MAX_REGIONS];
  char bboxpaaxismax[LIBUV_MAX_REGIONS];
  char bboxrlaxismin[LIBUV_MAX_REGIONS];
  char bboxrlaxismax[LIBUV_MAX_REGIONS];
  char bboxisaxismin[LIBUV_MAX_REGIONS];
  char bboxisaxismax[LIBUV_MAX_REGIONS];
  char volume[LIBUV_MAX_REGIONS];
  char uvval[LIBUV_MAX_REGIONS];
  char vol_arr;
  char rlaxismax;
  char paaxismax;
  char isaxismax;
  char inv_pixelsizeslices;
  char inv_pixelsizecolumns;
  char inv_pixelsizerows;
  char num_fiducial_markers;
  char num_constraint_markers;
  char fiducial_markers;
  char constraint_markers;
} geom_info_valid_t;

typedef struct _geom_info_t {
  char imagerowaxis[LIBUV_MAX_STRING];
  char imagecolumnaxis[LIBUV_MAX_STRING];
  char imagesliceaxis[LIBUV_MAX_STRING];
  char bodydatatitle[LIBUV_MAX_STRING];    /* first line of body_data.txt, may or may not be present (mbr 1-12-98)*/
  char dimensionality[LIBUV_MAX_STRING];   /* mm, cm, etc. */
  char sliceorientation[LIBUV_MAX_STRING];
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
  
  int regionnum[LIBUV_MAX_REGIONS];
  int mean_intensity[LIBUV_MAX_REGIONS];
  int color_red[LIBUV_MAX_REGIONS];
  int color_green[LIBUV_MAX_REGIONS];
  int color_blue[LIBUV_MAX_REGIONS];
  char bodyname[LIBUV_MAX_REGIONS][LIBUV_MAX_STRING];
  dose_info_t dose[LIBUV_MAX_REGIONS];
  char parent_body[LIBUV_MAX_REGIONS][LIBUV_MAX_STRING];
  float bboxpaaxismin[LIBUV_MAX_REGIONS];
  float bboxpaaxismax[LIBUV_MAX_REGIONS];
  float bboxrlaxismin[LIBUV_MAX_REGIONS];
  float bboxrlaxismax[LIBUV_MAX_REGIONS];
  float bboxisaxismin[LIBUV_MAX_REGIONS];
  float bboxisaxismax[LIBUV_MAX_REGIONS];
  float volume[LIBUV_MAX_REGIONS];          /* in current units */

  /* The following are _derived_ quantities not in the .uvh */
  int uvval[LIBUV_MAX_REGIONS];
  unsigned char *vol_arr;     /* ptr to all the volume information     */
  float rlaxismax;            /* Parallels to the mins                 */
  float paaxismax;
  float isaxismax;
  float inv_pixelsizeslices;  /* Inverses for quick divides            */
  float inv_pixelsizecolumns;
  float inv_pixelsizerows;
  int num_fiducial_markers;
  int num_constraint_markers;
  marker_type * fiducial_markers;
  marker_type * constraint_markers;

  /* Use this to keep track of what's valid, what's not */
  geom_info_valid_t valid;
} geom_info_t;

typedef struct {int x, y;} Pt;

/* Prototypes */
void initialize_dose_info(dose_info_t *);
void copy_dose_info(dose_info_t *, dose_info_t *);
void initialize_marker(marker_type *, MARKER_KIND, char *);
void reinitialize_marker(marker_type *);
void initialize_geom_info(geom_info_t *);
void read_uvh_minimal(geom_info_t *, char *);
void free_read_uvh_markers(geom_info_t *);
void read_uvh(geom_info_t *, char *);
void read_existing_uvh(geom_info_t *, char *);
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

/*
 * I moved these three prototypes in here
 * because I'm using them in seraConvert. MTC 8/17/99
 */
unsigned char add_line(geom_info_t *, unsigned char,
                       float, float, float, float, int);
void PUSH ( int X, int, Pt **, Pt *);
void POP  ( int *, int *, Pt ** );


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
