/* =============================================================> image_data.h
 *
 * This file defines a structure to be used for passing data between
 * the slice creation routines and file reading routines for qsh or
 * other file formats.
 *
 * ==========================================================================
 */

#ifndef __IMAGE_DATA__

#define MAX_BEADS            8     /* Maximum allowable number of beads */
#define BAD_LOC_VALUE -99999.0
#define BAD_LOC_CHECK -99990.0

typedef struct slice_data_S
{
   float     sl_xpixsize;
   float     sl_ypixsize;
   float     sl_zpixsize;
   float     sl_zposition;
   float     sl_thickness;
   int       sl_max_pixel;
   int       sl_min_pixel;
   float     sl_spacing;
} slice_data_T;

typedef struct image_data_S
{
   int             im_bytes_pp;
   char            im_byte_order [16];          
   int             im_format;
   int             im_nechos;
   int             im_plane_type;
   char            im_view [16];            /* character form of plane type */
   char            im_dimensionality [4];
   float           im_x_pixel_size;
   float           im_y_pixel_size;
   char            im_date [32];
   char            im_name [64];
   char            im_modality [16];
   int             im_enhancement_mode;
   float           im_fov;
   int             im_maxp;
   int             im_minp;
   float           im_zposition;
   int             im_nbeads;
   char            im_bead_loc [MAX_BEADS][16];
   float           im_bead_x [MAX_BEADS];
   float           im_bead_y [MAX_BEADS];
   float           im_bead_z [MAX_BEADS];
   float           im_slice_thickness;
   float           im_slice_edge_to_edge;      
   int             im_width, im_height;
   int             im_nslices;
   slice_data_T    *im_slices;
   char            *im_pixels;
} image_data_T;


/* 
 * QSH KEYS
 * Constants to be used in working with the qsh file keys.  These have
 * been through a few versions, so there are lots of recundancies.  Those
 * marked with (P) are primary keys that take precedence over the
 * others which are marked as (S) secondary and (O) obsolete.  Also,
 * keys with global significance in the file are labelled (G) and those
 * pertaining to a specific slice are labelled (L). Used in
 * the file qsh.c and possibly in inel_slice.c.  GJH, 310795.
 */


#define BEAD_LOCATION_KEY       "Bead location[]"    /* anterior | posterior | superior | left | right */ /* GP */
#define BEAD_X_LOCATION_KEY     "Bead X location in mm[]" /* GP */
#define BEAD_Y_LOCATION_KEY     "Bead Y location in mm[]" /* GP */
#define BEAD_Z_LOCATION_KEY     "Bead Z location in mm[]" /* GP */
#define BPP_KEY                 "Bytes per pixel"     /* (GP) */
#define BYTE_ORDER_KEY          "byte order"          /* (GP) */
#define CTC_KEY                 "Center-to-center slice distance" /* (GS) */
#define DATE_KEY                "Study date"          /* (GP) */
#define ECHO_KEY                "Number of echos"     /* (GP) */
#define FOV_KEY                 "Field of view - mm"  /* (GS) */
#define FOVHW_KEY               "Field of View Height and Width in mm[0]"/*(O)*/
#define FOVX_KEY                "Display field of view X"   /* (O) */
#define FOVY_KEY                "Display field of view Y"   /* (O) */
#define IMAGE_LOCATION_KEY      "Image Location"            /* (GP) */
#define MAX_P_KEY               "Maximum pixel"              /* (GP) */
#define MAX_P_SLICE_KEY         "Maximum pixel in slice[]"   /* (LP) */
#define MIN_P_KEY               "Minimum pixel"              /* (GP) */
#define MIN_P_SLICE_KEY         "Minimum pixel in slice[]"  /* (LP) */
#define MODALITY_KEY            "Modality"            /* (GP) */
#define NAME_KEY                "Patient name"        /* (GP) */
#define NUM_SETS                "Number of Images in Sequence" /* (GP) */
#define NUM_BEADS_KEY           "Number of beads"     /* (GP*) */
#define PLANE_KEY               "Plane type 0-axial, 1-sagittal, 2-cor, 3-oblique" /* (GP) */
#define SCAN_SPACING_KEY        "Scan Spacing mm?[]"  /* (O) */
#define SLICE_LOCATION_KEY      "Image Location[]"    /* (LS) */
#define SLICE_THICKNESS_KEY     "Slice Thickness in mm[]"       /* (LP) */
#define SLICE_KEY               "Number of Slices"     /* (GS) */
#define SLICE2_KEY              "Size of dimension[]"  /* (GP) */
#define SLICE_POSITION_KEY      "Slice Position in mm[]"  /* (LP) */
#define THICKNESS_KEY           "Image Thickness[]"  /* (O) */
#define THICK2_KEY              "Slice Thickness - Zmm[]"   /* (O) */
#define UNIFORM_THICKNESS_KEY   "Uniform Slice Thickness in mm"   /* (GP) */
#define UNIFORM_SPACING_KEY     "Uniform Slice Spacing in mm"   /* (GP) */
#define X_PIXEL_SIZE_KEY        "Pixel Size X in mm[]"    /* (LP) */
#define Y_PIXEL_SIZE_KEY        "Pixel Size Y in mm[]"    /* (LP) */
#define Z_PIXEL_SIZE_KEY        "Pixel Size Z in mm[]"    /* (LP) */

/*
 * bnct_rtpe key definitions.  This is the final word on keys for 
 * all qsh files created after 7/1/96.  One of the additions is
 * a VERSION key, so that these files can be identified.  All files
 * without such a key will be assumed to be version 1.  This
 * version represents version 2.
 */

/* Required global keys */

#define VERSION_KEY               "Version"

/* Standard required global qsh keys forced on us */

#define BPP_V2_KEY                 "Bytes per pixel"
#define BYTE_ORDER_V2_KEY          "Byte Order"
#define PIXEL_FORMAT_V2_KEY        "Pixel format"
#define NUM_OF_DIMENSIONS_V2_KEY   "Number of dimensions"
#define SIZE_OF_DIMENSION_V2_KEY   "Size of dimension[]"

/* Our own required global keys*/

#define DIMENSIONALITY_V2_KEY      "Dimensionality"
#define X_PIXEL_SIZE_V2_KEY        "X Pixel Size"
#define Y_PIXEL_SIZE_V2_KEY        "Y Pixel Size"
#define UNIFORM_THICKNESS_V2_KEY   "Uniform Thickness"
#define REF_LOCATION_V2_KEY        "Reference Location"
#define MIN_PIXEL_VALUE_V2_KEY     "Minimum Pixel Value"
#define MAX_PIXEL_VALUE_V2_KEY     "Maximum Pixel Value"
#define MODALITY_V2_KEY            "Modality"
#define VIEW_V2_KEY                "View"
#define NUM_OF_ECHOS_V2_KEY        "Number Of Echos"

/* Optional global Keys */

#define PATIENT_NAME_V2_KEY        "Patient Name"
#define DATE_V2_KEY                "Date"
#define NUM_OF_BEADS_V2_KEY        "Number Of Beads"
#define BEAD_X_LOC_V2_KEY          "Bead X Location[]"
#define BEAD_Y_LOC_V2_KEY          "Bead Y Location[]"
#define BEAD_Z_LOC_V2_KEY          "Bead Z Location[]"
#define ENHANCEMENT_MODE_V2_KEY    "Enhancement Mode"
#define UNIFORM_SPACING_V2_KEY     "Uniform Spacing"

/* Required local keys */

#define MIN_PIXEL_SLICE_V2_KEY     "Minimum Pixel Value[]"
#define MAX_PIXEL_SLICE_V2_KEY     "Maximum Pixel Value[]"
#define IMAGE_LOCATION_V2_KEY      "Image Location[]"

/* Optional local keys */

#define SPACING_V2_KEY             "Spacing[]"


#endif
