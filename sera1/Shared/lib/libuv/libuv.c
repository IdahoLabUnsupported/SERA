#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
/* for exec, fork, pipe, dup, etc. */
#include <unistd.h>
/* for wait */
#include <sys/types.h>
#include <sys/wait.h>

#include <ctype.h>
#include "libuv.h"

#ifndef LIBUV50
#ifndef LIBUV50APPROX
#define LIBUV
#endif
#endif

/******************************************************************
 * DEFINES
 ******************************************************************/
/* 1/2^10 */
#define EPSILON (0.0009765625)
/* 2^30 */
#define INFINITY 1073741824
#define ALMOSTONE (0.9990234375)
#define bot_slopef 1073741824.0
#define absdx 1073741824
#ifndef MAX
#define MAX(x,y) (x)>(y)?(x):(y)
#endif
#ifndef MIN
#define MIN(x,y) (x)<(y)?(x):(y)
#endif

#define CURRENT_VERSION "uvh2.0"
#define MAX_UVH_LINE_SIZE 512
#define KEY_VALUE_SPLIT_STRING ":"

/* All keys
 * (1) are to be in lower case (can be stored on disk in uppercase, though)
 * (2) cannot contain #'s
 * (3) cannot contain the split string (currently ":")
 * (4) may have internal spaces but not at front or at end
 */
#define UVH_VERSION          "version"
#define UVH_BODYDATATITLE    "bodydatatitle"
#define UVH_DIMENSIONALITY   "dimensionality"
#define UVH_SLICEORIENTATION "sliceorientation"
#define UVH_IMAGEROWS        "imagerows"
#define UVH_IMAGECOLUMNS     "imagecolumns"
#define UVH_IMAGESLICES      "imageslices"
#define UVH_PIXELSIZEROWS    "pixelsizerows"
#define UVH_PIXELSIZECOLUMNS "pixelsizecolumns"
#define UVH_PIXELSIZESLICES  "pixelsizeslices"
#define UVH_PAAXISMIN        "paaxismin"
#define UVH_PAAXISMAX        "paaxismax"
#define UVH_RLAXISMIN        "rlaxismin"
#define UVH_RLAXISMAX        "rlaxismax"
#define UVH_ISAXISMIN        "isaxismin"
#define UVH_ISAXISMAX        "isaxismax"
#define UVH_IMAGEROWAXIS     "imagerowaxis"
#define UVH_IMAGECOLUMNAXIS  "imagecolumnaxis"
#define UVH_IMAGESLICEAXIS   "imagesliceaxis"

/* This is for a body */
#define UVH_BEGIN            "begin"
/* These two are for markers */
#define UVH_CONSTRAINT       "constraint"
#define UVH_FIDUCIAL         "fiducial"

/* All will end with a simple "end" */
#define UVH_END              "end"

#define UVH_UVVAL            "uvval"

/******************************************************************
 * Prototypes (Some)
 * These are not in .h file because they are only used internally
 ******************************************************************/
static FILE * read_uvh_minimal_fptr(geom_info_t *, char *, int);
static int process_region_property(int, geom_info_t *, char *, char *);
static int process_marker_property(marker_type *, char *, char *);
static int UV_readln(FILE *, char *, int);
static int UV_readln_skip_comments_and_trim(FILE *, char *, int);
static void UV_break_into_lower_key_and_value(char *, char *, char **, char **);
static int UV_read_next_key_and_value(FILE *, char *, int, char **, char **);
static int UV_read_string_value_for_key(FILE *, char *, char *, int);
static int UV_trim_string(char *);
static int read_uvval_for_body(FILE *, char *);
static void read_body_information(FILE *, geom_info_t *, char *);
static void read_constraint_information(FILE *, geom_info_t *, char *);
static void read_fiducial_information(FILE *, geom_info_t *, char *);
static void UV_make_lower_string(char *); 
static int int_rnd(float);
static int int_rnd_down(float);
/*static float subtract_tiny(float);*/
static int ifsign(float);
static void place_in_bounds(float *, float *, float *);
static void round_to_bignum(float *, float *, float *);
static float find_voxel_edge(int, float);
/* Like geom_array_lookup, but returns -1 if out of bounds AND
 * converts the uvval to an actual regionnumber
 */
static int regionnum_lookup(geom_info_t *, int, int, int);
static unsigned char geom_array_lookup(geom_info_t *, int, int, int);
static unsigned char lookup_nochecking(int, int, int);
static void ndcf_to_wcf(geom_info_t *, float, float, float,
		 float *, float *, float *);
static void wcf_to_ndcf(geom_info_t *,
		 float, float, float,
		 float *, float *, float *);
static unsigned char get_mapping(geom_info_t *, char);
static void fill_region_driver(geom_info_t *, int, int, int, unsigned char, unsigned char);
static void fill_region(geom_info_t *, int, int, int, unsigned char, unsigned char, unsigned char *);
static void fill_region_wvect(geom_info_t *, float, float, float, float, int, unsigned char, unsigned char);
static void float_sort(float *, int);
static void float_sort_3(float *);
static void float_sort_4(float *);
static void enlarge_bounding_box_simple(geom_info_t *);
static void enlarge_bounding_box(geom_info_t *, int, int, int);
static void print_tracking_info(geom_info_t *,
				int, int, int,
				double,
				float, float, float,
				float, float, float);

/******************************************************************
 * GLOBALS
 ******************************************************************/

static geom_info_t ggg;  /* GLOBAL geometry info */
static geom_info_t *geom_ptr;
static unsigned char *garr;
static int gwidth;
static int gheight;
static int gnumslices;

static int global_accuracy = 1;     /* 1 means accurate, 0 means less accurate */
static int total_intersections = 0; /* global to keep track of number of
			      * intersections computed when timing
			      */
int univels_hit = 0;         /* keeps track of number of univels
			      * stepped through
			      */

/* Define the regions in powers of 2 to simplify filling them in
 * (That is, each bit can represent a certain region type
 * As an example, we may have the following:
 *   0 --> VOID (outside bounds)          ENFORCED
 *   1 --> BUFFER (in bounds)             ENFORCED
 *   2 --> SCALP                          (SAMPLE)
 *   4 --> SKULL                          (SAMPLE)
 *   8 --> BRAIN                          (SAMPLE)
 *  16 --> TARGET                         (SAMPLE)
 *  32 --> TUMOR                          (SAMPLE)
 *
 * As of 06-09-1997, output is of form:
 *   0 --> VOID (outside bounds)          ENFORCED
 *   1 --> BUFFER (in bounds)             ENFORCED
 *   2 --> SCALP                          (SAMPLE)
 *   3 --> SKULL                          (SAMPLE)
 *   4 --> BRAIN                          (SAMPLE)
 *   5 --> TARGET                         (SAMPLE)
 *   6 --> TUMOR                          (SAMPLE)
 * ** Program still uses powers of 2 (or bits) in
 *    intermediate representation
 */
static int bitregions[8] = {1, 2, 4, 8, 16, 32, 64, 128};





/******************************************************************
 * ROUTINES
 ******************************************************************/


/******************************************************************
 * static int process_region_property
 ******************************************************************
 * Given a string from a uvh file, processes string and stores the
 * information provided by the string in the geom struct
 ******************************************************************/
static int process_region_property(int uvvalue, geom_info_t * geom_ptr,
				   char * key, char * value) {
  int caseval=-1, i;
  int intval;
  float floatval;
  int numkeywords=NUM_KEYWORD_LABELS;
  enum keyword_labels e_keyval;
  char *ptr, *keywords[]=LCASE_KEYWORDS;

  for (i=0; i<numkeywords; i++) {
    if (!strcmp(key, keywords[i])) {
      caseval=i;
      break;
    }
  }

  if ((caseval==-1)||(uvvalue<0)||(uvvalue>=LIBUV_MAX_REGIONS)) return(0);
  
  e_keyval = caseval;

  switch(e_keyval)
    {
    case e_begin: /* begin == bodyname */
      if (strlen(value)<LIBUV_MAX_STRING) {
	strcpy(geom_ptr->bodyname[uvvalue], value);
	strcpy(geom_ptr->dose[uvvalue].bodyname, value);
      } else {
	geom_ptr->bodyname[uvvalue][LIBUV_MAX_STRING-1]='\0';
	strncpy(geom_ptr->bodyname[uvvalue], value, LIBUV_MAX_STRING-1);
	geom_ptr->dose[uvvalue].bodyname[LIBUV_MAX_STRING-1]='\0';
	strncpy(geom_ptr->dose[uvvalue].bodyname, value, LIBUV_MAX_STRING-1);
      }
      geom_ptr->valid.bodyname[uvvalue]=1;
      geom_ptr->dose[uvvalue].valid.bodyname=1;
      /* printf("Body name is %s\n", geom_ptr->bodyname[uvvalue]); */
      break;
    case e_regionnum: /* regionnum */
      intval = atoi(value);
      geom_ptr->regionnum[uvvalue]=intval;
      geom_ptr->uvval[intval]=uvvalue;
      geom_ptr->valid.regionnum[uvvalue]=1;
      geom_ptr->valid.uvval[intval]=1;
      /*printf("Region number is %d\n", intval); */
      break;
    case e_color_red: /* color */
      /* red */
      intval = atoi(value);
      /* printf("Body color is %d ", intval); */
      geom_ptr->color_red[uvvalue]=intval;
      geom_ptr->valid.color_red[uvvalue]=1;
      break;
    case e_color_green: /* color */
      /* green */
      intval = atoi(value);
      /* printf("Body color is %d ", intval); */
      geom_ptr->color_green[uvvalue]=intval;
      geom_ptr->valid.color_green[uvvalue]=1;
      break;
    case e_color_blue: /* color */
      /* blue */
      intval = atoi(value);
      /* printf("Body color is %d ", intval); */
      geom_ptr->color_blue[uvvalue]=intval;
      geom_ptr->valid.color_blue[uvvalue]=1;
      break;
    case e_mean_intensity: /* mean_intensity */
      intval = atoi(value);
      geom_ptr->mean_intensity[uvvalue]=intval;
      /* printf("Average gray is %d\n", intval); */
      geom_ptr->valid.mean_intensity[uvvalue]=1;
      break;
    case e_matname: /* matname */
      if (strlen(value)<LIBUV_MAX_STRING) {
	strcpy(geom_ptr->dose[uvvalue].matname, value);
      } else {
	geom_ptr->dose[uvvalue].matname[LIBUV_MAX_STRING-1]='\0';
	strncpy(geom_ptr->dose[uvvalue].matname, value, LIBUV_MAX_STRING-1);
      }
      geom_ptr->dose[uvvalue].valid.matname=1;
      break;
    case e_boron_cf: /* boron_cf */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].boron_cf=floatval;
      geom_ptr->dose[uvvalue].valid.boron_cf=1;
      break;
    case e_gamma_rbe: /* gamma_rbe */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].gamma_rbe=floatval;
      geom_ptr->dose[uvvalue].valid.gamma_rbe=1;
      break;
    case e_nitrogen_rbe: /* nitrogen_rbe */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].nitrogen_rbe=floatval;
      geom_ptr->dose[uvvalue].valid.nitrogen_rbe=1;
      break;
    case e_nitrogen_dens: /* nitrogen_dens */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].nitrogen_dens=floatval;
      geom_ptr->dose[uvvalue].valid.nitrogen_dens=1;
      break;
    case e_recoil_rbe: /* recoil_rbe */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].recoil_rbe=floatval;
      geom_ptr->dose[uvvalue].valid.recoil_rbe=1;
      break;
    case e_hydrogen_rbe: /* hydrogen_rbe */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].hydrogen_rbe=floatval;
      geom_ptr->dose[uvvalue].valid.hydrogen_rbe=1;
      break;
    case e_hydrogen_dens: /* hydrogen_dens */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].hydrogen_dens=floatval;
      geom_ptr->dose[uvvalue].valid.hydrogen_dens=1;
      break;
    case e_other_rbe: /* other_rbe */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].other_rbe=floatval;
      geom_ptr->dose[uvvalue].valid.other_rbe=1;
      break;
    case e_ultrafast_rbe: /* ultrafast_rbe */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].ultrafast_rbe=floatval;
      geom_ptr->dose[uvvalue].valid.ultrafast_rbe=1;
      break;
    case e_carbon_dens: /* carbon_dens */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].carbon_dens=floatval;
      geom_ptr->dose[uvvalue].valid.carbon_dens=1;
      break;
    case e_oxygen_dens: /* oxygen_dens */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].oxygen_dens=floatval;
      geom_ptr->dose[uvvalue].valid.oxygen_dens=1;
      break;
    case e_tissue_to_blood: /* tissue_to_blood */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].tissue_to_blood=floatval;
      geom_ptr->dose[uvvalue].valid.tissue_to_blood=1;
      break;
    case e_maximum_dose: /* maximum_dose */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].maximum_dose=floatval;
      geom_ptr->dose[uvvalue].valid.maximum_dose=1;
      break;
    case e_constraint_dose: /* constraint_dose */
      floatval = (float)atof(value);
      geom_ptr->dose[uvvalue].constraint_dose=floatval;
      geom_ptr->dose[uvvalue].valid.constraint_dose=1;
      break;
    case e_parent_body: /* parent_body */
      if (strlen(value)<LIBUV_MAX_STRING) {
	strcpy(geom_ptr->parent_body[uvvalue], value);
      } else {
	geom_ptr->parent_body[uvvalue][LIBUV_MAX_STRING-1]='\0';
	strncpy(geom_ptr->parent_body[uvvalue], value, LIBUV_MAX_STRING-1);
      }
      geom_ptr->valid.parent_body[uvvalue]=1;
      break;
    case e_bboxrlaxismin: /* bounding_box */
      floatval = (float)atof(value);
      geom_ptr->bboxrlaxismin[uvvalue]=floatval;
      geom_ptr->valid.bboxrlaxismin[uvvalue]=1;
      break;
    case e_bboxrlaxismax: /* bounding_box */
      floatval = (float)atof(value);
      geom_ptr->bboxrlaxismax[uvvalue]=floatval;
      geom_ptr->valid.bboxrlaxismax[uvvalue]=1;
      break;
    case e_bboxpaaxismin: /* bounding_box */
      floatval = (float)atof(value);
      geom_ptr->bboxpaaxismin[uvvalue]=floatval;
      geom_ptr->valid.bboxpaaxismin[uvvalue]=1;
      break;
    case e_bboxpaaxismax: /* bounding_box */
      floatval = (float)atof(value);
      geom_ptr->bboxpaaxismax[uvvalue]=floatval;
      geom_ptr->valid.bboxpaaxismax[uvvalue]=1;
      break;
    case e_bboxisaxismin: /* bounding_box */
      floatval = (float)atof(value);
      geom_ptr->bboxisaxismin[uvvalue]=floatval;
      geom_ptr->valid.bboxisaxismin[uvvalue]=1;
      break;
    case e_bboxisaxismax: /* bounding_box */
      floatval = (float)atof(value);
      geom_ptr->bboxisaxismax[uvvalue]=floatval;
      geom_ptr->valid.bboxisaxismax[uvvalue]=1;
      break;
    case e_volume: /* volume */
      floatval = (float)atof(value);
      geom_ptr->volume[uvvalue]=floatval;
      geom_ptr->valid.volume[uvvalue]=1;
      break;
    }
  return(1);
}


/******************************************************************
 * static int process_marker_property
 ******************************************************************
 * Given a string from a uvh file, processes string and stores the
 * information provided by the string in the marker struct
 ******************************************************************/
static int process_marker_property(marker_type * marker,
				   char * key, char * value) {
  int caseval=-1, i;
  int intval;
  float floatval;
  int numkeywords=NUM_KEYWORD_LABELS;
  enum keyword_labels e_keyval;
  char *keywords[]=LCASE_KEYWORDS;

  for (i=0; i<numkeywords; i++) {
    if (!strcmp(key, keywords[i])) {
      caseval=i;
      break;
    }
  }

  e_keyval = caseval;

  switch(e_keyval)
    {
    case e_begin: /* begin == bodyname */
      if (strlen(value)<LIBUV_MAX_STRING) {
	strcpy(marker->bodyname, value);
	strcpy(marker->dose.bodyname, value);
      } else {
	marker->bodyname[LIBUV_MAX_STRING-1]='\0';
	strncpy(marker->bodyname, value, LIBUV_MAX_STRING-1);
	marker->dose.bodyname[LIBUV_MAX_STRING-1]='\0';
	strncpy(marker->dose.bodyname, value, LIBUV_MAX_STRING-1);
      }
      marker->dose.valid.bodyname=1;
      /* printf("Body name is %s\n", marker->bodyname); */
      break;
    case e_boron_cf: /* boron_cf */
      floatval = (float)atof(value);
      marker->dose.boron_cf=floatval;
      marker->dose.valid.boron_cf=1;
      break;
    case e_gamma_rbe: /* gamma_rbe */
      floatval = (float)atof(value);
      marker->dose.gamma_rbe=floatval;
      marker->dose.valid.gamma_rbe=1;
      break;
    case e_nitrogen_rbe: /* nitrogen_rbe */
      floatval = (float)atof(value);
      marker->dose.nitrogen_rbe=floatval;
      marker->dose.valid.nitrogen_rbe=1;
      break;
    case e_nitrogen_dens: /* nitrogen_dens */
      floatval = (float)atof(value);
      marker->dose.nitrogen_dens=floatval;
      marker->dose.valid.nitrogen_dens=1;
      break;
    case e_recoil_rbe: /* recoil_rbe */
      floatval = (float)atof(value);
      marker->dose.recoil_rbe=floatval;
      marker->dose.valid.recoil_rbe=1;
      break;
    case e_hydrogen_rbe: /* hydrogen_rbe */
      floatval = (float)atof(value);
      marker->dose.hydrogen_rbe=floatval;
      marker->dose.valid.hydrogen_rbe=1;
      break;
    case e_hydrogen_dens: /* hydrogen_dens */
      floatval = (float)atof(value);
      marker->dose.hydrogen_dens=floatval;
      marker->dose.valid.hydrogen_dens=1;
      break;
    case e_other_rbe: /* other_rbe */
      floatval = (float)atof(value);
      marker->dose.other_rbe=floatval;
      marker->dose.valid.other_rbe=1;
      break;
    case e_ultrafast_rbe: /* ultrafast_rbe */
      floatval = (float)atof(value);
      marker->dose.ultrafast_rbe=floatval;
      marker->dose.valid.ultrafast_rbe=1;
      break;
    case e_carbon_dens: /* carbon_dens */
      floatval = (float)atof(value);
      marker->dose.carbon_dens=floatval;
      marker->dose.valid.carbon_dens=1;
      break;
    case e_oxygen_dens: /* oxygen_dens */
      floatval = (float)atof(value);
      marker->dose.oxygen_dens=floatval;
      marker->dose.valid.oxygen_dens=1;
      break;
    case e_tissue_to_blood: /* tissue_to_blood */
      floatval = (float)atof(value);
      marker->dose.tissue_to_blood=floatval;
      marker->dose.valid.tissue_to_blood=1;
      break;
    case e_maximum_dose: /* maximum_dose */
      floatval = (float)atof(value);
      marker->dose.maximum_dose=floatval;
      marker->dose.valid.maximum_dose=1;
      break;
    case e_constraint_dose: /* constraint_dose */
      floatval = (float)atof(value);
      marker->dose.constraint_dose=floatval;
      marker->dose.valid.constraint_dose=1;
      break;
    }
  return(1);
}


/******************************************************************
 * static int UV_readln
 ******************************************************************
 * Reads a line from a file up to (maxsize-1) characters.  Because
 * it is intended to read a line, will stop after reading a '\n'.
 * The line is then terminated with a '\0' which uses a byte in
 * the array and is why at most (maxsize-1) characters can be read.
 * The LENGTH returned should be the same as that that would be
 * reported by STRLEN.
 ******************************************************************/
static int UV_readln(FILE *fptr, char *s, int maxsize) {
  int i=0, ch;

  while (i<(maxsize-1)) {
    ch=fgetc(fptr);
    if (ch<0) {
      /* don't increment i because we didn't really read a character,
       * we hit the end of the file or had an error -- so just stop
       */
      s[i]='\0';
      return(i);
    } else {
      s[i] = (char)ch;
      i++;
      if ((char)ch=='\n') {
	s[i] = '\0';
	return(i);
      }
    }
  }
  /* If here, filled all the characters before encountering a \n */
  /* --Better terminate the string */
  s[maxsize-1] = '\0';
  return(i);
}


/******************************************************************
 * static int UV_readln_skip_comments_and_trim
 ******************************************************************
 * Acts a lot like UV_readln with these additions:
 *   * Skips comments
 *   * Trims the string when done (removes trailing and leading
 *   * white space)
 *   * If what remains is of length 0, will look for the next line
 *   * SO when 0 returns that means no more meaningful lines
 *   * in file
 ******************************************************************/
static int UV_readln_skip_comments_and_trim(FILE *fptr, char *s, int maxsize) {
  int length;
  char * loc;

  do {
    length = UV_readln(fptr, s, maxsize);
    if (length==0) return(0); /* hit end of file */
    
    /* In this case, remove comments and then trim */
    if (loc = strchr(s, '#')) {
      loc[0] = '\0';
      length = strlen(s);
    }

    if (length>0) {
      length = UV_trim_string(s);
    }
  } while (length==0);
  
  return(length);
}


/******************************************************************
 * static void UV_break_into_lower_key_and_value
 ******************************************************************
 * Looks for first br to break the string in two (if none, all
 * of string is key)
 *   throws out the br portion
 *   'trims' each remaining string
 *   converts the key to lowercase
 ******************************************************************/
static void UV_break_into_lower_key_and_value(char * br, char * s,
				       char ** key, char ** value) {
  char * wherebreak;
  
  if (wherebreak=strstr(s, br)) {
    *value = wherebreak + strlen(br);
    UV_trim_string(*value);
    wherebreak[0] = '\0';
  } else {
    *value = s+strlen(s); /* The '\0' at the end */
  }
  *key = s;
  UV_trim_string(*key);
  UV_make_lower_string(*key);
}


/******************************************************************
 * static int UV_read_next_key_and_value
 ******************************************************************
 * returns true whenever it can read another non-null line from
 * the file.  It's not necessarily a key-value pair.  (For example,
 * 'end' will say 'end' is the key and the value is N/A.)  It then
 * fills in each passed sub-part.
 ******************************************************************/
static int UV_read_next_key_and_value(FILE * fptr, char * str, int maxl, char ** key, char ** value) {
  int len;

  len = UV_readln_skip_comments_and_trim(fptr, str, maxl);
  if (len==0) return(0);
  UV_break_into_lower_key_and_value(KEY_VALUE_SPLIT_STRING, str, key, value);
  return(1);
}


/******************************************************************
 * static int UV_read_string_value_for_key
 ******************************************************************
 * rewinds the file, searches for first occurance of key,
 * returns value of the key
 * Returns 1 if it's found, 0 if it's not
 ******************************************************************/
static int UV_read_string_value_for_key(FILE * fptr, char * key, char * value, int maxl) {
  char local_str[MAX_UVH_LINE_SIZE], *local_key, *local_value;

  rewind(fptr);
  while (UV_read_next_key_and_value(fptr, local_str, MAX_UVH_LINE_SIZE,
				 &local_key, &local_value)) {
    if (!strcmp(local_key, key)) {
      if (strlen(local_value)<maxl) {
	strcpy(value, local_value);
      } else {
	strncpy(value, local_value, maxl-1);
	value[maxl-1] = '\0';
      }
      return(1); /* terminiate when first finds the key */
    }
  }

  return(0);
}


/******************************************************************
 * static int UV_trim_string
 ******************************************************************
 * Based on length of passed string, removes all 'isspace'
 * characters at end of string and then those at the beginning.
 * If it removes characters at the beginning, it must translate
 * the string backwards.  Returns the length of the 'trimmed'
 * string.
 ******************************************************************/
static int UV_trim_string(char * s) {
  int i, length, first_non_space;

  length = strlen(s);

  /* Trim the end of the string */
  while((length>0)&&(isspace(s[length-1]))) {
    length--;
    s[length] = '\0';
  }

  /* Now, trim the beginning of the string -- this will consist of
   * translating the string backwards a few bytes, as necessary
   */
  if (length>0) {
    first_non_space = 0;

    while((s[first_non_space]!='\0')&&(isspace(s[first_non_space])))
      first_non_space++;

    if (first_non_space>0) {
      for (i=first_non_space; i<=length; i++) {
	s[i-first_non_space] = s[i];
      }
      length -= first_non_space;
    }
  }

  return(length);
}


/******************************************************************
 * static int read_uvval_for_body
 ******************************************************************
 * Has a body name, needs the uvval for the body -- must
 * return the file pointer to its current position when done.
 * If uvval is bad, will just return -1 and skip past the 'end'
 * for the body.
 ******************************************************************/
static int read_uvval_for_body(FILE * fptr, char * bodyname) {
  int uvvalue;
  char str[MAX_UVH_LINE_SIZE], * key, * value;

  while (UV_read_next_key_and_value(fptr, str, MAX_UVH_LINE_SIZE,
				 &key, &value)) {
    if (!strcmp(key, UVH_END)) break;

    if (!strcmp(key, UVH_UVVAL)) {
      uvvalue = atoi(value);
      if ((uvvalue>=0)&&(uvvalue<LIBUV_MAX_REGIONS)) {
	rewind(fptr);
	while (UV_read_next_key_and_value(fptr, str, MAX_UVH_LINE_SIZE,
				       &key, &value)) {
	  if ((!strcmp(key, UVH_BEGIN))&&(!strcmp(value, bodyname))) {
	    return(uvvalue);
	  }
	}
      }
    }
  }

  return(-1);
}


/******************************************************************
 * static void read_body_information
 ******************************************************************
 * Just read a line from the uvh file indicating to start a new
 * body.  We have the body name and will store information on it.
 * The name of the body we are reading is in 'bodyname'.  We read
 * information until we find an 'end' for the body.
 ******************************************************************/
static void read_body_information(FILE * fptr, geom_info_t * geom_ptr,
			   char * bodyname) {
  int uvval;
  char str[MAX_UVH_LINE_SIZE], * key, * value;

  uvval = read_uvval_for_body(fptr, bodyname);
  /* If out of range, skipped the body and we should just
   * return at this time
   */
  if ((uvval<0)||(uvval>LIBUV_MAX_REGIONS-1)) return;

  process_region_property(uvval, geom_ptr, UVH_BEGIN, bodyname);

  while (UV_read_next_key_and_value(fptr, str, MAX_UVH_LINE_SIZE,
				 &key, &value)) {
    if (!strcmp(key, UVH_END)) break;
    process_region_property(uvval, geom_ptr, key, value);
  }
}


/* Don't do anything with pvt_ptr */
void initialize_marker(marker_type * marker, MARKER_KIND marker_kind,
		       char * name) {
  marker->marker_kind = marker_kind;
  marker->name = (char*)malloc(sizeof(char)*(strlen(name)+1));
  strcpy(marker->name, name);
  marker->index = -1;
  marker->x = -1;
  marker->y = -1;
  marker->wcf_x = 0.0;
  marker->wcf_y = 0.0;
  marker->wcf_z = 0.0;
  strcpy(marker->bodyname, "");
  marker->next = NULL;
  marker->is_used = 0;
  initialize_dose_info(&(marker->dose));
}


/* Don't do anything with pvt_ptr */
void reinitialize_marker(marker_type * marker) {
  marker->index = -1;
  marker->x = -1;
  marker->y = -1;
  marker->wcf_x = 0.0;
  marker->wcf_y = 0.0;
  marker->wcf_z = 0.0;
  marker->next = NULL;
  marker->is_used = 0;
  strcpy(marker->bodyname, "");
  initialize_dose_info(&(marker->dose));
}


/******************************************************************
 * static void read_constraint_information
 ******************************************************************
 * Just read a line from the uvh file indicating to start a new
 * constraint marker.  We have the marker name and will store
 * information on it.  The name of the marker we are reading is in
 * 'markername'.  We read information until we find an 'end' for
 * the marker.
 ******************************************************************/
static void read_constraint_information(FILE * fptr, geom_info_t * geom_ptr,
				 char * markername) {
  char str[MAX_UVH_LINE_SIZE], * key, * value;
  marker_type * new_marker;

  geom_ptr->num_constraint_markers = geom_ptr->num_constraint_markers + 1;
  new_marker = (marker_type *)malloc(sizeof(marker_type));
  initialize_marker(new_marker, CONSTRAINT, markername);
  
  new_marker->next = geom_ptr->constraint_markers;
  geom_ptr->constraint_markers = new_marker;

  /* Also set these two */
  new_marker->is_used = 1;
  new_marker->pvt_ptr = NULL;

  process_marker_property(new_marker, UVH_BEGIN, markername);

  while (UV_read_next_key_and_value(fptr, str, MAX_UVH_LINE_SIZE,
				 &key, &value)) {
    if (!strcmp(key, UVH_END)) break;
    process_marker_property(new_marker, key, value);
    UV_make_lower_string(value);
    if (!strcmp(key, "rl")) {
      new_marker->wcf_x = (float)atof(value);
    } else if (!strcmp(key, "pa")) {
      new_marker->wcf_y = (float)atof(value);
    } else if (!strcmp(key, "is")) {
      new_marker->wcf_z = (float)atof(value);
    } /* else do nothing... */
  }
}


/******************************************************************
 * static void read_fiducial_information
 ******************************************************************
 * Just read a line from the uvh file indicating to start a new
 * fiducial marker.  We have the marker name and will store
 * information on it.  The name of the marker we are reading is in
 * 'markername'.  We read information until we find an 'end' for
 * the marker.
 ******************************************************************/
static void read_fiducial_information(FILE * fptr, geom_info_t * geom_ptr,
				      char * markername) {
  char str[MAX_UVH_LINE_SIZE], * key, * value;
  marker_type * new_marker;

  geom_ptr->num_fiducial_markers = geom_ptr->num_fiducial_markers + 1;
  new_marker = (marker_type *)malloc(sizeof(marker_type));
  initialize_marker(new_marker, FIDUCIAL, markername);
  
  new_marker->next = geom_ptr->fiducial_markers;
  geom_ptr->fiducial_markers = new_marker;

  /* Set these two */
  new_marker->is_used = 1;
  new_marker->pvt_ptr = NULL;

  while (UV_read_next_key_and_value(fptr, str, MAX_UVH_LINE_SIZE,
				 &key, &value)) {
    if (!strcmp(key, UVH_END)) break;
    UV_make_lower_string(value);
    if (!strcmp(key, "rl")) {
      new_marker->wcf_x = (float)atof(value);
    } else if (!strcmp(key, "pa")) {
      new_marker->wcf_y = (float)atof(value);
    } else if (!strcmp(key, "is")) {
      new_marker->wcf_z = (float)atof(value);
    } /* else do nothing... */
  }
}


/******************************************************************
 * static void UV_make_lower_string
 ******************************************************************
 * Given a string, converts all uppercase characters found to
 * corresponding lowercase characters
 ******************************************************************/
static void UV_make_lower_string(char *s) {
  int i;
  char ch;
  
  i=0;
  while (ch=s[i]) {
    s[i]=(char)tolower((int)ch);
    i++;
  }
}

/******************************************************************
 * static int_rnd
 ******************************************************************
 * For rounding floats to integers:
 *   1.5   --> 2
 *   1.49  --> 1
 *  -1.5   --> -2
 *  -1.49  --> -1
 *   etc.
 ******************************************************************/
static int int_rnd(float f) {
  if (f<0.0) {
    return(-(int)(-f+0.5));
  } else {
    return((int)(f+0.5));
  }
}






/******************************************************************
 * static int_rnd_down
 ******************************************************************
 * For rounding down
 * Oddly, the #define took longer so use inline fcn
 * #define int_rnd_down(f) ((f<0.0)&&(f!=((float)((int)f))))?\
 * ((int)(f-1.0)):\
 * ((int)f)
 ******************************************************************/
static int int_rnd_down(float f) {
    /* Handle negatives differently -- unless the floats are exact
     * integers (like -3.0 ...)
     */
    if ((f<0.0)&&(f!=((float)((int)f)))) {
	return((int)(f-1.0));  /* because truncating rounds UP to next
				* higher when negative
				*/
    } else {
	return((int)f);
    }
}



/******************************************************************
 * static float subtract_tiny
 ******************************************************************
 * subtracts tiny number from f BUT not so tiny that f doesn't change
 ******************************************************************/
/*static float subtract_tiny(float f) {
 *    float tiny = EPSILON;
 *    float initial;
 *
 *    initial = f;
 *
 *    do {
 *	f = initial-tiny;
 *	if (initial!=f) return(f);
 *	tiny *= 2.0;
 *    } while (1);
 *}
 */

/******************************************************************
 * static int ifsign
 ******************************************************************
 * given a float value, returns 1 if positive, -1 if negative, 0
 * if neither
 ******************************************************************/
static int ifsign(float f) {
  if (f>0.0) return(1);
  else if (f<0.0) return(-1);
  else return(0);
}

/******************************************************************
 * static void place_in_bounds
 ******************************************************************
 * Given three float values, multiples them by a positive constant
 * so that the new largest number (in absolute value) is 1.0
 ******************************************************************/
static void place_in_bounds(float *x, float *y, float *z) {
  float largest_abs, multby;

  largest_abs = MAX(((float)ifsign(*x))*(*x) , ((float)ifsign(*y))*(*y));
  largest_abs = MAX(largest_abs , ((float)ifsign(*z))*(*z));

  multby = 1.0/largest_abs;
  *x *= multby;
  *y *= multby;
  *z *= multby;
  /*  if ((((*x)<-1.0)||((*x)>1.0))||(((*y)<-1.0)||((*y)>1.0))||
   *      (((*z)<-1.0)||((*z)>1.0))) {
   *    printf("Left -1 1 bound:  %f %f %f\n", *x, *y, *z);
   *  }
   */
}

/******************************************************************
 * static void round_to_bignum
 ******************************************************************
 * Given three float values, rounds each to the nearest 1/32768
 * (Currently 'nearest' means truncating -- or rounding towards
 * 0)
 * --> May be better to use a rounding strategy that doesn't use
 *     just truncating in rounding to nearest
 ******************************************************************/
static void round_to_bignum(float *x, float *y, float *z) {
  return;

  /* commented out by CLA 6-22-98 to rid of "statement not reached warning"*/
  /*
  *x = ((float)((int)((*x)*32768.0)))/32768.0;
  *y = ((float)((int)((*y)*32768.0)))/32768.0;
  *z = ((float)((int)((*z)*32768.0)))/32768.0;
  */
}

/******************************************************************
 * static float find_voxel_edge
 ******************************************************************
 * If velocity is to right, reports left side of voxel (x).
 * If velocity is to left, reports right side of voxel (x+1).
 * (Use opposite side so we see where the voxel was hit)
 * --> x is some integer position
 * (Basically, x as an integer as it stands represents the
 *  leftmost edge of a voxel.  So, if we're moving to the right
 *  then that is the edge hit first.  If we're instead moving
 *  to the left then we hit the right side which is infinitesimally
 *  smaller than (x+1) -- however, we just report x+1 and then
 *  leave it to the calling routine to add a small delta to the
 *  direction of the ray to move just inside the voxel rather than
 *  on the very edge.)
 * --> v is 'velocity'; convention is '-' to left, '+' to right
 ******************************************************************/
static float find_voxel_edge(int x, float v) {
  if (v<0.0) {
    return((float)(x+1));
  } else {
    return((float)x);
  }
}

/******************************************************************
 * static unsigned char geom_array_lookup
 ******************************************************************
 * Given 3-d array coordinates, returns the raw uvvalue
 * (returns 0 if out of bounds)
 * Note that y is reversed in some sense
 * *** CAN ONLY BE USED AFTER CALLING INIT RAY ENVIRONMENT ***
 ******************************************************************/
static unsigned char geom_array_lookup(geom_info_t *geom, int x, int y, int z) {

    if ((x<0)||(y<0)||(z<0)||(x>=gwidth)||(y>=gheight)||(z>=gnumslices)) {
	return(0);
    }
    /* Here's where we swap our y as computer screen y's are
     * opposite our standard y's
     */
#ifdef INTERSECT_CODE
    univels_hit++; /* global variable to keep track of */
#endif
    return((geom->regionnum)[*(garr+((z+1)*gheight - y - 1)*gwidth + x)]);
}

#define BOUNDS_ERROR -1
#define BUFFER_MATERIAL 1
/******************************************************************
 * static int regionnum_lookup
 ******************************************************************
 * Given 3-d array coordinates, is similar to geom_array_lookup
 * but:
 *   returns int vs unsigned char
 *     * BOUNDS_ERROR returned for error condition
 *   returns the actual regionnum rather than the raw uvval
 * Note that y is reversed in some sense
 * *** CAN ONLY BE USED AFTER CALLING INIT RAY ENVIRONMENT ***
 ******************************************************************/
static int regionnum_lookup(geom_info_t *geom, int x, int y, int z) {

    if ((x<0)||(y<0)||(z<0)||(x>=gwidth)||(y>=gheight)||(z>=gnumslices)) {
	return(BOUNDS_ERROR);
    }
    /* Here's where we swap our y as computer screen y's are
     * opposite our standard y's
     */
#ifdef INTERSECT_CODE
    univels_hit++; /* global variable to keep track of */
#endif
    return(geom_ptr->regionnum[*(garr+((z+1)*gheight - y - 1)*gwidth + x)]);
}

/******************************************************************
 * static unsigned char lookup_nochecking
 ******************************************************************
 * Given 3-d array coordinates, returns the type of material
 * (assumes x, y, and z are all in bounds)
 * Note that y is reversed in some sense
 * *** CAN ONLY BE USED AFTER CALLING INIT RAY ENVIRONMENT ***
 ******************************************************************/
/* XXX No longer used -- if used should be made to look like
   geom_array_lookup without the inital out of bounds check
static unsigned char lookup_nochecking(int x, int y, int z) {

    /* Here's where we swap our y as computer screen y's are
     * opposite our standard y's
     *
#ifdef INTERSECT_CODE
    univels_hit++; /* global variable to keep track of *
#endif
    return(*(garr+((z+1)*gheight - y - 1)*gwidth + x));
}
*/



/******************************************************************
 * static void ndcf_to_wcf
 ******************************************************************
 * Normalized device coordinates (floats) to world coordinates (floats)
 * given x, y, z of array (as floats) computes the actual x, y, z
 * corresponding to the length dimensions of the image
 * NOTE:  if xf, yf, and zf are first truncated to ints, then the
 *        standard would be to first add 0.5 to each coordinate
 *        before converting (0.5 means ~"in middle" of the pixel)
 ******************************************************************/
static void ndcf_to_wcf(geom_info_t * geom, float xf, float yf, float zf,
		 float *x, float *y, float *z) {
    
    *x = geom->rlaxismin + xf*geom->pixelsizecolumns;
    *y = geom->paaxismin + yf*geom->pixelsizerows;
    *z = geom->isaxismin + zf*geom->pixelsizeslices;
}




/******************************************************************
 * static void wcf_to_ndcf 
 ******************************************************************
 * World coordinates (floats) to normalized device coordinates (floats)
 * given x, y, z of actual position in image, returns the array
 * coordinates that correspond (returns array coords as FLOATS
 * and then these floats should be truncated if you actually want
 * to do a lookup into an array
 ******************************************************************/
static void wcf_to_ndcf(geom_info_t * geom,
		 float xf, float yf, float zf,
		 float *x, float *y, float *z) {
    
    *z = (zf - geom->isaxismin)*geom->inv_pixelsizeslices;
    *y = (yf - geom->paaxismin)*geom->inv_pixelsizerows;
    *x = (xf - geom->rlaxismin)*geom->inv_pixelsizecolumns;

}

/******************************************************************
 * static unsigned char get_mapping
 ******************************************************************
 * Given a 'raw' value created by drawing polygon borders and
 * filling them in, converts to the value that will be output
 * in the output file
 ******************************************************************/
static unsigned char get_mapping(geom_info_t * geom_ptr, char ch) {
  char low_bit;
  char high_bit;
  int i;

  /* If it's 0, it becomes 1 */
  if (ch==0) {
    return(1);
  }

  if (ch&128) {
    /* high bit set -- it's a boundary -- set to lowest bit*2 */
    /* Note:  this is a somewhat arbitrary decision that basically says that
     * on shared boundaries, the region with the lowest bit takes precedence
     * --> It is assumed that these are the outermost regions although that
     *     is not particularly necessary
     */
    for (i=0; i<=6; i++) {
      if (bitregions[i]&ch) {
	return(geom_ptr->uvval[bitregions[i]*2]);
      }
    }
  } else {
    /* high bit not set -- not a boundary -- set to highest bit*2 */
    /* NOTE: could probably do this simpler because as of 06-06-97
     * overlapped bodies are allowed to occur only on boundaries
     * (probably could just return ch*2)
     */
    for (i=6; i>=0; i--) {
      if (bitregions[i]&ch) {
	return(geom_ptr->uvval[bitregions[i]*2]);
      }
    }
  }
}

/******************************************************************
 * static unsigned char add_line
 ******************************************************************
 * draws a line and returns the most occurring previous value at
 * those points
 ******************************************************************/
unsigned char add_line(geom_info_t *geom_ptr, unsigned char regbit, float x1, float y1, float x2, float y2, int zval) {
  int xval, yval, stop, realyval, i;
  int index, first_index;
  float x1arr, y1arr, x2arr, y2arr, junk;
  float distx, disty;
  float slope;
  unsigned char prev_color;

  /* Note that the y's passed need to be subtracted from height before
   * writing into the array --> hence, realyval
   */
  /* Need to compute xvals and yvals along the line */

  wcf_to_ndcf(geom_ptr, x1, y1, 0.0, &x1arr, &y1arr, &junk);
  wcf_to_ndcf(geom_ptr, x2, y2, 0.0, &x2arr, &y2arr, &junk);

  /* Choose a sampling point for the previous color as the "right" end of
   * the current line being drawn
   */
  prev_color=(geom_array_lookup(geom_ptr, x2arr, y2arr, zval)&127);

  distx = (float)fabs((double)(x1arr-x2arr));
  disty = (float)fabs((double)(y1arr-y2arr));

  for (i=0; i<2; i++) {
    /* draw the endpoints first as there's a chance the last endpoint won't get drawn */
    switch(i)
      {
      case 0:
	xval=x1arr;
	yval=y1arr;
	break;
      case 1:
	first_index = index; /* save copy of first index for later */
	xval=x2arr;
	yval=y2arr;
	break;
      }
    realyval=(geom_ptr->imagerows-1-yval);
    
    index = zval*geom_ptr->imagecolumns*geom_ptr->imagerows+realyval*geom_ptr->imagecolumns+xval;
    if (geom_array_lookup(geom_ptr, xval, yval, zval)>=128) {
      /* if >= 128 then it's a _shared_ boundary */
      geom_ptr->vol_arr[index]=(geom_array_lookup(geom_ptr, xval, yval, zval)|regbit);
    } else {
      /* if < 128 then can eliminate what is currently there */
      geom_ptr->vol_arr[index]=regbit;
    }
  }

  /* Only look at in-between points if start and end don't map to
   * same spot
   */
  if (first_index!=index) {
      
      if (distx>=disty) {
	  /* here, step along in x */
	  /* swap if necessary */
	  if (x1arr>x2arr) {
	      junk=x1arr;
	      x1arr=x2arr;
	      x2arr=junk;
	      junk=y1arr;
	      y1arr=y2arr;
	      y2arr=junk;
	  }
	  slope=(y2arr-y1arr)/(x2arr-x1arr);
	  stop=x2arr;
	  xval=x1arr;
	  yval=y1arr;
	  realyval=(geom_ptr->imagerows-1-yval);
	  while (xval<=stop) {
	      index = zval*geom_ptr->imagecolumns*geom_ptr->imagerows+realyval*geom_ptr->imagecolumns+xval;
	      if (geom_array_lookup(geom_ptr, xval, yval, zval)>=128) {
		  /* if >= 128 then it's a _shared_ boundary */
		  geom_ptr->vol_arr[index]=(geom_array_lookup(geom_ptr, xval, yval, zval)|regbit);
	      } else {
		  /* if < 128 then can eliminate what is currently there */
		  geom_ptr->vol_arr[index]=regbit;
	      }
	      y1arr+=slope;
	      xval++;
	      yval=y1arr;
	      realyval=(geom_ptr->imagerows-1-yval);
	  }
	  
      } else {
	  /* here, step along in y */
	  /* swap if necessary */
	  if (y1arr>y2arr) {
	      junk=x1arr;
	      x1arr=x2arr;
	      x2arr=junk;
	      junk=y1arr;
	      y1arr=y2arr;
	      y2arr=junk;
	  }
	  slope=(x2arr-x1arr)/(y2arr-y1arr);
	  stop=y2arr;
	  xval=x1arr;
	  yval=y1arr;
	  realyval=(geom_ptr->imagerows-1-yval);
	  while (yval<=stop) {
	      index = zval*geom_ptr->imagecolumns*geom_ptr->imagerows+realyval*geom_ptr->imagecolumns+xval;
	      if (geom_array_lookup(geom_ptr, xval, yval, zval)>=128) {
		  /* if >= 128 then it's a _shared_ boundary */
		  geom_ptr->vol_arr[index]=(geom_array_lookup(geom_ptr, xval, yval, zval)|regbit);
	      } else {
		  /* if < 128 then can eliminate what is currently there */
		  geom_ptr->vol_arr[index]=regbit;
	      }
	      x1arr+=slope;
	      xval=x1arr;
	      yval++;
	      realyval=(geom_ptr->imagerows-1-yval);
	  }
      }
  }
  /* Return the representive previous color of points before line -- or
   * 255 if it's a border or not a power of 2
   */
  if (prev_color!=(regbit&127)) {
    switch(prev_color)
      {
      case 0:
	return(0);
      case 1:
	return(1);
      case 2:
	return(2);
      case 4:
	return(4);
      case 8:
	return(8);
      case 16:
	return(16);
      case 32:
	return(32);
      case 64:
	return(64);
      default:
	return(255);
      }
  } else {
    return(255);
  }
}

/******************************************************************
 * static void fill_region_driver
 ******************************************************************
 * mainly calls fill_region -- but, first looks at the passed
 * value to see what value we are to replace during our fill
 ******************************************************************/
static void fill_region_driver(geom_info_t *geom_ptr, int x, int y, int z, unsigned char fillbit, unsigned char color_to_replace) {
  int maxix, maxiy, maxiz;
  unsigned char *arr;
  unsigned char *buffer;

  arr = (unsigned char*)geom_ptr->vol_arr;

  maxix=geom_ptr->imagecolumns;
  maxiy=geom_ptr->imagerows;
  maxiz=geom_ptr->imageslices;

  /* just return if outside box */
  if ((x<0)||(y<0)||(z<0)||(x>=maxix)||(y>=maxiy)||(z>=maxiz)) return;

  /* color_to_replace=arr[z*maxix*maxiy+y*maxix+x]; now a parameter */

  /* just return if on a boundary */
  if (color_to_replace>=128) return;

  /* make a buffer and set all values to 0 */
  buffer = (unsigned char*)malloc(maxix*maxiy*sizeof(unsigned char));
  memset((void*)buffer, 0, maxix*maxiy);

  fill_region(geom_ptr, x, y, z, fillbit, color_to_replace, buffer);
  free((void*)buffer);
}


void PUSH(int X, int Y, Pt **sp_ptr, Pt *top) {
  if ((*sp_ptr)<top)
    {(*sp_ptr)->y=Y; (*sp_ptr)->x=X;  (*sp_ptr)++;}
  else
    printf("Warning.  Stack is full.\n");
}

void POP(int *X, int *Y, Pt **sp_ptr) {
  (*sp_ptr)--;
  *Y=(*sp_ptr)->y;
  *X=(*sp_ptr)->x;
}

/******************************************************************
 * static void fill_region
 ******************************************************************
 * assumes fillbit is a number:  1, 2, 4, 8, 16, 32, or 64
 * assumes the boundary around x, y, z is has a 1 in highest bit (>=128)
 ******************************************************************/
static void fill_region(geom_info_t *geom_ptr, int x, int y, int z, unsigned char fillbit, unsigned char color_to_replace, unsigned char *buffer) {
  int ix, iy, iz, maxix, maxiy, maxiz;
  unsigned char *arr;
  int xstop, ystop;
  int MAXSTACK;
  int i, j, mid_x, mid_y, index;
  int count=0, wh;
  Pt *stack, *sp, *top;

  arr = (unsigned char*)geom_ptr->vol_arr;

  maxix=geom_ptr->imagecolumns;
  maxiy=geom_ptr->imagerows;
  maxiz=geom_ptr->imageslices;

  /* just return if outside box */
  if ((x<0)||(y<0)||(z<0)||(x>=maxix)||(y>=maxiy)||(z>=maxiz)) return;
  wh = maxix*maxiy;
  MAXSTACK=wh;
  arr+=z*wh;

  index = y*maxix+x;

  /* buffer is assumed to have been initialized to 0's */
  buffer[index]=1;
  if (arr[index]==color_to_replace) {
      arr[index]=(unsigned char)fillbit;
      count++;
  } else if (arr[index]&fillbit) {
      /* In this case, started on border of the region
       * we're filling in -- not allowed! */
      return;
  }

  stack = (Pt *)malloc(MAXSTACK*sizeof(Pt));
  sp = stack;
  top = stack+MAXSTACK-1;

  PUSH(x, y, &sp, top);

  while (sp>stack) {
    POP(&mid_x, &mid_y, &sp);
    /* look at the 4-neighbors */
    x = mid_x - 2;
    for (i=0; i<3; i++) {
      x++;
      y = mid_y - 2;
      for (j=0; j<3; j++) {
	y++;
	if ((i==1)&&(j==1)) continue; /* don't look at self */

	/* Do not look at diagonal neighbors */
	if ((i==0)&&(j==0)) continue;
	if ((i==2)&&(j==2)) continue;
	if ((i==0)&&(j==2)) continue;
	if ((i==2)&&(j==0)) continue;

	/* don't go out of bounds */
	if ((x<0)||(x>=maxix)||(y<0)||(y>=maxiy)) continue;
	index = y*maxix+x;
	/* mark the element and visit neighbors, if needed */
	/* All neighbors will be checked if the element gets marked */
	if (!buffer[index]) {
	    buffer[index]=1;
	    if (arr[index]==color_to_replace) {
		/* This is the color we need to replace */
		arr[index]=(unsigned char)fillbit;
		count++;
		/* It wasn't a border so we visit the neighbors */
		PUSH(x, y, &sp, top);
	    } else if (!arr[index]&fillbit) {
		/* Here, we're inside some other regions but not
		 * at the border yet -- we don't fill the pixel
		 * in with fillbit but we still visit the neighbors
		 */
		PUSH(x, y, &sp, top);
	    } /* else we're on a border of the region being filled in
	      * and we stop visiting neighbors
	      */
	}
      }
    }
  }

  free((void*)stack);
}

/******************************************************************
 * static void fill_region_wvect
 ******************************************************************
 * Given a point in wc and a vector, moves along the vector to find
 * (hopefully) a point inside the region then calls other function
 * with that point to fill in the region
 ******************************************************************/
static void fill_region_wvect(geom_info_t *geom, float xstart, float ystart, float xvect, float yvect, int planeno, unsigned char regbit, unsigned char color_to_replace) {
  int xpt, ypt;
  float xf, yf, xfv, yfv, vsize, junk;
  int maxix, maxiy, maxiz, zoffset;
  unsigned char * arr;

  arr = (unsigned char *)geom->vol_arr;

  maxix=geom->imagecolumns;
  maxiy=geom->imagerows;
  maxiz=geom->imageslices;
  zoffset = planeno*maxix*maxiy;

  /* Change from WC to NDC */
  wcf_to_ndcf(geom, xstart, ystart, 0.0, &xf, &yf, &junk);
  xfv=xvect*geom->inv_pixelsizecolumns;
  yfv=yvect*geom->inv_pixelsizerows;
  vsize = (float)sqrt((double)(xfv*xfv+yfv*yfv));
  xfv/=vsize;
  yfv/=vsize;

  do {
    xf+=xfv;
    yf+=yfv;
    xpt=(int)xf;
    ypt=(int)yf;
    if ((xpt<0)||(ypt<0)||(xpt>=maxix)||(ypt>=maxiy)) {
      fprintf(stderr, "Error, error!  Out of bounds.\n");
      exit(EXIT_FAILURE);
    }
    /* Keep incrementing so long as we map to a region boundary */
  } while (arr[zoffset+(maxiy-1-ypt)*maxix+xpt]>=128);

  fill_region_driver(geom, xpt, maxiy-1-ypt, planeno, regbit, color_to_replace);
}


/******************************************************************
 * static void float_sort
 ******************************************************************
 * A simple bubble sort for an array of floats of size n
 ******************************************************************/
static void float_sort(float * arr, int n) {
  static int i, j;
  static float tmp;
  static int not_done;

  for (i=0; ((i<n-1)&&(not_done)); i++) {
    not_done = 1;
    for (j=n-2; j>=i; j--) {
      if (arr[j]>arr[j+1]) {
	tmp=arr[j];
	arr[j]=arr[j+1];
	arr[j+1]=tmp;
	not_done = 0;
      }
    }
  }
}

/******************************************************************
 * static void float_sort_3
 ******************************************************************
 * Sorts the 3 floating point elements in array
 ******************************************************************/
static void float_sort_3(float * arr) {
  static float tmp;

  if (arr[0]>arr[1]) {
      tmp = arr[0];
      arr[0] = arr[1];
      arr[1] = tmp;
  }
  if (arr[1]>arr[2]) {
      tmp = arr[1];
      arr[1] = arr[2];
      arr[2] = tmp;
  }
  if (arr[0]>arr[1]) {
      tmp = arr[0];
      arr[0] = arr[1];
      arr[1] = tmp;
  }
}

/******************************************************************
 * static void float_sort_4
 ******************************************************************
 * Sorts the 4 floating point elements in array
 ******************************************************************/
static void float_sort_4(float * arr) {
  static float tmp;

  if (arr[0]>arr[3]) {
      tmp = arr[0];
      arr[0] = arr[3];
      arr[3] = tmp;
  }
  if (arr[1]>arr[2]) {
      tmp = arr[1];
      arr[1] = arr[2];
      arr[2] = tmp;
  }
  if (arr[0]>arr[1]) {
      tmp = arr[0];
      arr[0] = arr[1];
      arr[1] = tmp;
  }
  if (arr[2]>arr[3]) {
      tmp = arr[2];
      arr[2] = arr[3];
      arr[3] = tmp;
  }
  if (arr[1]>arr[2]) {
      tmp = arr[1];
      arr[1] = arr[2];
      arr[2] = tmp;
  }
}


/******************************************************************
 * void initialize_geom_info
 ******************************************************************
 * Procedure that indicates all fields in the geom_info_t
 * structure are not valid.
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure
 ******************************************************************/
void initialize_geom_info(geom_info_t * geom_ptr) {
  int i;
  geom_info_valid_t * v;

  v = &(geom_ptr->valid);

  geom_ptr->num_fiducial_markers = 0;
  geom_ptr->num_constraint_markers = 0;
  geom_ptr->fiducial_markers = NULL;
  geom_ptr->constraint_markers = NULL;

  v->num_fiducial_markers = 1;
  v->num_constraint_markers = 1;
  v->fiducial_markers = 1;
  v->constraint_markers = 1;

  v->imagerowaxis=0;
  v->imagecolumnaxis=0;
  v->imagesliceaxis=0;
  v->bodydatatitle=0;
  v->dimensionality=0;
  v->sliceorientation=0;
  v->imageslices=0;
  v->imagecolumns=0;
  v->imagerows=0;
  v->pixelsizeslices=0;
  v->isaxismin=0;
  v->pixelsizecolumns=0;
  v->pixelsizerows=0;
  v->rlaxismin=0;
  v->paaxismin=0;
  v->vol_arr=0;
  v->rlaxismax=0;
  v->paaxismax=0;
  v->isaxismax=0;
  v->inv_pixelsizeslices=0;
  v->inv_pixelsizecolumns=0;
  v->inv_pixelsizerows=0;
  for (i=0; i<LIBUV_MAX_REGIONS; i++) {
    v->regionnum[i]=0;
    v->mean_intensity[i]=0;
    v->color_red[i]=0;
    v->color_green[i]=0;
    v->color_blue[i]=0;
    v->bodyname[i]=0;
    initialize_dose_info(&(geom_ptr->dose[i]));
    v->dose[i]=1;
    v->parent_body[i]=0;
    v->bboxpaaxismin[i]=0;
    v->bboxpaaxismax[i]=0;
    v->bboxrlaxismin[i]=0;
    v->bboxrlaxismax[i]=0;
    v->bboxisaxismin[i]=0;
    v->bboxisaxismax[i]=0;
    v->volume[i]=0;
    v->uvval[i]=0;
  }
}


/******************************************************************
 * void initialize_dose_info
 ******************************************************************
 * Procedure that indicates all fields in the dose_info_t
 * structure are not valid.
 *
 * Parameters:
 * dptr:  ptr to the structure to initialize
 ******************************************************************/
void initialize_dose_info(dose_info_t * dptr) {
  dose_info_valid_t * v;

  dptr->next = NULL;

  v = &(dptr->valid);

  v->bodyname=0;
  v->matname=0;
  v->boron_cf=0;
  v->gamma_rbe=0;
  v->nitrogen_rbe=0;
  v->nitrogen_dens=0;
  v->recoil_rbe=0;
  v->hydrogen_rbe=0;
  v->hydrogen_dens=0;
  v->other_rbe=0;
  v->ultrafast_rbe=0;
  v->carbon_dens=0;
  v->oxygen_dens=0;
  v->tissue_to_blood=0;
  v->maximum_dose=0;
  v->constraint_dose=0;
  v->editable=0;
  
}


/******************************************************************
 * void copy_dose_info
 ******************************************************************
 * Procedure that copies all fields from one dose_info_t
 * structure to another
 *
 * Parameters:
 *   toptr:    Source
 *   fromptr:  Destination
 * DOES NOT ALTER THE FIELD MARKED 'next' (in other words, preserves
 * placement if it's already in a linked list)
 ******************************************************************/
void copy_dose_info(dose_info_t * toptr, dose_info_t * fromptr) {
  if (fromptr->valid.bodyname) {
    strcpy(toptr->bodyname, fromptr->bodyname);
    toptr->valid.bodyname = 1;
  } else {
    toptr->valid.bodyname = 0;
  }
  if (fromptr->valid.matname) {
    strcpy(toptr->matname, fromptr->matname);
    toptr->valid.matname = 1;
  } else {
    toptr->valid.matname = 0;
  }
  toptr->boron_cf = fromptr->boron_cf;
  toptr->valid.boron_cf = fromptr->valid.boron_cf;
  toptr->gamma_rbe = fromptr->gamma_rbe;
  toptr->valid.gamma_rbe = fromptr->valid.gamma_rbe;
  toptr->nitrogen_rbe = fromptr->nitrogen_rbe;
  toptr->valid.nitrogen_rbe = fromptr->valid.nitrogen_rbe;
  toptr->nitrogen_dens = fromptr->nitrogen_dens;
  toptr->valid.nitrogen_dens = fromptr->valid.nitrogen_dens;
  toptr->recoil_rbe = fromptr->recoil_rbe;
  toptr->valid.recoil_rbe = fromptr->valid.recoil_rbe;
  toptr->hydrogen_rbe = fromptr->hydrogen_rbe;
  toptr->valid.hydrogen_rbe = fromptr->valid.hydrogen_rbe;
  toptr->hydrogen_dens = fromptr->hydrogen_dens;
  toptr->valid.hydrogen_dens = fromptr->valid.hydrogen_dens;
  toptr->other_rbe = fromptr->other_rbe;
  toptr->valid.other_rbe = fromptr->valid.other_rbe;
  toptr->ultrafast_rbe = fromptr->ultrafast_rbe;
  toptr->valid.ultrafast_rbe = fromptr->valid.ultrafast_rbe;
  toptr->carbon_dens = fromptr->carbon_dens;
  toptr->valid.carbon_dens = fromptr->valid.carbon_dens;
  toptr->oxygen_dens = fromptr->oxygen_dens;
  toptr->valid.oxygen_dens = fromptr->valid.oxygen_dens;
  toptr->tissue_to_blood = fromptr->tissue_to_blood;
  toptr->valid.tissue_to_blood = fromptr->valid.tissue_to_blood;
  toptr->maximum_dose = fromptr->maximum_dose;
  toptr->valid.maximum_dose = fromptr->valid.maximum_dose;
  toptr->constraint_dose = fromptr->constraint_dose;
  toptr->valid.constraint_dose = fromptr->valid.constraint_dose;
  toptr->editable = fromptr->editable;
  toptr->valid.editable = fromptr->valid.editable;
  
}


/******************************************************************
 * void read_uvh_minimal
 ******************************************************************
 * Function to call read_uvh_minimal_fptr below.
 * (See read_uvh_minimal_fptr below for more information.)
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure to fill
 *  name     --> name of the input filename
 ******************************************************************/
void read_uvh_minimal(geom_info_t * geom_ptr, char *headerfile) {
  FILE * fptr;
  
  /* The last 0 means to close the file */
  fptr = read_uvh_minimal_fptr(geom_ptr, headerfile, 0);
}


/******************************************************************
 * static FILE * read_uvh_minimal_fptr
 ******************************************************************
 * Gets information from uvh file and stores in geom_ptr data structure.
 * This particular function ignores body information and is
 * intended to be called by programs that only want to read partial
 * information of a transitory uvh file in association with _generating_
 * uv file pairs rather than reading a fully pre-made uv pair.  read_uvh
 * uses this function in conjunction with its own routines for extracting
 * additional data.
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure to fill
 *  name     --> name of the input filename
 *  leave_open-> 1 if should leave input file open, 0 if not
 *
 * Returns:
 *  FILE * ptr to current location in file
 ******************************************************************/
static FILE * read_uvh_minimal_fptr(geom_info_t * geom_ptr, char *headerfile, int leave_open) {
  int i;
  FILE *fptr;
  char value[MAX_UVH_LINE_SIZE];
  
  /* Set all fields initially to invalid */
  initialize_geom_info(geom_ptr);

  /* Make some assumptions for defaults -- overwritten by .uvh file */
  for (i=0; i<LIBUV_MAX_REGIONS; i++) {
    geom_ptr->regionnum[i]=i;
    geom_ptr->uvval[i]=i;
    sprintf(geom_ptr->bodyname[i], "body%d", i);
  }
  /* End initialization assumptions */
  
  if (!(fptr=fopen(headerfile, "r"))) {
    fprintf(stderr, "Could not open specified file %s.\nExiting.\n",
	    headerfile);
    exit(EXIT_FAILURE);
  }
  
  /* Check the version */
  if (!UV_read_string_value_for_key(fptr, UVH_VERSION, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_VERSION);
    exit(EXIT_FAILURE);
  } else {
    if (strcmp(value, CURRENT_VERSION)!=0) {
      fprintf(stderr, "Invalid header file:  %s\n", headerfile);
      fprintf(stderr, "Not of proper version.  File must have line:\n");
      fprintf(stderr, "%s%s%s\n", UVH_VERSION, KEY_VALUE_SPLIT_STRING,
	      CURRENT_VERSION);
      fprintf(stderr, "Must exit.\n");
      exit(EXIT_FAILURE);
    }
  }

  /* Read the units */
  if (!UV_read_string_value_for_key(fptr, UVH_DIMENSIONALITY, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_DIMENSIONALITY);
    exit(EXIT_FAILURE);
  } else {
    UV_make_lower_string(value);
    if (!strcmp(value, "mm")) {
      /* great, mm is acceptable */
    } else if (!strcmp(value, "cm")) {
      /* great, cm is acceptable */
    } else {
      fprintf(stderr, "Unacceptable units in .uvh file.  Must be cm or mm.\n");
      fprintf(stderr, "Found:  %s\n", value);
      exit(EXIT_FAILURE);
    }
    strcpy(geom_ptr->dimensionality, value);
    geom_ptr->valid.dimensionality = 1;
  }

  /* Read SliceOrientation */
  if (!UV_read_string_value_for_key(fptr, UVH_SLICEORIENTATION, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_SLICEORIENTATION);
    exit(EXIT_FAILURE);
  } else {
    UV_make_lower_string(value);
    if (!strcmp(value, "transverse"))
    {
        /* great, transverse is acceptable */
        strcpy(geom_ptr->sliceorientation, "Transverse");
    }
    else if (!strcmp(value, "axial"))
    {
        /* great, axial is acceptable */
        strcpy(geom_ptr->sliceorientation, "Axial");
    }
    else if (!strcmp(value, "sagittal"))
    {
        /* great, sagittal is acceptable */
        strcpy(geom_ptr->sliceorientation, "Sagittal");
    }
    else if (!strcmp(value, "coronal"))
    {
        /* great, coronal is acceptable */
        strcpy(geom_ptr->sliceorientation, "Coronal");
    }
    else if (!strcmp(value, "oblique"))
    {
        /* great, oblique is acceptable */
        strcpy(geom_ptr->sliceorientation, "Oblique");
    
        /* However, it is not yet supported */
        fprintf(stderr, "Oblique slices are not yet supported.\n");
        fprintf(stderr, "Must exit.\n");
        exit(EXIT_FAILURE);
    }
    else
    {
        fprintf(stderr, "Unacceptable slice orientation in .uvh file.\n");
        fprintf(stderr, "Must be transverse, sagittal, coronal, or oblique.\n");
        fprintf(stderr, "Found:  %s\n", value);
        exit(EXIT_FAILURE);
    }
    geom_ptr->valid.sliceorientation = 1;
  }
  
  /* Read ImageRows */
  if (!UV_read_string_value_for_key(fptr, UVH_IMAGEROWS, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGEROWS);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->imagerows = atoi(value);
    geom_ptr->valid.imagerows = 1;
  }
  
  /* Read ImageColumns */
  if (!UV_read_string_value_for_key(fptr, UVH_IMAGECOLUMNS, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGECOLUMNS);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->imagecolumns = atoi(value);
    geom_ptr->valid.imagecolumns = 1;
  }
  
  /* Read ImageSlices */
  if (!UV_read_string_value_for_key(fptr, UVH_IMAGESLICES, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGESLICES);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->imageslices = atoi(value);
    geom_ptr->valid.imageslices = 1;
  }
  
  /* Read PixelSizeRows */
  if (!UV_read_string_value_for_key(fptr, UVH_PIXELSIZEROWS, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_PIXELSIZEROWS);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->pixelsizerows = (float)atof(value);
    geom_ptr->valid.pixelsizerows = 1;
    geom_ptr->inv_pixelsizerows = 1.0/geom_ptr->pixelsizerows;
    geom_ptr->valid.inv_pixelsizerows = 1;
  }
  
  /* Read PixelSizeColumns */
  if (!UV_read_string_value_for_key(fptr, UVH_PIXELSIZECOLUMNS, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_PIXELSIZECOLUMNS);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->pixelsizecolumns = (float)atof(value);
    geom_ptr->valid.pixelsizecolumns = 1;
    geom_ptr->inv_pixelsizecolumns = 1.0/geom_ptr->pixelsizecolumns;
    geom_ptr->valid.inv_pixelsizecolumns = 1;
  }
  
  /* Read PixelSizeSlices */
  if (!UV_read_string_value_for_key(fptr, UVH_PIXELSIZESLICES, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_PIXELSIZESLICES);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->pixelsizeslices = (float)atof(value);
    geom_ptr->valid.pixelsizeslices = 1;
    geom_ptr->inv_pixelsizeslices = 1.0/geom_ptr->pixelsizeslices;
    geom_ptr->valid.inv_pixelsizeslices = 1;
  }
  
  /* Read PAaxisMin */
  if (!UV_read_string_value_for_key(fptr, UVH_PAAXISMIN, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_PAAXISMIN);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->paaxismin=atof(value);
    geom_ptr->valid.paaxismin = 1;
  }
  
  /* Read PAaxisMax */
  if (!UV_read_string_value_for_key(fptr, UVH_PAAXISMAX, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_PAAXISMAX);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->paaxismax=atof(value);
    geom_ptr->valid.paaxismax = 1;
  }
  
  /* Read RLaxisMin */
  if (!UV_read_string_value_for_key(fptr, UVH_RLAXISMIN, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_RLAXISMIN);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->rlaxismin=atof(value);
    geom_ptr->valid.rlaxismin = 1;
  }
  
  /* Read RLaxisMax */
  if (!UV_read_string_value_for_key(fptr, UVH_RLAXISMAX, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_RLAXISMAX);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->rlaxismax=atof(value);
    geom_ptr->valid.rlaxismax = 1;
  }
  
  /* Read ISaxisMin */
  if (!UV_read_string_value_for_key(fptr, UVH_ISAXISMIN, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_ISAXISMIN);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->isaxismin=atof(value);
    geom_ptr->valid.isaxismin = 1;
  }
  
  /* Read ISaxisMax */
  if (!UV_read_string_value_for_key(fptr, UVH_ISAXISMAX, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_ISAXISMAX);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->isaxismax=atof(value);
    geom_ptr->valid.isaxismax = 1;
  }
  
  {
    char *axes[6]={"pa+", "pa-", "rl+", "rl-", "is+", "is-"};
    char *write_axes[6]={"PA+", "PA-", "RL+", "RL-", "IS+", "IS-"};
    int is_used[3]={0,0,0};

    /* Read ImageRowAxis */
    if (!UV_read_string_value_for_key(fptr, UVH_IMAGEROWAXIS, value, MAX_UVH_LINE_SIZE)) {
      fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGEROWAXIS);
      exit(EXIT_FAILURE);
    } else {
      UV_make_lower_string(value);
      for (i=0; i<6; i++) {
	if (!strcmp(value, axes[i])) {
	  if (!is_used[i/2]) {
	    is_used[i/2] = 1;
	    strcpy(geom_ptr->imagerowaxis, write_axes[i]);
	    geom_ptr->valid.imagerowaxis = 1;
	  } else {
	    fprintf(stderr, "%s axis used twice.\n", value);
	    exit(EXIT_FAILURE);
	  }
	  break;
	}
      }
      if (!geom_ptr->valid.imagerowaxis) {
	fprintf(stderr, "Invalid key value %s for %s\n", value,
		UVH_IMAGEROWAXIS);
	exit(EXIT_FAILURE);
      }
    }
    
    /* Read ImageColumnAxis */
    if (!UV_read_string_value_for_key(fptr, UVH_IMAGECOLUMNAXIS, value, MAX_UVH_LINE_SIZE)) {
      fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGECOLUMNAXIS);
      exit(EXIT_FAILURE);
    } else {
      UV_make_lower_string(value);
      for (i=0; i<6; i++) {
	if (!strcmp(value, axes[i])) {
	  if (!is_used[i/2]) {
	    is_used[i/2] = 1;
	    strcpy(geom_ptr->imagecolumnaxis, write_axes[i]);
	    geom_ptr->valid.imagecolumnaxis = 1;
	  } else {
	    fprintf(stderr, "%s axis used twice.\n", value);
	    exit(EXIT_FAILURE);
	  }
	  break;
	}
      }
      if (!geom_ptr->valid.imagecolumnaxis) {
	fprintf(stderr, "Invalid key value %s for %s\n", value,
		UVH_IMAGECOLUMNAXIS);
	exit(EXIT_FAILURE);
      }
    }
    
    /* Read ImageSliceAxis */
    if (!UV_read_string_value_for_key(fptr, UVH_IMAGESLICEAXIS, value, MAX_UVH_LINE_SIZE)) {
      fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGESLICEAXIS);
      exit(EXIT_FAILURE);
    } else {
      UV_make_lower_string(value);
      for (i=0; i<6; i++) {
	if (!strcmp(value, axes[i])) {
	  if (!is_used[i/2]) {
	    is_used[i/2] = 1;
	    strcpy(geom_ptr->imagesliceaxis, write_axes[i]);
	    geom_ptr->valid.imagesliceaxis = 1;
	  } else {
	    fprintf(stderr, "%s axis used twice.\n", value);
	    exit(EXIT_FAILURE);
	  }
	  break;
	}
      }
      if (!geom_ptr->valid.imagesliceaxis) {
	fprintf(stderr, "Invalid key value %s for %s\n", value,
		UVH_IMAGESLICEAXIS);
	exit(EXIT_FAILURE);
      }
    }
  }
  
  if (!leave_open) {
    fclose(fptr);
  }
  
  return(fptr);
}


/******************************************************************
 * void free_read_uvh_markers
 ******************************************************************
 * When read_uvh is performed, space is malloced for each marker
 * that is found and its name.  This should be freed.
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure
 ******************************************************************/
void free_read_uvh_markers(geom_info_t * geom_ptr) {
  marker_type * cur = NULL, * next;
  int num;

  num = geom_ptr->num_fiducial_markers;
  if (num>0)
    cur = geom_ptr->fiducial_markers;
  while (cur) {
    next = cur->next;
    if (cur->name) {
      free((void*)cur->name);
    }
    free((void*)cur);
    cur = next;
  }

  cur = NULL;

  num = geom_ptr->num_constraint_markers;
  if (num>0)
    cur = geom_ptr->constraint_markers;
  while (cur) {
    next = cur->next;
    if (cur->name) {
      free((void*)cur->name);
    }
    free((void*)cur);
    cur = next;
  }
}


/******************************************************************
 * void read_uvh
 ******************************************************************
 * Gets information from uvh file and stores in geom_ptr data structure
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure to fill
 *  name     --> name of the input filename
 * XXX Must call free_read_uvh_markers afterward
 ******************************************************************/
void read_uvh(geom_info_t * geom_ptr, char *headerfile) {
  FILE *fptr;
  char str[MAX_UVH_LINE_SIZE], * key, * value;
  
  /* Read basic information in uvh file -- most important info */
  /* (The 1 parameter means to leave the file open and return a ptr to it) */

  /* This call will first initialize the structure */
  fptr = read_uvh_minimal_fptr(geom_ptr, headerfile, 1);

  /* Here, want to read groups of 1 of 3 things (at a time)
   * (1) Body information
   * (2) Constraint marker information
   * (3) Fiducial marker information
   */

  rewind(fptr);
  while (UV_read_next_key_and_value(fptr, str, MAX_UVH_LINE_SIZE,
				    &key, &value)) {
    if (!strcmp(key, UVH_BEGIN)) {
      read_body_information(fptr, geom_ptr, value);
    } else if (!strcmp(key, UVH_CONSTRAINT)) {
      read_constraint_information(fptr, geom_ptr, value);
    } else if (!strcmp(key, UVH_FIDUCIAL)) {
      read_fiducial_information(fptr, geom_ptr, value);
    } /* else { ignore it } */
  }

  fclose(fptr);
}


/******************************************************************
 * void read_existing_uvh
 ******************************************************************
 * Gets information from uvh file and stores in geom_ptr data structure
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure to fill
 *  name     --> name of the input filename
 * XXX Must call free_read_uvh_markers afterward
 ******************************************************************/
void read_existing_uvh(geom_info_t * geom_ptr, char *headerfile) {
  int i;
  FILE *fptr;
  char str[MAX_UVH_LINE_SIZE], * key, * lvalue, value[MAX_UVH_LINE_SIZE];
  
  /* Read basic information in uvh file -- most important info */
  /* (The 1 parameter means to leave the file open and return a ptr to it) */

  if (!(fptr=fopen(headerfile, "r"))) {
    fprintf(stderr, "Could not open specified file %s.\nExiting.\n",
	    headerfile);
    exit(EXIT_FAILURE);
  }
  
  /* Check the version */
  if (!UV_read_string_value_for_key(fptr, UVH_VERSION, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_VERSION);
    exit(EXIT_FAILURE);
  } else {
    if (strcmp(value, CURRENT_VERSION)!=0) {
      fprintf(stderr, "Invalid header file:  %s\n", headerfile);
      fprintf(stderr, "Not of proper version.  File must have line:\n");
      fprintf(stderr, "%s%s%s\n", UVH_VERSION, KEY_VALUE_SPLIT_STRING,
	      CURRENT_VERSION);
      fprintf(stderr, "Must exit.\n");
      exit(EXIT_FAILURE);
    }
  }

  /* Read the units */
  if (!UV_read_string_value_for_key(fptr, UVH_DIMENSIONALITY, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_DIMENSIONALITY);
    exit(EXIT_FAILURE);
  } else {
    UV_make_lower_string(value);
    if (!strcmp(value, "mm")) {
      /* great, mm is acceptable */
    } else if (!strcmp(value, "cm")) {
      /* great, cm is acceptable */
    } else {
      fprintf(stderr, "Unacceptable units in .uvh file.  Must be cm or mm.\n");
      fprintf(stderr, "Found:  %s\n", value);
      exit(EXIT_FAILURE);
    }
    strcpy(geom_ptr->dimensionality, value);
    geom_ptr->valid.dimensionality = 1;
  }

  /* Read SliceOrientation */
  if (!UV_read_string_value_for_key(fptr, UVH_SLICEORIENTATION, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_SLICEORIENTATION);
    exit(EXIT_FAILURE);
  } else {
    UV_make_lower_string(value);
    if (!strcmp(value, "transverse"))
    {
        /* great, transverse is acceptable */
        strcpy(geom_ptr->sliceorientation, "Transverse");
    }
    else if (!strcmp(value, "axial"))
    {
        /* great, axial is acceptable */
        strcpy(geom_ptr->sliceorientation, "Axial");
    }
    else if (!strcmp(value, "sagittal"))
    {
        /* great, sagittal is acceptable */
        strcpy(geom_ptr->sliceorientation, "Sagittal");
    }
    else if (!strcmp(value, "coronal"))
    {
        /* great, coronal is acceptable */
        strcpy(geom_ptr->sliceorientation, "Coronal");
    }
    else if (!strcmp(value, "oblique"))
    {
        /* great, oblique is acceptable */
        strcpy(geom_ptr->sliceorientation, "Oblique");
    
        /* However, it is not yet supported */
        fprintf(stderr, "Oblique slices are not yet supported.\n");
        fprintf(stderr, "Must exit.\n");
        exit(EXIT_FAILURE);
    }
    else
    {
        fprintf(stderr, "Unacceptable slice orientation in .uvh file.\n");
        fprintf(stderr, "Must be transverse, sagittal, coronal, or oblique.\n");
        fprintf(stderr, "Found:  %s\n", value);
        exit(EXIT_FAILURE);
    }
    geom_ptr->valid.sliceorientation = 1;
  }
  
  /* Read ImageRows */
  if (!UV_read_string_value_for_key(fptr, UVH_IMAGEROWS, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGEROWS);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->imagerows = atoi(value);
    geom_ptr->valid.imagerows = 1;
  }
  
  /* Read ImageColumns */
  if (!UV_read_string_value_for_key(fptr, UVH_IMAGECOLUMNS, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGECOLUMNS);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->imagecolumns = atoi(value);
    geom_ptr->valid.imagecolumns = 1;
  }
  
  /* Read ImageSlices */
  if (!UV_read_string_value_for_key(fptr, UVH_IMAGESLICES, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGESLICES);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->imageslices = atoi(value);
    geom_ptr->valid.imageslices = 1;
  }
  
  /* Read PixelSizeRows */
  if (!UV_read_string_value_for_key(fptr, UVH_PIXELSIZEROWS, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_PIXELSIZEROWS);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->pixelsizerows = (float)atof(value);
    geom_ptr->valid.pixelsizerows = 1;
    geom_ptr->inv_pixelsizerows = 1.0/geom_ptr->pixelsizerows;
    geom_ptr->valid.inv_pixelsizerows = 1;
  }
  
  /* Read PixelSizeColumns */
  if (!UV_read_string_value_for_key(fptr, UVH_PIXELSIZECOLUMNS, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_PIXELSIZECOLUMNS);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->pixelsizecolumns = (float)atof(value);
    geom_ptr->valid.pixelsizecolumns = 1;
    geom_ptr->inv_pixelsizecolumns = 1.0/geom_ptr->pixelsizecolumns;
    geom_ptr->valid.inv_pixelsizecolumns = 1;
  }
  
  /* Read PixelSizeSlices */
  if (!UV_read_string_value_for_key(fptr, UVH_PIXELSIZESLICES, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_PIXELSIZESLICES);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->pixelsizeslices = (float)atof(value);
    geom_ptr->valid.pixelsizeslices = 1;
    geom_ptr->inv_pixelsizeslices = 1.0/geom_ptr->pixelsizeslices;
    geom_ptr->valid.inv_pixelsizeslices = 1;
  }
  
  /* Read PAaxisMin */
  if (!UV_read_string_value_for_key(fptr, UVH_PAAXISMIN, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_PAAXISMIN);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->paaxismin=atof(value);
    geom_ptr->valid.paaxismin = 1;
  }
  
  /* Read PAaxisMax */
  if (!UV_read_string_value_for_key(fptr, UVH_PAAXISMAX, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_PAAXISMAX);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->paaxismax=atof(value);
    geom_ptr->valid.paaxismax = 1;
  }
  
  /* Read RLaxisMin */
  if (!UV_read_string_value_for_key(fptr, UVH_RLAXISMIN, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_RLAXISMIN);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->rlaxismin=atof(value);
    geom_ptr->valid.rlaxismin = 1;
  }
  
  /* Read RLaxisMax */
  if (!UV_read_string_value_for_key(fptr, UVH_RLAXISMAX, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_RLAXISMAX);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->rlaxismax=atof(value);
    geom_ptr->valid.rlaxismax = 1;
  }
  
  /* Read ISaxisMin */
  if (!UV_read_string_value_for_key(fptr, UVH_ISAXISMIN, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_ISAXISMIN);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->isaxismin=atof(value);
    geom_ptr->valid.isaxismin = 1;
  }
  
  /* Read ISaxisMax */
  if (!UV_read_string_value_for_key(fptr, UVH_ISAXISMAX, value, MAX_UVH_LINE_SIZE)) {
    fprintf(stderr, "Header file missing key:  %s\n", UVH_ISAXISMAX);
    exit(EXIT_FAILURE);
  } else {
    geom_ptr->isaxismax=atof(value);
    geom_ptr->valid.isaxismax = 1;
  }
  
  {
    char *axes[6]={"pa+", "pa-", "rl+", "rl-", "is+", "is-"};
    char *write_axes[6]={"PA+", "PA-", "RL+", "RL-", "IS+", "IS-"};
    int is_used[3]={0,0,0};

    /* Read ImageRowAxis */
    if (!UV_read_string_value_for_key(fptr, UVH_IMAGEROWAXIS, value, MAX_UVH_LINE_SIZE)) {
      fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGEROWAXIS);
      exit(EXIT_FAILURE);
    } else {
      UV_make_lower_string(value);
      for (i=0; i<6; i++) {
	if (!strcmp(value, axes[i])) {
	  if (!is_used[i/2]) {
	    is_used[i/2] = 1;
	    strcpy(geom_ptr->imagerowaxis, write_axes[i]);
	    geom_ptr->valid.imagerowaxis = 1;
	  } else {
	    fprintf(stderr, "%s axis used twice.\n", value);
	    exit(EXIT_FAILURE);
	  }
	  break;
	}
      }
      if (!geom_ptr->valid.imagerowaxis) {
	fprintf(stderr, "Invalid key value %s for %s\n", value,
		UVH_IMAGEROWAXIS);
	exit(EXIT_FAILURE);
      }
    }
    
    /* Read ImageColumnAxis */
    if (!UV_read_string_value_for_key(fptr, UVH_IMAGECOLUMNAXIS, value, MAX_UVH_LINE_SIZE)) {
      fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGECOLUMNAXIS);
      exit(EXIT_FAILURE);
    } else {
      UV_make_lower_string(value);
      for (i=0; i<6; i++) {
	if (!strcmp(value, axes[i])) {
	  if (!is_used[i/2]) {
	    is_used[i/2] = 1;
	    strcpy(geom_ptr->imagecolumnaxis, write_axes[i]);
	    geom_ptr->valid.imagecolumnaxis = 1;
	  } else {
	    fprintf(stderr, "%s axis used twice.\n", value);
	    exit(EXIT_FAILURE);
	  }
	  break;
	}
      }
      if (!geom_ptr->valid.imagecolumnaxis) {
	fprintf(stderr, "Invalid key value %s for %s\n", value,
		UVH_IMAGECOLUMNAXIS);
	exit(EXIT_FAILURE);
      }
    }
    
    /* Read ImageSliceAxis */
    if (!UV_read_string_value_for_key(fptr, UVH_IMAGESLICEAXIS, value, MAX_UVH_LINE_SIZE)) {
      fprintf(stderr, "Header file missing key:  %s\n", UVH_IMAGESLICEAXIS);
      exit(EXIT_FAILURE);
    } else {
      UV_make_lower_string(value);
      for (i=0; i<6; i++) {
	if (!strcmp(value, axes[i])) {
	  if (!is_used[i/2]) {
	    is_used[i/2] = 1;
	    strcpy(geom_ptr->imagesliceaxis, write_axes[i]);
	    geom_ptr->valid.imagesliceaxis = 1;
	  } else {
	    fprintf(stderr, "%s axis used twice.\n", value);
	    exit(EXIT_FAILURE);
	  }
	  break;
	}
      }
      if (!geom_ptr->valid.imagesliceaxis) {
	fprintf(stderr, "Invalid key value %s for %s\n", value,
		UVH_IMAGESLICEAXIS);
	exit(EXIT_FAILURE);
      }
    }
  }

  /* Initialize the bodynames, region numbers, and uvvals
   * for each region to zero
   */

    for ( i=0; i<LIBUV_MAX_REGIONS; i++ ) {
       geom_ptr->regionnum[i] = 0;
       geom_ptr->uvval[i] = 0;
       strcpy ( geom_ptr->bodyname[i], "" );
    }

  /* Here, want to read groups of 1 of 3 things (at a time)
   * (1) Body information
   * (2) Constraint marker information
   * (3) Fiducial marker information
   */

  rewind(fptr);
  while (UV_read_next_key_and_value(fptr, str, MAX_UVH_LINE_SIZE,
				    &key, &lvalue)) {
    if (!strcmp(key, UVH_BEGIN)) {
      read_body_information(fptr, geom_ptr, lvalue);
    } else if (!strcmp(key, UVH_CONSTRAINT)) {
      read_constraint_information(fptr, geom_ptr, lvalue);
    } else if (!strcmp(key, UVH_FIDUCIAL)) {
      read_fiducial_information(fptr, geom_ptr, lvalue);
    } /* else { ignore it } */
  }

  fclose(fptr);
}


/******************************************************************
 * void read_uv
 ******************************************************************
 * Gets information from uv file and stores in geom_ptr data structure.
 * Naming policy:  filename need not include extension but file must
 * have .uv.gz or .uv extension in order to be valid and loaded.  If both
 * versions of the file exist, the .uv.gz will take precendence.
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure to fill
 *  name     --> name of the input filename
 ******************************************************************/
void read_uv(geom_info_t * geom_ptr, char *basename) {
  char regionfile[256];
  FILE *fptr;
  int total_pixels;

  total_pixels = geom_ptr->imageslices * geom_ptr->imagecolumns
    * geom_ptr->imagerows;

  /* In this case, we first see if we can load a .gz (compressed) file */
  /* To get .gz, 3 things can happen to basename:
   *   filename.uv.gz    --> no change
   *   filename.uv       --> filename.uv.gz
   *   filename          --> filename.uv.gz
   */
  strcpy(regionfile, basename);
  if (!strstr(regionfile, ".uv.gz")) {
    if (!strstr(regionfile, ".uv"))
      strcat(regionfile, ".uv.gz");  /* convert to fname.uv.gz */
    else
      strcat(regionfile, ".gz");     /* convert to fname.uv.gz */
  }

  /* We assume if basename.uv.gz exists then we use it */
  /* If exists, a gzip process is spawned and pipes are used to
   * extract the uncompressed image
   */
  if ((fptr = fopen(regionfile, "r"))!=NULL) {
    int dd, pd[2], pid, rdsize, status, blocksize, remaining;
    char *bptr;
    
    fclose(fptr); /* only opened it to see if it exists */
	
    /* Create a pipe:
     * pd[0] is for reading (parent does the reading),
     * pd[1] is for writing (child does the writing)
     */
    if (pipe (pd) < 0) {
      fprintf (stderr, "Couldn't open pipe\n");
      exit (EXIT_FAILURE);
    }
    
    /* We fork to have both a parent and a child process */
    /* Upon return, the child sees pid==0 whereas the parent
     * sees the child's pid OR -1 if there was an error
     * Note:  Both child and parent have a copy of pd[]
     */
    
    switch(pid=fork())
      {
      case -1:
	fprintf(stderr, "Can't fork new process.  Must exit.\n");
	exit(EXIT_FAILURE);
	break;
      case 0: /* The child sees this */
	close(pd[0]);                  /* only parent uses this      */
	close(1 /*stdout->_fileno*/);  /* child closes stdout        */
	dd=dup(pd[1]);                 /* child stdout goes to pd[1] */
	
	/* Execute the gzip program to uncompress the file */
	/* command:  gzip -dc regionfile                   */
	execlp("gzip", "gzip", "-dc", regionfile, NULL);
	
	fprintf(stderr, "Execlp failed.  Must exit.  It's likely that gzip is not in your path.\n");
	fprintf(stderr, "Suggestion:  Make sure gzip is in your path _or_ use uncompressed files only.\n");
	exit(EXIT_FAILURE);
	
	/* close(dd); --> no need cause we never get here */
	/* close(pd[1]); */
	break;
      default: /* The parent sees this */
	close(pd[1]);            /* only child uses this */
	/* Here, load the gzipped file into the geom_ptr->vol_arr buffer */
	blocksize=64;            /* assume write buffers at least 64 */
	remaining = total_pixels;
	bptr=(char *)geom_ptr->vol_arr;  /* pointer to voxel data */
	
	while (remaining>0) {
	  if (blocksize>remaining) blocksize=remaining;
	  /* Here, reading pd[0] is same as reading child's stdout */
	  rdsize=read(pd[0], bptr, blocksize);
	  if (rdsize<=0) {
	    fprintf(stderr, "Error reading file:  %s\n", regionfile);
	    fprintf(stderr, "Please check file integrity, make sure gzip is in your path,\nor uncompress file and try again.\n");
	    exit(EXIT_FAILURE);
	  }
	  remaining-=rdsize;
	  bptr+=rdsize;
	}
	wait(&status);
	/* printf("Child exited with status %d\n", status); */
	close(pd[0]);            /* We're done reading the pipe */
      }
    
  } else {
    /* Here, try to load a plain .uv region file */

    /* Add ".uv" to basename if not already present. */
    strcpy(regionfile, basename);
    if (!strstr(regionfile, ".uv"))
      strcat(regionfile, ".uv");   /* basename --> basename.uv */

    if ((fptr = fopen(regionfile, "r"))!=NULL) {
      if (fread(geom_ptr->vol_arr, 1, total_pixels, fptr)!=total_pixels) {
	fprintf(stderr, "File:  %s ended prematurely.  Must exit.\n",
		regionfile);
	exit(EXIT_FAILURE);
      }
      fclose(fptr);
    } else {
      fprintf(stderr, "Sorry, can't open %s.\n", regionfile);
      exit(EXIT_FAILURE);
    }
  }
}




/******************************************************************
 * int validate_top_header
 ******************************************************************
 * Makes sure that all of the top values for the .uvh
 * file are in the structure (marked as valid).
 * If not, will print diagnosis and return 0.
 * Otherwise, returns 1 to indicate success.
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure
 ******************************************************************/
int validate_top_header(geom_info_t * geom_ptr) {
  int retval = 1;
  geom_info_valid_t * v;
  
  v = &(geom_ptr->valid);

  if (!v->dimensionality) {
    fprintf(stderr, "Dimensionality units not found.\n");
    retval = 0;
  }
  if (!v->sliceorientation) {
    fprintf(stderr, "SliceOrientation not found.\n");
    retval = 0;
  }
  if (!v->imageslices) {
    fprintf(stderr, "Number of slices not found.\n");
    retval = 0;
  }
  if (!v->imagecolumns) {
    fprintf(stderr, "Number of columns not found.\n");
    retval = 0;
  }
  if (!v->imagerows) {
    fprintf(stderr, "Number of rows found.\n");
    retval = 0;
  }
  if (!v->pixelsizecolumns) {
    fprintf(stderr, "PixelSizeColumns not found.\n");
    retval = 0;
  }
  if (!v->pixelsizerows) {
    fprintf(stderr, "PixelSizeRows not found.\n");
    retval = 0;
  }
  if (!v->pixelsizeslices) {
    fprintf(stderr, "PixelSizeSlices not found.\n");
    retval = 0;
  }
  if (!v->rlaxismin) {
    fprintf(stderr, "RL start location not found.\n");
    retval = 0;
  }
  if (!v->paaxismin) {
    fprintf(stderr, "PA start location not found.\n");
    retval = 0;
  }
  if (!v->isaxismin) {
    fprintf(stderr, "IS start location not found.\n");
    retval = 0;
  }
  if (!v->rlaxismax) {
    fprintf(stderr, "X end location not found.\n");
    retval = 0;
  }
  if (!v->paaxismax) {
    fprintf(stderr, "Y end location not found.\n");
    retval = 0;
  }
  if (!v->isaxismax) {
    fprintf(stderr, "Z end location not found.\n");
    retval = 0;
  }
  if (!v->imagerowaxis) {
    fprintf(stderr, "ImageRowAxis not specified.\n");
    retval = 0;
  }
  if (!v->imagecolumnaxis) {
    fprintf(stderr, "ImageColumnAxis not specified.\n");
    retval = 0;
  }
  if (!v->imagesliceaxis) {
    fprintf(stderr, "ImageSliceAxis not specified.\n");
    retval = 0;
  }

  return(retval);
}


/******************************************************************
 * void write_uvh
 ******************************************************************
 * Given a name, writes the .uvh file
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure
 *  name     --> name of the output filename
 ******************************************************************/
void write_uvh(geom_info_t * geom_ptr, char *name) {
  char outname[256];
  int i, j, total_pixels;
  char uvinfo[16384]; /* DON'T EXCEED THIS FOR ANY UVVAL */
  char moreinfo[1024]; /* DON'T EXCEED THIS ON A LINE EITHER */
  FILE *fptr;
  dose_info_t * dose;
  char lower_string[256];

  /* Check to make sure that all values at top of header
   * that are required are marked as valid
   */
  if (!validate_top_header(geom_ptr)) {
    fprintf(stderr, "Program terminating due to above missing information.\n");
    exit(EXIT_FAILURE);
  }

  /* Mike:  Also, could consider computing some of the computed
   * values like:
   *   bounding_box  (bounding box for each body)
   *   volume        (volume, in univels, of each body)
   *   mean_intensity (average gray of a given body)
   * Catches:
   *   Don't necessarily have the data to do these computations
   *     as we're not guaranteed to have the vol_arr filled
   *   Mean_intensity requires the original grey-level images
   *     which we especially probably don't have.
   */

  strcpy(outname, name);

  /* add .uvh extension if not present */
  if (!strstr(outname, ".uvh")) {
    strcat(outname, ".uvh");
  }

  if (!(fptr=fopen(outname, "w"))) {
    fprintf(stderr, "Could not generate output file:  %s.\n", outname);
    fprintf(stderr, "Must exit.\n");
    exit(EXIT_FAILURE);
  }

  fprintf(fptr, "\
# In the following file, PA, RL, and IS refer to the following axes\n\
# that correspond to the patient:\n\
# PA -> Posterior to Anterior (back of patient to front)\n\
# RL -> Right to Left         (right side of patient to left)\n\
# IS -> Inferior to Superior  (feet of patient to head)\n\
# These are the primary axes that you should be concerned with.\n\
# The top of this file describes how we map from the saved image\n\
# data of the .uv file into the RL, PA, IS system.\n\n");
    
  fprintf(fptr, "\
Version:                uvh2.0\n\
BodyDataTitle:          %-s\n\
Dimensionality:         %-s\n\
SliceOrientation:       %-s\n\
\n\
# Image is stored primarily by columns, then rows, then slices\n\
# (Column by column to fill each row, then row by row to fill each slice\n\
ImageColumns:           %-4d           # number of columns in .uv slice\n\
ImageRows:              %-4d           # number of rows in .uv slice\n\
ImageSlices:            %-4d           # number of slices in .uv file\n\
\n\
PixelSizeColumns:       %-12.7f   # width of a column\n\
PixelSizeRows:          %-12.7f   # height of a row\n\
PixelSizeSlices:        %-12.7f   # distance between slices\n\
\n\
ImageColumnAxis:        %-10s     # axis as move from column to column\n\
ImageRowAxis:           %-10s     # axis as move from row to row\n\
ImageSliceAxis:         %-10s     # axis as move from slice to slice\n\
\n\
PAaxisMin:              %-12.7f\n\
PAaxisMax:              %-12.7f\n\
RLaxisMin:              %-12.7f\n\
RLaxisMax:              %-12.7f\n\
ISaxisMin:              %-12.7f\n\
ISaxisMax:              %-12.7f\n",
	  geom_ptr->bodydatatitle,
	  geom_ptr->dimensionality,
	  geom_ptr->sliceorientation,
	  geom_ptr->imagecolumns,
	  geom_ptr->imagerows,
	  geom_ptr->imageslices,
	  geom_ptr->pixelsizecolumns,
	  geom_ptr->pixelsizerows, 
	  geom_ptr->pixelsizeslices,
	  geom_ptr->imagecolumnaxis,
	  geom_ptr->imagerowaxis,
	  geom_ptr->imagesliceaxis,
	  geom_ptr->paaxismin, 
	  geom_ptr->paaxismax, 
	  geom_ptr->rlaxismin, 
	  geom_ptr->rlaxismax, 
	  geom_ptr->isaxismin, 

	  geom_ptr->isaxismax);

  fprintf(fptr, "\n# Body and marker information follows:\n");

  for (i=0; i<LIBUV_MAX_REGIONS; i++) {
    if (!geom_ptr->valid.uvval[i]) continue;
    if (!geom_ptr->valid.bodyname[i]) continue;
    sprintf(uvinfo, "\nbegin:  %s\n", geom_ptr->bodyname[i]);
    sprintf(moreinfo, "  uvval:           %d\n", geom_ptr->uvval[i]);
    strcat(uvinfo, moreinfo);
    if (geom_ptr->valid.regionnum[i]) {
      sprintf(moreinfo, "  regionnum:       %d\n", geom_ptr->regionnum[i]);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->valid.color_red[i]) {
      sprintf(moreinfo, "  color_red:       %d\n", geom_ptr->color_red[i]);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->valid.color_green[i]) {
      sprintf(moreinfo, "  color_green:     %d\n", geom_ptr->color_green[i]);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->valid.color_blue[i]) {
      sprintf(moreinfo, "  color_blue:      %d\n", geom_ptr->color_blue[i]);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->valid.mean_intensity[i]) {
      sprintf(moreinfo, "  mean_intensity:  %d\n", geom_ptr->mean_intensity[i]);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.matname) {
      strcpy ( lower_string, geom_ptr->bodyname[i] );
      UV_trim_string ( lower_string );
      UV_make_lower_string ( lower_string );
      if ( strcmp ( lower_string, "buffer" ) != 0 ){
	sprintf(moreinfo, "  matname:         %s\n", geom_ptr->dose[i].matname);
	strcat(uvinfo, moreinfo);
      }
    }
    if (geom_ptr->dose[i].valid.boron_cf) {
      sprintf(moreinfo, "  boron_CF:        %f\n", geom_ptr->dose[i].boron_cf);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.gamma_rbe) {
      sprintf(moreinfo, "  gamma_RBE:       %f\n", geom_ptr->dose[i].gamma_rbe);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.nitrogen_rbe) {
      sprintf(moreinfo, "  nitrogen_RBE:    %f\n", geom_ptr->dose[i].nitrogen_rbe);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.nitrogen_dens) {
      sprintf(moreinfo, "  nitrogen_DENS:   %f\n", geom_ptr->dose[i].nitrogen_dens);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.recoil_rbe) {
      sprintf(moreinfo, "  recoil_RBE:      %f\n", geom_ptr->dose[i].recoil_rbe);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.hydrogen_rbe) {
      sprintf(moreinfo, "  hydrogen_RBE:    %f\n", geom_ptr->dose[i].hydrogen_rbe);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.hydrogen_dens) {
      sprintf(moreinfo, "  hydrogen_DENS:   %f\n", geom_ptr->dose[i].hydrogen_dens);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.other_rbe) {
      sprintf(moreinfo, "  other_RBE:       %f\n", geom_ptr->dose[i].other_rbe);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.ultrafast_rbe) {
      sprintf(moreinfo, "  ultrafast_RBE:   %f\n", geom_ptr->dose[i].ultrafast_rbe);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.carbon_dens) {
      sprintf(moreinfo, "  carbon_DENS:     %f\n", geom_ptr->dose[i].carbon_dens);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.oxygen_dens) {
      sprintf(moreinfo, "  oxygen_DENS:     %f\n", geom_ptr->dose[i].oxygen_dens);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.tissue_to_blood) {
      sprintf(moreinfo, "  tissue_to_blood: %f\n", geom_ptr->dose[i].tissue_to_blood);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.maximum_dose) {
      sprintf(moreinfo, "  maximum_dose:    %f\n", geom_ptr->dose[i].maximum_dose);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->dose[i].valid.constraint_dose) {
      sprintf(moreinfo, "  constraint_dose: %f\n", geom_ptr->dose[i].constraint_dose);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->valid.parent_body[i]) {
      sprintf(moreinfo, "  parent_body:     %s\n", geom_ptr->parent_body[i]);
      strcat(uvinfo, moreinfo);
    }
    if (geom_ptr->valid.bboxrlaxismin[i]) {
      sprintf(moreinfo, "  BBoxRLaxisMin:   %f\n", geom_ptr->bboxrlaxismin[i]);
      strcat(uvinfo, moreinfo);      
    }
    if (geom_ptr->valid.bboxrlaxismax[i]) {
      sprintf(moreinfo, "  BBoxRLaxisMax:   %f\n", geom_ptr->bboxrlaxismax[i]);
      strcat(uvinfo, moreinfo);      
    }
    if (geom_ptr->valid.bboxpaaxismin[i]) {
      sprintf(moreinfo, "  BBoxPAaxisMin:   %f\n", geom_ptr->bboxpaaxismin[i]);
      strcat(uvinfo, moreinfo);      
    }
    if (geom_ptr->valid.bboxpaaxismax[i]) {
      sprintf(moreinfo, "  BBoxPAaxisMax:   %f\n", geom_ptr->bboxpaaxismax[i]);
      strcat(uvinfo, moreinfo);      
    }
    if (geom_ptr->valid.bboxisaxismin[i]) {
      sprintf(moreinfo, "  BBoxISaxisMin:   %f\n", geom_ptr->bboxisaxismin[i]);
      strcat(uvinfo, moreinfo);      
    }
    if (geom_ptr->valid.bboxisaxismax[i]) {
      sprintf(moreinfo, "  BBoxISaxisMax:   %f\n", geom_ptr->bboxisaxismax[i]);
      strcat(uvinfo, moreinfo);      
    }
    if (geom_ptr->valid.volume[i]) {
      sprintf(moreinfo, "  volume:          %f\n", geom_ptr->volume[i]);
      strcat(uvinfo, moreinfo);
    }
    sprintf(moreinfo, "end:  %s\n", geom_ptr->bodyname[i]);
    strcat(uvinfo, moreinfo);
    fprintf(fptr, uvinfo);
  }

  /* Constraint and Fiducial Marker Information */
  /* Need to write out the markers */
  {
    char * textbox_text;
    int x, y, z;
    float value;
    
    for (i=0; i<geom_ptr->num_fiducial_markers; i++) {
      if (geom_ptr->fiducial_markers[i].is_used) {
	fprintf(fptr, "\n%s: %s\n\
  RL:              %f\n\
  PA:              %f\n\
  IS:              %f\n\
end: %s\n",
		UVH_FIDUCIAL,
		geom_ptr->fiducial_markers[i].name,
		(float)geom_ptr->fiducial_markers[i].wcf_x,
		(float)geom_ptr->fiducial_markers[i].wcf_y,
		(float)geom_ptr->fiducial_markers[i].wcf_z,
		geom_ptr->fiducial_markers[i].name);
      }
    }

    for (i=0; i<geom_ptr->num_constraint_markers; i++) {
      if (geom_ptr->constraint_markers[i].is_used) {
	fprintf(fptr, "\n%s: %s\n\
  RL:              %f\n\
  PA:              %f\n\
  IS:              %f\n\
  bodyname:        %s\n",
                UVH_CONSTRAINT,
		geom_ptr->constraint_markers[i].name,
		(float)geom_ptr->constraint_markers[i].wcf_x,
		(float)geom_ptr->constraint_markers[i].wcf_y,
		(float)geom_ptr->constraint_markers[i].wcf_z,
		geom_ptr->constraint_markers[i].bodyname);
	/* Print valid dose info components */
	uvinfo[0] = '\0';
	dose = &(geom_ptr->constraint_markers[i].dose);
	if (dose->valid.matname) {
	  sprintf(moreinfo, "  matname:         %s\n", dose->matname);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.boron_cf) {
	  sprintf(moreinfo, "  boron_CF:        %f\n", dose->boron_cf);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.gamma_rbe) {
	  sprintf(moreinfo, "  gamma_RBE:       %f\n", dose->gamma_rbe);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.nitrogen_rbe) {
	  sprintf(moreinfo, "  nitrogen_RBE:    %f\n", dose->nitrogen_rbe);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.nitrogen_dens) {
	  sprintf(moreinfo, "  nitrogen_DENS:   %f\n", dose->nitrogen_dens);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.recoil_rbe) {
	  sprintf(moreinfo, "  recoil_RBE:      %f\n", dose->recoil_rbe);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.hydrogen_rbe) {
	  sprintf(moreinfo, "  hydrogen_RBE:    %f\n", dose->hydrogen_rbe);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.hydrogen_dens) {
	  sprintf(moreinfo, "  hydrogen_DENS:   %f\n", dose->hydrogen_dens);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.other_rbe) {
	  sprintf(moreinfo, "  other_RBE:       %f\n", dose->other_rbe);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.ultrafast_rbe) {
	  sprintf(moreinfo, "  ultrafast_RBE:   %f\n", dose->ultrafast_rbe);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.carbon_dens) {
	  sprintf(moreinfo, "  carbon_DENS:     %f\n", dose->carbon_dens);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.oxygen_dens) {
	  sprintf(moreinfo, "  oxygen_DENS:     %f\n", dose->oxygen_dens);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.tissue_to_blood) {
	  sprintf(moreinfo, "  tissue_to_blood: %f\n", dose->tissue_to_blood);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.maximum_dose) {
	  sprintf(moreinfo, "  maximum_dose:    %f\n", dose->maximum_dose);
	  strcat(uvinfo, moreinfo);
	}
	if (dose->valid.constraint_dose) {
	  sprintf(moreinfo, "  constraint_dose: %f\n", dose->constraint_dose);
	  strcat(uvinfo, moreinfo);
	}
	fprintf(fptr, "%send: %s\n",
		uvinfo,
		geom_ptr->constraint_markers[i].name);
      }
    }
  }

  fclose(fptr);
}


/******************************************************************
 * void write_uv
 ******************************************************************
 * Given a name, writes the .uv file
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure
 *  name     --> name of the output filename
 ******************************************************************/
void write_uv(geom_info_t * geom_ptr, char *name) {
  char outname[256];
  int total_pixels;
  FILE *fptr;
  
  strcpy(outname, name);

  /* add .uv extension if not present */
  if (!strstr(outname, ".uv")) {
    strcat(outname, ".uv");
  }

  if (!(fptr=fopen(outname, "w"))) {
    fprintf(stderr, "Could not generate output file:  %s.\n", outname);
    fprintf(stderr, "Must exit.\n");
    exit(EXIT_FAILURE);
  }

  total_pixels = geom_ptr->imageslices * geom_ptr->imagecolumns
    * geom_ptr->imagerows;

  fwrite(geom_ptr->vol_arr, 1, total_pixels, fptr);
  fclose(fptr);
}


/******************************************************************
 * void initialize_empty_slices(geom_info_t * geom)
 ******************************************************************
 * Mallocs space for the slices and sets them all to 0
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure
 ******************************************************************/
void initialize_empty_slices(geom_info_t * geom_ptr) {
  int i, total_pixels;

  total_pixels = geom_ptr->imageslices * geom_ptr->imagecolumns
    * geom_ptr->imagerows;
  
  /* Note that this is freed before exiting */
  geom_ptr->vol_arr = (unsigned char *) malloc(sizeof(unsigned char)
					       * total_pixels);

  if (!(geom_ptr->vol_arr)) {
    fprintf(stderr, "Cannot malloc memory for all slices.  Must exit.\n");
    exit(EXIT_FAILURE);
  }

  geom_ptr->valid.vol_arr = 1; /* malloced but not filled */

  memset((void*)geom_ptr->vol_arr, 0, total_pixels);

  geom_ptr->regionnum[1] = 1;
  geom_ptr->uvval[1] = 1;
  geom_ptr->valid.regionnum[1] = 1;
  geom_ptr->valid.uvval[1] = 1;
  strcpy(geom_ptr->bodyname[1], "'buffer'");
  geom_ptr->valid.bodyname[1] = 1;
}


/******************************************************************
 * free_geom
 ******************************************************************
 * frees memory malloced for all slices
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure
 ******************************************************************/
void free_geom(geom_info_t * geom_ptr) {
  /* Only free if the slices are valid */
  if (geom_ptr->valid.vol_arr) {
    free((void*)geom_ptr->vol_arr);
    geom_ptr->valid.vol_arr = 0;
  }
}

/******************************************************************
 * get_geometry_ptr
 ******************************************************************
 * Returns pointer to the global geom_ptr.  This, of course, 
 * assumes that the global version is being used and has
 * hopefully been initialized.
 */
geom_info_t * get_geometry_ptr(void) {
  return(&ggg);
}

/******************************************************************
 * void get_geometry
 ******************************************************************
 * Gets information from uvh file in order to load a 3D dataset
 *
 * Parameters:
 *  geom_ptr --> a ptr to the geometry structure to fill
 *  name     --> name of the input filename
 ******************************************************************/
void get_geometry(geom_info_t * geom_ptr, char *in_name) {
    FILE *fptr;
    unsigned int total_pixels;
    char c, *tmp;
    char headerfile[256];
    char basename[256];
    char name[256];

    strcpy(name, in_name);
    if (tmp=strstr(name, ".rs")) {
      tmp[0]='\0';
    }
    if (tmp=strstr(name, ".bs")) {
      tmp[0]='\0';
    }
    if (tmp=strstr(name, ".rm")) {
      tmp[0]='\0';
    }
    if (tmp=strstr(name, ".bm")) {
      tmp[0]='\0';
    }
    strcpy(basename, name);

    /* In this case, assume split in two files and maybe gzipped */
    if (tmp=strstr(basename, ".uv")) {
      tmp[0]='\0';
    }
    /* Assert:  We now should have the base name of the file(s) */
    strcpy(headerfile, basename);
    strcat(headerfile, ".uvh");

    if ((fptr = fopen(headerfile, "r"))==NULL) {
      /* In this case, we don't have a header file so assume all is
       * stored in 1 file given by name
       */
      if ((fptr = fopen(name, "r"))!=NULL) {
	strcpy(headerfile, name);
      } else {
	fprintf(stderr, "Could not open specified file %s or %s\nExiting.\n",
		headerfile, name);
	exit(EXIT_FAILURE);
      }
    }

    /* At this point, we read the uncompressed header file */

    fclose(fptr);
    read_uvh(geom_ptr, headerfile);

    printf("PA-range:  [%f, %f)\n", geom_ptr->paaxismin, geom_ptr->paaxismax);
    printf("RL-range:  [%f, %f)\n", geom_ptr->rlaxismin, geom_ptr->rlaxismax);
    printf("IS-range:  [%f, %f)\n", geom_ptr->isaxismin, geom_ptr->isaxismax);

    total_pixels = geom_ptr->imageslices * geom_ptr->imagecolumns
	* geom_ptr->imagerows;

    /* Note that this is (or should be) freed before exiting */
    geom_ptr->vol_arr = (unsigned char *) malloc(sizeof(unsigned char)
						   * total_pixels);
    geom_ptr->valid.vol_arr = 1; /* malloced but not filled... */

    if (!strstr(headerfile, ".uvh")) {
      /* in this case, have a single file with header followed by
       * region labels
       */
      /* At the end of all this, we're picky and expect a single return
       * with no padding before it or after it
       */
      fprintf(stderr, "File format of %s no longer supported.  Must exit.\n",
	      headerfile);
      exit(EXIT_FAILURE);
    } else {
      read_uv(geom_ptr, basename);
    }
}

/******************************************************************
 * make_regions
 ******************************************************************
 * Moves through all of the raw body information data in the
 * geom structure and remaps all values to the actual values that
 * will be sent to the output file
 ******************************************************************/
void make_regions(geom_info_t *geom_ptr) {
  int total_pixels, i;

  /* printf("*********************************\n"); */
  /* printf("*********************************\n"); */
  /* printf("Remapping regions:\n"); */

  total_pixels = geom_ptr->imageslices * geom_ptr->imagecolumns
    * geom_ptr->imagerows;

  for (i=0; i<total_pixels; i++) {
    geom_ptr->vol_arr[i]=get_mapping(geom_ptr, geom_ptr->vol_arr[i]);
  }
}

/******************************************************************
 * cp_to_vox
 ******************************************************************
 * A set of points in wc is passed that are closed to form a
 * polygon and then this region is filled in with the appropriate
 * body type
 * Format:  float pts[x0, y0, x1, y1, x2, y2, ...]
 * Currently, regnum (region number) must be a number between
 * 1 and 7, inclusive (0 represents void, and 8 represents boundary)
 ******************************************************************/
void cp_to_vox(geom_info_t *geom_ptr, int npoints, float *pts,
	       int regnum, char *bodyname, float zval) {
  int i, i2, xi1, xi2, yi1, yi2, xav, yav, top_y_index, left_x_index, 
    bot_y_index, right_x_index, index;
  float x1, x2, y1, y2;
  float xleft, yleft, xmid, ymid, xright, yright;
  float xv1, yv1, xv2, yv2, v1size, v2size, xvect, yvect;
  float top_y, bot_y, left_x, right_x;
  int planeno;
  unsigned char regbit;
  int most=0;
  unsigned char index_most=0;
  static int freq[LIBUV_MAX_REGIONS];
  int num;

  /* printf("*****************************************************\n"); */
  /* printf("For body:  %s at z-val:  %f\n", bodyname, zval); */

  memset((void *)freq, 0, LIBUV_MAX_REGIONS*sizeof(int));

  /* Initialize */
  top_y=pts[1];
  top_y_index=0;
  bot_y=pts[1];
  bot_y_index=0;
  left_x=pts[0];
  left_x_index=0;
  right_x=pts[0];
  right_x_index=0;
  /**************/

  if ((regnum<1)||(regnum>7)) {
    fprintf(stderr, "Region number %d invalid.\n", regnum);
    fprintf(stderr, "Passed region must be between 1 and 7\n");
    exit(EXIT_FAILURE);
  }
  regnum++; /* so region1 is buffer then 2 on are free for whatever */
  regbit=bitregions[regnum-2];

  /* printf("Will initialize region with color %d\n", regbit); */

  /* initialize region if not done yet */
  if (!(geom_ptr->valid.bodyname[regbit*2])) {
    geom_ptr->uvval[regbit*2]=regnum;
    geom_ptr->regionnum[regbit*2]=regbit*2;
    strcpy(geom_ptr->bodyname[regbit*2], bodyname);
    geom_ptr->valid.uvval[regbit*2]=1;
    geom_ptr->valid.regionnum[regbit*2]=1;
    geom_ptr->valid.bodyname[regbit*2]=1;
  }

  regbit=regbit|128; /* also, mark region bit as being a boundary */

  planeno=(zval-(geom_ptr->isaxismin))/geom_ptr->pixelsizeslices;
  if ((planeno<0)||(planeno>=geom_ptr->imageslices)) {
    fprintf(stderr, "Z-value %f invalid.\n", zval);
    exit(EXIT_FAILURE);    
  }

  for (i=0; i<npoints; i++) {
    i2=(i+1)%npoints;
    xi1=2*i;
    yi1=xi1+1;
    xi2=2*i2;
    yi2=xi2+1;
    x1=pts[xi1];
    y1=pts[yi1];
    x2=pts[xi2];
    y2=pts[yi2];
    if (y1>top_y) {
      top_y = y1;
      top_y_index=xi1;
    }
    if (y1<bot_y) {
      bot_y = y1;
      bot_y_index=xi1;
    }
    if (x1>right_x) {
      right_x = x1;
      right_x_index=xi1;
    }
    if (x1<left_x) {
      left_x = x1;
      left_x_index=xi1;
    }
    num=add_line(geom_ptr, regbit, x1, y1, x2, y2, planeno);
    freq[num]=freq[num]+1;
  }

  /* Note:  'index_most' will represent the current value inside the curve
   * that will be changed
   */
  /* don't count values over 128 */
  for (i=0; i<128; i++) {
    if (freq[i]>most) {
      most=freq[i];
      index_most=(unsigned char)i;
    }
    /*    if (freq[i]>0) {
     *      printf("Color %d was found on border %d times.\n", index_most, freq[i]);
     *    }
     */
  }

  /* printf("Color that occurred the most:  %d\n", index_most); */

  regbit=regbit&127; /* chop off uppermost bit */
  
  /* This fills in the region from 4 places in case it closes in on itself
   * and creates an accidental boundary (figure eight, etc.)
   * So, fills in from the following places:
   *   top most point
   *   bottom most point
   *   left most point
   *   right most point
   */
  for (i=0; i<4; i++) {
    switch(i)
      {
      case 0:
	index = top_y_index;
	/* in case y's are in a straight line, get y on the side */
	while (pts[top_y_index+1]<=pts[(index+3)%(2*npoints)]+EPSILON) {
	  index=(index+2)%(2*npoints);
	}
	break;
      case 1:
	index = bot_y_index;
	/* in case y's are in a straight line, get y on the side */
	while (pts[bot_y_index+1]>=pts[(index+3)%(2*npoints)]-EPSILON) {
	  index=(index+2)%(2*npoints);
	}
	break;
      case 2:
	index = right_x_index;
	/* in case x's are in a straight line, get x on top or bottom */
	while (pts[right_x_index]<=pts[(index+2)%(2*npoints)]+EPSILON) {
	  index=(index+2)%(2*npoints);
	}
	break;
      case 3:
	index = left_x_index;
	/* in case x's are in a straight line, get x on top or bottom */
	while (pts[left_x_index]>=pts[(index+2)%(2*npoints)]-EPSILON) {
	  index=(index+2)%(2*npoints);
	}
	break;
      }

    xmid = pts[index];
    ymid = pts[index+1];
    xright = pts[(index+2)%(2*npoints)];
    yright = pts[(index+3)%(2*npoints)];
    xleft  = pts[(index+2*npoints-2)%(2*npoints)];
    yleft  = pts[(index+2*npoints-1)%(2*npoints)];
    
    xv1 = xleft-xmid;
    yv1 = yleft-ymid;
    xv2 = xright-xmid;
    yv2 = yright-ymid;
    v1size = (float)sqrt((double)(xv1*xv1+yv1*yv1));
    v2size = (float)sqrt((double)(xv2*xv2+yv2*yv2));
    xv1/=v1size;
    yv1/=v1size;
    xv2/=v2size;
    yv2/=v2size;
    xvect = xv1+xv2;
    yvect = yv1+yv2;
    v1size = (float)sqrt((double)(xvect*xvect+yvect*yvect));
    xvect/=v1size;
    yvect/=v1size;
    
    switch(i)
      {
      case 0:
	/* just in case we have a vertical line */
	if (yvect>=-EPSILON) {
	  yvect = -1.0;
	  xvect = 0.0;
	}
	break;
      case 1:
	/* just in case we have a vertical line */
	if (yvect<=EPSILON) {
	  yvect = 1.0;
	  xvect = 0.0;
	}
	break;
      case 2:
	/* just in case we have a horizontal line */
	if (xvect>=-EPSILON) {
	  xvect = -1.0;
	  yvect = 0.0;
	}
	break;
      case 3:
	/* just in case we have a horizontal line */
	if (xvect<=EPSILON) {
	  xvect = 1.0;
	  yvect = 0.0;
	}
	break;
      }
    
    fill_region_wvect(geom_ptr, xmid, ymid, xvect, yvect, planeno, regbit, index_most);
  }
}

static void enlarge_bounding_box_simple(geom_info_t * geom_ptr) {
    /* This will add 20 to width, 20 to height, and 4 to imageslices */
    enlarge_bounding_box(geom_ptr, 10, 10, 2);
}

static void enlarge_bounding_box(geom_info_t * geom_ptr, int add_half_x, int add_half_y, int add_half_z) {
    int i, j, k, ii, jj, kk, new_x, new_y, new_z, old_x, old_y, old_z;
    unsigned char * old_ptr;
    unsigned char * new_ptr;

    old_x = geom_ptr->imagecolumns;
    old_y = geom_ptr->imagerows;
    old_z = geom_ptr->imageslices;
    old_ptr = geom_ptr->vol_arr;

    geom_ptr->imagecolumns = new_x = old_x + 2 * add_half_x;
    geom_ptr->imagerows = new_y = old_y + 2 * add_half_y;
    geom_ptr->imageslices = new_z = old_z + 2 * add_half_z;
    geom_ptr->valid.imagecolumns = 1;
    geom_ptr->valid.imagerows = 1;
    geom_ptr->valid.imageslices = 1;
    
    geom_ptr->rlaxismin = geom_ptr->rlaxismin - ((float)add_half_x)*
	geom_ptr->pixelsizecolumns;
    geom_ptr->paaxismin = geom_ptr->paaxismin - ((float)add_half_y)*
	geom_ptr->pixelsizerows;
    geom_ptr->isaxismin = geom_ptr->isaxismin - ((float)add_half_z)*
	geom_ptr->pixelsizeslices;
    geom_ptr->valid.rlaxismin = 1;
    geom_ptr->valid.paaxismin = 1;
    geom_ptr->valid.isaxismin = 1;

    geom_ptr->rlaxismax = geom_ptr->rlaxismax + ((float)add_half_x)*
	geom_ptr->pixelsizecolumns;
    geom_ptr->paaxismax = geom_ptr->paaxismax + ((float)add_half_y)*
	geom_ptr->pixelsizerows;
    geom_ptr->isaxismax = geom_ptr->isaxismax + ((float)add_half_z)*
	geom_ptr->pixelsizeslices;
    geom_ptr->valid.rlaxismax = 1;
    geom_ptr->valid.paaxismax = 1;
    geom_ptr->valid.isaxismax = 1;

    new_ptr = (unsigned char *) malloc(sizeof(unsigned char) * new_x * new_y * new_z);
    /* All new area is labelled 1 */
    memset(new_ptr, 1, new_x * new_y * new_z);
    
    /* Copy old to new */
    for (i=0; i<old_z; i++) {
	ii = i + add_half_z;
	for (j=0; j<old_y; j++) {
	    jj = j + add_half_y;
	    for (k=0; k<old_x; k++) {
		kk = k + add_half_x;
		new_ptr[(ii*new_y+jj)*new_x + kk]
		    = old_ptr[(i*old_y+j)*old_x + k];
	    }
	}
    }

    free((void*)old_ptr);

    geom_ptr->vol_arr = new_ptr;
    geom_ptr->valid.vol_arr = 1; /* Just in case */
}

/* Reads libuvrc resource file to see whether accuracy is on/off and how much
 * the size of the bounding box will be increased
 */
void process_resource_file(geom_info_t * geom_ptr, int * accuracy, float * enlarge) {
  int i;
  char *ResourcePath;
  char name[512], rname[]="libuvrc";
  char contents[512], *cptr;
  FILE *fptr;
  int enlx, enly, enlz;

  /* Defaults if nothing found */
  *accuracy = 1;
  *enlarge = 0.0;

  name[0] = '\0';

  ResourcePath = (char *)getenv("SERA_RESOURCES");
  if ((ResourcePath)&&(strlen(ResourcePath)>0)) {
      strcpy(name, ResourcePath);
  } else {
      fprintf(stderr, "\nSERA_RESOURCES environment variable not set.\n\
Using current directory for file '%s'.\n", rname);
  }

  for (i=strlen(name)-1; i>=0; i--) {
      if (isspace(name[i])||(name[i]=='/')) {
	  name[i] = '\0';
      } else break;
  }

  strcat(name, "/");
  strcat(name, rname);

  /* Now, read the resource file */
  if (!(fptr=fopen(name, "r"))) {
      fprintf(stderr, "Could not open file %s.\n", name);
      fprintf(stderr, "Will create one.\n");
      if (!(fptr=fopen(name, "w"))) {
	  fprintf(stderr, "Could not create resource file %s.\n", name);
	  fprintf(stderr, "Using defaults.\n");
      } else {
	  fprintf(fptr, "accuracy=1\nenlarge=0\n");
	  fclose(fptr);
      }
  } else {
      fread((void*)contents, 1, 512, fptr);
      contents[511] = '\0';
      fclose(fptr);
      
      for (i=0; i<strlen(contents); i++) {
	  contents[i] = tolower(contents[i]);
      }

      if (cptr=strstr(contents, "accuracy")) {
	  if (cptr=strstr(cptr, "=")) {
	      *accuracy = atoi(cptr+1);
	  }
      }

      if (cptr=strstr(contents, "enlarge")) {
	  if (cptr=strstr(cptr, "=")) {
	      *enlarge = (float)atof(cptr+1);
	  }
      }
  }

  if (*accuracy) 
      printf("\nIncreased accuracy is ON.\n");
  else
      printf("\nIncreased accuracy is OFF.\n");
  printf("Each dimension of bounding box will be increased by %f percent.\n",
	 *enlarge);

  enlx = ((float)geom_ptr->imagecolumns)*(*enlarge)/200.0+0.5;
  enly = ((float)geom_ptr->imagerows)*(*enlarge)/200.0+0.5;
  enlz = ((float)geom_ptr->imageslices)*(*enlarge)/200.0+0.5;

  global_accuracy = *accuracy;
  if (enlx<0) enlx = 0;
  if (enly<0) enly = 0;
  if (enlz<0) enlz = 0;

  /* Pad the bounding box by specified amount */
  if ((enlx>0)||(enly>0)||(enlz>0))
      enlarge_bounding_box(geom_ptr, enlx, enly, enlz);
}

/******************************************************************
 * IRE_NAME
 ******************************************************************
 * init_ray_environ will be called once from the rtt_MC wrapper to
 * set up for the ray tracer.
 *
 ******************************************************************/

void IRE_NAME (subdiv_tree_depth, t_eps_in,
	       f_name, exclude_body_list,\
	       f_length1, f_length2)
int    *subdiv_tree_depth;
double *t_eps_in;
char   f_name[INEL_BUFF_SIZE];  /* file name of surface object file */
char   exclude_body_list[INEL_BUFF_SIZE];
int    f_length1, f_length2;    /* by value when using UNIX f77 */
{
  char *tmp,c;
  int i,j,ok = 1;
  char input_name[INEL_BUFF_SIZE];
  int accuracy;
  float enlarge;


  /** Exclude body list unused, warn if not empty **/
  if (f_length2 != 0)
      {
	for (j=0;j<(f_length2+1);j++)
	    {
		c = exclude_body_list[j];
		if (isprint(c) && !isspace(c) && (c != '\0'))
		    {
		      /* Found printable non-whitespace chars
		       * so the string isn't just blank.  We
		       * found non-whitespace characters but
		       * do nothing with them so indicate
		       * warning
		       */
			ok = 0;
		    }
		else if (c == '\0')
		    {
			/*** its ok we hit the end of string ***/
			break;
		    }
		if (!ok) break;
	     }
	
	if (!ok){
	    fprintf(stderr,
	     "NOTE! The exclude body list is ignored in this version.\n");
	     }
      }

  /* given f_name and f_length1 need to create the input file name */
  strncpy( input_name, f_name, f_length1 );
  input_name[f_length1]='\0';

  if (tmp=strchr(input_name, ' '))
    tmp[0] = '\0';

  /* Read the info in the .uv and .uvh file into geom structure */
  geom_ptr=&ggg;

  if (strstr(input_name, ".rs")) {
#ifdef NO_NURBS
    printf("Sorry, nurbs not compiled into this version.  Use uv files only.\n");
    exit(EXIT_FAILURE);
#else
    process_files_for_univels(input_name);
#endif
  }

  get_geometry(geom_ptr, input_name);

  /* In this case, want 'body 0' to be 'VOID' */
  sprintf(geom_ptr->bodyname[0], "VOID");
  geom_ptr->valid.bodyname[0] = 1;
  geom_ptr->valid.regionnum[0] = 1;
  geom_ptr->valid.uvval[0] = 1;
  /* End */

  /* Keep track of these in globals so they can be used quickly */
  garr = geom_ptr->vol_arr;
  gwidth = geom_ptr->imagecolumns;
  gheight = geom_ptr->imagerows;
  gnumslices = geom_ptr->imageslices;

  printf("\n** Resource file not being used **\n");
  /* process_resource_file(geom_ptr, &accuracy, &enlarge); */

  printf("\n\n*******************************************************\n");
  printf("* You are using LIBUV VA2.0 for fast tracking and     *\n");
  printf("* computation of intersections.                       *\n");
  printf("*******************************************************\n");
  printf("* Version 'A' is currently the fastest method         *\n");
  printf("* although 'short' intersections may be ignored.      *\n");
  printf("* (Typically, it is desirable to ignore such          *\n");
  printf("*  intersections anyway.)                             *\n");
  printf("*******************************************************\n");
  printf("* Version 2.0 has the following changes:              *\n");
  printf("*   * No longer need univel space to have 'buffer' at *\n");
  printf("*     all extremes of bounding box.                   *\n");
  printf("*   * Better handling of roundoff error and lost      *\n");
  printf("*     particles.                                      *\n");
  printf("*******************************************************\n\n");
}


/* This is a routine that can be called by interrogate_geometry
 * to print the initial material, next material, and distance
 * from initial material to final (unless there's a miss)
 */
static void print_tracking_info(geom_info_t * geom_ptr,
			 int initial, int final, int miss,
			 double dist,
			 float wcf_x0, float wcf_y0, float wcf_z0,
			 float wcf_x,  float wcf_y,  float wcf_z) {
    printf("Start %0.2f %0.2f %0.2f:  %s\n", wcf_x0, wcf_y0, wcf_z0,
	   geom_ptr->bodyname[geom_ptr->uvval[initial]]);
    if (!miss) {
	printf("End   %0.2f %0.2f %0.2f:  %s\n", wcf_x, wcf_y, wcf_z,
	       geom_ptr->bodyname[geom_ptr->uvval[final]]);
	printf("  Distance:  %0.4f\n", (float)dist);
    } else {
	printf("End   EXIT TO BUFFER.\n");
    }
}


/* #define transform_to_use_x(...)
 * This macro serves to convert the ray tracking problem into new
 * coordinates where vx varies the most.  Then, after tracking in
 * these transformed coordinates, we apply an inverse transform to
 * see where we really ended up.
 *
 * Done as macro because we
 *   (1) want speed rather than overhead of function call
 *   (2) have very symmetric (and long) code based on which of
 *       (vx, vy, or vz) varies the most
 *
 * Many of the comments below pertain to x varying the most.
 * However, they could also be meant to refer to y or z depending
 * on the order of the parameters passed to the macro.
 *
 * Note variables used that aren't passed:
 *   fractmp
 *   fractional1
 *   fractional2
 *   bot_slopef
 */
#define transform_to_use_x(xx, yy, zz, \
                         dx, dy, dz, \
  			 ndcf_first_x, ndcf_first_y, ndcf_first_z, \
			 ndcf_vx, ndcf_vy, ndcf_vz, \
			 ndcf_inv_vx, ndcf_inv_vy, ndcf_inv_vz); \
      xx = int_rnd_down(ndcf_first_x); \
      /* want to be sure xx+0.5 is between 0 and 1 steps _behind_ \
       * ndcf_first_x (behind based on the direction of increment) \
       * (we do this because that way we can increment initially by \
       * 1 step blindly to get to the first element we need to check) \
       */ \
      if (((float)xx+0.5>ndcf_first_x)&&(ndcf_vx>0.0)) { \
	xx--; \
      } else if (((float)xx+0.5<ndcf_first_x)&&(ndcf_vx<0.0)) { \
	xx++; \
      } \
      /* This is the amount of 'time' to get from the given starting \
       * point to the starting point modified to the middle in the \
       * x direction -- Note:  (float)xx+0.5 is center of start
       * element in x \
       */ \
      fractmp = ((float)xx+0.5-ndcf_first_x)*ndcf_inv_vx; \
      ndcf_y = ndcf_first_y+fractmp*ndcf_vy; /* float value of y on line \
					      * given the value of xx+0.5 \
					      */ \
      ndcf_z = ndcf_first_z+fractmp*ndcf_vz; /* float value of z ... */ \
      yy = int_rnd_down(ndcf_y); \
      zz = int_rnd_down(ndcf_z); \
\
      /* fractional1 represents the fractional position of y and yy \
       * represents the integral position \
       */ \
      if (ndcf_vy>0.0) \
	fractional1 = ndcf_y - (float)yy; \
      else \
	fractional1 = 1.0 - ndcf_y + (float)yy; \
\
      /* fractional2 represents the fractional position of z and zz \
       * represents the integral position \
       */ \
      if (ndcf_vz>0.0) \
	fractional2 = ndcf_z - (float)zz; \
      else \
	fractional2 = 1.0 - ndcf_z + (float)zz; \
      fractmp = bot_slopef*(float)fabs((double)ndcf_inv_vx); \
      if (ndcf_vx>0.0) \
	dx = bot_slopef; \
      else \
	dx = -bot_slopef; \
      dy = int_rnd(ndcf_vy*fractmp); \
      dz = int_rnd(ndcf_vz*fractmp);
/* end #define transform_to_use_x */


#ifdef LIBUV
/*************************************************************************
 * IG_NAME (INTERROGATE_GEOMETRY)
 *
 * Called by RAFFLE through subroutine DTB. This is the basic interface 
 * to spline surfaces. The starting point (pt) and direction
 * cosines(dir), which must be NORMALIZED, are passed to the interface via
 * the "wrapper". The "hit" variable contains the intersection point if there
 * is one (returns TRUE).
 * 
 * INPUT VALUES
 *    fray_pt -> starting point (x,y,z)
 *    fray_dir -> direction cosines
 *
 * OUTPUT VALUES
 *    dist -> distance to boundary  !! (currently in NORMALIZED units) !!
 *    this ->  material number or value of current region
 *    next ->  material number of next region
 *    miss ->  if no intersection is found miss_flag > 0
 **************************************************************************/
void IG_NAME ( double *fray_pt, double *fray_dir, double *dist,
	       int *this, int *next, int *miss )
{
    /* global variable:  geom_ptr */
    double ndcf_abs_vx, ndcf_abs_vy, ndcf_abs_vz;
    int last_material, material, next_material;
    float t[4], fractional1, fractional2, fractmp,
	wcf_x0,        wcf_y0,        wcf_z0,
	wcf_x,         wcf_y,         wcf_z,
	ndcf_x0,       ndcf_y0,       ndcf_z0,
	ndcf_first_x,  ndcf_first_y,  ndcf_first_z,
	ndcf_x,        ndcf_y,        ndcf_z,
	wcf_vx,        wcf_vy,        wcf_vz,
	ndcf_vx,       ndcf_vy,       ndcf_vz,
	ndcf_inv_vx,   ndcf_inv_vy,   ndcf_inv_vz;
    int i, si, varies_most, success,
	ndci_end_x,    ndci_end_y,    ndci_end_z,
	xx,            yy,            zz,
	dx,            dy,            dz,
	step_xx,       step_yy,       step_zz,
	d1, d2,
	absdy, absdz, /* absdx #defined above */
	incrE1, incrNE1, incrE2, incrNE2;
    
    /* assume a miss unless proven otherwise */
    *miss = 1;
    
    wcf_x = wcf_x0 = (float) fray_pt[0];
    wcf_y = wcf_y0 = (float) fray_pt[1];
    wcf_z = wcf_z0 = (float) fray_pt[2];
    
    wcf_vx = (float) fray_dir[0];
    wcf_vy = (float) fray_dir[1];
    wcf_vz = (float) fray_dir[2];
    
    /* get pixel coords in floats -- initialization */
    wcf_to_ndcf(geom_ptr, wcf_x0, wcf_y0, wcf_z0,
		&ndcf_x0, &ndcf_y0, &ndcf_z0);

    material = regionnum_lookup(geom_ptr, int_rnd_down(ndcf_x0),
			        int_rnd_down(ndcf_y0), int_rnd_down(ndcf_z0));

    ndcf_vx = wcf_vx*geom_ptr->inv_pixelsizecolumns;
    ndcf_vy = wcf_vy*geom_ptr->inv_pixelsizerows;
    ndcf_vz = wcf_vz*geom_ptr->inv_pixelsizeslices;
    if (ndcf_vx!=0.0)
	ndcf_inv_vx = 1.0/ndcf_vx;
    else
	ndcf_inv_vx = -INFINITY;
    if (ndcf_vy!=0.0)
	ndcf_inv_vy = 1.0/ndcf_vy;
    else
	ndcf_inv_vy = -INFINITY;
    if (ndcf_vz!=0.0)
	ndcf_inv_vz = 1.0/ndcf_vz;
    else
	ndcf_inv_vz = -INFINITY;
    
    /* If the material is BOUNDS_ERROR, we start _outside_ the box
     * Options are to:
     *  (1) Continue along ray until we enter the box, then start
     *      tracking through box.
     *  (2) Give up and say we never hit the box if no intersection
     *      exists (return and indicate a miss).
     * Additionally, if we do intersect the box, if we hit 'buffer'
     * then we continue along our merry way _but_ if we hit
     * non-buffer then we must report this intersection point
     * immediately.  (Outside of box and 'buffer' are to be treated
     * as being the same void material type so no intersections
     * occur on these types of boundaries.)
     *
     * Convention:
     *  (ndcf_x0, ndcf_y0, ndcf_zo) --> where we actually start
     *  (ndcf_x,  ndcf_y,  ndcf_z ) --> first point on ray inside
     *                                  of bounding box
     */
    if (material != BOUNDS_ERROR) {
	/* We start inside the box */
	ndcf_first_x = ndcf_x0;
	ndcf_first_y = ndcf_y0;
	ndcf_first_z = ndcf_z0;
    } else {
        /* Need to indicate that the material is really buffer rather than
         * BOUNDS_ERROR
	 */
        material=BUFFER_MATERIAL;

	/* Use t[3] (times) to find when we intersect an x-plane,
	 * y-plane, and z-plane of the box.  We consider these times
	 * in order from lowest to highest and start at the point
	 * that first puts us in the box -- if it exists.  No times
	 * less than 0 are considered as this would require us to
	 * move 'backwards' from our starting point.
	 */
	
	/* Choose to take the right/left, top/bottom, front/back 
	 * faces of the box.
	 */
	if (ndcf_x0<0.0) ndcf_first_x = 0.0;
	else ndcf_first_x = (float)(gwidth);
	
	if (ndcf_y0<0.0) ndcf_first_y = 0.0;
	else ndcf_first_y = (float)(gheight);
	
	if (ndcf_z0<0.0) ndcf_first_z = 0.0;
	else ndcf_first_z = (float)(gnumslices);
	
	/* Note:  Any inverses that did not exist were stored as
	 * extremely negative numbers that will result in negative
	 * times that won't be considered.  The EPSILON puts us
	 * just inside the box rather than on an edge.
	 */
	t[0] = (ndcf_first_x-ndcf_x0)*ndcf_inv_vx + EPSILON;
	t[1] = (ndcf_first_y-ndcf_y0)*ndcf_inv_vy + EPSILON;
	t[2] = (ndcf_first_z-ndcf_z0)*ndcf_inv_vz + EPSILON;
	
	/* Sort times from lowest to highest */
	float_sort_3(t);
	
	/* Try times in order and stop if enter bounding box */
	i = 0;
        next_material = BOUNDS_ERROR;
	do {
	    if (t[i]>0.0) {
		ndcf_first_x = ndcf_x0+t[i]*ndcf_vx;
		ndcf_first_y = ndcf_y0+t[i]*ndcf_vy;
		ndcf_first_z = ndcf_z0+t[i]*ndcf_vz;
		next_material = regionnum_lookup(geom_ptr,
						 int_rnd_down(ndcf_first_x),
						 int_rnd_down(ndcf_first_y),
						 int_rnd_down(ndcf_first_z));
	    }
	    i++;
	} while ((next_material==BOUNDS_ERROR)&&(i<3));
	
	if (next_material==BOUNDS_ERROR) {	    
	    /* Started outside bounding box and ray does not even
	     * intersect bounding box.  Simply return and indicate
	     * a miss -- ray starts and ends in buffer.
	     */
	    *this = BUFFER_MATERIAL;
            *next = BUFFER_MATERIAL;
#ifdef PRINTS
	    print_tracking_info(geom_ptr, *this, *next, *miss, *dist,
				wcf_x0, wcf_y0, wcf_z0,
				wcf_x,  wcf_y,  wcf_z);
#endif
    
	    return;
	} else {
	    /* Found point where ray enters the bounding box.
	     * next_material is the first thing that is hit -- which
	     * is set and is anything other than BOUNDS_ERROR
	     */

	    ndcf_to_wcf(geom_ptr, ndcf_first_x, ndcf_first_y, ndcf_first_z, &wcf_x, &wcf_y, &wcf_z);
	
	    /* If we don't hit buffer immediately upon intersecting the
	     * bounding box then we must indicate an intersection
	     * with whatever material we do hit at this point.
	     * (This is a consequence of the bounding box not
	     * necessarily being padded by 'buffer' on all sides.)
	     */
	     if (next_material!=BUFFER_MATERIAL) {
	        *this = material;
	        *next = next_material;
	        *miss = 0;
	        ndcf_to_wcf(geom_ptr, ndcf_first_x, ndcf_first_y,
			    ndcf_first_z, &wcf_x, &wcf_y, &wcf_z);

	        *dist = (double)(sqrt((double)
				      ((wcf_x-wcf_x0)*(wcf_x-wcf_x0)
				       +(wcf_y-wcf_y0)*(wcf_y-wcf_y0)
				       +(wcf_z-wcf_z0)*(wcf_z-wcf_z0))));
#ifdef PRINTS
	        print_tracking_info(geom_ptr, *this, *next, *miss, *dist,
				    wcf_x0, wcf_y0, wcf_z0,
				    wcf_x,  wcf_y,  wcf_z);
#endif
	        return;
	    }
	}
    }
    
    /* At this point, either started in bounding box or found where
     * ray first enters bounding box and immediately hit 'buffer'.
     */
    *this = material;

    ndcf_abs_vx=fabs((double)ndcf_vx);
    ndcf_abs_vy=fabs((double)ndcf_vy);
    ndcf_abs_vz=fabs((double)ndcf_vz);
    
    /* Set varies_most to:
     * Here, x=ndcf_abs_vx, y=ndcf_abs_vy, z=ndcf_abs_vz
     * 1,2 -> x>=z>=y or x>=y>=z   (note:  1 and 2 are treated the same)
     * 3,4 -> y>=x>=z or y>=z>=x
     * 5,6 -> z>=x>=y or z>=y>=x
     */
    if (ndcf_abs_vx>=ndcf_abs_vy)
	if (ndcf_abs_vx>=ndcf_abs_vz)
	    varies_most=1;
	else
	    varies_most=5;
    else if (ndcf_abs_vy>=ndcf_abs_vz) 
	varies_most=3;
    else 
	varies_most=5;

    /* Transform the problem into one where some 'standard' direction
     * varies the most.  This is done rather than have 3 versions
     * of the speedy algorithm for each of x, y, or z varying the
     * most.
     */
    switch(varies_most) 
	{
	case 1:
	case 2:
	    /* Here, vx varies most */
	    transform_to_use_x(xx, yy, zz,
			       dx, dy, dz,
			       ndcf_first_x, ndcf_first_y, ndcf_first_z,
			       ndcf_vx, ndcf_vy, ndcf_vz,
			       ndcf_inv_vx, ndcf_inv_vy, ndcf_inv_vz);
	    break;
	case 3:
	case 4:
	    /* Here, vy varies most */
	    transform_to_use_x(xx, yy, zz,
			       dx, dy, dz,
			       ndcf_first_y, ndcf_first_x, ndcf_first_z,
			       ndcf_vy, ndcf_vx, ndcf_vz,
			       ndcf_inv_vy, ndcf_inv_vx, ndcf_inv_vz);
	    
	    break;
	case 5:
	case 6:
	    /* Here, vz varies most */
	    transform_to_use_x(xx, yy, zz,
			       dx, dy, dz,
			       ndcf_first_z, ndcf_first_y, ndcf_first_x,
			       ndcf_vz, ndcf_vy, ndcf_vx,
			       ndcf_inv_vz, ndcf_inv_vy, ndcf_inv_vx);
	    break;
	default:
	    fprintf(stderr, "Error!  Error!\n");
	}
    
    /* sets step direction to -1, 1, or 0 */
    if (dx<0) { step_xx = -1; }
    else if (dx>0) { step_xx = 1; }
    else { step_xx = 0; }  /* --> shouldn't ever occur */
    
    if (dy<0) { step_yy = -1; absdy = -dy; }
    else if (dy>0) { step_yy = 1; absdy = dy; }
    else { step_yy = 0; absdy = 0; }
    
    if (dz<0) { step_zz = -1; absdz = -dz; }
    else if (dz>0) { step_zz = 1; absdz = dz; }
    else { step_zz = 0; absdz = 0; }
    
    incrE1 = absdy;
    incrNE1 = absdy-absdx;
    incrE2 = absdz;
    incrNE2 = absdz-absdx;
    /* d1 and d2 do three things:
     * (1) move up the fractional amount fractionalX
     * (2) account for x moving to right one
     * (3) shift by -bot_slopef so we stay in range
     *     -bot_slopef to 0 (rather than 0 to bot_slopef)
     */
    d1 = int_rnd(fractional1*bot_slopef+incrE1)-absdx;
    d2 = int_rnd(fractional2*bot_slopef+incrE2)-absdx;
    
    last_material = material; /* Need to keep track of when it changes */
    
    do {
	if (d1 < 0) {
	    d1+=incrE1;
	} else {
	    d1+=incrNE1;
	    yy+=step_yy;
	}
	if (d2 < 0) {
	    d2+=incrE2;
	} else {
	    d2+=incrNE2;
	    zz+=step_zz;
	}
	xx+=step_xx;
	
	switch(varies_most)
	    {
	    case 1:
	    case 2:
		material = regionnum_lookup(geom_ptr, xx, yy, zz);
		break;
	    case 3:
	    case 4:
		material = regionnum_lookup(geom_ptr, yy, xx, zz);
		break;
	    case 5:
	    case 6:
		material = regionnum_lookup(geom_ptr, zz, yy, xx);
		break;
	    default:
		fprintf(stderr, "Error!  Error!\n");
	    }
	
	/* Keep moving voxel by voxel UNTIL we
	 * (1) hit a different material _or_
	 * (2) exit our bounding box entirely
	 *     (this will yield a 0 material #)
	 */
    } while (material==last_material);
    next_material = material;
#ifdef INTERSECT_CODE
    total_intersections++; /* global variable to keep track of */
#endif
    /* If material is BOUNDS_ERROR, we've exited the box.  'buffer' to box exit
     * is simply reported as a miss.  If we're in any other material
     * when we exit the box, we must report at which point we exit
     * the box and say the next material is 'buffer'.  First, make sure
     * we change mark of BOUNDS_ERROR to BUFFER_MATERIAL if necessary.
     */
    if (material == BOUNDS_ERROR)
        material = BUFFER_MATERIAL;
    if ((material==BUFFER_MATERIAL)&&(last_material==BUFFER_MATERIAL)) {
        *next = BUFFER_MATERIAL;
#ifdef PRINTS
	print_tracking_info(geom_ptr, *this, *next, *miss, *dist,
			    wcf_x0, wcf_y0, wcf_z0,
			    wcf_x,  wcf_y,  wcf_z);
#endif
	return;
    } else { /* not both were buffer */
	/* Now that we have array coordinates for our intersection point,
	 * we need to perform an inverse tranform the real x, y, and z.
	 * (Recall, we did a transform so we'd only have to write
	 * the version of the fast tracking algorithm that applies
	 * to the case of x varying most.)
	 */
	switch(varies_most)
	    {
	    case 1:
	    case 2:
		ndci_end_x = xx;  ndci_end_y = yy;  ndci_end_z = zz;
		/* x varies most, use time to center of x in case
		 * we later wish to find the corresponding exact
		 * y and z values.
		 */
		t[3] = (((float)ndci_end_x)+0.5-ndcf_x0)*ndcf_inv_vx;
		break;
	    case 3:
	    case 4:
		ndci_end_x = yy;  ndci_end_y = xx;  ndci_end_z = zz;
		/* y varies most, ... */
		t[3] = (((float)ndci_end_y)+0.5-ndcf_y0)*ndcf_inv_vy;
		break;
	    case 5:
	    case 6:
		ndci_end_x = zz;  ndci_end_y = yy;  ndci_end_z = xx;
		/* z varies most, ... */
		t[3] = (((float)ndci_end_z)+0.5-ndcf_z0)*ndcf_inv_vz;
		break;
	    }
	
	/* Now, need to convert from the pixel geometry (a big array)
	 * back into world coordinates.  Basically, we need to see
	 * exactly where we entered the voxel with the new material.
	 * We either entered through a plane parallel to the x-z,
	 * y-z, or x-y plane.  Because of roundoff, t[3] is also
	 * considered as a point near the center of the voxel of
	 * interest rather than an edge.  (It corresponds exactly
	 * to one of the points we used while sampling along the
	 * ray.  It's in the center of the voxel based on the
	 * direction that varies the most.)
	 */
	if (global_accuracy) {
	    /* need to move to right or left of voxel depending
	     * on direction of motion
	     */
	    ndcf_x = find_voxel_edge(ndci_end_x, ndcf_vx);
	    ndcf_y = find_voxel_edge(ndci_end_y, ndcf_vy);
	    ndcf_z = find_voxel_edge(ndci_end_z, ndcf_vz);
	    
	    /* Find corresponding "times" it would take to move
	     * to those points.  (t[3] already set above)
	     */
	    t[0] = (ndcf_x-ndcf_x0)*ndcf_inv_vx+EPSILON;
	    t[1] = (ndcf_y-ndcf_y0)*ndcf_inv_vy+EPSILON;
	    t[2] = (ndcf_z-ndcf_z0)*ndcf_inv_vz+EPSILON;
	    
	    /* Sort times so we can try lowest first */
	    float_sort_4(t);
	    si = 0;
	} else { /* if not global accuracy */
	    si = 3;
	}
	
	/* Now, try up to 4 times in order and see if any time > 0
	 * puts us inside the desired new material type.  (It
	 * should unless there's a roundoff problem.)
	 */
	success=0;
	for (i=si; i<4; i++) {
	    if (t[i]>=0.0) {
		ndcf_x = ndcf_x0+t[i]*ndcf_vx;
		ndcf_y = ndcf_y0+t[i]*ndcf_vy;
		ndcf_z = ndcf_z0+t[i]*ndcf_vz;
		if (next_material==
		    regionnum_lookup(geom_ptr,
					       int_rnd_down(ndcf_x),
					       int_rnd_down(ndcf_y),
					       int_rnd_down(ndcf_z))) {
		    success=1;
		    /* Note:  It's possible we went from 'buffer' to
		     *        outside of bounding box.  We'll need
		     *        to handle this later
		     */
		    break;
		}
	    }
	}
	
	if (!success) {
	    double dist1, dist2, t_elapsed;
	    double new_start_vec[3];
	    int no_change = 1;
	    int original_this;
	    /* If still not success, had a rounding problem so:
	     * Make a recursive call to self and let the ray
	     * simply continue.
	     */

	    fprintf(stderr, "*** Fixing apparent round-off error. ***\n");

	    t_elapsed = t[3];
	    for (i=si; i<3; i++) {
		if (t[i]>=0.0) {
		    t_elapsed = (double)t[i];
		    break;
		}
	    }
	    /* Need to be assured that t>0.0 else not getting anywhere */
	    if (t_elapsed<=0.0) {
		fprintf(stderr, "  SOMEHOW, TIMES INDICATE RAY MUST GO IN REVERSE!\n");
		fprintf(stderr, "  Will indicate a miss and return...\n");
		/* t_elapsed=EPSILON; */ /* alternate way to try to
		 * solve the problem:  give the ray a small push */	       
#ifdef PRINTS
		print_tracking_info(geom_ptr, *this, *next, *miss, *dist,
				    wcf_x0, wcf_y0, wcf_z0,
				    wcf_x,  wcf_y,  wcf_z);
#endif
		return;
	    }

	    /* Create the new starting vector.  At same time,
	     * make sure that the new point isn't the same as the
	     * old point.  (could happen if fray_dir = 0.0 0.0 0.0
	     * _or_ EPSILON is too tiny)
	     */
	    for (i=0; i<3; i++) {
		new_start_vec[i] = fray_pt[i] + t_elapsed*fray_dir[i];
		if (fray_pt[i]!=new_start_vec[i]) {
		    no_change = 0;
		}
	    }
	    if (no_change) {
		/* just indicate a miss and return */
		fprintf(stderr, "  Bypassed an infinite loop (that should not \
occur) by indicating a miss.\n");
		fprintf(stderr, "  Suspect EPSILON is too tiny or passed direction is not a unit vector.\n");
#ifdef PRINTS
		print_tracking_info(geom_ptr, *this, *next, *miss, *dist,
				    wcf_x0, wcf_y0, wcf_z0,
				    wcf_x,  wcf_y,  wcf_z);
#endif
		return;
	    }
	    dist1 = (double)sqrt((new_start_vec[0]-fray_pt[0])*
				 (new_start_vec[0]-fray_pt[0])+
				 (new_start_vec[1]-fray_pt[1])*
				 (new_start_vec[1]-fray_pt[1])+
				 (new_start_vec[2]-fray_pt[2])*
				 (new_start_vec[2]-fray_pt[2]));
	    original_this = *this;
#ifdef PRINTS
	    printf("  dist = %0.4f -- RECURSE from %0.2f %0.2f %0.2f\n",
		   (float)dist1,
		   (float)new_start_vec[0],
		   (float)new_start_vec[1],
		   (float)new_start_vec[2]);
#endif
	    IG_NAME( new_start_vec, fray_dir, &dist2,
		     this, next, miss );
	    if (!(*miss)) {
		*dist = dist1 + dist2;
#ifdef PRINTS
		printf("  Recursive return, total distance %0.4f.\n",
		       (float)(*dist));
#endif
	    } else {
#ifdef PRINTS
		printf("  Made recursive call and missed.\n");
#endif
	    }
	    if (original_this!=*this) {
		*this = original_this;
		fprintf(stderr, "  Inconclusive starting material.\n");
	    }
	    return;
	}
	ndcf_to_wcf(geom_ptr, ndcf_x, ndcf_y, ndcf_z, &wcf_x, &wcf_y, &wcf_z);
    }

    *next = material;
    *miss = 0;
    /* EPSILON added above causes distance to put us just inside
     * the edge of the voxel of interest; landing on an edge
     * exactly can be ambiguous so we don't do it.
     */
    *dist = (double)sqrt((double)((wcf_x-wcf_x0)*(wcf_x-wcf_x0)
				  +(wcf_y-wcf_y0)*(wcf_y-wcf_y0)
				  +(wcf_z-wcf_z0)*(wcf_z-wcf_z0)));
#ifdef PRINTS
    print_tracking_info(geom_ptr, *this, *next, *miss, *dist,
			wcf_x0, wcf_y0, wcf_z0,
			wcf_x,  wcf_y,  wcf_z);
#endif
}
#endif



#ifdef LIBUV50
/*************************************************************************
 * IG_NAME (INTERROGATE_GEOMETRY)
 *
 * Called by RAFFLE through subroutine DTB. This is the basic interface 
 * to spline surfaces. The starting point (pt) and direction
 * cosines(dir), which must be NORMALIZED, are passed to the interface via
 * the "wrapper". The "hit" variable contains the intersection point if there
 * is one (returns TRUE).
 * 
 * INPUT VALUES
 *    fray_pt -> starting point (x,y,z) 
 *    fray_dir -> direction cosines
 *
 * OUTPUT VALUES
 *    dist -> distance to boundary  !! (currently in NORMALIZED units) !!
 *    this ->  material number or value of current region
 *    next ->  material number of next region
 *    miss ->  if no intersection is found miss_flag > 0
 **************************************************************************/
void IG_NAME ( double *fray_pt, double *fray_dir, double *dist,
	       int *this, int *next, int *miss )
{
  float wcf_x0, wcf_y0, wcf_z0,
    wcf_vx, wcf_vy, wcf_vz;
  /* Also, in this case, geom_ptr is not passed -- use global variable geom_ptr */
  float wcf_x, wcf_y, wcf_z;
  float ndcf_x, ndcf_y, ndcf_z;
  float ndcf_x0, ndcf_y0, ndcf_z0, ndcf_first_x, ndcf_first_y, ndcf_first_z;
  float pfdx, pfdy, pfdz;
  float ndcf_vx, ndcf_vy, ndcf_vz, ndcf_inv_vx, ndcf_inv_vy, ndcf_inv_vz;
  float t[3];
  /*long*/ int eyx, eyz, ezx, incrx, incry, incrz;
  static /*long*/ int bignum = 1073741824; /* 2^30 */
  int x, y, z, saved_x, saved_y, saved_z;
  float xedge, yedge, zedge;
  int sgndx, sgndy, sgndz, possibly_done;
  float diffx, diffy, diffz;
  int which_to_increment, last_incremented, valid_position, i;
  float time;
  region_t material, last_material;
  int success;
  int is_even;

  *miss = 1; /* assume a miss unless proven otherwise */

  wcf_x = wcf_x0 = (float) fray_pt[0];
  wcf_y = wcf_y0 = (float) fray_pt[1];
  wcf_z = wcf_z0 = (float) fray_pt[2];

  wcf_vx = (float) fray_dir[0];
  wcf_vy = (float) fray_dir[1];
  wcf_vz = (float) fray_dir[2];

  /* get pixel coords in floats -- initialization */
  wcf_to_ndcf(geom_ptr, wcf_x0, wcf_y0, wcf_z0, &ndcf_x0, &ndcf_y0, &ndcf_z0);
  /*round_to_bignum(&ndcf_x0, &ndcf_y0, &ndcf_z0);*/
  
  material = geom_array_lookup(geom_ptr, int_rnd_down(ndcf_x0), int_rnd_down(ndcf_y0), int_rnd_down(ndcf_z0));
  
  ndcf_vx = wcf_vx*geom_ptr->inv_pixelsizecolumns;
  ndcf_vy = wcf_vy*geom_ptr->inv_pixelsizerows;
  ndcf_vz = wcf_vz*geom_ptr->inv_pixelsizeslices;
  
  /* A vector to step along in array dimensions.  Each component should be
   * between -1 and 1.  Call function to place all within the bounds.
   */
  place_in_bounds(&ndcf_vx, &ndcf_vy, &ndcf_vz);
  /*round_to_bignum(&ndcf_vx, &ndcf_vy, &ndcf_vz);*/

  /* Print where we initially start */
#ifdef PRINTS
  printf("(%0.2f, %0.2f, %0.2f) (d=%0.2f) contains regions:",
	 wcf_x, wcf_y, wcf_z,
	 sqrt((double)((wcf_x-wcf_x0)*(wcf_x-wcf_x0)+(wcf_y-wcf_y0)*(wcf_y-wcf_y0)+(wcf_z-wcf_z0)*(wcf_z-wcf_z0))));
  printf("  %s\n", geom_ptr->bodyname[material]);
#endif

  /* If the material is 0, we are outside the box
   * Didn't start in bounding box -- try to move into bounding box but
   * we'll just return if we can't intersect it -----
   * (ndcf_x0, ndcf_y0, ndcf_z0) will be modified as necessary
   */
  if (material!=0) {
    /* We start inside the box */
    ndcf_first_x = ndcf_x0;
    ndcf_first_y = ndcf_y0;
    ndcf_first_z = ndcf_z0;
  } else { /* material==0 */
    /* We start outside the box -- try to move inside */
    /* use float t[3]:  time to (ndcf_x0, ?, ?), (?, ndcf_y0, ?),
     * and (?, ?, ndcf_z0) respectively
     * --> must be >= 0 else moving wrong way
     */

    /* Maybe only need these if outside of box... */
    /* Yes -- changed on 6-25-97 */
    if (ndcf_vx!=0.0)
      ndcf_inv_vx = 1.0/ndcf_vx;
    else
      ndcf_inv_vx = -INFINITY;
    if (ndcf_vy!=0.0)
      ndcf_inv_vy = 1.0/ndcf_vy;
    else
      ndcf_inv_vy = -INFINITY;
    if (ndcf_vz!=0.0)
      ndcf_inv_vz = 1.0/ndcf_vz;
    else
      ndcf_inv_vz = -INFINITY;
    
    if (ndcf_x0<0.0)
      ndcf_first_x = 0.0;
    else
      ndcf_first_x = (float)(gwidth);

    if (ndcf_y0<0.0)
      ndcf_first_y = 0.0;
    else
      ndcf_first_y = (float)(gheight);

    if (ndcf_z0<0.0)
      ndcf_first_z = 0.0;
    else
      ndcf_first_z = (float)(gnumslices);

    /* don't divide by 0 */
    t[0] = (ndcf_first_x-ndcf_x0)*ndcf_inv_vx + EPSILON;
    t[1] = (ndcf_first_y-ndcf_y0)*ndcf_inv_vy + EPSILON;
    t[2] = (ndcf_first_z-ndcf_z0)*ndcf_inv_vz + EPSILON;

    /* printf("The t's are:  %f %f %f\n", t[0], t[1], t[2]); */

    /* swap to order t[0], t[1], t[2] */
    float_sort_3(t);

    /* Assert t[0] < t[1] < t[2] */

    /* Now, loop until first positive t that gives all 3 x,y,z in range */
    /* If none give all three in range, don't intersect box */
    i = 0;
    do {
      if (t[i]>0.0) {
	ndcf_first_x = ndcf_x0+t[i]*ndcf_vx;
	ndcf_first_y = ndcf_y0+t[i]*ndcf_vy;
	ndcf_first_z = ndcf_z0+t[i]*ndcf_vz;
	last_material = geom_array_lookup(geom_ptr,
					  int_rnd_down(ndcf_first_x),
					  int_rnd_down(ndcf_first_y),
					  int_rnd_down(ndcf_first_z));
      }
      i++;
      /*if (last_material) {
       *round_to_bignum(&ndcf_first_x, &ndcf_first_y, &ndcf_first_z);
       *}
       */
      /* Only do above up to 3 times and stop upon first time the material
       * is non-zero
       */
      /*      printf("trial %d:  %d %d %d for t=%f\n", i, int_rnd_down(ndcf_first_x),
       *	     int_rnd_down(ndcf_first_y),
       *	     int_rnd_down(ndcf_first_z),
       *	     t[i-1]);
       */
    } while ((!last_material)&&(i<3));

    if ((!material)&&(!last_material)) {
      /* In this case, do not start in box and do not hit box */
      /* printf("Did not even hit bounding box.\n"); */
      return;
    }

    /* If we start in region 0, we are just starting outside the box.
     * region 0 is outside box and region 1 is buffer/void.  So, provided
     * we are in the box, we say we actually start in region 1 rather than
     * 0
     */
    if (material==0) {
      /* printf("Changing initial material number from 0 to 1.\n"); */
      material=geom_ptr->uvval[1];

      /* STOP HERE IF WE DON'T ENTER BUFFER AT THIS POINT AND RETURN
       * DISTANCE TO CURRENT POINT.  THEN, CAN REMOVE BELOW
       * ASSUMPTION
       */
    }

    /* Note:  One assumption is that the 'buffer' zone is at all edges
     *        of the boxlike volume
     */
  }
  
  *this = geom_ptr->regionnum[material];

  /* Here's where the stepping algorithm comes in ... */
  sgndx = ifsign(ndcf_vx);
  sgndy = ifsign(ndcf_vy);
  sgndz = ifsign(ndcf_vz);
  
  if (sgndx>=0) pfdx = ndcf_vx;
  else pfdx = -ndcf_vx;
  if (sgndy>=0) pfdy = ndcf_vy;
  else pfdy = -ndcf_vy;
  if (sgndz>=0) pfdz = ndcf_vz;
  else pfdz = -ndcf_vz;
  
  x = int_rnd_down(ndcf_first_x);
  y = int_rnd_down(ndcf_first_y);
  z = int_rnd_down(ndcf_first_z);
  
  incrx = bignum*pfdx;
  incry = bignum*pfdy;
  incrz = bignum*pfdz;
  
  if (sgndx<0) diffx = ndcf_first_x - (float)x;
  else diffx = (float)(x+1)-ndcf_first_x;

  if (sgndy<0) diffy = ndcf_first_y - (float)y;
  else diffy = (float)(y+1)-ndcf_first_y;

  if (sgndz<0) diffz = ndcf_first_z - (float)z;
  else diffz = (float)(z+1)-ndcf_first_z;

#ifdef PRINTS
  printf("diffx = %f   diffy = %f   diffz = %f\n",
	 diffx, diffy, diffz);
#endif
  
  eyx = bignum*(pfdx*diffy-pfdy*diffx);
  eyz = bignum*(pfdz*diffy-pfdy*diffz);
  ezx = bignum*(pfdx*diffz-pfdz*diffx);
 
  /* when eyx < 0, x doesn't want to increase */
  /* when eyx > 0, y doesn't want to increase */
  /* when eyx = 0, no error, increase both */
  /* when eyz < 0, z doesn't want to increase */
  /* when eyz > 0, y doesn't want to increase */
  /* when eyz = 0, both... */
  /* when ezx < 0, x doesn't want to increase */
  /* when ezx > 0, z doesn't want to increase */
  /* when ezx = 0, both... */
  
  last_material = material; /* Need to keep track of when it changes */
  valid_position = 1;
  is_even = 1;
  possibly_done = 0;
  do {
    /* Keep moving voxel by voxel UNTIL we
     * (1) hit a different material _or_
     * (2) exit our bounding box entirely
     *     (this will yield a 0 material #)
     */
    
    is_even = !is_even;
    univels_hit++;
    which_to_increment = 7;
    if (eyx!=0) {
      if (eyx>0) {
	which_to_increment &=5; /* don't increment y -- 101*/
      } else {
	which_to_increment &=3; /* don't increment x -- 011*/	
      }
    }
    if (eyz!=0) {
      if (eyz>0) {
	which_to_increment &=5; /* don't increment y -- 101*/
      } else {
	which_to_increment &=6; /* don't increment z -- 110*/
      }
    }
    if (ezx!=0) {
      if (ezx>0) {
	which_to_increment &=6; /* don't increment z -- 110*/
      } else {
	which_to_increment &=3; /* don't increment x -- 011*/	
      }
    }
    
    /* Now, we move according the numbers calculated above */
    switch(which_to_increment)
      {
      case 1:                       /* 001 -> increment z */
	z+=sgndz;
	eyz-=incry;
	ezx+=incrx;
	if ((z<0)||(z>=gnumslices))
	  valid_position = 0;
	break;
      case 2:                       /* 010 -> increment y */
	y+=sgndy;
	eyx+=incrx;
	eyz+=incrz;
	if ((y<0)||(y>=gheight))
	  valid_position = 0;	
	break;
      case 3:                       /* 011 -> increment y and z */
	y+=sgndy;
	z+=sgndz;
	eyx+=incrx;
	eyz += incrz - incry;
	ezx+=incrx;
	if ((y<0)||(y>=gheight)||(z<0)||(z>=gnumslices))
	  valid_position = 0;	
	break;
      case 4:                       /* 100 -> increment x */
	x+=sgndx;
	eyx-=incry;
	ezx-=incrz;
	if ((x<0)||(x>=gwidth))
	  valid_position = 0;	
	break;
      case 5:                       /* 101 -> increment x and z */
	x+=sgndx;
	z+=sgndz;
	eyx-=incry;
	eyz-=incry;
	ezx+=incrx-incrz;
	if ((x<0)||(x>=gwidth)||(z<0)||(z>=gnumslices))
	  valid_position = 0;	
	break;
      case 6:                       /* 110 -> increment x and y */
	x+=sgndx;
	y+=sgndy;
	eyx+=incrx-incry;
	eyz+=incrz;
	ezx-=incrz;
	if ((x<0)||(x>=gwidth)||(y<0)||(y>=gheight))
	  valid_position = 0;	
	break;
      case 7:                       /* 111 -> increment x and y and z */
	x+=sgndx;
	y+=sgndy;
	z+=sgndz;
	eyx+=incrx-incry;
	eyz+=incrz-incry;
	ezx+=incrx-incrz;
	if ((x<0)||(x>=gwidth)||(y<0)||(y>=gheight)
	    ||(z<0)||(z>=gnumslices))
	  valid_position = 0;	
	break;
      default:
	/* If we get to here, we've quit incrementing. */
	fprintf(stderr, "We're stuck!\nMust exit.\n");
	fprintf(stderr, "The e's are:  eyx: %ld eyz: %ld ezx %ld\n",
		eyx, eyz, ezx);
	exit(1);
      }

    if (valid_position) {
      if ((is_even)||(global_accuracy)) {
	/* On even times through, check what material we are in */
	/* Or, check every time if global_accuracy is on */
	material = lookup_nochecking(x, y, z);
	if (material!=last_material) {
	  /* Cut out quick if global accuracy is on -- if so, we're
	   * checking every univel and stop asap
	   */
	  if (global_accuracy) {
	    saved_x = x;
	    saved_y = y;
	    saved_z = z;
	    last_incremented = which_to_increment;
	    break; /* --> Jump out of loop */
	  }
	  if (last_material!=lookup_nochecking(saved_x, saved_y, saved_z)) {
	    /* In this case, 2 univels in a row found so stop */
	    /* last even kept track of saved_x, y, z, and last_incremented */
	    break; /* --> Jump out of loop */
	  } else {
	    /* In this case, the new material _just_ hit on this even
	     * trial so save where we are and see if we hit it 2 in a row
	     */
	    possibly_done = 1;
	    saved_x = x;
	    saved_y = y;
	    saved_z = z;
	    last_incremented = which_to_increment;
	  }
	}
      } else { /* is_even is false */
	if (possibly_done) {
	  /* In this case, last even found a new material type and we
	   * just went one extra step -- see if still in new material
	   */
	  material = lookup_nochecking(x, y, z);
	  if (material!=last_material)
	    /* assert that saved_x, y, z and last_incremented are valid */
	    /* _and_ 2 univels in row of same material type -- endloop */
	    break;
	  else 
	    possibly_done = 0;
	}
	/* On odd times through, save values for x, y, and z */
	saved_x = x;
	saved_y = y;
	saved_z = z;
	last_incremented = which_to_increment;
      }
    } else { /* valid_position is false */
      /* We've left the box */
      material = 0;
      break;
    }
  } while (1); /* we break out when 2 in row or exit box */


  /* Do no further computations if material is 0 as we've exited
   * the box.  For speed, just return.
   * (Note:  Miss=1 was set above so we need do nothing else)
   */
  if (material==0) {
#ifdef PRINTS
    printf("-- EXITED BOUNDING BOX)\n");
#endif
    return;
  } else {
    if (sgndx>=0) {
      xedge = (float)saved_x;
    } else {
      xedge = (float)(saved_x+1);
    }
    if (sgndy>=0) {
      yedge = (float)saved_y;
    } else {
      yedge = (float)(saved_y+1);
    }
    if (sgndz>=0) {
      zedge = (float)saved_z;
    } else {
      zedge = (float)(saved_z+1);
    }
    switch(last_incremented) {
    case 1: /* z */
      /*      time = (zedge-ndcf_z0)*ndcf_inv_vz;*/
      *dist = (double)(((zedge-ndcf_z0)*geom_ptr->pixelsizeslices)/wcf_vz);
      break;
    case 2: /* y */
      /*      time = (yedge-ndcf_y0)*ndcf_inv_vy;*/
      *dist = (double)(((yedge-ndcf_y0)*geom_ptr->pixelsizerows)/wcf_vy);
      break;
    case 3: /* y and z */
      if (pfdy>pfdz) { /* Here use y */
	/*	time = (yedge-ndcf_y0)*ndcf_inv_vy;*/	
	*dist = (double)(((yedge-ndcf_y0)*geom_ptr->pixelsizerows)/wcf_vy);
      } else { /* Here, use z */
	/*	time = (zedge-ndcf_z0)*ndcf_inv_vz;*/
	*dist = (double)(((zedge-ndcf_z0)*geom_ptr->pixelsizeslices)/wcf_vz);
      }
      break;
    case 4: /* x */
      /*      time = (xedge-ndcf_x0)*ndcf_inv_vx;*/
      *dist = (double)(((xedge-ndcf_x0)*geom_ptr->pixelsizecolumns)/wcf_vx);
      break;
    case 5: /* x and z */
      if (pfdx>pfdz) { /* Here use x */
	/*	time = (xedge-ndcf_x0)*ndcf_inv_vx;*/
	*dist = (double)(((xedge-ndcf_x0)*geom_ptr->pixelsizecolumns)/wcf_vx);
      } else { /* Here, use z */
	/*	time = (zedge-ndcf_z0)*ndcf_inv_vz;*/
	*dist = (double)(((zedge-ndcf_z0)*geom_ptr->pixelsizeslices)/wcf_vz);
      }
      break;
    case 6: /* x and y */
      if (pfdx>pfdz) { /* Here use x */
	/*	time = (xedge-ndcf_x0)*ndcf_inv_vx;*/
	*dist = (double)(((xedge-ndcf_x0)*geom_ptr->pixelsizecolumns)/wcf_vx);
      } else { /* Here, use y */
	/*	time = (yedge-ndcf_y0)*ndcf_inv_vy;*/
	*dist = (double)(((yedge-ndcf_y0)*geom_ptr->pixelsizerows)/wcf_vy);
      }
      break;
    case 7: /* x and y and z */
      if ((pfdx>=pfdz)&&(pfdx>=pfdy)) { /* Here use x */
	/*	time = (xedge-ndcf_x0)*ndcf_inv_vx;*/
	*dist = (double)(((xedge-ndcf_x0)*geom_ptr->pixelsizecolumns)/wcf_vx);
      } else if (pfdy>=pfdz) { /* Here, use y */
	/*	time = (yedge-ndcf_y0)*ndcf_inv_vy;*/
	*dist = (double)(((yedge-ndcf_y0)*geom_ptr->pixelsizerows)/wcf_vy);
      } else { /* Here, use z */
	/*	time = (zedge-ndcf_z0)*ndcf_inv_vz;*/
	*dist = (double)(((zedge-ndcf_z0)*geom_ptr->pixelsizeslices)/wcf_vz);
      }
      break;
    }
    
#ifdef DONTUSE
    time+=EPSILON; /* move 'into' the material a little */
    ndcf_x = ndcf_x0+time*ndcf_vx;
    ndcf_y = ndcf_y0+time*ndcf_vy;
    ndcf_z = ndcf_z0+time*ndcf_vz;

    /*if (material==geom_array_lookup(geom_ptr,
      int_rnd_down(ndcf_x),
      int_rnd_down(ndcf_y),
      int_rnd_down(ndcf_z))) {*/
    if ((saved_x==int_rnd_down(ndcf_x))&&(saved_y==int_rnd_down(ndcf_y))&&(saved_z==int_rnd_down(ndcf_z))) {
      success=1;
    } else {
      success=0;
      fprintf(stderr, "Small roundoff error.\n");
    }
    
    if (!success) {
      static int tried_so_far = 0;
      static int fixed_so_far = 0;
      float new_eps = EPSILON;
      int looptimes = 1;
      
      fprintf(stderr, "For starting point:           %f %f %f\n",
	      ndcf_x0, ndcf_y0, ndcf_z0);
      fprintf(stderr, "In direction:                 %f %f %f\n",
	      ndcf_vx, ndcf_vy, ndcf_vz);
      fprintf(stderr, "At time:  %f    (without epsilon %f)\n",
	      time, time-EPSILON);
      fprintf(stderr, "With diffs:                   %f %f %f\n",
	      diffx, diffy, diffz);
      fprintf(stderr, "When rounded down, expected:  %d %d %d\n", saved_x, saved_y, saved_z);
      fprintf(stderr, "But got (with epsilon):       %f %f %f\n", ndcf_x, ndcf_y, ndcf_z);
      fprintf(stderr, "Without:                      %f %f %f\n", ndcf_x-EPSILON*ndcf_vx, ndcf_y-EPSILON*ndcf_vy, ndcf_z-EPSILON*ndcf_vz);
      
      fprintf(stderr, "Possible roundoff error #%d.\n", tried_so_far);
      fprintf(stderr, "Fixed so far:  #%d.\n", fixed_so_far);
      
      tried_so_far++;
      
      while (!success) {
	new_eps/=-2.0;
	looptimes*=2;
	
	for (i=0; i<looptimes; i++) {
	  ndcf_x+=new_eps;
	  ndcf_y+=new_eps;
	  ndcf_z+=new_eps;
	  if ((saved_x==int_rnd_down(ndcf_x))&&(saved_y==int_rnd_down(ndcf_y))&&(saved_z==int_rnd_down(ndcf_z))) {
	    success=1;
	    fixed_so_far++;
	    break;
	  }
	}
	if (looptimes>=1024) {
	  success = 1; /* just to stop and move on... */
	  fprintf(stderr, "Stuck and will move on.\n");
	  /*sleep(1);*/
	}
      }
    }
#endif
  
    /*ndcf_to_wcf(geom_ptr, ndcf_x, ndcf_y, ndcf_z, &wcf_x, &wcf_y, &wcf_z);*/
    
    /* printf("%f %f %f    %f %f %f\n", ndcf_x, ndcf_y, ndcf_z, wcf_x, wcf_y, wcf_z);*/
    
#ifdef PRINTS
    /*    printf("(%0.2f, %0.2f, %0.2f) (d=%0.2f) contains regions:",
	   wcf_x, wcf_y, wcf_z,
	   sqrt((double)((wcf_x-wcf_x0)*(wcf_x-wcf_x0)+(wcf_y-wcf_y0)*(wcf_y-wcf_y0)+(wcf_z-wcf_z0)*(wcf_z-wcf_z0))));
	   */
    if (!material)
      printf("  (NOTHING -- EXITED BOX)\n");
    else {
      printf("  %s at %f\n", geom_ptr->bodyname[material], (float)(*dist));
    }
#endif
  }
    
  if (material!=0) {
#ifdef INTERSECT_CODE
    total_intersections++; /* global variable to keep track of */
#endif
    *next = geom_ptr->regionnum[material];
    *miss = 0;
    /* added epsilon to the distance to be sure we entered voxel
     * and don't just hit the edge or round down to being
     * outside it
     */
    *dist += EPSILON;
    /*    *dist = (double)(sqrt((double)((wcf_x-wcf_x0)*(wcf_x-wcf_x0)
     *      +(wcf_y-wcf_y0)*(wcf_y-wcf_y0)+(wcf_z-wcf_z0)*(wcf_z-wcf_z0))));
     */
  }
}
#endif


#ifdef LIBUV50APPROX
/* Also known as libuv60 */
/*************************************************************************
 * IG_NAME (INTERROGATE_GEOMETRY)
 *
 * Called by RAFFLE through subroutine DTB. This is the basic interface 
 * to spline surfaces. The starting point (pt) and direction
 * cosines(dir), which must be NORMALIZED, are passed to the interface via
 * the "wrapper". The "hit" variable contains the intersection point if there
 * is one (returns TRUE).
 * 
 * INPUT VALUES
 *    fray_pt -> starting point (x,y,z) 
 *    fray_dir -> direction cosines
 *
 * OUTPUT VALUES
 *    dist -> distance to boundary  !! (currently in NORMALIZED units) !!
 *    this ->  material number or value of current region
 *    next ->  material number of next region
 *    miss ->  if no intersection is found miss_flag > 0
 **************************************************************************/
void IG_NAME ( double *fray_pt, double *fray_dir, double *dist,
	       int *this, int *next, int *miss )
{
  float wcf_x0, wcf_y0, wcf_z0,
    wcf_vx, wcf_vy, wcf_vz;
  /* Also, in this case, geom_ptr is not passed -- use global variable geom_ptr */
  float wcf_x, wcf_y, wcf_z;
  float ndcf_x, ndcf_y, ndcf_z;
  float ndcf_x0, ndcf_y0, ndcf_z0, ndcf_first_x, ndcf_first_y, ndcf_first_z;
  float pfdx, pfdy, pfdz;
  float ndcf_vx, ndcf_vy, ndcf_vz, ndcf_inv_vx, ndcf_inv_vy, ndcf_inv_vz;
  float t[3];
  /*long*/ int eyx, eyz, ezx, incrx, incry, incrz;
  static /*long*/ int bignum = 1073741824; /* 2^30 */
  int x, y, z, saved_x, saved_y, saved_z;
  float xedge, yedge, zedge;
  int sgndx, sgndy, sgndz, possibly_done;
  float diffx, diffy, diffz;
  int which_to_increment, last_incremented, valid_position, i;
  float time;
  region_t material, last_material;
  int success;
  int is_even;

  *miss = 1; /* assume a miss unless proven otherwise */

  wcf_x = wcf_x0 = (float) fray_pt[0];
  wcf_y = wcf_y0 = (float) fray_pt[1];
  wcf_z = wcf_z0 = (float) fray_pt[2];

  wcf_vx = (float) fray_dir[0];
  wcf_vy = (float) fray_dir[1];
  wcf_vz = (float) fray_dir[2];

  /* get pixel coords in floats -- initialization */
  wcf_to_ndcf(geom_ptr, wcf_x0, wcf_y0, wcf_z0, &ndcf_x0, &ndcf_y0, &ndcf_z0);
  /*round_to_bignum(&ndcf_x0, &ndcf_y0, &ndcf_z0);*/
  
  material = geom_array_lookup(geom_ptr, int_rnd_down(ndcf_x0), int_rnd_down(ndcf_y0), int_rnd_down(ndcf_z0));
  
  ndcf_vx = wcf_vx*geom_ptr->inv_pixelsizecolumns;
  ndcf_vy = wcf_vy*geom_ptr->inv_pixelsizerows;
  ndcf_vz = wcf_vz*geom_ptr->inv_pixelsizeslices;
  
  /* A vector to step along in array dimensions.  Each component should be
   * between -1 and 1.  Call function to place all within the bounds.
   */
  place_in_bounds(&ndcf_vx, &ndcf_vy, &ndcf_vz);
  /*round_to_bignum(&ndcf_vx, &ndcf_vy, &ndcf_vz);*/

  /* Print where we initially start */
#ifdef PRINTS
  printf("(%0.2f, %0.2f, %0.2f) (d=%0.2f) contains regions:",
	 wcf_x, wcf_y, wcf_z,
	 sqrt((double)((wcf_x-wcf_x0)*(wcf_x-wcf_x0)+(wcf_y-wcf_y0)*(wcf_y-wcf_y0)+(wcf_z-wcf_z0)*(wcf_z-wcf_z0))));
  printf("  %s\n", geom_ptr->bodyname[material]);
#endif

  /* If the material is 0, we are outside the box
   * Didn't start in bounding box -- try to move into bounding box but
   * we'll just return if we can't intersect it -----
   * (ndcf_x0, ndcf_y0, ndcf_z0) will be modified as necessary
   */
  if (material!=0) {
    /* We start inside the box */
    ndcf_first_x = ndcf_x0;
    ndcf_first_y = ndcf_y0;
    ndcf_first_z = ndcf_z0;
  } else { /* material==0 */
    /* We start outside the box -- try to move inside */
    /* use float t[3]:  time to (ndcf_x0, ?, ?), (?, ndcf_y0, ?),
     * and (?, ?, ndcf_z0) respectively
     * --> must be >= 0 else moving wrong way
     */

    /* Maybe only need these if outside of box... */
    /* Yes -- changed on 6-25-97 */
    if (ndcf_vx!=0.0)
      ndcf_inv_vx = 1.0/ndcf_vx;
    else
      ndcf_inv_vx = -INFINITY;
    if (ndcf_vy!=0.0)
      ndcf_inv_vy = 1.0/ndcf_vy;
    else
      ndcf_inv_vy = -INFINITY;
    if (ndcf_vz!=0.0)
      ndcf_inv_vz = 1.0/ndcf_vz;
    else
      ndcf_inv_vz = -INFINITY;
    
    if (ndcf_x0<0.0)
      ndcf_first_x = 0.0;
    else
      ndcf_first_x = (float)(gwidth);

    if (ndcf_y0<0.0)
      ndcf_first_y = 0.0;
    else
      ndcf_first_y = (float)(gheight);

    if (ndcf_z0<0.0)
      ndcf_first_z = 0.0;
    else
      ndcf_first_z = (float)(gnumslices);

    /* don't divide by 0 */
    t[0] = (ndcf_first_x-ndcf_x0)*ndcf_inv_vx + EPSILON;
    t[1] = (ndcf_first_y-ndcf_y0)*ndcf_inv_vy + EPSILON;
    t[2] = (ndcf_first_z-ndcf_z0)*ndcf_inv_vz + EPSILON;

    /* printf("The t's are:  %f %f %f\n", t[0], t[1], t[2]); */

    /* swap to order t[0], t[1], t[2] */
    float_sort_3(t);

    /* Assert t[0] < t[1] < t[2] */

    /* Now, loop until first positive t that gives all 3 x,y,z in range */
    /* If none give all three in range, don't intersect box */
    i = 0;
    do {
      if (t[i]>0.0) {
	ndcf_first_x = ndcf_x0+t[i]*ndcf_vx;
	ndcf_first_y = ndcf_y0+t[i]*ndcf_vy;
	ndcf_first_z = ndcf_z0+t[i]*ndcf_vz;
	last_material = geom_array_lookup(geom_ptr,
					  int_rnd_down(ndcf_first_x),
					  int_rnd_down(ndcf_first_y),
					  int_rnd_down(ndcf_first_z));
      }
      i++;
      /*if (last_material) {
       *round_to_bignum(&ndcf_first_x, &ndcf_first_y, &ndcf_first_z);
       *}
       */
      /* Only do above up to 3 times and stop upon first time the material
       * is non-zero
       */
      /*      printf("trial %d:  %d %d %d for t=%f\n", i, int_rnd_down(ndcf_first_x),
       *	     int_rnd_down(ndcf_first_y),
       *	     int_rnd_down(ndcf_first_z),
       *	     t[i-1]);
       */
    } while ((!last_material)&&(i<3));

    if ((!material)&&(!last_material)) {
      /* In this case, do not start in box and do not hit box */
      /* printf("Did not even hit bounding box.\n"); */
      return;
    }

    /* If we start in region 0, we are just starting outside the box.
     * region 0 is outside box and region 1 is buffer/void.  So, provided
     * we are in the box, we say we actually start in region 1 rather than
     * 0
     */
    if (material==0) {
      /* printf("Changing initial material number from 0 to 1.\n"); */
      material=geom_ptr->uvval[1];

      /* STOP HERE IF WE DON'T ENTER BUFFER AT THIS POINT AND RETURN
       * DISTANCE TO CURRENT POINT.  THEN, CAN REMOVE BELOW
       * ASSUMPTION
       */
    }

    /* Note:  One assumption is that the 'buffer' zone is at all edges
     *        of the boxlike volume
     */
  }
  
  *this = geom_ptr->regionnum[material];

  /* Here's where the stepping algorithm comes in ... */
  sgndx = ifsign(ndcf_vx);
  sgndy = ifsign(ndcf_vy);
  sgndz = ifsign(ndcf_vz);
  
  if (sgndx>=0) pfdx = ndcf_vx;
  else pfdx = -ndcf_vx;
  if (sgndy>=0) pfdy = ndcf_vy;
  else pfdy = -ndcf_vy;
  if (sgndz>=0) pfdz = ndcf_vz;
  else pfdz = -ndcf_vz;
  
  x = int_rnd_down(ndcf_first_x);
  y = int_rnd_down(ndcf_first_y);
  z = int_rnd_down(ndcf_first_z);
  
  incrx = bignum*pfdx;
  incry = bignum*pfdy;
  incrz = bignum*pfdz;
  
  if (sgndx<0) diffx = ndcf_first_x - (float)x;
  else diffx = (float)(x+1)-ndcf_first_x;

  if (sgndy<0) diffy = ndcf_first_y - (float)y;
  else diffy = (float)(y+1)-ndcf_first_y;

  if (sgndz<0) diffz = ndcf_first_z - (float)z;
  else diffz = (float)(z+1)-ndcf_first_z;

#ifdef PRINTS
  printf("diffx = %f   diffy = %f   diffz = %f\n",
	 diffx, diffy, diffz);
#endif
  
  eyx = bignum*(pfdx*diffy-pfdy*diffx);
  eyz = bignum*(pfdz*diffy-pfdy*diffz);
  ezx = bignum*(pfdx*diffz-pfdz*diffx);
 
  /* when eyx < 0, x doesn't want to increase */
  /* when eyx > 0, y doesn't want to increase */
  /* when eyx = 0, no error, increase both */
  /* when eyz < 0, z doesn't want to increase */
  /* when eyz > 0, y doesn't want to increase */
  /* when eyz = 0, both... */
  /* when ezx < 0, x doesn't want to increase */
  /* when ezx > 0, z doesn't want to increase */
  /* when ezx = 0, both... */
  
  last_material = material; /* Need to keep track of when it changes */
  valid_position = 1;
  is_even = 1;
  possibly_done = 0;
  do {
    /* Keep moving voxel by voxel UNTIL we
     * (1) hit a different material _or_
     * (2) exit our bounding box entirely
     *     (this will yield a 0 material #)
     */
    
    is_even = !is_even;
    univels_hit++;
    if (eyx<0) {
      /* don't increment x */
      if (eyz>0) {
	/* don't increment x or y */
	which_to_increment = 1;              /* increment z */
	z+=sgndz;
	eyz-=incry;
	ezx+=incrx;
	if ((z<0)||(z>=gnumslices))
	  valid_position = 0;
      } else {
	/* don't increment x or z */
	which_to_increment = 2;              /* increment y */
	y+=sgndy;
	eyx+=incrx;
	eyz+=incrz;
	if ((y<0)||(y>=gheight))
	  valid_position = 0;	
      }
    } else {
      /* don't increment y */
      if (ezx<0) {
	/* don't increment y or x */
	which_to_increment = 1;              /* increment z */
	z+=sgndz;
	eyz-=incry;
	ezx+=incrx;
	if ((z<0)||(z>=gnumslices))
	  valid_position = 0;
      } else {
	/* don't increment y or z */
	which_to_increment = 4;              /* increment x */
	x+=sgndx;
	eyx-=incry;
	ezx-=incrz;
	if ((x<0)||(x>=gwidth))
	  valid_position = 0;	
      }
    }
    
    if (valid_position) {
      if ((is_even)||(global_accuracy)) {
	/* On even times through, check what material we are in */
	/* Or, check every time if global_accuracy is on */
	material = lookup_nochecking(x, y, z);
	if (material!=last_material) {
	  /* Cut out quick if global accuracy is on -- if so, we're
	   * checking every univel and stop asap
	   */
	  if (global_accuracy) {
	    saved_x = x;
	    saved_y = y;
	    saved_z = z;
	    last_incremented = which_to_increment;
	    break; /* --> Jump out of loop */
	  }
	  if (last_material!=lookup_nochecking(saved_x, saved_y, saved_z)) {
	    /* In this case, 2 univels in a row found so stop */
	    /* last even kept track of saved_x, y, z, and last_incremented */
	    break; /* --> Jump out of loop */
	  } else {
	    /* In this case, the new material _just_ hit on this even
	     * trial so save where we are and see if we hit it 2 in a row
	     */
	    possibly_done = 1;
	    saved_x = x;
	    saved_y = y;
	    saved_z = z;
	    last_incremented = which_to_increment;
	  }
	}
      } else { /* is_even is false */
	if (possibly_done) {
	  /* In this case, last even found a new material type and we
	   * just went one extra step -- see if still in new material
	   */
	  material = lookup_nochecking(x, y, z);
	  if (material!=last_material)
	    /* assert that saved_x, y, z and last_incremented are valid */
	    /* _and_ 2 univels in row of same material type -- endloop */
	    break;
	  else 
	    possibly_done = 0;
	}
	/* On odd times through, save values for x, y, and z */
	saved_x = x;
	saved_y = y;
	saved_z = z;
	last_incremented = which_to_increment;
      }
    } else { /* valid_position is false */
      /* We've left the box */
      material = 0;
      break;
    }
  } while (1); /* we break out when 2 in row or exit box */


  /* Do no further computations if material is 0 as we've exited
   * the box.  For speed, just return.
   * (Note:  Miss=1 was set above so we need do nothing else)
   */
  if (material==0) {
#ifdef PRINTS
    printf("-- EXITED BOUNDING BOX)\n");
#endif
    return;
  } else {
    if (sgndx>=0) {
      xedge = (float)saved_x;
    } else {
      xedge = (float)(saved_x+1);
    }
    if (sgndy>=0) {
      yedge = (float)saved_y;
    } else {
      yedge = (float)(saved_y+1);
    }
    if (sgndz>=0) {
      zedge = (float)saved_z;
    } else {
      zedge = (float)(saved_z+1);
    }
    switch(last_incremented) {
    case 1: /* z */
      /*      time = (zedge-ndcf_z0)*ndcf_inv_vz;*/
      *dist = (double)(((zedge-ndcf_z0)*geom_ptr->pixelsizeslices)/wcf_vz);
      break;
    case 2: /* y */
      /*      time = (yedge-ndcf_y0)*ndcf_inv_vy;*/
      *dist = (double)(((yedge-ndcf_y0)*geom_ptr->pixelsizerows)/wcf_vy);
      break;
    case 4: /* x */
      /*      time = (xedge-ndcf_x0)*ndcf_inv_vx;*/
      *dist = (double)(((xedge-ndcf_x0)*geom_ptr->pixelsizecolumns)/wcf_vx);
      break;
    }
    
#ifdef DONTUSE
    time+=EPSILON; /* move 'into' the material a little */
    ndcf_x = ndcf_x0+time*ndcf_vx;
    ndcf_y = ndcf_y0+time*ndcf_vy;
    ndcf_z = ndcf_z0+time*ndcf_vz;

    /*if (material==geom_array_lookup(geom_ptr,
      int_rnd_down(ndcf_x),
      int_rnd_down(ndcf_y),
      int_rnd_down(ndcf_z))) {*/
    if ((saved_x==int_rnd_down(ndcf_x))&&(saved_y==int_rnd_down(ndcf_y))&&(saved_z==int_rnd_down(ndcf_z))) {
      success=1;
    } else {
      success=0;
      fprintf(stderr, "Small roundoff error.\n");
    }
    
    if (!success) {
      static int tried_so_far = 0;
      static int fixed_so_far = 0;
      float new_eps = EPSILON;
      int looptimes = 1;
      
      fprintf(stderr, "For starting point:           %f %f %f\n",
	      ndcf_x0, ndcf_y0, ndcf_z0);
      fprintf(stderr, "In direction:                 %f %f %f\n",
	      ndcf_vx, ndcf_vy, ndcf_vz);
      fprintf(stderr, "At time:  %f    (without epsilon %f)\n",
	      time, time-EPSILON);
      fprintf(stderr, "With diffs:                   %f %f %f\n",
	      diffx, diffy, diffz);
      fprintf(stderr, "When rounded down, expected:  %d %d %d\n", saved_x, saved_y, saved_z);
      fprintf(stderr, "But got (with epsilon):       %f %f %f\n", ndcf_x, ndcf_y, ndcf_z);
      fprintf(stderr, "Without:                      %f %f %f\n", ndcf_x-EPSILON*ndcf_vx, ndcf_y-EPSILON*ndcf_vy, ndcf_z-EPSILON*ndcf_vz);
      
      fprintf(stderr, "Possible roundoff error #%d.\n", tried_so_far);
      fprintf(stderr, "Fixed so far:  #%d.\n", fixed_so_far);
      
      tried_so_far++;
      
      while (!success) {
	new_eps/=-2.0;
	looptimes*=2;
	
	for (i=0; i<looptimes; i++) {
	  ndcf_x+=new_eps;
	  ndcf_y+=new_eps;
	  ndcf_z+=new_eps;
	  if ((saved_x==int_rnd_down(ndcf_x))&&(saved_y==int_rnd_down(ndcf_y))&&(saved_z==int_rnd_down(ndcf_z))) {
	    success=1;
	    fixed_so_far++;
	    break;
	  }
	}
	if (looptimes>=1024) {
	  success = 1; /* just to stop and move on... */
	  fprintf(stderr, "Stuck and will move on.\n");
	  /*sleep(1);*/
	}
      }
    }
#endif
  
    /*ndcf_to_wcf(geom_ptr, ndcf_x, ndcf_y, ndcf_z, &wcf_x, &wcf_y, &wcf_z);*/
    
    /* printf("%f %f %f    %f %f %f\n", ndcf_x, ndcf_y, ndcf_z, wcf_x, wcf_y, wcf_z);*/
    
#ifdef PRINTS
    /*    printf("(%0.2f, %0.2f, %0.2f) (d=%0.2f) contains regions:",
	   wcf_x, wcf_y, wcf_z,
	   sqrt((double)((wcf_x-wcf_x0)*(wcf_x-wcf_x0)+(wcf_y-wcf_y0)*(wcf_y-wcf_y0)+(wcf_z-wcf_z0)*(wcf_z-wcf_z0))));
	   */
    if (!material)
      printf("  (NOTHING -- EXITED BOX)\n");
    else {
      printf("  %s at %f\n", geom_ptr->bodyname[material], (float)(*dist));
    }
#endif
  }
    
  if (material!=0) {
#ifdef INTERSECT_CODE
    total_intersections++; /* global variable to keep track of */
#endif
    *next = geom_ptr->regionnum[material];
    *miss = 0;
    /* added epsilon to the distance to be sure we entered voxel
     * and don't just hit the edge or round down to being
     * outside it
     */
    *dist += EPSILON;
    /*    *dist = (double)(sqrt((double)((wcf_x-wcf_x0)*(wcf_x-wcf_x0)
     *      +(wcf_y-wcf_y0)*(wcf_y-wcf_y0)+(wcf_z-wcf_z0)*(wcf_z-wcf_z0))));
     */
  }
}
#endif
