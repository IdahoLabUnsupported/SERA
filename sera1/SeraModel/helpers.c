#include "include.h"
#include "functions.h"
#include "helpers.h"
#include "debug_tools.h"
#include "file_tools.h"

void print_children(Widget w)
{
    DEBUG_TRACE_IN printf("Entering print_children\n");
    twc_rec(w, 0);
    DEBUG_TRACE_OUT printf("Leaving print_children\n");
}

void twc_rec(Widget w, int depth) {
  image_matrix_type * image_matrix_ptr;
  int i;
  DEBUG_TRACE_IN printf("Entering twc_rec\n");
  
  image_matrix_ptr = get_image_matrix();

  for (i=0; i<depth; i++) {
    printf(" ");
  }
  if (XtIsWidget(w)) {
    printf("%2d %s", depth, XtName(w));
  } else {
    printf("Not a widget???  %s", XtName(w));
  }
  if (depth>0) {
    printf(" (%s)", XtName(XtParent(w)));
  }
  printf("\n");

  if (XtIsComposite(w)) {
    WidgetList theList;
    Cardinal listCount;
    int i;
    
    XtVaGetValues(w, XmNnumChildren, &listCount,
		  XmNchildren, &theList, NULL); 
    for (i = 0; i < listCount; i ++ )
      twc_rec(theList[i], depth+1);
  }
  DEBUG_TRACE_OUT printf("Leaving twc_rec\n");
}

/* Basically, gets all lines from the passed name of the assumed
 * text file and prints these lines to the screen.
 */
void print_lines_from_file(char * filename) {
  char ** lines;
  int numlines, i;
  DEBUG_TRACE_IN printf("Entering print_lines_from_file\n");
  
  lines = get_lines_from_file(filename, &numlines);

  for (i=0; i<numlines; i++) {
    printf("%3d \"%s\"\n", i, lines[i]);
  }

  free_lines_from_file(lines);
  DEBUG_TRACE_OUT printf("Leaving print_lines_from_file\n");
}

/* Checks to make sure that certain files, utilities, and environment
 * variables are available
 * IN FUTURE, WOULD BE GOOD TO CHECK IF
 *   'BNCT3D' AND 'GZIP' ARE IN THE PATH
 *   and soon BODYDATA.TXT
 */
void print_environment_integrity_check(void) {
  char * fname;
  char * ResourcePath;
  int i;

  DEBUG_TRACE_IN printf("Entering print_environment_integrity_check\n");
  /* Checks:
   * SERA_RESOURCES environment variable,
   * itrc           resource file,
   * regions.sav    saved regions file,
   * images.sav     saved image files,
   *
   *mbr 10-28-98 -> Not going to check for these any more
   *                because we no longer have default
   *                .txt files, the user must supply them
   *#######################################################
   * Fiducial.txt   fiducial markers,
   * Constraint.txt constraint markers,
   * body_data.txt  material dose information, etc.
   *#######################################################
   */
  
  printf("\nCHECKING YOUR ENVIRONMENT.");
  printf("\n==========================\n");
  ResourcePath = (char *)getenv("SERA_RESOURCES");
  if (ResourcePath) {
    printf("SERA_RESOURCES set to \"%s\"\n", ResourcePath);
  } else {
    printf("SERA_RESOURCES environment variable NOT SET!\n");
  }

  for (i=0; i<3; i++) {
    switch(i)
      {
      case 0:
	fname = get_resource_name();
	break;
      case 1:
	fname = get_saved_regions_fname();
	break;
      case 2:
	fname = get_saved_images_fname();
	break;

	/*case 3:
	 *fname = get_fiducial_markers_fname();
	 *break;
	 *case 4:
	 *fname = get_constraint_markers_fname();
	 *break;
	 *case 5:
	 *fname = get_body_data_fname();
	 *break;*/

      }
    printf("Checking \"%s\"\n", fname);
    if (FT_fileExists(fname)) {
      printf("  Present.\n");
    } else {
      printf("  NOT FOUND.\n");
    }
  }
  DEBUG_TRACE_OUT printf("Leaving print_environment_integrity_check\n");
}
