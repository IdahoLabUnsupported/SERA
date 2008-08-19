#include "include.h"
#include "read_raw.h"
#include "commonfcns.h"
#include "memory_tools.h"
#include "global.h"

unsigned char* read_raw(char *infilename, qhd_data_type *qhd_data, int ival) {
  char *buffer;
  int items_read;
  FILE *in_ptr;
  int FILEBUFFSIZE;
  int BPP_KEY_VALUE, DIMENSION1_VALUE, DIMENSION2_VALUE;
  int maxpix, minpix;
  static int first_call = 1;

  /* Check to see if file name is "legal" */
  if (!is_image_file(infilename)) {
      if (!confirm_popup("The file you have selected does not appear to be a valid image file.\nAre you sure you want to try to load this file?\n")) {
	  return(NULL);
      }
  }


  set_cursor(1);
  
  if (first_call) { /* forge information that would be in the .qhd typically */
    first_call=0;
    qhd_data->dimx=256;
    qhd_data->dimy=256;
    qhd_data->BPP=1;
    qhd_data->swap=0;
  }

  DIMENSION1_VALUE=qhd_data->dimx;
  DIMENSION2_VALUE=qhd_data->dimy;
  BPP_KEY_VALUE=qhd_data->BPP;
  
  /* get enough space to hold exactly 1 image file */
  FILEBUFFSIZE=BPP_KEY_VALUE*DIMENSION1_VALUE*DIMENSION2_VALUE;
  
  if ((in_ptr = fopen (infilename,"r"))!=NULL) {
    if (!(buffer=(char *)(MT_malloc(FILEBUFFSIZE))))
      exit(13);
    items_read = fread (buffer, (size_t) 1, FILEBUFFSIZE, in_ptr );
    if (items_read == FILEBUFFSIZE) {
      add_pixmap((unsigned char *)buffer, ival, *qhd_data);
    } else {
      printf("Sorry, your raw file is too short.\n");
      MT_free ((void *) buffer);
      buffer=NULL;
    }
    fclose ( in_ptr );
  }
  set_cursor(0);

  return((unsigned char *)buffer);
}
