#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "gen_resize.h"
#include "memory_tools.h"

#ifndef square
#define square(x) ((x)*(x))
#endif

#ifndef max
#define max(x, y) (x>y)?x:y
#endif

/* Generic resize routine taking 'nearest' pixel from input */
/* Note that input and output can point to the same memory as of 6-26-96, mwf */
void generic_resize_image(unsigned char *input, unsigned char *output, 
			  unsigned int inwidth, unsigned int inheight, 
			  unsigned int outwidth, unsigned int outheight, 
			  int pres_aspect_rat) {

    static unsigned int xcoorout;
    static unsigned int ycoorout;
    static float xcoorin;
    static float ycoorin;
    static unsigned int i1;
    static unsigned char *data;
    static float rat_x, rat_y, ratio;
    static float inw_over_outw, inh_over_outh;

    if (pres_aspect_rat) {
	rat_x = (float)outwidth/(float)inwidth;
	rat_y = (float)outheight/(float)inheight;
	
	/*	printf("X ratio, Y ratio :: %f, %f\n", rat_x, rat_y);*/

	if (rat_x<rat_y)
	    ratio = rat_x/rat_y;
	else
	    ratio = rat_y/rat_x;

	/* Note:  0 <= ratio <= 1.0 */
	/* Only consider it a change in aspect ratio if 1 coordinate
	 * is at least 2% off
	 */
	if (ratio<0.98) {
	    /* In this case, need to change input to make it work right */
	    int new_inheight = inheight;
	    int new_inwidth = inwidth;
	    unsigned char *newdata;

	    if (rat_x>rat_y) /* then inwidth needs to get bigger */
		new_inwidth = (float)(outwidth)/rat_y+0.5;
	    else
		new_inheight = (float)(outheight)/rat_x+0.5;

	    if ((new_inwidth>inwidth)||(new_inheight>inheight)) {
		int i, j;
		unsigned char *newdata;

		/*		printf("Converting to preserve aspect ratio.\n");
		 *		printf("%dx%d to %dx%d\n", inwidth, inheight, new_inwidth,
		 *		       new_inheight);
		 */

		newdata = (unsigned char*)MT_malloc(new_inwidth*new_inheight*sizeof(unsigned char));
		/* set all the new data to 0, black -- MWF */
		memset(newdata, 0, new_inwidth*new_inheight);

		if (new_inwidth>inwidth) {
		    int offs = (new_inwidth-inwidth)/2;
		    /* in this case, pad the left and the right */
		    for (i=0; i<inheight; i++) {
			for (j=0; j<inwidth; j++) {
			    newdata[i*new_inwidth+j+offs] = input[i*inwidth+j];
			}
		    }

		} else {
		    int offs = (new_inheight-inheight)/2;
		    /* in this case, pad the top and bottom */
		    for (i=0; i<inheight; i++) {
			for (j=0; j<inwidth; j++) {
			    newdata[(i+offs)*new_inwidth+j] = input[i*inwidth+j];
			}
		    }
		}

		/* Now, recurse with the new input so that the aspect ratio will be preserved. */
		generic_resize_image(newdata, output, new_inwidth, new_inheight, outwidth, outheight, 0);

		MT_free((void*)newdata);
		return;
	    } /* else, don't do anything */
	} /* else, do no extra work as both
	   * dimensions changed by same fraction
	   */
    }

    /* check for special case of image not changing size */
    if ((inwidth==outwidth)&&(inheight==outheight)) {
	if (input!=output)
	    memcpy(output, input, (size_t)(inwidth*inheight*sizeof(char)));
	return;
    }
    
    /* make a copy of the input if input and output point to same place */
    if (input==output) {
      data = (unsigned char *) MT_malloc(inwidth*inheight*sizeof(char));
      memcpy(data, input, (size_t)(inwidth*inheight*sizeof(char)));
    } else {
      data = input;
    }

    /*********************/
    /* The standard case */

    /* Keep track of these ratios for use over and over */
    inw_over_outw = ((float)inwidth)/((float)outwidth);
    inh_over_outh = ((float)inheight)/((float)outheight);

    /* For each pixel in the output, look back to see where it came from */
    
    for (ycoorout=0; ycoorout<outheight; ycoorout++) {
      for (xcoorout=0; xcoorout<outwidth; xcoorout++) {
	
	/* Find corresponding array indices of x and y in the input
	 * images.  The addition of 0.5 is to move to the 'center'
	 * of the pixel
	 */
	xcoorin=(((float)xcoorout)+0.5)*inw_over_outw;
	ycoorin=(((float)ycoorout)+0.5)*inh_over_outh;
	
	/* Now, use xcoorin and ycoorin to get the actual array lookup */
	/* the (int) conversion simply rounds down which is the convention
	 * as 1.0, 1.2, 1.5, 1.7, 1.9, ... 1.999 all represent the pixel
	 * integer value 1.
	 */
	i1=((int)ycoorin)*inwidth+((int)xcoorin);

	output[ycoorout*outwidth+xcoorout]=data[i1];
      }
    }
}
