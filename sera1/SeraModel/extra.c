#include "include.h"
#include "image_matrix.h"
#include "extra.h"
#include "debug_tools.h"

/* The size of mask for median filter within the pyramid */
#define MASK_SIZE 3

/* Minimum dimension of an image in the pyramid -- when to stop */
#define MIN_DIM 128

/* Set to 1 to turn on pyramid filtering */
#define PYRAMID_FILTER 0

/* Some very small number */
#define EPSILON 0.0000001

/********************************************/
/* reduce_grays_optimal_driver
 */
void reduce_grays_optimal_driver(int numgrays) {
  int i, j, size, histo[256], regionnums[256], breaks[256];
  int total_size = 0;
  image_matrix_type * image_matrix_ptr;
  unsigned char * data;

  DEBUG_TRACE_IN printf("Entering reduce_grays_optimal_driver\n");
  
  image_matrix_ptr = get_image_matrix();

  /* set histogram to 0 */
  memset(histo, 0, 256*sizeof(int));

  /* calculate histogram for all images together */
  for (i=0; i<image_matrix_ptr->num_pics; i++) {
    size = (image_matrix_ptr->img_arr[i].data_w)
      *(image_matrix_ptr->img_arr[i].data_h);
    total_size+=size;
    data = image_matrix_ptr->img_arr[i].data;
    for (j=0; j<size; j++) {
      (histo[data[j]])++;
    }
  }

  /* Using the histogram, find out breakpoints for different regions */
  reduce_grays_optimal(regionnums, numgrays, histo, total_size, breaks+1);

  /* regionnums contains the mapping from an arbitrary pixel value
   * from 0 to 255 to it's label.  The label is currently the average
   * gray of a group of pixels all sharing the same label.
   */

  /* Finally, try 'filtering' the resultant images using a pyramid
   * structure along with a cost minimization to relabel potentially
   * mislabelled pixels.
   */
  if (PYRAMID_FILTER) {
    pyramid_filter(regionnums);
  } else {

    /* Now, reduce number of grays in image */
    for (i=0; i<image_matrix_ptr->num_pics; i++) {
      size = (image_matrix_ptr->img_arr[i].data_w)
	*(image_matrix_ptr->img_arr[i].data_h);
      data = image_matrix_ptr->img_arr[i].data;
      /*refine(data, breaks, numgrays,
	image_matrix_ptr->img_arr[i].data_w,
	image_matrix_ptr->img_arr[i].data_h,
	2);*/
      for (j=0; j<size; j++) {
	data[j] = (unsigned char)(regionnums[data[j]]);
      }
    }
  }
  DEBUG_TRACE_OUT printf("Leaving reduce_grays_optimal_driver\n");
}

/**************************************************************************
 * reduce_grays_optimal
 **************************************************************************
 * break the image set up into initial regions based on the number of labels
 * 
 * Parameters:
 *  regionnums  --> array of 256 integers that maps gray level to avg grey
 *  numlabels   --> number of labels (or regions) we desire
 *  histo       --> ptr to an array of 256 integers holding histogram
 *  size        --> total # pixels histogram accounts for
 *  breaks      --> array to store breakpoints in
 */
void reduce_grays_optimal(int *regionnums,
		 int numlabels, int *histo, int size,
		 int *breaks) {

    int i, j, done=0, maxp, minp;
    int maxnumlabels=256;
    float *breakpoints, *z, *lambda, lambdatop, lambdabottom, jf,
      pixels = (float) size;
    int num_non_zero = 0;

    DEBUG_TRACE_IN printf("Entering reduce_grays_optimal\n");

    for (i=0; i<256; i++) {
      if (histo[i]>0) num_non_zero++;
    }
    debug("Number of non-zero entries in histogram (out of 256):  %d\n",
	   num_non_zero);

    i=0;
    while(histo[i]==0) i++;
    minp = i;

    i=255;
    while(histo[i]==0) i--;
    maxp = i;

    breakpoints = (float *) MT_malloc((maxnumlabels+1)*sizeof(float));
    z = (float *) MT_malloc((maxnumlabels+1)*sizeof(float));
    lambda = (float *) MT_malloc(maxnumlabels*sizeof(float));

    /* The break points will partition the image such that:
     * region 0 = breakpoints[0] .. breakpoints[1]-1
     * region 1 = breakpoints[1] .. breakpoints[2]-1
     * etc...
     */

    /* Each region must initially have at least 1 pixel
     */
    debug("Initial breakup:  (Numbers between %d and %d)\n", minp, maxp);
    z[0] = 0.0;
    /*z[numlabels] = 256.0;*/
    
    {
      int pixels_used=0;
      int num_desired = 1; /* at least 1 pixel in a region */
      /*int num_desired = 1+0.25*pixels/numlabels; /* min number of pixels in a region */

      for (i=1; i<maxnumlabels; i++) {
	pixels_used = histo[(int)(z[i-1])];
	z[i] = z[i-1]+1.0;
	while (pixels_used<num_desired) {
	  if (((int)z[i])>255) {
	    /*debug("Unable to break images into %d regions.\n", numlabels);*/
	    debug("Using %d regions at most.\n", i-1);
	    z[i-1] = 256.0;
	    if (i-1<numlabels) {
	      numlabels = i-1;
	    }
	    maxnumlabels = i-1;
	    break;
	    /*exit(EXIT_FAILURE);*/
	  }
	  pixels_used += histo[(int)(z[i])];
	  z[i] = z[i] + 1.0;
	}
      }

      /* Assert, at this point, we have possibly more breakpoints than we
       * want.  Thin them out.  Have 'maxnumlabels' that need to be
       * decreased to 'numlabels'.
       */
      {
	float findex = 0.0, fincr;

	fincr = ((float)maxnumlabels)/((float)numlabels);
	if (numlabels!=maxnumlabels) {
	  for (i=1; i<numlabels; i++) {
	    findex+=fincr;
	    z[i]=z[(int)findex];
	  }
	  z[0] = 0.0; /* should be set but just to be sure... */
	  z[numlabels] = 256.0;
	}
      }

    }


    for (i=0; i<=numlabels; i++) {
	debug("%3.2f  ", z[i]);
    }
     
    debug("\n");

    while (!done) { /* iterate until convergence */
	for (i=0; i<=numlabels; i++) {
	    breakpoints[i] = z[i];
	}

	/* update the lambdas -- 1 less lambda than z */
	for (i=0; i<numlabels; i++) {
	    lambdatop = 0.0;
	    lambdabottom = 0.0;
	    for (j=myceil(z[i]); j<myceil(z[i+1]); j++) {
		lambdatop = lambdatop + ((float)j)*((float)histo[j]);
		lambdabottom = lambdabottom + (float)histo[j];
	    }
	    if (lambdabottom<=EPSILON) {
		debug("\nAt least one region contained no pixels.\n");
		exit(EXIT_FAILURE);
	    }
	    lambda[i] = (lambdatop/lambdabottom);
	    /* Note:  lambda[0] is the average gray level of the
	     *        first region, etc.
	     */
	}

	debug("Regions and averages:\n");
	for (i=1; i<=numlabels; i++) {
	  debug("[%3d,%3d]~%6.2f ", myceil(z[i-1]), myceil(z[i])-1, lambda[i-1]);
	  if (!(i%4)) debug("\n");
	}
	debug("\n");

	/* update the z's */
	/* Note:  here z[0] and z[numlabels] stays fixed... */
	for (i=1; i<numlabels; i++) {
	    z[i]=(lambda[i-1]+lambda[i])/2.0;
	}

	/* if we've converged, we're done */
	done=1;
	for (i=0; i<=numlabels; i++) {
	    if (breakpoints[i]!=z[i]) {
		done = 0;
		break;
	    }
	}
    }



    /***************************************************************/
    

    /* compute what region number each grey level maps to. */
    /* for example, regionnums = [0 0 0 0 0 0 28 28 28 28 28 57 57 57 57 57 99 99 99 99 99 99 99 99 ...] */
    {
      int total_pixels[256];

      for (i=0; i<numlabels; i++) {
	total_pixels[i]=0;
	for (j=myceil(breakpoints[i]); j<myceil(breakpoints[i+1]); j++) {
	  regionnums[j] = (int)(lambda[i]+0.5); /* set it to avg grey for region -- rounded to nearest int */
	  total_pixels[i]+=histo[j];
	}
      }
      
      debug("The breaks are:\n");
      for (i=0; i<=numlabels; i++) {
	breaks[i] = ceil(breakpoints[i]);
	if (i>0)
	  debug("%3d  [%3d, %3d]  Range %3d  Ave %6.2f  Sum %d\n", i,
		 myceil(breakpoints[i-1]), myceil(breakpoints[i])-1,
		 myceil(breakpoints[i]) - myceil(breakpoints[i-1]),
		 lambda[i-1], total_pixels[i-1]);
      }
    }

    MT_free((void *) breakpoints);
    MT_free((void *) lambda);
    MT_free((void *) z);

    DEBUG_TRACE_OUT printf("Leaving reduce_grays_optimal\n");
}

void pyramid_filter(int * regionnums) {
  image_matrix_type * image_matrix_ptr;
  int numpics, i, j, k, full_w, full_h, cur_w, cur_h, whichindex;
  unsigned char * tmp_image;
  unsigned char *** pyramid;
  unsigned char *** pyramid_labels;
  int numlevels = 0;
  int min_dim;
  int mask_size = MASK_SIZE;

  DEBUG_TRACE_IN printf("Entering pyramid_filter\n");
  
  image_matrix_ptr = get_image_matrix();
  numpics = image_matrix_ptr->num_pics;

  if (numpics<=0)
  {
      DEBUG_TRACE_OUT printf("Leaving pyramid_filter, numpics<=0\n");
      return;
  }
  

  /* Assume images are square and all the size of the first image -- 
   * and further that width and height are a power of 2
   */
  full_w = image_matrix_ptr->img_arr[0].data_w;
  full_h = image_matrix_ptr->img_arr[0].data_h;

  if (full_w!=full_h)
  {
      DEBUG_TRACE_OUT printf("Leaving pyramid_filter\n");
      return;
  }
  

  switch(full_w)
    {
    case 512:         /* 512, 256, 128, 64, 32, ... stop where appropriate */
      numlevels = 5;
      break;
    case 256:
      numlevels = 4;  /*      256, 128, 64, 32 */
      break;
    case 128:
      numlevels = 3;  /*           128, 64, 32 */
      break;
    default:
      DEBUG_TRACE_OUT printf("Leaving pyramid_filter\n");
      return;
    }

  /* The above was based on a 32x32 minimum.  So, we may need to change
   * this
   */
  switch(MIN_DIM)
    {
    case 8:
      numlevels+=2;
      break;
    case 16:
      numlevels+=1;
      break;
    case 32:
      /* do nothing -- this is the default */
    case 64:
      numlevels-=1;
      break;
    case 128:
      numlevels-=2;
      break;
    case 256:
      numlevels-=3;
      break;
    case 512:
      numlevels-=4;
      break;
    }

  /* If numlevels <= 0 then we're trying to decrease to a size that's
   * already larger than our base size -- not possible
   */
  if (numlevels<=0)
    numlevels=1; /* 1 --> just the base level */

  min_dim = full_w;
  for (i=1; i<numlevels; i++) {
    min_dim/=2;
  }

  pyramid = (unsigned char ***)MT_malloc(numpics*sizeof(unsigned char **));
  pyramid_labels = (unsigned char ***)MT_malloc(numpics*sizeof(unsigned char **));
  debug("Level %d of %d\n", 1, numlevels);
  debug("Be patient, median filtering original images.\n");
  for (i=0; i<numpics; i++) {
    pyramid[i] = (unsigned char **)MT_malloc(numlevels*sizeof(unsigned char *));
    pyramid[i][0] = image_matrix_ptr->img_arr[i].data;
    median_filter(pyramid[i][0], full_w, full_h, mask_size);

    pyramid_labels[i] = (unsigned char **)MT_malloc(numlevels*sizeof(unsigned char *));
  }

  cur_w = full_w/2;
  cur_h = full_h/2;

  for (whichindex=1; whichindex<numlevels; whichindex++) {
    debug("Level %d of %d\n", whichindex+1, numlevels);
    tmp_image = (unsigned char *)MT_malloc(cur_w*cur_h*4*sizeof(unsigned char));
    
    for (i=0; i<image_matrix_ptr->num_pics; i++) {
      memcpy(tmp_image, pyramid[i][whichindex-1], cur_w*cur_h*4*sizeof(unsigned char));
      median_filter(tmp_image, cur_w*2, cur_h*2, mask_size);
      pyramid[i][whichindex] = (unsigned char *)MT_malloc(cur_w*cur_h*sizeof(unsigned char));
      generic_resize_image(tmp_image, pyramid[i][whichindex],
			   cur_w*2, cur_h*2,
			   cur_w, cur_h,
			   1);
    }

    MT_free((void*)tmp_image);
    cur_w/=2;
    cur_h/=2;
  }

  /******************************/
  /* Now, we need to label the top of the pyramid, iteratively update
   * the labels until we reach steady-state, then copy this to the
   * next level, etc. until we've processed the largest level
   */
  cur_w = min_dim;
  cur_h = min_dim;
  for (j=numlevels-1; j>=0; j--) {
    debug("\nBackwards level %d of %d.\n", j+1, numlevels);
    for (i=0; i<numpics; i++) {
      pyramid_labels[i][j] = (unsigned char *)MT_malloc(cur_w*cur_h*sizeof(unsigned char));
      if (j==numlevels-1) {
	for (k=0; k<cur_w*cur_h; k++) {
	  pyramid_labels[i][j][k]=regionnums[pyramid[i][j][k]];
	}
      } else {
	generic_resize_image(pyramid_labels[i][j+1], pyramid_labels[i][j],
			     cur_w/2, cur_h/2,
			     cur_w, cur_h,
			     1);	
      }
    }
    iterative_update(pyramid, pyramid_labels, regionnums, numpics, cur_w, j);
    cur_w*=2;
    cur_h*=2;
  }

  /******************************/

  for (i=0; i<numpics; i++) {
    memcpy(image_matrix_ptr->img_arr[i].data, pyramid_labels[i][0],
	   full_w*full_h*sizeof(unsigned char));
    for (j=1; j<numlevels; j++) {
      MT_free((void*)pyramid[i][j]);
      MT_free((void*)pyramid_labels[i][j]);
    }
    MT_free((void*)pyramid_labels[i][0]);
    MT_free((void*)pyramid[i]);
    MT_free((void*)pyramid_labels[i]);
  }
  MT_free((void*)pyramid);
  MT_free((void*)pyramid_labels);

  DEBUG_TRACE_OUT printf("Leaving pyramid_filter\n");
}

/**************************************************************************
 * refine
 **************************************************************************
 * refine the initial segmentation based on thresholding
 * 
 * Parameters:
 *  image       --> array of pixels for an image
 *  breaks      --> where to break up the regions [0, 32, 45, 256]
 *              -->   0-31  32-44  45-255
 *  numlabels   --> number of labels (or regions) we desire
 *  imagewidth  --> width, in pixels, of image in set
 *  imageheight --> height, in pixels, of image in set
 *  kmax        --> number of iterations, example:  2
 */
void refine(unsigned char *image, int *breaks, int numlabels,
	    int imagewidth, int imageheight, int kmax) {
    int regionnums[256];
    int histo_values[256];
    float averages[256];
    pixel_inf * pixel_arr1;
    int grey_to_label[256];
    float grey_sum_of_labels[256]; /* this could be size numlabels */
    int num_pixels_at_label[256];  /* this could be size numlabels */
    float * G; /* G[i] = avg grey level of label i */
    /* labels will simply be 0 .. (numlabels - 1) */
    int size = imagewidth*imageheight;
    int i, j, k, l, I, J, P;
    float ALPHA = 0.001;  /* coeff. of D */
    float H;               /* basic measure of goodness -- minimize */
    int *randomarray;

    DEBUG_TRACE_IN printf("Entering refine\n");
    
    /* NEW:  NEED TO COMPUTE REGIONNUMS VARIABLE */
    /* First, need histogram */
    for (i=0; i<256; i++)
      histo_values[i]=0;
    for (j=0; j<imagewidth*imageheight; j++) {
      (histo_values[image[j]])++;
    }

    /* Compute average grey levels for each labelled region */
    {
      int tot_pixels_in_region;
      int start, stop;
      j=0;
      do {
	start = breaks[j];
	stop = breaks[j+1];
	if (stop>=255) stop=256;

	tot_pixels_in_region=0;
	for (i=start; i<stop; i++) {
	  tot_pixels_in_region+=histo_values[i];
	}
	averages[j]=0.0;
	for (i=start; i<stop; i++) {
	  averages[j]=averages[j]+((float)(histo_values[i]*i))/((float)tot_pixels_in_region);
	}
	for (i=start; i<stop; i++) {
	  regionnums[i]=(int)(averages[j]+0.5); /* round */
	}
	debug("regionnums[%d] is %d\n", start, regionnums[start]);
	j++;
      } while (stop!=256);
    }
    /*********************************************/

    j = 0; /* the current label */
    grey_to_label[0] = 0;
    for (i=1; i<256; i++) {
	if (regionnums[i] == regionnums[i-1])
	    grey_to_label[i] = j;
	else {
	    j++;
	    grey_to_label[i] = j;
	}
    }

    /*** Mallocing here ***/
    randomarray = (int *) MT_malloc((size_t) size*sizeof(int));
    G = (float *) MT_malloc((size_t)numlabels*sizeof(float));
    pixel_arr1 = (pixel_inf *) MT_malloc((size_t)size*
				      sizeof(pixel_inf));
    /*    pixel_arr2 = (pixel_inf *) malloc((size_t)size*
     *				      sizeof(pixel_inf));
     */

    for (i=0; i<size; i++) {
	pixel_arr1[i].prob_vector =
	    (float *) MT_malloc((size_t)numlabels*sizeof(float));
	/*	pixel_arr2[i].prob_vector =
	 *	    (float *) malloc((size_t)numlabels*sizeof(float));
	 */
    }
    /************************************/

    for (i=0; i<numlabels; i++) {
	grey_sum_of_labels[i]=0.0;
	num_pixels_at_label[i]=0;
    }

    /* initialize pixel_arr1 */
    for (i=0; i<size; i++) {
	randomarray[i] = i;
	/* debug("setting the current label.\n"); */
	pixel_arr1[i].cur_label = grey_to_label[image[i]];
	/* debug("initializing a probability vector.\n"); */
	for (j=0; j<numlabels; j++)
	    pixel_arr1[i].prob_vector[j] = 0.0;
	pixel_arr1[i].prob_vector[grey_to_label[image[i]]] = 1.0;
	/* debug("updating sum and number of pixels at grey level.\n"); */
	num_pixels_at_label[grey_to_label[image[i]]]=
	    num_pixels_at_label[grey_to_label[image[i]]] + 1;
	grey_sum_of_labels[grey_to_label[image[i]]]=
	    grey_sum_of_labels[grey_to_label[image[i]]] + image[i];
    }

    /* initialize G */
    debug("The G's (average grey level for each label) are:\n");
    for (i=0; i<numlabels; i++) {
	G[i] = grey_sum_of_labels[i]/(float)num_pixels_at_label[i];
	debug("  %f\n", G[i]);
    }

    /************************** Actual Stuff Starts Here *********/
    for (k=0; k<kmax; k++) {
	/* randomize random array */
	{
	    int tmp;
	    int tmpi;
	    
	    for (i=size-1; i>0; i--) {
		tmpi = random()%i+1;
		tmp = randomarray[tmpi];
		randomarray[tmpi] = randomarray[i];
		randomarray[i] = tmp;
	    }
	}
	
	/* Calculate R for each pixel */
	for (i=0; i<imagewidth; i++) {
	    int R;
	    for (j=0; j<imageheight; j++) {
		R = 0;
		P = i+imagewidth*j; /* no need here to:  randomarray[i+imagewidth*j];*/
		I = P%imagewidth;
		J = P/imagewidth;
		/* add up number of 4 neighbors w/differing labels */
		
		if (J!=0) {
		    if (pixel_arr1[P].cur_label!=
			pixel_arr1[P-imagewidth].cur_label)
			R++;
		}
		if (J!=imageheight-1) {
		    if (pixel_arr1[P].cur_label!=
			pixel_arr1[P+imagewidth].cur_label)
			R++;
		}
		if (I!=0) {
		    if (pixel_arr1[P].cur_label!=
			pixel_arr1[P-1].cur_label)
			R++;
		}
		if (I!=imagewidth-1) {
		    if (pixel_arr1[P].cur_label!=
			pixel_arr1[P+1].cur_label)
			R++;
		}
		pixel_arr1[P].R = R;
		/*	    if (R>0)
		 *		debug("Differing 4 neighbors[%d][%d] is %d\n",
		 *		       i, j, R);
		 */
	    }
	}
	/* compute (Intensity(x,y) - (AveIntens(Label(x,y))))^2 */
	for (i=0; i<size; i++) {
	    pixel_arr1[i].D = ((float)image[i] - G[pixel_arr1[i].cur_label])*
		((float)image[i] - G[pixel_arr1[i].cur_label]);
	}
	
	/* do some prints */
	/*	for (i=0; i<size; i+=size/20) {
	 *	    debug("i is:  %d\n", i);
	 *	    debug("  Differing 4-neighbors:  %d\n", pixel_arr1[i].R);
	 *	    debug("  Discrepancy:            %f\n", pixel_arr1[i].D);
	 *	}
	 */
	
	H = 0.0;
	for (i=0; i<size; i++) {
	    H += (float)pixel_arr1[i].R + ALPHA * pixel_arr1[i].D;
	}
	
	debug("H is:  %f\n", H);

	/* Calculate R for each pixel */
	for (i=0; i<imagewidth; i++) {
	    int R, D, bestlabel;
	    float besth, curh;
	    for (j=0; j<imageheight; j++) {
		/* try all possible labels */
		P = randomarray[i+imagewidth*j];
		I = P%imagewidth;
		J = P/imagewidth;
		for (l=0; l<numlabels; l++) {
		    R = 0;
		    /* add up number of 4 neighbors w/differing labels */
		    
		    if (J!=0) {
			if (l!=
			    pixel_arr1[P-imagewidth].cur_label)
			    R++;
		    }
		    if (J!=imageheight-1) {
			if (l!=
			    pixel_arr1[P+imagewidth].cur_label)
			    R++;
		    }
		    if (I!=0) {
			if (l!=
			    pixel_arr1[P-1].cur_label)
			    R++;
		    }
		    if (I!=imagewidth-1) {
			if (l!=
			    pixel_arr1[P+1].cur_label)
			    R++;
		    }
		    D = ((float)image[P] - G[l])*
			((float)image[P] - G[l]);
		    curh = (float)R + ALPHA * D;
		    if (l==0) {
			bestlabel = l;
			besth = curh;
		    } else {
			if (curh<besth) {
			    besth = curh;
			    bestlabel = l;
			}
		    }
		}
		num_pixels_at_label[bestlabel] = 
		    num_pixels_at_label[bestlabel]+1;
		grey_sum_of_labels[bestlabel] = grey_sum_of_labels[bestlabel]+
		    image[P];
		num_pixels_at_label[pixel_arr1[P].cur_label] = 
		    num_pixels_at_label[pixel_arr1[P].cur_label]-1;
		grey_sum_of_labels[pixel_arr1[P].cur_label] = 
		    grey_sum_of_labels[pixel_arr1[P].cur_label]
		    -image[P];
		pixel_arr1[P].cur_label = bestlabel;
	    }
	}
	/* could instead update G above each time 1 pixel changes but
	 * that would be very time-consuming and probably not do too much
	 */
	debug("G's:\n");
	for (i=0; i<numlabels; i++) {
	    G[i] = grey_sum_of_labels[i]/(float)num_pixels_at_label[i];
	    debug("  %f\n", G[i]);
	}
    }

    /* change the image */
    for (i=0; i<size; i++) {
	image[i] = G[pixel_arr1[i].cur_label]+0.5;
    }

    /*** Freeing memory here ***/
    MT_free((void*)randomarray);
    G = (float *) MT_malloc((size_t)numlabels*sizeof(float));
    for (i=0; i<size; i++) {
	MT_free((void*)pixel_arr1[i].prob_vector);
    }
    MT_free((void*)pixel_arr1);

    DEBUG_TRACE_OUT printf("Leaving refine\n");
}

void iterative_update(unsigned char *** pyramid,
		      unsigned char *** pyramid_labels,
		      int * regionnums,
		      int numpics, int dim_wh, int whichlevel) {
  int max_iters = 10; /* allow no more iterations than this */
  unsigned char ** new_labels;
  unsigned char *labels_to_try;
  unsigned char bestlabel, thislabel, cur_label;
  int bestlabelcost, currentcost, numlabels;
  int i, j, k, l, dummy, done;
  int label_to_index[256], middle_index, first, last;
  int label_radius=2;

  DEBUG_TRACE_IN printf("Entering iterative_update\n");
  /* Assert pyramid[imagenum][whichlevel] is filtered grey-level image */
  /* pyramid_labels[imagenum][whichlevel] is labelled version of image */
  /* We need to iteratively update all labels at the current level for all
   * numpics images
   */

  numlabels = 1;
  for (i=1; i<256; i++) {
    if (regionnums[i]!=regionnums[i-1])
      numlabels++;
  }
  debug("%d labels found.\n", numlabels);

  labels_to_try = (unsigned char *)MT_malloc(numlabels*sizeof(unsigned char));
  numlabels = 1;
  labels_to_try[0]=regionnums[0];
  label_to_index[regionnums[0]]=0;
  for (i=1; i<256; i++) {
    if (regionnums[i]!=regionnums[i-1]) {
      labels_to_try[numlabels]=regionnums[i];
      label_to_index[regionnums[i]]=numlabels;
      numlabels++;
    }
  }


  debug("Inside iterative_update at level %d.\n", whichlevel+1);
  debug("The size is %dx%d with %d images.\n", dim_wh, dim_wh, numpics);
  
  new_labels = (unsigned char **)MT_malloc(numpics*sizeof(unsigned char *));
  for (i=0; i<numpics; i++) {
    new_labels[i] = (unsigned char *)MT_malloc(dim_wh*dim_wh*sizeof(unsigned char));
    /* This memcpy probably won't be necessary later */
    memcpy(new_labels[i], pyramid_labels[i][whichlevel],
	   dim_wh*dim_wh*sizeof(unsigned char));
  }

  /* Made initial copy, let's iterate */
  debug("Made initial copy.  Let's iterate.\n");

  done = 0;
  for (dummy = 0; dummy<max_iters; dummy++) {
    
    debug("  Iteration %d.\n", dummy+1);
    /* At this point, update each label ****************************/
    for (l=0; l<numpics; l++) {
      for (i=0; i<dim_wh; i++) {
	for (j=0; j<dim_wh; j++) {
	  bestlabelcost = 99999;
	  cur_label=pyramid_labels[l][whichlevel][i*dim_wh+j];
	  middle_index = label_to_index[cur_label];
	  first = middle_index-label_radius;
	  last = middle_index+label_radius;
	  if (first<0) first=0;
	  if (last>=numlabels) last=numlabels-1;
	  for (k=first; k<=last; k++) {
	    thislabel = labels_to_try[k];
	    currentcost = get_cost(pyramid, pyramid_labels, l, numpics,
				   whichlevel, j, i, dim_wh, thislabel);
	    if (currentcost<bestlabelcost) {
	      bestlabelcost=currentcost;
	      bestlabel=thislabel;
	    }
	  }
	  new_labels[l][i*dim_wh+j] = bestlabel;
	}
      }
    }
    /***************************************************************/
    
    done = 1;
    for (i=0; i<numpics; i++) {
      for (j=0; j<dim_wh*dim_wh; j++) {
	if (pyramid_labels[i][whichlevel][j]!=new_labels[i][j]) {
	  done = 0;
	  break;
	}
      }
      if (!done) break;
    }

    /* If we got all the way through without done being set to 0, the
     * original and new labels are all the same and we are done.  (done=1)
     */
    if (done) {
      debug("On level %d, stopped after %d iterations\n", whichlevel+1,
	     dummy+1);
      break;
    }

    for (i=0; i<numpics; i++) {
      memcpy(pyramid_labels[i][whichlevel], new_labels[i],
	     dim_wh*dim_wh*sizeof(unsigned char));
    }
  }

  for (i=0; i<numpics; i++) {
    MT_free((void*)new_labels[i]);
  }
  MT_free((void*)new_labels);

  DEBUG_TRACE_OUT printf("Leaving iterative_update\n");
}

int get_cost(unsigned char *** pyramid, unsigned char *** pyramid_labels, int whichimage, int numpics, int whichlevel, int x, int y, int dim_wh,
	     unsigned char thislabel) {
  unsigned char grey_val;
  int diffneighbors=0, x_try, y_try, z_try, i, z;
  int greydiff_sq;

  DEBUG_TRACE_IN printf("Entering get_cost\n");
  
  grey_val = pyramid[whichimage][whichlevel][y*dim_wh+x];
  greydiff_sq = (int)grey_val-(int)thislabel;
  greydiff_sq*=greydiff_sq;

  z = whichimage;

  /* first 4 are 4-neighbors,
   * next 2 are directly above and below
   * remaining 20 are the rest of the 26-neighbors
   */
  for (i=0; i<26; i++) {
    switch(i)
      {
      case 0:
	x_try = x-1;
	y_try = y;
	z_try = z;
	break;
      case 1:
	x_try = x+1;
	y_try = y;
	z_try = z;
	break;
      case 2:
	x_try = x;
	y_try = y-1;
	z_try = z;
	break;
      case 3:
	x_try = x;
	y_try = y+1;
	z_try = z;
	break;
      case 4:
	x_try = x;
	y_try = y;
	z_try = z-1;
	break;
      case 5:
	x_try = x;
	y_try = y;
	z_try = z+1;
	break;
      case 6:
	x_try = x-1;
	y_try = y;
	z_try = z-1;
	break;
      case 7:
	x_try = x-1;
	y_try = y;
	z_try = z+1;
	break;
      case 8:
	x_try = x+1;
	y_try = y;
	z_try = z-1;
	break;
      case 9:
	x_try = x+1;
	y_try = y;
	z_try = z+1;
	break;
      case 10:
	x_try = x;
	y_try = y-1;
	z_try = z-1;
	break;
      case 11:
	x_try = x;
	y_try = y-1;
	z_try = z+1;
	break;
      case 12:
	x_try = x;
	y_try = y+1;
	z_try = z-1;
	break;
      case 13:
	x_try = x;
	y_try = y+1;
	z_try = z+1;
	break;
      case 14:
	x_try = x-1;
	y_try = y-1;
	z_try = z-1;
	break;
      case 15:
	x_try = x-1;
	y_try = y-1;
	z_try = z;
	break;
      case 16:
	x_try = x-1;
	y_try = y-1;
	z_try = z+1;
	break;
      case 17:
	x_try = x-1;
	y_try = y+1;
	z_try = z-1;
	break;
      case 18:
	x_try = x-1;
	y_try = y+1;
	z_try = z;
	break;
      case 19:
	x_try = x-1;
	y_try = y+1;
	z_try = z+1;
	break;
      case 20:
	x_try = x+1;
	y_try = y+1;
	z_try = z-1;
	break;
      case 21:
	x_try = x+1;
	y_try = y+1;
	z_try = z;
	break;
      case 22:
	x_try = x+1;
	y_try = y+1;
	z_try = z+1;
	break;
      case 23:
	x_try = x+1;
	y_try = y-1;
	z_try = z-1;
	break;
      case 24:
	x_try = x+1;
	y_try = y-1;
	z_try = z;
	break;
      case 25:
	x_try = x+1;
	y_try = y-1;
	z_try = z+1;
	break;
      }
    if ((x_try>=0)&&(y_try>=0)&&(x_try<dim_wh)&&(y_try<dim_wh)&&
	(z_try>=0)&&(z_try<numpics)) {
      if (pyramid_labels[z][whichlevel][y_try*dim_wh+x_try]
	  != thislabel) diffneighbors++;
    }
  }
  DEBUG_TRACE_OUT printf("Leaving get_cost\n");
  return(greydiff_sq+diffneighbors*16);
}

void rotate_images_FCN(image_matrix_type * image_matrix_ptr, int degrees) {
  int i, numpics, tmp;
  img_arr_type * iap;
  undo_type * undo_ptr;
  unsigned char * undo_data;

  DEBUG_TRACE_IN printf("Entering rotate_images_FCN\n");
  
  if ((degrees!=90)&&(degrees!=180))
  {
      DEBUG_TRACE_OUT printf("Leaving rotate_images_FCN\n");
      return;
  }
  
  numpics = image_matrix_ptr->num_pics;

  if (numpics<=0)
  {
      DEBUG_TRACE_OUT printf("Leaving rotate_images_FCN\n");
      return;
  }
  

  /* Get rid of the single image edit window if up */
  edit_single_image_FCN(get_image_matrix(), -1);
  /*************************************************/

  for (i=0; i<numpics; i++) {
    iap = &(image_matrix_ptr->img_arr[i]);
    rotate_easy(iap->data, iap->data_w, iap->data_h, degrees);
    rotate_easy(iap->pimage_data, iap->data_w, iap->data_h, degrees);
    rotate_easy(iap->region_data, iap->data_w, iap->data_h, degrees);
    undo_ptr = image_matrix_ptr->img_arr[i].undo_list;
    while (undo_ptr) {
      if (undo_data = get_undo_data(undo_ptr)) {
	rotate_easy(undo_data, undo_ptr->width, undo_ptr->height,
		    degrees);
	if (degrees==90) {
	  tmp = undo_ptr->width;
	  undo_ptr->height = undo_ptr->width;
	  undo_ptr->width = tmp;
	}
	compress_undo_data(undo_ptr);
      }
      undo_ptr = undo_ptr->next;
    }
    if (degrees==90) {
      tmp = iap->data_h;
      iap->data_h = iap->data_w;
      iap->data_w = tmp;
      tmp = iap->prev_h;
      iap->prev_h = iap->prev_w;
      iap->prev_w = tmp;
    }
    clear_and_draw_image(image_matrix_ptr, i);
  }
  DEBUG_TRACE_OUT printf("Leaving rotate_images_FCN\n");
}

void rotate_easy(unsigned char * data, int w, int h, int deg) {
  unsigned char * copy_data;
  int i, j, k, new_w, new_h, size, pos;

  DEBUG_TRACE_IN printf("Entering rotate_easy\n");
  
  size = w*h;
  copy_data = (unsigned char *) MT_malloc(size*sizeof(unsigned char));

  memcpy(copy_data, data, w*h*sizeof(unsigned char));

  switch(deg)
    {
    case 90:
      new_w = h;
      new_h = w;
      k = 0;
      for (i=0; i<new_h; i++) {
	pos = w-1-i;
	for (j=0; j<new_w; j++) {
	  data[k++] = copy_data[pos];
	  pos += w;
	}
      }
      break;
    case 180:
      j = size;
      for(i=0; i<size; i++) {
	j--;
	data[i]=copy_data[j];
      }
      break;
    }

  MT_free((void*)copy_data);
  DEBUG_TRACE_OUT printf("Leaving rotate_images_FCN\n");
}

void rotate_images_CB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf("Entering rotate_images_CB\n");
  
  if (!is_allowed_callback(CB_MENU_ROTATE_IMAGES))
  {
      DEBUG_TRACE_OUT printf("Leaving rotate_images_CB\n");
      return;
  }
  
  image_matrix_ptr = get_image_matrix();

  switch((int)ClientData) {
  case 1:
    rotate_images_FCN(image_matrix_ptr, 90);
    break;
  case 2:
    rotate_images_FCN(image_matrix_ptr, 180);
    break;
  }
  DEBUG_TRACE_OUT printf("Leaving rotate_images_CB\n");
}
