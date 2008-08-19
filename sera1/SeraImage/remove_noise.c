/******************************************************************************
 * remove_noise.c                                                             *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Matt Cohen 3/9/99                                                          *
 *****************************************************************************/
#include "toqsh.h"

#define MAX_NOISE_SIZE 100

typedef struct _pixel_t
{
    unsigned char value;
    int checked;
    int noise_num;
    int is_noise;
}
pixel_t;

/* Local Function Prototypes */
int noise_should_be_kept ( unsigned char *image, pixel_t *pixels, int x, int y, int width, int heigth, int noise_num );
int remove_threshold_from_region ( unsigned char *b_image, int width, int height );
void reverse_binary_image ( unsigned char *b_image, int width, int height );
int dilate_threshold_areas             ( unsigned char **, int, int, int );
int erode_threshold_areas              ( unsigned char **, int, int, int );
void rebuild_image_overwritten         ( unsigned char *, unsigned char *, 
				         int, int, unsigned char );
int count_pixels_in_noise     ( unsigned char *, pixel_t *,
					 int, int, int, int, int );
void eliminate_binary_noise            ( unsigned char *, int, int, int, int );
void build_binary_image_by_threshold   ( unsigned char *, unsigned char *, 
					 int, int, int, int, unsigned char );

void noise_threshold_scale_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    XmScaleCallbackStruct *cbs = ( XmScaleCallbackStruct * ) callData;

    DEBUG_TRACE_IN printf ( "Entering noise_threshold_scale_CB\n" );

    fill_colormap_with_threshold ( gui, 0, scaled_threshold ( cbs->value ) ) ;
    gui->manip_gui.threshold = cbs->value;

    refresh_manip_image ( gui );
    refresh_images_in_image_block ( gui );

    DEBUG_TRACE_OUT printf ( "Leaving noise_threshold_scale_CB\n" );
}

int scaled_threshold ( int scale_val )
{
    static int range = MAX_GRAY - MIN_GRAY;     /* Number of gray values used */
    static float spread;

    spread = (float)range/256.0;

    return ( (int) ( (float)scale_val*spread ) );
}


void fill_colormap_with_threshold ( main_gui_t *gui, int low_thresh, int high_thresh  )
{
    XColor new_color;
    int i, temp;
    static int range = MAX_GRAY-MIN_GRAY+1;
    static float scale;
    static float spread;

    DEBUG_TRACE_IN printf ( "Entering fill_colormap_with_threshold.\n" );

    scale = (float)65535.0/(float)range;
    spread = (float)256.0/(float)range;

    low_thresh  += MIN_GRAY;
    high_thresh += MIN_GRAY;

    colormap_load_rgb ( gui );

    for ( i = MIN_GRAY; i <= MAX_GRAY; i++ )
    {
        new_color.pixel = i;

        if ( i >= low_thresh && i <= high_thresh )
	{  
	    temp = (i-MIN_GRAY+(range/2)) * spread * 255;
	    if ( temp > 65535 )
	        new_color.green = 65535;
	    else
	        new_color.green = ( unsigned short ) ( temp );

	    new_color.red  = 0;
	    new_color.blue = 0;

	    new_color.flags = DoRed | DoGreen | DoBlue;
	    myXStoreColor ( gui, gui->color_info.cmap, &new_color );
	}
    }

    DEBUG_TRACE_OUT printf ( "Leaving fill_colormap_with_threshold.\n" );
}


void preview_noise_removal_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;
    char *str;
    unsigned char *image, *binary_image;
    int width, height;

    DEBUG_TRACE_IN printf ( "Entering preview_noise_removal_CB\n" );

    /* Rebuild the manipulation image... just in case the user previews 
       multiple times without clearing the previous preview image. */
    generic_redraw_manip_image ( gui );

    image  = ( unsigned char * ) gui->manip_gui.image->data;
    width  = gui->manip_gui.image->width;
    height = gui->manip_gui.image->height;

    apply_noise_removal_to_image ( gui, image, width, height, (int)RESERVED_BLUE,
				   scaled_threshold ( gui->manip_gui.threshold ), (int)MIN_GRAY );

    refresh_manip_image ( gui );

    DEBUG_TRACE_OUT printf ( "Leaving preview_noise_removal_CB\n" );
}


void apply_noise_removal_to_image_block ( main_gui_t *gui )
{
    int width  = gui->qsh_gui->qsh_info->size_of_dimension[1];
    int height = gui->qsh_gui->qsh_info->size_of_dimension[2];
    unsigned char *image;
    int i;

    DEBUG_TRACE_IN printf ( "Entering apply_noise_removal_to_image_block\n" );

    image = ( unsigned char * ) MT_malloc ( width * height );

    if ( gui->manip_gui.apply_to_all )
    {
	for ( i = 0; i < gui->image_block.num_images; i++ )
        {
	    memcpy ( image, &gui->qsh_gui->qsh_info->images[i*width*height], width*height );
	    apply_noise_removal_to_image ( gui, image, width, height, 0, gui->manip_gui.threshold, 0 );
	    memcpy ( &gui->qsh_gui->qsh_info->images[i*width*height], image, width*height );
	}
    }
    else
    {
        memcpy ( image, &gui->qsh_gui->qsh_info->images[width*height*gui->manip_gui.current_image], width*height );
	apply_noise_removal_to_image ( gui, image, width, height, 0, gui->manip_gui.threshold, 0 );
        memcpy ( &gui->qsh_gui->qsh_info->images[width*height*gui->manip_gui.current_image], image, width*height );
    }

    MT_free ( (void *) image );

    DEBUG_TRACE_OUT printf ( "Leaving apply_noise_removal_to_image_block\n" );
}

void apply_noise_removal_to_image ( main_gui_t *gui, unsigned char *image, int width, int height, 
				    int fill_val, int thresh_val, int min_gray )
{
    unsigned char *binary_image;

    DEBUG_TRACE_IN printf ( "Entering apply_noise_removal_to_image\n" );

    binary_image = ( unsigned char * ) MT_malloc ( width*height );

    /* Build a binary image based on the threshold value */
    build_binary_image_by_threshold ( image, binary_image, width, height, 
				      0, thresh_val, (unsigned char) min_gray );

    /* Apply closure to the threshold by dilating and then eroding */
    dilate_threshold_areas ( &binary_image, width, height, 5 );
    erode_threshold_areas ( &binary_image, width, height, 5 );

    /* Remove noise in the threshold areas */
    eliminate_binary_noise ( binary_image, width, height, MAX_NOISE_SIZE, 1 );

    /* Remove threshold from the region we want to keep */
    remove_threshold_from_region ( binary_image, width, height );

    /* Rebuild the image overwritting the threshold with the specified value */
    rebuild_image_overwritten ( image, binary_image, width, height, (unsigned char ) fill_val );

    MT_free ( (void *) binary_image );

    DEBUG_TRACE_OUT printf ( "Leaving apply_noise_removal_to_image\n" );
}


void refresh_manip_image_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    main_gui_t *gui = ( main_gui_t * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering refresh_manip_image_CB\n" );
    generic_redraw_manip_image ( gui );
    DEBUG_TRACE_OUT printf ( "Leaving refresh_manip_image_CB\n" );
}


/*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  GENERIC FUNCTIONS
/*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
void rebuild_image_overwritten ( unsigned char *image, unsigned char *b_image, 
				 int width, int height, unsigned char pix_val )
{
    int i, j;

    for ( i = 0; i < height; i++ )
    {
        for ( j = 0; j < width; j++ )
	{
	    if ( b_image[i*width+j] )
	        image[i*width+j] = pix_val;
	}
    }
}


int count_pixels_in_noise ( unsigned char *image, pixel_t *pixels, int x, int y, 
			    int width, int height, int noise_index )
{
    int count = 0;

    if ( image[y*width+x] && !pixels[y*width+x].checked )
    {
	pixels[y*width+x].checked = 1;
	pixels[y*width+x].is_noise = 1;
	pixels[y*width+x].noise_num = noise_index;
	
	count++;
	if ( x > 0 )
	    count += count_pixels_in_noise ( image, pixels, x - 1, y, width, height, noise_index );
	if ( x < width - 1 )
	    count += count_pixels_in_noise ( image, pixels, x + 1, y, width, height, noise_index );
	if ( y > 0 )
	    count += count_pixels_in_noise ( image, pixels, x, y - 1, width, height, noise_index );
	if ( y < height - 1 )
	    count += count_pixels_in_noise ( image, pixels, x, y + 1, width, height, noise_index );
    }

    return ( count );
}


void reverse_binary_image ( unsigned char *b_image, int width, int height )
{
    int x, y;

    for ( y = 0; y < height; y++ )
    {
	for ( x = 0; x < width; x++ )
        {
	    if ( b_image[y*width+x] )
	        b_image[y*width+x] = 0;
	    else
	        b_image[y*width+x] = 1;
	}
    }
}


int remove_threshold_from_region ( unsigned char *b_image, int width, int height )
{
    int x, y, i;
    int *noises;
    int *remove_noise;
    int *checked_noise;
    pixel_t *pixels;
    int noise_index = 0;

    /* Get some memory */
    noises = ( int * ) MT_malloc ( sizeof(int)*width*height/2 );

    remove_noise = ( int * ) MT_malloc ( sizeof(int)*width*height/2 );

    checked_noise = ( int * ) MT_malloc ( sizeof(int)*width*height/2 );

    pixels = ( pixel_t * ) MT_malloc ( sizeof ( pixel_t ) * width * height );

    /* Initialize noise_removal flags */
    for ( i = 0; i < width*height/2; i++ )
    {
        remove_noise[i] = 0;
	checked_noise[i] = 0;
    }

    /* Initialze pixel array */
    for ( y = 0; y < height; y++ )
    {
	for ( x = 0; x < width; x++ )
	{
	    pixels[y*width+x].checked = 0;
	    pixels[y*width+x].noise_num = 0;
	    pixels[y*width+x].is_noise = 0;
        }
    }

    /* Find all chunks of threshold and count there sizes */
    /* The counting isn't really necessary here */
    for ( y = 0; y < height; y++ )
    {
	for ( x = 0; x < width; x++ )
        {
	    if ( b_image[y*width+x] && !pixels[y*width+x].checked )
            {
		noises[noise_index] 
		    = count_pixels_in_noise ( b_image, pixels, x, y, width, height, noise_index );
		noise_index++;	    
	    }
	}
    }

    /* Re-initialize the checked variable in the pixel array to zero */
    for ( y = 0; y < height; y++ )
    {
	for ( x = 0; x < width; x++ )
	{
	    pixels[y*width+x].checked = 0;
	}
    }

    /* Check if each chunk of threshold should be removed */
    for ( y = 0; y < height; y++ )
    {
	for ( x = 0; x < width; x++ )
        {
	    if ( pixels[y*width+x].is_noise && !pixels[y*width+x].checked
		 && !checked_noise[pixels[y*width+x].noise_num] )
            {
	        if ( !noise_should_be_kept ( b_image, pixels, x, y, width, height, pixels[y*width+x].noise_num ) )
		  {
		    remove_noise[pixels[y*width+x].noise_num] = 1;
		  }
		checked_noise[pixels[y*width+x].noise_num] = 1;
	    }
	}
    }

    /* Now clean out the necessary chucks of threshold */
    for ( y = 0; y < height; y++ )
    {
	for ( x = 0; x < width; x++ )
        {
	    if ( remove_noise[pixels[y*width+x].noise_num] )
            {
		b_image[y*width+x] = 0;
	    }
	}
    }

    /* De-allocate used memory */
    MT_free ( (void *) remove_noise );
    MT_free ( (void *) pixels );
    MT_free ( (void *) noises );

    return ( 1 ); /* Success! */
}


int noise_should_be_kept ( unsigned char *image, pixel_t *pixels, int x, int y, int width, int height, int noise_num ) 
{
    int retval = 0;

    pixels[y*width+x].checked = 1;

    if ( x <= 0 || x >= width-1 || y <= 0 || y >= height-1 )
    {
        retval = 1;
    }    
    else
    {
        if ( pixels[y*width+(x-1)].noise_num == noise_num && !pixels[y*width+(x-1)].checked )
	    retval += noise_should_be_kept ( image, pixels, x-1, y, width, height, noise_num );
	if ( pixels[y*width+(x+1)].noise_num == noise_num && !pixels[y*width+(x+1)].checked )
	    retval += noise_should_be_kept ( image, pixels, x+1, y, width, height, noise_num );
	if ( pixels[(y-1)*width+x].noise_num == noise_num && !pixels[(y-1)*width+x].checked ) 
	    retval += noise_should_be_kept ( image, pixels, x, y-1, width, height, noise_num );
	if ( pixels[(y+1)*width+x].noise_num == noise_num && !pixels[(y+1)*width+x].checked )
	    retval += noise_should_be_kept ( image, pixels, x, y+1, width, height, noise_num );
    }
    
    return ( retval );
}

void eliminate_binary_noise ( unsigned char *image, int width, int height,
			      int max_size, int clean_positive  )
{
    int x, y;
    int *noises;
    pixel_t *pixels;
    int noise_index = 0;

    noises = ( int * ) MT_malloc ( sizeof(int)*width*height/2 );
    pixels = ( pixel_t * ) MT_malloc ( sizeof ( pixel_t ) * width * height );

    for ( y = 0; y < height; y++ )
      {
	for ( x = 0; x < width; x++ )
	  {
	    pixels[y*width+x].checked = 0;
	    pixels[y*width+x].noise_num = 0;
	    pixels[y*width+x].is_noise = 0;
	  }
      }

    if ( clean_positive )
      {
	reverse_binary_image ( image, width, height );

	for ( y = 0; y < height; y++ )
	  {
	    for ( x = 0; x < width; x++ )
	      {
		if ( image[y*width+x] && !pixels[y*width+x].checked )
		  {
		    noises[noise_index] 
		        = count_pixels_in_noise ( image, pixels, x, y, width, height, noise_index );
		    noise_index++;	    
		  }
	      }
	  }

	reverse_binary_image ( image, width, height );
      }
    else
     {
	for ( y = 0; y < height; y++ )
	  {
	    for ( x = 0; x < width; x++ )
	      {
		if ( image[y*width+x] && !pixels[y*width+x].checked )
		  {
		    noises[noise_index] 
		        = count_pixels_in_noise ( image, pixels, x, y, width, height, noise_index );
		    noise_index++;	    
		  }
	      }
	  }
     }

    /* Now clean out the noises */
    for ( y = 0; y < height; y++ )
    {
	for ( x = 0; x < width; x++ )
        {
	    if ( pixels[y*width+x].is_noise )
            {
		if ( noises[pixels[y*width+x].noise_num] <= max_size )
                {
	            if ( clean_positive )
	            {
			image[y*width+x] = 1;
		    }
		    else
		    {
			image[y*width+x] = 0;
		    }
		}
	    }
	}
    }

    MT_free ( (void *) pixels );
    MT_free ( (void *) noises );
}


void build_binary_image_by_threshold ( unsigned char *image, unsigned char *b_image, 
				       int width, int height, int low, int high, unsigned char min_gray )
{
    int i, j;

    /* initialize binary_image */
    memset ( b_image, 0, width*height );

    for ( i = 0; i < height; i++ )
    {
	for ( j = 0; j < width; j++ )
	{
	    /* If pixel is in the threshold range, set to 1 */
	    if ( image[i*width+j] - min_gray >= low && image[i*width+j] - min_gray <= high )
	        b_image[i*width+j] = 1;
	}
    }
}

int dilate_threshold_areas ( unsigned char **image_ptr, int width, int height, int element )
{
    int i, j;
    unsigned char *new_image, *b_image;
    unsigned char val;

    b_image = *image_ptr;

    new_image = ( unsigned char * ) MT_malloc ( width * height );

    /* initialize new image */
    memset ( new_image, 0, width*height );

    for ( i = 0; i < height; i++ )
    {
	for ( j = 0; j < width; j++ )
	{
	    val = 0;
	    if ( b_image[i*width+j] )
	    {
	        if ( j > 0 )
		    val += b_image[i*width+(j-1)];
		if ( j+1 < width )
		    val += b_image[i*width+(j+1)];
		if ( i > 0 )
		    val += b_image[(i-1)*width+j];
		if ( i+1 < height )
		    val += b_image[(i+1)*width+j];

		if ( element >= 5 )
		{
		    if ( j > 1 )
		        val += b_image[i*width+(j-2)];
		    if ( j+2 < width )
		        val += b_image[i*width+(j+2)];
		    if ( i > 1 )
		        val += b_image[(i-2)*width+j];
		    if ( i+2 < height )
		        val += b_image[(i+2)*width+j];
		    
		    if ( j > 0 && i > 0 )
		      val += b_image[(i-1)*width+(j-1)];
		    if ( j+1 < width && i > 0 )
		      val += b_image[(i-1)*width+(j+1)];
		    if ( j > 0 && i+1 < height )
		      val += b_image[(i+1)*width+(j-1)];
		    if ( j+1 < width && i+1 < height )
		      val += b_image[(i+1)*width+(j+1)];
		}

		if ( val )
		{
		    if ( j > 0 )
		        new_image[i*width+(j-1)] = 1;
		    if ( j+1 < width )
		        new_image[i*width+(j+1)] = 1;
		    if ( i > 0 )
		        new_image[(i-1)*width+j] = 1;
		    if ( i+1 < height )
		        new_image[(i+1)*width+j] = 1;

		    if ( element >= 5 )
	            {
			if ( j > 1 )
			    new_image[i*width+(j-2)] = 1;
			if ( j+2 < width )
			    new_image[i*width+(j+2)] = 1;
			if ( i > 1 )
			    new_image[(i-2)*width+j] = 1;
			if ( i+2 < height )
			    new_image[(i+2)*width+j] = 1;
			
			if ( j > 0 && i > 0 )
			    new_image[(i-1)*width+(j-1)] = 1;
			if ( j+1 < width && i > 0 )
			    new_image[(i-1)*width+(j+1)] = 1;
			if ( j > 0 && i+1 < height )
			    new_image[(i+1)*width+(j-1)] = 1;
			if ( j+1 < width && i+1 < height )
			    new_image[(i+1)*width+(j+1)] = 1;
		    }

		}

		new_image[i*width+j] = 1;
	    }
	}
    }

    MT_free ( (void *) *image_ptr );
    *image_ptr = new_image;

    return ( 1 );
}


int erode_threshold_areas ( unsigned char **image_ptr, int width, int height, int element )
{
    int i, j;
    unsigned char *new_image, *b_image;
    unsigned char val;

    b_image = *image_ptr;

    new_image = ( unsigned char * ) MT_malloc ( width * height );

    /* initialize new image */
    memset ( new_image, 0, width*height );

    for ( i = 0; i < height; i++ )
    {
	for ( j = 0; j < width; j++ )
	{
	    val = 0;
	    if ( b_image[i*width+j] )
	    {
	        if ( j > 0 )
		    if ( !b_image[i*width+(j-1)] )
		        val++;
		if ( j+1 < width )
		    if ( !b_image[i*width+(j+1)] )
		        val++;
		if ( i > 0 )
		    if ( !b_image[(i-1)*width+j] )
		        val++;
		if ( i+1 < height )
		    if ( !b_image[(i+1)*width+j] )
		        val++;

		if ( element >= 5 && !val )
		{
		    if ( j > 1 )
		        if ( !b_image[i*width+(j-2)] )
			    val++;
		    if ( j+2 < width )
		        if ( !b_image[i*width+(j+2)] )
			    val++;
		    if ( i > 1 )
		        if ( !b_image[(i-2)*width+j] )
			    val++;
		    if ( i+2 < height )
		        if ( !b_image[(i+2)*width+j] )
			    val++;

		    if ( j > 0 && i > 0 )
		        if ( !b_image[(i-1)*width+(j-1)] )
			    val++;
		    if ( j+1 < width && i > 0 )
		        if ( !b_image[(i-1)*width+(j+1)] )
			    val++;
		    if ( j > 0 && i+1 < height )
		        if ( !b_image[(i+1)*width+(j-0)] )
			    val++;
		    if ( j+1 < width && i+1 < height )
		        if ( !b_image[(i+1)*width+(j+1)] )
			    val++;
		}

		if ( val )
		{
		    new_image[i*width+j] = 0;
		}
		else
		    new_image[i*width+j] = 1;
	    }
	}
    }

    MT_free ( (void *) *image_ptr );
    *image_ptr = new_image;

    return ( 1 );
}
