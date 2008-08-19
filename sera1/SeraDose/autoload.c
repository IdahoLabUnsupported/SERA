/*
 * BNCT
 *
 * David Helzer - March, 1998
 *
 * autoload.c
 * This file provides routines for the autoloader used to load
 * all images, dosages, and masks in specified directories
 *
 * PROBLEM:  Currently, files are loaded in the alphabetical order in
 * which they appear in the directories specified.  They should be 
 * loaded based on increasing (or decreasing ?) z-values.
 */

#include "global.h"
#include "autoload.h"
#include "color.h"
#include "picshell.h"
#include "read_raw.h"
#include "filenames.h"

float first_image_z_value;
float first_dose_z_value;
float first_mask_z_value;
float dose_scale_factor;
float mask_scale_factor;


char ** images;     /* List of image files */
int number_images;  /* Number of image files */
char ** dosages;    /* List of dose files */
int number_dosages; /* Number of dose files */
char ** masks;      /* List of mask files */
int number_masks;   /* Number of mask files */


/*
  Repeat after me...
  I Love doseplay.  Doseplay is my friend.

      Ok, now let's dig in.  I'm making some changes to the autoloader.
      Prior to these changes doseplay worked as follows:  You load an image,
      then, you loaded a dose, and finally a mask.  Basically, the loading
      of the dose and mask was to tell doseplay what directory they resided.
      Now doseplay just took the image name like SOMEIMAGE.-0.12.SI and looked
      for SOMEIMAGE.-0.12.SI,contour and SOMIMAGE.-0.12.SI,mask.
     
      Well, this is nice if the images and contours and mask have exactly the
      same names.  But with the way seraImage is now outputting raw images,
      this is not the case.
     
      Here's how I plan to make the autoloader work now:
      1.  Get the image name and extract it's z-value
      2.  Get the contour file and extract it's z-value
      3.  Get the mask file and extract it's z-value
      4.  Determine the units of the various z-values.  For example, if
      the z-value of the image is 4.50 and the z-value of the
      contour and mask files are 45.0 then the autoloader will know
      scale the image z-values by a factor of 10 (from cm to mm).
      5.  Load all corresponding image, contour, and mask files found.
*/


void auto_loader_callback (Widget w, XtPointer clientData, XtPointer callData)
{
    int proceed;
    
    DEBUG_TRACE_IN printf( "Entering auto_loader_callback\n" );

    /*
     * Get the image directory and filename.
     */
    if( getDirAndFilename( "Select Image File", image_directory, image_name, ",raw" ) )
        proceed = 1;
    else
        proceed = 0;

    /*
     * Get the dose directory and filename.
     */
    if( proceed )
    {
        if( getDirAndFilename( "Select Dose File", dose_directory, dose_name, ",contour" ) )
            proceed = 1;
        else
            proceed = 0;
    }

    /*
     * Get the mask directory and filename.
     */
    if( proceed )
    {
        if( getDirAndFilename( "Select Mask File", mask_directory, mask_name, ",mask" ) )
            proceed = 1;
        else
            proceed = 0;
    }

    if( proceed )
    {
        first_image_z_value = get_image_z_value_from_filename ( image_name );
        DEBUG_LOADING printf ( "First Image Z Value: %f\n", first_image_z_value );

        first_dose_z_value = get_dose_or_mask_z_value_from_filename ( dose_name );
        DEBUG_LOADING printf ( "First Dose Z Value: %f\n", first_dose_z_value );

        first_mask_z_value = get_dose_or_mask_z_value_from_filename ( mask_name );
        DEBUG_LOADING printf ( "First Mask Z Value: %f\n", first_mask_z_value );        

        if( DT_decide( xcontoursTopLevelShell, context, LOAD_ALL_MESSAGE, "Load All?", "Yes", "No" ) )
            load_all_images_callback( NULL, NULL, NULL );
        else
            load_one_image_callback( NULL, NULL, NULL );
    }
    
    DEBUG_TRACE_OUT printf( "Leaving auto_loader_callback\n" );
}

void load_all_images_callback (Widget w, XtPointer clientData, XtPointer callData)
{
    DEBUG_TRACE_IN printf( "Entering load_all_images_callback\n" );

    /*
     * No images, dosages, or masks have been loaded.
     */
    number_images = 0;
    number_dosages = 0; 
    number_masks = 0;
    
    /* Get the scale values */
    dose_scale_factor = get_autoload_scale_factor ( fabs ( first_image_z_value/first_dose_z_value) );
    DEBUG_LOADING printf ( "Dose Scale Factor: %f\n", dose_scale_factor );
    
    mask_scale_factor = get_autoload_scale_factor ( fabs ( first_image_z_value/first_mask_z_value ) );
    DEBUG_LOADING printf ( "Mask Scale Factor: %f\n", mask_scale_factor );

    /*
     * Get all the ,raw ,contour and ,mask in the specified directories.
     */
    
    get_filenames2( image_directory, ",raw",     &images,  &number_images  );
    get_filenames2( dose_directory,  ",contour", &dosages, &number_dosages );
    get_filenames2( dose_directory,  ",mask",    &masks,   &number_masks   );
    
    display_images_dosages_masks ();

    /* Free all the filenames */
    free_filenames( images,  number_images  );
    free_filenames( dosages, number_dosages );
    free_filenames( masks,   number_masks   );
    

    DEBUG_TRACE_OUT printf( "Leaving load_all_images_callback\n" );
}


float get_autoload_scale_factor ( float val )
{
    float retval;

    DEBUG_TRACE_IN printf( "Entering get_scale_value\n" );
    
    if ( val < 0.005 )
    {
        retval = 0.001;
    }
    else if (val >= 0.005 && val < 0.05 )
    {
        retval = 0.01;
    }
    else if ( val >= 0.05 && val < 0.5 )
    {
        retval = 0.1;
    }
    else if ( val >= 0.5 && val < 5.0 )
    {
        retval = 1.0;
    }
    else if ( val >= 5.0 && val < 50.0 )
    {
        retval = 10.0;
    }
    else
    {
        retval = 100.0;
    }

    DEBUG_TRACE_OUT printf( "Leaving get_scale_value\n" );
    return ( retval );
}


void load_one_image_callback (Widget w, XtPointer clientData, XtPointer callData)
{
    DEBUG_TRACE_IN printf( "Entering load_one_images_callback\n" );

    MT_free ((void *)image_file_name);
    image_file_name = (char *) MT_malloc (strlen (image_name) + 1);
    strcpy (image_file_name, image_name);

    autoload_image (image_name);
    autoload_dose (dose_name);
    autoload_mask (mask_name);

    DEBUG_TRACE_OUT printf( "Leaving load_one_images_callback\n" );
}


void autoload_image (char *filename_to_load)
{
    int num_images;

    DEBUG_TRACE_IN printf( "Entering autoload_image\n" );

    /*
     * Load the image 
     */

    if ((image_file = fopen (filename_to_load, "r")) == NULL)
    {
        printf ("Unable to open file %s!\n", filename_to_load);
        XBell (di, 100);
    }

    else
    {
        num_images = image_matrix.num_pics;

        if (!(image_matrix.img_arr[num_images].fname_data = 
              (char *)MT_malloc(strlen(filename_to_load) + 1)))
        {
            printf ("Error allocating memory for image\n");
            exit (13);
        }
        strcpy (image_matrix.img_arr[num_images].fname_data, filename_to_load);

        if (image_matrix.img_arr[num_images].raw_data = read_raw (filename_to_load, &qhd_data,
                                                                  num_images))
        {
            qsh_PIC_init(num_images);
            toggle_on (num_images, 1);     
        }

        /* Set the last picture to black... */
        if (num_images > 0)
            XtVaSetValues (image_matrix.img_arr[num_images-1].draw_area,
                           XtVaTypedArg, XmNborderColor, XtRString, "black", 6,
                           NULL);

        /* ...and set the current one to red */
        XtVaSetValues (image_matrix.img_arr[num_images].draw_area,
                       XtVaTypedArg, XmNborderColor, XtRString, "red", 4,
                       NULL);
    }

    DEBUG_TRACE_OUT printf( "Leaving autoload_image\n" );
}



void autoload_dose (char *filename_to_load)
{
    float z_value;

    DEBUG_TRACE_IN printf( "Entering autoload_dose\n" );

    if ((dose_file = fopen (filename_to_load, "r")) == NULL)
    {
        printf ("Unable to open dose file %s\n", filename_to_load);
        XBell (di, 100);
    }
    else
    {
        fclose(dose_file);
        /* Check to see if file name is "legal" */
        if (!is_dose_file (filename_to_load))
        {
            if (!confirm_popup ("The file you have selected does not appear to be a valid dose file.\n"))
            {
                XtManageChild (fileSelectionBox);
                DEBUG_TRACE_OUT printf( "Leaving autoload_dose\n" );
                return;
            }
        }

        load_dose (filename_to_load, &dose_data, NULL, &z_value);
        dosage_is_there = TRUE;
        addto_dosage_list (filename_to_load, image_matrix.active_picture, z_value);

        contours_are_current = FALSE;
        load_imageEH (mainWindowDrawingArea, 0, &SureEvent);
    }

    DEBUG_TRACE_OUT printf( "Leaving autoload_dose\n" );
}


void autoload_mask (char *filename_to_load)
{
    FILE *mask_file;
    int jj, kk;
    Boolean masks_replace_images;
        
    DEBUG_TRACE_IN printf( "Entering autoload_mask\n" );

    if ((mask_file = fopen (filename_to_load,"r")) == NULL) 
    {
        printf ("Unable to open file %s!!\n", filename_to_load);
        XBell(di,100);
    } 
    else 
    {    
        /* No need to remove it first -- smart load doesn't replace 'old' masks */
        load_mask(filename_to_load);

        XtVaGetValues(masksReplaceImagesButton,
                      XmNset, &masks_replace_images,
                      NULL);

        if (masks_replace_images) 
        {
            for (jj=0; jj<512; jj++)
                for (kk=0; kk<512; kk++)
                    values[jj*512+kk]=mask_pack.imagedata[(jj/2)*256+kk/2];
        }
     
        /* MTC added this 7/6/98 */
        fclose (mask_file);
    }

    DEBUG_TRACE_OUT printf( "Leaving autoload_mask\n" );
}


/*
 * Display only those images that have a corresponding
 * dosage file
 * If a mask file for a particular image/dose exists, 
 * load it as well.  
 */

void display_images_dosages_masks (void)
{
    int image_count, mask_count, dosage_count;
    char image_name_no_directory[MAX_LENGTH_NAME];
    char temp_string[MAX_LENGTH_NAME];
    int image_dose_loaded;

    DEBUG_TRACE_IN printf( "Entering display_images_dosages_masks\n" );

    for (image_count = 0; image_count < number_images; image_count ++)
    {
        image_dose_loaded = 0;

        for (dosage_count = 0; dosage_count < number_dosages; dosage_count ++)
        {
            if ( dose_goes_with_image ( dosages[dosage_count], images[image_count] ) )
            {
                DEBUG_LOADING printf ("Loading image %s\n", images[image_count]);
                autoload_image (images[image_count]);

                DEBUG_LOADING 
                    printf ("Loading dosage %s\n", dosages[dosage_count]);
                autoload_dose (dosages[dosage_count]);

                image_dose_loaded = 1;

                break;
            }
        }

        if (image_dose_loaded)
        {
            for (mask_count = 0; mask_count < number_masks; mask_count ++)
            {
                if ( mask_goes_with_image ( masks[mask_count], images[image_count] ) )
                {
                    DEBUG_LOADING printf ("Loading mask %s\n", masks[mask_count]);
                    autoload_mask (masks[mask_count]);

                    break;
                }
            }
        }
        else DEBUG_LOADING
                 printf ( "Couldn't find dose for %s\n", images[image_count] );

    }

    DEBUG_TRACE_OUT printf( "Leaving display_images_dosages_masks\n" );
}


int dose_goes_with_image ( char *dose_filename, char *image_filename )
{
    float image_z_value;
    float dose_z_value;
    int retval;
    
    DEBUG_TRACE_IN printf( "Entering dose_goes_with_image\n" );

    image_z_value = get_image_z_value_from_filename ( image_filename );
    dose_z_value = get_dose_or_mask_z_value_from_filename ( dose_filename ) * dose_scale_factor;
    
    if ( fabs ( image_z_value - dose_z_value ) < AUTOLOADER_ERROR_MARGIN * dose_scale_factor )
        retval = 1;
    else
        retval = 0;

    DEBUG_LOADING
        if ( retval )
            printf ( "%s goes with\n     %s\n", dose_filename, image_filename );
    
    DEBUG_TRACE_OUT printf( "Leaving dose_goes_with_image\n" );
    return ( retval );
}


int mask_goes_with_image ( char *mask_filename, char *image_filename )
{
    float image_z_value;
    float mask_z_value;
    int retval;

    DEBUG_TRACE_IN printf( "Entering mask_goes_with_image\n" );
    
    image_z_value = get_image_z_value_from_filename ( image_filename );
    mask_z_value = get_dose_or_mask_z_value_from_filename ( mask_filename ) * mask_scale_factor;
    
    if ( fabs ( image_z_value - mask_z_value ) < AUTOLOADER_ERROR_MARGIN * mask_scale_factor )
        retval = 1;
    else
        retval = 0;

    DEBUG_TRACE_OUT printf( "Leaving mask_goes_with_image\n" );
    return ( retval );
}


float get_image_z_value_from_filename ( char *filename )
{
    char tempStr[512];
    char *tempPtr;

    DEBUG_TRACE_IN printf( "Entering get_image_z_value_from_filename\n" );

    strcpy ( tempStr, filename );
    tempPtr = strrchr ( tempStr, ',' );

    if ( tempPtr )
        tempPtr[0] = '\0';

    tempPtr = strrchr ( tempStr, '/' );
    if( tempPtr != NULL )
        tempPtr++;
    else
        tempPtr = tempStr;
    
    tempPtr = strchr ( tempPtr, '.' );
    if( tempPtr != NULL )
        tempPtr++;
    
    if ( tempPtr )
    {
        DEBUG_TRACE_OUT printf( "Leaving get_image_z_value_from_filename\n" );
        return ( atof ( tempPtr ) );
    }
    else
    {
        DEBUG_TRACE_OUT printf( "Leaving get_image_z_value_from_filename\n" );
        return ( 0.0 );
    }
}

    
            
float get_dose_or_mask_z_value_from_filename ( char *filename )
{
    char tempStr[512];
    char *tempPtr;

    DEBUG_TRACE_IN printf( "Entering get_dose_or_mask_z_value_from_filename\n" );

    strcpy ( tempStr, filename );
    tempPtr = strrchr ( tempStr, ',' );

    if ( tempPtr )
        tempPtr[0] = '\0';

    tempPtr = strrchr ( tempStr, '/' );
    if( tempPtr != NULL )
        tempPtr++;
    else
        tempPtr = tempStr;
    
    tempPtr = strchr ( tempPtr, '.' );
    if( tempPtr != NULL )
        tempPtr++;
    
    if ( tempPtr )
    {
        DEBUG_TRACE_OUT printf( "Leaving get_dose_or_mask_z_value_from_filename\n" );
        return ( atof ( tempPtr ) );
    }
    else
    {
        DEBUG_TRACE_OUT printf( "Leaving get_dose_or_mask_z_value_from_filename\n" );
        return ( 0.0 );
    }
}
