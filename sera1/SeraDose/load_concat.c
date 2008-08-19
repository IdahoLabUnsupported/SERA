/***********************************************************************************
 load_concat.c

 Procedures for loading concatenated contour and mask files.

 Written by Matt Cohen
**********************************************************************************/
#include "load_concat.h"
#include "file_tools.h"

void load_single_3d_dose (char **fileArray, dose_concat_t *concat_data, 
			  floyd_data_ptr *data,
			  floyd_data_ptr *original_data_ptr, float z_value);


/*=======================================================================
  Function:    load_concatenated_contour_file_CB

  Purpose:     Callback to begin the loading process of a 3d contour file.

  Parameters:  Normal callback parameters.

  Returned:    None.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
void load_concatenated_contour_file_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    char contourFilename[256];
    char headerFilename[256];
    char setFilename[256];
    char *extPtr;
    int file_selected = 0;
    /*Widget popup;*/
    
    DEBUG_TRACE_IN printf ( "Entering load_concatenated_contour_file_CB\n" );

    if( rememberedFiles[ DOSE_FILES ].save_file_present )
    {
        if( get_filename_from_remembered_files_list( &rememberedFiles[DOSE_FILES], contourFilename ) )
            file_selected = 1;
    }
    else if( DT_select_file ( xcontoursTopLevelShell, context, contourFilename, "Select 3D Contour File" ) ) 
    {
        if( FT_filenameEndsIn( contourFilename, ".cdf.sz" ) && FT_fileExists( contourFilename ) )
        {
            file_selected = 1;
            add_to_saved_files( &rememberedFiles[DOSE_FILES], contourFilename );
            rememberedFiles[DOSE_FILES].save_file_present = 1;
        }
        else
        {
            DT_error ( xcontoursTopLevelShell, "File is not a 3D contour file.", "Load Error", NULL );
        }
    }
    
    if ( file_selected )
    {
        strcpy ( setFilename, contourFilename );
        
        extPtr = strstr ( setFilename, ".cdf.sz" );

        if( extPtr != NULL )
        {
            set_cursor ( 1 );
            DT_please_wait_popup ( xcontoursTopLevelShell,  1, "Loading... Please wait." );

            /* Strip off the .cdf and add .chd */
            extPtr[0] = '\0';
            strcpy ( headerFilename, setFilename );
            strcat ( headerFilename, ".chd" );

            load_concatenated_contour_file ( headerFilename, contourFilename );

            DT_please_wait_popup ( xcontoursTopLevelShell, 0, NULL );
            set_cursor ( 0 );
        }
        else
            printf("A serious error occured parsing the filenames\n");
    }

    DEBUG_TRACE_OUT printf ( "Leaving load_concatenated_contour_file_CB\n" );
}


/*=======================================================================
  Function:    load_concatenated_mask_file_CB

  Purpose:     Callback to begin the loading process of a 3d mask file.

  Parameters:  Normal callback parameters.

  Returned:    None.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
void load_concatenated_mask_file_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    char maskFilename[256];
    char headerFilename[256];
    char setFilename[256];
    char *extPtr;
    int file_selected = 0;
    /*Widget popup;*/
    
    DEBUG_TRACE_IN printf ( "Entering load_concatenated_mask_file_CB\n" );

    if( rememberedFiles[ MASK_FILES ].save_file_present )
    {
        if( get_filename_from_remembered_files_list( &rememberedFiles[MASK_FILES], maskFilename ) )
            file_selected = 1;
    }
    else if( DT_select_file ( xcontoursTopLevelShell, context, maskFilename, "Select 3D Mask File" ) )
    {
        if( FT_filenameEndsIn( maskFilename, ".cmf.sz" ) && FT_fileExists( maskFilename ) )
        {
            file_selected = 1;
            add_to_saved_files( &rememberedFiles[MASK_FILES], maskFilename );
            rememberedFiles[MASK_FILES].save_file_present = 1;
        }
        else
        {
            DT_error ( xcontoursTopLevelShell, "File is not a 3D mask file.", "Load Error", NULL );
        }
    }
    
    if ( file_selected )
    {
        strcpy ( setFilename, maskFilename );
        
        extPtr = strstr ( setFilename, ".cmf.sz" );

        if( extPtr != NULL )
        {
            set_cursor ( 1 );
            DT_please_wait_popup ( xcontoursTopLevelShell,  1, "Loading... Please wait." );

            /* Strip off the .cdf and add .chd */
            extPtr[0] = '\0';
            strcpy ( headerFilename, setFilename );
            strcat ( headerFilename, ".chd" );

            load_concatenated_mask_file ( headerFilename, maskFilename );

            DT_please_wait_popup ( xcontoursTopLevelShell, 0, NULL );
            set_cursor ( 0 );
        }
        else
            printf("A serious error occured while parsing the filenames\n");
        
    }

    DEBUG_TRACE_OUT printf ( "Leaving load_concatenated_mask_file_CB\n" );
}


/*=======================================================================
  Function:    load_concatenated_contour_file

  Purpose:     Opens the headerfile, creates temp contour files,
               and then loads the temp files 

  Parameters:  char *headerFilename - Name of the headerfile for the
                                      contours.
               char *contourFilename - Name of the contour file.

  Returned:    None.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
void load_concatenated_contour_file ( char *headerFilename, char *contourFilename )
{
    dose_concat_t concat_data;

    DEBUG_TRACE_IN printf ( "Entering load_concatenated_contour_file\n" );
    
    if ( read_concatenated_header_file ( headerFilename, &concat_data ) )
    {
        load_3d_contour_file ( contourFilename, &concat_data );
        free_concat_data ( &concat_data );
    }
    else
    {
        char temp[256];
        sprintf ( temp, "Could not load %s", headerFilename );
        DT_error ( xcontoursTopLevelShell, temp, "Load Error", NULL );
    }

    DEBUG_TRACE_OUT printf ( "Leaving load_concatenated_contour_file\n" );    
}


/*=======================================================================
  Function:    load_concatenated_mask_file

  Purpose:     Opens the headerfile, creates temp mask files,
               and then loads the temp files 

  Parameters:  char *headerFilename - Name of the headerfile for the
                                      masks.
               char *contourFilename - Name of the mask file.

  Returned:    None.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
void load_concatenated_mask_file ( char *headerFilename, char *maskFilename )
{
    dose_concat_t concat_data;

    DEBUG_TRACE_IN printf ( "Entering load_concatenated_mask_file\n" );
    
    if ( read_concatenated_header_file ( headerFilename, &concat_data ) )
    {
        create_temp_mask_files ( maskFilename, &concat_data );
        load_temp_mask_files ( &concat_data );
        free_concat_data ( &concat_data );
    }
    else
    {
        char temp[256];
        sprintf ( temp, "Could not load %s", headerFilename );
        DT_error ( xcontoursTopLevelShell, temp, "Load Error", NULL );
    }

    DEBUG_TRACE_OUT printf ( "Leaving load_concatenated_mask_file\n" );    
}


/*=======================================================================
  Function:    load_3d_contour_file

  Purpose:     Creates temp files for each contour.

  Parameters:  dose_concat_t *concat_data - pointer to all the headerfile
                                            information.
               char *contourFilename - Name of the contour file.

  Returned:    1 for success, 0 otherwise.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
int load_3d_contour_file ( char *contourFilename, dose_concat_t *concat_data )
{
    char *fileArray;
    char *fileArrayPtr;
    int arraySize;
    /*char tempStr[256];*/
    /*char systemCall[256];*/
    int i;
    
    DEBUG_TRACE_IN printf ( "Entering load_3d_contour_file\n" );
    
    if ( ! ( SZ_UnzipFileIntoArray ( contourFilename, &fileArray, &arraySize ) ) )
    {
        printf ( "Unable to uncompress %s\n", contourFilename );
        DEBUG_TRACE_OUT printf ( "Leaving load_3d_contour_file\n" );
        return ( 0 );        
    }

    if ( ! ( concat_data->filename = ( char ** ) MT_malloc ( sizeof ( char * ) * concat_data->num_planes ) ) )
    {
        printf ( "Error allocating memory for filenames\n" );
        DEBUG_TRACE_OUT printf ( "Leaving load_3d_contour_file\n" );
        return ( 0 );        
    }
    
    for ( i = 0; i < concat_data->num_planes; i++ )
    {
        if ( ! ( concat_data->filename[i] = ( char * ) MT_malloc ( sizeof ( char ) * 256 ) ) )
        {
            printf ( "Error allocating memory for filenames\n" );
            DEBUG_TRACE_OUT printf ( "Leaving load_3d_contour_file\n" );
            return ( 0 );        
        }
    }

    /*remove_temp_contour_files ( );
    strcpy ( systemCall, "mkdir " );
    strcat ( systemCall, TEMP_CONTOUR_FILE_DIRECTORY );
    system ( systemCall );*/
    
    fileArrayPtr = fileArray;
    for ( i = 0; i < concat_data->num_planes; i++ )
    {
        char *memory_file = MT_malloc(sizeof(unsigned char)*5);
	floyd_data_ptr orig_data;

	find_slice(concat_data->z_val[i],1);

        /*strcpy ( concat_data->filename[i], TEMP_CONTOUR_FILE_DIRECTORY );
        sprintf ( tempStr, "TEMP%01.2f,contour", concat_data->z_val[i] );
        strcat ( concat_data->filename[i], tempStr );*/
        
	load_single_3d_dose(&fileArrayPtr,concat_data,
			    &dose_data,&orig_data, 
			    concat_data->z_val[i]);

	add_dose_to_memory(i,dose_data,orig_data,
			   NUMPOINTS,concat_data->z_val[i]);


	memory_file[0] = '\b';
	memory_file[1] = i / 128+1;
	memory_file[2] = i % 128+1;
	memory_file[3] = '\000';        

	addto_dosage_list(memory_file,
			  image_matrix.active_picture,
			  concat_data->z_val[i]);
	contours_are_current = FALSE;

	load_imageEH(mainWindowDrawingArea, 0, &SureEvent);

	/*create_single_temp_contour_file ( concat_data->filename[i], &fileArrayPtr, concat_data, concat_data->z_val[i] );*/
    }
    
    SZ_FreeArray ( &fileArray );
    DEBUG_TRACE_OUT printf ( "Leaving load_3d_contour_file\n" );
    return ( 1 );
}


/*=======================================================================
  Function:    create_temp_mask_files

  Purpose:     Creates temp files for each mask.

  Parameters:  dose_concat_t *concat_data - pointer to all the headerfile
                                            information.
               char *contourFilename - Name of the concatenated mask file.

  Returned:    1 for success, 0 otherwise.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
int create_temp_mask_files ( char *maskFilename, dose_concat_t *concat_data )
{
    unsigned char *fileArray;
    unsigned char *fileArrayPtr;
    int arraySize;
    char tempStr[256];
    char systemCall[256];
    int i;
    
    DEBUG_TRACE_IN printf ( "Entering create_temp_mask_files\n" );
    
    if ( ! ( SZ_UnzipFileIntoBinaryArray ( maskFilename, &fileArray, (uLong*)&arraySize, sizeof ( unsigned char ) ) ) )
    {
        printf ( "Unable to uncompress %s\n", maskFilename );
        DEBUG_TRACE_OUT printf ( "Leaving load_3d_contour_file\n" );
        return ( 0 );        
    }

    if ( ! ( concat_data->filename = ( char ** ) MT_malloc ( sizeof ( char * ) * concat_data->num_planes ) ) )
    {
        printf ( "Error allocating memory for filenames\n" );
        DEBUG_TRACE_OUT printf ( "Leaving create_temp_mask_files\n" );
        return ( 0 );        
    }
    
    for ( i = 0; i < concat_data->num_planes; i++ )
    {
        if ( ! ( concat_data->filename[i] = ( char * ) MT_malloc ( sizeof ( char ) * 256 ) ) )
        {
            printf ( "Error allocating memory for filenames\n" );
            DEBUG_TRACE_OUT printf ( "Leaving create_temp_mask_files\n" );
            return ( 0 );        
        }
    }

    remove_temp_mask_files ( );
    strcpy ( systemCall, "mkdir " );
    strcat ( systemCall, TEMP_MASK_FILE_DIRECTORY );
    system ( systemCall );
    
    fileArrayPtr = fileArray;
    for ( i = 0; i < concat_data->num_planes; i++ )
    {
        strcpy ( concat_data->filename[i], TEMP_MASK_FILE_DIRECTORY );
        sprintf ( tempStr, "TEMP%01.2f,mask", concat_data->z_val[i] );
        strcat ( concat_data->filename[i], tempStr );
        
        create_single_temp_mask_file ( concat_data->filename[i], &fileArrayPtr, concat_data  );
    }
    
    SZ_FreeArray ( (char **)&fileArray );
    DEBUG_TRACE_OUT printf ( "Leaving create_temp_mask_files\n" );
    return ( 1 );
}


/*=======================================================================
  Function:    create_single_temp_mask_file

  Purpose:     Creates a single temp files for a mask.

  Parameters:  char *filename - name of the mask file to create.
               unsigned char **fileArray - Array containing concatenated
                                           mask files.
               dose_concat_t *concat_data - pointer to all the headerfile
                                            information.

  Returned:    1 for success, 0 otherwise.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
int create_single_temp_mask_file ( char *filename, unsigned char **fileArray, dose_concat_t *concat_data )
{
    FILE *outfile;
    /*char line[256];*/
    /*int i;*/

    DEBUG_TRACE_IN printf ( "Entering create_single_temp_mask_file\n" );
    
    /* open temp file */
    if ( ! ( outfile = fopen ( filename, "w" ) ) )
    {
        printf ( "Unable to open %s\n", filename );
        DEBUG_TRACE_OUT printf ( "Leaving create_single_temp_mask_file\n" );
        return ( 0 );
    }

    /* Write mask data to file */
    fwrite ( *fileArray, sizeof ( unsigned char ), 65536, outfile );

    /* Move pointer in array ahead to next file */
    *fileArray = &(*fileArray)[65536];
    
    /* Close the temp file */
    fclose ( outfile );

    /* Temp file created successfully */
    DEBUG_TRACE_OUT printf ( "Leaving create_single_temp_mask_file\n" );
    return ( 1 );
}


/*=======================================================================
  Function:    read_concatenated_header_file

  Purpose:     Reads information from 3d contour/mask header file.

  Parameters:  char *headerFile - name of the header file to read.
               dose_concat_t *concat_data - pointer to all the headerfile
                                            information.

  Returned:    1 for success, 0 otherwise.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
int read_concatenated_header_file ( char *headerFile, dose_concat_t *concat_data )
{
    FILE *infile;
    char line[256];
    char value[256];
    char z_val_str[256];
    int i;
    
    DEBUG_TRACE_IN printf ( "Entering read_concatenated_header_file\n" );
    
    KV_set_split_characters ( ":" );

    if ( ! ( infile = fopen ( headerFile, "r" ) ) )
    {
        printf ( "Could not load %s\n", headerFile );
        DEBUG_TRACE_OUT printf ( "Leaving read_concatenated_header_file\n" );
        return ( 0 );
    }

    fgets ( line, 256, infile );
    
    /*
     *  As of now (5/25/99), expect only version seraMC_1A0 for concatenated files
     */
    
    if ( strstr ( line, "seraMC_1A0" ) || strstr( line, "seraPlan_1B0") 
	 || strstr(line, "seraMC_1C0") || strstr( line, "seraPlan_1C0"))
    {
        DEBUG_LOADING printf(" ---- Version seraMC Contour Data ----\n");
    }    
    else
    {
      char temp[512];
      snprintf(temp,512,"The file has a different version than expected.\nIt uses:\n%s\nDo you want to load it anyway?",line);
      if(DT_decide ( xcontoursTopLevelShell,context, temp, "Different Version", "Load","Cancel load" )){	
	DEBUG_LOADING printf(" ---- Version seraMC Contour Data ----\n");
      } else {
        printf ( "This concatenated contour data file does not appear to be version seraMC_1A0 or seraPlan_1B0.\n" );
        printf ( "seraDose does not have the ability to load this file.\n" );
        DEBUG_TRACE_OUT printf ( "Leaving read_concatenated_header_file\n" );
        return ( 0 );
      }
    }

    /* Read in Field of View */
    if ( KV_read_string_value_for_key ( infile, "fov", value, 256 ) )
        sscanf ( value, "%e", &concat_data->FOV );
    else printf ( "Could not locate FOV\n" );

    /* Read in xmin, xmax, ymin, and ymax */
    if ( KV_read_string_value_for_key ( infile, "xmin", value, 256 ) )
        sscanf ( value, "%e", &concat_data->xmin );
    else printf ( "Could not locate xmin\n" );

    if ( KV_read_string_value_for_key ( infile, "xmax", value, 256 ) )
        sscanf ( value, "%e", &concat_data->xmax );
    else printf ( "Could not locate xmax\n" );

    if ( KV_read_string_value_for_key ( infile, "ymin", value, 256 ) )
        sscanf ( value, "%e", &concat_data->ymin );
    else printf ( "Could not locate ymin\n" );

    if ( KV_read_string_value_for_key ( infile, "ymax", value, 256 ) )
        sscanf ( value, "%e", &concat_data->ymax );
    else printf ( "Could not locate ymax\n" );

    /* Read in x_in_field, y_in_field, z_in_filed */
    if ( KV_read_string_value_for_key ( infile, "x_in_field", value, 256 ) )
        sscanf ( value, "%d", &concat_data->x_in_field );
    else printf ( "Could not locate x_in_field\n" );
    
    if ( KV_read_string_value_for_key ( infile, "y_in_field", value, 256 ) )
        sscanf ( value, "%d", &concat_data->y_in_field );
    else printf ( "Could not locate y_in_field\n" );
    
    if ( KV_read_string_value_for_key ( infile, "z_in_field", value, 256 ) )
        sscanf ( value, "%d", &concat_data->z_in_field );
    else printf ( "Could not locate z_in_field\n" );

    /* Read in num_cols and num_rows */
    if ( KV_read_string_value_for_key ( infile, "num_cols", value, 256 ) )
        sscanf ( value, "%d", &concat_data->num_cols );
    else printf ( "Could not locate num_cols\n" );

    if ( KV_read_string_value_for_key ( infile, "num_rows", value, 256 ) )
        sscanf ( value, "%d", &concat_data->num_rows );
    else printf ( "Could not locate num_rows\n" );

    /* Compute total number of points in dose image */
    concat_data->num_points = concat_data->num_cols * concat_data->num_rows;

    /* Read the run title */
    if ( ! ( KV_read_string_value_for_key ( infile, "run_title", concat_data->run_title, 256 ) ) )
        printf ( "Could not locate run_title\n" );

    /* Read the number of planes */
    if ( KV_read_string_value_for_key ( infile, "num_planes", value, 256 ) )
    {
        sscanf ( value, "%d", &concat_data->num_planes );

        /* num_planes and the number of image slices must match */
        if( concat_data->num_planes != image_matrix.num_pics )
        {
            char message[256];
            sprintf( message, "Expecting to find %d planes in your .chd file.\nFound %d instead.",
                     image_matrix.num_pics,
                     concat_data->num_planes );
            DT_error( xcontoursTopLevelShell, message, "File Error", NULL );
            return( 0 );
        }
    }
    else
    {
        DT_error( xcontoursTopLevelShell,
                  "Your .chd file does not specify the number of planes, cannot continue!",
                  "File Error",
                  NULL );
        return( 0 );
    }
    

    /* Read the dose concentrations */
    if ( KV_read_string_value_for_key ( infile, "conc_0", value, 256 ) )
        sscanf ( value, "%e", &concat_data->totalConc );
    else printf ( "Could not locate total concentration\n" );

    if ( KV_read_string_value_for_key ( infile, "conc_1", value, 256 ) )
        sscanf ( value, "%e", &concat_data->boronConc );
    else printf ( "Could not locate boron concentration\n" );
    
    if ( KV_read_string_value_for_key ( infile, "conc_2", value, 256 ) )
        sscanf ( value, "%e", &concat_data->gammaConc );
    else printf ( "Could not locate gamma concentration\n" );

    if ( KV_read_string_value_for_key ( infile, "conc_3", value, 256 ) )
        sscanf ( value, "%e", &concat_data->nitrogenConc );
    else printf ( "Could not locate nitrogen concentration\n" );    

    if ( KV_read_string_value_for_key ( infile, "conc_4", value, 256 ) )
        sscanf ( value, "%e", &concat_data->fastConc );
    else printf ( "Could not locate fast concentration\n" );

    if ( KV_read_string_value_for_key ( infile, "conc_6", value, 256 ) )
        sscanf ( value, "%e", &concat_data->fastFluenceConc );
    else printf ( "Could not locate fast fluence concentration\n" );

    if ( KV_read_string_value_for_key ( infile, "conc_7", value, 256 ) )
        sscanf ( value, "%e", &concat_data->epithermalFluenceConc );
    else printf ( "Could not locate epithermal fluence concentration\n" );

    if ( KV_read_string_value_for_key ( infile, "conc_8", value, 256 ) )
        sscanf ( value, "%e", &concat_data->thermalFluenceConc );
    else printf ( "Could not locate thermal fluence concentration\n" );

    if ( KV_read_string_value_for_key ( infile, "conc_5", value, 256 ) )
        sscanf ( value, "%e", &concat_data->otherConc );
    else printf ( "Could not locate other concentration\n" );
    
    /* Read the dose rbe values */
    if ( KV_read_string_value_for_key ( infile, "rbe_val_0", value, 256 ) )
        sscanf ( value, "%e", &concat_data->totalRBE );
    else printf ( "Could not locate total rbe value\n" );

    if ( KV_read_string_value_for_key ( infile, "rbe_val_1", value, 256 ) )
        sscanf ( value, "%e", &concat_data->boronRBE );
    else printf ( "Could not locate boron rbe value\n" );
    
    if ( KV_read_string_value_for_key ( infile, "rbe_val_2", value, 256 ) )
        sscanf ( value, "%e", &concat_data->gammaRBE );
    else printf ( "Could not locate gamma rbe value\n" );

    if ( KV_read_string_value_for_key ( infile, "rbe_val_3", value, 256 ) )
        sscanf ( value, "%e", &concat_data->nitrogenRBE );
    else printf ( "Could not locate nitrogen rbe value\n" );    

    if ( KV_read_string_value_for_key ( infile, "rbe_val_4", value, 256 ) )
        sscanf ( value, "%e", &concat_data->fastRBE );
    else printf ( "Could not locate fast rbe value\n" );

    if ( KV_read_string_value_for_key ( infile, "rbe_val_6", value, 256 ) )
        sscanf ( value, "%e", &concat_data->fastFluenceRBE );
    else printf ( "Could not locate fast fluence rbe value\n" );

    if ( KV_read_string_value_for_key ( infile, "rbe_val_7", value, 256 ) )
        sscanf ( value, "%e", &concat_data->epithermalFluenceRBE );
    else printf ( "Could not locate epithermal fluence rbe value\n" );

    if ( KV_read_string_value_for_key ( infile, "rbe_val_8", value, 256 ) )
        sscanf ( value, "%e", &concat_data->thermalFluenceRBE );
    else printf ( "Could not locate thermal fluence rbe value\n" );

    if ( KV_read_string_value_for_key ( infile, "rbe_val_5", value, 256 ) )
        sscanf ( value, "%e", &concat_data->otherRBE );
    else printf ( "Could not locate other reference rbe value\n" );    
    
    /* Read the reference dose values */
    if ( KV_read_string_value_for_key ( infile, "ref_dose_0", value, 256 ) )
        sscanf ( value, "%e", &concat_data->totalRef );
    else printf ( "Could not locate total reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_1", value, 256 ) )
        sscanf ( value, "%e", &concat_data->boronRef );
    else printf ( "Could not locate boron reference dose\n" );
    
    if ( KV_read_string_value_for_key ( infile, "ref_dose_2", value, 256 ) )
        sscanf ( value, "%e", &concat_data->gammaRef );
    else printf ( "Could not locate gamma reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_3", value, 256 ) )
        sscanf ( value, "%e", &concat_data->nitrogenRef );
    else printf ( "Could not locate nitrogen reference dose\n" );    

    if ( KV_read_string_value_for_key ( infile, "ref_dose_4", value, 256 ) )
        sscanf ( value, "%e", &concat_data->fastRef );
    else printf ( "Could not locate fast reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_6", value, 256 ) )
        sscanf ( value, "%e", &concat_data->fastFluenceRef );
    else printf ( "Could not locate fast fluence reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_7", value, 256 ) )
        sscanf ( value, "%e", &concat_data->epithermalFluenceRef );
    else printf ( "Could not locate epithermal fluence reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_8", value, 256 ) )
        sscanf ( value, "%e", &concat_data->thermalFluenceRef );
    else printf ( "Could not locate thermal fluence reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_5", value, 256 ) )
        sscanf ( value, "%e", &concat_data->otherRef );
    else printf ( "Could not locate other reference dose\n" );

    /* Get memory for z values */
    if ( ! ( concat_data->z_val = ( float * ) MT_malloc ( sizeof ( float ) * concat_data->num_planes ) ) )
    {
        printf ( "Unable to allocate memory for z values\n" );
        return ( 0 );
    }
    
    /* Read in the z_values */
    for ( i = 0; i < concat_data->num_planes; i++ )
    {
        /* Create the key for the file */
        sprintf ( z_val_str, "z_value_%02d", i );

        if ( KV_read_string_value_for_key ( infile, z_val_str, value, 256 ) )
        {
            sscanf ( value, "%e", &concat_data->z_val[i] );
            
        }
        else
        {
            printf ( "Could not locate z_value for slice #%d\n", i );
            continue;
        }
    }

    /* Concatenated Header file read successfully */
    DEBUG_TRACE_OUT printf ( "Leaving read_concatenated_header_file\n" );
    return ( 1 );
}


/*=======================================================================
  Function:    free_concat_data

  Purpose:     Frees arrays of malloced memory.

  Parameters:  dose_concat_t *concat_data - pointer to all the headerfile
                                            information.

  Returned:    None.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
void free_concat_data ( dose_concat_t *concat_data )
{
    int i;

    DEBUG_TRACE_IN printf ( "Entering free_concat_data\n" );
    
    MT_free ( (void *)concat_data->z_val );

    for ( i = 0; i < concat_data->num_planes; i++ )
    {
        MT_free ( (void *)concat_data->filename[i] );
    }

    MT_free ( (void *)concat_data->filename );

    DEBUG_TRACE_OUT printf ( "Leaving free_concat_data\n" );
}


/* loads a single 3d dose. */
void load_single_3d_dose (char **fileArray, dose_concat_t *concat_data, 
			  floyd_data_ptr *data,
			  floyd_data_ptr *original_data_ptr, float z_value)
{
  int             /*file_length,*/ count, skipInt /*, versionFlag*/;
   int             Xcolumn, Ycolumn/*, SKIPPED_LINES */;
   /*char            line[LINESIZE], string[35];*/
   float           container[3];
   float           totalRef, boronRef, gammaRef, nitrogenRef, otherRef;
   float           fastRef, group1Ref, group2Ref, thermalFluenceRef;
   int             abscissa, ordinate;
   static int      LDfirstCall = 1;


   floyd_data_ptr  original_data;

   /*char            z_value_string[100];*/

   DEBUG_TRACE_IN printf( "Entering load_single_3d_dose\n" );

   /*
    * read number of points, number of lines skipped to get to dose data,
    *      xmin, xmax, ymin, ymax, zmin, and zmax
    */

   /*file = fopen(filename,"r");
   if(!file){
     printf("Unable to open file %s\n",filename);
     return;
   }
   versionFlag = 0;
   fgets(line, LINESIZE - 1, file);
   if( strstr(line, " new_v107") ) {
     DEBUG_LOADING printf(" ---- Version 1.07 Contour Data ----\n");
     versionFlag = 107;
   }*/
     
   /*fscanf(file,"%e %e %e %e %e %e %d %d %d %d %d", &FOV, &ZVAL,
	  &xmin, &xmax, &ymin, &ymax, &Xcolumn, &Ycolumn,
	  &NUM_X_POINTS, &NUM_Y_POINTS, &SKIPPED_LINES);
   abscissa = Xcolumn - 1;
   ordinate = Ycolumn - 1;*/

   FOV = concat_data->FOV;
   ZVAL = z_value;
   xmin = concat_data->xmin;
   xmax = concat_data->xmax;
   ymin = concat_data->ymin,
   ymax = concat_data->ymax,
   Xcolumn = concat_data->x_in_field,
   Ycolumn = concat_data->y_in_field,
   NUM_X_POINTS = concat_data->num_cols,
   NUM_Y_POINTS = concat_data->num_rows;
   abscissa = Xcolumn - 1;
   ordinate = Ycolumn - 1;
   
   /*
    * set up dynamic storage for all of the plane's dose points
    */
   if (!LDfirstCall) {
     MT_free((void *)*data);
   }
   LDfirstCall = 0;
     
   NUMPOINTS = NUM_X_POINTS  * NUM_Y_POINTS;
   
   *data = (floyd_data *)MT_calloc(NUMPOINTS, sizeof(floyd_data) );
   original_data = (floyd_data *)MT_calloc(NUMPOINTS, sizeof(floyd_data) );
   
     
   /*
    *
    * Skip past header information
    *
    */
   /*for (count=0; count<SKIPPED_LINES + 1; count++)
     fgets(line, LINESIZE - 1, file);*/
   
   /* read the concentrations */
   /*fscanf(file,"%35c %e %e %e %e %e %e %e %e %e", string,
     &container[0], &BoronConc, &GammaConc, &NitrogenConc,
     &FastConc, &container[0], &container[0], &container[0], &OtherConc);*/

   container[0] = concat_data->totalConc;
   BoronConc = concat_data->boronConc;
   GammaConc = concat_data->gammaConc;
   NitrogenConc = concat_data->nitrogenConc;
   FastConc = concat_data->fastConc;
   container[0] = concat_data->fastFluenceConc;
   container[0] = concat_data->epithermalFluenceConc;
   container[0] = concat_data->thermalFluenceConc;
   OtherConc = concat_data->otherConc;

   CurConcs[0] = BoronConc, CurConcs[1] = GammaConc;
   CurConcs[2] = NitrogenConc, CurConcs[3] = FastConc;
   CurConcs[4] = OtherConc;
   
   /* read the dose factors */
   /*fscanf(file,"%35c %e %e %e %e %e %e %e %e %e", string,
	  &container[0], &BoronFactor, &GammaFactor, &NitrogenFactor,
	  &FastFactor, &container[0], &container[0], &container[0], &OtherFactor);*/
   container[0] = concat_data->totalRBE;
   BoronFactor = concat_data->boronRBE;
   GammaFactor = concat_data->gammaRBE;
   NitrogenFactor = concat_data->nitrogenRBE;
   FastFactor = concat_data->fastRBE;
   container[0] = concat_data->fastFluenceRBE;
   container[0] = concat_data->epithermalFluenceRBE;
   container[0] = concat_data->thermalFluenceRBE;
   OtherFactor = concat_data->otherRBE;
     
   CurFactors[0] = BoronFactor, CurFactors[1] = GammaFactor;
   CurFactors[2] = NitrogenFactor, CurFactors[3] = FastFactor;
   CurFactors[4] = OtherFactor;
     
     
   /* read the ref values at thermal peak */
   /*fscanf(file,"%35c %e %e %e %e %e %e %e %e %e", string,
	  &totalRef, &BoronRef, &GammaRef, &NitrogenRef,
	  &FastRef, &group1Ref, &group2Ref, &thermalFluenceRef, &OtherRef);*/

   totalRef = concat_data->totalRef;
   BoronRef = concat_data->boronRef; 
   GammaRef = concat_data->gammaRef; 
   NitrogenRef = concat_data->nitrogenRef;	  
   FastRef = concat_data->fastRef; 
   group1Ref = concat_data->fastFluenceRef; 
   group2Ref = concat_data->epithermalFluenceRef; 
   thermalFluenceRef = concat_data->thermalFluenceRef; 
   OtherRef = concat_data->otherRef;  

   CurRefs[0] = BoronRef, CurRefs[1] = GammaRef, CurRefs[2] = NitrogenRef;
   CurRefs[3] = FastRef, CurRefs[4] = group1Ref, CurRefs[5] = group2Ref;
   CurRefs[6] = thermalFluenceRef, CurRefs[7] = OtherRef;
     
   totalRef          = 100.0 / totalRef;
   boronRef          = 100.0 / BoronRef;
   gammaRef          = 100.0 / GammaRef;
   nitrogenRef       = 100.0 / NitrogenRef;
   fastRef           = 100.0 / FastRef;
   group1Ref         = 100.0 / group1Ref;
   group2Ref         = 100.0 / group2Ref;
   thermalFluenceRef = 100.0 / thermalFluenceRef; 
   otherRef          = 100.0 / OtherRef;
        
   for( count = 0; count < NUMPOINTS; count ++){
     char line[256];
     KV_SZ_readln(fileArray, line, 256);
     sscanf(line,"%f %f %f %d %f %f %f %f %f %f %f %f %f",
	    &container[0], &container[1], &container[2],
	    &skipInt,
	    &((original_data)[count].totalDose), 
	    &((original_data)[count].boronDose),
	    &((original_data)[count].gammaDose),
	    &((original_data)[count].nitrogenDose), 
	    &((original_data)[count].fastDose),
	    &((original_data)[count].otherDose),
	    &((original_data)[count].group1Fluence), 
	    &((original_data)[count].group2Fluence),
	    &((original_data)[count].thermalFluence));
     
     (*data)[count].x = container[abscissa];
     (*data)[count].y = container[ordinate];
     (*data)[count].totalDose      = (original_data)[count].totalDose * totalRef;
     (*data)[count].boronDose      = (original_data)[count].boronDose * boronRef;
     (*data)[count].gammaDose      = (original_data)[count].gammaDose * gammaRef;
     (*data)[count].nitrogenDose  = 
       (original_data)[count].nitrogenDose * nitrogenRef;
     (*data)[count].fastDose      = (original_data)[count].fastDose * fastRef;
     (*data)[count].group1Fluence = 
       (original_data)[count].group1Fluence * group1Ref;
     (*data)[count].group2Fluence = 
       (original_data)[count].group2Fluence * group2Ref;
     (*data)[count].thermalFluence = 
       (original_data)[count].thermalFluence * thermalFluenceRef;
     (*data)[count].otherDose      = (original_data)[count].otherDose * otherRef;
       
   }
     
   dosage_is_there = 1;
        
   reset_image_set_if_needed(image_matrix.image_set);
   
   /* This was added so the current_driver_data (which can be modified
    * by setting factors) will immediately replace any defaults loaded by
    * the file RATHER than having to click on the image first
    * Previously:
    *   toggle_on would call setfactors
    * Currently:
    *   toggle_on calls load_dose which calls setfactors
    *   ** advantage is that factors get immediately set up correctly
    *      as soon as a dose is loaded rather than having to click on
    *      an image to activate it
    */
   setfactors (&current_driver_data,original_data);

   *original_data_ptr = original_data;

   DEBUG_TRACE_OUT printf( "Leaving load_single_3d_dose\n" );
}


/*=======================================================================
  Function:    load_temp_mask_files

  Purpose:     Loads the temp maskfiles.

  Parameters:  dose_concat_t *concat_data - pointer to all the headerfile
                                            information.

  Returned:    None.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
void load_temp_mask_files ( dose_concat_t *concat_data )
{
    int i, jj, kk;
    Boolean masks_replace_images;

    DEBUG_TRACE_IN printf ( "Entering load_temp_mask_files.\n" );

    for ( i = 0; i < concat_data->num_planes; i++ )
    {
        find_slice ( concat_data->z_val[i], 0 );

        load_mask ( concat_data->filename[i] );

        XtVaGetValues ( masksReplaceImagesButton,
                        XmNset, &masks_replace_images,
                        NULL );
            
        if ( masks_replace_images ) 
        {
            for ( jj = 0; jj < 512; jj++ )
                for (kk = 0; kk < 512; kk++ )
                    values[jj * 512 + kk]
                        = mask_pack.imagedata[(jj/2)*256+kk/2];
        }
    }

    highlight_active_picture ();

    DEBUG_TRACE_OUT printf("Leaving load_temp_mask_files.\n");
}


/*=======================================================================
  Function:    remove_temp_contour_files

  Purpose:     Makes a system call to remove temp files and directory.

  Parameters:  None.

  Returned:    None.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
/*void remove_temp_contour_files ( void )
{
    char systemCall[256];

    DEBUG_TRACE_IN printf ( "Entering remove_temp_contour_files\n" );
    
    strcpy ( systemCall, "rm -rf " );
    strcat ( systemCall, TEMP_CONTOUR_FILE_DIRECTORY );
    system ( systemCall );

    DEBUG_TRACE_OUT printf ( "Leaving remove_temp_contour_files\n" );    
}*/


/*=======================================================================
  Function:    remove_temp_mask_files

  Purpose:     Makes a system call to remove temp files and directory.

  Parameters:  None.

  Returned:    None.

  Written by:  Matt Cohen 5/13/99
  =======================================================================*/
void remove_temp_mask_files (void)
{
    char systemCall[256];

    DEBUG_TRACE_IN printf ( "Entering remove_temp_mask_files\n" );
    
    strcpy ( systemCall, "rm -rf " );
    strcat ( systemCall, TEMP_MASK_FILE_DIRECTORY );
    system ( systemCall );

    DEBUG_TRACE_OUT printf ( "Leaving remove_temp_mask_files\n" );    
}
