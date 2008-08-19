/***********************************************************
 *   BNCT 2.3
 *
 *  Process files for univels - written by Cory Albright
 *
 *  This procedure takes the passed filename and opens/creates
 *  the appropriate (.rs,.uvh, .uv) files.  
 *
 *  .rs  - this file contains most the needed body information
 *         for creating the univel file.  
 *
 *  .uvh - contains the pixel size, x, y, z mins, etc.,  also 
 *         used for creating the univel file.
 *
 *  .uv  - this is the output univel file.
 * 
 *
 *  NOTE - the geom struct holds all the needed info as it
 *         comes in from the uvh and rs files, which is then
 *         used to output the uv file.
 *
 ************************************************************/
#include "seraConvert.h"
#include "nurb.h"

/* 1/2^10 */
#define EPSILON (0.0009765625)
#define INEL_BUFFER_SIZE 256
#define fineness 10
#define MAX_LINE_SIZE 256

char ordered_body[MAX_NUM_BODIES][MAX_LINE_SIZE];
int num_ordered_bodies = 0;

/* Local function prototypes */
int readSplinesForUnivels   ( main_gui_t *, geom_info_t *, char * );
int compareBodies           ( const void *, const void * );
void sortBodiesForEachSlice ( main_gui_t * );

static void my_cp_to_vox         ( main_gui_t *, int, float *, int, char *, int );
static void my_fill_region_wvect ( geom_info_t *, float, float, float, float, int, unsigned char );
static void my_fill_region       ( geom_info_t *, int, int, int, unsigned char );

static int bitregions[8] = {1, 2, 4, 8, 16, 32, 64, 128};


void initialize_spline_info ( spline_info_t *spline_info )
{
    spline_info->valid.field_of_view  = 0;
    spline_info->valid.dimensionality = 0;
    spline_info->valid.image_slices   = 0;
    spline_info->valid.x_pixel_size   = 0;
    spline_info->valid.y_pixel_size   = 0;
    spline_info->valid.z_spacing      = 0;
    spline_info->valid.pa_axis_min    = 0;
    spline_info->valid.pa_axis_max    = 0;
    spline_info->valid.rl_axis_min    = 0;
    spline_info->valid.rl_axis_max    = 0;
    spline_info->valid.is_axis_min    = 0;
    spline_info->valid.is_axis_max    = 0;
}


/* ===================================================================
   getValuesFromSplineFile

   Reads some initial values from the spline file.  This helps set
   up some needed values for building the univel file.
   =================================================================*/
int getValuesFromSplineFile ( main_gui_t *gui )
{
    FILE *infile;
    char errorString[MAX_LINE_SIZE];
    char line[MAX_LINE_SIZE];
    char temp[MAX_LINE_SIZE];
    int i, counter;
    float x, y;
    float half_fov;
    int eof, slice_ended;
    float max_x = -10000.0;
    float min_x = 10000.0;
    float max_y = -10000.0;
    float min_y = 10000.0;
    float z_value;
    float max_z_value = -10000.0;
    float min_z_value = 10000.0;
    int knot_vector_length;
    
    DEBUG_TRACE_IN printf ( "Entering getValuesFromSplineFile\n" );    
    
    if ( ! ( infile = fopen ( gui->rs_filename, "r" ) ) )
    {
        sprintf ( errorString, "Could not open file: %s", gui->rs_filename );
        DT_error ( gui->shell, errorString, "Load Error", NULL );
        DEBUG_TRACE_OUT printf ( "Leaving getValuesFromSplineFile\n" );    
        return ( 0 );
    }

    /*
     * Work through the file extracting the following if possible:
     *      image slices
     *      x pixel size
     *      y pixel size
     *      PA axis min
     *      PA axis max
     *      RL axis min
     *      PL axis max
     *      IS axis min
     *      IS axis max
     */

    fgets ( line, MAX_LINE_SIZE, infile );
    if ( strstr ( line, "fov" ) )
    {
        sscanf ( line, "%s%f", temp, &gui->spline.field_of_view );
    }
    else /* Make a guess :( */
    {
        DT_warn ( gui->shell, "Could not locate field of view... making a guess.", "Missing Information", NULL );
        gui->spline.field_of_view = 260.0;
    }
    gui->spline.valid.field_of_view = 1;
    
    if ( gui->spline.field_of_view > 100.0 )
    {
        /* For some reason, fov is in mm but the rest is in cm */
        gui->spline.x_pixel_size = gui->spline.field_of_view/2560.0;
        gui->spline.y_pixel_size = gui->spline.field_of_view/2560.0;
        half_fov = gui->spline.field_of_view/20.0;
        gui->spline.field_of_view *= 0.1;  /* Adjust field of view */
    }
    else
    {
        /* fov is same as rest */
        gui->spline.x_pixel_size = gui->spline.field_of_view/256.0;
        gui->spline.y_pixel_size = gui->spline.field_of_view/256.0;
        half_fov = gui->spline.field_of_view/2.0;
    }

    gui->spline.valid.x_pixel_size = 1;
    gui->spline.valid.y_pixel_size = 1;

    /* Going to assume these spline files are in cm */
    strcpy ( gui->spline.dimensionality, "cm" );
    gui->spline.valid.dimensionality = 1;
    
    /*
      gui->geom.inv_pixelsizecolumns = 1.0/gui->geom.pixelsizecolumns
      gui->geom.valid.inv_pixelsizecolumns = 1;
      gui->geom.inv_pixelsizerows = 1.0/gui->geom.pixelsizerows;
      gui->geom.valid.inv_pixelsizerows = 1;
    */
    
    gui->spline.image_slices = 0;
    
    eof = 0;
    while ( !eof ) /* Will break out when end of file is reached */
    {
        do {  /* Read until we find a slice */
            if ( !fgets ( line, MAX_LINE_SIZE, infile ) )  { eof = 1; break; }
        } while ( !strstr ( line, "begin slice" ) );
        if ( eof ) break;        

        gui->slice[gui->spline.image_slices].file_location = ftell ( infile );
        
        if ( !fgets ( line, MAX_LINE_SIZE, infile ) ) break;
        if ( !fgets ( line, MAX_LINE_SIZE, infile ) ) break;
        if ( !fgets ( line, MAX_LINE_SIZE, infile ) ) break;
        sscanf ( line, "%f", &z_value );
        
        if ( z_value > max_z_value )
            max_z_value = z_value;
        if ( z_value <= min_z_value )
            min_z_value = z_value;

        if ( !fgets ( line, MAX_LINE_SIZE, infile ) ) break;
        sscanf ( line, "%d", &gui->slice[gui->spline.image_slices].num_bodies );

        if ( ! ( gui->slice[gui->spline.image_slices].body
                 = ( body_info_t * ) malloc ( sizeof ( body_info_t ) * gui->slice[gui->spline.image_slices].num_bodies ) ) )
        {
            return ( 0 );
        }

        /* Read in all the bodies for the slice */
        for ( i = 0; i < gui->slice[gui->spline.image_slices].num_bodies; i++ )
        {
            do {  /* Read until we find start of the next body_slice */
                if ( !fgets ( line, MAX_LINE_SIZE, infile ) )  { eof = 1; break; }
            } while ( !strstr ( line, "begin body_slice" ) );
            if ( eof ) break;

            /* Get the location of this body in the file */
            gui->slice[gui->spline.image_slices].body[i].file_location = ftell ( infile );

            /* Get the name of this body */
            if ( !fgets ( line, MAX_LINE_SIZE, infile ) )  { eof = 1; break; }
            sscanf ( line, "%s", gui->slice[gui->spline.image_slices].body[i].bodyname );
            trimBodyName ( gui->slice[gui->spline.image_slices].body[i].bodyname );
            
            do {  /* Read until we find end of the current body_slice */
                if ( !fgets ( line, MAX_LINE_SIZE, infile ) )  { eof = 1; break; }

                /* Look for weird double decimals */
                if ( strstr ( line, ".." ) )
                {
                    DT_error ( gui->shell, DOUBLE_DECIMAL_ERROR, "Read Error", NULL );
                    DEBUG_TRACE_OUT printf ( "Leaving getValuesFromSplineFile\n" );    
                    removeMessage ( gui );
                    return ( 0 );                
                }
            } while ( !strstr ( line, "end body_slice" ) );
            if ( eof ) break;
        }

        gui->spline.image_slices++;
        /*  printf ( "Reading slice %d\n", gui->spline.image_slices ); */
        
        if ( eof ) break;
    }
    gui->spline.valid.image_slices = 1;
    
    sortBodiesForEachSlice ( gui );
    
    gui->spline.z_spacing = ( max_z_value - min_z_value ) / (float)(gui->spline.image_slices-1);
    gui->spline.valid.z_spacing = 1;

    gui->spline.rl_axis_min = -half_fov;
    gui->spline.valid.rl_axis_min = 1;

    gui->spline.rl_axis_max = half_fov;
    gui->spline.valid.rl_axis_max = 1;

    gui->spline.pa_axis_min = -half_fov;
    gui->spline.valid.pa_axis_min = 1;

    gui->spline.pa_axis_max = half_fov;
    gui->spline.valid.pa_axis_max = 1;

    gui->spline.is_axis_min = min_z_value - gui->spline.z_spacing/2.0;
    gui->spline.valid.is_axis_min = 1;

    gui->spline.is_axis_max = max_z_value + gui->spline.z_spacing/2.0;
    gui->spline.valid.is_axis_max = 1;

    fclose ( infile );
    DEBUG_TRACE_OUT printf ( "Leaving getValuesFromSplineFile\n" );    
    return ( 1 );
}


/* ===================================================================
   trimBodyName

   Trims the single quotes off of bodynames found in the spline file
   =================================================================*/
void trimBodyName ( char *bodyname )
{
    char temp[256];

    if ( bodyname[0] == '\'' )
        strcpy ( temp, &(bodyname[1]) );
    else
        strcpy ( temp, bodyname );

    if ( temp[strlen(temp)-1] == '\'' )
        temp[strlen(temp)-1] = '\0';

    strcpy ( bodyname, temp );
}


void sortBodiesForEachSlice ( main_gui_t *gui )
{
    int i, j;

    DEBUG_TRACE_IN printf ( "Entering sortBodiesForEachSlice\n" );
    
    for ( i = 0; i < gui->spline.image_slices; i++ )
        qsort ( gui->slice[i].body, gui->slice[i].num_bodies, sizeof ( body_info_t ), compareBodies );

    DEBUG_TRACE_OUT printf ( "Leaving sortBodiesForEachSlice\n" );
}


void buildBodyOrderList ( void )
{
    FILE *infile;
    char line[MAX_LINE_SIZE];
    char containment_file[MAX_LINE_SIZE];
    char *temp;
    
    DEBUG_TRACE_IN printf ( "Entering buildBodyOrderList\n" );

    temp = getenv ( "SERA_RESOURCES" );
    
    if ( temp )
    {
        strcpy ( containment_file, temp );
        strcat ( containment_file, "/SeraConvert/body_order.txt" );
    }
    else
    {
        printf ( "Could not locate SERA_RESOURCES directory.\nMake sure this environment variable has been set.\n" );
        DEBUG_TRACE_OUT printf ( "Leaving buildBodyOrderList\n" );
        exit ( 0 );
    }
        
    num_ordered_bodies = 0;
    
    if ( ! ( infile = fopen ( containment_file, "r" ) ) )
    {
        printf ( "Could not open %s\n", containment_file );
        DEBUG_TRACE_OUT printf ( "Leaving buildBodyOrderList\n" );
        exit ( 0 );
    }

    while ( fgets ( line, MAX_LINE_SIZE, infile ) )
    {
        if ( ! strchr ( line, '#' ) )
        {
            sscanf ( line, "%s", ordered_body[num_ordered_bodies] );
            num_ordered_bodies ++;
        }
    }

    fclose ( infile );
    DEBUG_TRACE_OUT printf ( "Leaving buildBodyOrderList\n" );
}
        

int compareBodies ( const void *b1, const void *b2 )
{
    body_info_t *body1 = ( body_info_t * ) b1;
    body_info_t *body2 = ( body_info_t * ) b2;
    int i;
    int body1_num = -1;
    int body2_num = -1;

    for ( i = 0; i < num_ordered_bodies; i++ )
    {
        if ( ! strcmp ( body1->bodyname, ordered_body[i] ) )
            body1_num = i;

        if ( ! strcmp ( body2->bodyname, ordered_body[i] ) )
            body2_num = i;
    }

    if ( body1_num == -1 )
    {
        printf ( "Could not locate %s in $SERA_RESOURCES/SeraConvert/body_order.txt,\n   please add this body.\n", body1->bodyname );
    }
    if ( body2_num == -1 )
    {
        printf ( "Could not locate %s in $SERA_RESOURCES/SeraConvert/body_order.txt,\n   please add this body.\n", body2->bodyname );
    }
    if ( body1_num == -1 || body2_num == -1 )
    {
        exit ( 1 );
    }

    return ( body1_num - body2_num );
}


void transferInfoToGeom ( main_gui_t *gui )
{
    DEBUG_TRACE_IN printf ( "Entering transferInfoToGeom\n" );

    /************************************************************
     *  First, try to get info from the qsh structure
     ***********************************************************/
    if ( gui->qsh_loaded )
    {
        /* Get number of slices */
        if ( gui->qsh.valid.size_of_dimension[0] )
        {
            gui->geom.imageslices = gui->qsh.size_of_dimension[0];
            gui->geom.valid.imageslices = 1;
        }
        if ( gui->qsh.valid.size_of_dimension[1] )
        {
            gui->geom.imagecolumns = gui->qsh.size_of_dimension[1];
            gui->geom.valid.imagecolumns = 1;
        }
        if ( gui->qsh.valid.size_of_dimension[2] )
        {
            gui->geom.imagerows = gui->qsh.size_of_dimension[2];
            gui->geom.valid.imagerows = 1;
        } 
        if ( gui->qsh.valid.uniform_spacing )
        {
            gui->geom.pixelsizeslices = gui->qsh.uniform_spacing;
            gui->geom.valid.pixelsizeslices = 1;

            if ( strcmp ( gui->qsh.dimensionality, "mm" ) == 0 )
                gui->geom.pixelsizeslices *= 0.1;
        }
        
        if ( gui->qsh.valid.slice_orientation )
        {
            strcpy ( gui->geom.sliceorientation, gui->qsh.slice_orientation );

            if ( strcmp ( gui->qsh.slice_orientation, "Axial" ) == 0 ||
                 strcmp ( gui->qsh.slice_orientation, "Transverse" ) == 0 )
            {
                strcpy ( gui->geom.imagecolumnaxis, "RL+" );
                strcpy ( gui->geom.imagerowaxis, "PA-" );
                strcpy ( gui->geom.imagesliceaxis, "IS+" );
            }
            else if ( strcmp ( gui->qsh.slice_orientation, "Coronal" ) == 0 )
            {
                strcpy ( gui->geom.imagecolumnaxis, "RL+" );
                strcpy ( gui->geom.imagerowaxis, "PA-" );
                strcpy ( gui->geom.imagesliceaxis, "IS-" );
            }
            else if ( strcmp ( gui->qsh.slice_orientation, "Sagittal" ) == 0 )
            {
                strcpy ( gui->geom.imagecolumnaxis, "RL-" );
                strcpy ( gui->geom.imagerowaxis, "PA-" );
                strcpy ( gui->geom.imagesliceaxis, "IS-" );
            } 
            else if ( strcmp ( gui->qsh.slice_orientation, "Oblique" ) == 0 )
            {
                printf ( "Oblique slice orientation not supported.  Defaulting to Axial.\n" );
                strcpy ( gui->geom.imagecolumnaxis, "RL+" );
                strcpy ( gui->geom.imagerowaxis, "PA-" );
                strcpy ( gui->geom.imagesliceaxis, "IS+" );
                
                strcpy ( gui->geom.sliceorientation, "Axial" );
            }
            else
            {
                printf ( "Unknown slice orientation: %s.  Defaulting to Axial.\n", gui->qsh.slice_orientation );
                strcpy ( gui->geom.imagecolumnaxis, "RL+" );
                strcpy ( gui->geom.imagerowaxis, "PA-" );
                strcpy ( gui->geom.imagesliceaxis, "IS+" );

                strcpy ( gui->geom.sliceorientation, "Axial" );
            }
            
            gui->geom.valid.sliceorientation = 1;
            gui->geom.valid.imagecolumnaxis = 1;
            gui->geom.valid.imagerowaxis = 1;
            gui->geom.valid.imagesliceaxis = 1;
        }
    }
    else
    {
        if ( gui->spline.valid.image_slices )
        {
            gui->geom.imageslices = gui->spline.image_slices;
            gui->geom.valid.imageslices = 1;
        }
        if ( gui->spline.valid.z_spacing )
        {
            gui->geom.pixelsizeslices = gui->spline.z_spacing;
            gui->geom.valid.pixelsizeslices = 1;
        }            
        
        strcpy ( gui->geom.sliceorientation, "Axial" );
        strcpy ( gui->geom.imagecolumnaxis, "RL+" );
        strcpy ( gui->geom.imagerowaxis, "PA-" );
        strcpy ( gui->geom.imagesliceaxis, "IS+" );
            
        gui->geom.valid.sliceorientation = 1;
        gui->geom.valid.imagecolumnaxis = 1;
        gui->geom.valid.imagerowaxis = 1;
        gui->geom.valid.imagesliceaxis = 1;
        
        gui->geom.imagecolumns = 256;
        gui->geom.valid.imagecolumns = 1;
        gui->geom.imagerows = 256;
        gui->geom.valid.imagerows = 1;
    }
            
    /* Get dimensionality */
    if ( gui->spline.valid.dimensionality )
    {
        strcpy ( gui->geom.dimensionality, gui->spline.dimensionality );
        gui->geom.valid.dimensionality = 1;
    }
    if ( gui->spline.valid.x_pixel_size )
    {
        gui->geom.pixelsizecolumns = gui->spline.x_pixel_size;
        gui->geom.valid.pixelsizecolumns = 1;
        gui->geom.inv_pixelsizecolumns = 1.0/gui->geom.pixelsizecolumns;
        gui->geom.valid.inv_pixelsizecolumns = 1;
    }
    if ( gui->spline.valid.y_pixel_size )
    {
        gui->geom.pixelsizerows = gui->spline.x_pixel_size;
        gui->geom.valid.pixelsizerows = 1;
        gui->geom.inv_pixelsizerows = 1.0/gui->geom.pixelsizerows;
        gui->geom.valid.inv_pixelsizerows = 1;
    }
    if ( gui->spline.valid.pa_axis_min )
    {
        gui->geom.paaxismin = gui->spline.pa_axis_min;
        gui->geom.valid.paaxismin = 1;
    }
    if ( gui->spline.valid.pa_axis_max )
    {
        gui->geom.paaxismax = gui->spline.pa_axis_max;
        gui->geom.valid.paaxismax = 1;
    }
    if ( gui->spline.valid.rl_axis_min )
    {
        gui->geom.rlaxismin = gui->spline.rl_axis_min;
        gui->geom.valid.rlaxismin = 1;
    }
    if ( gui->spline.valid.rl_axis_max )
    {
        gui->geom.rlaxismax = gui->spline.rl_axis_max;
        gui->geom.valid.rlaxismax = 1;
    }
    if ( gui->spline.valid.is_axis_min )
    {
        gui->geom.isaxismin = gui->spline.is_axis_min;
        gui->geom.valid.isaxismin = 1;
    }
    if ( gui->spline.valid.is_axis_max )
    {
        gui->geom.isaxismax = gui->spline.is_axis_max;
        gui->geom.valid.isaxismax = 1;
    }
    
    DEBUG_TRACE_OUT printf ( "Leaving transferInfoToGeom\n" );
}

        


void convertSplinesToUnivels ( main_gui_t *gui )
{
    int i, err;
    
    DEBUG_TRACE_IN printf ( "Entering processFilesForUnivels\n" );

    /* Make some assumptions for defaults -- overwritten by .uvh file */
    for ( i = 0; i < 256; i++ )
    {
        gui->geom.regionnum[i] = i;
        gui->geom.uvval[i] = i;
        sprintf ( gui->geom.bodyname[i], "body%d", i );
    }

    initialize_empty_slices ( &gui->geom );

    postMessage ( gui, "Reading spline file..." );

    err = readSplinesForUnivels ( gui, &gui->geom, gui->rs_filename );

    switch ( err )
    {
        case MIN_Z_VALUE_ERROR:
            DT_warn ( gui->shell, MIN_Z_VALUE_ERROR_STRING, "Minimum Z-value Problem", NULL );
            /** build the voxels **/
            postMessage ( gui, "Building univel regions..." );
            make_regions ( &gui->geom );
            
            /** update the univel files **/
            postMessage ( gui, "Writing univel header file..." );
            write_uvh ( &gui->geom, gui->uvh_filename );
            postMessage ( gui, "Writing univel file..." );
            write_uv ( &gui->geom, gui->uv_filename );
            break;
        case NON_UNIFORM_SPACING:
            DT_warn ( gui->shell, NON_UNIFORM_SPACING_STRING, "Non-uniform Spacing", NULL );
            /* Fall into NO_ERRORS case... */
        case NO_ERRORS:
            /** build the voxels **/
            postMessage ( gui, "Building univel regions..." );
            make_regions ( &gui->geom );
            
            /** update the univel files **/
            postMessage ( gui, "Writing univel header file..." );
            write_uvh ( &gui->geom, gui->uvh_filename );
            postMessage ( gui, "Writing univel file..." );
            write_uv ( &gui->geom, gui->uv_filename );
            break;
        case FILE_READ_ERROR:
            DT_error ( gui->shell, FILE_READ_ERROR_STRING, "File Read Error", NULL );
            removeMessage ( gui );
            break;
        case UNEXPECTED_LINE:
            DT_error ( gui->shell, UNEXPECTED_LINE_STRING, "File Read Error", NULL );
            removeMessage ( gui );
            break;
        default:
            DT_error ( gui->shell, "An unknown error occured while reading spline file.", "Unknown Error", NULL );
            removeMessage ( gui );
            break;
    }
        
    DEBUG_TRACE_OUT printf ( "Leaving processFilesForUnivels\n" );
}


/*************************************************************
 *
 *  Read rs for univels - written by Cory Albright
 *                      - Modified by Matt Cohen 8/18/99
 *
 *  This procedure opens the .rs file and extracts all of the
 *  needed info.  
 *
 *   for every slice
 *      for every body
 *            get the body info (ctrlpts, name, etc)
 *            rebuild the full curve from control points
 *             send the info into the voxelizer (cp_to_voxel)
 *
 *
 *
 *************************************************************/
int readSplinesForUnivels ( main_gui_t *gui, geom_info_t *geom_ptr, char *fname_rs )
{

    FILE *in_fp;
    int  i, j, k;  /** loop counters **/
    char slice_filename[INEL_BUFFER_SIZE];
    char body_name[INEL_BUFFER_SIZE];
    char body_names[INEL_BUFFER_SIZE][INEL_BUFFER_SIZE];
    char buffer1[INEL_BUFFER_SIZE];
    char buffer2[INEL_BUFFER_SIZE];
 
    float z_value;
     
    int num_bodies;
    int num_ctlpts, num_knots;
    int num_attributes, num_coords;
    int sliceNum;
    
    int region_num = 0;
    int num_regions = 0;
    int already_have_name =0;

    float *knots;
    float *ctl_pts;
    struct knot_vector refine_kv;

    struct cnurb curve;
    struct cnurb *new_curve;
    int curve_order, curve_type;

    /*** for reading unwanted info ***/
    int junk_int;
    float junk_float;
    char junk_string[30];

    /*int n;*/
    int retval = NO_ERRORS;

    DEBUG_TRACE_IN printf ( "Entering readSplinesForUnivels\n" );
    
    /*** open the rs file for reading ***/
    if ( ! ( in_fp = fopen ( fname_rs, "r" ) ) )
    {
        fprintf(stderr,"Could not open the file\n");
        fprintf(stderr, "File name attempted = %s\n", fname_rs);
        DEBUG_TRACE_OUT printf ( "Leaving readSplinesForUnivels\n" );
        return ( FILE_READ_ERROR );
    }

    /************************************
     * extract the needed info out of rs
     *************************************/

    /** skip the first line if necessary **/
    fgets ( buffer1, 255, in_fp );
    if ( !strstr ( buffer1, "fov" ) )
        rewind ( in_fp );
    
    /**** slice loop ****/
    for ( i = 0; i < gui->spline.image_slices; i++ )
    {
        fseek ( in_fp, gui->slice[i].file_location, SEEK_SET );
        
	/*** read in the slice filename ***/
	fscanf ( in_fp, "%s", slice_filename );
 
	/*** read in some un-needed info ***/
	fscanf ( in_fp, "%s", junk_string );

	/*** get the z value ***/
	fscanf ( in_fp, "%f", &z_value );

        /* Get the slice number for the regions */
        if ( gui->qsh_loaded )
            sliceNum = findQshSliceNumber ( gui, z_value );
        else
            sliceNum = i;

        /**** body loop ****/
        for ( j = 0; j < gui->slice[i].num_bodies; j++ )
        {
            fseek ( in_fp, gui->slice[i].body[j].file_location, SEEK_SET );            

            /** get the body name **/
	    fscanf ( in_fp, "%s", body_name );

            /*  printf("The body name is : %s\n",body_name); */	    
  
            /**** check to see if we have seen this body before *****/
            for ( k = 1; k < num_regions + 1; k++ )
	    {
		if ( strcmp ( body_names[k], body_name ) == 0)
                {
		    region_num = k;
		    already_have_name = 1;
                }
	    }
	    /*** if not, add it  to the list **/
	    if ( ! already_have_name )
	    {
                num_regions++;
		strcpy(body_names[num_regions],body_name);
		region_num = num_regions;
	    }

	    /*** reset ***/
	    already_have_name = 0;

	    /*** read in some un-needed info ***/
	    fscanf(in_fp,"%d %d %d %d %f %f %d %d",&junk_int,&junk_int,
		   &junk_int,&junk_int,&junk_float,&junk_float,
		   &junk_int,&junk_int);

	    /*** read in:  begin curve ***/
     	    fscanf(in_fp,"%s %s",buffer1, buffer2);
	    /*** if this is not what we got, quit ***/
	    if((strcmp(buffer1,"begin") != 0) && (strcmp(buffer2,"curve")!=0))
	    {
		fprintf ( stderr, "Did not read: begin curve\n");
		fprintf ( stderr, "Read   %s %s, instead\n", buffer1, buffer2 );
                DEBUG_TRACE_OUT printf ( "Leaving readSplinesForUnivels\n" );
		return ( UNEXPECTED_LINE );
	    }

	    /*** read in curve order and type ***/
	    fscanf(in_fp,"%d %d",&curve_order,&curve_type);
	    /*** copy them into the cnurb ***/
	    curve.order = curve_order;
      	    /** set up the pt_type correctly **/
	    curve.pt_type = RT_NURB_MAKE_PT_TYPE(curve_type,RT_NURB_PT_XY,0);

	    /*** read in the number of knots ***/
	    fscanf(in_fp,"%d",&num_knots);

            /*** allocate the space for the knots ***/
            knots = ( float * ) malloc ( sizeof ( float ) * num_knots );

            /**** knot loop ****/
	    for ( k = 0; k < num_knots; k++ )
	    {
		/** get the knot **/
		fscanf ( in_fp,"%f",&knots[k] );
	    }
	    
	    /** get the number of control points **/
	    fscanf(in_fp,"%d %d",&num_ctlpts,&num_coords);
	    /*** copy it to the cnurb ***/
	    curve.c_size = num_ctlpts;

            /*** allocate enough room for all the control points ***/
            ctl_pts = (float *)malloc (sizeof(float)*(num_ctlpts+3)*num_coords);
	    curve.ctl_points = (fastf_t *)
		malloc (sizeof(fastf_t)*(num_ctlpts+3)*num_coords);

	    /**** control point loop ****/
	    for ( k = 0; k < num_ctlpts * 2; k += 2 )
	    {
		/*** get the control point ***/
		fscanf(in_fp,"%f",&ctl_pts[k]);
		fscanf(in_fp,"%f",&ctl_pts[k+1]);

                /* copy it into the cnurb structure */
                curve.ctl_points[k+2] = (fastf_t)ctl_pts[k];
		curve.ctl_points[k+3] = (fastf_t)ctl_pts[k+1];
	    }

	    /**** need to close the loop, wrap control points ****/
	    /*** first add the last point to the beginning ***/
	    curve.ctl_points[0] = curve.ctl_points[num_ctlpts*2];
	    curve.ctl_points[1] = curve.ctl_points[num_ctlpts*2+1];
	    /*** then add the first two to the end ***/
	    curve.ctl_points[num_ctlpts*2+2] = curve.ctl_points[2];
	    curve.ctl_points[num_ctlpts*2+3] = curve.ctl_points[3];

	    curve.ctl_points[num_ctlpts*2+4] = curve.ctl_points[4];
	    curve.ctl_points[num_ctlpts*2+5] = curve.ctl_points[5];

	    /*************************************/
	    /*****  Need to refine the curve *****/
	    /*************************************/

	    /*** init the # of knots ***/
	   
      	    curve.knot.k_size = (num_ctlpts+1)+(curve.order -1)*2;
	    curve.knot.knots = (fastf_t *)malloc(sizeof(fastf_t)*
						 curve.knot.k_size);
	    /** make room for the knots **/

	    /*** first build the knot vector (rebuild) ***/
            for ( k = 0; k < curve.knot.k_size; k++ )
            { 
                curve.knot.knots[k] = knots[k] = (float) (k-2);
            }

	    /** next  build the new knot_vector **/
            rt_nurb_kvknot ( &refine_kv, curve.order, 0.0,
                             curve.knot.knots[curve.knot.k_size - curve.order],
                             num_knots * fineness );

            /***** refine the curve *****/
	    new_curve = rt_nurb_c_refine(&curve, &refine_kv);

	    /**** bring the refined points into the float array ****/
	    ctl_pts = (float *)malloc (sizeof(float)*new_curve->c_size*2);

	    for ( k = 0; k < (new_curve->c_size*2); k += 2 )
	    {
		ctl_pts[k] = (float)new_curve->ctl_points[k];
		ctl_pts[k+1] = (float)new_curve->ctl_points[k+1];
            }

	    /*********************************************/
	    /*** Pass the new points into the voxelizer **/
	    /*********************************************/
            my_cp_to_vox ( gui, new_curve->c_size, ctl_pts, region_num,
                           body_names[region_num], sliceNum ); /* Put region on the correct slice number */

	    /*** read in:  begin alist ***/
     	    fscanf ( in_fp,"%s %s", buffer1, buffer2 );
	    /*** if this is not what we got, quit ***/
	    if ( ( strcmp(buffer1,"begin") != 0) && ( strcmp(buffer2,"alist") != 0 ) )
	    {
		fprintf ( stderr, "Did not read: begin alist\n" );
		fprintf ( stderr, "Read   %s %s, instead\n", buffer1, buffer2 );
                DEBUG_TRACE_OUT printf ( "Leaving readSplinesForUnivels\n" );
		return ( UNEXPECTED_LINE );
	    }

	    /** get the number of attributes **/
	    fscanf ( in_fp, "%d", &num_attributes );
            
	    /*** read the attributes ***/
	    for ( k = 0; k < num_attributes; k++ )
	    {
		fgets ( buffer1, 120, in_fp );
	    }

            /*** read in:  end alist ***/
     	    fscanf ( in_fp,"%s %s", buffer1, buffer2 );
	    /*** if this is not what we got, quit ***/
	    if ( ( strcmp ( buffer1, "end" ) != 0 ) && ( strcmp ( buffer2, "alist" ) != 0 ) )
	    {
		fprintf ( stderr,"Did not read: end alist\n" );
                fprintf ( stderr, "Read   %s %s, instead\n", buffer1, buffer2 );
                DEBUG_TRACE_OUT printf ( "Leaving readSplinesForUnivels\n" );
		return ( UNEXPECTED_LINE );
	    }

            /*** read in:  end curve ***/
     	    fscanf ( in_fp, "%s %s", buffer1, buffer2 );
	    /*** if this is not what we got, quit ***/
	    if ( ( strcmp ( buffer1, "end" ) != 0 ) && ( strcmp ( buffer2, "curve" ) != 0 ) )
	    {
		fprintf ( stderr,"Did not read: end curve\n" );
                fprintf ( stderr, "Read   %s %s, instead\n", buffer1, buffer2 );
                DEBUG_TRACE_OUT printf ( "Leaving readSplinesForUnivels\n" );
		return ( UNEXPECTED_LINE );
	    }

	    /*** since the curve is no longer needed free the space ***/
	    free ( ctl_pts );
      	    free ( knots );

            free ( curve.ctl_points);
            free ( curve.knot.knots);

            free ( new_curve->ctl_points );
            free ( new_curve->knot.knots );

            free ( new_curve );

	    /*** read in:  end body_slice  ***/
     	    fscanf ( in_fp, "%s %s", buffer1, buffer2 );
	    /*** if this is not what we got, quit ***/
	    if ( ( strcmp(buffer1,"end") != 0) && ( strcmp(buffer2,"body_slice") != 0 ) )
	    {
		fprintf ( stderr, "Did not read: end body_slice\n" );
		fprintf ( stderr, "Read   %s %s, instead\n", buffer1, buffer2 );
                DEBUG_TRACE_OUT printf ( "Leaving readSplinesForUnivels\n" );
		return ( UNEXPECTED_LINE );
	    }

	} /* End reading body_slice */
    } /* End reading slices */

    fclose ( in_fp );

    DEBUG_TRACE_OUT printf ( "Leaving readSplinesForUnivels\n" );
    return ( retval );
}


int findQshSliceNumber ( main_gui_t *gui, float z_value )
{
    float factor = 0.1;  /* assume qsh info is in mm and spline info is cm */
    float diff, temp;
    int i, retval = 0;
    
    DEBUG_TRACE_IN printf ( "Entering findQshSliceNumber\n" );
    
    /* Check qsh dimensionality */
    if ( gui->qsh.valid.dimensionality )
        if ( strcmp ( gui->qsh.dimensionality, "cm" ) == 0 )
            factor = 1.0;

    /* initialize first slice to be closest match */
    diff = fabs ( gui->qsh.image_location[0]*factor - z_value );
    
    /* Look for a better match */
    for ( i = 0; i < gui->qsh.size_of_dimension[0]; i++ )
    {
        temp = fabs ( gui->qsh.image_location[i]*factor - z_value );
        if ( temp < diff )
        {
            diff = temp;
            retval = i;
        }
    }

    DEBUG_TRACE_OUT printf ( "Leaving findQshSliceNumber\n" );
    return ( retval );
}


/* This function is appearently needed by libuv... */
void process_files_for_univels(char *fname)
{
    return;
}


/******************************************************************
 * my_cp_to_vox
 ******************************************************************
 * A set of points in wc is passed that are closed to form a
 * polygon and then this region is filled in with the appropriate
 * body type
 * Format:  float pts[x0, y0, x1, y1, x2, y2, ...]
 * Currently, regnum (region number) must be a number between
 * 1 and 7, inclusive (0 represents void, and 8 represents boundary)
 ******************************************************************/
static void my_cp_to_vox ( main_gui_t *gui, int npoints, float *pts,
                           int regnum, char *bodyname, int planeno )
{
    int i, i2, xi1, xi2, yi1, yi2, xav, yav, top_y_index, left_x_index, 
        bot_y_index, right_x_index, index;
    float x1, x2, y1, y2;
    float xleft, yleft, xmid, ymid, xright, yright;
    float xv1, yv1, xv2, yv2, v1size, v2size, xvect, yvect;
    float top_y, bot_y, left_x, right_x;
    unsigned char regbit;
    int most=0;
    unsigned char index_most=0;
    static int freq[256];
    int num;
    geom_info_t *geom_ptr = &gui->geom;

    DEBUG_TRACE_IN printf ( "Entering my_cp_to_vox\n" );
    
    memset((void *)freq, 0, 256*sizeof(int));

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

    for ( i = 0; i < npoints; i++ )
    {
        i2  = (i+1) % npoints;
        xi1 = 2*i;
        yi1 = xi1+1;
        xi2 = 2*i2;
        yi2 = xi2+1;
        x1  = pts[xi1];
        y1  = pts[yi1];
        x2  = pts[xi2];
        y2  = pts[yi2];

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

        add_line ( geom_ptr, regbit, x1, y1, x2, y2, planeno );
    }

    /* If we aren't filling the regions, just return now */
    if ( !gui->filledRegions )
    {
        DEBUG_TRACE_OUT printf ( "Leaving my_cp_to_vox\n" );
        return;
    }
    
    regbit = regbit & 127; /* chop off uppermost bit */

    index = top_y_index;
    while (pts[top_y_index+1]<=pts[(index+3)%(2*npoints)]+EPSILON)
    {
        index=(index+2)%(2*npoints);
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
    
    /* just in case we have a vertical line */
    if ( yvect >= -EPSILON )
    {
        yvect = -1.0;
        xvect = 0.0;
    }
    
    my_fill_region_wvect ( geom_ptr, xmid, ymid, xvect, yvect, planeno, regbit );

    DEBUG_TRACE_OUT printf ( "Leaving my_cp_to_vox\n" );
}


/******************************************************************
 * static void my_fill_region_wvect
 ******************************************************************
 * Given a point in wc and a vector, moves along the vector to find
 * (hopefully) a point inside the region then calls other function
 * with that point to fill in the region
 ******************************************************************/
static void my_fill_region_wvect ( geom_info_t *geom, float xstart, float ystart, float xvect, float yvect,
                                   int planeno, unsigned char regbit )
{
    int xpt, ypt;
    float xf, yf, xfv, yfv, vsize, junk;
    int maxix, maxiy, maxiz, zoffset;
    unsigned char * arr;

    DEBUG_TRACE_IN printf ( "Entering my_fill_region_wvect\n" );
    
    arr = (unsigned char *)geom->vol_arr;

    maxix=geom->imagecolumns;
    maxiy=geom->imagerows;
    maxiz=geom->imageslices;
    zoffset = planeno*maxix*maxiy;

    /* Change from WC to NDC */

    xf = (xstart - geom->rlaxismin)*geom->inv_pixelsizecolumns;
    yf = (ystart - geom->paaxismin)*geom->inv_pixelsizerows;
    
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
            fprintf(stderr, "Error, error!  Out of bounds: xpt=%d ypt=%d\n",xpt, ypt);
            exit(EXIT_FAILURE);
        }
        /* Keep incrementing so long as we map to a region boundary */
    } while (arr[zoffset+(maxiy-1-ypt)*maxix+xpt]>=128);

    my_fill_region ( geom, xpt, maxiy-1-ypt, planeno, regbit );

    DEBUG_TRACE_OUT printf ( "Leaving my_fill_region_wvect\n" );
}


/******************************************************************
 * static void my_fill_region
 ******************************************************************
 * assumes fillbit is a number:  1, 2, 4, 8, 16, 32, or 64
 * assumes the boundary around x, y, z is has a 1 in highest bit (>=128)
 ******************************************************************/
static void my_fill_region ( geom_info_t *geom_ptr, int x, int y, int z, unsigned char fillbit )
{
    int ix, iy, iz, maxix, maxiy, maxiz;
    unsigned char *arr, *buffer;
    int xstop, ystop;
    int MAXSTACK;
    int i, j, mid_x, mid_y, index;
    int wh;
    Pt *stack, *sp, *top;

    DEBUG_TRACE_IN printf ( "Entering my_fill_region\n" );
    
    /* make a buffer and set all values to 0 */
    buffer = (unsigned char*)malloc(geom_ptr->imagecolumns*geom_ptr->imagerows*sizeof(unsigned char));
    memset((void*)buffer, 0, geom_ptr->imagecolumns*geom_ptr->imagerows );
    
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
    if ( arr[index]&fillbit )
    {
        /* In this case, started on border of the region
         * we're filling in -- not allowed! */
        DEBUG_TRACE_OUT printf ( "Leaving my_fill_region\n" );
        return;
    }
    else
    {
        arr[index]=(unsigned char)fillbit;
    }

    stack = (Pt *)malloc(MAXSTACK*sizeof(Pt));
    sp = stack;
    top = stack+MAXSTACK-1;

    PUSH ( x, y, &sp, top );

    while ( sp > stack )
    {
        POP ( &mid_x, &mid_y, &sp );
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
                if ((x<0)||(x>=maxix)||(y<0)||(y>=maxiy)){
                    /*printf ( "Out of Bounds!\n" );*/
                    continue;
                }

                index = y*maxix+x;
                /* mark the element and visit neighbors, if needed */
                /* All neighbors will be checked if the element gets marked */
                if (!buffer[index])
                {
                    buffer[index] = 1;
                    /*if ( (int)arr[index] != (int)(fillbit|128) )*/
                    if ( !(((int)arr[index])&128) )
                    {
                        arr[index]=(unsigned char)fillbit;

                        /* It wasn't a border so we visit the neighbors */
                        PUSH ( x, y, &sp, top );
                    }
                }
            }
        }
    }

    free((void*)stack);
    free ( (void*) buffer );

    DEBUG_TRACE_OUT printf ( "Leaving my_fill_region\n" );
}
