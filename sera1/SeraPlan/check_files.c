/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  File:        check_files.c
%%%
%%%  Purpose:     Definitions of procedures used to do preliminary checks
%%%               of rst files.
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#include <stdlib.h>
#include <stdio.h>
#include "check_files.h"
#include "debug_tools.h"
#include "dialog_tools.h"
#include "file_tools.h"


void checkFilenamesInRstFile( char * filename, unsigned int * flags )
{
    FILE * file;
    char buffer[256];
    char string[128];
    int numSets;
    int conversions;
    char * status;

    DEBUG_TRACE_IN printf("Entering checkFilenamesInRstFile\n");
    
    /* Zero out the flags */
    *flags = (unsigned int) 0;

    /*
     * If we find something at the top of the given filename
     * that is missing or in the wrong format, etc, we will
     * set a bit in the flags variable using the masks
     * defined in check_files.h. Upon return, these
     * bits can be checked, and appropriate messages can
     * be displayed telling the user that their rst file
     * appears to be invalid.
     */

    /*
     * If the file doesn't exist, we'll just set
     * the FILE_NO_EXIST_MASK, and that's it.
     */
    
    file = fopen( filename, "r" );
    if( file )
    {
        /* Read first line, should contain number of sets and version stamp */
        status = fgets( buffer, 256, file );
        if( status )
        {
            conversions = sscanf( buffer, "%4d%s", &numSets, string );
            if( conversions != 2 || strstr( string, "seraMC_" ) == NULL )
                *flags |= FORMAT_MASK;
        }
        else
            *flags |= FORMAT_MASK;

        /* Read second line, should contain the plan filename or "none" */
        status = fgets( buffer, 256, file );
        if( status )
        {
            conversions = sscanf( buffer, "%s", string );
            if( conversions == 1 )
            {
                if( strcmp( string, "none" ) != 0 ) /* if not 'none' */
                    if( !FT_fileExists( string ) )
                        *flags |= PLAN_FILE_MASK;
            }
            else
                *flags |= FORMAT_MASK;
        }
        else
            *flags |= FORMAT_MASK;

        /* Read third line, this should be the title */
        status = fgets( buffer, 256, file );
        if( status )
        {
            conversions = sscanf( buffer, "%s", string );
            if( conversions != 1 )
                *flags |= TITLE_MASK;
        }
        else
            *flags |= FORMAT_MASK;

        /* Read fourth line, should be the geometry file name */
        status = fgets( buffer, 256, file );
        if( status )
        {
            conversions = sscanf( buffer, "%s", string );
            if( conversions != 1 || !FT_fileExists( string ) || !FT_filenameEndsIn( string, ".geom" ) )
                *flags |= GEOM_FILE_MASK;
        }
        else
            *flags |= FORMAT_MASK;

        /* Read fifth line, should be the .uv file */
        status = fgets( buffer, 256, file );
        if( status )
        {
            conversions = sscanf( buffer, "%s", string );

            if( conversions == 1 )
            {
                if( FT_filenameEndsIn( string, ".uv" ) )
                {
                    if( FT_fileExists( string ) )
                    {
                        /* Now see if the .uvh file exists too */
                        strcat( string, "h" );
                        if( !FT_fileExists( string ) ) *flags |= NO_UVH_MASK;
                    }
                    else
                        *flags |= UV_FILE_MASK;
                }
                else
                    *flags |= UV_FILE_MASK;
            }
            else
                *flags |= UV_FILE_MASK;
        }
        else
            *flags |= FORMAT_MASK;

        /* Read sixth line, should be mat file */
        status = fgets( buffer, 256, file );
        if( status )
        {
            conversions = sscanf( buffer, "%s", string );
            if( conversions != 1 || !FT_fileExists( string ) || !FT_filenameEndsIn( string, ".mat" ) )
                *flags |= MAT_FILE_MASK;
        }
        else
            *flags |= FORMAT_MASK;

        /* Read seventh line, should be sigma file */
        status = fgets( buffer, 256, file );
        if( status )
        {
            conversions = sscanf( buffer, "%s", string );
            if( conversions != 1 || !FT_fileExists( string ) )
                *flags |= SIGMA_FILE_MASK;
        }
        else
            *flags |= FORMAT_MASK;
        
        fclose( file );
    }
    else  /* the file doesn't exist */
    {
        *flags |= FILE_NO_EXIST_MASK;
    }

    DEBUG_TRACE_OUT printf("Leaving checkFilenamesInRstFile\n");
}


void displayResultsOfRstCheck( Widget parent, char * filename, unsigned int flags )
{
    char errorString[1024];
    char error[256];
    unsigned int status;
    int valid = 1;
    int i;

    DEBUG_TRACE_IN printf("Entering displayResultsOfRstCheck\n");
    
    sprintf( errorString, "The rst file\n%s\ncould not be read because of the following:\n\n", filename );

    /*
     * Just check the appropriate bits in flags using the
     * masks in check_files.h. If a bit is set, add
     * an error message to the full errorString.
     */

    /* Check that the file exists */
    status = flags & FILE_NO_EXIST_MASK;
    if( status )
    {
        sprintf( error, "The rst file does not exist\n" );
        strcat ( errorString, error );
        valid = 0;
    }
        
    /* Check for format */
    status = flags & FORMAT_MASK;
    if( status )
    {
        sprintf( error, "The rst file is not properly formatted\n" );
        strcat ( errorString, error );
        valid = 0;
    }

    /* Check for plan file */
    status = flags & PLAN_FILE_MASK;
    if( status )
    {
        sprintf( error, "The plan file name is not valid. Line 2\n" );
        strcat ( errorString, error );
        valid = 0;
    }

    /* Check for title */
    status = flags & TITLE_MASK;
    if( status )
    {
        sprintf( error, "The title of the plan is not present. Line 3\n" );
        strcat ( errorString, error );
        valid = 0;
    }

    /* Check for geom file */
    status = flags & GEOM_FILE_MASK;
    if( status )
    {
        sprintf( error, "The .geom file name is not valid. Line 4\n" );
        strcat ( errorString, error );
        valid = 0;
    }

    /* Check for uv file */
    status = flags & UV_FILE_MASK;
    if( status )
    {
        sprintf( error, "The .uv file name is not valid. Line 5\n" );
        strcat ( errorString, error );
        valid = 0;
    }

    /* Check that the corresponding uvh file exists too */
    status = flags & NO_UVH_MASK;
    if( status )
    {
        sprintf( error, "The .uvh file corresponding to the .uv file does not exist\n" );
        strcat ( errorString, error );
        valid = 0;
    }
    

    /* Check for mat file */
    status = flags & MAT_FILE_MASK;
    if( status )
    {
        sprintf( error, "The .mat file name is not valid. Line 6\n" );
        strcat ( errorString, error );
        valid = 0;
    }

    /* Check for sigma file */
    status = flags & SIGMA_FILE_MASK;
    if( status )
    {
        sprintf( error, "The sigma file name is not valid. Line 7\n" );
        strcat ( errorString, error );
        valid = 0;
    }

    if( !valid )
    {
        strcat( errorString, "\nPlease check your rst file" );
        DT_error( parent, errorString, "rst File Error", NULL );
    }

    DEBUG_TRACE_OUT printf("Leaving displayResultsOfRstCheck\n");
}
