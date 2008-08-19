/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  File:         file_tools.c
%%%
%%%  Purpose:      Function definitions for common file routines.
%%%
%%%  Notes:        Function descriptions can be found in file_tools.h.
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "file_tools.h"


int FT_fileExists( char * filename )
{
    struct stat buf;
    int returnValue = 0;
    
    DEBUG_TRACE_IN printf("Entering FT_fileExists with %s\n", filename);

    /* stat() will return zero if filename exists */
    if( stat( filename, &buf ) == 0 )
        returnValue = 1;

    DEBUG_TRACE_OUT printf("Leaving FT_fileExists\n");
    return( returnValue );
}


int FT_isADirectory( char * filename )
{
    DIR * dirPtr;
    int returnValue = 0;
    
    DEBUG_TRACE_IN printf("Entering FT_isADirectory with %s\n", filename);

    if( filename != NULL )
    {
        /* Try to open as a directory */
        dirPtr = opendir( filename );
        if( dirPtr != NULL )
        {
            closedir( dirPtr );
            returnValue = 1;
        }
    }
    
    DEBUG_TRACE_OUT printf("Leaving isADirectory\n");
    return( returnValue );
}


int FT_filenameEndsIn( char * filename, char * suffix )
{
    int lengthOfFilename;
    int lengthOfSuffix;
    int i;
    char * ptr;
    int returnValue = 0;
    
    DEBUG_TRACE_IN printf("Entering FT_filenameEndsIn\n");

    lengthOfFilename = strlen( filename );
    lengthOfSuffix   = strlen( suffix   );

    /* Make sure suffix can fit into filename */
    if( lengthOfFilename >= lengthOfSuffix )
    {
        ptr = &filename[ lengthOfFilename ];

        for( i = 0; i < lengthOfSuffix; i++ )
            ptr--;

        if( strcmp( ptr, suffix ) == 0 )
            returnValue = 1;
    }

    DEBUG_TRACE_OUT printf("Leaving FT_filenameEndsIn\n");
    return( returnValue );
}


void FT_deleteFile( char * filename )
{
    DEBUG_TRACE_IN printf("Entering FT_deleteFile with %s\n", filename);

    if( FT_fileExists( filename ) )
    {
        if( remove( filename ) != 0 )
            perror( "on remove" );
    }

    DEBUG_TRACE_OUT printf("Leaving FT_deleteFile\n");
}


int FT_sizeOfFile( char * filename )
{
    FILE * file;
    int returnValue = 0;

    DEBUG_TRACE_IN printf("Entering FT_sizeOfFile, filename = %s\n", filename);

    file = fopen( filename, "r" );

    if( file != NULL )
    {
        fseek( file, (long)0, SEEK_END );
        returnValue = (int) ftell( file );
        fclose( file );
    }

    DEBUG_TRACE_OUT printf("Leaving FT_sizeOfFile, file size = %d\n", returnValue);
    return( returnValue );
}
