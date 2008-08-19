/*********************************************************************************/
/* libsz.c                                                                       */
/*                                                                               */
/* Library of zipping utilities.                                                 */
/*********************************************************************************/
#include "libsz.h"


/*============================================================================
  Function:         SZ_ZipFile

  Purpose:          Zips a file into another file.

  Parameters:       char *sourceFile  -  The name of the file to zip.
                    char *destFile    -  The zipped filename.
                    int verbose       -  Flag to indicate to print status
                    int keepfile      -  Indicates whether to delet orginal

  Returned:         int               -  Indicates success

  Author:           Matt Cohen
  ==========================================================================*/
int SZ_ZipFile ( char *sourceFile, char *destFile, int verbose, int keepFile )
{
    FILE *infile, *outfile;
    Bytef *compr, *uncompr;
    uLong comprLen, uncomprLen;
    int factor = 1;
    int i, err;

    /* Open the infile */
    if ( ! ( infile = fopen ( sourceFile, "r" ) ) )
    {
        printf ( "Could not open %s.\n", sourceFile );
        return ( 0 );
    }

    /* Determine length of the infile */
    fseek ( infile, 0, 2 );
    uncomprLen = ftell ( infile );
    rewind ( infile );

    /* Print out status */
    if ( verbose )
        printf ( "Original file size is %ld.\n", uncomprLen );

    /* Allocate memory for file data */
    if ( ! ( uncompr = ( Bytef * ) calloc ( ( uInt ) uncomprLen, 1 ) ) )
    {
        printf ( "Error allocating memory in SZ_ZipFile.\n" );
        fclose ( infile );
        return ( 0 );
    }

    /* Read the infile into the array */
    fread ( uncompr, sizeof ( Bytef ), uncomprLen, infile );

    /* Try to compress 10 times */
    for ( i = 0; i < 10; i++ )
    {
        /* factor increases by one with each try */
        comprLen = uncomprLen * factor;

        /* Allocate memory for unzipped data */
        if ( ! ( compr = ( Byte * ) calloc ( ( uInt ) comprLen, 1 ) ) )
        {
            printf ( "Error allocating memory in SZ_ZipFile.\n" );
            free ( uncompr );
            fclose ( infile );
            return ( 0 );
        }

        /* Do the compression */
        err = compress2 ( compr, &comprLen,
			  ( const Bytef * ) uncompr, uncomprLen,
			  Z_BEST_SPEED );

        /* Check if compression was successful */
        if ( err == Z_OK )
        {
            break;
        }
        else if ( err == Z_BUF_ERROR )
        {
            free ( compr );
            factor++;   /* if not successful, increase factor */
        }
    }

    /* Free the uncompressed array */
    free ( uncompr );

    /* In case still couldn't compress after 10 tries */
    if ( err != Z_OK )
    {
        printf ( "Unable to compress file.\n" );
        fclose ( infile  );

        return ( 0 );
    }
    else /* Create compressed file */
    {
        if ( verbose )  /* Report status */
            printf ( "Compressed file size is %ld.\n", comprLen );

        /* Open outfile */
        if ( ! ( outfile = fopen ( destFile, "w" ) ) )
        {
            printf ( "Could not open %s.\n", destFile );

            free ( compr );
            fclose ( infile  );
            return ( 0 );
        }

        /* Write the file */
        fwrite ( compr, sizeof ( Bytef ), comprLen, outfile );

        /* Free and close */
        free ( compr );
        fclose ( infile  );
        fclose ( outfile );

        /* Delete original if desired */
        if ( !keepFile )
            SZ_DeleteFile( sourceFile );
        
        return ( 1 );    /* Success */
    }
}


/*============================================================================
  Function:         SZ_UnzipFile

  Purpose:          Unzips a file into another file.

  Parameters:       char *sourceFile  -  The name of the file to unzip.
                    char *destFile    -  The unzipped filename.
                    int verbose       -  Flag to indicate to print status
                    int keepfile      -  Indicates whether to delet orginal

  Returned:         int               -  Indicates success

  Author:           Matt Cohen
  ==========================================================================*/
int SZ_UnzipFile ( char *sourceFile, char *destFile, int verbose, int keepFile )
{
    FILE *infile, *outfile;
    Bytef *compr;
    Bytef *uncompr;
    uLong comprLen;
    uLong uncomprLen;
    char outFilename[256];
    char *endOfName;
    int factor = 2;
    int i, err;

    /* Make sure it is a seraZipped file first */
    if ( !SZ_IsASeraZippedFile ( sourceFile ) )
    {
        printf ( "Error! %s in wrong format\n", sourceFile );
        return ( 0 );
    }

    /* Open the file */
    if ( ! ( infile = fopen ( sourceFile, "r" ) ) )
    {
        printf ( "Could not open %s.\n", sourceFile );
        return ( 0 );
    }
    
    /* Find file size */
    fseek ( infile, 0, 2 );
    comprLen = ftell ( infile );
    rewind ( infile );

    /* Allocate memory for data */
    if ( ! ( compr = ( Byte * ) calloc ( ( uInt ) comprLen, 1 ) ) )
    {
        printf ( "Error allocating memory in SZ_UnzipFile.\n" );
        fclose ( infile );
        return ( 0 );
    }
    
    if ( verbose ) /* Report status */   
        printf ( "Compressed file size is %ld.\n", comprLen );

    /* Read file into array */
    fread ( compr, sizeof ( Bytef ), comprLen, infile );

    /* Try to uncompress 10 times */
    for ( i = 0; i < 10; i++ )
    {
        /* Factor doubles with each time */
        uncomprLen = comprLen * factor;

        /* Allocate memory for uncompressed data */
        if ( ! ( uncompr = ( Byte * ) calloc ( ( uInt ) uncomprLen, 1 ) ) )
        {
            printf ( "Unable to allocate memory in SZ_UnzipFile.\n" );
            fclose ( infile );
            free ( compr );
        }

        /* Do the uncompression */
        err = uncompress ( uncompr, &uncomprLen, compr, comprLen );

        /* Check if uncompression was successful */
        if ( err == Z_OK )
        {
            break;
        }
        else if ( err == Z_BUF_ERROR )
        {
            free ( uncompr );  
            factor *= 2;         /* Increase factor */
        }
    }

    /* Free the compressed data */
    free ( compr );

    /* Check if success after 10 tries */
    if ( err != Z_OK )
    {
        printf ( "Unable to uncompress file.\n" );

        fclose ( infile  );

        return ( 0 );
    }
    else  /* Create the file */
    {
        if ( verbose ) /* Status report */
            printf ( "Uncompressed file size is %ld.\n", uncomprLen );

        /* Open the outfile */
        if ( ! ( outfile = fopen ( destFile, "w" ) ) )
        {
            printf ( "Could not open %s.\n", destFile );

            free ( uncompr );
            fclose ( infile );

            return ( 0 );
        }

        /* Write uncompessed data to outfile */
        fwrite ( uncompr, sizeof ( Bytef ), uncomprLen, outfile );

        /* Free and close */
        free ( uncompr );
        fclose ( infile  );
        fclose ( outfile );

        /* Remove file if so desired */
        if ( !keepFile )
            SZ_DeleteFile( sourceFile );
        
        return ( 1 );    /* Success */
    }
}


/*============================================================================
  Function:         SZ_UnzipFileIntoArray

  Purpose:          Unzips a file, allocates memory and then stores the file
                    contents in the array.

  Parameters:       char *filename    -  The name of the zipped file.
                    char *array       -  The array to store the file contents.
                    int *arraySize    -  The size of the array is returned.
                    
  Returned:         int               -  Indicates success

  Author:           Matt Cohen
  ==========================================================================*/
int SZ_UnzipFileIntoArray ( char *filename, char **array, int *arraySize )
{
    int retval;
    
    retval = SZ_UnzipFileIntoBinaryArray ( filename, (Bytef**)array,
                                           (uLong*)arraySize, 1 );

    /* Append a null terminator at the end of the array    */
    /* I think zlib already does this, but just in case... */
    if ( retval )
    {
        (*array)[*arraySize] = '\0';
        *arraySize += 1;
    }
    
    return ( retval );
}


/*============================================================================
  Function:         SZ_ZipArrayIntoFile

  Purpose:          Compresses an array and writes the compressed data into
                    a file.

  Parameters:       char *filename    -  The name of the zipped file.
                    char *array       -  The array storing the data.
                    int *arraySize    -  The size of the array.
                    
  Returned:         int               -  Indicates success

  Author:           Matt Cohen
  ==========================================================================*/
int SZ_ZipArrayIntoFile ( char *filename, char *array, int arraySize )
{
    return ( SZ_ZipBinaryArrayIntoFile ( filename, (Bytef*)array,
                                         (uLong)arraySize, 1 ) );
}


/*============================================================================
  Function:         SZ_UnzipFileIntoBinaryArray

  Purpose:          Unzips a file, allocates memory and then stores the file
                    contents in the array.

  Parameters:       char *filename    -  The name of the zipped file.
                    Bytef *array      -  The array to store the file contents.
                    uLong *arraySize  -  The size of the array is returned.
                    int dataSize      -  The size of one element in the array.
                    
  Returned:         int               -  Indicates success

  Author:           Matt Cohen
  ==========================================================================*/
int SZ_UnzipFileIntoBinaryArray ( char *filename, Bytef **array,
                                  uLong *arraySize, int dataSize )
{
    FILE *infile;
    Bytef *compr;
    Bytef *uncompr;
    uLong comprLen;
    uLong uncomprLen;
    int factor = 2;
    int i, err;

    *arraySize = 0;   /* Set size to zero first */
    
    /* Make sure it is a seraZip file */
    if( !SZ_IsASeraZippedFile( filename ) )
    {
        printf ( "%s does not appear to be a seraZip file\n", filename );
        return ( 0 );
    }

    /* Open file */
    if ( ! ( infile = fopen ( filename, "r" ) ) )
    {
        printf ( "Could not open %s.\n", filename );
        return ( 0 );
    }

    /* Find file size */
    fseek ( infile, 0, 2 );
    comprLen = ftell ( infile );
    rewind ( infile );

    /* Malloc memory for compressed data */
    if ( ! ( compr = ( Byte * ) calloc ( ( uInt ) comprLen, 1 ) ) )
    {
        printf ( "Unable to allocate memory.\n" );
        fclose ( infile );
        return ( 0 );
    }
    
    /* Read data from file into uncompressed data array */
    fread ( compr, sizeof ( Bytef ), comprLen, infile );

    /* Close the file */
    fclose ( infile );
    
    for ( i = 0; i < 10; i++ ) /* Try to uncompress 10 times */
    {
        uncomprLen = comprLen * factor;
        
        if ( ! ( uncompr = ( Byte * ) calloc ( ( uInt ) uncomprLen, 1 ) ) )
        {
            printf ( "Unable to allocate memory.\n" );
            return ( 0 );
        }
        
        /* Try to uncompress the data */
        err = uncompress ( uncompr, &uncomprLen, compr, comprLen );

        if ( err == Z_OK )
        {
            break;   /* Successful uncompress */
        }
        else if ( err == Z_BUF_ERROR )
        {
            free ( uncompr );      /* Free array         */
            factor *= 2; /*++;*/   /* Try a bigger array */
        }
    }

    if ( err != Z_OK )
    {
        printf ( "Unable to uncompress file.\n" );
        return ( 0 );
    }
    else
    {
        /* Malloc memory for the array */
        /* Adding one here incase this is a character array
           and we want to have a null character at the end */
        if ( ! ( *array = ( Bytef * ) malloc ( uncomprLen + 1 ) ) )
        {
            printf ( "Unable to allocate memory.\n" );
            free ( uncompr );  /* Free uncompressed data array */
            return ( 0 );
        }
        
        /* Copy uncompressed data into array */
        memcpy ( *array, uncompr, uncomprLen );
                
        /* Free uncompressed data array */
        free ( uncompr );

        *arraySize = uncomprLen/dataSize; /* Return array size on success */
        return ( 1 );                     /* Return 1 for success */
    }
}


/*============================================================================
  Function:         SZ_ZipBinaryArrayIntoFile

  Purpose:          Zips a data array into a file.

  Parameters:       char *filename    -  The name of the zipped file.
                    Bytef *array      -  The array storing the data.
                    uLong *arraySize  -  The size of the array.
                    int dataSize      -  The size of one element in array.
                    
  Returned:         int               -  Indicates success

  Author:           Matt Cohen
  ==========================================================================*/
int SZ_ZipBinaryArrayIntoFile ( char *filename, Bytef *array,
                                uLong arraySize, int dataSize )
{
    FILE *outfile;
    char *sz_filename = NULL;
    Bytef *compr;
    Bytef *uncompr;
    uLong comprLen;
    uLong uncomprLen;
    int factor = 1;
    int i, err;

    uncomprLen = arraySize * dataSize;
    
    /* Malloc memory for uncompressed data */
    if ( ! ( uncompr = ( Byte * ) calloc ( ( uInt ) uncomprLen, 1 ) ) )
    {
        printf ( "Unable to allocate memory.\n" );
        return ( 0 );
    }

    /* Copy array into uncompr */
    memcpy ( uncompr, array, uncomprLen );
    
    for ( i = 0; i < 10; i++ ) /* Try to compress 10 times */
    {
        comprLen = uncomprLen * factor;
        
        if ( ! ( compr = ( Byte * ) calloc ( ( uInt ) comprLen, 1 ) ) )
        {
            printf ( "Unable to allocate memory.\n" );
            return ( 0 );
        }

        /* Try to do the compression */
        err = compress2 ( compr, &comprLen,
			  ( const Bytef * ) uncompr, uncomprLen,
			  Z_BEST_SPEED );
        
        if ( err == Z_OK )
        {
            break;   /* Successful compress */
        }
        else if ( err == Z_BUF_ERROR )
        {
            free ( compr );      /* Free array         */
            factor ++;           /* Try a bigger array */
        }
    }

    free ( uncompr );
    
    if ( err != Z_OK )
    {
        printf ( "Unable to compress array.\n" );
        return ( 0 );
    }
    else
    {
        /* Make sure it is a seraZip filename */
        SZ_MakeIntoSeraZippedFilename ( filename, &sz_filename );

        /* Open file */
        if ( ! ( outfile = fopen ( sz_filename, "w" ) ) )
        {
            printf ( "Could not open %s.\n", sz_filename );

            free ( compr );

            /* Free memory */
            if ( sz_filename )
                free ( sz_filename );

            return ( 0 );
        }
        else
        {
            /* Write the file */
            fwrite ( compr, sizeof ( Bytef ), comprLen, outfile );                

            /* Free uncompressed data array */
            free ( compr );
            fclose ( outfile );
        }

        /* Free memory */
        if ( sz_filename )
            free ( sz_filename );
        
        return ( 1 );   /* Return 1 for success */
    }
}


/*============================================================================
  Function:         SZ_ReadSwitches

  Purpose:          Scans through command line switches.  Used for seraZip
                    and seraUnzip

  Parameters:       char *str         -  The string containing the switches.
                    switches_t *s     -  Return value container.
                    
  Returned:         int               -  Indicates success

  Author:           Matt Cohen
  ==========================================================================*/
int SZ_ReadSwitches ( char *str, switches_t *s )
{
    char *zarg;

    s->keepFile = 1;
    s->nameFile = 0;
    s->verbose = 0;
    s->help = 0;
    
    if ( str[0] == '-' )
    {
        zarg = str;
        zarg++;
        
        while ( zarg[0] != '\0' )
        {
            switch ( zarg[0] )
            {
                case 'r':
                    s->keepFile = 0;
                    break;
                case 'n':
                    s->nameFile = 1;
                    break;
                case 'h':
                    s->help = 1;
                    break;
                case 'v':
                    s->verbose = 1;
                    break;
                default:
                    printf ( "Unknown switch: -%c\n", zarg[0] );
                    break;
            }
            zarg ++;
        }
        return ( 1 );
    }
    else
    {
        return ( 0 );
    }
}


void SZ_PrintHelp ( void )
{
    printf ( "\n    Sera(Un)Zip Help:\n" );
    printf ( "       -r     Removes original file.\n" );
    printf ( "       -n     Rename the original file.  For example:\n" );
    printf ( "                    seraZip -n file1.txt file2.txt\n" );
    printf ( "              results in a zipped file named\n" );
    printf ( "                    file2.txt.sz\n" );
    printf ( "       -v     Verbose mode.\n" );
    printf ( "       -h     Print out this information.\n\n" );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     SZ_IsASeraZippedFile
%%% 
%%% Purpose:      Determine if a given file is in the sera zipped format.
%%%               It does this by seeing if the file name ends in .sz.
%%% 
%%% Parameters:   filename -> A NULL-terminated string representing the
%%%                           name of a file to check.
%%% 
%%% Return Value: 1 if the file ends in .sz
%%%               0 if the file doesn't end in .sz
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int SZ_IsASeraZippedFile( char * filename )
{
    int returnValue = 0;
    char * ptr;

    if ( strlen( filename ) > 3 )
    {
        /* First find the period starting from the end of the filename */
        ptr = strrchr ( filename, '.' );
    
        if ( ptr != NULL )
        {
            if ( strcmp( ptr, ".sz" ) == 0 )
                returnValue = 1;
        }
    }
    
    return ( returnValue );
}


/*============================================================================
  Function:         SZ_MakeIntoSeraZippedFilename

  Purpose:          Checks if a filename ends in .sz and adds it if not.

  Parameters:       char *filename    -  The name of the zipped file.
                    char **sz_filename - The new filename
                    
  Returned:         None.

  Author:           Matt Cohen
  ==========================================================================*/
void SZ_MakeIntoSeraZippedFilename ( char *filename, char **sz_filename )
{
    int length;

    length = strlen ( filename );
    
    if ( length < 1 )
    {
        *sz_filename = ( char * ) malloc ( 16 );
        strcpy ( *sz_filename, "sz_temp_file.sz" );
    }
    else if ( strstr ( filename, ".sz" ) )
    {
        if ( length == 3 )
        {
            *sz_filename = ( char * ) malloc ( 16 );
            strcpy ( *sz_filename, "sz_temp_file.sz" );
        }
        else
        {
            *sz_filename = ( char * ) malloc ( length + 1 );
            strcpy ( *sz_filename, filename );
        }    
    }
    else
    {
        *sz_filename = ( char * ) malloc ( length + 4 );
        strcpy ( *sz_filename, filename );
        strcat ( *sz_filename, ".sz" );
    }
}

            

    

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     SZ_DeleteFile
%%% 
%%% Purpose:      Delete a file from the system.
%%% 
%%% Parameters:   filename -> A NULL-terminated string representing the
%%%                           name of the file to delete.
%%% 
%%% Return Value: none
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void SZ_DeleteFile( char * filename )
{
    char systemCall[256];

    /* Form the string to send to the system */
    sprintf( systemCall, "rm -f %s", filename );

    system( systemCall );
}


/*============================================================================
  Function:         SZ_FreeArray

  Purpose:          This call is for consistancy.  It would be just as easy
                    to free the array from within the calling procedure, but
                    this way seems to keep things understandable.

  Parameters:       char *array        -  The array to free.
                    
  Returned:         None.

  Author:           Matt Cohen
  ==========================================================================*/
void SZ_FreeArray ( char **array )
{
    free ( *array );
    *array = NULL;
}
