/*******************************************************************************
 * szip.c
 *
 * Builds seraZip executable
 ******************************************************************************/
#include "libsz.h"

/* Print out usage of seraZip to user */
void printUsage ( void )
{
    printf ( "\nUSAGE: seraZip [-[r][n][v][h]] infilename [outfilename]\n" );
    printf ( "    Try -h for help.\n\n" );
}


int main ( int argc, char *argv[] )
{
    char outFilename[256];
    char inFilename[256];
    char *sz_outFilename;
    int numFiles = 1;
    switches_t s;

    /* Make sure there are the correct number of arguments */
    if ( argc > 1 && argc < 5 )
    {
        /* Read any possible switches */
        if ( SZ_ReadSwitches ( argv[1], &s ) )
        {
            /* If there are filenames */
            if ( argc > 2 )
            {
                strcpy ( inFilename, argv[2] );
            }
            else /* No filename included */
            {
                if ( s.help )
                    SZ_PrintHelp ( );
                else
                    printUsage ( );
                exit ( 0 );
            }

            /* If an outfilename was specified */
            if ( argc > 3 )
            {    
                strcpy ( outFilename, argv[3] );
                numFiles = 2;
            }
            
        }
        else  /* No switches were provided */
        {
            if ( argc > 1 ) /* Make sure there are filenames */
            {
                strcpy ( inFilename, argv[1] );
            }
            else
            {
                printUsage ( );
                exit ( 0 );
            }

            /* If an outfile was specified */
            if ( argc > 2 )
            {
                strcpy ( outFilename, argv[2] );
                numFiles = 2;
            }
        }
    }
    else /* No arguments */
    {
        printUsage ( );
        exit ( 0 );
    }
    
    /* If user used -h */
    if ( s.help )
    {
        SZ_PrintHelp ( );
        exit ( 0 );
    }

    /* user used -n but didn't provide an outfile */
    if ( s.nameFile && numFiles != 2 )
    {
        printUsage ( );
        exit ( 0 );
    }

    /* user didn't use -n but provided an outfile */
    if ( !s.nameFile && numFiles == 2 )
    {
        printUsage ( );
        exit ( 0 );
    }

    /* Copy the infile to the outfile */
    if ( !s.nameFile && numFiles != 2 )
    {
        strcpy ( outFilename, inFilename );
    }

    /* Make sure outFilename has .sz on it */
    SZ_MakeIntoSeraZippedFilename ( outFilename, &sz_outFilename );
    
    /* Zip the file */
    if ( ! ( SZ_ZipFile ( inFilename, sz_outFilename, s.verbose, s.keepFile ) ) )
    {
        printf ( "Error on zip.  Exiting.\n" );
    }

    /* Free memory used for sz_outFilename */
    free ( sz_outFilename );
    
    return ( 0 );  /* Success */
}


