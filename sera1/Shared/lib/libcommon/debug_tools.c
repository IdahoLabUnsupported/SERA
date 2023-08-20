/******************************************************************
 * debug_tools.c
 *
 * AUTHOR:  Matt Cohen
 *
 * DATE:    July 1, 1998
 *
 * USAGE:   Complete directions are in debug_tools.h
 ******************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "keyval_tools.h"
#include "debug_tools.h"


DebugOptionsStruct debugData;
/*
 * sets the values in the structure debugData by calling
 * XtGetApplicationResources.  If -debug_all is set, then
 * all flags are set.
 */
void set_debug_values ( char * program, Widget topLevel)
{
    XtGetApplicationResources (topLevel, &debugData, resources, 
                               XtNumber (resources), NULL, 0 );

    if (debugData.version)
        output_version ( program );

    if (debugData.debugAll)
    {
        debugData.debugGui        = TRUE;
        debugData.debugLoading    = TRUE;
        debugData.debugData       = TRUE;
        debugData.debugIO         = TRUE;
        debugData.debugLibqsh     = TRUE;
        debugData.debugLibqshKeys = TRUE;
	debugData.debugMemory     = TRUE;
    }
 
    if (debugData.debugTrace)  printf("debug_trace is on.\n"); 
    DEBUG_GUI         printf("debug_gui is on.\n");
    DEBUG_LOADING     printf("debug_loading is on.\n");
    DEBUG_DATA        printf("debug_data is on.\n");
    DEBUG_IO          printf("debug_io is on.\n");
    DEBUG_LIBQSH      printf("debug_libqsh is on.\n");
    DEBUG_LIBQSH_KEYS printf("debug_libqshkeys is on.\n");
    if (debugData.debugMemory) printf("debug_memory is on.\n");
}


/*
 * Output the current version if available and quit
 */
void output_version ( char * program )
{
    char version[32];
    char *localProgram;

    /* Get just the executable name... lose the path */
    localProgram = strrchr ( program, '/' );
    if ( ! localProgram )
        localProgram = program;
    else
        localProgram++;
    
    if( get_version_string( localProgram, version ) )
        printf("Current Version: %s\n", version);
    else
        printf("The current version could not be found\n");

    exit( 0 );
}


/*
 * If an incorrect flag is used, it is identified and the proper
 * usage is printed.
 */
void debug_syntax (int argc, char *argv[])
{
    int i;

    printf("\n=============================================================\n");
    printf("                    Unknown flag(s):\n");

    for (i = 1; i < argc; i++)
        printf("                        %s\n", argv[i]);

    printf("\nThe following are the possible debugging options:\n\n");
    printf(" -debug_trace       Trace all function calls\n");
    printf(" -debug_all         Output all debug statements\n");
    printf(" -debug_gui         Output gui debug statements\n");
    printf(" -debug_loading     Output loading debug statements\n");
    printf(" -debug_data        Output data processing debug statements\n");
    printf(" -debug_io          Output input/output debug statements\n");
    printf(" -debug_libqsh      Output libqsh debug statements\n");
    printf(" -debug_libqshkeys  Output libqsh keys debug statements\n");
    printf(" -debug_memory      Trace allocation and deallocation of memory\n");
    printf(" -version           Output the current version and quit\n");
    printf("===============================================================\n\n");
}

/* Outputs tabs and increments counter */
void debug_push ( void )
{
    int i;

    for (i = 0; i < number_of_tabs; i++)
        printf("   ");

    number_of_tabs ++;
}

/* Decrements counter and outputs tabs */
void debug_pop ( void )
{
    int i;

    number_of_tabs --;

    for (i = 0; i < number_of_tabs; i++)
        printf("   ");
}

/* Outputs spaces and increments counter */
void debug_memory_push ( void )
{
    int i;

    /*for (i = 0; i < number_of_memory_tabs; i++)
      printf(" ");*/

    printf("%-6d",number_of_memory_tabs);
    number_of_memory_tabs ++;
}

/* Outputs spaces and leaves counter alone */
void debug_memory_noop (void )
{ 
  int i;
  /*for(i = 0; i < number_of_memory_tabs - 1; i++)
    printf(" ");*/
  printf("%-6d",number_of_memory_tabs - 1);

}

/* Decrements counter and outputs tabs */
void debug_memory_pop ( void )
{
    int i;

    number_of_memory_tabs --;

    /*for (i = 0; i < number_of_memory_tabs; i++)
      printf(" ");*/
    printf("%-6d",number_of_memory_tabs);
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    get_version_string()
%%%
%%%  Purpose:     Given a module's executable name, return the current
%%%               version number for that module. The versions are kept
%%%               in $SERA_RESOURCES/Shared/.Versions.txt.
%%%
%%%  Parameters:  module  -> The executable name of a program.
%%%               version -> If found, the module's version is returned
%%%                          in this variable.
%%%
%%%  Returns:     1 if a version was found successfully, 0 otherwise
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_version_string( char * module, char * version )
{
    FILE * versFile;
    char value[32];
    char localCopy[32];
    char filename[256];
    char * resPath;
    int found = 0;

    /* Get the path to the Resources directory */
    resPath = getenv( "SERA_RESOURCES" );
    if( resPath )
    {
        strcpy( filename, resPath );
        strcat( filename, "/Shared/.Versions.txt" );

        /* Now try to open the file */
        versFile = fopen( filename, "r" );
        if( versFile )
        {
            strncpy( localCopy, module, 31 );
            KV_make_lower_string( localCopy );
            KV_set_split_characters( ":=" );

            if( KV_read_string_value_for_key( versFile, localCopy, value, 32 ) )
            {
                strcpy( version, value );
                found = 1;
            }
            
            fclose( versFile );
        }
    }

    return( found );
}
