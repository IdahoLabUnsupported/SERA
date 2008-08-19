/************************************************************************************
 * Added this as the activate callback for the start Simplot button in do_simplot 
 */
#include "simplot.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

static void free_memory_from_struct( simplot_struct * struct_var );

#define MAX_ARG_LIST 30      /* The maximum number of arguments in the simplot call */

void StartSimplotCB (Widget w, XtPointer clientData, XtPointer callData)
{
    simplot_struct * simplot_struct_var = (simplot_struct *) clientData;

    int     i;
    int     argv_no;
    char  * input;
    char  * output;
    char  * sample_rate;
    char  * norm_factor;
    char  * boron_conc;
    int     toggle_set = 0;
    char  * argv[MAX_ARG_LIST];
    FILE  * tmp_fptr;
    int     error;
    int     status;
    int     pid;
 
    /*
     * Assign the values of the text field widgets strings,
     * to the appropriate members of the simplot_struct_var.
     */

    input = XmTextFieldGetString(simplot_struct_var->SimplotInputFile);
    simplot_struct_var->inputfile = (char *)MT_malloc( strlen(input)+1 );
    strcpy( simplot_struct_var->inputfile, input );

    output = XmTextFieldGetString(simplot_struct_var->SimplotOutputFile);
    simplot_struct_var->outputfile = (char *)MT_malloc( strlen(output)+1 );
    strcpy( simplot_struct_var->outputfile, output );
   
    sample_rate = XmTextFieldGetString(simplot_struct_var->SimplotSampleRate);
    simplot_struct_var->sample_rate = (char *)MT_malloc( strlen(sample_rate)+1 );
    strcpy( simplot_struct_var->sample_rate, sample_rate );
   
    norm_factor = XmTextFieldGetString(simplot_struct_var->SimplotNormFactor);
    simplot_struct_var->norm_factor = (char *)MT_malloc( strlen(norm_factor)+1 );
    strcpy( simplot_struct_var->norm_factor, norm_factor );

    boron_conc = XmTextFieldGetString(simplot_struct_var->SimplotBoronConc);
    simplot_struct_var->boron_conc = (char *)MT_malloc( strlen(boron_conc)+1 );
    strcpy( simplot_struct_var->boron_conc, boron_conc );

    /*
     * Free the memory used
     * for the temporary strings
     */

    XtFree( (char *) input );        
    XtFree( (char *) output ); 
    XtFree( (char *) sample_rate );
    XtFree( (char *) norm_factor );        
    XtFree( (char *) boron_conc );       


    /* Check to see if user entered an input file and output file */
    if (strlen(simplot_struct_var->inputfile) == 0)
    {
        DT_error( w, "No input file was specified", "Simplot Error", NULL ); 
        SetCursorPos( simplot_struct_var->SimplotInputFile );
        free_memory_from_struct( simplot_struct_var );
        return;
    }
    if ( !FT_filenameEndsIn(simplot_struct_var->inputfile, "lin") ) {
        DT_error( w, "An invalid input filename was specified.\nValid filenames end with .lin.", "Simplot Error", NULL ); 
        SetCursorPos( simplot_struct_var->SimplotInputFile );
        free_memory_from_struct( simplot_struct_var );
        return;
    }
    if (strlen(simplot_struct_var->outputfile) == 0)
    {
        DT_error( w, "No output file was specified", "Simplot Error", NULL ); 
        SetCursorPos( simplot_struct_var->SimplotOutputFile );
        free_memory_from_struct( simplot_struct_var );
        return;
    }
    if (strlen(simplot_struct_var->sample_rate) == 0)
    {
        DT_error( w, "No stride distance was specified", "Simplot Error", NULL);
        SetCursorPos( simplot_struct_var->SimplotSampleRate );
        free_memory_from_struct( simplot_struct_var );
        return;
    }
    if (strlen(simplot_struct_var->norm_factor) == 0)
    {
        DT_error( w, "No normalization factor was specified", "Simplot Error", NULL);
        SetCursorPos( simplot_struct_var->SimplotNormFactor );
        free_memory_from_struct( simplot_struct_var );
        return;
    }
    if (strlen(simplot_struct_var->boron_conc) == 0)
    {
        DT_error( w, "No boron concentration was specified", "Simplot Error", NULL);
        SetCursorPos( simplot_struct_var->SimplotBoronConc );
        free_memory_from_struct( simplot_struct_var );
        return;
    }

    /*
     * Check to see if the user set at least one toggle button
     */

    i = 0;
    while( toggle_set == 0 && i < NUM_SIMPLOT_TOGGLES )
    {
        if( simplot_struct_var->options[i] == 1 )
            toggle_set = 1;
        i++;
    }

    if ( toggle_set == 0 )
    {
        DT_error( w, "No toggle buttons were set", "Simplot Error", NULL );
        free_memory_from_struct( simplot_struct_var );
        return;
    }

    /* Check to see if input file entered is a valid input file */
    if( !FT_fileExists( simplot_struct_var->inputfile ) )
    {
        DT_error( w, "The input file entered does not exist, \nor is not in the local directory", "Simplot Error", NULL );
        free_memory_from_struct( simplot_struct_var );
        return;
    }

    for ( i = 0; i < MAX_ARG_LIST; argv[i++] = NULL );

    /* Setting up the argument list for the call to simplot */
    argv[0] = (char *) MT_malloc( 256 );
    simplot_struct_var->bnct_home = (char *) getenv( "SERA_HOME" );
    strcpy (argv[0], (simplot_struct_var->bnct_home));
    strcat (argv[0], "/Target/bin/simplot");
  
    argv[1] = (char *) MT_malloc( 256 );
    strcpy (argv[1], simplot_struct_var->inputfile);
   
    argv[2] = (char *) MT_malloc( 256 );
    strcpy (argv[2], simplot_struct_var->outputfile);
   
    argv[3] = (char *) MT_malloc( 25 );
    strcpy (argv[3], simplot_struct_var->sample_rate);

    argv[4] = (char *) MT_malloc( 25 );
    strcpy (argv[4], simplot_struct_var->norm_factor);
   
    argv[5] = (char *) MT_malloc( 25 );
    strcpy (argv[5], simplot_struct_var->boron_conc);
   
    argv_no = 6;
    for (i = 0; i < NUM_SIMPLOT_TOGGLES; i++)
    {
        if (simplot_struct_var->options[i] == 1)
        {
            argv[argv_no] = (char *) MT_malloc( 2 );
            if(i<10)
                argv[argv_no][0] = (char)(i+'0');
            else 
                argv[argv_no][0] = (char)(i+'a'-10);
            argv[argv_no][1] = '\0';
            argv_no ++;
        }
    }

    /*
     * Everything has been copied to argv, so we can free the memory
     * in the simplot_struct
     */
    free_memory_from_struct( simplot_struct_var );


    /* Starting a new child process to run the Simplot program */
    switch (pid = fork ()) 
    {

        case 0:  
            error = execvp(argv[0], argv);
            printf("Error in exec: %d %d\n", error, errno);
            break;

        case -1:
            /* Case where the fork command did not work */
            printf("The fork has failed \n");

    }

    /*
     * Free memory.
     */

    for( i = 0; i < MAX_ARG_LIST; i++ )
        MT_free( (void *) argv[i] );
    

    /* Wait for the child process to finish before continuing */
    while ( wait(&status) != pid);
   
}

/*
 * Free the memory from the simplot_struct.
 */
void free_memory_from_struct( simplot_struct * struct_var )
{
    DEBUG_TRACE_IN printf("Entering free_memory_from_struct\n");

    MT_free( (void *) struct_var->inputfile );
    MT_free( (void *) struct_var->outputfile );
    MT_free( (void *) struct_var->sample_rate );
    MT_free( (void *) struct_var->norm_factor );
    MT_free( (void *) struct_var->boron_conc );

    DEBUG_TRACE_OUT printf("Leaving free_memory_from_struct\n");

}
