/*
 * Added this as the activate callback for the start histo button in do_histo
 */

#include "histo.h"

#define MAX_ARG_LIST 20    /* The maximum number of arguments in the histo call */

void StartHistoCB (Widget w, XtPointer clientData, XtPointer callData)
{
   histo_struct * histo_struct_var = (histo_struct *) clientData;

   int     i;
   int     argv_no;
   char  * input;
   char  * output;
   int     toggle_set = 0;
   char  * argv[MAX_ARG_LIST];
   FILE  * tmp_fptr;
   int     error;
   int     status;
   int     pid;

   /*
    * Assign the values of the text field widgets' strings,
    * to the appropriate members of the histo_struct_var.
    */

   input = XmTextFieldGetString(histo_struct_var->HistoInputFile);
   histo_struct_var->inputfile = input;
   
   output = XmTextFieldGetString(histo_struct_var->HistoOutputFile);
   histo_struct_var->outputfile = output;

   /* Check to see if user entered an input file and output file */
   if (strlen(histo_struct_var->inputfile) == 0)
      {
         DT_error( w, "No input file was specified", "Histo Error", NULL ); 
	 SetCursorPos( histo_struct_var->HistoInputFile ); 
         return; 
      }
   if (strlen(histo_struct_var->outputfile) == 0)
      {
         DT_error( w, "No output file was specified", "Histo Error", NULL ); 
	 SetCursorPos( histo_struct_var->HistoOutputFile );
         return;  
      }

   /*
    * Check to see if the user set at least one toggle button
    */

   i = 0;
   while( toggle_set == 0 && i <= NUM_HISTO_TOGGLES )
      if( histo_struct_var->options[ i++ ] == 1 )
          toggle_set = 1;
   
   if( toggle_set == 0 )
     {
       DT_error( w, "No toggle buttons were set", "Histo Error", NULL );
       return;
     }

   /* Check to see if input file entered is a valid input file */
   if( (tmp_fptr = fopen(histo_struct_var->inputfile, "r")) == 0)
      {
         DT_error( w, "The input file entered does not exist, \nor is not in the local directory", "Histo Error", NULL);
         return; 
      }
   else
      fclose(tmp_fptr);

   /* Initializing all of the arguments to NULL */
   for ( i = 0; i < MAX_ARG_LIST; argv[i++] = NULL );

   /* Starting a new child process to run the Histo program */
   switch (pid = fork ())
      {

      case 0:
         /* Setting up the argument list for the call to histo */
         argv[0] = (char *) GetMemory(256,"StartHistoCB");
         histo_struct_var->sera_home = (char *) getenv( "SERA_HOME" );
         strcpy (argv[0], histo_struct_var->sera_home);
         strcat (argv[0], "/Target/bin/histo");

         argv[1] = (char *) GetMemory(256,"StartHistoCB");
         strcpy (argv[1], histo_struct_var->inputfile);

	 argv[2] = (char *) GetMemory(256,"StartHistoCB");
	 strcpy (argv[2], histo_struct_var->outputfile);

	 argv_no = 3;
	 for (i=0; i < NUM_HISTO_TOGGLES; i++)
	    {
	    if (histo_struct_var->options[i] == 1)
	       {
	       argv[argv_no] = (char *) GetMemory(2,"StartHistoCB");
	       argv[argv_no][0] = (char)(i+48);
	       argv[argv_no][1] = '\0';
	       argv_no ++;
	       }
	    }
	 argv[argv_no] = histo_struct_var->normalizedose;

	 error = execvp(argv[0], argv);
	 printf("Error in exec: %d %d\n", error, errno);
	 break;


      case -1:
         /* Case where the fork command did not work */
	 printf("The fork has failed \n");

      }

   /*
    * Destroy the Histo widget and free the memory used
    * for the temporary strings
    */
   DestroyShell( GetTopShell( w ) ); 
   XtFree ( input );
   XtFree ( output );

   for( i = 0; i < MAX_ARG_LIST; i++ )
     XtFree( argv[i] );

   /* Wait for the child process to finish before continuing */
   while ( wait(&status) != pid);
   exit( 0 );

}
