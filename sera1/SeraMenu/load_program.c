
#include "seramenu.h"

/*
 * Fork a child process, and then 
 * execute the executable_command
 */

int load_program (Widget top_parent, char ** argv)

{
   int pid;
   char error_string[256];

   DEBUG_TRACE_IN  printf("Entering load_program\n");

    pid = fork(); 
    switch (pid) 
    { 
       case 0:  execvp( argv[0], argv );
	 /*
	  * This should rarely happen because we have already checked to make
	  * sure the program is in the correct directory, but if it does the 
	  * user should know about it.
	  */
       case -1: sprintf( error_string, "There was an error loading\n%s", argv[0]);
	        DT_error( top_parent, error_string, "SERA Execution Error", NULL);

       default: break; 
    } 

    DEBUG_TRACE_OUT  printf("Leaving load_program\n");
    return( pid ); 

} 
