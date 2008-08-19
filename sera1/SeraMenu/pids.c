 
#include "seramenu.h"

/* Prototypes for functions local to this file */

static int find_zombies( char * initial_start );
static void get_current_pids( char *process_id_string );


void add_pid (main_gui_t * gui, int pid_to_add, char *program_name)
{
  DEBUG_TRACE_IN  printf("Entering add_pid\n");

  gui->pids.elements[gui->pids.pids_recorded].pid = pid_to_add;
  strcpy (gui->pids.elements[gui->pids.pids_recorded].pid_name, program_name);
  gui->pids.pids_recorded ++;

  DEBUG_TRACE_OUT printf("Leaving add_pid\n");
}



void kill_all_pids ( main_gui_t * gui )
{
   int count;
   char kill_string[25];
   char pid[10];

   DEBUG_TRACE_IN  printf("Entering kill_all_pids\n");
   
   for (count = 0; count < gui->pids.pids_recorded; count ++)
   {
      strcpy (kill_string, "kill -KILL ");
      sprintf (pid, "%d", gui->pids.elements[count].pid);
      strcat (kill_string, pid);
      system (kill_string);
   }
   gui->pids.pids_recorded = 0;
   DEBUG_TRACE_OUT  printf("Leaving kill_all_pids\n");
}


void kill_pid ( main_gui_t * gui, XmString pid_to_kill)
{
   char kill_string[25];
   char pid[10];
   int pid_found = 0;
   int count;
   char possible_pid[MAX_LENGTH_NAME + 10];
   XmString possible_pid_xmstring;
   char pid_number[10];

   DEBUG_TRACE_IN  printf("Entering kill_pid\n");
   /*
    * Find the PID in the array, delete it and shift all
    * other pids
    */
   for (count = 0; count < gui->pids.pids_recorded; count++)
   {
      strcpy (possible_pid, gui->pids.elements[count].pid_name);
      strcat (possible_pid, " - ");
      sprintf (pid_number, "%d", gui->pids.elements[count].pid);
      strcat (possible_pid, pid_number);
      possible_pid_xmstring = XmStringCreateLtoR (possible_pid, 
                                                  XmFONTLIST_DEFAULT_TAG);

      if (XmStringCompare (possible_pid_xmstring, pid_to_kill))
      {
         strcpy (kill_string, "kill -KILL ");
         sprintf (pid, "%d", gui->pids.elements[count].pid);
         strcat (kill_string, pid);
         system (kill_string);
         pid_found = 1;
         gui->pids.pids_recorded --;
      }
      if (pid_found)
         gui->pids.elements[count] = gui->pids.elements[count + 1];
   }
   DEBUG_TRACE_OUT  printf("Leaving kill_pid\n");
}


void get_pids_and_names ( main_gui_t * gui, XmString *list)
{
   int count;
   char pid_string[MAX_LENGTH_NAME + 10];
   char pid_number[10];

   DEBUG_TRACE_IN  printf("Entering get_pids_and_names\n");
   
   for (count = 0; count < gui->pids.pids_recorded; count ++)
   {
      strcpy (pid_string, gui->pids.elements[count].pid_name);
      strcat (pid_string, " - ");
      sprintf (pid_number, "%d", gui->pids.elements[count].pid);
      strcat (pid_string, pid_number);
      list[count] = XmStringCreateLtoR (pid_string, XmFONTLIST_DEFAULT_TAG);
   }
   DEBUG_TRACE_OUT  printf("Leaving get_pids_and_names\n");
}


void get_current_pids ( char *process_id_string )
{
    FILE *pipe_ptr;
    int length;
    char buf[256];

    DEBUG_TRACE_IN  printf("Entering get_current_pids\n");  

    process_id_string[0] = '\0';
    /*
     * Open a pipe to the ps command
     * and read the output of the command into
     * the process_id_string.
     */
    
    if( (pipe_ptr = (FILE *) popen("ps", "r")) != NULL )
    {
        while( (fgets(buf, 256, pipe_ptr)) != NULL )
            strcat( process_id_string, buf );
    }
    else
        printf("Pipe to the ps command failed\n");

    pclose( pipe_ptr );  /* close the pipe */
    
    DEBUG_TRACE_OUT  printf("Leaving get_current_pids\n");
}

/*
 * This is a routine to find pids that have turned into zombie processes
 * from the ps command.  It takes the pointer of the pid within the pid
 * string and then reads that line.  If it finds <zombie> or <defunct> on that line
 * then it returns a 1 for a successful find, otherwise 0.
 */
int find_zombies( char * initial_start )
{
    char temp_buffer[256]; /* temporary buffer to read in the line */
    int i = 0;
  
    DEBUG_TRACE_IN  printf("Entering find_zombies\n");
    /*
     * Read in the line beginning at initial_start.
     */
    do
    {
        temp_buffer[i] = *(initial_start + i);
        i++;

    } while( *(initial_start + i ) != '\n' );

    temp_buffer[i] = '\0';

    /* Look for a zombie in this line */
    if( strstr( temp_buffer, "<zombie>" ) || strstr( temp_buffer, "<defunct>" ) )
    { 
        DEBUG_TRACE_OUT printf("Leaving find_zombies, zombie found\n");
        return( 1 ); /* and return 1 if found */
    }

    DEBUG_TRACE_OUT  printf("Leaving find_zombies, zombie not found\n");
    return( 0 );
}

void update_process_ids_cb(Widget w, XtPointer clientData, XtPointer callData)
{
   main_gui_t * gui = (main_gui_t *) clientData;
   int count = 0;
   int front_index = 0;
   int back_index = gui->pids.pids_recorded - 1;
   int i;
   int number_removed = 0;         /* number of pids removed from the scrolled list widget */
   char pid_string[8000];          /* string of currently running pids and names */
   char pid[10];                   /* the current pid we are looking for */

   DEBUG_TRACE_IN  printf("Entering update_process_ids_cb\n");

   get_current_pids( pid_string ); /* get a list of pids that are actually running */
   
   while( front_index <= back_index )
   {
     sprintf( pid, "%d", gui->pids.elements[front_index].pid ); 

     /*
      * strstr will return NULL if the pid we are currently looking for is not actually still
      * running; if this is the case we can remove it from the list of pids displayed by the scrolled
      * list widget. If find_zombies returns a 1, then we know that the current process
      * has turned into a zombie process and we can remove it from the list also.
      *
      * NOTE: By remove we are using lazy deletion. The number of processes running
      *       in the pids array is just decremented so if cannot be displayed
      *       in the scrolled list widget.
      *
      */

     if( strstr( pid_string, pid ) == NULL || find_zombies( strstr( pid_string, pid)) )
     {
         number_removed ++; 
       
 	 if( front_index == back_index ) 
	 
 	   back_index --; 
	   
         else 
 	 { 
	   for( i=front_index; i < back_index; i++ ) /*shift the pids down*/ 
              gui->pids.elements[i] = gui->pids.elements[i+1]; 

 	   back_index --; 
 	 } 
      } 
      else 
        front_index ++; 
    } 
    gui->pids.pids_recorded -= number_removed;
  
    DEBUG_TRACE_OUT  printf("Leaving update_process_ids_cb\n");
}
