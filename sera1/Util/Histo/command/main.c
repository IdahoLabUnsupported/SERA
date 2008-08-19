#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "dose_vol_struct.h"
#include "file_ops.h"

/*
 *
 * main can take up to three command-line arguments.  The first argument is
 * the name of the input file.  The second is the name of the output file.  
 * Any numbers after that are columns in the data file to be plotted.  
 *
 * The possible data sets to be plotted include
 * 0 -- total dose
 * 1 -- b10 dose
 * 2 -- gamma dose
 * 3 -- n-14 dose
 * 4 -- fast dose
 * 5 -- fast flux
 * 6 -- thermal flux
 * 7 -- ultrafast dose
 *
 *
 * For example, the program might be invoked with 
 *
 * $>go infile.dat outfile.xmgr 2 3 4 y
 *
 * The "2 3 4" correspond to the gamma dose, n14 dose, and fast dose
 *
 * The "y" means to normalize the histograms by the total dose.  A "n" means
 * not to normalize by the total dose.
 *
 * In case no column numbers are given, only the total dose will be plotted.
 *
 *
 *
 */

char usage_message[400];                /*  Prints if mistake in invocation */
char * CommandsPath;
char * HistoPath;


 

main(int argc, char *argv[])
{
        int  not_end_of_data_set = 1,   /* boolean flag to denote
                                         * that the end of the current
					 * data set has been reached  */
             num_enablers,              /* number of edits on graph   */
             not_ultra = 1,             /* boolean flag to denote which
                                           code version of rtt was used
                                           0=107, 1=106               */
             norm_by_total=0,           /* flag to determine whether to
                                           normalize doses by total   */
	     data_set_num = 0;          /* Number of the data set we
					 * are currently working on.  */

	FILE *in_file,			/*   file to read from        */
 	     *out_file;			/*   *.xmgr file to write to  */

	char in_file_name[80],          /* File to take input commands
					 * from for xmgr's benefit.   */
             out_file_prefix_name[80],  /* Prefix for all output 
					 * files.  If prefix = "out",
					 * then the output files 
					 * will be out1, out2, etc.   */
	     out_file_name[80],             /* Name of a particular 
					 * output file, e.g. "out1".  */
             error_message[80];         


	dose_vol_struct data;           /*   contains column data    */
					/*   to be read from input   */
					/*   file.                   */

        int i;



  set_up_stuff(argc, argv);



  /* Process the command line arguments. */
  /* Get the input file name. */
  if (argc < 2)
    get_in_file_name (in_file_name);
  else
    strcpy (in_file_name, argv[1]);
	
  if ( (in_file = fopen ( in_file_name, "r" ) ) == NULL ) {
    sprintf (error_message, "Can't open %s.  Bummer!\n", in_file_name);
    puts (error_message);
    puts (usage_message);
    exit (EXIT_FAILURE);
  }
	

  /* get the output file prefix */
   if (argc < 3)
     get_out_file_name (out_file_prefix_name);
   else
     strcpy (out_file_prefix_name, argv[2]);
	



   /* loop until the end of the data set is reached */
   while ( not_end_of_data_set ) {

	data_set_num++;
	  
	/* Allocate space for data. */
	initialize (&data);

	position_fp_to_start_of_numbers ( in_file, &not_end_of_data_set, &not_ultra );

	if ( not_end_of_data_set ) {

	  /* construct the name of the output file */
	  sprintf (out_file_name, "%s%i\0", out_file_prefix_name, data_set_num);

	  if ( (out_file = fopen ( out_file_name, "w" ) ) == NULL ) {
		  sprintf (error_message, "Can't open %s.  Bummer!\n", out_file_prefix_name);
		  puts (error_message);
		  puts (usage_message);
		  exit (EXIT_FAILURE);
	  }

	  num_enablers = process_variable_enablers (argc, argv, &data, data_set_num);

          norm_by_total = read_norm_by_total ( argc, argv, num_enablers );

	  write_standard_commands (out_file);

	  write_titles_to (out_file, data_set_num);

	  read_in_cols ( in_file, &data, not_ultra );

          read_in_reference ( in_file, &data, data_set_num, not_ultra );

	  write_cols_to_file ( out_file, &data, data_set_num, norm_by_total );		

	  deactivate_sets (out_file, &data);
	  
	  fclose (out_file);
	  
	} /*  if (not_end_of_data_set)  */

   } /*  while  */

   /* Now, loop through all xmgr files created and run the xmgr processes */

   for ( i=1; i < data_set_num; i++) {

      /* construct the name of the output file */
      sprintf (out_file_name, "%s%i\0", out_file_prefix_name, i);

      /* Fork off xmgr as a child process with outfile as it's command line 
       * argument. */

      if ( fork () != 0 ) 
         ;
      else {
         execl (HistoPath, "xmgr", out_file_name, NULL);
         fprintf (stderr,"exec failed, probably couldn't find %s \n", HistoPath);
         fprintf(stderr,"Make sure you have set the SERA_BIN environment variable \n");
         exit (EXIT_FAILURE);
      }

   }


	fclose (in_file);

 
}
