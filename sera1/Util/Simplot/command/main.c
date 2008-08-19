#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>

#include "col_struct.h"
#include "file_ops.h"
#include "math_ops.h"
#include "memory.h"
/*
 *
 * main can take many command-line arguments.  The first argument is
 * the name of the input file.  The second is the name of the output file.  
 * The third is the region number of the data wanted to be graphed.
 * The fourth is a number which determines the sampling rate of the data.
 * Any numbers after that are columns in the data file to be plotted.  
 *
 * The possible data sets to be plotted include
 * 0 -- total dose
 * 1 -- b10 dose
 * 2 -- gamma dose
 * 3 -- n-14 dose
 * 4 -- fast dose
 * 5 -- ultrafast dose
 * 6 -- Gp 1 fluence
 * 7 -- Gp 2 fluence
 * 8 -- Thermal fluence
 * 9 -- Gamma production
 * a -- Ultrafast gamma production
 * b -- Reaction rate 1
 * c -- Reaction rate 2
 *
 *
 * For example, the program might be invoked with 
 *
 * $>go infile.dat outfile.xmgr 15 2 3 4
 *
 * The "15" means that every 15th line in "infile.dat" will be used for
 * xmgr purposes.  
 *
 * The "2 3 4" correspond to the gamma dose, n14 dose, and fast dose
 *
 * 
 *
 * In case there is no third argument, the default is 1.
 *
 * If no components are specified for the plots, the default is to plot
 * the total, gamma, n14, and fast dose components.
 *
 *
 */

char usage_message[400];                /*  Prints if mistake in invocation */
char *CommandsPath;                    /*  For command file.               */
char *SimplotPath;                     /*  For xmgr executable             */

 

int main(int argc, char *argv[])
{ 
            int sample_rate = 1,        /*   sample rate for lines    */
                                        /*   of data in the input     */
                                        /*   file.                    */

            not_end_of_data_set = 1,    /* boolean flag to denote
                                         * that the end of the current
                                         * data set has been reached  */
	    data_set_num = 0;           /* Number of the data set we
					 * are currently working on.  */


	FILE *in_file,			/*   file to read from        */
 	     *out_file;			/*   *.xmgr file to write to  */

	char in_file_name[80],		/* Name of generic commands
					   file from which all output
					   file copy from.            */
	     out_file_prefix_name[80],  /* Prefix for all output file
					   names to be copied from.
					   If prefix = "thing", then
					   the output files will be
					   "thing1", "thing2", etc.   */
	     out_file_name[80],         /* Name to be generated from
					   the output file prefix.    */
             error_message[80];         /* Printed in case of file io
					   error.                     */

        float norm_factor,             /* Normalization factor for
                                         * the doses and fluxes       */
             boron_conc;                /* Boron concentration to be
                                         * used for boron dose
                                         * normalization              */


	col_struct data;                /*   contains column data    */
					/*   to be read from input   */
					/*   file.                   */

        char title[80];
        char *ptr;

        int i;
        int length_of_filename, length_of_suffix;


	
	set_up_stuff();


	/* Process the command line arguments. */
	/* Get the input file name. */
	if (argc < 2)
		get_in_file_name (in_file_name);
	else
		strcpy (in_file_name, argv[1]);

/*
 *  Check to see if filename ends in .lin
 */
        length_of_filename = strlen ( in_file_name );
        length_of_suffix = strlen ( "lin" );
        if ( length_of_filename >= length_of_suffix ) {
           ptr = &in_file_name [ length_of_filename ];
           for ( i = 0; i < length_of_suffix; i++ ) {
              ptr--;
           }
           if ( strcmp (ptr, "lin") ) {
              sprintf (error_message, "Invalid input filename.  Valid filenames end with .lin\n" );
	      puts (error_message);
	      puts (usage_message);
	      exit (EXIT_FAILURE);
	   }
	}

	if ( (in_file = fopen ( in_file_name, "r" ) ) == NULL ) {
		sprintf (error_message, "Can't open %s.  Bummer!\n", in_file_name);
		puts (error_message);
		puts (usage_message);
		exit (EXIT_FAILURE);
	}


	/* get the output file prefix name */
	if (argc < 3)
		get_out_file_name (out_file_prefix_name);
	else
		strcpy (out_file_prefix_name, argv[2]);



	/* Get the sample rate */
	if (argc < 4) {
		sample_rate = 1;
                get_sample_rate (&sample_rate);
           }
	else
		sample_rate = atoi(argv[3]);



        /* Get the normalization factor for the dose/flux results */
        if (argc < 5) {
                norm_factor = 1.0;
                get_norm_factor (&norm_factor);
           }
        else
                norm_factor = atof(argv[4]);



        /* Get the boron concentration level in ppm */
        if (argc < 6) {
                boron_conc = 1.0;
                get_boron_conc (&boron_conc);
           }
        else
                boron_conc = atof(argv[5]);




   /* loop until the end of the data set is reached */
   while ( not_end_of_data_set ) {

        data_set_num++;

	/* Allocate space for data. */
	initialize (&data);


	/* put the file pointer at the start of the data set */
	position_fp_to_start_of_numbers ( in_file, &not_end_of_data_set, title );

	
	if ( not_end_of_data_set ) {

	  /* construct the name of the output file */
	  sprintf (out_file_name, "%s%i", out_file_prefix_name, data_set_num);
          
	  if ( (out_file = fopen ( out_file_name, "w" ) ) == NULL ) {
		sprintf (error_message, "Can't open %s.  Bummer!\n", out_file_prefix_name);
		puts (error_message);
		puts (usage_message);
		exit (EXIT_FAILURE);
	  }

	  /* write generic commands to output file */
	  write_standard_commands (out_file, title);


	  /* "check" all variables that we wish to plot. */
	  process_variable_enablers (argc, argv, &data, data_set_num );


	  /* read in the data columns - use separate routine for ultra-fast format */

          read_in_cols ( in_file, &data, sample_rate );


	  /* calculates the x coordinate from the column data */
	  compute_x_coord (&data);

	  
	  check_if_logarithmic (&data);
	  
          write_axis_label ( &data, out_file, norm_factor );

          /* normalizes the dose and fluence results */
          renorm_dose_comps (&data, norm_factor, boron_conc);

	  /* calculates the range of the x and y axes from the column data */
	  axes_extent_compute (out_file, &data);
	  
	  write_cols_to_file ( out_file, &data );		

	  /* marks off those columns which we don't want to plot */
	  deactivate_sets (out_file, &data);
	  
	  fclose (out_file);

	}  /* if (not_end_of_data_set */

   } /* while */

   /* Now, go through and run the xmgr cases for each data file created */

   for ( i=1; i < data_set_num; i++ ) {

       /* construct the name of the output file */
       sprintf (out_file_name, "%s%i", out_file_prefix_name, i);

       /* Fork off xmgr as a child process with outfile as it's command 
          line argument. */
       if ( fork () != 0 ) 
          ;
       else {
          /* printf("SimplotPath = %s\n", SimplotPath); */
          execl ( SimplotPath, "xmgr", out_file_name, NULL);
          fprintf (stderr, "exec failed, probably couldn't find xmgr\n");
          exit (EXIT_FAILURE);
       }

   }


   fclose (in_file);
   return( 0 );

}
