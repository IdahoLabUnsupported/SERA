#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <math.h>

#include "col_struct.h"
#include "file_ops.h"
#include "memory.h"
#include "math_ops.h"



#define  EPSILON_PLUS   1E-17

extern char usage_message[400];

extern char *CommandsPath;
extern char *SimplotPath;

int x_log=0, y_log=0;         /* global flags for logarithmic x and y scales */

/*
 * set_up_stuff
 *
 * This procedure does some initialization of stuff.  
 * Helps to keep the main procedure a little cleaner looking.
 */
void set_up_stuff ()
{
 char *RttPath;
 char *BinPath;

  /* set up the usage message */
  strcpy (usage_message, "\nUsage:\n$ simplot file1 file2 sr c1 c2 c3....\n\nwhere \tfile1 is the input file, \n\tfile2 is the name of the file to write the necessary xmgr commands to, \n\tsr is the sample rate, \n\tand c1, c2, c3... denote the columns you want plotted.\n"); 


  /* set up path to find common commands file for all xmgr plots */
  RttPath = (char *)getenv("SERA_HOME");
  if (RttPath == NULL){
    fprintf(stderr, "Environment variable SERA_HOME not supplied - exiting !\n");
    exit(0);
  }
  CommandsPath = (char *)malloc( (strlen(RttPath) + 36) );
  strcpy(CommandsPath, RttPath);
  strcat(CommandsPath, "/Resources/SeraPlot/SimplotCommands");


  /* set up path to find path to xmgr */

  BinPath = getenv("SERA_HOME");
  if (BinPath == NULL) {
    fprintf(stderr, "Environment variable SERA_HOME not set - exiting !\n");
    exit(0);
  }
  SimplotPath = (char *)malloc( (strlen(BinPath) + 17) );
  strcpy(SimplotPath, BinPath);
  strcat(SimplotPath, "/Target/bin/xmgr");
  /*
  printf("strlen(BinPath) = %d\n",strlen(BinPath));
  printf("SimplotPath = %s\n",SimplotPath);
  printf("CommandsPath = %s\n",CommandsPath);
  */ 

}











/*
 * get_in_file_name
 *
 * This procedure reads in the name of an input file.
 * 
 */
void get_in_file_name (char *name)
{
	printf ("I need the name of the input file.\n");
	scanf ("%s", name);
}








/*
 * get_out_file_name
 *
 * This procedure reads in the name of the output file.
 */
void get_out_file_name (char *name)
{
	printf ("I need the name of the output file.\n");
	scanf ("%s", name);
}










/*
 * get_sample_rate
 *
 * This procedure reads in the sample rate for the data plots
 */
void get_sample_rate (int *sample_rate)
{
        printf ("I need the sample rate.\n");
        scanf ("%d", sample_rate);
}










/*
 * get_norm_factor
 *
 * This procedure reads in the sample rate for the data plots
 */
void get_norm_factor (float *norm_factor)
{
        printf ("I need the dose normalization factor.\n");
        scanf ("%e", norm_factor);
}










/*
 * get_boron_conc
 *
 * This procedure reads in the sample rate for the data plots
 */
void get_boron_conc (float *boron_conc)
{
        printf ("I need the boron concentration (ppm).\n");
        scanf ("%e", boron_conc);
}










/*
 * write_standard_commands
 *
 * This procedure writes all the control codes for xmgr to the output file.
 */
void write_standard_commands (FILE *outfile, char *title)
{
	FILE *header_file;
	
        char string[50] = " title \"E";
        char buf[120];


	if ((header_file = fopen(CommandsPath, "r")) == NULL) {
		fprintf (stderr, 
			 "Can't open commands file %s.  Bummer!\n", 
			 CommandsPath);
		exit (EXIT_FAILURE);
	}

	while ( ( fgets(buf, 120, header_file) ) != NULL ) {
           if ( strstr(buf, string) == 0 )
              fputs ( buf, outfile );
           else
              fprintf ( outfile, "@    title \"%s\"\n", title );
        }

	fclose ( header_file );

}










/*
 * position_fp_to_start_of_numbers
 *
 * This procedure positions the file pointer to the correct position 
 * to start reading in data.  If the end of the file is reached, then
 * the "not_end" flag is set.
 */
void position_fp_to_start_of_numbers ( FILE *infile, int *not_end, char *title )
{
	char buf[170];
	static char stop_string[50] = "Title: ";
	static char test_string[50] = " x (cm)";
	int s_len = strlen (stop_string);
        int i, j, t_len;


	/* read into the file until the stop_string is found */
	
	fgets (buf, 170, infile);

	while ( ( strncmp (buf, stop_string, s_len)  != 0 )
	       && (*not_end) )
	  if ( fgets (buf, 170, infile) == (char *)NULL) {
	    *not_end = 0;
	    return;
	  }


        /* read the title string */

        t_len = strlen ( buf );
        for ( j = 0, i = 7; i <= t_len; i++ ) {
           if ( buf[i] != '\n' )
              title[j++] = buf[i];
        }


	/* Now, read into the file until the test_string is found
         * This puts the file pointer at the first line of data */

        while ( strncmp ( buf, test_string, 7 ) != 0 )
	   fgets (buf, 170, infile);

}









/*
 * read_in_cols
 *  
 * This procedure reads in the numbers in the columns in the data file.
 * The data is read from "infile" and placed in "data".
 * 
 * The region number tells where to begin reading the data in the input file,
 * and the sample_rate tells how often to read.
 *
 */
void read_in_cols ( FILE *infile, col_struct *data, int sample_rate )
{
	char buf[170];          /* input line read from file */
        char *check;

	int line_count = 0,     /* # of lines processed from input file */
	    sample_count = 0,   /* # of lines copied into output file  */
            ok = 1;             /* Set to zero if end of current data set reached */

        static char skip_string[50] = "The point";
        int ss_len = strlen(skip_string);

	int  reg;               /* region number */
	


	/* Loop until the end of the data set is reached */
	while (ok) {

	   if ( sample_count < data->max_size ) {
	      check = fgets (buf, 170, infile);


	      if ( buf[0] == '\n' || !check )
	         ok = 0;

	      else {
		 /* if the mod comes out zero, process the line from the     */
		 /* input file, otherwise skip the line.                     */

                 if ( strncmp(buf, skip_string, ss_len) != 0) {

		    if ( ( (line_count % sample_rate) == 0) ) {
		       sscanf (buf, "%f %f %f %i %e %e %e %e %e %e %e %e %e %e %e %e %e",
		   		   &(data->x[sample_count]), 
		   		   &(data->y[sample_count]), 
				   &(data->z[sample_count]), &reg,
				   &(data->total_dose[sample_count]), 
				   &(data->b_10[sample_count]),
				   &(data->gamma[sample_count]), 
				   &(data->n_14[sample_count]),
				   &(data->fast[sample_count]), 
				   &(data->ultra_fast[sample_count]), 
				   &(data->gp1_fluence[sample_count]),
				   &(data->gp2_fluence[sample_count]), 
				   &(data->thermal_fluence[sample_count]),
				   &(data->gamma_prod[sample_count]),
				   &(data->ultra_gamma_prod[sample_count]),
				   &(data->rr1[sample_count]),
				   &(data->rr2[sample_count]));
		         sample_count++;

		    }   /* if (line_count % sample_rate) */

		    line_count++;

                 }  /* if buf */

	      }   /* if-else strncmp */


	   }   

	   else   /* if line_count < data->max_size */
	      mem_reallocate (data);


	}   /* while ok */

	data->current_size = sample_count-1;

	if (data->current_size < 0) {
	  puts ("You probably have an incorrect region number.  Bummer.  Try again.");
	  puts (usage_message);
	  exit (EXIT_FAILURE);
	}

}









/*
 * read_in_ultra_cols
 *  
 * This procedure reads in the numbers in the columns in the data file.
 * The data is read from "infile" and placed in "data".
 * 
 * The region number tells where to begin reading the data in the input file,
 * and the sample_rate tells how often to read.
 *
 */
void read_in_ultra_cols ( FILE *infile, col_struct *data, int sample_rate, int *not_ultra )
{
	char buf[138];          /* input line read from file */

	int line_count = 0,     /* # of lines processed from input file */
	    sample_count = 0,   /* # of lines copied into output file  */
            ok = 1,             /* Set to zero if end of current data set reached */
            skip = 0;           /* Set to one if end of first data block reached */

	static char stop_string[50] = " EndLineEdit";
	static char skip_string[50] = "     x";
	int s_len = strlen(stop_string);
	int ss_len = strlen(skip_string);

	int  reg;               /* region number */
	


	/* Loop until the end of the data set is reached */
	while (ok) {

	   if ( sample_count < data->max_size ) {
	      fgets (buf, 138, infile);


	      if ( (strncmp(buf, stop_string, s_len)) == 0)
	         ok = 0;

              /* check for skip string - if match, skip to header, then 2 more lines */

	      else if ( (strncmp(buf, "\n", 1)) == 0) {
	         skip = 1;
	         sample_count = 0;

                 while ( strncmp(buf, skip_string, ss_len) != 0 )
                    fgets (buf, 138, infile);

                 fgets (buf, 138, infile);
                 fgets (buf, 138, infile);

              }  /* if (strncmp) */

              /* checking to see if first data block or second        */
              /* first data block is read here - second below         */

	      else {
		 /* if the mod comes out zero, process the line from the     */
		 /* input file, otherwise skip the line.                     */

                 if ( ( (line_count % sample_rate) == 0) ) {

                    if ( !skip )
		       sscanf (buf, "%f %f %f %i %f %f %f %f %f %f",
				   &(data->x[sample_count]), 
				   &(data->y[sample_count]), 
				   &(data->z[sample_count]), &reg,
				   &(data->total_dose[sample_count]), 
				   &(data->b_10[sample_count]),
				   &(data->gamma[sample_count]), 
				   &(data->n_14[sample_count]),
				   &(data->fast[sample_count]), 
				   &(data->ultra_fast[sample_count]));

		    else
		       sscanf (buf, "%f %f %f %i %f %f %f %f %f",
				   &(data->x[sample_count]), 
				   &(data->y[sample_count]), 
				   &(data->z[sample_count]), &reg,
				   &(data->gp1_fluence[sample_count]),
				   &(data->gp2_fluence[sample_count]), 
				   &(data->thermal_fluence[sample_count]),
				   &(data->gamma_prod[sample_count]),
				   &(data->ultra_gamma_prod[sample_count]));

		    sample_count++;

		 }   /* if (line_count % sample_rate) */

		 line_count++;

	      }   /* if-else strncmp */

	   }   /* if sample_count */

	   else   /* if line_count < data->max_size */
	      mem_reallocate (data);


	}   /* while ok */

	data->current_size = sample_count-1;

	if (data->current_size < 0) {
	  puts ("You probably have an incorrect region number.  Bummer.  Try again.");
	  puts (usage_message);
	  exit (EXIT_FAILURE);
	}

}
/*
 * write_axis_label
 *
 * This procedure writes the axis label on the output file, if
 * this is a dose plot.  If not, it just returns.
 */
void write_axis_label ( col_struct *data, FILE *outfile, float norm)
{
        if ( data->total_dose_enabled | data->b_10_enabled | 
             data->gamma_enabled | data->n_14_enabled | data->fast_enabled | 
             data->ultra_fast_enabled ) 
           if ( norm == 1.0 )
              fprintf (outfile, "@      yaxis  label \"Dose, cGy/MW-min\"\n");
           else
              fprintf (outfile, "@      yaxis  label \"Dose, cGy\"\n");

}







/*
 * write_cols_to_file
 * 
 * This procedure writes the numbers from the col_struct to the *.xmgr file.
 */
void write_cols_to_file (FILE *outfile, col_struct *data )
{
	fprintf (outfile, "@WITH G0\n");
	fprintf (outfile, "@G0 ON\n");
	
	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->total_dose, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->b_10, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->gamma, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->n_14, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->fast, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->ultra_fast, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->gp1_fluence, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->gp2_fluence, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->thermal_fluence, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->gamma_prod, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->ultra_gamma_prod, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->rr1, data->current_size, y_log);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->xmgr_x_coord, data->rr2, data->current_size, y_log);
	fprintf (outfile, "&\n");


}	









/* 
 * write_col
 *
 * This procedure writes a SINGLE column out to the *.xmgr file.
 */
void write_col (FILE *outfile, float *x_col, float *col, int size, int logarithmic)
{
  int i;

  if (logarithmic) {
    for (i = 0; i <= size; ++i) {
      if ( (col[i] < EPSILON_PLUS) && (i > 0) ) {
	;
      }
      else
	fprintf (outfile, "%.2f %7.2E\n", x_col[i], col[i]);
    }
  }
  else {
    for (i = 0;  i <= size;  ++i)
      fprintf (outfile, "%.2f %7.2E\n", x_col[i], col[i]); 
  }
}









/*
 * deactivate_sets
 *
 * This procedure writes the appropriate commands to the *.xmgr file
 * depending on which data sets are activated.
 */

void deactivate_sets (FILE *outfile, col_struct *data)
{
	if (!data->total_dose_enabled)
		fprintf (outfile, "@    kill s0\n");
	
	if (!data->b_10_enabled)
		fprintf (outfile, "@    kill s1\n"); 

        if (!data->gamma_enabled)
		fprintf (outfile, "@    kill s2\n");    

	if (!data->n_14_enabled)
		fprintf (outfile, "@    kill s3\n");    

        if (!data->fast_enabled)
		fprintf (outfile, "@    kill s4\n");    

        if (!data->ultra_fast_enabled)
		fprintf (outfile, "@    kill s5\n");    

        if (!data->gp1_fluence_enabled)
		fprintf (outfile, "@    kill s6\n");    

        if (!data->gp2_fluence_enabled)
		fprintf (outfile, "@    kill s7\n");    

        if (!data->thermal_fluence_enabled)
		fprintf (outfile, "@    kill s8\n");    

	if (!data->gamma_prod_enabled)
		fprintf (outfile, "@    kill s9\n");    

	if (!data->ultra_gamma_prod_enabled)
		fprintf (outfile, "@    kill s10\n");    

	if (!data->rr1_enabled)
		fprintf (outfile, "@    kill s11\n");    

	if (!data->rr2_enabled)
		fprintf (outfile, "@    kill s12\n");    

}










/*
 * This procedure processes command line arguments which determine which
 * variables to use in the plot.  It is assumed that these come after
 * argv[3].  They are assumed to be simple characters.  For example,
 * if argv[4] = "2", then variable #2, which is gamma dose, is enabled.
 * If there are no arguments after argv[3], then ALL are plotted.
 *
 */
void process_variable_enablers (int argc, char *argv[], col_struct *data, int set_num)
{
  int i;
  char enabler;
  
  if (argc <= 6) {

  /* prompt user for list of enablers */

     printf ( "I need to know what to plot for data set %d.  List all that you need, and"
              " terminate with 's':\n", set_num);
     printf ( "0=total dose, 1=b-10 dose  2=gamma dose  3=n-14 dose  4=fast dose\n");
     printf ( "5=other dose  6=fast flux  7=epithermal flux  8=thermal flux\n" );
     printf ( "9=gamma production  a=ultrafast gamma production\n" );

  /* read enablers one at a time, and process */

     for ( i=0, scanf("%1s", &enabler); enabler != 's'; scanf("%1s", &enabler), i++ )
         switch ( enabler ) {
                case '0': 
	          data->total_dose_enabled = 1;
	          break;
                case '1':
	          data->b_10_enabled = 1;
	          break;
                case '2':
	          data->gamma_enabled = 1;
	          break;
                case '3':
	          data->n_14_enabled = 1;
	          break;
                case '4':
	          data->fast_enabled = 1;
	          break;
                case '5':
	          data->ultra_fast_enabled = 1;
	          break;
                case '6':
	          data->gp1_fluence_enabled = 1;
	          break;
                case '7':
	          data->gp2_fluence_enabled = 1;
	          break;
                case '8':
	          data->thermal_fluence_enabled = 1;
	          break;
                case '9':
	          data->gamma_prod_enabled = 1;
	          break;
                case 'a':
	          data->ultra_gamma_prod_enabled = 1;
	          break;
                case 'b':
	          data->rr1_enabled = 1;
	          break;
                case 'c':
	          data->rr2_enabled = 1;
	          break;
                default:
	          fprintf (stderr, "Cannot process %c as a variable enabler.\n", enabler);
	          exit (EXIT_FAILURE);
         }

         /* Fall out here when enabler equals 's' - check if just using defaults */

         if ( !i ) {
            data->total_dose_enabled = 1;
            data->gamma_enabled = 1;
            data->n_14_enabled = 1;
            data->fast_enabled = 1;
         }

  } /* if */

  /* Enablers from command line */

  else {
    for (i = 7; i <= argc; ++i)
      switch ( *argv[i-1] ) {
      case '0': 
	data->total_dose_enabled = 1;
	break;
      case '1':
	data->b_10_enabled = 1;
	break;
      case '2':
	data->gamma_enabled = 1;
	break;
      case '3':
	data->n_14_enabled = 1;
	break;
      case '4':
	data->fast_enabled = 1;
	break;
      case '5':
	data->ultra_fast_enabled = 1;
	break;
      case '6':
	data->gp1_fluence_enabled = 1;
	break;
      case '7':
	data->gp2_fluence_enabled = 1;
	break;
      case '8':
	data->thermal_fluence_enabled = 1;
	break;
      case '9':
	data->gamma_prod_enabled = 1;
	break;
      case 'a':
	data->ultra_gamma_prod_enabled = 1;
	break;
      case 'b':
	data->rr1_enabled = 1;
	break;
      case 'c':
	data->rr2_enabled = 1;
	break;
      default:
	fprintf (stderr, "Cannot process %s as a variable enabler.\n", argv[i-1]);
	exit (EXIT_FAILURE);
      }

  } /* else */

}










/*
 * This procedure records whether either axis is logarithmic or not.  If any of the following fields are enabled,
 *
 *     total dose, b-10 dose, gamma dose, n-14 dose, fast dose, ultra_fast dose
 *
 * then the y axis format will be decimal.  Otherwise, it is power.
 *
 */
void check_if_logarithmic (col_struct *data)
{
        float xmax, xmin, ymax, ymin;

        xmin = col_min ( data->total_dose, data->current_size, x_log );
        xmax = col_max ( data->total_dose, data->current_size, x_log );
        x_log = 0;

        ymin = col_min ( data->total_dose, data->current_size, y_log );
        ymax = col_max ( data->total_dose, data->current_size, y_log );

	if ( (data->gp1_fluence_enabled) | (data->gp2_fluence_enabled) |
	     (data->thermal_fluence_enabled) | (data->gamma_prod_enabled) |
             (data->ultra_gamma_prod_enabled) )
	     y_log = 1;
        else if ( ymax/ymin >= 50.0 )
             y_log = 1;
	else
             y_log = 0;

}

