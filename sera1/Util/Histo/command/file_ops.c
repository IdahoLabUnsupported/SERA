#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "dose_vol_struct.h"
#include "file_ops.h"
#include "memory.h"



#define  EPSILON_PLUS   1E-6

extern char usage_message[400];

extern char *CommandsPath;
extern char *HistoPath;

int logarithmic;                /* global flag for logarithmic y scale */


/*
 * This procedure does some initialization of stuff.  
 * Helps to keep the main procedure a little cleaner looking.
 */
void set_up_stuff ()
{

 /* Only initialize these variables if you have well-defined paths to xmgr (BinPath)
  * and to the xmgr template file (RttPath).  If you are using the environment variables
  * (SERAMC_PATH and SERA_PATH), initialize to a null character.
  */
 char *RttPath;
 char *BinPath;

  strcpy (usage_message, "\nUsage:\n$ dosevol file1 file2 c1 c2 c3....\n\nwhere \tfile1 is the input file, \n\tfile2 is the name of the file to write the necessary xmgr commands to, \n\tand c1, c2, c3... denote the columns you want plotted.\n"); 


  /* set up path to find common commands file for all xmgr plots */
  RttPath = (char *)getenv("SERAMC_PATH");
  if (RttPath == NULL) {
    fprintf(stderr, "Environment variable SERAMC_PATH not set - exiting !\n");
    exit(0);
  }
  CommandsPath = (char *)malloc( (strlen(RttPath) + 19) );
  strcpy(CommandsPath, RttPath);
  strcat(CommandsPath, "/lib/HistoCommands");


  /* set up path to find path to xmgr */
  BinPath = (char *)getenv("SERA_HOME");
  if (BinPath == NULL){
    fprintf(stderr, "Environment variable SERA_HOME not set - exiting !\n");
    exit(0);
  }
  HistoPath = (char *)malloc( (strlen(BinPath) + 17) );
  strcpy(HistoPath, BinPath);
  strcat(HistoPath, "/Target/bin/xmgr");
  /*
  printf("HistoPath = %s\n",HistoPath);
  printf("CommandsPath = %s\n",CommandsPath);
   */


}


/*
 * This procedure reads in the name of an input file.
 */
void get_in_file_name (char *name)
{
	printf ("I need the name of the input file.\n");
	scanf ("%s", name);
}








/*
 * This procedure reads in the name of the output file.
 */
void get_out_file_name (char *name)
{
	printf ("I need the name of the output file.\n");
	scanf ("%s", name);
}










/*
 * This procedure writes all the control codes for xmgr to the output file.
 */
void write_standard_commands (FILE *outfile)
{
	FILE *header_file;
	
	int c;                   /* character read from HistoCommands */

	/*
        fprintf(stderr,"Commands path is %s \n",CommandsPath);
	 */


	/* rewind to beginning of file */
	if ((header_file = fopen(CommandsPath, "r")) == NULL) {
		fprintf (stderr,"Can't open commands file %s \n", CommandsPath);
		exit (EXIT_FAILURE);
	}
/*
	if ((header_file = fopen("/usr/people/astrakan/histo/histo/HistoCommands", "r")) == NULL) {
		fprintf (stderr,"Can't open commands file %s \n", CommandsPath);
		exit (EXIT_FAILURE);
	}
*/
	while ( (c = getc(header_file)) != EOF )
		putc (c, outfile);

	fclose (header_file);
}










/*
 * This procedure writes the information concerning the titles
 * to the *.xmgr file.
 *
 */
void write_titles_to ( FILE *outfile, int data_set_num )
{
  fprintf (outfile, 
	   "@    title \"Dose/Volume Relationship, data set %i\"\n",
	   data_set_num);
}










/*
 * This procedure positions the file pointer to the correct position to 
 * start reading in data.  If the end of the data set is read, set the 
 * end flag.
 */
void position_fp_to_start_of_numbers ( FILE *infile, int *not_end, int *not_ultra )
{
	char buf[130];
	static char stop_string[50] = "     interval       total";
	static char test_string[10] = "other";
	int s_len = strlen (stop_string);       

	fgets (buf, 130, infile);
	
	while (   ( strncmp (buf, stop_string, s_len) != 0 )
	       && (*not_end) )
		if ( fgets (buf, 130, infile) == (char *)NULL) {
		  *not_end = 0;
		  return;
		}

        /* Now, check to see if this case was run with version 106 or 107 */

        if ( strstr ( buf, test_string ) != 0 )
           *not_ultra = 0;

	fgets (buf, 130, infile);
	fgets (buf, 130, infile);
		

}

/**********************************************************************/
/* this procedure reads in the line starting with reference.  It stores
 * these fields into total, b10, gamma, n14, and fast _dose_sum fields
 * of the data structure.  LLV
 */
void read_in_reference ( FILE *infile, dose_vol_struct *data, int count, int not_ult)
{
   char buf[130];                     /* input line read from file */
   char *buf_ptr;
   char temp[20];
   int done = 0;
   char total[20], b10[20], gamma[20], n14[20], fast[20], fflux[20], tflux[20], ultra[20];

   do
   {
      fgets (buf, 130, infile);

      /* Overlook the first two columns when reading in these buffers
       * as the new files will have fp 's in these first columns 
       ***/
      buf_ptr = buf;
      buf_ptr = buf_ptr + 2;

      sscanf (buf_ptr, "%s ", temp);
      if (!strcmp(temp, "reference"))
      {
         done = 1;
         if ( not_ult ) {
            sscanf (buf_ptr, "%s %s %s %s %s %s ",
                   temp, total, b10, gamma, n14, fast);
            data->total_dose_sum[count] = atof(total);
            data->b10_dose_sum[count] = atof(b10);
            data->gamma_dose_sum[count] = atof(gamma);
            data->n14_dose_sum[count] = atof (n14);
            data->fast_dose_sum[count] = atof (fast);
         }
         else {
            sscanf (buf_ptr, "%s %s %s %s %s %s %s %s %s ",
                   temp, total, b10, gamma, n14, fast, fflux, tflux, ultra); 
            data->total_dose_sum[count] = atof(total);
            data->b10_dose_sum[count] = atof(b10);
            data->gamma_dose_sum[count] = atof(gamma);
            data->n14_dose_sum[count] = atof (n14);
            data->fast_dose_sum[count] = atof (fast);
            data->ultra_fast_dose_sum[count] = atof (ultra);
         }
      }

   }
   while (done == 0);
}

/* 
 * This procedure reads in the numbers in the columns in the data file.
 */
void read_in_cols ( FILE *infile, dose_vol_struct *data, int not_ult )
{
  char buf[130];                     /* input line read from file */
  char bin_left[10], bin_right[10], dash[10];
  char *buf_ptr;

  
  int count = 0,
      ok = 1;                        /* set to 0 if memory allocated         */
                                     /* for data fields exceeded             */
  
  static char stop_string[50] = "   > 100%";
  int s_len = strlen(stop_string);
  
  
  while (ok) {
    
    if ( count < data->max_size ) {
      fgets (buf, 130, infile);
      
/* Overlook the first two columns when reading in these buffers 
 * as the new files will have fp 's in these first columns 
 ***/
   buf_ptr = buf;
   buf_ptr = buf_ptr + 2;
      
/* everything that was using buf for sscanf and strncmp now uses buf_ptr  
 * to overlook the first two columns
 */
      if ( (strncmp(buf_ptr, stop_string, s_len)) == 0)
	ok = 0;
      
      else {
        if ( not_ult )
	   sscanf (buf_ptr, "%s %s %s %f %f %f %f %f %f %f",
	      	   bin_left, dash, bin_right,
		   &(data->total_dose[count]), 
		   &(data->b10_dose[count]),
		   &(data->gamma_dose[count]), 
		   &(data->n14_dose[count]),
		   &(data->fast_dose[count]), 
		   &(data->fast_flux[count]),
		   &(data->thermal_flux[count]));
        else
	   sscanf (buf_ptr, "%s %s %s %f %f %f %f %f %f %f %f",
	      	   bin_left, dash, bin_right,
		   &(data->total_dose[count]), 
		   &(data->b10_dose[count]),
		   &(data->gamma_dose[count]), 
		   &(data->n14_dose[count]),
		   &(data->fast_dose[count]), 
		   &(data->fast_flux[count]),
		   &(data->thermal_flux[count]),
		   &(data->ultra_fast_dose[count]));

	/* strip out the % */
	bin_left[strlen(bin_left) - 1] = '\0';
	bin_right[strlen(bin_right) - 1] = '\0';

	data->histogram_bin[count].bin_left = atoi (bin_left);
	data->histogram_bin[count].bin_right = atoi (bin_right);
	  
	count++;
      }   /* if strncmp */ 
      
    }   /* if ( sample_count < data->max_size ) */ 
 
    
    else   
      mem_reallocate (data);
    
    
  }   /* while ok */
  

  /* Need to read in the >100% stuff */
  if ( not_ult )
     sscanf (buf_ptr, "%s %s  %f %f %f %f %f %f %f",
	     bin_left, bin_right,
	     &(data->total_dose[count]), 
	     &(data->b10_dose[count]),
	     &(data->gamma_dose[count]), 
	     &(data->n14_dose[count]),
	     &(data->fast_dose[count]), 
	     &(data->fast_flux[count]),
	     &(data->thermal_flux[count]));
  else
     sscanf (buf_ptr, "%s %s  %f %f %f %f %f %f %f %f",
	     bin_left, bin_right,
	     &(data->total_dose[count]), 
	     &(data->b10_dose[count]),
	     &(data->gamma_dose[count]), 
	     &(data->n14_dose[count]),
	     &(data->fast_dose[count]), 
	     &(data->fast_flux[count]),
	     &(data->thermal_flux[count]),
	     &(data->ultra_fast_dose[count]));

  data->histogram_bin[count].bin_left = 100;
  data->histogram_bin[count].bin_right = 110;
	  
  count++;

  data->current_size = count-1;
  
  if (data->current_size < 0) {
    puts ("You probably used a file with the incorrect format.  Bummer.  Try again.");
    puts (usage_message);
    exit (EXIT_FAILURE);
  }
  
}







/* 
 * This procedure writes the numbers from the dose_vol_struct to the *.xmgr file.
 */
void write_cols_to_file (FILE *outfile, dose_vol_struct *data, int counter, int norm_by )
{
/* The write_col2 are similar to write_col except the output bins will be relative
 * to the total dose by the extra parameter that gets passed to this function.
 */
	fprintf (outfile, "@WITH G0\n");
	fprintf (outfile, "@G0 ON\n");
	
	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->histogram_bin, data->total_dose, data->current_size);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
        if ( norm_by )
	   write_col2 (outfile, data->histogram_bin, data->b10_dose, data->current_size,
                      (data->b10_dose_sum[counter] / data->total_dose_sum[counter]));
        else
           write_col (outfile, data->histogram_bin, data->b10_dose, data->current_size);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
        if ( norm_by )
	   write_col2 (outfile, data->histogram_bin, data->gamma_dose, data->current_size,
		      (data->gamma_dose_sum[counter] / data->total_dose_sum[counter]));
        else
           write_col (outfile, data->histogram_bin, data->gamma_dose, data->current_size);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
        if ( norm_by )
	   write_col2 (outfile, data->histogram_bin, data->n14_dose, data->current_size,
		      (data->n14_dose_sum[counter] / data->total_dose_sum[counter]));
        else
           write_col (outfile, data->histogram_bin, data->n14_dose, data->current_size);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
        if ( norm_by )
	   write_col2 (outfile, data->histogram_bin, data->fast_dose, data->current_size,
		      (data->fast_dose_sum[counter] / data->total_dose_sum[counter]));
        else
           write_col (outfile, data->histogram_bin, data->fast_dose, data->current_size);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->histogram_bin, data->fast_flux, data->current_size);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
	write_col (outfile, data->histogram_bin, data->thermal_flux, data->current_size);
	fprintf (outfile, "&\n");

	fprintf (outfile, "@TYPE xy\n");
        if ( norm_by )
	   write_col2 (outfile, data->histogram_bin, data->ultra_fast_dose, data->current_size,
		      (data->ultra_fast_dose_sum[counter] / data->total_dose_sum[counter]));
        else
           write_col (outfile, data->histogram_bin, data->ultra_fast_dose, data->current_size);
	fprintf (outfile, "&\n");

}	









/*
 * This procedure writes a SINGLE column out to the *.xmgr file.
 */
void write_col (FILE *outfile, bin *histo_bin, float *col, int size)
{
  int i;

  float sum = 1.0, last_sum = 1.0;

  for (i = 0;  i <= size;  ++i) {
    sum -= col[i];
    fprintf (outfile, "%d %f\n", histo_bin[i].bin_left, last_sum ); 
    fprintf (outfile, "%d %f\n", histo_bin[i].bin_right, last_sum );
    last_sum = sum;
  }

}


/*
 * This procedure writes a SINGLE column out to the *.xmgr file.
 * This changes the bins from write_col so that they are relative to 
 * the total dose values
 */
void write_col2 (FILE *outfile, bin *histo_bin, float *col, int size, float ratio)
{
  int i;

  float sum = 1.0, last_sum = 1.0;

  for (i = 0;  i <= size;  ++i) {
    sum -= col[i];
    fprintf (outfile, "%f %f\n", (histo_bin[i].bin_left * ratio), last_sum );
    fprintf (outfile, "%f %f\n", (histo_bin[i].bin_right * ratio), last_sum );
    last_sum = sum;
  }

}






/*
 * This procedure writes the appropriate commands to the *.xmgr file
 * depending on which data sets are activated.
 */

void deactivate_sets (FILE *outfile, dose_vol_struct *data)
{
	if (!data->total_dose_enabled)
		fprintf (outfile, "@    kill s0\n");
	
	if (!data->b10_dose_enabled)
		fprintf (outfile, "@    kill s1\n"); 

        if (!data->gamma_dose_enabled)
		fprintf (outfile, "@    kill s2\n");    

	if (!data->n14_dose_enabled)
		fprintf (outfile, "@    kill s3\n");    

        if (!data->fast_dose_enabled)
		fprintf (outfile, "@    kill s4\n");    

        if (!data->fast_flux_enabled)
		fprintf (outfile, "@    kill s5\n");    

        if (!data->thermal_flux_enabled)
		fprintf (outfile, "@    kill s6\n");    

        if (!data->ultra_fast_dose_enabled)
		fprintf (outfile, "@    kill s7\n");    


}










/*
 * This procedure processes command line arguments which determine which
 * variables to use in the plot.  It is assumed that these come after
 * argv[3].  They are assumed to be simple characters.  For example,
 * if argv[4] = "2", then variable #2, which is gamma dose, is enabled.
 * If there are no arguments after argv[3], then ALL are plotted.
 *
 */
int process_variable_enablers (int argc, char *argv[], dose_vol_struct *data, int set_num)
{
  int i, nume, numarg;
  char enabler;
  
  if (argc < 4) {

  /* prompt user for list of enablers */

     printf ( "I need to know what to plot for data set %d.  List all that you need, and"
              " terminate with 's':\n", set_num);
     printf ( "0=total dose, 1=b10 dose  2=gamma dose  3=n-14 dose  4=fast dose\n");
     printf ( "5=fast flux  6=thermal flux  7=other dose\n" );

  /* read enablers one at a time, and process */

     for ( i=0, scanf("%1s", &enabler); enabler != 's'; i++, scanf("%1s", &enabler) )
         switch ( enabler ) {
                case '0': 
	          data->total_dose_enabled = 1;
	          break;
                case '1':
	          data->b10_dose_enabled = 1;
	          break;
                case '2':
	          data->gamma_dose_enabled = 1;
	          break;
                case '3':
	          data->n14_dose_enabled = 1;
	          break;
                case '4':
	          data->fast_dose_enabled = 1;
	          break;
                case '5':
	          data->fast_flux_enabled = 1;
	          break;
                case '6':
	          data->thermal_flux_enabled = 1;
	          break;
                case '7':
	          data->ultra_fast_dose_enabled = 1;
	          break;
                default:
	          fprintf (stderr, "Cannot process %s as a variable enabler.\n", enabler);
	          exit (EXIT_FAILURE);
         }

     /* Fall out here when enabler equals 's' - check if just using default */
     if ( !i )
        data->total_dose_enabled = 1;
     nume = i;

  } /* if */

  /* Enablers from command line */

  else {
    numarg = argc;
    if ( isalpha( *argv[argc-1] ) )
       numarg--;
    for (i = 3; i < numarg; ++i)
      switch ( *argv[i] ) {
      case '0': 
	data->total_dose_enabled = 1;
	break;
      case '1':
	data->b10_dose_enabled = 1;
	break;
      case '2':
	data->gamma_dose_enabled = 1;
	break;
      case '3':
	data->n14_dose_enabled = 1;
	break;
      case '4':
	data->fast_dose_enabled = 1;
	break;
      case '5':
	data->fast_flux_enabled = 1;
	break;
      case '6':
	data->thermal_flux_enabled = 1;
	break;
      case '7':
	data->ultra_fast_dose_enabled = 1;
	break;
      default:
	fprintf (stderr, "Cannot process %s as a variable enabler.\n", argv[i-1]);
	exit (EXIT_FAILURE);
      }

    nume = i-3;

  } /* else */

  return nume;

}









/*
 * read_norm_by_total
 *
 * This function reads the response regarding normalization of the data sets by
 * the total dose, and sets an integer parameter to pass the response to the
 * write_cols_to_files function
 *
 */

int read_norm_by_total ( int argc, char *argv[], int nume )

{

  int i;
  char pans = 'n';

/*
 * Check to see whether answer was supplied on command line
 */
  if ( (argc > nume+3) ) {
     for ( pans = argv[argc-1][0]; ; )
        switch ( pans ) {
            case 'y':
               return 1;
            case 'Y':
               return 1;
            case 'n':
               return 0;
            case 'N':
               return 0;
            default:
               printf ("%c is not a valid response!  Please try again.\n", pans);
               goto prompt;
        }
     }
/*
 * Otherwise, prompt for it
 */
  else
     goto prompt;
     
prompt: printf ( "Do you wish to normalize the dose components by the total dose "
                 "(y/n, default=n)? " );
        scanf ( "%1s", &pans );
        if ( pans == 'y' || pans == 'Y' )
           return 1;
        else
           return 0;

}

