/* rtt_output.c - A function to build the source file for input to 
 *                the Monte Carlo transport code.
 *  
 *  by: Ray S. Babcock, Montana State University
 *      August 13, 1996
 */

#include "rtt.h"
#include "rtt_mc.h"
#include "file_tools.h"
#include <errno.h>
#include <sys/types.h>

int checkTargetLoc ( );

/* local prototypes */

int
rtt_output()
{

    int i;
    int nbatch, nhist, nedit2;
    int num_convert;
    FILE * o_ptr;
    double wncut;
    double Xp,Yp,Zp;
    double Zb, phi, theta;
    double x1, x2, x3;
    char buffer1[256], buffer2[256], buffer3[256];
    int success;
    /*char * currentField;*/
    char * s1;
    
    DEBUG_TRACE_IN printf("Entering rtt_output\n");
    
    /*
     *  This prevents the code from writing a file called CANCEL
     *  in the event that the Cancel button is hit on the FSB
     */
    if (CONFIRM == 0) {

/*
 *  Start by saving the save directory
 */
        s1 = XmTextGetString ( rtt_file_data->textinfo[0] );
        sscanf ( s1, "%s", rtt_file_data->saveDirectory );
        rtt_file_data->saveDirectoryValid = 1;

        if( rtt_file_data->originalInputFileValid )
        {
/*
 *  Do some error checking here, too - mainly, looking for unrealistic input combinations
 */
            if ( checkTargetLoc( ) ) {
               DEBUG_TRACE_OUT printf("Leaving rtt_output\n");
               return (0);
            }

            (void) getValueFromTextBox( rtt_file_data->rtt_control_text2, buffer1, NO_EXPAND );
            sscanf( buffer1, "%d", &nhist );
            if ( nhist > MAX_NHIST ) {
               DT_error ( rtt_file_data->rtt_control_text2, 
                          "nhist has a maximum allowable value of 2000 histories per batch",
                          NULL, NULL);
               DEBUG_TRACE_OUT printf("Leaving rtt_output\n");
               return (0);
            }

/*
 *  Now, can open file and start saving data to it
 */
            if( FT_filenameEndsIn( rtt_file_data->originalInputFile, ".input" ) == 0 )
                strcat( rtt_file_data->originalInputFile, ".input" );

            /* open the file */
            o_ptr = fopen( rtt_file_data->originalInputFile, "w" );
            
            if( o_ptr == NULL)
            {
                fprintf(stderr,"Could not output file %s\n", rtt_file_data->originalInputFile);
                DEBUG_TRACE_OUT printf("Leaving rtt_output\n");
                return (0);
            }

            /* write file */

            /* Record 1 : title line */
            (void) getValueFromTextBox( rtt_file_data->rtt_title_text, buffer1, NO_EXPAND );
            fprintf( o_ptr, "%s\n", buffer1 );

            /* Record 2 : nbatch, nhist, wncut (default 0.01) */
            
            /* convert nbatch */
            (void) getValueFromTextBox( rtt_file_data->rtt_control_text1, buffer1, NO_EXPAND );
            sscanf( buffer1, "%d", &nbatch );
            
            /* convert wncut */
            (void) getValueFromTextBox( rtt_file_data->rtt_tally_text3, buffer1, NO_EXPAND );
            sscanf( buffer1, "%lf", &wncut );
            
            fprintf(o_ptr, "%10d%10d%20E\n", nbatch, nhist, wncut);

            /* Record 3 : Target Point */
            /* convert Xp, Yp, Zp */
            (void) getValueFromTextBox( rtt_file_data->rtt_position_text1, buffer1, NO_EXPAND );
            sscanf( buffer1, "%lf %lf %lf", &Xp, &Yp, &Zp );
            
            fprintf(o_ptr, "%20.7E%20.7E%20.7E\n", Xp, Yp, Zp);

            /* Record 4 : Zb, phi, theta */
            /* convert Zb, phi, theta */
            (void) getValueFromTextBox( rtt_file_data->rtt_position_text2, buffer1, NO_EXPAND );
            sscanf( buffer1, "%lf", &Zb );

            (void) getValueFromTextBox( rtt_file_data->rtt_position_text3, buffer1, NO_EXPAND );
            sscanf( buffer1, "%lf", &phi );

            (void) getValueFromTextBox( rtt_file_data->rtt_position_text4, buffer1, NO_EXPAND );
            sscanf( buffer1, "%lf", &theta );
            
            fprintf(o_ptr, "%20.7E%20.7E%20.7E\n", Zb, phi, theta);

            /* Record 5 : CG geometry file name */
            (void) getValueFromTextBox( rtt_file_data->textinfo[1], buffer1, NO_EXPAND );
            fprintf( o_ptr, "%s\n", buffer1 );

            /* Record 6 : univel file name */
            (void) getValueFromTextBox( rtt_file_data->textinfo[2], buffer1, NO_EXPAND );
            if( strlen( buffer1 ) <= 0 )
                fprintf( o_ptr, "none\n" );
            else
                fprintf( o_ptr, "%s\n", buffer1 );

            /* Record 7 : nedit2, number_energy_bins */
            (void) getValueFromTextBox( rtt_file_data->rtt_tally_text2a, buffer1, NO_EXPAND );
            sscanf( buffer1, "%d", &nedit2 );
            fprintf( o_ptr,"%10d%10d\n", nedit2, rtt_file_data->number_energy_bins );

            /* Record 8 : Cross section group DEFAULT */
            fprintf( o_ptr,"%10d%10d%10d\n", 
                     rtt_file_data->break_point1,
                     rtt_file_data->break_point2,
                     rtt_file_data->break_point3 );

            /* Record 9 : Low coordinates of subelement mesh */
            /* parse into three values */
            (void) getValueFromTextBox( rtt_file_data->rtt_tally_text1, buffer1, NO_EXPAND );
            num_convert = sscanf( buffer1, "%le %le %le", &x1, &x2, &x3 );

            if(num_convert != 3)
            { /* fatal error */
                if(num_convert <= 0) x1 = x2 = x3 = -15.0;
                if(num_convert == 1) x2 = x3 = -15.0;
                if(num_convert == 2) x3 = -15.0;
            }
            
            fprintf(o_ptr, "%20.8E%20.8E%20.8E\n", x1, x2, x3);

            /* Record 10: delw */
            /* parse value */
            (void) getValueFromTextBox( rtt_file_data->rtt_tally_text2, buffer1, NO_EXPAND );
            num_convert = sscanf( buffer1, "%le", &x1 );
            
            if(num_convert != 1) x1 = 1.0;
            
            fprintf(o_ptr, "%20.8E%10d\n", x1, rtt_file_data->ntrk);

            /* Record 11: old restart file or none */
            (void) getValueFromTextBox( rtt_file_data->textinfo[3], buffer1, NO_EXPAND );
            if( strlen( buffer1 ) <= 0 || strcmp( buffer1, "none" ) == 0 )
                fprintf( o_ptr, "none\n" );
            else
                fprintf( o_ptr, "%s\n", buffer1 );
    
            /* Record 12: new restart file or none */
            (void) getValueFromTextBox( rtt_file_data->textinfo[4], buffer1, NO_EXPAND );
            if( strlen( buffer1 ) <= 0 || strcmp( buffer1, "none" ) == 0 )
                fprintf( o_ptr, "none\n" );
            else
                fprintf( o_ptr, "%s%s\n", rtt_file_data->saveDirectory, buffer1 );

            /* Record 13: beam source or dose table file name or none*/
            (void) getValueFromTextBox( rtt_file_data->textinfo[5], buffer1, NO_EXPAND );
            if( strlen( buffer1 ) <= 0 )
                fprintf( o_ptr, "none\n" );
            else
                fprintf( o_ptr, "%s\n", buffer1 );

            /* Record 14: random_seed and debug_flag */
            fprintf(o_ptr,"%10d%10d\n",
                    rtt_file_data->random_seed,
                    rtt_file_data->debug_flag);

            /* Record 15: run_dir run_date */
            (void) getValueFromTextBox( rtt_file_data->rtt_control_text3, buffer1, NO_EXPAND );
            (void) getValueFromTextBox( rtt_file_data->rtt_control_text4, buffer2, NO_EXPAND );
            
            fprintf(o_ptr, "%-19s %-20s\n", buffer1, buffer2);

            /* Record 16: material file name or none */
            (void) getValueFromTextBox( rtt_file_data->textinfo[6], buffer1, NO_EXPAND );
            if( strlen( buffer1 ) <= 0 )
                fprintf(o_ptr, "none\n");
            else
                fprintf(o_ptr, "%s\n", buffer1);

            /* Record 17: cross section file name or none */
            (void) getValueFromTextBox( rtt_file_data->textinfo[7], buffer1, NO_EXPAND );
            if( strlen( buffer1 ) <= 0 )
                fprintf(o_ptr, "none\n");
            else
                fprintf(o_ptr, "%s\n", buffer1);

            /* Record 18: */
            fprintf(o_ptr,"%4d%4d%4d%4d%4d%4d%4d%11.4E%11.4E%11.4E%11.4E%11.4E%11.4E%11.4E\n", 
                    rtt_file_data->nuclide_id[0],
                    rtt_file_data->nuclide_id[1],
                    rtt_file_data->nuclide_id[2],
                    rtt_file_data->nuclide_id[3],
                    rtt_file_data->nuclide_id[4],
                    rtt_file_data->nuclide_id[5],
                    rtt_file_data->nuclide_id[6],
                    rtt_file_data->nuclide_density[0],
                    rtt_file_data->nuclide_density[1],
                    rtt_file_data->nuclide_density[2],
                    rtt_file_data->nuclide_density[3],
                    rtt_file_data->nuclide_density[4],
                    rtt_file_data->nuclide_density[5],
                    rtt_file_data->nuclide_density[6]);

            /* Record 19: code_vers */
            (void) getValueFromTextBox( rtt_file_data->textinfo[8], buffer1, NO_EXPAND );
            if( strlen( buffer1 ) <= 0 )
                fprintf(o_ptr, "none\n");
            else
                fprintf(o_ptr, "%s\n", buffer1);

            /* Records 20-22: ultrafast data files */
            s1 = XmTextGetString(rtt_file_data->rtt_control_text3);
            if( strchr(s1,'U') || strchr(s1,'P') ) {
                (void) getValueFromTextBox( rtt_file_data->textinfo[9],  buffer1, NO_EXPAND );
                (void) getValueFromTextBox( rtt_file_data->textinfo[10], buffer2, NO_EXPAND );
                (void) getValueFromTextBox( rtt_file_data->textinfo[11], buffer3, NO_EXPAND );
                fprintf(o_ptr, "%s\n", buffer1);
                fprintf(o_ptr, "%s\n", buffer2);
                fprintf(o_ptr, "%s\n", buffer3);
            }
            XtFree( s1 );

            /* pre-processor output */
            
            /********************************
             * MTC 12/23/1999
             * Commented this out because the
             * transbs matrix is no longer being used
             **********************************
              if(rtt_file_data->rtt_transbs_saveflag)
              {
              fprintf(o_ptr, "\ntrans_bs\n");
              
              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_matrix[0], buffer1, NO_EXPAND );
              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_matrix[1], buffer2, NO_EXPAND );
              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_matrix[2], buffer3, NO_EXPAND );
              fprintf(o_ptr, " %s %s %s\n", buffer1, buffer2, buffer3);

              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_matrix[3], buffer1, NO_EXPAND );
              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_matrix[4], buffer2, NO_EXPAND );
              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_matrix[5], buffer3, NO_EXPAND );
              fprintf(o_ptr, " %s %s %s\n", buffer1, buffer2, buffer3);
                
              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_matrix[6], buffer1, NO_EXPAND );
              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_matrix[7], buffer2, NO_EXPAND );
              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_matrix[8], buffer3, NO_EXPAND );
              fprintf(o_ptr, " %s %s %s\n", buffer1, buffer2, buffer3);

              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_scale, buffer1, NO_EXPAND );
              fprintf(o_ptr, " %s\n", buffer1);
                
              (void) getValueFromTextBox( rtt_file_data->rtt_transbs_reindex, buffer1, NO_EXPAND );
              fprintf(o_ptr," %s\n", buffer1);
              }
            */
            
            if(rtt_file_data->rtt_iop_saveflag)
            {
                /* Nothing prints for iop=1 */
                if( rtt_file_data->rtt_iop_button != 1 )
                {
                    if ( rtt_file_data->rtt_iop_plusflag == 1 )
                        fprintf(o_ptr,"\niop+\n");
                    else 
                        fprintf(o_ptr,"\niop\n");

                    fprintf( o_ptr, " %d %s %s\n",
                             rtt_file_data->rtt_iop_button,
                             rtt_file_data->rtt_iop_regions[0],
                             rtt_file_data->rtt_iop_regions[1] );

                    fprintf( o_ptr, " %-.2f\n", rtt_file_data->rtt_iop_zsep );

                    if( rtt_file_data->rtt_iop_button == 2 )
                    {
                        fprintf( o_ptr, " %-.2f %-.2f %-.2f\n",
                                 rtt_file_data->rtt_iop_beamline[0],
                                 rtt_file_data->rtt_iop_beamline[1],
                                 rtt_file_data->rtt_iop_beamline[2] );
                    }
                }
            }
            
            /********************************
             * MTC 12/23/1999
             * Commented this out because the
             * excluded regions are no longer being used.
             **********************************
             *
             if(rtt_file_data->rtt_excluded_saveflag)
             {
             (void) getValueFromTextBox( rtt_file_data->rtt_excluded_text1, buffer1, NO_EXPAND );
             
             fprintf(o_ptr, "\nexclude %s\n", buffer1);
             }
            */
            
            /* Edit directives */
            fprintf(o_ptr,"\nedit_dir\n");
            s1 = XmTextGetString(rtt_file_data->rtt_directive_text1);
            fprintf(o_ptr,"%s", s1);
            XtFree( s1 );

            /* close file */
            fclose(o_ptr);
            
        } /* end on saveDirectoryValid */
        else
        {
            DT_error( rtt_file_data->rtt_shell, "You must do a Save As first!", NULL, NULL );
            return (0);
        }
    } /* end on CONFIRM == 0 */
    
    DEBUG_TRACE_OUT printf("Leaving rtt_output\n");

    return (1);

} /* end of rtt_output */




/******************************************************************************/

int checkTargetLoc ( )

{

   int    num_convert, k, nedit2;
   char   s1[256];
   double delw, xp, yp, zp, x0, y0, z0, center;

   DEBUG_TRACE_IN printf("Entering checkTargetLoc\n");

/*
 *  Gather the values from the input widget
 */

   (void) getValueFromTextBox( rtt_file_data->rtt_position_text1, s1, NO_EXPAND );
   sscanf( s1, "%lf %lf %lf", &xp, &yp, &zp );

   (void) getValueFromTextBox( rtt_file_data->rtt_tally_text1, s1, NO_EXPAND );
   num_convert = sscanf( s1, "%le %le %le", &x0, &y0, &z0 );
   if(num_convert != 3)
   { /* fatal error */
       if(num_convert <= 0) x0 = y0 = z0 = -15.0;
       if(num_convert == 1) y0 = z0 = -15.0;
       if(num_convert == 2) z0 = -15.0;
   }

   (void) getValueFromTextBox( rtt_file_data->rtt_tally_text2a, s1, NO_EXPAND );
   num_convert = sscanf( s1, "%d", &nedit2 );
   if(num_convert != 1) nedit2 = 30;

   (void) getValueFromTextBox( rtt_file_data->rtt_tally_text2, s1, NO_EXPAND );
   num_convert = sscanf( s1, "%le", &delw );
   if(num_convert != 1) delw = 1.0;

/*
 *  Now, check to see if the origin and delw place (0,0,0) in the center of the
 *  edit mesh grid
 */

   center = nedit2*delw/2.0;
   if ( fabs(x0 + center) > 0.01 || fabs(y0 + center) > 0.01 || fabs(z0 + center) > 0.01 ) {
      k = DT_decide ( rtt_file_data->rtt_shell, rtt_app, "The edit mesh you have specified is not centered about the point (0,0,0).\nThis will result in the patient geometry not being centered, and may produce\nunreliable results.  If this was your intent, press Continue.", NULL, "Continue", "Cancel" );
      if ( !k ) {
          DEBUG_TRACE_OUT printf("Leaving checkTargetLoc\n");
          return 1;
      }
   }


/*
 *  Check to see if the target point lies within the edit mesh grid
 */

   center *= 2.0;
   if ( xp < x0 || xp > x0 + center || yp < y0 || yp > y0 + center || zp < z0 || zp > z0 + center ) {
      DT_error ( rtt_file_data->rtt_shell, "The specified target point does not lie within the defined edit mesh.\nPlease make the appropriate adjustments.", NULL, NULL );
      DEBUG_TRACE_OUT printf("Leaving checkTargetLoc\n");
      return 1;
   }

   DEBUG_TRACE_OUT printf("Leaving checkTargetLoc\n");
   return 0;
}




/******************************************************************************/
