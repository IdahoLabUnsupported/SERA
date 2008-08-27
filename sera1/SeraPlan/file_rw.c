#include "basic.h"
#include "data.h"
#include "editdata.h"
#include "dose.h"
#include "file_rw.h"
#include "debug_tools.h"
#include "memory_tools.h"

void read_rst_file ( dose_struct *dose, FILE *file, int *irr )

{

   char test, tmpstr[MAX_FILE];
   int  bsize, i, j, nned;

/*
 *  Read number of data sets, case title, file names, and rtt_MC code version
 */

   DEBUG_TRACE_IN printf ("Entering read_rst_file\n");

   *irr = 0;

   fscanf ( file, "%4d%12s\n", &dose->nset, dose->vers_stmp );
   fscanf ( file, "%80s\n", dose->plan_file_name );
   fscanf ( file, "%80c\n", dose->title );
   fscanf ( file, "%80s\n", dose->geomfile );
   fscanf ( file, "%80s\n", dose->bsg_file );
   fscanf ( file, "%80s\n", dose->matfile );
   fscanf ( file, "%80s\n", dose->sigmafile );
   fscanf ( file, "%12s\n", dose->code_vers );

/*
 *  Read edit mesh information and number of energy groups
 */

   DEBUG_TRACE_IN printf ("Reading edit mesh info\n");

   fscanf ( file, "%15le", dose->x0);
   fscanf ( file, "%15le", dose->y0);
   fscanf ( file, "%15le", dose->z0);
   fscanf ( file, "%15le", dose->delw);
   fscanf ( file, "%8d", &dose->nedit);
   fscanf ( file, "%8d\n", dose->nedt);
   for ( i = 0; i < *dose->nedt; i++ ) {
      if ( (i+1)%10 && (i+1)%*dose->nedt )
         fscanf ( file, "%8d", &dose->iged[i] );
      else
         fscanf ( file, "%8d\n", &dose->iged[i] );
   }

/*
 *  Read edit directives, date, source info, number of histories run, 
 *  gamma ratio, sourcefile, and the starting random number
 */

   DEBUG_TRACE_IN printf ("Reading calculation info for each field/fraction\n");

   for ( i = 0; i < dose->nset; i++ ) {
      fscanf ( file, "%6c", dose->sets[i].run_dir );
      fscanf ( file, "%15c", dose->sets[i].date );
      fscanf ( file, "%15le", dose->sets[i].rel_wt );
      fscanf ( file, "%15le", dose->sets[i].wn0 );
      fscanf ( file, "%15le\n", dose->sets[i].s_tot );
      fscanf ( file, "%d", dose->sets[i].nnhist );
      fscanf ( file, "%d", dose->sets[i].nghist );
      fscanf ( file, "%d", dose->sets[i].nfhist );
      fscanf ( file, "%le\n", dose->sets[i].gamratio );
      fscanf ( file, "%80s\n", dose->sets[i].sourcefile );
      fscanf ( file, "%10d\n", dose->sets[i].irand_num );

/*
 *  Read source positioning parameters and restart file name (input)
 */

      fscanf ( file, "%15le", dose->sets[i].xp );
      fscanf ( file, "%15le", dose->sets[i].yp );
      fscanf ( file, "%15le", dose->sets[i].zp );
      fscanf ( file, "%15le", dose->sets[i].zb );
      fscanf ( file, "%15le", dose->sets[i].phi );
      fscanf ( file, "%15le\n", dose->sets[i].theta );
      fscanf ( file, "%80s\n", dose->sets[i].new_rst );

/*
 *  Read # batches, # histories, cutoff weight, nuclide IDs, and nuclide densities
 */

      fscanf ( file, "%8d", dose->sets[i].nbatch );
      fscanf ( file, "%8d", dose->sets[i].nhist );
      fscanf ( file, "%15le", dose->sets[i].wncut );
      fscanf ( file, "%8d", dose->sets[i].id_b10 );
      fscanf ( file, "%8d", dose->sets[i].id_h );
      fscanf ( file, "%8d", dose->sets[i].id_n );
      fscanf ( file, "%8d", dose->sets[i].id_c );
      fscanf ( file, "%8d", dose->sets[i].id_o );
      fscanf ( file, "%c", &test );
      if ( test == ' ' ) {
         *irr = 1;
         fscanf ( file, "%7d", dose->sets[i].id_rr1 );
         fscanf ( file, "%8d\n", dose->sets[i].id_rr2 );
      }
      fscanf ( file, "%15le", dose->sets[i].b10_dens );
      fscanf ( file, "%15le", dose->sets[i].h_dens );
      fscanf ( file, "%15le", dose->sets[i].n_dens );
      fscanf ( file, "%15le", dose->sets[i].c_dens );
      fscanf ( file, "%15le", dose->sets[i].o_dens );
      if ( *irr ) {
         fscanf ( file, "%15le", dose->sets[i].rr1_dens );
         fscanf ( file, "%15le\n", dose->sets[i].rr2_dens );
      }
      else {
         fscanf ( file, "\n" );
      }

/*
 *  Read the beam entry point, and the beam directional vector
 */

      for ( j = 0; j < 3; j++ )
         fscanf ( file, "%15le", &dose->sets[i].entry[j] );

      for ( j = 0; j < 2; j++ )
         fscanf ( file, "%15le", &dose->sets[i].bvec[j] );
      fscanf ( file, "%15le\n", &dose->sets[i].bvec[2] );
   }

/*
 *  Setup and read bflux array - dose data by physical dose component
 */

   DEBUG_TRACE_IN printf ("Reading bflux array\n");

   nned = ( *irr ? MAX_NNED : MAX_NNED-2 );
   bsize = dose->nedit * dose->nedit * dose->nedit * nned;
   dose->bflux = (float *) MT_malloc ( bsize * sizeof ( float ) );
   for ( i = 0; i < bsize; i++ ) {
      if ( (i+1)%6 && (i+1)%bsize )
         fscanf ( file, "%15e", &dose->bflux[i] );
      else
         fscanf ( file, "%15e\n", &dose->bflux[i] );
   }

   DEBUG_TRACE_OUT printf ("Done with read_rst_file\n");

   return;

}




/******************************************************************************/

void write_rst_file ( dose_struct *dose, FILE *file, int irr )

{

   int  bsize, i, j, nned;

/*
 *  Write number of data sets, case title, file names, and rtt_MC code version
 */

   DEBUG_TRACE_IN printf ("Entering write_rst_file\n");

   fprintf ( file, "%4d%12.12s\n", dose->nset, dose->vers_stmp );
   bsize = strlen ( dose->plan_file_name ) - 1;
   for ( i = 0; i < bsize; i++ )
      fprintf ( file, "%c", dose->plan_file_name[i] );
   fprintf ( file, "%c\n", dose->plan_file_name[bsize] );
/*
 *  Check for embedded carriage returns in the title - happens occasionally
 */
   bsize = strlen ( dose->title );
   if ( dose->title[bsize-1] == '\n' )
      dose->title[bsize-1] = 0;

   bsize = strlen ( dose->title ) - 1;
   for ( i = 0; i < bsize; i++ )
      fprintf ( file, "%c", dose->title[i] );
   fprintf ( file, "%c\n", dose->title[bsize] );

   bsize = strlen ( dose->geomfile ) - 1;
   for ( i = 0; i < bsize; i++ )
      fprintf ( file, "%c", dose->geomfile[i] );
   fprintf ( file, "%c\n", dose->geomfile[bsize] );

   bsize = strlen ( dose->bsg_file ) - 1;
   for ( i = 0; i < bsize; i++ )
      fprintf ( file, "%c", dose->bsg_file[i] );
   fprintf ( file, "%c\n", dose->bsg_file[bsize] );

   bsize = strlen ( dose->matfile ) - 1;
   for ( i = 0; i < bsize; i++ )
      fprintf ( file, "%c", dose->matfile[i] );
   fprintf ( file, "%c\n", dose->matfile[bsize] );

   bsize = strlen ( dose->sigmafile ) - 1;
   for ( i = 0; i < bsize; i++ )
      fprintf ( file, "%c", dose->sigmafile[i] );
   fprintf ( file, "%c\n", dose->sigmafile[bsize] );

   bsize = strlen ( dose->code_vers ) - 1;
   for ( i = 0; i < bsize; i++ )
      fprintf ( file, "%c", dose->code_vers[i] );
   fprintf ( file, "%c\n", dose->code_vers[bsize] );

/*
 *  Write edit mesh information and number of energy groups
 */

   DEBUG_TRACE_IN printf ("Writing edit mesh info\n");

   fprintf ( file, "%15e", *dose->x0);
   fprintf ( file, "%15e", *dose->y0);
   fprintf ( file, "%15e", *dose->z0);
   fprintf ( file, "%15e", *dose->delw);
   fprintf ( file, "%8d", dose->nedit);
   fprintf ( file, "%8d\n", *dose->nedt);
   for ( i = 0; i < *dose->nedt; i++ ) {
      if ( (i+1)%10 && (i+1)%*dose->nedt )
         fprintf ( file, "%8d", dose->iged[i] );
      else
         fprintf ( file, "%8d\n", dose->iged[i] );
   }

/*
 *  Write run directives, date, source info, number of histories run, 
 *  gamma ratio, sourcefile, and the starting random number
 */

   DEBUG_TRACE_IN printf ("Writing info for each field/fraction\n");

   for ( i = 0; i < dose->nset; i++ ) {
      fprintf ( file, "%6.6s", dose->sets[i].run_dir );
      fprintf ( file, "%15.15s", dose->sets[i].date );
      fprintf ( file, "%15e", *dose->sets[i].rel_wt );
      fprintf ( file, "%15e", *dose->sets[i].wn0 );
      fprintf ( file, "%15e\n", *dose->sets[i].s_tot );
      fprintf ( file, "%9d", *dose->sets[i].nnhist );
      fprintf ( file, "%9d", *dose->sets[i].nghist );
      fprintf ( file, "%9d", *dose->sets[i].nfhist );
      fprintf ( file, "%15e\n", *dose->sets[i].gamratio );

      bsize = strlen ( dose->sets[i].sourcefile ) - 1;
      for ( j = 0; j < bsize; j++ )
         fprintf ( file, "%c", dose->sets[i].sourcefile[j] );
      fprintf ( file, "%c\n", dose->sets[i].sourcefile[bsize] );

      fprintf ( file, "%10d\n", *dose->sets[i].irand_num );

/*
 *  Write source positioning parameters and restart file name (input)
 */

      fprintf ( file, "%15e", *dose->sets[i].xp );
      fprintf ( file, "%15e", *dose->sets[i].yp );
      fprintf ( file, "%15e", *dose->sets[i].zp );
      fprintf ( file, "%15e", *dose->sets[i].zb );
      fprintf ( file, "%15e", *dose->sets[i].phi );
      fprintf ( file, "%15e\n", *dose->sets[i].theta );

      bsize = strlen ( dose->sets[i].new_rst ) - 1;
      for ( j = 0; j < bsize; j++ )
         fprintf ( file, "%c", dose->sets[i].new_rst[j] );
      fprintf ( file, "%c\n", dose->sets[i].new_rst[bsize] );

/*
 *  Write # batches, # histories, cutoff weight, nuclide IDs, and nuclide densities
 */

      fprintf ( file, "%8d", *dose->sets[i].nbatch );
      fprintf ( file, "%8d", *dose->sets[i].nhist );
      fprintf ( file, "%15e", *dose->sets[i].wncut );
      fprintf ( file, "%8d", *dose->sets[i].id_b10 );
      fprintf ( file, "%8d", *dose->sets[i].id_h );
      fprintf ( file, "%8d", *dose->sets[i].id_n );
      fprintf ( file, "%8d", *dose->sets[i].id_c );
      fprintf ( file, "%8d", *dose->sets[i].id_o );
      if ( irr ) {
         fprintf ( file, "%8d", *dose->sets[i].id_rr1 );
         fprintf ( file, "%8d\n", *dose->sets[i].id_rr2 );
      }
      else {
         fprintf ( file, "\n" );
      }
      fprintf ( file, "%15e", *dose->sets[i].b10_dens );
      fprintf ( file, "%15e", *dose->sets[i].h_dens );
      fprintf ( file, "%15e", *dose->sets[i].n_dens );
      fprintf ( file, "%15e", *dose->sets[i].c_dens );
      fprintf ( file, "%15e", *dose->sets[i].o_dens );
      if ( irr ) {
         fprintf ( file, "%15e", *dose->sets[i].rr1_dens );
         fprintf ( file, "%15e\n", *dose->sets[i].rr2_dens );
      }
      else {
         fprintf ( file, "\n" );
      }

/*
 *  Write the beam entry point, and the beam directional vector
 */

      for ( j = 0; j < 3; j++ )
         fprintf ( file, "%15e", dose->sets[i].entry[j] );

      for ( j = 0; j < 2; j++ )
         fprintf ( file, "%15e", dose->sets[i].bvec[j] );
      fprintf ( file, "%15e\n", dose->sets[i].bvec[2] );
   }

/*
 *  Write bflux array - dose data by physical dose component
 */

   DEBUG_TRACE_IN printf ("Writing bflux array\n");

   nned = ( irr ? MAX_NNED : MAX_NNED-2 );
   bsize = dose->nedit * dose->nedit * dose->nedit * nned;
   for ( i = 0; i < bsize; i++ ) {
      if ( (i+1)%6 && (i+1)%bsize )
         fprintf ( file, "%15e", dose->bflux[i] );
      else
         fprintf ( file, "%15e\n", dose->bflux[i] );
   }

   DEBUG_TRACE_OUT printf ("Done with write_rst_file\n");

   return;

}




/******************************************************************************/

void read_plan_file ( char *file_in )

{

   FILE *plan_file;
   int   i, j;

   DEBUG_TRACE_IN printf ("Entered read_plan_file\n");

/*
 *  Read in and store a plan file on file file_in - start by opening file
 */

   plan_file = fopen ( file_in, "r" );

/*
 *  Read the entire contents of the plan_data structure from the file
 *  This is a text file - no binary reads
 *
 *  Start with general information (patient ID, plan, fractions, fields)
 */

   fscanf ( plan_file, "Patient: %[^\n]\n", data.patient_ID );
   fscanf ( plan_file, "Treatment date: %[^\n]\n", data.treat_date );
   fscanf ( plan_file, "FRACTIONS=%d FIELDS=%d\n", &data.FRACTIONS, &data.FIELDS );

/*
 *  Now, read the field data for each fraction, followed by the fraction sums
 */

   DEBUG_TRACE_IN printf ("Reading field/fraction data\n");

   for ( i = 0; i < data.FRACTIONS; i++ ) {
      fscanf ( plan_file, "Fraction %d\n", &i );
      for ( j = 0; j < data.FIELDS; j++ ) {
         fscanf ( plan_file, "Field %d\n", &j );
         fscanf ( plan_file, "%s\n", data.field_file[i][j] );
         fscanf ( plan_file, "%g %g %g %d\n", &data.field_B10[i][j], &data.field_EXPOSURE[i][j],
                  &data.field_GAMMA[i][j], &data.field_ACTIVE[i][j] );
      }
      fscanf ( plan_file, "Fraction sums %g %g %g %g\n", &data.fraction_BAVE[i],
               &data.fraction_BEFF[i], &data.fraction_EXPOSURE[i], &data.fraction_WEIGHT[i] );
  }

/*
 *  Finish up with the plan sums
 */

   DEBUG_TRACE_IN printf ("Reading plan sum data\n");

   fscanf ( plan_file, "Plan sums %g %g %g\n", &data.total_BAVE, &data.total_BEFF,
            &data.total_EXPOSURE );

/*
 *  Close file
 */

   fclose ( plan_file );

   DEBUG_TRACE_OUT printf ("Done with read_plan_file\n");

   return;

}




/******************************************************************************/

void read_edit_file ( char *file_in )

{

   FILE *edit_file;
   char *tmpstr;
   int   i, j;

   DEBUG_TRACE_IN printf ("Entered read_edit_file\n");

/*
 *  Read in and store an edit file on file file_in - start by opening file
 */

   edit_file = fopen ( file_in, "r" );

/*
 *  Read the edit_data structure from the file
 *  This is a text file - no binary reads
 *
 *  Start with general information - file names and stuff
 */

   fscanf ( edit_file, "Dose file = %s\n", edit_data.dose_file );
   fscanf ( edit_file, "Plan name = %s\n", edit_data.plan_name );
   fscanf ( edit_file, "Patient name = %s\n", edit_data.patient_name );

/*
 *  Basic and reference data
 */

   fscanf ( edit_file, "Reference information\n" );
   fscanf ( edit_file, "%lg %d %d %d %d\n", &edit_data.blood_b10,
                                            &edit_data.n_avg,
                                            &edit_data.upper_dv,
                                            &edit_data.n_bin,
                                            &edit_data.ref_dose_opt );

   fscanf ( edit_file, "%d ", &edit_data.ref_opt );
   if ( (edit_data.ref_opt - 1) ) {
      fscanf ( edit_file, "Point option\n" );
      fscanf ( edit_file, "%lg %lg %lg\n", &edit_data.ref_pt[0],
                                           &edit_data.ref_pt[1],
                                           &edit_data.ref_pt[2] );
   }
   else {
      fscanf ( edit_file, "Volume option\n" );
      fscanf ( edit_file, "%lg\n", &edit_data.ref_b10 );
      fscanf ( edit_file, "%d\n", &edit_data.ref_regions.num_bodies );
      for ( i = 0; i < edit_data.ref_regions.num_bodies; i++ ) {
         fscanf ( edit_file, "%s\n", edit_data.ref_regions.bodies[i] );
      }
   }

/*
 *  RBE data and edit flags
 */

   fscanf ( edit_file, "Reference RBE values\n" );
   for ( i = 0; i < NUM_RBE; i++ ) {
      fscanf ( edit_file, "%lg\n", &edit_data.ref_rbe[i] );
   }

   fscanf ( edit_file, "%d %d\n", &edit_data.perf_edits, &edit_data.calc_edits );

/*
 *  Now, read the user-defined edits, if any are present
 */

   if ( edit_data.calc_edits ) {

/*
 *  Point edits
 */
      fscanf ( edit_file, "Number of point edits = %d\n", &edit_data.points->num_points );
      for ( i = j = 0; i < edit_data.points->num_points; i++ ) {
         fscanf ( edit_file, "%lg %lg %lg\n", &edit_data.points->points[j++],
                                              &edit_data.points->points[j++],
                                              &edit_data.points->points[j++] );
      }

/*
 *  Line edits
 */
      fscanf ( edit_file, "Number of line edits = %d\n", &edit_data.lines->num_lines );
      for ( i = j = 0; i < edit_data.lines->num_lines; i++, j+=3 ) {
         fscanf ( edit_file, "%lg\n", &edit_data.lines->delta[i] );
         fscanf ( edit_file, "%lg %lg %lg\n", &edit_data.lines->line_starts[j],
                                              &edit_data.lines->line_starts[j+1],
                                              &edit_data.lines->line_starts[j+2] );
         fscanf ( edit_file, "%lg %lg %lg\n", &edit_data.lines->line_ends[j],
                                              &edit_data.lines->line_ends[j+1],
                                              &edit_data.lines->line_ends[j+2] );
      }

/*
 *  DV histogram edits
 */
      fscanf ( edit_file, "Number of DV edits = %d\n", &edit_data.boxes->num_boxes );
      for ( i = 0; i < edit_data.boxes->num_boxes; i++ ) {
         fscanf ( edit_file, "%d\n", &edit_data.boxes->bodlist[i].num_bodies );
         for ( j = 0; j < edit_data.boxes->bodlist[i].num_bodies; j++ ) {
            fscanf ( edit_file, "%s\n", edit_data.boxes->bodlist[i].bodies[j] );
         }
      }

/*
 *  Contour edits
 */
      fscanf ( edit_file, "Number of contour edits = %d\n", &edit_data.contours->num_contours );
      for ( i = j = 0; i < edit_data.contours->num_contours; i++, j+=3 ) {
         fscanf ( edit_file, "%s\n", &edit_data.contours->files[i] );
         fscanf ( edit_file, "%lg %lg %lg\n", &edit_data.contours->points[j],
                                              &edit_data.contours->points[j+1],
                                              &edit_data.contours->points[j+2] );
         fscanf ( edit_file, "%lg %lg %lg\n", &edit_data.contours->vector1[j],
                                              &edit_data.contours->vector1[j+1],
                                              &edit_data.contours->vector1[j+2] );
         fscanf ( edit_file, "%lg %lg %lg\n", &edit_data.contours->vector2[j],
                                              &edit_data.contours->vector2[j+1],
                                              &edit_data.contours->vector2[j+2] );
      }
   }

/*
 *  Close file
 */

   fclose ( edit_file );

   DEBUG_TRACE_OUT printf ("Done with read_edit_file\n");

}




/******************************************************************************/

void write_edit_file ( char *file_in )

{

   FILE *edit_file;
   int   i, j;

   DEBUG_TRACE_IN printf ("Entered write_edit_file\n");

/*
 *  Write an edit file on file file_in - start by opening file
 */

   edit_file = fopen ( file_in, "w" );

/*
 *  Write the edit_data structure from the file
 *  This is a text file - no binary reads
 *
 *  Start with general information - file names and stuff
 */

   fprintf ( edit_file, "Dose file = %s\n", edit_data.dose_file );
   fprintf ( edit_file, "Plan name = %s\n", edit_data.plan_name );
   fprintf ( edit_file, "Patient name = %s\n", edit_data.patient_name );

/*
 *  Basic and reference data
 */

   fprintf ( edit_file, "Reference information\n" );
   fprintf ( edit_file, "%g %d %d %d %d\n", edit_data.blood_b10,
                                            edit_data.n_avg,
                                            edit_data.upper_dv,
                                            edit_data.n_bin,
                                            edit_data.ref_dose_opt );

   fprintf ( edit_file, "%d ", edit_data.ref_opt );
   if ( (edit_data.ref_opt - 1) ) {
      fprintf ( edit_file, "Point option\n" );
      fprintf ( edit_file, "%g %g %g\n", edit_data.ref_pt[0],
                                         edit_data.ref_pt[1],
                                         edit_data.ref_pt[2] );
   }
   else {
      fprintf ( edit_file, "Volume option\n" );
      fprintf ( edit_file, "%g\n", edit_data.ref_b10 );
      fprintf ( edit_file, "%d\n", edit_data.ref_regions.num_bodies );
      for ( i = 0; i < edit_data.ref_regions.num_bodies; i++ ) {
         fprintf ( edit_file, "%s\n", edit_data.ref_regions.bodies[i] );
      }
   }

/*
 *  RBE data and edit flags
 */

   fprintf ( edit_file, "Reference RBE values\n" );
   for ( i = 0; i < NUM_RBE; i++ ) {
      fprintf ( edit_file, "%g\n", edit_data.ref_rbe[i] );
   }

   fprintf ( edit_file, "%d %d\n", edit_data.perf_edits, edit_data.calc_edits );

/*
 *  Now, read the user-defined edits, if any are present
 */

   if ( edit_data.calc_edits ) {

/*
 *  Point edits
 */
      fprintf ( edit_file, "Number of point edits = %d\n", edit_data.points->num_points );
      for ( i = j = 0; i < edit_data.points->num_points; i++ ) {
         fprintf ( edit_file, "%g %g %g\n", edit_data.points->points[j++],
                                            edit_data.points->points[j++],
                                            edit_data.points->points[j++] );
      }

/*
 *  Line edits
 */
      fprintf ( edit_file, "Number of line edits = %d\n", edit_data.lines->num_lines );
      for ( i = j = 0; i < edit_data.lines->num_lines; i++, j+=3 ) {
         fprintf ( edit_file, "%g\n", edit_data.lines->delta[i] );
         fprintf ( edit_file, "%g %g %g\n", edit_data.lines->line_starts[j],
                                            edit_data.lines->line_starts[j+1],
                                            edit_data.lines->line_starts[j+2] );
         fprintf ( edit_file, "%g %g %g\n", edit_data.lines->line_ends[j],
                                            edit_data.lines->line_ends[j+1],
                                            edit_data.lines->line_ends[j+2] );
      }

/*
 *  DV histogram edits
 */
      fprintf ( edit_file, "Number of DV edits = %d\n", edit_data.boxes->num_boxes );
      for ( i = 0; i < edit_data.boxes->num_boxes; i++ ) {
         fprintf ( edit_file, "%d\n", edit_data.boxes->bodlist[i].num_bodies );
         for ( j = 0; j < edit_data.boxes->bodlist[i].num_bodies; j++ ) {
            fprintf ( edit_file, "%s\n", edit_data.boxes->bodlist[i].bodies[j] );
         }
      }

/*
 *  Contour edits
 */
      fprintf ( edit_file, "Number of contour edits = %d\n", edit_data.contours->num_contours );
      for ( i = j = 0; i < edit_data.contours->num_contours; i++, j+=3 ) {
         fprintf ( edit_file, "%s\n", edit_data.contours->files[i] );
         fprintf ( edit_file, "%g %g %g\n", edit_data.contours->points[j],
                                            edit_data.contours->points[j+1],
                                            edit_data.contours->points[j+2] );
         fprintf ( edit_file, "%g %g %g\n", edit_data.contours->vector1[j],
                                            edit_data.contours->vector1[j+1],
                                            edit_data.contours->vector1[j+2] );
         fprintf ( edit_file, "%g %g %g\n", edit_data.contours->vector2[j],
                                            edit_data.contours->vector2[j+1],
                                            edit_data.contours->vector2[j+2] );
      }
   }

/*
 *  Close file
 */

   fclose ( edit_file );

   DEBUG_TRACE_OUT printf ("Done with write_edit_file\n");

   return;

}




/******************************************************************************/
