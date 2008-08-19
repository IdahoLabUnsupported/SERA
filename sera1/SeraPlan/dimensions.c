#include "basic.h"
#include "primitive.h"
#include "manager.h"
#include "data.h"
#include "dose.h"
#include "editdata.h"
#include "panel.h"
#include "editpanel.h"
#include "results.h"
#include "dimensions.h"
#include "debug_tools.h"
#include "memory_tools.h"
#include <time.h>
#include <ctype.h>

void initialize_data ( )

/*
 *  Initialize the data structures DATA and EDIT_DATA
 */

{

   int     i, j, k, iwd;
   time_t  timer;

   FILE *fptr;
   char *file, *tmp;

   DEBUG_TRACE_IN printf ("Entering initialize_data\n");

/*
 *  DATA first
 */

   data.FRACTIONS = 2;
   data.FIELDS = 2;

   for ( i = 0; i < MAX_ID; i++ )
      data.patient_ID[i] = 0;
   timer = time ( NULL );
   (void) strftime ( data.treat_date, 50, "%a %d %b %Y %R %Z", localtime ( &timer ) );

   for ( i = 0; i < MAX_FRACTIONS; i++ ) {
      data.fraction_same[i] = 1;
      data.fraction_ACTIVE[i] = data.FIELDS;
      for ( j = 0; j < MAX_FIELDS; j++ ) {
         data.field_ACTIVE[i][j] = 1;
         for ( k = 0; k < MAX_FILE; k++ ) {
            data.field_file[i][j][k] = 0;
            data.base_field_file[j][k] = 0;
         }
         data.field_B10[i][j] = 0.0;
         data.field_EXPOSURE[i][j] = 0.0;
         data.field_GAMMA[i][j] = 1.0;
      }

      data.fraction_BAVE[i] = 0.0;
      data.fraction_BEFF[i] = 0.0;
      data.fraction_EXPOSURE[i] = 0.0;
      data.fraction_WEIGHT[i] = 0.0;
   }

   data.total_BAVE = 0.0;
   data.total_BEFF = 0.0;
   data.total_EXPOSURE = 0.0;

/*
 *  Then EDIT_DATA
 */

   edit_data.n_avg = 1;
   edit_data.n_bin = 11;
   edit_data.upper_dv = 110;

   for ( i = 0; i < NUM_RBE; i++ )
      edit_data.ref_rbe[i] = 1.0;

   edit_data.ref_b10 = edit_data.blood_b10 = 1.0;
   edit_data.ref_dose_opt = 2;
   edit_data.ref_opt = edit_data.calc_edits = 0;

/*
 *  Now, check to see if we have a preferences file - if so,
 *  open and read it, and store the information in DATA and EDIT_DATA
 */

   tmp = getenv ( "SERA_RESOURCES" );
   if ( tmp ) {
      file = (char *) MT_malloc ( MAX_FILE * sizeof (char) );
      strcpy ( file, tmp );
      if ( file[strlen(file)-1] != '/' )
         strcat ( file, "/" );
      strcat ( file, "SeraPlan/SeraPlan.rsc" );
   }

   if ( fptr = fopen ( file, "r" ) ) {
      iwd = fscanf ( fptr, "# SeraPlan preferences resource file\n#\n#  ******************\n#\n" );
      if ( iwd == EOF ) {
         fclose ( fptr );
         MT_free ( (void *) file );
         return;
      }
      iwd = fscanf ( fptr, "Number of fractions = %d\t# 1 - MAX_FRACTIONS\n", &data.FRACTIONS );
      if ( iwd == EOF || !iwd ) {
         fclose ( fptr );
         MT_free ( (void *) file );
         return;
      }
      iwd = fscanf ( fptr, "Number of fields per fraction = %d\t# 1 - MAX_FIELDS\n", &data.FIELDS );
      if ( iwd == EOF || !iwd ) {
         fclose ( fptr );
         MT_free ( (void *) file );
         return;
      }

      iwd = fscanf ( fptr, "#\n#  ******************\n#\n" );
      if ( iwd == EOF ) {
         fclose ( fptr );
         MT_free ( (void *) file );
         return;
      }
      iwd = fscanf ( fptr, "Reference dose type = %d\t# 2=total dose 8=thermal flux 0=gamma dose\n",
                     &edit_data.ref_dose_opt );
      if ( iwd == EOF || !iwd ) {
         fclose ( fptr );
         MT_free ( (void *) file );
         return;
      }

      for ( i = 0; i < NUM_RBE; i++ ) {
         iwd = fscanf ( fptr, "%s RBE = %lf\n", file, &edit_data.ref_rbe[i] );
         if ( iwd == EOF || !iwd ) {
            fclose ( fptr );
            MT_free ( (void *) file );
            return;
         }
      }

      iwd = fscanf ( fptr, "#\n#  ******************\n#\n" );
      if ( iwd == EOF ) {
         fclose ( fptr );
         MT_free ( (void *) file );
         return;
      }
      iwd = fscanf ( fptr, "Number of equal-volume regions for DV edits = %d\t# 1 - 4\n",
                     &edit_data.n_avg );
      if ( iwd == EOF || !iwd ) {
         fclose ( fptr );
         MT_free ( (void *) file );
         return;
      }
      iwd = fscanf ( fptr, "Number of bins for DV histograms = %d\n", &edit_data.n_bin );
      if ( iwd == EOF || !iwd ) {
         fclose ( fptr );
         MT_free ( (void *) file );
         return;
      }
      iwd = fscanf ( fptr, "Upper value for DV histogram bins = %d\n", &edit_data.upper_dv );

      fclose ( fptr );
   }

   MT_free ( (void *) file );

   DEBUG_TRACE_OUT printf ("Done with initialize_data\n");

   return;

}




/******************************************************************************/

void set_up_mem ( panel_data *panel )

/*
 *  Set up memory requirements for the construction panel widgets
 */

{

   int k;

   DEBUG_TRACE_IN printf ("Entering set_up_mem\n");

   panel->frac_form = (Widget *) MT_malloc( MAX_FRACTIONS * sizeof ( Widget ) );
   panel->frac_button = (Widget *) MT_malloc( MAX_FRACTIONS * sizeof ( Widget ) );
   panel->frac_label = (Widget *) MT_malloc( MAX_FRACTIONS * sizeof ( Widget ) );

   panel->avgb10 = (summ_struct *) MT_malloc( MAX_FRACTIONS * sizeof ( summ_struct ) );

   panel->effb10 = (summ_struct *) MT_malloc( MAX_FRACTIONS * sizeof ( summ_struct ) );

   panel->field_label = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );
   panel->file_frame = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );
   panel->file_form = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );
   panel->b10_frame = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );
   panel->exp_frame = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );
   panel->gam_frame = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );

   panel->field_buttons = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );
   panel->field_text = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );
   panel->field_b10 = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );
   panel->field_exposure = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );
   panel->field_gamma = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );
   panel->field_active = (Widget *) MT_malloc( MAX_FRACTIONS * MAX_FIELDS * sizeof ( Widget ) );

   panel->ref_opt = (Widget *) MT_malloc( MAX_REF_OPTS * sizeof ( Widget ) );
   panel->pref_dialog = 0;

   DEBUG_TRACE_OUT printf ("Done with set_up_mem\n");

}




/******************************************************************************/

void set_up_dose_mem ( dose_struct *dose, int num_sets )

{

   int   i;

   DEBUG_TRACE_IN printf ("Entering set_up_dose_mem\n");

   dose->nset = (int *) MT_malloc ( sizeof ( int ) );
   *dose->nset = 0;

   dose->nedit = (int *) MT_malloc ( sizeof ( int ) );
   dose->nedt = (int *) MT_malloc ( sizeof ( int ) );
   dose->iged = (int *) MT_malloc ( MAX_GROUPS * sizeof ( int ) );


   dose->vers_stmp = (char *) MT_malloc ( MAX_VERS * sizeof ( char ) );

   dose->plan_file_name = (char *) MT_malloc ( MAX_FILE * sizeof ( char ) );

   dose->title = (char *) MT_malloc ( MAX_FILE * sizeof ( char ) );
   dose->geomfile = (char *) MT_malloc ( MAX_FILE * sizeof ( char ) );
   dose->bsg_file = (char *) MT_malloc ( MAX_FILE * sizeof ( char ) );
   dose->matfile = (char *) MT_malloc ( MAX_FILE * sizeof ( char ) );
   dose->sigmafile = (char *) MT_malloc ( MAX_FILE * sizeof ( char ) );
   dose->code_vers = (char *) MT_malloc ( MAX_VERS * sizeof ( char ) );


   dose->x0 = (double *) MT_malloc ( sizeof ( double ) );
   dose->y0 = (double *) MT_malloc ( sizeof ( double ) );
   dose->z0 = (double *) MT_malloc ( sizeof ( double ) );
   dose->delw = (double *) MT_malloc ( sizeof ( double ) );


   dose->sets = (set_struct *) MT_malloc ( num_sets * MAX_RUNDIR * sizeof ( set_struct ) );
   for ( i = 0; i < num_sets * MAX_RUNDIR; i++ ) {

      dose->sets[i].run_dir = (char *) MT_malloc ( MAX_RUNDIR * sizeof ( char ) );
      dose->sets[i].date = (char *) MT_malloc ( LN_DATE * sizeof ( char ) );
      dose->sets[i].sourcefile = (char *) MT_malloc ( MAX_FILE * sizeof ( char ) );
      dose->sets[i].new_rst = (char *) MT_malloc ( MAX_FILE * sizeof ( char ) );

      dose->sets[i].nnhist = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].nghist = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].nfhist = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].irand_num = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].nbatch = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].nhist = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].id_b10 = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].id_h = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].id_n = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].id_c = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].id_o = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].id_rr1 = (int *) MT_malloc ( sizeof ( int ) );
      dose->sets[i].id_rr2 = (int *) MT_malloc ( sizeof ( int ) );

      dose->sets[i].rel_wt = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].wn0 = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].s_tot = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].gamratio = (double *) MT_malloc ( sizeof ( double ) );

      dose->sets[i].xp = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].yp = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].zp = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].zb = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].phi = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].theta = (double *) MT_malloc ( sizeof ( double ) );

      dose->sets[i].wncut = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].b10_dens = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].h_dens = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].n_dens = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].c_dens = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].o_dens = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].rr1_dens = (double *) MT_malloc ( sizeof ( double ) );
      dose->sets[i].rr2_dens = (double *) MT_malloc ( sizeof ( double ) );

      dose->sets[i].entry = (double *) MT_malloc ( 6*sizeof ( double ) );
      dose->sets[i].bvec = (double *) MT_malloc ( 3*sizeof ( double ) );
   }

   DEBUG_TRACE_OUT printf ("Done with set_up_dose_mem\n");

   return;

}




/******************************************************************************/

void set_up_edit_mem ( edit_data_struct *edit )

{

   int     i, j, k;
   char    *file, *tmp, tmpstr[MAX_FILE];
   FILE    *fptr;

   DEBUG_TRACE_IN printf ("Entering set_up_edit_mem\n");

   for ( i = 0; i < MAX_ID; i++ ) {
      edit->plan_name[i] = 0;
      edit->patient_name[i] = 0;
   }

   edit->points = (point_edit_struct *) MT_malloc ( sizeof ( point_edit_struct ) );
   edit->points->num_points = 0;
   for ( i = 0; i < MAX_POINTS; i++ )
      edit->points->saved[i] = 0;
   edit->points->points = (double *) MT_malloc ( 3 * MAX_POINTS * sizeof ( double ) );
   for ( i = 0; i < 3*MAX_POINTS; i++ )
      edit->points->points[i] = 0.0;

   edit->lines = (line_edit_struct *) MT_malloc ( sizeof ( line_edit_struct ) );
   edit->lines->num_lines = 0;
   for ( i = 0; i < MAX_LINES; i++ )
      edit->lines->saved[i] = 0;

   edit->lines->delta = (double *) MT_malloc ( MAX_LINES * sizeof ( double ) );
   for ( i = 0; i < MAX_LINES; i++ )
      edit->lines->delta[i] = 0.0;

   edit->lines->line_starts = (double *) MT_malloc ( 3 * MAX_LINES * sizeof ( double ) );

   edit->lines->line_ends = (double *) MT_malloc ( 3 * MAX_LINES * sizeof ( double ) );
   for ( i = 0; i < 3*MAX_LINES; i++ ) {
      edit->lines->line_starts[i] = 0.0;
      edit->lines->line_ends[i] = 0.0;
   }

   edit->boxes = (box_edit_struct *) MT_malloc ( sizeof ( box_edit_struct ) );
   edit->boxes->num_boxes = 0;
   for ( i = 0; i < MAX_BOXES; i++ )
      edit->boxes->saved[i] = 0;
   edit->boxes->bodlist = (body_struct *) MT_malloc ( MAX_BOXES * sizeof ( body_struct ) );

/*
 *  Get information on standard body edits from Preferences file
 */

   tmp = getenv ( "SERA_RESOURCES" );
   if ( tmp ) {
      file = (char *) MT_malloc ( MAX_FILE * sizeof (char) );
      strcpy ( file, tmp );
      if ( file[strlen(file)-1] != '/' )
         strcat ( file, "/" );
      strcat ( file, "SeraPlan/SeraPlan.rsc" );
   }

   if ( fptr = fopen ( file, "r" ) ) {
      while ( strncmp(tmpstr, "Number of extra", 15) ) {
         if ( fgets ( tmpstr, MAX_FILE-1, fptr ) == NULL )
            return;
      }
      sscanf ( tmpstr, "Number of extra DV edits = %d\t# 0 - 10\n", &edit->boxes->num_boxes );
      for ( i = 0; i < edit->boxes->num_boxes; i++ ) {
         fgets ( tmpstr, MAX_FILE-1, fptr );
         tmp = strchr ( tmpstr, '=' ) + 2;
         for ( j = 0, k = 0; *tmp; tmp++ ) {
            if ( *tmp == ' ' ) {
               k++;
               j = 0;
            }
            else if ( *tmp == '\n') {
               k++;
            }
            else {
               edit->boxes->bodlist[i].bodies[k][j] = *tmp;
               j++;
            }
         }
         edit->boxes->bodlist[i].num_bodies = k;
         edit->boxes->saved[i] = edit->calc_edits = 1;
      }
      fclose ( fptr );
   }
   MT_free ( (void *) file );

   edit->contours = (contour_struct *) MT_malloc ( sizeof ( contour_struct ) );
   edit->contours->num_contours = 0;
   for ( i = 0; i < MAX_CONTOURS; i++ )
      edit->contours->saved[i] = 0;

   for ( i = 0; i < MAX_CONTOURS; i++ ) {
      edit->contours->files[i] = (char *) MT_malloc ( MAX_ID * sizeof ( char ) );
      strcpy ( edit->contours->files[i], "" );
   }

   edit->contours->points = (double *) MT_malloc ( 3 * MAX_CONTOURS * sizeof ( double ) );
   edit->contours->vector1 = (double *) MT_malloc ( 3 * MAX_CONTOURS * sizeof ( double ) );
   edit->contours->vector2 = (double *) MT_malloc ( 3 * MAX_CONTOURS * sizeof ( double ) );

   for ( i = 0; i < 3*MAX_CONTOURS; i++ ) {
      edit->contours->points[i] = 0.0;
      edit->contours->vector1[i] = 0.0;
      edit->contours->vector2[i] = 0.0;
   }

   DEBUG_TRACE_OUT printf ("Done with set_up_edit_mem\n");

   return;

}




/******************************************************************************/

void edit_panel_mem_setup ( edit_panel_struct *edit )

{

   int  i;

   DEBUG_TRACE_IN printf ("Entering edit_panel_mem_setup\n");

   edit->point = (point_panel_struct *) MT_malloc ( sizeof ( point_panel_struct ) );
      edit->point->number = (inp_struct *) MT_malloc ( sizeof ( inp_struct ) );
      edit->point->number->form = NULL;
      edit->point->location = (point_inp *) MT_malloc ( MAX_POINTS * sizeof ( point_inp ) );
      for ( i = 0; i < MAX_POINTS; i++ )
         edit->point->location[i].form = NULL;
      edit->point->base = (base_struct *) MT_malloc ( sizeof ( base_struct ) );

   edit->line = (line_panel_struct *) MT_malloc ( sizeof ( line_panel_struct ) );
      edit->line->number = (inp_struct *) MT_malloc ( sizeof ( inp_struct ) );
      edit->line->number->form = NULL;
      edit->line->delta = (point_inp *) MT_malloc ( MAX_LINES * sizeof ( point_inp ) );
      edit->line->begin = (point_inp *) MT_malloc ( MAX_LINES * sizeof ( point_inp ) );
      edit->line->end = (point_inp *) MT_malloc ( MAX_LINES * sizeof ( point_inp ) );
      for ( i = 0; i < MAX_LINES; i++ ) {
         edit->line->delta[i].form = NULL;
         edit->line->begin[i].form = NULL;
         edit->line->end[i].form = NULL;
      }
      edit->line->base = (base_struct *) MT_malloc ( sizeof ( base_struct ) );

   edit->box = (box_panel_struct *) MT_malloc ( sizeof ( box_panel_struct ) );
      edit->box->number = (inp_struct *) MT_malloc ( sizeof ( inp_struct ) );
      edit->box->number->form = NULL;
      edit->box->bodies = (point_inp *) MT_malloc ( MAX_BOXES * sizeof ( point_inp ) );
      for ( i = 0; i < MAX_BOXES; i++ )
         edit->box->bodies[i].form = NULL;
      edit->box->base = (base_struct *) MT_malloc ( sizeof ( base_struct ) );

   edit->contour = (contour_panel_struct *) MT_malloc ( sizeof ( contour_panel_struct ) );
      edit->contour->number = (inp_struct *) MT_malloc ( sizeof ( inp_struct ) );
      edit->contour->number->form = NULL;
      edit->contour->file = (point_inp *) MT_malloc ( MAX_CONTOURS * sizeof ( point_inp ) );
      edit->contour->point = (point_inp *) MT_malloc ( MAX_CONTOURS * sizeof ( point_inp ) );
      edit->contour->vector1 = (point_inp *) MT_malloc ( MAX_CONTOURS * sizeof ( point_inp ) );
      edit->contour->vector2 = (point_inp *) MT_malloc ( MAX_CONTOURS * sizeof ( point_inp ) );
      for ( i = 0; i < MAX_CONTOURS; i++ ) {
         edit->contour->file[i].form = NULL;
         edit->contour->point[i].form = NULL;
         edit->contour->vector1[i].form = NULL;
         edit->contour->vector2[i].form = NULL;
      }
      edit->contour->base = (base_struct *) MT_malloc ( sizeof ( base_struct ) );

   DEBUG_TRACE_OUT printf ("Done with edit_panel_mem_setup\n");

   return;

}




/******************************************************************************/

void results_mem_setup ( edit_results_struct *results, int n_bin, int n_const, int num_regions,
                         int n_avg )

{

   int i, j, len, fields, size;

   len = MAX_XY * MAX_XY;
   fields = MAX_FRACTIONS * MAX_FIELDS;
   size = MAX_NNED * sizeof(double);

   results->ref_loc = (double *) MT_malloc ( 3 * sizeof ( double ) );
   results->ref_dose = (double *) MT_malloc ( size );

   results->ref_loc[0] = results->ref_loc[1] = results->ref_loc[2] = 0.0;
   for ( i=0; i < MAX_NNED; i++ ) {
      results->ref_dose[i] = 0.0;
   }

   results->points = (point_res *) MT_malloc ( (fields + n_const + MAX_POINTS) * sizeof(point_res) );
      for ( i=0; i < (fields + n_const + MAX_POINTS); i++ ) {
         results->points[i].data = (double *) MT_malloc ( size );
         for ( j=0; j < MAX_NNED; j++ ) {
            results->points[i].data[j] = 0.0;
         }
      }

   results->lines = (line_res *) MT_malloc ( fields * sizeof ( line_res ) );
      for ( i=0; i < (fields + MAX_LINES); i++ ) {
         results->lines[i].data = (double *) MT_malloc ( MAX_RESOLUTION * size );
         for ( j=0; j < MAX_RESOLUTION*MAX_NNED; j++ ) {
            results->lines[i].data[j] = 0.0;
         }
      }

   results->boxes = (box_res *) MT_malloc ( sizeof ( box_res ) );
      for ( i=0; i < (num_regions + MAX_BOXES); i++ ) {
         results->boxes->data[i] = (double *) MT_malloc ( size );
         results->boxes->dose_max[i] = (double *) MT_malloc ( size );
         results->boxes->max_loc[i] = (double *) MT_malloc ( 3 * sizeof (double) );
         results->boxes->dose_min[i] = (double *) MT_malloc ( size );
         results->boxes->min_loc[i] = (double *) MT_malloc ( 3 * sizeof (double) );
         results->boxes->dose_mean[i] = (double *) MT_malloc ( size );
         results->boxes->dose_ref[i] = (double *) MT_malloc ( size );
         results->boxes->dv_bins[i] = (float *) MT_malloc ( (n_bin+1) * MAX_NNED * sizeof (float) );
         results->boxes->dose_avg[i] = (double *) MT_malloc ( n_avg * sizeof (double) );

         for ( j=0; j < MAX_NNED; j++ ) {
            results->boxes->data[i][j] = 0.0;
            results->boxes->dose_max[i][j] = 0.0;
            results->boxes->dose_min[i][j] = 1.0e+20;
            results->boxes->dose_mean[i][j] = 0.0;
            results->boxes->dose_ref[i][j] = 0.0;
         }

         for ( j=0; j < 3; j++ ) {
            results->boxes->max_loc[i][j] = 0.0;
            results->boxes->min_loc[i][j] = 0.0;
         }

         for ( j=0; j < (n_bin+1)*MAX_NNED; j++ ) {
            results->boxes->dv_bins[i][j] = 0.0;
         }
      }

   results->surfaces = (surf_res *) MT_malloc ( sizeof ( surf_res ) );
      for ( i=0; i < MAX_CONTOURS; i++ ) {
         results->surfaces->data[i] = (double *) MT_malloc ( len * size );
         for ( j=0; j < len*MAX_NNED; j++ ) {
            results->surfaces->data[i][j] = 0.0;
         }
      }

   return;

}




/******************************************************************************/
