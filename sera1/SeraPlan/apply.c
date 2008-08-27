#define PATIENTMESSAGE "No patient ID has been specified - please do so!"
#define PATIENTMESSAGE2 "Patient ID contains embedded blanks - please modify."
#define DONEMESSAGE "The plan .rst files have been generated - Apply complete!"
#define FILEMESSAGE1 "The requested file, named\n"
#define FILEMESSAGE2 ",\ncould not be located.  Check path and/or filename."

#include "basic.h"
#include "manager.h"
#include "primitive.h"
#include "menudial.h"
#include <string.h>
#include "data.h"
#include "dose.h"
#include "editdata.h"
#include "check_files.h"
#include "panel.h"
#include "apply.h"
#include "debug_tools.h"
#include "memory_tools.h"
#include "dialog_tools.h"
#include "file_tools.h"
#include "pixmaps/xm_hour32"   /* hourglass bitmap */
#include "pixmaps/xm_hour32m"   /* hourglass bitmap mask */

#define ONE 1.0
#define ZERO 0.0

static int allRstFilesValid( Widget parent );


void ApplyCallback ( Widget w, Widget parent, XtPointer callData )

{

   static Widget  errdialog = NULL;
   static Widget  donedialog = NULL;
   static Widget  startdialog = NULL;

   Pixmap         hour, hmask;
   Cursor         cursor;
   XColor         black_def, white_def;

   char           file_name[MAX_FILE+MAX_ID];


   DEBUG_TRACE_IN printf("Entered ApplyCallback\n");

   /* Let the user pick the directory into which the files will be saved */
   if( !DT_select_file( parent, XtWidgetToApplicationContext( parent ),
                        data.save_dir, "Save Directory" ) )
   {
       DEBUG_TRACE_OUT printf("Leaving ApplyCallback, FSB was cancelled\n");
       return;
   }

   /*
    * OK, we've got the directory name, make sure that it's
    * actually a directory on the system.
    */
   if( !FT_isADirectory( data.save_dir ) )
   {
       DT_error( parent, "That is not a valid directory!", "Not A Directory", NULL );
       DEBUG_TRACE_OUT printf("Leaving ApplyCallback, directory not valid\n");
       return;
   }
   
/*
 *  Check to ensure that directory name ends with a / - needed to do simple concatenation
 *  to construct save filenames
 */

   if ( data.save_dir[strlen(data.save_dir)-1] != '/' )
      strcat ( data.save_dir, "/" );
      

/*
 *  Create the "wait" cursor pixmap and mask from the appropriate bitmaps
 */

   hour = XCreatePixmapFromBitmapData ( XtDisplay ( w ), RootWindowOfScreen ( XtScreen ( w ) ),
                                        (char *) hour32_bits, hour32_width, hour32_height, 1, 0, 1 );
   hmask = XCreatePixmapFromBitmapData ( XtDisplay ( w ), RootWindowOfScreen ( XtScreen ( w ) ),
                                         (char *) hour32m_bits, hour32m_width, hour32m_height, 1, 0, 1 );
   XParseColor ( XtDisplay ( w ), DefaultColormapOfScreen ( XtScreen ( w ) ), "black", &black_def );
   XParseColor ( XtDisplay ( w ), DefaultColormapOfScreen ( XtScreen ( w ) ), "white", &white_def );
   cursor = XCreatePixmapCursor ( XtDisplay ( w ), hour, hmask, &black_def, &white_def,
                                  hour32_x_hot, hour32_y_hot );
   XDefineCursor ( XtDisplay ( w ), XtWindow ( parent ), cursor );
   XFreePixmap ( XtDisplay ( w ), hour );
   XFreePixmap ( XtDisplay ( w ), hmask );

/*
 *  Now, make the popup shell insensitive, to prevent the user from getting
 *  impatient and mucking up the works
 */

   XtSetSensitive ( parent, False );

/*
 *  Check to see if a patient name is given - if not, issue error message and
 *  exit callback
 */

   if ( strlen( data.patient_ID ) == 0 ) {
      XtSetSensitive ( parent, True );
      error_dialog ( parent, errdialog, PATIENTMESSAGE );
      XUndefineCursor ( XtDisplay ( w ), XtWindow ( parent ) );
      return;
   }
/*
 *  Check for embedded blanks in patient ID - not allowed (using the patient ID
 *  to construct filenames)
 */
   else
      if ( strchr(data.patient_ID, ' ') ) {
         XtSetSensitive ( parent, True );
         error_dialog ( parent, errdialog, PATIENTMESSAGE2 );
         XUndefineCursor ( XtDisplay ( w ), XtWindow ( parent ) );
         return;
      }

/*
 *  If no embedded blanks, then create the plan file name
 */

      else {
         strcpy(file_name, data.save_dir );
         strcat(file_name, data.patient_ID );
         strcat(file_name, "plan" );
      }

   /*
    * Check that all the rst files are valid before
    * writing them to the plan file.
    */
   if( !allRstFilesValid( parent ) )
   {
       XtSetSensitive ( parent, True );
       XUndefineCursor ( XtDisplay ( w ), XtWindow ( parent ) );
       DEBUG_TRACE_OUT printf("Leaving ApplyCallback, rst files not valid\n");
       return;
   }
 
/*
 *  Write the data structure to the save file
 */

   if ( write_data ( parent, file_name ) ) {
      XtSetSensitive ( parent, True );
      XUndefineCursor ( XtDisplay ( w ), XtWindow ( parent ) );
      return;
   }

/*
 *  Combine the fields
 */

   if ( combine_fields ( TRUE, parent ) ) {
      error_dialog ( parent, errdialog, "Error - combination failed!" );
   }

/*
 *  Issue a dialog window announcing that the Apply has completed
 */

   XtSetSensitive ( parent, True );
   XUndefineCursor ( XtDisplay ( w ), XtWindow ( parent ) );
   message_dialog ( parent, donedialog, DONEMESSAGE );

   DEBUG_TRACE_OUT printf("Done with ApplyCallback");

}




/******************************************************************************/

int write_data ( Widget w, char *file_out )

{

   static Widget  errdialog;
   FILE           *plan_file;
   char           filechk[MAX_FILE+MAX_ID];
   int            i, j, err=0;

/*
 *  Open the new file (write only)
 */

   DEBUG_TRACE_IN printf("Entered write_data\n");

   plan_file = fopen ( file_out, "w" );
   if ( !plan_file ) {
      DEBUG_TRACE_OUT printf("Problem opening plan file\n");
      strcpy ( filechk, FILEMESSAGE1 );
      strcat ( filechk, file_out );
      strcat ( filechk, FILEMESSAGE2 );
      error_dialog ( w, errdialog, filechk );
      err = 1;
      return err;
   }

/*
 *  Write the entire contents of the plan_data structure to the file
 *  This is a text file - no binary writes
 *
 *  Start with general information (patient ID, plan, fractions, fields)
 */

   fprintf ( plan_file, "Patient: %s\n", data.patient_ID );
   fprintf ( plan_file, "Treatment date: %s\n", data.treat_date );
   fprintf ( plan_file, "FRACTIONS=%d FIELDS=%d\n", data.FRACTIONS, data.FIELDS );

/*
 *  Now, write the field data for each fraction, followed by the fraction sums
 */

   for ( i = 0; i < data.FRACTIONS; i++ ) {
      fprintf ( plan_file, "Fraction %d\n", i );
      for ( j = 0; j < data.FIELDS; j++ ) {
         fprintf ( plan_file, "Field %d\n", j );
         fprintf ( plan_file, "%s\n", data.field_file[i][j] );
         fprintf ( plan_file, "%g %g %g %d\n", data.field_B10[i][j], data.field_EXPOSURE[i][j],
                   data.field_GAMMA[i][j], data.field_ACTIVE[i][j] );
      }
      fprintf ( plan_file, "Fraction sums %g %g %g %g\n", data.fraction_BAVE[i],
                data.fraction_BEFF[i], data.fraction_EXPOSURE[i], data.fraction_WEIGHT[i] );
   }

/*
 *  Finish up with the plan sums
 */

   fprintf ( plan_file, "Plan sums %g %g %g\n", data.total_BAVE, data.total_BEFF,
             data.total_EXPOSURE );

/*
 *  Close file
 */

   fclose ( plan_file );
   DEBUG_TRACE_OUT ("Done with write_data\n");

   return err;

}




/******************************************************************************/

void ErrCallback ( Widget w, XtPointer clientData, XtPointer callData )

{

   DEBUG_TRACE_IN printf("Entering ErrCallback\n");

   XtUnmanageChild ( w );

   DEBUG_TRACE_OUT printf("Done with ErrCallback\n");

}




/******************************************************************************/

float sum_dose ( dose_struct *dose, dose_struct *dose_sum, float exposure,
                 float boron, float gamma, float gnorm, float nnorm, int nned )

/*
 *  Calculate the fraction dose components based on the boron concentrations and
 *  exposures specified in the input widget - sum over all fields in the fraction
 */

{

   int ix, jy, kz, ncom, addr, nxyz;
   float sgc=0.0, sgtot=0.0;

   DEBUG_TRACE_IN printf("Entering sum_dose\n");
   nxyz = *dose->nedit;

/*
 *  Construct the weighting for the gamma dose - have to weight each field by the
 *  gamma production sum for the field
 */

   DEBUG_TRACE_OUT printf("Starting gamma production sum\n");
   for ( addr = 0, ix = 0; ix < nxyz; ix++ ) {
      for ( jy = 0; jy < nxyz; jy++ ) {
         for ( kz = 0; kz < nxyz; addr += nned, kz++ ) {
            sgc += dose->bflux[addr+4];
         }
      }
   }
   sgtot = ( gnorm + sgc * nnorm ) * exposure;

   DEBUG_TRACE_OUT printf("Starting dose sum\n");
   for ( ix = 0; ix < addr; ix+=nned ) {
      for ( ncom = 0; ncom < nned; ncom++ ) {
/*
 *  All components (including fluences) are multiplied by the exposure
 *  Gamma dose is also multiplied by the gamma repair factor
 *  Boron dose is also multiplied by the boron concentration
 */
         if ( ncom == 0 )
            dose_sum->bflux[ix+ncom] += dose->bflux[ix+ncom] * sgtot * gamma;
         else if ( ncom == 3 )
            dose_sum->bflux[ix+ncom] += dose->bflux[ix+ncom] * nnorm * exposure * boron;
         else
            dose_sum->bflux[ix+ncom] += dose->bflux[ix+ncom] * nnorm * exposure;
      }
/*
 *  Set total dose equal to sum of the dose components
 */
      dose_sum->bflux[ix+2] = dose_sum->bflux[ix] + dose_sum->bflux[ix+1] +
                              dose_sum->bflux[ix+3] + dose_sum->bflux[ix+5] +
                              dose_sum->bflux[ix+9] + dose_sum->bflux[ix+11];
   }

   DEBUG_TRACE_OUT printf("Done with sum_dose\n");

   return ( sgtot );

}




/******************************************************************************/

int initialize_rst ( Widget w, dose_struct *dose, dose_struct *dose_sum, int frac, int field )

/*
 *  Writes the problem data from the field structure to the fraction structure
 *  but only for the first field - for all subsequent fields, check the information
 *  against that resident in the sum structure for consistency - inconsistency results
 *  in a fatal error, and no further summation.
 */

{

   int  error=0;
   int  i;

   DEBUG_TRACE_IN printf("Entering initialize_rst\n");

   if ( !dose_sum->nset ) {
      DEBUG_TRACE_IN printf("First pass through - writing information\n");
      dose_sum->nset += dose->nset;
      strcpy ( dose_sum->vers_stmp, dose->vers_stmp );
      strcpy ( dose_sum->title, dose->title );
      strcpy ( dose_sum->geomfile, dose->geomfile );
      strcpy ( dose_sum->bsg_file, dose->bsg_file );
      strcpy ( dose_sum->matfile, dose->matfile );
      strcpy ( dose_sum->sigmafile, dose->sigmafile );
      strcpy ( dose_sum->code_vers, dose->code_vers );
      *dose_sum->x0 = *dose->x0;
      *dose_sum->y0 = *dose->y0;
      *dose_sum->z0 = *dose->z0;
      *dose_sum->delw = *dose->delw;
      *dose_sum->nedit = *dose->nedit;
      *dose_sum->nedt = *dose->nedt;
      for ( i = 0; i < *dose->nedt; i++ )
         dose_sum->iged[i] = dose->iged[i];
   }

   else {
      DEBUG_TRACE_IN printf("Subsequent passes through - comparing information\n");
      dose_sum->nset += dose->nset;
      if ( strcmp (dose_sum->vers_stmp, dose->vers_stmp) ) {
         error = file_error ( w, field, frac, 7, dose_sum->vers_stmp, dose->vers_stmp );
         if ( error )
            return error;
      }
      if ( strcmp (dose_sum->geomfile, dose->geomfile) ) {
         error = file_error ( w, field, frac, 0, dose_sum->geomfile, dose->geomfile );
         if ( error )
            return error;
      }
      if ( strcmp (dose_sum->bsg_file, dose->bsg_file) ) {
         error = file_error ( w, field, frac, 1, dose_sum->bsg_file, dose->bsg_file );
         if ( error )
            return error;
      }
      if ( strcmp (dose_sum->matfile, dose->matfile) ) {
         error = file_error ( w, field, frac, 2, dose_sum->matfile, dose->matfile );
         if ( error )
            return error;
      }
      if ( strcmp (dose_sum->sigmafile, dose->sigmafile) ) {
         error = file_error ( w, field, frac, 3, dose_sum->sigmafile, dose->sigmafile );
         if ( error )
            return error;
      }
      if ( strcmp (dose_sum->code_vers, dose->code_vers) ) {
         error = file_error ( w, field, frac, 4, dose_sum->code_vers, dose->code_vers );
         if ( error )
            return error;
      }
      if ( (*dose_sum->x0 != *dose->x0) || (*dose_sum->y0 != *dose->y0) ||
           (*dose_sum->z0 != *dose->z0) || (*dose_sum->delw != *dose->delw) ||
           (*dose_sum->nedit != *dose->nedit) ) {
         error = file_error ( w, field, frac, 5, NULL, NULL );
         if ( error )
            return error;
      }
      if ( *dose_sum->nedt != *dose->nedt ) {
         error = file_error ( w, field, frac, 6, NULL, NULL );
         if ( error )
            return error;
      }
      for ( i = 0; i < *dose_sum->nedt; i++ ) {
         if ( dose_sum->iged[i] != dose->iged[i] ) {
            error = file_error ( w, field, frac, 6, NULL, NULL );
            if ( error )
               return error;
         }
      }
   }

   DEBUG_TRACE_IN printf("Done with initialize_rst\n");

   return error;

}




/******************************************************************************/

void set_rst ( dose_struct *dose, dose_struct *dose_sum, double weight )

/*
 *  Writes the data from the sets substructure of the field data to the fraction
 *  sum structure - fraction sum will hold all the set information from each field,
 *  and plan will have all data from each fraction (could get rather extensive, but
 *  such is life).
 */

{

   int  i, j, k;

   DEBUG_TRACE_IN printf("Entering set_rst\n");

   for ( i = 0, j = dose_sum->nset - dose->nset; i < dose->nset; i++, j++ ) {
      strcpy ( dose_sum->sets[j].run_dir, dose->sets[i].run_dir );
      strcpy ( dose_sum->sets[j].date, dose->sets[i].date );
      *dose_sum->sets[j].rel_wt = *dose->sets[i].rel_wt * weight;
      *dose_sum->sets[j].wn0 = *dose->sets[i].wn0;
      *dose_sum->sets[j].s_tot = *dose->sets[i].s_tot;
      *dose_sum->sets[j].nnhist = *dose->sets[i].nnhist;
      *dose_sum->sets[j].nghist = *dose->sets[i].nghist;
      *dose_sum->sets[j].nfhist = *dose->sets[i].nfhist;
      *dose_sum->sets[j].gamratio = *dose->sets[i].gamratio;
      strcpy ( dose_sum->sets[j].sourcefile, dose->sets[i].sourcefile );
      *dose_sum->sets[j].irand_num = *dose->sets[i].irand_num;

      *dose_sum->sets[j].xp = *dose->sets[i].xp;
      *dose_sum->sets[j].yp = *dose->sets[i].yp;
      *dose_sum->sets[j].zp = *dose->sets[i].zp;
      *dose_sum->sets[j].zb = *dose->sets[i].zb;
      *dose_sum->sets[j].phi = *dose->sets[i].phi;
      *dose_sum->sets[j].theta = *dose->sets[i].theta;
      strcpy ( dose_sum->sets[j].new_rst, dose->sets[i].new_rst );

      *dose_sum->sets[j].nbatch = *dose->sets[i].nbatch;
      *dose_sum->sets[j].nhist = *dose->sets[i].nhist;
      *dose_sum->sets[j].wncut = *dose->sets[i].wncut;
      *dose_sum->sets[j].id_b10 = *dose->sets[i].id_b10;
      *dose_sum->sets[j].id_h = *dose->sets[i].id_h;
      *dose_sum->sets[j].id_n = *dose->sets[i].id_n;
      *dose_sum->sets[j].id_c = *dose->sets[i].id_c;
      *dose_sum->sets[j].id_o = *dose->sets[i].id_o;
      *dose_sum->sets[j].id_rr1 = *dose->sets[i].id_rr1;
      *dose_sum->sets[j].id_rr2 = *dose->sets[i].id_rr2;
      *dose_sum->sets[j].b10_dens = *dose->sets[i].b10_dens;
      *dose_sum->sets[j].h_dens = *dose->sets[i].h_dens;
      *dose_sum->sets[j].n_dens = *dose->sets[i].n_dens;
      *dose_sum->sets[j].c_dens = *dose->sets[i].c_dens;
      *dose_sum->sets[j].o_dens = *dose->sets[i].o_dens;
      *dose_sum->sets[j].rr1_dens = *dose->sets[i].rr1_dens;
      *dose_sum->sets[j].rr2_dens = *dose->sets[i].rr2_dens;

      for ( k = 0; k < 3; k++ ) {
         dose_sum->sets[j].entry[k] = dose->sets[i].entry[k];
         dose_sum->sets[j].bvec[k] = dose->sets[i].bvec[k];
      }
   }

   DEBUG_TRACE_OUT printf("Done with set_rst\n");

   return;

}




/******************************************************************************/

int file_error ( Widget w, int field, int frac, int err_code, char *s1, char *s2 )

{

   static Widget   err_dialog=NULL;
   char            mess[500], *numb, middle[25];
   int             code;

/*
 *  Allocate storage for numb
 */

   DEBUG_TRACE_IN printf("Entering file_error\n");

   numb = ( char * ) MT_malloc ( sizeof ( char ) );

/*
 *  Build the middle of the message - differs for fraction sum or plan sum
 *  Fraction sum has individual fields, plan sum is sum of fractions
 */

   if ( field ) {
      strcpy ( middle, "field " );
      sprintf ( numb, "%d", field+1 );
      strcat ( middle, numb );
      strcat ( middle, " in fraction " );
   }
   else {
      strcpy ( middle, "fraction " );
   }
   sprintf ( numb, "%d", frac+1 );
   strcat ( middle, numb );

   switch ( err_code ) {
      case  0:
               strcpy ( mess, "The geometry file for " );
               strcat ( mess, middle );
               strcat ( mess, "\n" );
               strcat ( mess, s2 );
               strcat ( mess, "\ndoes not agree with those for the other .rst files,\n" );
               strcat ( mess, s1 );
               strcat ( mess, "\nShould I continue with the plan construction?" );
               code = warn_dialog ( w, err_dialog, mess );
               break;
      case  1:
               strcpy ( mess, "The CG geometry file for " );
               strcat ( mess, middle );
               strcat ( mess, "\n" );
               strcat ( mess, s2 );
               strcat ( mess, "\ndoes not agree with those for the other .rst files,\n" );
               strcat ( mess, s1 );
               strcat ( mess, "\nShould I continue with the plan construction?" );
               code = warn_dialog ( w, err_dialog, mess );
               break;
      case  2:
               strcpy ( mess, "The material file for " );
               strcat ( mess, middle );
               strcat ( mess, "\n" );
               strcat ( mess, s2 );
               strcat ( mess, "\ndoes not agree with those for the other .rst files,\n" );
               strcat ( mess, s1 );
               strcat ( mess, "\nShould I continue with the plan construction?" );
               code = warn_dialog ( w, err_dialog, mess );
               break;
      case  3:
               strcpy ( mess, "The cross section file for " );
               strcat ( mess, middle );
               strcat ( mess, "\n" );
               strcat ( mess, s2 );
               strcat ( mess, "\ndoes not agree with those for the other .rst files,\n" );
               strcat ( mess, s1 );
               strcat ( mess, "\nShould I continue with the plan construction?" );
               code = warn_dialog ( w, err_dialog, mess );
               break;
      case  4:
               strcpy ( mess, "The rtt_MC code version for " );
               strcat ( mess, middle );
               strcat ( mess, "\n" );
               strcat ( mess, s2 );
               strcat ( mess, "\ndoes not agree with those for the other .rst files,\n" );
               strcat ( mess, s1 );
               strcat ( mess, "\nShould I continue with the plan construction?" );
               code = warn_dialog ( w, err_dialog, mess );
               break;
      case  5:
               strcpy ( mess, "The edit geometry for " );
               strcat ( mess, middle );
               strcat ( mess, " does not agree\n with the edit geometry for the other " );
               strcat ( mess, ".rst files.\n Please check your input and try again." );
               error_dialog ( w, err_dialog, mess );
               code = 1;
               break;
      case  6:
               strcpy ( mess, "The energy group structure for " );
               strcat ( mess, middle );
               strcat ( mess, " does not agree\n with the energy group structure for the other " );
               strcat ( mess, ".rst files.\n Please check your input and try again." );
               error_dialog ( w, err_dialog, mess );
               code = 1;
               break;
      case  7:
               strcpy ( mess, "The code version stamp for " );
               strcat ( mess, middle );
               strcat ( mess, "\n" );
               strcat ( mess, s2 );
               strcat ( mess, "\ndoes not agree with those for the other .rst files,\n" );
               strcat ( mess, s1 );
               strcat ( mess, "\nShould I continue with the plan construction?" );
               code = warn_dialog ( w, err_dialog, mess );
               break;
   }

   DEBUG_TRACE_OUT printf("Done with file_error\n");

   return code;

}




/******************************************************************************/

void message_dialog ( Widget parent, Widget child, String MESSAGE )

{

   DEBUG_TRACE_IN printf("Entering message_dialog\n");

   if ( !child ) {
      child = XmCreateMessageDialog ( parent, "Apply", NULL, 0 );
      XtUnmanageChild ( XmMessageBoxGetChild ( child, XmDIALOG_CANCEL_BUTTON ) );
      XtUnmanageChild ( XmMessageBoxGetChild ( child, XmDIALOG_HELP_BUTTON ) );
      XtAddCallback ( child, XmNokCallback, (XtCallbackProc) ErrCallback, NULL );
   }
   XtVaSetValues ( child, XtVaTypedArg, XmNmessageString, XmRString,
                   MESSAGE, strlen ( MESSAGE )+1, NULL );
   XtManageChild ( child );

   DEBUG_TRACE_OUT printf("Done with message_dialog\n");

}




/******************************************************************************/

void error_dialog ( Widget parent, Widget child, String MESSAGE )

{

   DEBUG_TRACE_IN printf("Entering error_dialog\n");

   if ( !child ) {
      child = XmCreateErrorDialog ( parent, "Filename error", NULL, 0 );
      XtUnmanageChild ( XmMessageBoxGetChild ( child, XmDIALOG_CANCEL_BUTTON ) );
      XtUnmanageChild ( XmMessageBoxGetChild ( child, XmDIALOG_HELP_BUTTON ) );
      XtAddCallback ( child, XmNokCallback, (XtCallbackProc) ErrCallback, NULL );
   }
   XtVaSetValues ( child, XtVaTypedArg, XmNmessageString, XmRString,
                   MESSAGE, strlen ( MESSAGE )+1, NULL );
   XtManageChild ( child );

   DEBUG_TRACE_OUT printf("Done with error_dialog\n");

}




/******************************************************************************/

int warn_dialog ( Widget parent, Widget child, String MESSAGE )

{

   static int  code=0;

   DEBUG_TRACE_IN printf("Entering warn_dialog\n");

   if ( !child ) {
      child = XmCreateWarningDialog ( parent, "Filename warning", NULL, 0 );
      XtVaSetValues ( child, XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL, NULL );
      XtUnmanageChild ( XmMessageBoxGetChild ( child, XmDIALOG_HELP_BUTTON ) );
      XtAddCallback ( child, XmNokCallback, (XtCallbackProc) ErrCallback, NULL );
      XtAddCallback ( child, XmNcancelCallback, (XtCallbackProc) StopApplyCB, &code );
   }
   XtVaSetValues ( child, XtVaTypedArg, XmNmessageString, XmRString,
                   MESSAGE, strlen ( MESSAGE )+1, NULL );
   XtManageChild ( child );

/*
 *  Enter an event loop, which applies as long as the dialog exists, to
 *  make sure that the answer actually influences the code execution
 */

   while ( XtIsManaged (child) ) {
      XEvent event;
      XtAppNextEvent ( XtWidgetToApplicationContext ( child ), &event );
      XtDispatchEvent ( &event );
   }

   DEBUG_TRACE_OUT printf("Done with warn_dialog\n");

   return code;

}




/******************************************************************************/

void StopApplyCB ( Widget w, int *code, XtPointer callData )

{

   DEBUG_TRACE_IN printf("Entering StopApplyCB\n");

   XtUnmanageChild ( w );
   *code = 1;

   DEBUG_TRACE_OUT printf("Done StopApplyCB\n");

}




/******************************************************************************/

void norm_dose ( dose_struct *dose, float gfactor, float factor, int nned )

{

   int ix, jy, kz, ncom, addr, nxyz;

   DEBUG_TRACE_IN printf("Entering norm_gam_dose\n");

   nxyz = *dose->nedit;

   for ( addr = 0, ix = 0; ix < nxyz; ix++ ) {
      for ( jy = 0; jy < nxyz; jy++ ) {
         for ( kz = 0; kz < nxyz; addr += nned, kz++ ) {
            dose->bflux[addr] /= gfactor;
            for ( ncom = 1; ncom < nned; ncom++ ) {
               dose->bflux[addr+ncom] /= factor;
            }
         }
      }
   }

   DEBUG_TRACE_OUT printf("Done with norm_gam_dose\n");

}




/******************************************************************************/

void SaveDirSelectCallback ( Widget w, XtPointer clientData, XmFileSelectionBoxCallbackStruct *cbs )

{

   char  *buffer;

   DEBUG_TRACE_IN printf("Entering SaveDirSelectCallback\n");

   XtUnmanageChild ( w );
   XmStringGetLtoR ( cbs->value, XmFONTLIST_DEFAULT_TAG, &buffer );
   strcpy ( data.save_dir, buffer );

   XtFree( buffer );

   DEBUG_TRACE_OUT printf("Done with SaveDirSelectCallback\n");

}


/*
 * Go through all the rst files in all FIELDS and all FRACTIONS
 * Make sure that they exist and are valid. Display errors if
 * we find any. A return value of 1 indicates no errors, 0
 * indicates at least one of the files is invalid.
 */
static int allRstFilesValid( Widget parent )
{
    int i, j;
    unsigned int flags;
    char errorString [MAX_FIELDS*MAX_FRACTIONS*80+256];
    char error[256];

    int valid = 1;
    
    DEBUG_TRACE_IN printf("Entering allRstFilesValid\n");

    sprintf( errorString, "The combination process could not begin because\n" );
    strcat ( errorString, "the following files do not exist or are invalid.\n\n" );
    
    /*
     * Loop through all files, and check each one.
     * If we find something wrong, add the FRACTION
     * and FIELD numbers to the overall error string.
     */
    for( i = 0; i < data.FRACTIONS; i++ )
    {
        for( j = 0; j < data.FIELDS; j++ )
        {
            if( data.field_ACTIVE )
            {
                checkFilenamesInRstFile( data.field_file[i][j], &flags );
                if( flags )
                {
                    sprintf( error, "     FRACTION #%d  FIELD #%d\n", i+1, j+1 );
                    strcat ( errorString, error );
                    valid = 0;
                }
            }
        }
    }
    
    if( !valid )
    {
        DT_error( parent, errorString, "Invalid rst Files", NULL );
    }

    DEBUG_TRACE_OUT printf("Leaving allRstFilesValid\n");
    return( valid );
}




/******************************************************************************/

int combine_fields ( int isInteractive, Widget parent )

{

   dose_struct    *field_dose, *frac_dose, *plan_dose;

   FILE           *rst_file;
   char           file_name[MAX_FILE+MAX_ID], dog[7], filechk[MAX_FILE+MAX_ID];
   int            error, i, j, k, irr, plan_flag=0, frac_flag[MAX_FRACTIONS], bsize, nned;
   float          sgtot, sgfrac, sfrac, stot, gfrac, gnorm, nnorm;
   double         exposure;

   static Widget  errdialog = NULL;

/*
 *  Check to ensure that directory name ends with a / - needed to do simple concatenation
 *  to construct save filenames
 */

   if ( data.save_dir[strlen(data.save_dir)-1] != '/' )
      strcat ( data.save_dir, "/" );
      
/*
 *  Set up memory for the plan dose sum
 */
   plan_dose = (dose_struct *) MT_malloc ( sizeof ( dose_struct ) );
   set_up_dose_mem ( plan_dose, MAX_FRACTIONS * MAX_FIELDS );

/*
 *  Loop over all defined fractions, and set up the memory for the
 *  dose sum for each fraction
 */
   sgtot = ZERO;
   stot = ZERO;
   for ( i = 0; i < data.FRACTIONS; i++ ) {
      if ( data.FRACTIONS > 1 ) {
         frac_dose = (dose_struct *) MT_malloc ( sizeof ( dose_struct ) );
         set_up_dose_mem ( frac_dose, MAX_FIELDS );
         frac_flag[i] = 0;
      }

/*
 *  Loop through all fields, set up the memory for the field dose data,
 *  open the dose file, read and store the data in the field_dose structure
 */
      sgfrac = sfrac = ZERO;
      gfrac = ZERO;
      for ( j = 0; j < data.FIELDS; j++ ) {
         if ( data.field_ACTIVE ) {
            field_dose = (dose_struct *) MT_malloc ( sizeof ( dose_struct ) );
            set_up_dose_mem ( field_dose, (int) ONE );
            rst_file = fopen ( data.field_file[i][j], "r" );
            DEBUG_TRACE_OUT printf("Opening .rst file - OK\n");
            if ( !rst_file ) {
               DEBUG_TRACE_OUT printf("Opening .rst file - not present\n");
               if ( isInteractive ) {
                  XtSetSensitive ( parent, True );
                  strcpy ( filechk, FILEMESSAGE1 );
                  strcat ( filechk, data.field_file[i][j] );
                  strcat ( filechk, FILEMESSAGE2 );
                  error_dialog ( parent, errdialog, filechk );
                  XUndefineCursor ( XtDisplay ( parent ), XtWindow ( parent ) );
               }
               else {
                  printf ( "%s %s %s\n", FILEMESSAGE1, data.field_file[i][j], FILEMESSAGE2 );
               }
               return ( 1 );
            }
            read_rst_file ( field_dose, rst_file, &irr );
            fclose ( rst_file );
/*
 *  Allocate storage for bflux array in plan and fraction dose structures
 *  Need to do this only for these because new memory management scheme
 *  normally does this (as for field dose) in read_rst_file
 */
            nned = ( irr ? MAX_NNED : MAX_NNED-2 );
            bsize = *field_dose->nedit * *field_dose->nedit * *field_dose->nedit * nned;
            if ( !plan_flag ) {
               plan_dose->bflux = (float *) MT_malloc ( bsize * sizeof(float) );
               plan_flag = 1;
            }
            if ( !frac_flag[i] ) {
               frac_dose->bflux = (float *) MT_malloc ( bsize * sizeof(float) );
               frac_flag[i] = 1;
            }

            if ( data.FRACTIONS > 1 ) {
               error = initialize_rst ( parent, field_dose, frac_dose, i, j );
               DEBUG_TRACE_OUT printf("Multiple fractions case\n");
            }
            else {
               error = initialize_rst ( parent, field_dose, plan_dose, i, j );
               DEBUG_TRACE_OUT printf("Single fraction case\n");
            }

/*
 *  In case of error, free allocated memory, and return
 */

            if ( error ) {
               DEBUG_TRACE_OUT printf("Error in initialize_rst\n");
               XtSetSensitive ( parent, True );
               MT_free ( (void *)field_dose->bflux );
               MT_free ( (void *)field_dose );
               if ( data.FRACTIONS > 1 ) {
                  MT_free ( (void *)frac_dose->bflux );
                  MT_free ( (void *)frac_dose );
               }
               MT_free ( (void *)plan_dose->bflux );
               MT_free ( (void *)plan_dose );
               if ( isInteractive )
                  XUndefineCursor ( XtDisplay ( parent ), XtWindow ( parent ) );
               return ( 1 );
            }

            gnorm = *field_dose->sets[0].s_tot * *field_dose->sets[0].gamratio;
            nnorm = *field_dose->sets[0].s_tot * ( 1.0 - *field_dose->sets[0].gamratio );
            if ( data.FRACTIONS > 1 ) {
               DEBUG_TRACE_OUT printf("Multiple fractions case - combining fields\n");
               exposure = data.field_EXPOSURE[i][j]/data.fraction_EXPOSURE[i];
               set_rst ( field_dose, frac_dose, exposure );
               sgfrac += sum_dose ( field_dose, frac_dose, exposure,
                                    data.field_B10[i][j]/data.fraction_BAVE[i],
                                    data.field_GAMMA[i][j], gnorm, nnorm, nned );
               sfrac += nnorm * exposure;
            }
            else {
               DEBUG_TRACE_OUT printf("Single fraction case - combining fields\n");
               exposure = data.field_EXPOSURE[i][j]/data.total_EXPOSURE;
               set_rst ( field_dose, plan_dose, exposure );
               sgtot += sum_dose ( field_dose, plan_dose, exposure,
                                   data.field_B10[i][j]/data.total_BAVE,
                                   data.field_GAMMA[i][j], gnorm, nnorm, nned );
               stot += nnorm * exposure;
            }
            gfrac += gnorm * exposure;

         }  /* if data.field_ACTIVE */

         MT_free ( (void *)field_dose );
         DEBUG_TRACE_OUT printf("Done with field_dose structure\n");

      }  /* for j */

/*
 *  Construct the file name and write the fraction sum data to a .frac.rst file
 *  Only perform this section if there is more than one fraction (otherwise, it
 *  is the same as the plan dose)
 */

      if ( data.FRACTIONS > 1 ) {
         DEBUG_TRACE_OUT printf("Multiple fractions case - normalization\n");
         strncpy ( file_name, data.save_dir, MAX_FILE+MAX_ID );
         strcat ( file_name, data.patient_ID );
         strcat ( file_name, "plan" );
         strcpy( frac_dose->plan_file_name, file_name );
         sprintf ( dog, ".frac%d", i+1 );
         strcat ( file_name, dog );
         strcat ( file_name, ".rst" );
         rst_file = fopen ( file_name, "w" );
         if ( !rst_file ) {
            if ( isInteractive ) {
               XtSetSensitive ( parent, True );
               strcpy ( filechk, FILEMESSAGE1 );
               strcat ( filechk, file_name );
               strcat ( filechk, FILEMESSAGE2 );
               error_dialog ( parent, errdialog, file_name );
               XUndefineCursor ( XtDisplay ( parent ), XtWindow ( parent ) );
            }
            else {
               printf ( "%s %s %s\n", FILEMESSAGE1, file_name, FILEMESSAGE2 );
            }
            return ( 1 );
         }
         norm_dose ( frac_dose, sgfrac, sfrac, nned );
         write_rst_file ( frac_dose, rst_file, irr );
         fclose ( rst_file );
      }

/*
 *  Only sum fractions if # of fractions is greater than 1
 */

      if ( data.FRACTIONS > 1 ) {
         DEBUG_TRACE_OUT printf("Multiple fractions case - summing fractions\n");
         error = initialize_rst ( parent, frac_dose, plan_dose, i, (int) ZERO );
/*
 *  In case of error, free allocated memory, and return
 */
         if ( error ) {
            DEBUG_TRACE_OUT printf("Error initializing fractions summation\n");
            XtSetSensitive ( parent, True );
            if ( data.FRACTIONS > 1 ) {
               MT_free ( (void *)frac_dose->bflux );
               MT_free ( (void *)frac_dose );
            }
            MT_free ( (void *)plan_dose->bflux );
            MT_free ( (void *)plan_dose );
            if ( isInteractive )
               XUndefineCursor ( XtDisplay ( parent ), XtWindow ( parent ) );
            return ( 1 );
         }
         set_rst ( frac_dose, plan_dose, (double) data.fraction_WEIGHT[i] );
         sgtot += sum_dose ( frac_dose, plan_dose, data.fraction_WEIGHT[i],
                             data.fraction_BAVE[i]/data.total_BAVE, 1.0, gfrac, sfrac, nned );
         stot += sfrac * data.fraction_WEIGHT[i];
      }

/*
 *  Free the memory allocated for the fraction dose
 */

      if ( data.FRACTIONS > 1 ) {
         MT_free ( (void *)frac_dose->bflux );
         MT_free ( (void *)frac_dose );
         DEBUG_TRACE_OUT printf("Done with frac_dose structure\n");
      }

   }  /* for i */

/*
 *  Open plan file and write data
 */

   strncpy ( file_name, data.save_dir, MAX_FILE+MAX_ID );
   strcat ( file_name, data.patient_ID );
   strcat ( file_name, "plan" );
   strcpy( plan_dose->plan_file_name, file_name );
   strcat ( file_name, ".rst" );
   rst_file = fopen ( file_name, "w" );
   if ( !rst_file ) {
      DEBUG_TRACE_OUT printf("Error opening plan file\n");
      if ( isInteractive ) {
         XtSetSensitive ( parent, True );
         strcpy ( filechk, FILEMESSAGE1 );
         strcat ( filechk, file_name );
         strcat ( filechk, FILEMESSAGE2 );
         error_dialog ( parent, errdialog, file_name );
         XUndefineCursor ( XtDisplay ( parent ), XtWindow ( parent ) );
      }
      else {
         printf ( "%s %s %s\n", FILEMESSAGE1, file_name, FILEMESSAGE2 );
      }
      return ( 1 );
   }
   norm_dose ( plan_dose, sgtot, stot, nned );
   write_rst_file ( plan_dose, rst_file, irr );
   fclose ( rst_file );

/*
 *  Free the allocated memory for the plan dose data
 */

   MT_free ( (void *)plan_dose->bflux );
   MT_free ( (void *)plan_dose );
   DEBUG_TRACE_OUT printf("Done with plan_dose structure\n");

   return ( 0 );

}




/******************************************************************************/
