#include "basic.h"
#include "primitive.h"
#include <string.h>
#include "data.h"
#include "dose.h"
#include "editdata.h"
#include "panel.h"
#include "editpanel.h"
#include "results.h"
#include "check_files.h"
#include "libuv.h"
#include "editcalc.h"
#include "debug_tools.h"
#include "dialog_tools.h"
#include "keyval_tools.h"
#include "memory_tools.h"
#include "dimensions.h"
#include "file_rw.h"
#include "libsz.h"
#include "file_tools.h"
#include "pixmaps/xm_hour32"   /* hourglass bitmap */
#include "pixmaps/xm_hour32m"   /* hourglass bitmap mask */

#define MIN( a, b ) ( ((a) < (b)) ? (a) : (b) )
#define MAX( a, b ) ( ((a) > (b)) ? (a) : (b) )

#define ZERO           0
#define ONE            1

#define REG_RBE        0
#define REF_RBE        1
#define CONSTRAINT_RBE 2
#define WIDGET_RBE     3
#define OTHER_RBE      4

#define NO_PRINT      -1
#define NO_HEADER      0
#define ADD_HEADER     1
#define IS_POINT_EDIT  2

#define PI 3.14159265

static int num_regions, reg_uvval[MAX_REGIONS], nned, oflag;
static char *other[2] = { "Other", "Ultra" };

static dose_struct  *edit_dose;
static geom_info_t  *uvhdata;

static FILE   *ofile, *dvfile, *linefile, *cdffile, *cmffile, *chdfile; 
static Widget  status;

extern char *ref_type[3];


void CalcEdits ( Widget w, Widget parent, XtPointer callData )

{
    Arg     al[2];

    Pixmap  hour, hmask;
    Cursor  cursor;
    XColor  black_def, white_def;

    DEBUG_TRACE_IN printf("Entering CalcEdits\n");
    
    /* Get a directory name from a file selection box */
    if ( !DT_select_file( seraplan, app, edit_data.save_dir, "Save Directory Selection" ) )
    {
       DEBUG_TRACE_OUT printf("Leaving CalcEdits, FSB was cancelled\n");
       return;
    }

    /* Make sure that we got a directory that is currently on the system */
    if ( FT_isADirectory( edit_data.save_dir ) == 0 )
    {
       DT_error( seraplan, "You must select an existing directory!", "Not A Directory", NULL );
       DEBUG_TRACE_OUT printf("Leaving CalcEdits, valid directory not selected");
       return;
    }
    
/*
 *  Check to ensure that directory name ends with a / - needed to do simple concatenation
 *  to construct save filenames
 */

    if ( edit_data.save_dir[strlen(edit_data.save_dir)-1] != '/' ) {
       strcat ( edit_data.save_dir, "/" );
    }

/*
 *  Save information from the edit panel
 */

    if ( !store_edit_panel_data ( w ) ) {
       DEBUG_TRACE_OUT printf("Leaving CalcEdits, valid directory not selected");
       return;
    }

/*
 *  Create the "wait" cursor pixmap and mask from the appropriate bitmaps
 */

    hour = XCreatePixmapFromBitmapData ( XtDisplay ( w ), RootWindowOfScreen ( XtScreen ( w ) ),
                                         (char *) hour32_bits, hour32_width, hour32_height, 1, 0, 1 );
    hmask = XCreatePixmapFromBitmapData ( XtDisplay ( w ), RootWindowOfScreen ( XtScreen ( w ) ),
                                          (char *) hour32m_bits, hour32m_width, hour32m_height, 1, 0, 1
                                          );
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
 *  Create and popup the edit status window
 */

    XtSetArg ( al[0], XmNmessageString, XmStringCreateLocalized("Starting edits - reading files") );
    status = XmCreateWorkingDialog ( parent, "Status", al, 1 );
    XtUnmanageChild ( XmMessageBoxGetChild (status, XmDIALOG_HELP_BUTTON) );
    XtUnmanageChild ( XmMessageBoxGetChild (status, XmDIALOG_CANCEL_BUTTON) );
    XtUnmanageChild ( XmMessageBoxGetChild (status, XmDIALOG_OK_BUTTON) );

    XtManageChild ( status );
    PostStatusMessage ( "Starting edits - reading files" );

/*
 *  Perform the edits
 */

    perform_edits ( TRUE, parent );

/*
 *  Display message signifying completion of edit process
 */

    PostStatusMessage ( "Edits complete!" );
    XtManageChild ( XmMessageBoxGetChild (status, XmDIALOG_OK_BUTTON) );

/*
 *  Return control of widget to user
 */

    XtSetSensitive ( parent, True );
    XUndefineCursor ( XtDisplay ( w ), XtWindow ( parent ) );

    DEBUG_TRACE_OUT printf("Done with CalcEdits\n");

}




/******************************************************************************/

void standard_edits ( double gnorm, double nnorm )

{

    char   fname[MAX_FILE], chdname[MAX_FILE], cdfname[MAX_FILE], cmfname[MAX_FILE];
    double  pt[3], vec1[3], vec2[3], *rbe, *pval, boron, delta, xold, yold, zold;
    int     i, ii, j, ihead, reg, uvval;

    marker_type *dog;

    DEBUG_TRACE_IN printf("Entering standard_edits\n");
    
    PostStatusMessage ( "Performing standard edits" );

/*
 *  Point edits at all target points
 */

    fprintf ( ofile, "\n\n\n\n     **************************************************    \n\n" );
    fprintf ( ofile, "Point edits for all target points defined in the .rst file\n\n" );
    ii = i = 0;
    xold = yold = zold = 1.0e+20;
    while ( ii < data.FIELDS * data.FRACTIONS ) {
       if ( *edit_dose->sets[i].xp == xold && *edit_dose->sets[i].yp == yold &&
            *edit_dose->sets[i].zp == zold ) {
          i++;
       }
       else {
          xold = *edit_dose->sets[i].xp;
          yold = *edit_dose->sets[i].yp;
          zold = *edit_dose->sets[i].zp;
          uvval = locate_regionnum ( xold, yold, zold );
          reg = uvhdata->regionnum[uvval];
          rbe = assemble_rbe ( REG_RBE, uvval );
          boron = uvhdata->dose[uvval].tissue_to_blood * edit_data.blood_b10;
          pval = point_dose ( xold, yold, zold, gnorm, nnorm, reg, rbe, IS_POINT_EDIT,
                              boron, ofile );
          if ( pval ) {
             for ( j = 0; j < nned; j++ ) {
                results->points[ii].data[j] = pval[j];
             }
          }
          ii++, i++;
       }  /* else */
    }  /* while i */

/*
 *  Point edits for all constraint markers
 */

    fprintf ( ofile, "\n\n\n\n     **************************************************    \n\n" );
    fprintf ( ofile, "Point edits for all defined constraint markers\n\n" );
    i = data.FIELDS * data.FRACTIONS;
    for ( dog = uvhdata->constraint_markers; dog; dog = dog->next ) {
       if ( dog->marker_kind == CONSTRAINT ) {
          rbe = assemble_rbe ( CONSTRAINT_RBE, i - (data.FIELDS * data.FRACTIONS) );
          uvval = locate_regionnum ( dog->wcf_x, dog->wcf_y, dog->wcf_z );
          reg = uvhdata->regionnum[uvval];
          boron = dog->dose.tissue_to_blood * edit_data.blood_b10;
          fprintf ( ofile, "\n\nEdit for constraint marker %s\n\n", dog->name );
          pval = point_dose ( dog->wcf_x, dog->wcf_y, dog->wcf_z, gnorm, nnorm, reg, rbe,
                              ADD_HEADER, boron, ofile );
          if ( pval ) {
             for ( j = 0; j < nned; j++ ) {
                results->points[i].data[j] = pval[j];
             }
          }
          i++;
       }
    }
    fclose ( ofile );

/*
 *  Line edits for all beam lines
 */

    delta = 0.25;
    strcpy ( fname, edit_data.save_dir );
    strcat ( fname, edit_data.plan_name );
    strcat ( fname, ".lin" );
    linefile = fopen ( fname, "w" );
    ii = i = 0;
    vec1[0] = vec1[1] = vec1[2] = 1.0e+20;
    while ( ii < data.FIELDS * data.FRACTIONS ) {
       if ( edit_dose->sets[i].entry[3] == vec1[0] && edit_dose->sets[i].entry[4] == vec1[1] &&
            edit_dose->sets[i].entry[5] == vec1[2] ) {
          i++;
       }
       else {
          vec1[0] = edit_dose->sets[i].entry[3];
          vec1[1] = edit_dose->sets[i].entry[4];
          vec1[2] = edit_dose->sets[i].entry[5];
          fprintf ( linefile, "Title: Beamline edit #%d for %s\n", ii+1, edit_data.patient_name );
          line_edit ( results->lines[i].data, edit_dose->sets[i].entry, vec1, delta, gnorm,
                      nnorm, linefile );
          ii++, i++;
       }
    }
    fclose ( linefile );

/*
 *  Dose-volume histograms for all defined bodies
 */
    strcpy ( fname, edit_data.save_dir );
    strcat ( fname, edit_data.plan_name );
    strcat ( fname, ".dvh" );
    dvfile = fopen ( fname, "w" );
    DV_histogram ( gnorm, nnorm, dvfile );
    fclose ( dvfile );
    
/*
 *  Contours for all defined planes
 */
    PostStatusMessage ( "Performing standard contour edits" );
    pt[0] = pt[1] = 0.0;
    pt[2] = uvhdata->isaxismin + 0.5*uvhdata->pixelsizeslices;
    vec2[0] = vec2[2] = vec1[1] = vec1[2] = 0.0;
    vec1[0] = vec2[1] = 1.0;
    if ( !strcmp ( uvhdata->sliceorientation, "Sagittal" ) ) {
       vec2[0] = vec2[1] = vec1[1] = vec1[2] = 0.0;
       vec1[0] = vec2[2] = 1.0;
    }
    else if ( !strcmp ( uvhdata->sliceorientation, "Coronal" ) ) {
       vec2[0] = vec2[2] = vec1[0] = vec1[2] = 0.0;
       vec1[1] = vec2[1] = 1.0;
    }
    rbe = assemble_rbe ( OTHER_RBE, 0 );

    strcpy ( chdname, edit_data.save_dir );
    strcat ( chdname, edit_data.plan_name );
    strcat ( chdname, ".chd" );
    chdfile = fopen ( chdname, "w" );

    strcpy ( cdfname, edit_data.save_dir );
    strcat ( cdfname, edit_data.plan_name );
    strcat ( cdfname, ".cdf" );
    cdffile = fopen ( cdfname, "w" );

    strcpy ( cmfname, edit_data.save_dir );
    strcat ( cmfname, edit_data.plan_name );
    strcat ( cmfname, ".cmf" );
    cmffile = fopen ( cmfname, "w" );

    if ( chdfile && cdffile && cmffile ) {
       ihead = ADD_HEADER;
       for ( i = 0; i < uvhdata->imageslices; i++ ) {
          create_chd ( i, uvhdata->imageslices, pt[2], rbe, ihead, chdfile );
          surface_edit ( pt, vec1, vec2, rbe, gnorm, nnorm, cdffile );
          create_mask ( pt, vec2, vec1, cmffile );
          ihead = NO_HEADER;
          pt[2] += uvhdata->pixelsizeslices;
       }

       fclose ( chdfile );

/*
 *  Close and seraZip .cdf and .cmf files
 */

       fclose ( cdffile );
       strcpy ( fname, cdfname );
       strcat ( fname, ".sz" );
       SZ_ZipFile ( cdfname, fname, ZERO, ZERO ); 

       fclose ( cmffile );
       strcpy ( fname, cmfname );
       strcat ( fname, ".sz" );
       SZ_ZipFile ( cmfname, fname, ZERO, ZERO ); 
    }
    else {
       printf("In standard_edits, chdfile, cdffile and cmffile not opened for writing!\n");
    }
    
    DEBUG_TRACE_OUT printf("Done with standard_edits\n");

    return;

}




/******************************************************************************/

void other_edits ( double gnorm, double nnorm )

/*
 *  Perform the user-defined edits
 */

{

    char   fname[MAX_FILE], chdname[MAX_FILE], cdfname[MAX_FILE], cmfname[MAX_FILE];
    int    i, j, k, ii, jj, kk, num, reg, uvval;
    int    imin, imax, jmin, jmax, kmin, kmax;
    double boron, *pval, *rbe, vec[3], vec2[3];

    marker_type *dog;

    DEBUG_TRACE_IN printf("Entering other_edits\n");

    PostStatusMessage ( "Performing user-defined edits" );

    num = data.FIELDS * data.FRACTIONS;

/*
 *  Perform point edits for all constraint markers, if haven't already
 */

    if ( !edit_data.perf_edits ) {
       strcpy ( fname, edit_data.save_dir );
       strcat ( fname, edit_data.plan_name );
       strcat ( fname, ".edit" );
       ofile = fopen ( fname, "a+" );
       fprintf ( ofile, "\n\n\n\n     **************************************************    \n\n" );
       fprintf ( ofile, "Point edits for all defined constraint markers\n\n" );
       i = data.FIELDS * data.FRACTIONS;
       for ( dog = uvhdata->constraint_markers; dog; dog = dog->next ) {
          if ( dog->marker_kind == CONSTRAINT ) {
             rbe = assemble_rbe ( CONSTRAINT_RBE, i - (data.FIELDS * data.FRACTIONS) );
             uvval = locate_regionnum ( dog->wcf_x, dog->wcf_y, dog->wcf_z );
             reg = uvhdata->regionnum[uvval];
             boron = dog->dose.tissue_to_blood * edit_data.blood_b10;
             fprintf ( ofile, "\n\nEdit for constraint marker %s\n\n", dog->name );
             pval = point_dose ( dog->wcf_x, dog->wcf_y, dog->wcf_z, gnorm, nnorm, reg, rbe,
                                 ADD_HEADER, boron, ofile );
             if ( pval ) {
                for ( j = 0; j < nned; j++ ) {
                   results->points[i].data[j] = pval[j];
                }
             }
             i++;
          }  /* if marker */
       }  /* for dog */
    }  /* if perf_edits */
    
/*
 *  Start with point edits,
 */

    if ( edit_data.points->num_points ) {
        strcpy ( fname, edit_data.save_dir );
        strcat ( fname, edit_data.plan_name );
        strcat ( fname, ".edit" );
        ofile = fopen ( fname, "a+" );
        for ( i = 0, j = 0; i < edit_data.points->num_points; i++, j+=3 ) {
            uvval = locate_regionnum ( edit_data.points->points[j], edit_data.points->points[j+1],
                                       edit_data.points->points[j+2] );
            reg = uvhdata->regionnum[uvval];
            rbe = assemble_rbe ( REG_RBE, uvval );
            boron = uvhdata->dose[uvval].tissue_to_blood * edit_data.blood_b10;
            pval = point_dose ( edit_data.points->points[j], edit_data.points->points[j+1],
                                edit_data.points->points[j+2], gnorm, nnorm, reg, rbe, IS_POINT_EDIT,
                                boron, ofile );
            if ( pval ) {
                for ( k = 0; k < nned; k++ ) {
                    results->points[num+i].data[k] = pval[k];
                }
            }
        }
        fclose ( ofile );
    }  /* if num_points */

/*
 *  then line edits (need to calculate unit vector from start to end),
 */

    if ( edit_data.lines->num_lines ) {
        strcpy ( fname, edit_data.save_dir );
        strcat ( fname, edit_data.plan_name );
        strcat ( fname, ".lin" );
        linefile = fopen ( fname, "a+" );
        for ( i = 0, j = 0; i < edit_data.lines->num_lines; i++, j+=3 ) {
            fprintf ( linefile, "Title: User-defined line edit #%d for %s\n", i+1, edit_data.patient_name );
            line_edit ( results->lines[num+i].data, &edit_data.lines->line_starts[j],
                        &edit_data.lines->line_ends[j], edit_data.lines->delta[i], gnorm,
                        nnorm, linefile );
        }
        fclose ( linefile );
    }  /* if num_lines */

/*
 *  and box edits (vec contains minima, vec2 contains maxima),
 */

    if ( edit_data.boxes->num_boxes ) {
        strcpy ( fname, edit_data.save_dir );
        strcat ( fname, edit_data.plan_name );
        strcat ( fname, ".dvh" );
        if ( !fopen ( fname, "r" ) ) {
            dvfile = fopen ( fname, "w" );
            fprintf ( dvfile, "DV histogram edits for patient %s\n", edit_data.patient_name );
            fprintf ( dvfile, "Number of bins in edits = %d\n\n", edit_data.n_bin );
            fclose ( dvfile );
        }
        dvfile = fopen ( fname, "a+" );
        for ( i = 0; i < edit_data.boxes->num_boxes; i++ ) {
            vec[0] = vec[1] = vec[2] = 1.0e+20;
            vec2[0] = vec2[1] = vec2[2] = 0.0;
            for ( j = 1; j <= num_regions; j++ ) {
                if ( is_in_body_list (reg_uvval[j], &edit_data.boxes->bodlist[i]) ) {
                    vec[0] = MIN ( vec[0], uvhdata->bboxpaaxismin[reg_uvval[j]] );
                    vec[1] = MIN ( vec[1], uvhdata->bboxrlaxismin[reg_uvval[j]] );
                    vec[2] = MIN ( vec[2], uvhdata->bboxisaxismin[reg_uvval[j]] );
                    vec2[0] = MAX ( vec2[0], uvhdata->bboxpaaxismax[reg_uvval[j]] );
                    vec2[1] = MAX ( vec2[1], uvhdata->bboxrlaxismax[reg_uvval[j]] );
                    vec2[2] = MAX ( vec2[2], uvhdata->bboxisaxismax[reg_uvval[j]] );
/*
 *  This makes certain that the starting point is in the center of a univel,
 *  which assures that the volumes for sums of regions will equal the sums of
 *  the volumes of the individual regions
 */
                    ii = (vec[0] - uvhdata->rlaxismin)*uvhdata->inv_pixelsizecolumns;
                    jj = (vec[1] - uvhdata->paaxismin)*uvhdata->inv_pixelsizerows;
                    kk = (vec[2] - uvhdata->isaxismin)*uvhdata->inv_pixelsizeslices;
                    vec[0] = uvhdata->rlaxismin + ((double)ii + 0.5)*uvhdata->pixelsizecolumns;
                    vec[1] = uvhdata->paaxismin + ((double)jj + 0.5)*uvhdata->pixelsizerows;
                    vec[2] = uvhdata->isaxismin + ((double)kk + 0.5)*uvhdata->pixelsizeslices;
                }
            }
            box_edit ( num_regions+i+1, vec[0], vec2[0], vec[1], vec2[1], vec[2], vec2[2], gnorm,
                       nnorm, &edit_data.boxes->bodlist[i], dvfile );
            if ( edit_data.n_avg > 1 ) {
                compute_n_avg ( num_regions+i+1, gnorm, nnorm, &edit_data.boxes->bodlist[i], dvfile );
            }
            fprintf ( dvfile, "\n\n\n\n     **************************************************\n\n" );
        }
        fclose ( dvfile );
    }  /* if num_boxes */

/*
 *  and, finally, contour edits (create and open files for each contour)
 */

    PostStatusMessage ( "Performing user-defined contour edits" );

    rbe = assemble_rbe ( OTHER_RBE, 0 );
    for ( i = 0, j = 0; i < edit_data.contours->num_contours; i++, j+=3 ) {
        strcpy ( chdname, edit_data.save_dir );
        strcat ( chdname, edit_data.contours->files[i] );
        strcat ( chdname, ".chd" );
        chdfile = fopen ( chdname, "w" );

        strcpy ( cmfname, edit_data.save_dir );
        strcat ( cmfname, edit_data.contours->files[i] );
        strcat ( cmfname, ".cmf" );
        cmffile = fopen ( cmfname, "w" );

        strcpy ( cdfname, edit_data.save_dir );
        strcat ( cdfname, edit_data.contours->files[i] );
        strcat ( cdfname, ".cdf" );
        cdffile = fopen ( cdfname, "w" );

        create_chd ( ZERO, ONE, edit_data.contours->points[j+2], rbe, ADD_HEADER, chdfile );
        surface_edit ( &edit_data.contours->points[j], &edit_data.contours->vector1[j],
                       &edit_data.contours->vector2[j], rbe, gnorm, nnorm, cdffile );
        create_mask ( &edit_data.contours->points[j], &edit_data.contours->vector2[j],
                      &edit_data.contours->vector1[j], cmffile );

        fclose ( chdfile );

/*
 *  Close and seraZip .cdf and .cmf files
 */

        fclose ( cdffile );
        strcpy ( fname, cdfname );
        strcat ( fname, ".sz" );
        SZ_ZipFile ( cdfname, fname, ZERO, ZERO );

        fclose ( cmffile );
        strcpy ( fname, cmfname );
        strcat ( fname, ".sz" );
        SZ_ZipFile ( cmfname, fname, ZERO, ZERO );
    }

    DEBUG_TRACE_OUT printf("Done with other_edits\n");

    return;

}




/******************************************************************************/

void line_edit ( double *line, double *start, double *end, double delta, double gnorm,
                 double nnorm, FILE *fptr )

{

    double  vec[3], *pval, *rbe, norm, sum, dist, x, y, z, boron;
    int     i, j, reg, ihead, uvval;

    DEBUG_TRACE_IN printf("Entering line_edit\n");

    j = 0;
    
/*
 *  Determine vector from start to end
 */

    sum = 0.0;
    for ( i=0; i < 3; i++ ) {
       vec[i] = end[i] - start[i];
       sum += vec[i] * vec[i];
    }
    norm = sqrt ( sum );
    vec[0] /= norm;
    vec[1] /= norm;
    vec[2] /= norm;

/*
 *  Print header for the line edit
 */

    fprintf ( fptr, "Line edit from %f %f %f to %f %f %f at increments of %f cm.\n\n",
              start[0], start[1], start[2], end[0], end[1], end[2], delta );

/*
 *   Now, initialize and start loop to generate points along line
 */

    dist = 0.0;
    ihead = ADD_HEADER;
    x = start[0];
    y = start[1];
    z = start[2];
    while ( dist < norm ) {
       uvval = locate_regionnum ( x, y, z );
       reg = uvhdata->regionnum[uvval];
       rbe = assemble_rbe ( REG_RBE, uvval );
       boron = edit_data.blood_b10 * uvhdata->dose[uvval].tissue_to_blood;
       pval = point_dose ( x, y, z, gnorm, nnorm, reg, rbe, ihead, boron, fptr );
       if ( pval ) {
          for ( i=0; i < nned; i++ ) {
             line[j+i] = pval[i];
          }
       }
       ihead = NO_HEADER;
       j += nned;
       dist += delta;
       x += delta * vec[0];
       y += delta * vec[1];
       z += delta * vec[2];
    }

/*
 *  Make sure that the end point is always included in the edit
 */

    x = end[0];
    y = end[1];
    z = end[2];
    uvval = locate_regionnum ( x, y, z );
    reg = uvhdata->regionnum[uvval];
    rbe = assemble_rbe ( REG_RBE, uvval );
    boron = edit_data.blood_b10 * uvhdata->dose[uvval].tissue_to_blood;
    pval = point_dose ( x, y, z, gnorm, nnorm, reg, rbe, ihead, boron, fptr );
    if ( pval ) {
       for ( i=0; i < nned; i++ ) {
          line[j+i] = pval[i];
       }
    }

/*
 *  Print end of edit signal
 */

    fprintf ( ofile, "\n\n\n\n     **************************************************    \n\n" );

    DEBUG_TRACE_OUT printf("Done with line_edit\n");

    return;

}




/******************************************************************************/

void surface_edit ( double *pt, double *vec1, double *vec2, double *rbe, double gnorm,
                    double nnorm, FILE *fptr )

/*
 *  The contour data is always written on a MAX_XY x MAX_XY grid
 */

{

    double boron, delta, fov, norm, sum, *pval, vec3[3], x, xprime, y, yprime, z, zprime;
    int    i, j, reg, uvval;

    DEBUG_TRACE_IN printf("Entering surface_edit\n");
    
/*
 *  Determine third basis vector as cross product of other two
 */

    vec3[0] = vec1[1]*vec2[2] - vec1[2]*vec2[1];
    vec3[1] = vec1[2]*vec2[0] - vec1[0]*vec2[2];
    vec3[2] = vec1[0]*vec2[1] - vec1[1]*vec2[0];

/*
 *  Now, normalize all three basis vectors (they may already
 *  be normalized to unity, but need to be certain)
 */

    sum = 0.0;
    for ( i=0; i < 3; i++ ) {
       sum += vec1[i] * vec1[i];
    }
    norm = sqrt ( sum );
    for ( i=0; i < 3; i++ ) {
       vec1[i] /= norm;
    }

    sum = 0.0;
    for ( i=0; i < 3; i++ ) {
       sum += vec2[i] * vec2[i];
    }
    norm = sqrt ( sum );
    for ( i=0; i < 3; i++ ) {
       vec2[i] /= norm;
    }

    sum = 0.0;
    for ( i=0; i < 3; i++ ) {
       sum += vec3[i] * vec3[i];
    }
    norm = sqrt ( sum );
    for ( i=0; i < 3; i++ ) {
       vec3[i] /= norm;
    }

/*
 *  This set of basis vectors provides a self-orthogonal transformation
 *  from the original space to the new space, where the new space is the
 *  standard cartesian coordinate system
 *
 *  Using this, transform the basis point to the new space (only need the
 *  new z-component, which is invariant on the transformed plane)
 */

    zprime = pt[0] * vec3[0] + pt[1] * vec3[1] + pt[2] * vec3[2];

/*
 *  This now serves as the z-plane in the transformed space
 *  We can treat this as a normal plane, and transform the space points
 *  back to the original space for writing to the contour data file
 *
 *  So, knowing this, we simply step through the transformed plane in x- and y- directions
 *  and convert back to the real world for the dose calculation
 */

    fov = uvhdata->pixelsizecolumns * uvhdata->imagecolumns * 0.5;
    xprime = -fov;
    delta = 2.0*fov/(MAX_XY-1);
    boron = 1.0;
    for ( j = 0; j < MAX_XY; j++ ) {
       yprime = -fov;
       for ( i = 0; i < MAX_XY; i++ ) {
          x = xprime * vec1[0] + yprime * vec2[0] + zprime * vec3[0];
          y = xprime * vec1[1] + yprime * vec2[1] + zprime * vec3[1];
          z = xprime * vec1[2] + yprime * vec2[2] + zprime * vec3[2];
          uvval = locate_regionnum ( x, y, z );
          reg = uvhdata->regionnum[uvval];
          if ( reg ) {
             pval = point_dose ( x, y, z, gnorm, nnorm, reg, rbe, NO_PRINT, boron, fptr );
             fprintf ( fptr, "%7.4f %7.4f %7.4f %2d %6.3e %6.3e %6.3e",
                       y/fov, x/fov, z/fov, reg, pval[I_TOT], pval[I_B10], pval[I_GAM]);
             fprintf ( fptr, " %6.3e %6.3e %6.3e %6.3e %6.3e %6.3e\n",
                       pval[I_N14], pval[I_HYD]+pval[I_PR], pval[I_OTH], pval[I_FF],
                       pval[I_EF], pval[I_TF] );
          }
          else {
             fprintf ( fptr, "%7.4f %7.4f %7.4f %2d %6.3e %6.3e %6.3e",
                       y/fov, x/fov, z/fov, reg, 0.0, 0.0, 0.0);
             fprintf ( fptr, " %6.3e %6.3e %6.3e %6.3e %6.3e %6.3e\n",
                       0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );
          }
          yprime += delta;
       }
       xprime += delta;
    }

    DEBUG_TRACE_OUT printf("Done with surface_edit\n");

    return;

}




/******************************************************************************/

void create_chd ( int plane, int num_planes, double zval, double *rbe, int ihead, FILE *chdfile )

{

    char    version[32];
    int     i, j;
    double  phi_old, theta_old, ang1[24], ang2[24];

    DEBUG_TRACE_IN printf("Entering create_chd\n");

    if ( get_version_string ( "seraPlan", version ) == 0 ) {
       strcpy ( version, "Unavailable" );
    }

    if ( ihead > NO_HEADER ) {
       fprintf (chdfile, "Version_stamp:  seraPlan_%s\n", version );
       fprintf (chdfile, "FOV:             %13.5e\n", 0.5*uvhdata->pixelsizecolumns*uvhdata->imagecolumns);
       fprintf (chdfile, "xmin:            -1.0\n");
       fprintf (chdfile, "xmax:             1.0\n");
       fprintf (chdfile, "ymin:            -1.0\n");
       fprintf (chdfile, "ymax:             1.0\n");
       fprintf (chdfile, "x_in_field:         1\n");
       fprintf (chdfile, "y_in_field:         2\n");
       fprintf (chdfile, "z_in_field:         3\n");
       fprintf (chdfile, "num_cols:          40\n");
       fprintf (chdfile, "num_rows:          40\n");
       fprintf (chdfile, "run_title:      %.80s\n", edit_dose->title);
       fprintf (chdfile, "num_planes:       %3d\n", num_planes);
       fprintf (chdfile, "dose_comp_0:    total\n");
       fprintf (chdfile, "dose_comp_1:    boron\n");
       fprintf (chdfile, "dose_comp_2:    gamma\n");
       fprintf (chdfile, "dose_comp_3:    nitrogen\n");
       fprintf (chdfile, "dose_comp_4:    fast\n");
       fprintf (chdfile, "dose_comp_5:    other\n");
       fprintf (chdfile, "dose_comp_6:    fast fluence\n");
       fprintf (chdfile, "dose_comp_7:    epithermal fluence\n");
       fprintf (chdfile, "dose_comp_8:    thermal fluence\n");
       fprintf (chdfile, "conc_0:           1.0\n");
       fprintf (chdfile, "conc_1:           1.0\n");
       fprintf (chdfile, "conc_2:           1.0\n");
       fprintf (chdfile, "conc_3:          %13.5e\n", *edit_dose->sets[0].n_dens/4.29947e-04);
       fprintf (chdfile, "conc_4:          %13.5e\n", *edit_dose->sets[0].h_dens/5.97493e-03);
       fprintf (chdfile, "conc_5:           1.0\n");
       fprintf (chdfile, "conc_6:           1.0\n");
       fprintf (chdfile, "conc_7:           1.0\n");
       fprintf (chdfile, "conc_8:           1.0\n");
       fprintf (chdfile, "rbe_val_0:        1.0\n");
       fprintf (chdfile, "rbe_val_1:        1.0\n");
       fprintf (chdfile, "rbe_val_2:        1.0\n");
       fprintf (chdfile, "rbe_val_3:        1.0\n");
       fprintf (chdfile, "rbe_val_4:        1.0\n");
       fprintf (chdfile, "rbe_val_5:        1.0\n");
       fprintf (chdfile, "rbe_val_6:        1.0\n");
       fprintf (chdfile, "rbe_val_7:        1.0\n");
       fprintf (chdfile, "rbe_val_8:        1.0\n");
       fprintf (chdfile, "ref_dose_0:      %13.5e\n", results->ref_dose[I_TOT]);
       fprintf (chdfile, "ref_dose_1:      %13.5e\n", results->ref_dose[I_B10]);
       fprintf (chdfile, "ref_dose_2:      %13.5e\n", results->ref_dose[I_GAM]);
       fprintf (chdfile, "ref_dose_3:      %13.5e\n", results->ref_dose[I_N14]);
       fprintf (chdfile, "ref_dose_4:      %13.5e\n", results->ref_dose[I_HYD]+results->ref_dose[I_PR]);
       fprintf (chdfile, "ref_dose_5:      %13.5e\n", results->ref_dose[I_OTH]);
       fprintf (chdfile, "ref_dose_6:      %13.5e\n", results->ref_dose[I_FF]);
       fprintf (chdfile, "ref_dose_7:      %13.5e\n", results->ref_dose[I_EF]);
       fprintf (chdfile, "ref_dose_8:      %13.5e\n", results->ref_dose[I_TF]);
 
       phi_old = theta_old = 1.0e+20;
       for ( j=0, i = 0; i < edit_dose->nset; i++ ) {
          if ( phi_old != *edit_dose->sets[i].phi || theta_old != *edit_dose->sets[i].theta ) {
             ang1[j] = phi_old = *edit_dose->sets[i].phi;
             ang2[j] = theta_old = *edit_dose->sets[i].theta;
             j++;
          }
       }
       fprintf (chdfile, "number_beams:     %3d\n", j);
       for ( i = 0; i < j; i++ ) {
          fprintf (chdfile, "phi_val_%02d:    %13.5e\n", i, ang1[i]);
          fprintf (chdfile, "theta_val_%02d:  %13.5e\n", i, ang2[i]);
       }
    }

    fprintf (chdfile, "z_value_%02d:    %13.5e\n", plane, zval);

    DEBUG_TRACE_OUT printf("Done with create_chd\n");

    return;

}




/*****************************************************************************/

void create_mask ( double *pt, double *vec1, double *vec2, FILE *cmffile )

{

    int i, j, idiff, uvval;
    double delta, fov, sum, vec3[3], x, xprime, y, yprime, z, zprime;
    unsigned char buf[MAX_RESOLUTION];

    DEBUG_TRACE_IN printf("Entering create_mask\n");

/*
 *  idiff is used to maximize the grey-scale contrast between regions
 */

    if ( num_regions > 0 )
       idiff = 255/num_regions;
    else {
       printf("num_regions = 0 in create_mask; setting idiff to one\n");
       idiff = 1;
    }

/*
 *  Determine third basis vector as cross product of other two
 */

    vec3[0] = vec1[1]*vec2[2] - vec1[2]*vec2[1];
    vec3[1] = vec1[2]*vec2[0] - vec1[0]*vec2[2];
    vec3[2] = vec1[0]*vec2[1] - vec1[1]*vec2[0];

/*
 *  Normalize all three basis vectors (they may already
 *  be normalized to unity, but need to be certain)
 */

    sum = 0.0;
    for ( i=0; i < 3; i++ ) {
       sum += vec1[i] * vec1[i];
    }
    sum = sqrt ( sum );
    for ( i=0; i < 3; i++ ) {
       vec1[i] /= sum;
    }

    sum = 0.0;
    for ( i=0; i < 3; i++ ) {
       sum += vec2[i] * vec2[i];
    }
    sum = sqrt ( sum );
    for ( i=0; i < 3; i++ ) {
       vec2[i] /= sum;
    }

    sum = 0.0;
    for ( i=0; i < 3; i++ ) {
       sum += vec3[i] * vec3[i];
    }
    sum = sqrt ( sum );
    for ( i=0; i < 3; i++ ) {
       vec3[i] /= sum;
    }

/*
 *  Transform the basis point to the new space (only need the
 *  new z-component, which is invariant on the transformed plane)
 */
    zprime = pt[0] * vec3[0] + pt[1] * vec3[1] + pt[2] * vec3[2];

/*
 *  Pack integers into unsigned character array for writing
 */

    fov = uvhdata->pixelsizecolumns * uvhdata->imagecolumns * 0.5;
    yprime = fov;
    delta = 2.0*fov/MAX_RESOLUTION;
    for ( j = 0; j < MAX_RESOLUTION; j++ ) {
       xprime = -fov;
       for ( i = 0; i < MAX_RESOLUTION; i++ ) {
          xprime += delta;
          x = xprime * vec1[0] + yprime * vec2[0] + zprime * vec3[0];
          y = xprime * vec1[1] + yprime * vec2[1] + zprime * vec3[1];
          z = xprime * vec1[2] + yprime * vec2[2] + zprime * vec3[2];
          uvval = locate_regionnum ( x, y, z );
          uvval *= idiff;
          buf[i] = (unsigned char) uvval;
       }
       fwrite ( (char *) buf, MAX_RESOLUTION, 1, cmffile );
       yprime -= delta;
    }

    DEBUG_TRACE_OUT printf("Done with create_mask\n");

    return;

}




/******************************************************************************/

void box_edit ( int box_num, double xmin, double xmax, double ymin, double ymax, double zmin,
                double zmax, double gnorm, double nnorm, body_struct *bodies, FILE *fptr )

{

    double x, y, z, vol, volsum, *pval, *rbe, *mean, *max, *min, *ref;
    double bin_width, dmin, dmax, boron ;
    float *bins;
    int    i, ibin, int_width, int1, int2, nbin, num_bins, reg, rreg, uvval;
    char   tmpstr[MAX_FILE];

    DEBUG_TRACE_IN printf("Entering box_edit\n");
    
    PostStatusMessage ( "Performing user-defined dose-volume histogram edits" );

/*
 *  Make local copies of some arrays to shorten the references
 */
    mean = results->boxes->dose_mean[box_num];
    max = results->boxes->dose_max[box_num];
    min = results->boxes->dose_min[box_num];
    ref = results->boxes->dose_ref[box_num];
    bins = results->boxes->dv_bins[box_num];

    nbin = edit_data.n_bin;
    bin_width = edit_data.upper_dv/nbin;
    int_width = bin_width;
    num_bins = nbin * nned;

    vol = uvhdata->pixelsizecolumns * uvhdata->pixelsizerows * uvhdata->pixelsizeslices;

/*
 *  Set up RBE and reference dose data for regions
 *  Assume here that all bodies in the acceptable body list have the same RBEs,
 *  and use these RBEs for all subsequent dose computations
 */

    for ( reg=1; strcmp(uvhdata->bodyname[reg_uvval[reg]], bodies->bodies[0]) && 
                 reg <= num_regions; reg++ ) {
       ;
    }
    rbe = assemble_rbe ( REG_RBE, reg_uvval[reg] );
    boron = edit_data.blood_b10 * uvhdata->dose[reg_uvval[reg]].tissue_to_blood;
    pval = point_dose ( results->ref_loc[0], results->ref_loc[1], results->ref_loc[2], gnorm, nnorm,
                        reg, rbe, NO_PRINT, boron, fptr );
    for ( i = 0; i < nned; i++ ) {
       ref[i] = pval[i];
    }

/*
 *  Initialize min and max values for search
 */
    volsum = dmax = 0.0;
    dmin = 1.0e+20;

/*
 *  Loop over all univels in box (slices, then rows, then columns)
 */
    for ( z = zmin; z <= zmax; z += uvhdata->pixelsizeslices ) {
       for ( y = ymin; y <= ymax; y += uvhdata->pixelsizecolumns ) {
          for ( x = xmin; x <= xmax; x += uvhdata->pixelsizerows ) {
             uvval = locate_regionnum ( x, y, z );
             rreg = uvhdata->regionnum[uvval];
             if ( rreg && is_in_body_list ( uvval, bodies ) ) {
                volsum += vol;
                pval = point_dose ( x, y, z, gnorm, nnorm, rreg, rbe, NO_PRINT, boron, fptr );
/*
 *  Check for maximum total dose
 */
                if ( pval[2] > dmax ) {
                   dmax = pval[2];
                   results->boxes->max_loc[box_num][0] = x;
                   results->boxes->max_loc[box_num][1] = y;
                   results->boxes->max_loc[box_num][2] = z;
                }
/*
 *  Check for minimum total dose
 */
                if ( pval[2] < dmin ) {
                   dmin = pval[2];
                   results->boxes->min_loc[box_num][0] = x;
                   results->boxes->min_loc[box_num][1] = y;
                   results->boxes->min_loc[box_num][2] = z;
                }
/*
 *  Update mean and appropriate histogram bin
 */
                for ( i = 0; i < nned; i++ ) {
                   mean[i] += pval[i];
                   min[i] = MIN ( pval[i], min[i] );
                   max[i] = MAX ( pval[i], max[i] );
                   if ( ref[i] > 0.0 ) {
                      ibin = bin_width * pval[i]/ref[i];
                      ibin = nned * MIN ( ibin, nbin );
                      bins[ibin+i] += 1.0;
                   }
                }
             }  /* if reg */
          }  /* for x */
       }  /* for y */
    }  /* for z */

/*
 *  Complete calculation of the mean dose, and normalize the dose bins
 */

    if ( volsum > 0.0 ) {
       boron = vol/volsum;
       for ( i = 0; i < nned; i++ ) {
          mean[i] *= boron;
       }
       for ( ibin = 0; ibin < num_bins+nned; ibin++ ) {
          bins[ibin] *= boron;
       }

/*
 *  Now, finish by printing the results to the output file
 *  Need to create a string containing the acceptable body list
 */
       strcpy ( tmpstr, "" );
       for ( i = 0; i < bodies->num_bodies-1; i++ ) {
          strcat ( tmpstr, bodies->bodies[i] );
          strcat ( tmpstr, "+" );
       }
       strcat ( tmpstr, bodies->bodies[bodies->num_bodies-1] );

       fprintf ( fptr, "Dose-volume edit for %s with volume %5.4e cc\n", tmpstr, volsum );
       fprintf ( fptr, "Blood boron concentration is %4.3f ppm with a tissue-to-blood ratio of %4.3f\n", edit_data.blood_b10, uvhdata->dose[reg_uvval[reg]].tissue_to_blood );
       boron = edit_data.blood_b10 * uvhdata->dose[reg_uvval[reg]].tissue_to_blood;
       fprintf ( fptr, "Total body boron concentration is %4.3f ppm\n\n", boron );

       fprintf ( fptr, "                 Total      B-10       Gamma      N-14     Hydrogen     Fast      Thermal     %5s\n", other[oflag] );
       fprintf ( fptr, "  Interval       Dose       Dose       Dose       Dose       Dose       Flux       Flux       Dose\n");

       int1 = int2 = 0;
       for ( ibin = 0; ibin < num_bins; ibin+= nned ) {
          int2 += int_width;
          fprintf ( fptr, "%3d%% - %3d%%    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f\n",
                    int1, int2, bins[ibin+I_TOT], bins[ibin+I_B10], bins[ibin+I_GAM],
                    bins[ibin+I_N14], bins[ibin+I_PR]+bins[ibin+I_HYD], bins[ibin+I_FF],
                    bins[ibin+I_TF], bins[ibin+I_OTH] );
          int1 += int_width;
       }
       fprintf ( fptr, "  > %3d%%       %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f\n\n",
                 edit_data.upper_dv, bins[ibin+I_TOT], bins[ibin+I_B10], bins[ibin+I_GAM],
                 bins[ibin+I_N14], bins[ibin+I_PR]+bins[ibin+I_HYD], bins[ibin+I_FF],
                 bins[ibin+I_TF], bins[ibin+I_OTH] );

/*
 *  Finally, print the max, min, mean, and reference dose components, and the
 *  maximum and minimum locations
 */

       fprintf ( fptr, "DV maximum     %7.3f    %7.3f    %7.3f    %7.3f    %7.3f  %4.3e  %4.3e    %7.3f\n",
                 max[I_TOT], max[I_B10], max[I_GAM], max[I_N14], max[I_PR]+max[I_HYD],
                 max[I_FF], max[I_TF], max[I_OTH] );
       fprintf ( fptr, "DV minimum     %7.3f    %7.3f    %7.3f    %7.3f    %7.3f  %4.3e  %4.3e    %7.3f\n",
                 min[I_TOT], min[I_B10], min[I_GAM], min[I_N14], min[I_PR]+min[I_HYD],
                 min[I_FF], min[I_TF], min[I_OTH] );
       fprintf ( fptr, "DV mean        %7.3f    %7.3f    %7.3f    %7.3f    %7.3f  %4.3e  %4.3e    %7.3f\n",
                 mean[I_TOT], mean[I_B10], mean[I_GAM], mean[I_N14], mean[I_PR]+mean[I_HYD],
                 mean[I_FF], mean[I_TF], mean[I_OTH] );
       fprintf ( fptr, "Reference      %7.3f    %7.3f    %7.3f    %7.3f    %7.3f  %4.3e  %4.3e    %7.3f\n",
                 ref[I_TOT], ref[I_B10], ref[I_GAM], ref[I_N14], ref[I_PR]+ref[I_HYD],
                 ref[I_FF], ref[I_TF], ref[I_OTH] );
       fprintf ( fptr, "RBE or CF      %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f\n\n",
                 rbe[I_TOT], rbe[I_B10], rbe[I_GAM], rbe[I_N14], rbe[I_HYD],
                 rbe[I_FF], rbe[I_TF], rbe[I_OTH] );

       fprintf ( fptr, "   Location where minimum total dose occurs is %7.3f %7.3f %7.3f\n",
                 results->boxes->min_loc[box_num][0], results->boxes->min_loc[box_num][1],
                 results->boxes->min_loc[box_num][2] );
       fprintf ( fptr, "   Location where maximum total dose occurs is %7.3f %7.3f %7.3f\n\n\n",
                 results->boxes->max_loc[box_num][0], results->boxes->max_loc[box_num][1],
                 results->boxes->max_loc[box_num][2] );

    }  /* if volsum */

    DEBUG_TRACE_OUT printf("Done with box_edit\n");

    return;

}




/******************************************************************************/

void DV_histogram ( double gnorm, double nnorm, FILE *fptr )

{

    double x, xmin, xmax, y, ymin, ymax, z, zmin, zmax, vol;
    double *pval, *rbe, **mean, **max, **min, **ref, ratio;
    double bin_width, volsum[MAX_REGIONS], dmin[MAX_REGIONS], dmax[MAX_REGIONS], boron ;
    float  **bins;
    int    i, ibin, int_width, int1, int2, nbin, num_bins, reg, uvval;

    int          imin, imax, jmin, jmax, kmin, kmax;
    body_struct *bodlist;

    DEBUG_TRACE_IN printf("Entering DV_histogram\n");
    
    PostStatusMessage ( "Performing standard dose-volume histogram edits" );

    bodlist = (body_struct *) MT_malloc ( sizeof (body_struct) );

/*
 *  Make local copies of some arrays to shorten the references
 */
    mean = results->boxes->dose_mean;
    max = results->boxes->dose_max;
    min = results->boxes->dose_min;
    ref = results->boxes->dose_ref;
    bins = results->boxes->dv_bins;

    nbin = edit_data.n_bin;
    bin_width = edit_data.upper_dv/nbin;
    int_width = bin_width;
    num_bins = nbin * nned;

    vol = uvhdata->pixelsizecolumns * uvhdata->pixelsizerows * uvhdata->pixelsizeslices;

/*
 *  Set up RBE and reference dose data for all regions
 */

    for ( reg = 1; reg <= num_regions; reg++ ) {
       rbe = assemble_rbe ( REG_RBE, reg_uvval[reg] );
       boron = edit_data.blood_b10 * uvhdata->dose[reg_uvval[reg]].tissue_to_blood;
       pval = point_dose ( results->ref_loc[0], results->ref_loc[1], results->ref_loc[2], gnorm,
                           nnorm, reg, rbe, NO_PRINT, boron, fptr );
       if ( pval ) {
          for ( i = 0; i < nned; i++ ) {
             ref[reg][i] = pval[i];
          }
/*
 *  Initialize min and max values for search
 */
          dmin[reg] = 1.0e+20;
          volsum[reg] = dmax[reg] = 0.0;
       }
    }

/*
 *  Loop over all univels in subelement mesh (each column and row in each slice)
 */
    xmin = uvhdata->rlaxismin + 0.5*uvhdata->pixelsizecolumns;
    xmax = uvhdata->rlaxismax;
    ymin = uvhdata->paaxismin + 0.5*uvhdata->pixelsizerows;
    ymax = uvhdata->paaxismax;
    zmin = uvhdata->isaxismin + 0.5*uvhdata->pixelsizeslices;
    zmax = uvhdata->isaxismax;
    for ( z = zmin; z <= zmax; z += uvhdata->pixelsizeslices ) {
       for ( y = ymin; y <= ymax; y += uvhdata->pixelsizecolumns ) {
          for ( x = xmin; x <= xmax; x += uvhdata->pixelsizerows ) {
             uvval = locate_regionnum ( x, y, z );
             reg = uvhdata->regionnum[uvval];
             if ( reg && strcmp( uvhdata->bodyname[uvval], "buffer") ) {
                rbe = assemble_rbe ( REG_RBE, uvval );
                boron = edit_data.blood_b10 * uvhdata->dose[uvval].tissue_to_blood;
                volsum[reg] += vol;
                pval = point_dose ( x, y, z, gnorm, nnorm, reg, rbe, NO_PRINT, boron, fptr );
/*
 *  Check for maximum total dose
 */
                if ( pval[I_TOT] > dmax[reg] ) {
                   dmax[reg] = pval[I_TOT];
                   results->boxes->max_loc[reg][0] = x;
                   results->boxes->max_loc[reg][1] = y;
                   results->boxes->max_loc[reg][2] = z;
                }
/*
 *  Check for minimum total dose
 */
                if ( pval[I_TOT] < dmin[reg] ) {
                   dmin[reg] = pval[I_TOT];
                   results->boxes->min_loc[reg][0] = x;
                   results->boxes->min_loc[reg][1] = y;
                   results->boxes->min_loc[reg][2] = z;
                }
/*
 *  Update mean and appropriate histogram bin
 */
                for ( i = 0; i < nned; i++ ) {
                   mean[reg][i] += pval[i];
                   min[reg][i] = MIN ( pval[i], min[reg][i] );
                   max[reg][i] = MAX ( pval[i], max[reg][i] );
                   if ( ref[reg][i] > 0.0 ) {
                      ibin = bin_width * pval[i]/ref[reg][i];
                      ibin = nned * MIN ( ibin, nbin );
		      bins[reg][ibin+i] += 1.0;
                   }
                }
             }  /* if reg */
          }  /* for x */
       }  /* for y */
    }  /* for z */


/*
 *  Complete calculation of the mean dose, and normalize the dose bins
 */

    fprintf ( dvfile, "DV histogram edits for patient %s\n", edit_data.patient_name );
    fprintf ( dvfile, "Number of bins in edits = %d\n\n", nbin );
    for ( reg = 1; reg <= num_regions; reg++ ) {
       if ( volsum[reg] > 0.0 ) {
          rbe = assemble_rbe ( REG_RBE, reg_uvval[reg] );
          boron = vol/volsum[reg];
          for ( i = 0; i < nned; i++ ) {
             mean[reg][i] *= boron;
          }
          for ( ibin = 0; ibin < num_bins+nned; ibin++ ) {
             bins[reg][ibin] *= boron;
          }

/*
 *  Now, finish by printing the results to the output file
 */

          fprintf ( fptr, "Dose-volume edit for %s with volume %5.4e cc\n",
                    uvhdata->bodyname[reg_uvval[reg]], volsum[reg] );
          fprintf ( fptr, "Blood boron concentration is %4.3f ppm with a tissue-to-blood ratio of %4.3f\n", edit_data.blood_b10, uvhdata->dose[reg_uvval[reg]].tissue_to_blood );
          boron = edit_data.blood_b10 * uvhdata->dose[reg_uvval[reg]].tissue_to_blood;
          fprintf ( fptr, "Total body boron concentration is %4.3f ppm\n\n", boron );

          fprintf ( fptr, "                 Total      B-10       Gamma      N-14     Hydrogen     Fast      Thermal     %5s\n", other[oflag] );
          fprintf ( fptr, "  Interval       Dose       Dose       Dose       Dose       Dose       Flux       Flux       Dose\n");

          int1 = int2 = 0;
          for ( ibin = 0; ibin < num_bins; ibin+= nned ) {
             int2 += int_width;
             fprintf ( fptr, "%3d%% - %3d%%    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f\n",
                       int1, int2, bins[reg][ibin+I_TOT], bins[reg][ibin+I_B10],
                       bins[reg][ibin+I_GAM], bins[reg][ibin+I_N14],
                       bins[reg][ibin+I_PR]+bins[reg][ibin+I_HYD],
                       bins[reg][ibin+I_FF], bins[reg][ibin+I_TF],
                       bins[reg][ibin+I_OTH] );
             int1 += int_width;
          }
          fprintf ( fptr, "  > %3d%%       %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f\n\n",
                    edit_data.upper_dv, bins[reg][ibin+I_TOT], bins[reg][ibin+I_B10],
                    bins[reg][ibin+I_GAM], bins[reg][ibin+I_N14],
                    bins[reg][ibin+I_PR]+bins[reg][ibin+I_HYD],
                    bins[reg][ibin+I_FF], bins[reg][ibin+I_TF],
                    bins[reg][ibin+I_OTH] );
 
/*
 *  Finally, print the max, min, mean, and reference dose components, and the
 *  maximum and minimum locations
 */

          fprintf ( fptr, "DV maximum     %7.3f    %7.3f    %7.3f    %7.3f    %7.3f  %4.3e  %4.3e    %7.3f\n",
                    max[reg][I_TOT], max[reg][I_B10], max[reg][I_GAM],
                    max[reg][I_N14], max[reg][I_PR]+max[reg][I_HYD],
                    max[reg][I_FF], max[reg][I_TF], max[reg][I_OTH] );
          fprintf ( fptr, "DV minimum     %7.3f    %7.3f    %7.3f    %7.3f    %7.3f  %4.3e  %4.3e    %7.3f\n",
                    min[reg][I_TOT], min[reg][I_B10], min[reg][I_GAM],
                    min[reg][I_N14], min[reg][I_PR]+min[reg][I_HYD],
                    min[reg][I_FF], min[reg][I_TF], min[reg][I_OTH] );
          fprintf ( fptr, "DV mean        %7.3f    %7.3f    %7.3f    %7.3f    %7.3f  %4.3e  %4.3e    %7.3f\n",
                    mean[reg][I_TOT], mean[reg][I_B10], mean[reg][I_GAM],
                    mean[reg][I_N14], mean[reg][I_PR]+mean[reg][I_HYD],
                    mean[reg][I_FF], mean[reg][I_TF], mean[reg][I_OTH] );
          fprintf ( fptr, "Reference      %7.3f    %7.3f    %7.3f    %7.3f    %7.3f  %4.3e  %4.3e    %7.3f\n",
                    ref[reg][I_TOT], ref[reg][I_B10], ref[reg][I_GAM],
                    ref[reg][I_N14], ref[reg][I_PR]+ref[reg][I_HYD],
                    ref[reg][I_FF], ref[reg][I_TF], ref[reg][I_OTH] );
          fprintf ( fptr, "RBE or CF      %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f    %7.3f\n\n",
                    rbe[I_TOT], rbe[I_B10], rbe[I_GAM], rbe[I_N14], rbe[I_HYD],
                    rbe[I_FF], rbe[I_TF], rbe[I_OTH] );

          fprintf ( fptr, "   Location where minimum total dose occurs is %7.3f %7.3f %7.3f\n",
                    results->boxes->min_loc[reg][0], results->boxes->min_loc[reg][1],
                    results->boxes->min_loc[reg][2] );
          fprintf ( fptr, "   Location where maximum total dose occurs is %7.3f %7.3f %7.3f\n\n\n",
                    results->boxes->max_loc[reg][0], results->boxes->max_loc[reg][1],
                    results->boxes->max_loc[reg][2] );

/*
 *  Now, check for need to perform the equal volumes edit - n_avg > 1
 *
 *  This edit divides the body, on an edit-voxel level, into n_avg equal subvolumes,
 *  and computes the average dose in each subvolume
 */

          if ( edit_data.n_avg > 1 ) {
             bodlist->num_bodies = 1;
             strcpy ( bodlist->bodies[0], uvhdata->bodyname[reg_uvval[reg]] );
             compute_n_avg ( reg, gnorm, nnorm, bodlist, fptr );
          }

          fprintf ( fptr, "\n\n\n\n     **************************************************\n\n" );

       }  /* if volsum > 0 */
    }  /* for reg */

/*
 *  Free allocated memory
 */

    MT_free ( (void *) bodlist );

    DEBUG_TRACE_OUT printf("Done with DV_histogram\n");

    return;

}




/******************************************************************************/

void compute_n_avg ( int num, double gnorm, double nnorm, body_struct *body, FILE *fptr )

{

/*
 *  Computes the average dose in N_avg equal-volume sections of a body
 *
 *  Note that these will be different from the mean dose for the dose-volume
 *  integrals, as we are using the edit mesh here, not the univel mesh
 */

    char   tmpstr[MAX_FILE];
    int    k, k1, k2, k3, kk, nedit, reg, uvval, igap, kg;
    double boron, delw, *pval, *rbe, sum, tmp, vol;
    double x, xmin, xmax, y, ymin, ymax, z, zmin, zmax ;
    double *dk;

    DEBUG_TRACE_IN printf("Entered compute_n_avg\n");

/*
 *  Setup parameters for voxel mesh search - minima and maxima of spatial mesh
 */

    delw = *edit_dose->delw;
    nedit = edit_dose->nedit;
    xmin = *edit_dose->x0 + 0.5*delw;
    xmax = *edit_dose->x0 + nedit*delw;
    ymin = *edit_dose->y0 + 0.5*delw;
    ymax = *edit_dose->y0 + nedit*delw;
    zmin = *edit_dose->z0 + 0.5*delw;
    zmax = *edit_dose->z0 + nedit*delw;

/*
 *  Allocate memory for dk
 */

    dk = (double *) MT_malloc ( nedit * nedit * nedit * sizeof (double) );

/*
 *  Setup RBEs and boron for dose computation - assume that all bodies in the
 *  acceptable list have same RBE, and just use first name in list
 */

    for ( reg=1; strcmp(uvhdata->bodyname[reg_uvval[reg]], body->bodies[0]) &&
                 reg <= num_regions; reg++ ) {
       ;
    }
    rbe = assemble_rbe ( REG_RBE, reg_uvval[reg] );
    boron = edit_data.blood_b10 * uvhdata->dose[reg_uvval[reg]].tissue_to_blood;

    for ( k = 0, z = zmin; z < zmax; z += delw ) {
       for ( y = ymin; y < ymax; y += delw ) {
          for ( x = xmin; x < xmax; x += delw ) {
             uvval = locate_regionnum ( x, y, z );
             reg = uvhdata->regionnum[uvval];
             if ( uvval && is_in_body_list ( uvval, body ) ) {
                pval = point_dose ( x, y, z, gnorm, nnorm, reg, rbe, NO_PRINT, boron, fptr );
                dk[k] = pval[2];
                k++;
             }
          }  /* for x */
       }  /* for y */
    }  /* for z */

    if ( edit_data.n_avg > k ) {
       fprintf ( fptr, "\nCan't determine averages for %d volumes - more volumes than voxels\n",
                 edit_data.n_avg );
       DEBUG_TRACE_OUT printf("Done with compute_n_avg\n");
       return;
    }

/*
 *  Sort total doses, from largest (at head of list) to smallest
 *  Now using a Shell sort (as given in Elements of Programming Style
 *  by Kernighan and Plauger)
 */

    for ( igap = k; igap > 1; igap /= 2 ) {
       k2 = k - igap;
       k1 = 1;
       while ( k1 ) {
          k1 = 0;
          for ( kk = 0; kk < k2; kk++ ) {
             kg = kk + igap;
             if ( dk[kk] < dk[kg] ) {
                tmp = dk[kg];
                dk[kg] = dk[kk];
                dk[kk] = tmp;
                k1++;
             }
          }
       }
    }

/*
 *  Now, compute average dose for each of n_avg volumes
 */

    tmpstr[0] = '\0';
    for ( kk = 0; kk < body->num_bodies-1; kk++ ) {
       strcat ( tmpstr, body->bodies[kk] );
       strcat ( tmpstr, "+" );
    }
    strcat ( tmpstr, body->bodies[body->num_bodies-1] );

    k2 = 0;
    vol = delw * delw * delw;
    fprintf ( fptr, "Average doses for equal volume segments for %s\n\n", tmpstr );
    for ( kk = 0; kk < edit_data.n_avg; kk++ ) {
       sum = 0.0;
       k1 = k2;
       k2 = k * (kk+1)/edit_data.n_avg;
       for ( k3 = k1; k3 < k2; k3++ ) {
          sum += dk[k3] * vol;
       }
       results->boxes->dose_avg[num][kk] = sum/((k2 - k1)*vol);
       fprintf ( fptr, "   Average total dose for segment %d = %7.3f\n", kk+1,
                 results->boxes->dose_avg[num][kk] );
    }

/*
 *  Free allocated memory for dk
 */

    MT_free ( (void *) dk );

    DEBUG_TRACE_OUT printf("Done with compute_n_avg\n");

    return;

}




/******************************************************************************/

int find_reference ( body_struct *body, double gnorm, double nnorm, FILE *fptr )

{

    double  x, xmin, xmax, y, ymin, ymax, z, zmin, zmax, delw, *rbe, *pval, boron;
    int     loc, reg, uvval;

    PostStatusMessage ( "Defining the reference dose" );

/*
 *  Define reference doses using selected method
 */
    switch ( edit_data.ref_opt ) {

/*
 *  Volume 
 */
       case 1:

          delw = *edit_dose->delw;
          xmin = *edit_dose->x0 + 0.5 * delw;
          xmax = xmin + edit_dose->nedit * delw;
          ymin = *edit_dose->y0 + 0.5 * delw;
          ymax = ymin + edit_dose->nedit * delw;
          zmin = *edit_dose->z0 + 0.5 * delw;
          zmax = zmin + edit_dose->nedit * delw;

          rbe = assemble_rbe ( REF_RBE, 0 );
          boron = edit_data.ref_b10;
          loc = edit_data.ref_dose_opt;
          for ( z = zmin; z < zmax; z += delw ) {
             for ( x = xmin; x < xmax; x += delw ) {
                for ( y = ymin; y < ymax; y += delw ) {
                   uvval = locate_regionnum ( x, y, z );
                   reg = uvhdata->regionnum[uvval];
                   if ( uvval ) {
                      if ( is_in_body_list ( uvval, body ) ) {
                         pval = point_dose ( x, y, z, gnorm, nnorm, reg, rbe, NO_PRINT, boron, fptr );
                         if ( results->ref_dose[loc] < pval[loc] ) {
                            results->ref_dose[loc] = pval[loc];
                            results->ref_loc[0] = x;
                            results->ref_loc[1] = y;
                            results->ref_loc[2] = z;
                         }
                      }  /* if is_in_body_list */
                   }  /* if uvval */
                }  /* for x */
             }  /* for y */
          }  /* for z */
          uvval = locate_regionnum (results->ref_loc[0], results->ref_loc[1], results->ref_loc[2]);
          reg = uvhdata->regionnum[uvval];
          rbe = assemble_rbe ( OTHER_RBE, 0 );
          boron = 1.0;
          pval = point_dose ( results->ref_loc[0], results->ref_loc[1], results->ref_loc[2], gnorm,
                              nnorm, reg, rbe, NO_PRINT, boron, fptr );
          break;

/*
 *  Point
 */
       case 2:
          uvval = locate_regionnum ( edit_data.ref_pt[0], edit_data.ref_pt[1], edit_data.ref_pt[2] );
          reg = uvhdata->regionnum[uvval];
          rbe = assemble_rbe ( OTHER_RBE, 0 );
          boron = 1.0;
          pval = point_dose ( edit_data.ref_pt[0], edit_data.ref_pt[1], edit_data.ref_pt[2], gnorm,
                              nnorm, reg, rbe, NO_PRINT, boron, fptr );
          results->ref_loc[0] = edit_data.ref_pt[0];
          results->ref_loc[1] = edit_data.ref_pt[1];
          results->ref_loc[2] = edit_data.ref_pt[2];
          break;

    }  /* switch ref_opt */

/*
 *  Check to see if in subelement mesh - if not, error message and return
 */
    if ( pval == NULL ) {
       DT_error ( panel->menubar, "Reference point is outside subelement mesh.\nThis is generally considered a bad thing.", "Very Fatal Error", NULL );
       return 1;
    }

/*
 *  Store reference dose values, and edit
 */

    for ( loc = 0; loc < nned; loc++ ) {
       results->ref_dose[loc] = pval[loc];
    }

    fprintf ( fptr, "\n\n      Normalized Reference location\n\n" );
    fprintf ( fptr, "   Reference region...........................=  %d/%s\n",
              reg, uvhdata->bodyname[uvval] );
    fprintf ( fptr, "   Reference x (cm)...........................=  %12.3f\n", results->ref_loc[0] );
    fprintf ( fptr, "   Reference y (cm)...........................=  %12.3f\n", results->ref_loc[1] );
    fprintf ( fptr, "   Reference z (cm)...........................=  %12.3f\n", results->ref_loc[2] );
    fprintf ( fptr, "   Reference thermal flux (n/cm**2/s).........=  %12.5e\n", results->ref_dose[I_TF] );
    fprintf ( fptr, "   Reference Group 1 flux (n/cm**2/s).........=  %12.5e\n", results->ref_dose[I_FF] );
    fprintf ( fptr, "   Reference Group 2 flux (n/cm**2/s).........=  %12.5e\n", results->ref_dose[I_EF] );
    fprintf ( fptr, "   Reference gamma dose (cGy/s)...............=  %12.5e\n", results->ref_dose[I_GAM] );
    fprintf ( fptr, "   Reference proton recoil dose (cGy/s).......=  %12.5e\n", 
              results->ref_dose[I_HYD]+results->ref_dose[I_PR] );
    fprintf ( fptr, "   Reference boron-10 dose (cGy/s/ppm)........=  %12.5e\n", results->ref_dose[I_B10] );
    fprintf ( fptr, "   Reference N-14 dose (cGy/s)................=  %12.5e\n", results->ref_dose[I_N14] );
    fprintf ( fptr, "   Reference other neutron dose (cGy/s).......=  %12.5e\n", results->ref_dose[I_OTH] );
    fprintf ( fptr, "   Reference total dose (cGy/s)...............=  %12.5e\n\n", results->ref_dose[I_TOT] );
    fprintf ( fptr, "   Total gamma production (gammas/cc/s).......=  %12.5e\n", results->ref_dose[I_GP] );
    fprintf ( fptr, "   Ultrafast gamma production (gammas/cc/s)...=  %12.5e\n\n", results->ref_dose[I_UGP] );

    fprintf ( fptr, "\n   Total gamma source normalization factor (g/s)....=  %12.5e\n", gnorm );
    fprintf ( fptr, "   Total neutron normalization factor (n/s).........=  %12.5e\n\n", nnorm );

    return 0;

}




/******************************************************************************/

double *point_dose ( double x, double y, double z, double gnorm, double nnorm, int reg,
                     double *rbe, int ihead, double boron_level, FILE *fptr )

{

    static double rval[MAX_NNED];

    double *pval;
    int     i;

    DEBUG_TRACE_IN printf("Entered point_dose\n");

    pval = dose_interp ( x, y, z, fptr, ihead );

/*
 *  Print header, if needed
 */

    if ( ihead > NO_HEADER ) {
       if ( ihead == IS_POINT_EDIT ) {
          fprintf ( fptr, "\n\nPoint edit\n\n" );
       }
       fprintf ( fptr, "                                                                                        Group 1   Group 2   Thermal    Gamma   Ultrafast Reaction  Reaction\n" );
       fprintf ( fptr, " x (cm)  y (cm)  z (cm) Reg  Total   Boron-10    Gamma   Nitrogen  Hydrogen    %5s    Fluence   Fluence   Fluence    Prod    Gam Prod   Rate 1    Rate 2\n", other[oflag] );
    }

/*
 *  Now, normalize and print values, if values were returned
 */

    if ( pval ) {
       rval[I_GAM] = pval[I_GAM] * rbe[I_GAM] * gnorm;
       rval[I_HYD] = pval[I_HYD] * nnorm * rbe[I_HYD];
       rval[I_B10] = pval[I_B10] * nnorm * rbe[I_B10] * boron_level;
       rval[I_GP] = pval[I_GP] * nnorm * rbe[I_GP];
       rval[I_N14] = pval[I_N14] * nnorm * rbe[I_N14];
       rval[I_FF] = pval[I_FF] * nnorm * rbe[I_FF];
       rval[I_EF] = pval[I_EF] * nnorm * rbe[I_EF];
       rval[I_TF] = pval[I_TF] * nnorm * rbe[I_TF];
       rval[I_PR] = pval[I_PR] * nnorm * rbe[I_PR];
       rval[I_UGP] = pval[I_UGP] * nnorm * rbe[I_UGP];
       rval[I_OTH] = pval[I_OTH] * nnorm * rbe[I_OTH];
       rval[I_RR1] = pval[I_RR1] * nnorm;
       rval[I_RR2] = pval[I_RR2] * nnorm;

/*
 *  Compute total dose as sum of components
 */
       rval[I_TOT] = rval[I_GAM] + rval[I_HYD] + rval[I_B10] + rval[I_N14] + rval[I_PR] + rval[I_OTH];

       if ( ihead > NO_PRINT ) {
          fprintf ( fptr, "%7.3f %7.3f %7.3f %2d %7.3e %7.3e %7.3e",
                    x, y, z, reg, rval[I_TOT], rval[I_B10], rval[I_GAM]);
          fprintf ( fptr, " %7.3e %7.3e %7.3e %7.3e %7.3e %7.3e %7.3e %7.3e %7.3e %7.3e\n",
                    rval[I_N14], rval[I_HYD]+rval[I_PR], rval[I_OTH], rval[I_FF], rval[I_EF],
                    rval[I_TF], rval[I_GP], rval[I_UGP], rval[I_RR1], rval[I_RR2] );
       }
    }
    else {
       for ( i=0; i < nned; i++ ) {
          rval[i] = 0.0;
       }
    }

    DEBUG_TRACE_OUT printf("Done with point_dose\n");

    return rval;

}




/******************************************************************************/

double *dose_interp ( double x, double y, double z, FILE *fptr, int iprt )

{

    static double rval[MAX_NNED];

    double delw;
    double t[3], cb[3], t4, t5, s2, rval_min, rval_max, prod;

/*
 *  Remember to reverse array indices from Fortran code in usage of d
 */
    double d[3][3] = { { 0.5,  -0.5,  -0.0416666666666 },
                       {-1.0,   0.0,   1.0833333333333 },
                       { 0.5,   0.5,  -0.0416666666666 } };

    int i, j, k, ad, nedit, ii, jj, kk, ad1, l, m, n, iact, izero;
    int im, jm, km;

    DEBUG_TRACE_IN printf("Entered dose_interp\n");

/*
 *  Local copy of stuff used repeatedly
 */
    delw = *edit_dose->delw;
    nedit = edit_dose->nedit;

/*
 *  Find voxel containing (x,y,z) and check for existance in subelement mesh
 *  If outside subelement mesh, return null pointer
 */
    i = (x - *edit_dose->x0)/delw;
    j = (y - *edit_dose->y0)/delw;
    k = (z - *edit_dose->z0)/delw;
    ad = nned * (k + nedit * (j + i*nedit) );
    if ( (i < 0 || i >= nedit) || (j < 0 || j >= nedit) || (k < 0 || k >= nedit) ) {
       if ( iprt > NO_PRINT ) {
          fprintf ( fptr, "The point %f %f %f is outside the subelement mesh.\n", x, y, z );
       }
       return NULL;
    }

/*
 *  Set up coordinate system centered in the voxel of interest
 */
    cb[0] = x - *edit_dose->x0 - delw * ( (double)i + 0.5 );
    cb[1] = y - *edit_dose->y0 - delw * ( (double)j + 0.5 );
    cb[2] = z - *edit_dose->z0 - delw * ( (double)k + 0.5 );

/*
 *  Start loop over all dose components in bflux
 */
    for ( iact = 0; iact < nned; iact++ ) {
       rval[iact] = 0.0;

/*
 *  Check for zeroes in surrounding voxels - can't interpolate garbage
 */
       if ( edit_dose->bflux[ad+iact] > 0.0 ) {

          izero = 0;
          im = MIN(nedit-1,i+1);
          jm = MIN(nedit-1,j+1);
          km = MIN(nedit-1,k+1);
          for ( ii = MAX(0,i-1); ii <= im; ii++ ) {
             for ( jj = MAX(0,j-1); jj <= jm; jj++ ) {
                for ( kk = MAX(0,k-1); kk <= km; kk++ ) {
                   ad1 = nned * (kk + nedit * (jj + ii*nedit) );
                   if ( edit_dose->bflux[ad1+iact] <= 0.0 ) izero = 1;
                }  /* for kk */
             }  /* for jj */
          }  /* for ii */

/*
 *  Doing a second-order polynomial interpolation for each direction - start loops
 *  to compute the polynomial coefficients
 *
 *  n is direction
 *  m is coefficients
 *  l is data points
 */
          if ( izero )
             rval[iact] = edit_dose->bflux[ad+iact];
          else {
             prod = 1.0;
             for ( n = 0; n < 3; n++ ) {
                for ( m = 0; m < 3; m++ ) {
                   t[m] = 0.0;
                   for ( l = 0; l < 3; l++ ) {
                      switch ( n ) {
/*
 *  X-direction
 */
                         case 0:
                            if ( !i && !l ) {
                               ad1 = nned * (k + nedit * (j + (i+1)*nedit) );
                               s2 = bdry ( edit_dose->bflux[ad+iact], edit_dose->bflux[ad1+iact] );
                            }
                            else if ( i == nedit-1 && l == 2 ) {
                               ad1 = nned * (k + nedit * (j + (i-1)*nedit) );
                               s2 = bdry ( edit_dose->bflux[ad+iact], edit_dose->bflux[ad1+iact] );
                            }
                            else {
                               ad1 = nned * (k + nedit * (j + (i+l-1)*nedit) );
                               s2 = edit_dose->bflux[ad1+iact];
                            }
                            break;
/*
 *  Y-direction
 */
                         case 1:
                            if ( !j && !l ) {
                               ad1 = nned * (k + nedit * ( (j+1) + i*nedit) );
                               s2 = bdry ( edit_dose->bflux[ad+iact], edit_dose->bflux[ad1+iact] );
                            }
                            else if ( j == nedit-1 && l == 2 ) {
                               ad1 = nned * (k + nedit * ( (j-1) + i*nedit) );
                               s2 = bdry ( edit_dose->bflux[ad+iact], edit_dose->bflux[ad1+iact] );
                            }
                            else {
                               ad1 = nned * (k + nedit * ( (j+l-1) + i*nedit) );
                               s2 = edit_dose->bflux[ad1+iact];
                            }
                            break;
/*
 *  Z-direction
 */
                         case 2:
                            if ( !k && !l ) {
                               ad1 = nned * ( (k+1) + nedit * (j + i*nedit) );
                               s2 = bdry ( edit_dose->bflux[ad+iact], edit_dose->bflux[ad1+iact] );
                            }
                            else if ( k == nedit-1 && l == 2 ) {
                               ad1 = nned * ( (k-1) + nedit * (j + i*nedit) );
                               s2 = bdry ( edit_dose->bflux[ad+iact], edit_dose->bflux[ad1+iact] );
                            }
                            else {
                               ad1 = nned * ( (k+l-1) + nedit * (j + i*nedit) );
                               s2 = edit_dose->bflux[ad1+iact];
                            }
                            break;
                      }  /* switch */

                      t[m] += d[l][m] * s2;

                   }  /* for l (loop for all three points in interpolative scheme */

                }  /* for m (loop to create three polynomial coefficients */

/*
 *  Finish assembling the coefficients, and compute the resulting point value
 */
                t4 = cb[n]/delw;
                t5 = t4*( t[0]*t4 + t[1] ) + t[2];
                prod *= t5;

             }  /* for n (loop over all three coordinate axes */

             t4 = edit_dose->bflux[ad+iact];
             rval[iact] = prod/(t4*t4);
             if ( rval[iact] <= 0.0 ) rval[iact] = 0.0;
          }  /* if izero */

/*
 *  Check for rval within bounds for iact = 2, 7, 12 (total dose, fast flux, other dose)
 *  This is patch to give better (smoother) results for ultrafast cases
 */
          switch ( iact ) {
             case I_TOT:
             case I_FF:
             case I_OTH:
                rval_min = 1.0e99;
                rval_max = 0.0;
                for ( ii = MAX(0,i-1); ii < MIN(nedit-1,i+1); ii++ ) {
                   for ( jj = MAX(0,j-1); jj < MIN(nedit-1,j+1); jj++ ) {
                      for ( kk = MAX(0,k-1); kk < MIN(nedit-1,k+1); kk++ ) {
                         ad1 = nned * (kk + nedit * (jj + ii*nedit) );
                         rval_min = MIN(edit_dose->bflux[ad1+iact],rval_min);
                         rval_max = MAX(edit_dose->bflux[ad1+iact],rval_max);
                      }  /* for kk */
                   }  /* for jj */
                }  /* for ii */
                if ( rval[iact] > rval_max || rval[iact] < rval_min ) {
                   rval[iact] = edit_dose->bflux[ad+iact];
                }
                break;
             default:
                break;
          }  /* switch iact */

       }  /* if dose > 0 */
    }  /* for iact */

    DEBUG_TRACE_OUT printf("Done with dose_interp\n");

    return rval;

}




/******************************************************************************/

double bdry ( double a, double b )

{

    if ( b > 0 )
       return a*a/b;
    else
       return 0.0;

}




/******************************************************************************/

double *assemble_rbe ( int opt, int reg )

{

    static double data[MAX_NNED];

    int i;

    marker_type *dog;

    DEBUG_TRACE_IN printf("Entering assemble_rbe\n");

    switch ( opt ) {

/*
 *  Use the RBE values from the .uvh file
 */
       case REG_RBE:
          for ( i=0; i < nned; i++ ) {
             data[i] = 1.0;
          }
          if ( uvhdata->dose[reg].valid.gamma_rbe )
             data[I_GAM] = uvhdata->dose[reg].gamma_rbe;
          if ( uvhdata->dose[reg].valid.hydrogen_rbe )
             data[I_HYD] = uvhdata->dose[reg].hydrogen_rbe;
          if ( uvhdata->dose[reg].valid.boron_cf )
             data[I_B10] = uvhdata->dose[reg].boron_cf;
          if ( uvhdata->dose[reg].valid.nitrogen_rbe )
             data[I_N14] = uvhdata->dose[reg].nitrogen_rbe;
          if ( uvhdata->dose[reg].valid.recoil_rbe )
             data[I_PR] = uvhdata->dose[reg].recoil_rbe;
          if ( uvhdata->dose[reg].valid.other_rbe )
             data[I_OTH] = uvhdata->dose[reg].other_rbe;
       /* if ( uvhdata->dose[reg].valid.ultrafast_rbe )
             data[I_UK] = uvhdata->dose[reg].ultrafast_rbe; */
          break;

/*
 *  Use reference RBE values
 */
       case REF_RBE:
          for ( i=0; i < nned; i++ ) {
             data[i] = 1.0;
          }
          data[I_GAM] = edit_data.ref_rbe[0];
          data[I_HYD] = edit_data.ref_rbe[1];
          data[I_B10] = edit_data.ref_rbe[2];
          data[I_N14] = edit_data.ref_rbe[3];
          data[I_PR] = edit_data.ref_rbe[4];
          data[I_OTH] = edit_data.ref_rbe[5];
      /*  data[I_UK] = edit_data.ref_rbe[6]; */
          break;

/*
 *  Use input RBE values from constraint marker
 */
       case CONSTRAINT_RBE:
          dog = uvhdata->constraint_markers;
          for ( i = 0; i < reg; i++ ) {
             dog = dog->next;
          }
          for ( i = 0; i < nned; i++ ) {
             if ( dog->dose.valid.gamma_rbe )
                data[I_GAM] = dog->dose.gamma_rbe;
             if ( dog->dose.valid.hydrogen_rbe )
                data[I_HYD] = dog->dose.hydrogen_rbe;
             if ( dog->dose.valid.boron_cf )
                data[I_B10] = dog->dose.boron_cf;
             if ( dog->dose.valid.nitrogen_rbe )
                data[I_N14] = dog->dose.nitrogen_rbe;
             if ( dog->dose.valid.recoil_rbe )
                data[I_PR] = dog->dose.recoil_rbe;
             if ( dog->dose.valid.other_rbe )
                data[I_OTH] = dog->dose.other_rbe;
          /* if ( dog->dose.valid.ultrafast_rbe )
                data[I_UK] = dog->dose.ultrafast_rbe; */
          }
          break;

/*
 *  Use input RBE values from widget (not implemented)
 */
       case WIDGET_RBE:
          for ( i=0; i < nned; i++ ) {
             data[i] = 1.0;
          }
          break;

/*
 *  Default - RBE values are all 1.0
 */
       default:
          for ( i=0; i < nned; i++ ) {
             data[i] = 1.0;
          }
          break;

    }  /* switch opt */

    DEBUG_TRACE_OUT printf("Done with assemble_rbe\n");

    return data;

}




/******************************************************************************/

int locate_regionnum ( double x, double y, double z )

{

    int    i, j, k, reg;

/*
 *  Find univel containing (x,y,z) and determine region number
 *  If outside univel mesh, return 0
 *
 *  Also, take advantage of this situation to swap x and y axes,
 *  and reverse the original x axis, to put us into the univel space
 */

    j = (uvhdata->paaxismax - x)*uvhdata->inv_pixelsizerows;
    if ( j < 0 || j >= uvhdata->imagerows ) return (0);

    i = (y - uvhdata->rlaxismin)*uvhdata->inv_pixelsizecolumns;
    if ( i < 0 || i >= uvhdata->imagecolumns ) return (0);

    k = (z - uvhdata->isaxismin)*uvhdata->inv_pixelsizeslices;
    if ( k < 0 || k >= uvhdata->imageslices ) return (0);

    reg = (int) *(uvhdata->vol_arr + (k*uvhdata->imagerows + j)*uvhdata->imagecolumns + i);

    return reg;

}




/******************************************************************************/

int is_in_body_list ( int reg, body_struct *bodies )

{

    char *lname;
    int   i;

    if ( bodies->num_bodies > 0 ) {
       lname = uvhdata->bodyname[reg];
       for ( i = 0; i < bodies->num_bodies; i++ ) {
          if ( !strcmp(bodies->bodies[i], lname) ) {
             return 1;
          }
       }
       return 0;
    }
    else {
       return 1;
    }

}




/******************************************************************************/

void calc_norm_factors ( double *gnorm, double *nnorm, double delw )

{

    double exposure, gam_ratio, sgc, s_tot, vol;
    int    addr, ix, jy, kz, nxyz;

/*
 *  Initialize
 */

    sgc = *gnorm = *nnorm = 0.0;
    vol = delw * delw * delw;

/*
 *  Sum gamma production over all voxels
 */

    nxyz = edit_dose->nedit;
    for ( addr = 0, ix = 0; ix < nxyz; ix++ ) {
       for ( jy = 0; jy < nxyz; jy++ ) {
          for ( kz = 0; kz < nxyz; addr += nned, kz++ ) {
             sgc += edit_dose->bflux[addr+I_GP];
             sgc += edit_dose->bflux[addr+I_UGP];
          }
       }
    }

/*
 *  Sum weighted beam gamma component over all fields;
 *  also sum weighted beam neutron components
 */

    for ( ix = 0; ix < edit_dose->nset; ix++ ) {
       if ( edit_dose->sets[ix].run_dir[0] == 'N' || edit_dose->sets[ix].run_dir[0] == 'U') {
          exposure = *edit_dose->sets[ix].rel_wt;
          s_tot = *edit_dose->sets[ix].s_tot;
          gam_ratio = *edit_dose->sets[ix].gamratio;
          *gnorm += s_tot * gam_ratio * exposure;
          *nnorm += s_tot * (1.0 - gam_ratio) * exposure;
       }
    }

    *nnorm /= vol;
    *gnorm /= vol;
    *gnorm += sgc * *nnorm;
    *gnorm /= 3600.;

    return;

}




/******************************************************************************/

/*
 *  Cory Albright wrote the original function for Sera3d;
 *  I "borrowed" it for use here.
 */

void PostStatusMessage ( char *string )

{

    Display *display;
    XEvent  event;

    DEBUG_TRACE_IN printf ("Entered PostStatusMessage");

    if ( seraplan ) {
       display = XtDisplay ( seraplan );
       XSynchronize ( display, TRUE );
       XtVaSetValues ( status, XmNmessageString, XmStringCreateLocalized(string), NULL );

       XFlush ( display );
       while ( XtAppPending(app) ) {
          XtAppNextEvent ( app, &event );
          XtDispatchEvent ( &event );
       }

       XSynchronize ( display, FALSE );
    }

    DEBUG_TRACE_OUT printf ("Done with PostStatusMessage");

}




/******************************************************************************/

int store_edit_panel_data ( Widget w )

{

    char    filename[MAX_FILE], prefix[MAX_FILE];
    char    s1[MAX_FILE];
    char   *tmpstr, *ptr;
    int     ifile=0, ii, k;

    Boolean option;
    Widget  dose_type;

    unsigned int rstCheckFlags;

    DEBUG_TRACE_IN printf ("Entering store_edit_panel_data");

/*
 *  Store information from edit panel
 *
 *     Start with basic information at top of panel
 */
    tmpstr = XmTextFieldGetString ( panel->dose_filename );
    strcpy ( edit_data.dose_file, tmpstr );
    XtFree( tmpstr );
    
    if ( strlen(edit_data.dose_file) == 0 || !FT_fileExists( edit_data.dose_file ) )
    {
       DT_error ( w, "A valid dose (.rst) file has not been specified.", NULL, NULL );
       DEBUG_TRACE_OUT printf("Done with CalcEdits\n");
       return ( 0 );
    }

    /*
     * The .rst file exists. Now do some preliminary checking
     * of the format of the file, and the filenames in the file
     */
    checkFilenamesInRstFile( edit_data.dose_file, &rstCheckFlags );

    /*
     * If any bits in rstCheckFlags have been set, then something is
     * wrong with the .rst file.
     */
    if( rstCheckFlags )
    {
        displayResultsOfRstCheck( seraplan, edit_data.dose_file, rstCheckFlags );
        DEBUG_TRACE_OUT printf("Done with store_edit_panel_data\n");
        return ( 0 );
    }
    
        
    tmpstr = XmTextFieldGetString ( panel->id.text );
    strcpy ( edit_data.plan_name, tmpstr );
    XtFree( tmpstr );
    
    if ( strlen(edit_data.plan_name) == 0 ) {
       DT_error ( w, "No output file prefix (plan name) has been specified.", NULL, NULL );
       DEBUG_TRACE_OUT printf("Done with store_edit_panel_data\n");
       return ( 0 );
    }

    tmpstr = XmTextFieldGetString ( panel->name.text );
    strcpy ( edit_data.patient_name, tmpstr );
    XtFree( tmpstr );
    
    if ( strlen(edit_data.patient_name) == 0 ) {
       k = DT_decide ( seraplan, app, "No patient identifier has been specified.\nContinue?", "FYI",
                       "Yes", "No" );
       if ( !k ) {
          DEBUG_TRACE_OUT printf("Done with store_edit_panel_data\n");
          return ( 0 );
       }
    }
/*
 *  Check for existance of files to be written - check to see if
 *  user wants to overwrite.
 */

   strcpy ( prefix, edit_data.save_dir );
   strcat ( prefix, edit_data.plan_name );
   
   
   strcpy ( filename, prefix );
   strcat ( filename, ".edit" );
   if ( FT_fileExists( filename ) ) ifile = 1;

   strcpy ( filename, prefix );
   strcat ( filename, ".lin" );
   if ( FT_fileExists( filename ) ) ifile = 1;

   strcpy ( filename, prefix );
   strcat ( filename, ".dvh" );
   if ( FT_fileExists( filename ) ) ifile = 1;

   strcpy ( filename, prefix );
   strcat ( filename, ".chd" );
   if ( FT_fileExists( filename ) ) ifile = 1;

   strcpy ( filename, prefix );
   strcat ( filename, ".cdf.sz" );
   if ( FT_fileExists( filename ) ) ifile = 1;

   strcpy ( filename, prefix );
   strcat ( filename, ".cmf.sz" );
   if ( FT_fileExists( filename ) ) ifile = 1;
   

   if ( ifile ) {
       k = DT_decide ( w, app, "One or more files to be written already exist.\nShall I overwrite?",
                        "FYI", "Overwrite", "Don't Overwrite" );
       if ( !k ) {
          DEBUG_TRACE_OUT printf("Done with store_edit_panel_data\n");
          return ( 0 );
       }
   }

/*
 *     Next, the reference information
 *
 *        Blood boron level
 */
    tmpstr = XmTextFieldGetString ( panel->blood.text );
    sscanf ( tmpstr, "%lf", &edit_data.blood_b10 );
    XtFree( tmpstr );
    
/*
 *        Number of equal volume regions for DV histogram results
 */
    tmpstr = XmTextFieldGetString ( panel->navg.text );
    sscanf ( tmpstr, "%d", &edit_data.n_avg );
    XtFree( tmpstr );
    
/*
 *        Upper limit of bins for DV histograms
 */
    tmpstr = XmTextFieldGetString ( panel->upper_bin.text );
    sscanf ( tmpstr, "%d", &edit_data.upper_dv );
    XtFree( tmpstr );
    
/*
 *        Number of bins for DV histograms
 */
    tmpstr = XmTextFieldGetString ( panel->nbin.text );
    sscanf ( tmpstr, "%d", &edit_data.n_bin );
    XtFree( tmpstr );

/*
 *        Reference dose type
 */

    XtVaGetValues ( panel->opt_pulldown, XmNmenuHistory, &dose_type, NULL );
    tmpstr = XtName ( dose_type );
    if ( !strcmp(tmpstr, ref_type[0]) )
       edit_data.ref_dose_opt = 2;
    if ( !strcmp(tmpstr, ref_type[1]) )
       edit_data.ref_dose_opt = 8;
    if ( !strcmp(tmpstr, ref_type[2]) )
       edit_data.ref_dose_opt = 0;
    
/*
 *        State of reference option buttons (make sure one is selected)
 *
 *           Volume option
 */
    XtVaGetValues ( panel->ref_opt[0], XmNset, &option, NULL );
    if ( option ) {
       edit_data.ref_opt = 1;

       tmpstr = XmTextGetString ( panel->bor.text );
       sscanf ( tmpstr, "%lg", &edit_data.ref_b10 );
       XtFree( tmpstr );
        
       tmpstr = XmTextGetString ( panel->reg.text );
       strcpy( s1, tmpstr );
       XtFree( tmpstr );

       ptr = s1;
       for ( k=0, ii=0; *ptr; ptr++ ) {
          if ( isspace((int)*ptr) ) {
             edit_data.ref_regions.bodies[ii][k] = '\0';
             ii++;
             k = 0;
          }
          else {
             edit_data.ref_regions.bodies[ii][k] = *ptr;
             k++;
          }
       }
       edit_data.ref_regions.bodies[ii][k] = '\0';
       if ( k ) edit_data.ref_regions.num_bodies = ii+1;
    }
    else {

/*
 *           Point option
 */
       XtVaGetValues ( panel->ref_opt[1], XmNset, &option, NULL );
       if ( option ) {
          edit_data.ref_opt = 2;
            
          tmpstr = XmTextGetString ( panel->pt.text );
          sscanf ( tmpstr, "%lg %lg %lg", &edit_data.ref_pt[0],
                   &edit_data.ref_pt[1],
                   &edit_data.ref_pt[2] );
          XtFree( tmpstr );
       }

/*
 *           No selection - error
 */
       else {
          DT_error ( w, "No reference dose option has been selected.\nPlease choose one.",
                     NULL, NULL );
          DEBUG_TRACE_OUT printf("Done with store_edit_panel_data\n");
          return ( 0 );
       }
    }

/*
 *        Reference RBE values
 */
    for ( k=0; k < NUM_RBE; k++ ) {
       tmpstr = XmTextGetString ( panel->rbe[k].text );
       ii = sscanf ( tmpstr, "%lg", &edit_data.ref_rbe[k] );
       XtFree( tmpstr );
       if ( ii <= 0 ) {
          DT_error ( w, "One or more reference RBE values are undefined.\nPlease correct and try again.", NULL, NULL );
          DEBUG_TRACE_OUT printf("Done with store_edit_panel_data\n");
          return ( 0 );
       }
    }

/*
 *     Finally, store the standard edits flag
 */
    XtVaGetValues ( panel->std_edit, XmNset, &option, NULL );
    if ( option )
       edit_data.perf_edits = 1;
    else
       edit_data.perf_edits = 0;

    DEBUG_TRACE_OUT printf ("Done with store_edit_panel_data");

    return ( 1 );

}




/******************************************************************************/

int perform_edits ( int isInteractive, Widget parent )

{

    char    *tmpstr;
    char    filename[MAX_FILE];
    char    uv_filename[MAX_FILE];
    double  gnorm, nnorm;

    char    s1[MAX_FILE+60];
    int     k, ii, num_pixels, irr;

    FILE   *editrst, *test;

    DEBUG_TRACE_IN printf ("Entering perform_edits");

/*
 *  Check to ensure that directory name ends with a / - needed to do simple concatenation
 *  to construct save filenames
 */

    if ( edit_data.save_dir[strlen(edit_data.save_dir)-1] != '/' ) {
       strcat ( edit_data.save_dir, "/" );
    }

/*
 *  Set up dose file name, and read dose file
 */

    edit_dose = (dose_struct *) MT_malloc ( sizeof ( dose_struct ) );
    set_up_dose_mem ( edit_dose, MAX_FIELDS * MAX_FRACTIONS );

    if ( !( editrst = fopen ( edit_data.dose_file, "r" ) ) ) {
       DEBUG_TRACE_OUT printf("Done with perform_edits\n");
       if ( isInteractive ) {
          sprintf ( s1, "The dose file %s does not exist.\nPlease enter another filename.",
                    edit_data.dose_file );
          DT_error ( parent, s1, "File error", NULL );
          XUndefineCursor ( XtDisplay ( parent ), XtWindow ( parent ) );
          XtSetSensitive ( parent, True );
          XtUnmanageChild ( status );
       }
       else {
          printf ( "The dose file %s does not exist.\nPlease enter another filename.",
                   edit_data.dose_file );
       }
       return ( 1 );
    }

    read_rst_file ( edit_dose, editrst, &irr );
    fclose( editrst );
    nned = ( irr ? MAX_NNED : MAX_NNED-2 );
    for ( oflag = k = 0; k < edit_dose->nset; k++ ) {
       if ( strchr(edit_dose->sets[k].run_dir, 'U') || strchr(edit_dose->sets[k].run_dir, 'P') )
          oflag = 1;
    }
    
/*
 *  Set up univel file names, and read univel files
 */

    uvhdata = (geom_info_t *) MT_malloc ( sizeof ( geom_info_t ) );
    strcpy ( uv_filename, edit_dose->bsg_file );
    strcpy ( filename, uv_filename );
    strcat ( filename, "h" );
    if ( FT_fileExists( filename ) == 0 ) { /* Shouldn't happen, we've already checked */
       DEBUG_TRACE_OUT printf("Done with perform_edits\n");
       if ( isInteractive ) {
          sprintf ( s1, "The .uvh file %s does not exist.\nPlease check your dose file.", filename );
          DT_error ( parent, s1, "File error", NULL );
          XUndefineCursor ( XtDisplay ( parent ), XtWindow ( parent ) );
          XtSetSensitive ( parent, True );
          XtUnmanageChild ( status );
       }
       else {
          printf ( "The .uvh file %s does not exist.\nPlease check your dose file.", filename );
       }
       return ( 1 );
    }
    read_existing_uvh ( uvhdata, filename );

    if ( !( test = fopen ( uv_filename, "r" ) ) ) { /* Shouldn't happen, we've already checked */
       DEBUG_TRACE_OUT printf("Done with perform_edits\n");
       if ( isInteractive ) {
          sprintf ( s1, "The uv file %s does not exist.\nPlease check your dose file.", uv_filename );
          DT_error ( parent, s1, "File error", NULL );
          XUndefineCursor ( XtDisplay ( parent ), XtWindow ( parent ) );
          XtSetSensitive ( parent, True );
          XtUnmanageChild ( status );
       }
       else {
          printf ( "The uv file %s does not exist.\nPlease check your dose file.", uv_filename );
       }
       return ( 1 );
    }
    num_pixels = uvhdata->imageslices * uvhdata->imagecolumns * uvhdata->imagerows;
    uvhdata->vol_arr = (unsigned char *) MT_malloc ( num_pixels * sizeof ( unsigned char ) );
    fread ( uvhdata->vol_arr, sizeof (unsigned char), num_pixels, test );
    fclose( test );

/*
 *  Calculate entry and exit points for beamline edits
 */

    calc_entry_points ();
    
/*
 *  Open edit output file
 */
    strcpy ( filename, edit_data.save_dir );
    strcat ( filename, edit_data.plan_name );
    strcat ( filename, ".edit" );
    ofile = fopen ( filename, "w" );
    if( !ofile )
    {
        DEBUG_TRACE_OUT printf("Done with perform_edits\n");
        if ( isInteractive ) {
           sprintf( s1, "The file %s\ncould not be opened for writing!", filename );
           DT_error( seraplan, s1, "File Error", NULL );
           XUndefineCursor ( XtDisplay ( parent ), XtWindow ( parent ) );
           XtSetSensitive ( parent, True );
           XtUnmanageChild ( status );
        }
        else {
           printf( "The file %s\ncould not be opened for writing!", filename );
        }
        return ( 1 );
    }

/*
 *  Determine number of regions in problem
 */

    num_regions = 0;
    ii = 1;
    for ( k = 0; k < MAX_REGIONS; k++ ) {
       if ( uvhdata->regionnum[k] ) {
          num_regions = MAX ( num_regions, uvhdata->regionnum[k] );
          reg_uvval[ii++] = k;
       }
    }
    fprintf ( ofile, "Number of regions = %d\n", num_regions );
    for ( ii=1; ii <= num_regions; ii++ )
       fprintf ( ofile, "Region %d is %s\n", ii, uvhdata->bodyname[reg_uvval[ii]] );

/*
 *  Read plan file (name given in dose file)
 */

    if ( strcmp (edit_dose->plan_file_name, "none") )
    {
        if( FT_fileExists( edit_dose->plan_file_name ) == 0 ) /* Shouldn't happen, we've already checked */
        {
            DEBUG_TRACE_OUT printf("Done with perform_edits\n");
            if ( isInteractive ) {
               sprintf ( s1, "The plan file %s does not exist.\nPlease check your dose file.",
                         edit_dose->plan_file_name );
               DT_error ( parent, s1, "File error", NULL );
               XUndefineCursor ( XtDisplay ( parent ), XtWindow ( parent ) );
               XtSetSensitive ( parent, True );
               XtUnmanageChild ( status );
            }
            else {
               printf ( "The plan file %s does not exist.\nPlease check your dose file.",
                        edit_dose->plan_file_name );
            }
            return ( 1 );
        }
        read_plan_file ( edit_dose->plan_file_name );
    }
    else {
       data.FRACTIONS = data.FIELDS = 1;
    }

/*
 *  Determine the normalization factors gnorm (gamma) and nnorm (neutron)
 */

    calc_norm_factors ( &gnorm, &nnorm, *edit_dose->delw );

/*
 *  Set up memory to store edit results
 */

    results = (edit_results_struct *) MT_malloc ( sizeof ( edit_results_struct ) );
    results_mem_setup ( results, edit_data.n_bin, uvhdata->num_constraint_markers, num_regions,
                        edit_data.n_avg );
    
/*
 *  Determine the reference dose - do this here, because we always need
 *  to know the reference dose, whether we do standard edits or not
 */
    if ( find_reference ( &edit_data.ref_regions, gnorm, nnorm, ofile ) ) {
       DEBUG_TRACE_OUT printf("Leaving perform_edits...find_reference returned 1\n");
       if ( isInteractive ) {
          XUndefineCursor( XtDisplay( parent ), XtWindow( parent ) );
          XtSetSensitive( parent, True );
          XtUnmanageChild ( status );
       }
       else {
          printf("Error in calculating the reference dose.\n");
       }
       return ( 1 );
    }
    fclose ( ofile );

/*
 *  Perform standard edits
 */

    if ( edit_data.perf_edits ) {
        strcpy( filename, edit_data.save_dir );
        strcat( filename, edit_data.plan_name );
        strcat( filename, ".edit" );
        ofile = fopen ( filename, "a+" );
        fprintf ( ofile, "\n\n\n\nGenerating standard edits\n\n" );
        standard_edits ( gnorm, nnorm );
    }

/*
 *  Perform other edits, as requested
 */

    if ( edit_data.calc_edits ) {
       other_edits ( gnorm, nnorm );
    }

    DEBUG_TRACE_OUT printf("Done with perform_edits\n");

    return ( 0 );

}




/******************************************************************************/

void calc_entry_points ()

{

    double theta, phi, x_vec, y_vec, z_vec, x, y, z;
    double step, dist;
    int    i, uvval, nbuf;

    DEBUG_TRACE_IN printf("Entering calc_entry_points\n");

/*
 *  Find uvval for buffer region
 */

    for ( i = 0; i < LIBUV_MAX_REGIONS; i++ ) {
       if ( !strcmp(uvhdata->bodyname[i], "buffer") ) {
          nbuf = uvhdata->regionnum[i];
       }
    }

/*
 *  For each beamline, compute the entry and exit points from the
 *  isocenter and the two angles
 */

    for ( i = 0; i < edit_dose->nset; i++ ) {
       theta = *edit_dose->sets[i].theta;
       phi = *edit_dose->sets[i].phi;

/*
 *  Set up the three components of the directional vector
 */

       x_vec = sin ( PI * phi/180.0 ) * sin ( PI * theta/180.0 );
       y_vec = -1.0 * sin ( PI * phi/180.0 ) * cos ( PI * theta/180.0 );
       z_vec = cos ( PI * phi/180.0 );

/*
 *  Find major direction of motion, and set step to be 1 univel in that direction
 */

       if ( fabs(x_vec) >= fabs(y_vec) ) {
          if ( fabs(x_vec) >= fabs(z_vec) ) {
             step = uvhdata->pixelsizecolumns;
          }
          else {
             step = uvhdata->pixelsizeslices;
          }
       }
       else {
          if ( fabs(y_vec) >= fabs(z_vec) ) {
             step = uvhdata->pixelsizerows;
          }
          else {
             step = uvhdata->pixelsizeslices;
          }
       }

/*
 *  Now, step along beamline from isocenter for distance equal to Zb
 *  This gives the entry point
 */

       x = *edit_dose->sets[i].xp;
       y = *edit_dose->sets[i].yp;
       z = *edit_dose->sets[i].zp;

       for ( dist = 0.0; dist < *edit_dose->sets[i].zb; dist += step ) {
          x += x_vec * step;
          y += y_vec * step;
          z += z_vec * step;
       }

       edit_dose->sets[i].entry[0] = x;
       edit_dose->sets[i].entry[1] = y;
       edit_dose->sets[i].entry[2] = z;

/*
 *  Step in the opposite direction from the isocenter until reach the buffer
 *  This gives exit point (more or less)
 */

       x_vec *= -1.0;
       y_vec *= -1.0;
       z_vec *= -1.0;
       x = *edit_dose->sets[i].xp;
       y = *edit_dose->sets[i].yp;
       z = *edit_dose->sets[i].zp;

       for ( uvval = locate_regionnum(x,y,z); (uvval != nbuf) && uvval; uvval = locate_regionnum(x,y,z) ) {
          x += x_vec * step;
          y += y_vec * step;
          z += z_vec * step;
       }

       edit_dose->sets[i].entry[3] = x;
       edit_dose->sets[i].entry[4] = y;
       edit_dose->sets[i].entry[5] = z;

    }  /* for i */

    DEBUG_TRACE_OUT printf("Done with calc_entry_points\n");

    return;

}




/******************************************************************************/
