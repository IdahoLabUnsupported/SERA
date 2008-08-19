/*
 * contours_file.c
 * 
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 *
 * David Helzer
 * November, 1997
 *
 * This file contains the functions used to generate the 
 * contours data file which contains the contour points for 
 * multiple layers of contours.  This file is meant to be 
 * used in the BNCT-3D application to view the contours in 
 * three dimensions.
 */


#include <Xm/MessageB.h>
#include "contours_file.h"
#include "global.h"
#include "contours_calc.h"
#include "contours.h"
#include "scale_points.h"


#define FILE_CONTROL_DIALOG_MSG "Please select the file to\nsave the contour data to."
#define SELECT_FILES_MSG "Please select the contour\ndata files to be saved to the\nexternal counter file.\n\nClick OK when finished."



void cancel_select_file_callback (Widget w, XtPointer clientData, XtPointer callData);
void ok_add_file_callback (Widget w, XtPointer clientData, XtPointer callData);
void ok_contour_all_files_callback (Widget w, XtPointer clientData, XtPointer callData);
void generate_contours_file (void);
void generate_centered_points_from_contour_data (contour_data_type *contours,
                                        CONTOUR_LINES_TYPE *contour_lines,
                                        short image_width, short image_height,
                                        float *file_scale_factor);


char *files_to_contour[MAX_NUM_FILES];
char *contours_file_name;
int current_num_files = 0;

/*
 * generate_contours_file_callback() - A callback that is invoked when the
 * user selects the "Generate Contours File" button.  It prompts the user 
 * for a file in which to write the contours  and then calls the appropriate
 * function to save the levels
 *
 * David Helzer 8/97
 */
 
void generate_contours_file_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   static Widget file_selection_dialog = NULL;
   static Widget file_control_dialog = NULL;
   /*float scale_factor;
     */ 
   
   if (!file_control_dialog)
   {
      file_control_dialog = XmCreateInformationDialog (xcontoursTopLevelShell,
                                                       "file_control_dialog", NULL, 0);
                                                       
      XtUnmanageChild (XmMessageBoxGetChild (file_control_dialog, XmDIALOG_OK_BUTTON));
      XtUnmanageChild (XmMessageBoxGetChild (file_control_dialog, XmDIALOG_CANCEL_BUTTON));
      XtUnmanageChild (XmMessageBoxGetChild (file_control_dialog, XmDIALOG_HELP_BUTTON));

      XtVaSetValues (file_control_dialog,
                     XtVaTypedArg, XmNmessageString, XmRString,
                     FILE_CONTROL_DIALOG_MSG, strlen (FILE_CONTROL_DIALOG_MSG) + 1,
                     NULL);
   }
   
   
   if (!file_selection_dialog)
   {
      file_selection_dialog = 
	(Widget)XmCreateFileSelectionDialog (xcontoursTopLevelShell,
                                            "file_selection_dialog", NULL, 0);
      XtAddCallback (file_selection_dialog, XmNokCallback, 
                     ok_input_files_callback, (XtPointer) file_control_dialog);
      XtAddCallback (file_selection_dialog, XmNcancelCallback,
                     cancel_generate_contours_file_callback, 
                     (XtPointer) file_control_dialog);
   }                                    
        
   XtManageChild (file_selection_dialog);
   XtManageChild (file_control_dialog);
}


void ok_input_files_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) callData;
   Widget file_control_dialog = (Widget) clientData;
   char *file_name;
   
   XtUnmanageChild (w);
   XtUnmanageChild (file_control_dialog);

   XmStringGetLtoR ((*cbs).value, XmFONTLIST_DEFAULT_TAG, &file_name);
   
   contours_file_name = (char *) MT_malloc (strlen (file_name) + 1);
   strcpy (contours_file_name, file_name);
   generate_contours_file ();
}


void cancel_generate_contours_file_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   Widget file_control_dialog = (Widget) clientData;
   
   XtUnmanageChild (w);
   XtUnmanageChild (file_control_dialog);
}



void generate_contours_file (void)
{
   static Widget file_control_dialog;
   static Widget select_files_dialog;
   
   
      if (!file_control_dialog)
      {
         file_control_dialog = XmCreateInformationDialog (xcontoursTopLevelShell,
                                                          "file_control_dialog", NULL, 0);
                                                       
         XtUnmanageChild (XmMessageBoxGetChild (file_control_dialog, XmDIALOG_CANCEL_BUTTON));
         XtUnmanageChild (XmMessageBoxGetChild (file_control_dialog, XmDIALOG_HELP_BUTTON));

         XtVaSetValues (file_control_dialog,
                        XtVaTypedArg, XmNmessageString, XmRString,
                        SELECT_FILES_MSG, strlen (SELECT_FILES_MSG) + 1,
                        NULL);
      }

      
      if (!select_files_dialog)
      {
         select_files_dialog = 
	   (Widget)XmCreateFileSelectionDialog (xcontoursTopLevelShell,
                                               "select_files_dialog", NULL, 0);
         XtAddCallback (select_files_dialog, XmNokCallback, 
                        ok_add_file_callback, (XtPointer) file_control_dialog);
         XtAddCallback (select_files_dialog, XmNcancelCallback,
                        cancel_select_file_callback, NULL);

      }
      
      XtAddCallback (file_control_dialog, XmNokCallback,
                     ok_contour_all_files_callback, 
                     (XtPointer) select_files_dialog);
      
      XtManageChild (select_files_dialog);
      XtManageChild (file_control_dialog);
}


void ok_add_file_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   char *file_name;
   FILE *dose_file;
   XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) callData;


   XmStringGetLtoR ((*cbs).value, XmFONTLIST_DEFAULT_TAG, &file_name);
   printf ("File to add is %s\n", file_name);
   if ((dose_file = fopen (file_name, "r")) == NULL)
   {
      printf ("Unable to open file %s!\n", file_name);
      XBell (di, 100);
   }
   else
   {
      /* ensure that the file is a dose file */
      if (!is_dose_file (file_name))
      {
         if (!confirm_popup ("The file you have selected does not appear to be a valid dose file.\nA valid dose file typically ends in ',contour'.\nAre you sure you want to try to load this file?\n")) 
         {
            XtManageChild( fileSelectionBox );
            return;
         }
      }
      files_to_contour[current_num_files] = (char *) MT_malloc (strlen (file_name) + 1);
      strcpy (files_to_contour[current_num_files], file_name);
      current_num_files ++;
      fclose (dose_file);
   }
}
   
   

void cancel_select_file_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   XtUnmanageChild (w);
}



void ok_contour_all_files_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   int count, count2, count3, count4;
   floyd_data_ptr file_dose_data;
   FILE *contour_dose_file;
   FILE *contours_file = NULL;
   Widget file_selection = (Widget) clientData;
   static contour_data_type *contours;
   contour_data_type *cp;
   CONTOUR_LINES_TYPE contour_lines;
   float file_scale_factor;
   float z_value;    /*actually not needed when generating contours file*/

/* Some of these unnecessary */
   long ct, sct, kct;
   mesh_type *mtt;
   float *fpx, *fpy;
   float greatest_dimension = 0.0;
   float points[MAX_NUMBER_POINTS][2];



   XtUnmanageChild (file_selection);
   printf ("Name of file to write is %s\n", contours_file_name);
   contours_file = fopen (contours_file_name, "w+");
   if (!contours_file)
      printf ("Error writing to %s\n", contours_file_name);
   else
   {
      /* 
       * Output the number of contours and the contour names (percent levels) to the file
       */
       
      fprintf (contours_file, "%d\n", contour_levels.number_levels);
      for (count = 0; count < contour_levels.number_levels; count ++)
         fprintf (contours_file, "%-2d ", contour_levels.rlevel[count]);
      fprintf (contours_file, "\n\n");
      
      
      /*
       * Output the number of slices (files) to the file
       */
       
      fprintf (contours_file, "%d\n\n", current_num_files);
     
      contours = (contour_data_type *) MT_malloc (contour_levels.number_levels * sizeof (contour_data_type));
      if (contours == NULL) {
         CntrError ("Could not allocate contour space", "CntrMain");
         exit (0);
      } else printf ("Got the memory!!!!!!!!1\n");
      
      cp = contours;
      for (count = 0; count < contour_levels.number_levels; count++) {
         (*cp).cd_value = contour_levels.rlevel[count];
         (*cp).cd_color = contour_levels.colors[count];
         cp ++;
      }  
      

      for (count = 0; count < current_num_files; count ++)
      {
         printf ("  %s\n", files_to_contour[count]);
         
         
         /* 
          * Output the Z-Value (from filename) to the file
          */
          
         fprintf (contours_file, "%s\n", files_to_contour[count]);
         
         
         /*contour_dose_file = fopen (files_to_contour[count], "r");*/
         
         load_dose (files_to_contour[count], &file_dose_data,
		    NULL, &z_value);
                       
         ContoursCalculate (file_dose_data, contours, 
                            contour_levels.number_levels, doseFlag);

 /*        generate_centered_points_from_contour_data (contours, &contour_lines, 
                                                     256, 256, &file_scale_factor);
*/
file_scale_factor = 128.0;
printf ("Scale factor is %f\n", file_scale_factor);         
/*         for (count2 = 0; count2 < contour_levels.number_levels; count2 ++)
         {
            fprintf (contours_file, "%.2f\n", contour_levels.rlevel[count2]);
          }
  */ 
   
   
   
   
   cp = contours; 

   for (ct = 0; ct < contour_levels.number_levels; ct ++)
   {
      /* Print the dosage level to the file */
      fprintf (contours_file, "%-2d\n", contour_levels.rlevel[ct]);

      mtt = cp -> cd_mesh;
      for (sct = 0; sct < cp -> cd_nseg; sct ++)
      {
         if ((*mtt).me_npt > 0) 
         {
            fpx = mtt -> me_x;
            fpy = mtt -> me_y; 
            
            /* Print the number of points in this level to the file */
            fprintf (contours_file, "%d\n", (*mtt).me_npt);
            for (kct = 0; kct < mtt -> me_npt; kct ++)
            {
              /* points[kct][0] = *fpx;
               points[kct][1] = *fpy;*/

              /* Print the points to the file */
              fprintf (contours_file, "%.2f %.2f  ", 
                       *fpx * file_scale_factor, *fpy * file_scale_factor);
               
               *fpx ++;
               *fpy ++;
            }
            fprintf (contours_file, "\n");
/*            scale_points (points, (*contour_lines).lines[(*contour_lines).number_lines].points, 
                          (*mtt).me_npt, *file_scale_factor, image_width, image_height);
     
            (*contour_lines).lines[(*contour_lines).number_lines].number_points = (*mtt).me_npt;
            (*contour_lines).lines[(*contour_lines).number_lines].color = (*cp).cd_color;
            sprintf ((*contour_lines).lines[(*contour_lines).number_lines].label, "%.1f", (*cp).cd_value);
            (*contour_lines).lines[(*contour_lines).number_lines].value = (*cp).cd_value;
            (*contour_lines).number_lines ++;
*/
         }
         mtt ++;
      }    
      cp ++;
   }                         
         fclose (contour_dose_file);
         MT_free ((void *)files_to_contour[count]);
         fprintf (contours_file, "\n");
      }
      MT_free ((void *)contours);
      fclose (contours_file);
   }   
   MT_free ((void *)contours_file);
   printf ("Contours file has been constructed!  :)\n");
}







void generate_centered_points_from_contour_data (contour_data_type *contours,
                                                 CONTOUR_LINES_TYPE *contour_lines,
                                                 short image_width, short image_height,
                                                 float *file_scale_factor)
{
   long ct, sct, kct;
   contour_data_type *cp;
   mesh_type *mtt;
   float *fpx, *fpy;
   float greatest_dimension = 0.0;
   float points[MAX_NUMBER_POINTS][2];
   
   scaling_set_centered_origin();
   (*contour_lines).number_lines = 0;

   /* 
    * Determine the factor by which all points are to be scaled by 
    * finding the points with the greatest dimension (in either the
    * negative or positive direction)
    */
/*   cp = contours;

   for (ct = 0; ct < contour_levels.number_levels; ct ++)
   {
      mtt = cp -> cd_mesh;
      for (sct = 0; sct < cp -> cd_nseg; sct ++)
      {
         if ((*mtt).me_npt > 0) {
            fpx = mtt -> me_x;
            fpy = mtt -> me_y;
            for (kct = 0; kct < mtt -> me_npt; kct ++)
            {
               if ((float) fabs((double) *fpx) > greatest_dimension) {
                  greatest_dimension = (float) fabs ((double) *fpx);
                  points[0][0] = *fpx;
                  points[0][1] = *fpy;
               }
               if ((float) fabs ((double) *fpy) > greatest_dimension) {
                  greatest_dimension = (float) fabs ((double) *fpy);
                  points[0][0] = *fpx;
                  points[0][1] = *fpy;
               }
               *fpx ++;
               *fpy ++;
            }
         }
         mtt ++;
      }    
      cp ++;
   }
   printf ("Greatest point is %f %f\n", points[0][0], points[0][1]); 

   (*file_scale_factor) = get_scale_factor (image_width, image_height, points, 1);
*/

/*if (image_width == 512)
   *file_scale_factor = 256.0;
else*/

   *file_scale_factor = 128.0;

  /* 
   * Fill the contour_lines array with the scaled points.  
   */
 /*  cp = contours; 

   for (ct = 0; ct < contour_levels.number_levels; ct ++)
   {printf ("Level %f\n", contour_levels.rlevel[ct]);
      mtt = cp -> cd_mesh;
      for (sct = 0; sct < cp -> cd_nseg; sct ++)
      {
         if ((*mtt).me_npt > 0) 
         {
            fpx = mtt -> me_x;
            fpy = mtt -> me_y; 
            for (kct = 0; kct < mtt -> me_npt; kct ++)
            {
               points[kct][0] = *fpx;
               points[kct][1] = *fpy; printf ("%f %f\n", points[kct][0], points[kct][1]);
               *fpx ++;
               *fpy ++;
            }
            scale_points (points, (*contour_lines).lines[(*contour_lines).number_lines].points, 
                          (*mtt).me_npt, *file_scale_factor, image_width, image_height);
     
            (*contour_lines).lines[(*contour_lines).number_lines].number_points = (*mtt).me_npt;
            (*contour_lines).lines[(*contour_lines).number_lines].color = (*cp).cd_color;
            sprintf ((*contour_lines).lines[(*contour_lines).number_lines].label, "%.1f", (*cp).cd_value);
            (*contour_lines).lines[(*contour_lines).number_lines].value = (*cp).cd_value;
            (*contour_lines).number_lines ++;
         }
         mtt ++;
      }    
      cp ++;
   }
*/}
