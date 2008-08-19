/**********************************************************************
* 
* callbacks.c
*
* INEEL BNCT Research Project
* Montana State University - Bozeman
*
*
* This is a set of callbacks for the XContours program.  For 
* organizational purposes, all callbacks should be placed in 
* this file
*
*********************************************************************/
#define Local_vars

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/FileSB.h>
#include <X11/Xutil.h>
#include <X11/Shell.h>
#include <Xm/DialogS.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawingA.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/SelectioB.h>
#include <Xm/MessageB.h>
#include <Xm/ToggleBG.h>

/* local header files */
#include "global.h"
#include "commonfcns.h"
#include "picshell.h"
#include "read_raw.h"
#include "common.h"
#include "color.h"
#include "connection_tools.h"

#define MAX_LENGTH_STRING 200
#define MAX_LENGTH_MESSAGE 500

/*
 * MAX_LENGTH_CONTOUR_LEVEL_STRING - used by check_contour_level_values.
 * This same constant must also be defined in functions.c
 */
#define MAX_LENGTH_CONTOUR_LEVEL_STRING 300


/*
 * currently_selected_contour_level is a global string indicating the 
 * contour level that is highlighted at any given time in the list
 * widget from which the user can change the colors of contour levels.
 *   David Helzer  7-8-97
 */
char currently_selected_contour_level[MAX_LENGTH_MESSAGE];


/*
 * Messages displayed by various error and selection widgets
 * David Helzer
 */

#define CONTOUR_LEVEL_LABEL_MESSAGE "The following contour levels\nwill be displayed.\n\nEdit the window's text\nto change these values."

#define BAD_CHARACTER_FOUND_MESSAGE "An error occurred while attempting\nto load the new contour levels!\n\nWhen editing the desired levels,\nmake sure that no letters,\ndecimal points or symbols are included\nand that all values are\ngreater than or equal to zero.\n\nAt least one level and nomore than\n256 levels must be included.\n\nNo editing changes\nhave taken effect.\n"

#define CONTOUR_COLORS_MESSAGE "The following contour levels\nwill be displayed.\n\nEach contour level has an\nassociated color with which it\nis drawn in.  Each contour\ncolorwash region is displayed\nin the color of the greatest\nbordering contour level value.\n\nChanges may be used for only\nthis session or saved for later\nsessions by selecting the\nappropriate 'Apply' button."




static int locate_state = 0;

void FileSelectionCallback(Widget w, XtPointer client, XtPointer call)
{
   XmFileSelectionBoxCallbackStruct *fsbcbs =
      (XmFileSelectionBoxCallbackStruct *) call;

   DEBUG_TRACE_IN printf( "Entering FileSelectionCallback\n" );

     /* get the currently selected filename. */
     if ( !XmStringGetLtoR(fsbcbs->value, XmSTRING_DEFAULT_CHARSET,
          &image_file_name ) )
        return;  /* must have been an internal error */

   #if defined (MORE_VERBOSE)
      printf( "   image_file_name = \"%s\"\n", image_file_name );
   #endif  /* MORE_VERBOSE */

      if ((image_file = fopen(image_file_name,"r")) == NULL)
        {
	  printf ("Unable to open file!! %s\n", image_file_name);
	  XBell(di,100);
        }
      else
        {
	  load_image(image_file, mainWindowDrawingArea);
	  slice_is_there = TRUE;
	  contours_are_current = FALSE;
	  load_imageEH(mainWindowDrawingArea, 0, &SureEvent);
        }

   #if defined (MORE_VERBOSE)
      printf( "after FileSelectionCallback\n" );
   #endif /* MORE_VERBOSE */

   DEBUG_TRACE_OUT printf( "Leaving FileSelectionCallback\n" );
}



/*
 * FileMaskCallback() - prompts the user to load a file
 * that contains masking regions to use.
 */
 
void FileMaskCallback (Widget w, XtPointer client, XtPointer call)
{
    XmFileSelectionBoxCallbackStruct *fsbcbs =
	                  (XmFileSelectionBoxCallbackStruct *) call;
    Boolean masks_replace_images;
    int redraw_contours = 0;
    int j, k;
    
    DEBUG_TRACE_IN printf( "Entering FileMaskCallback\n" );

    XtVaGetValues(masksReplaceImagesButton,
		  XmNset, &masks_replace_images,
		  NULL);

    /* get the currently selected filename. */
    if ( !XmStringGetLtoR(fsbcbs->value, XmSTRING_DEFAULT_CHARSET,
			  &image_file_name ) ) {
      DEBUG_TRACE_OUT printf( "Leaving FileMaskCallback\n" );
      return;  /* must have been an internal error */
    }

    /* Check to see if file name is "legal" */
    if (!is_mask_file(image_file_name)) {
	if (!confirm_popup("The file you have selected does not appear to be a valid mask file.\nA valid mask file typically ends in ',mask'.\nAre you sure you want to try to load this file?\n")) {
            DEBUG_TRACE_OUT printf( "Leaving FileMaskCallback\n" );
	    return;
	}
    }

    XtUnmanageChild (fileSelectionBox);    
    
    /* Get rid of old one if present */
    if (image_matrix.img_arr[image_matrix.active_picture].mask_in_use) {
      if (mask_pack.in_use) {
	mask_pack.in_use = FALSE;
	XtPopdown(mask_pack.shell);
      }
      removefrom_mask_list (image_matrix.active_picture);
      redraw_contours = 1;
    }
    /* Now, load the new one */
    load_mask(image_file_name);
    if (masks_replace_images) {
	for (j=0; j<512; j++)
	    for (k=0; k<512; k++)
		values[j*512+k]=mask_pack.imagedata[(j/2)*256+k/2];
    }
    if ((masks_replace_images)||(redraw_contours)) {
        contours_are_current = FALSE;
	load_imageEH(mainWindowDrawingArea, 0, &SureEvent);
    }
    DEBUG_TRACE_OUT printf( "Leaving FileMaskCallback\n" );
}


/*==========================================================================
   Function:        apply_mask_to_all_CB
 
   Purpose:         Toggles whether masks will be applied to one image or
                    to all images.  This is the callback for both 
		    applyToAllButtons.

   Parameters:      Normal callback parameters.  clientData tells which 
                    button is calling.  1 = applyToAllButton
		                        0 = applyToAllButton2

   Returned:        None.

   MTC 12/14/98
 ==========================================================================*/
void apply_mask_to_all_CB ( Widget w, XtPointer clientData, 
			    XtPointer callData )
{
    XmToggleButtonCallbackStruct *cbs 
        = (XmToggleButtonCallbackStruct *) callData;

    int button1 = (int) clientData;

    if ( button1 && XtIsManaged ( applyToAllButton2 ) )
    {
        if ( cbs->set )
	    XtVaSetValues ( applyToAllButton2, XmNset, TRUE, NULL );
	else
	    XtVaSetValues ( applyToAllButton2, XmNset, FALSE, NULL );
    }
    else if ( !button1 )
    {
        if ( cbs->set )
	    XtVaSetValues ( applyToAllButton, XmNset, TRUE, NULL );
	else
	    XtVaSetValues ( applyToAllButton, XmNset, FALSE, NULL );
    }
}


void clear_masksCB(Widget w, XtPointer client, XmAnyCallbackStruct *cbs)
{
   DEBUG_TRACE_IN printf( "Entering clear_masksCB\n" );
   set_unset_masks(image_matrix.active_picture, FALSE);
   DEBUG_TRACE_OUT printf( "Leaving clear_masksCB\n" );
}



void set_masksCB(Widget w, XtPointer client, XmAnyCallbackStruct *cbs)
{
   int i;

   DEBUG_TRACE_IN printf( "Entering set_masksCB\n" );   
   set_unset_masks(image_matrix.active_picture, 255);
   DEBUG_TRACE_OUT printf( "Leaving set_masksCB\n" );
}



/* called when mouse is pressed on a region */
void region_selectCB(Widget w, XtPointer client, XmDrawingAreaCallbackStruct *cbs)
{
   XEvent *event = cbs->event;
   unsigned int value;
   Boolean set_to;

   DEBUG_TRACE_IN printf( "Entering region_selectCB\n" );   

   if (cbs->reason == XmCR_INPUT)
   {
	if (event->xany.type == ButtonPress)
	{
	   value = mask_pack.maskdata[event->xbutton.y * DATA_WIDTH + event->xbutton.x];	

	   if (mask_pack.masks[value] == FALSE)
	     set_to = True;
	   else
	     set_to = False;

	   /* This function does different things depending on whether or
	    * not the apply to all button is selected.
	    */
	   update_masks(image_matrix.active_picture, value, set_to);
	}
   }
   DEBUG_TRACE_OUT printf( "Leaving region_selectCB\n" );   
}	



void UnmanageCallback (Widget w, XtPointer client, XtPointer call)
{
   DEBUG_TRACE_IN printf( "Entering UnmanageCallback\n" );   
   XtUnmanageChild( w );
   DEBUG_TRACE_OUT printf( "Leaving UnmanageCallback\n" );   
}



void ColorSlidersCallback (Widget w, XtPointer client, XtPointer call)
{
   DEBUG_TRACE_IN printf( "Entering ColorSliderCallback\n" );   

   XtVaSetValues( editColorRowColumn,
      XmNresizeHeight, True,
      XmNresizeWidth, True,
      NULL );

   XtUnmanageChild( editColorScrolledWindow );
   XtVaSetValues ( editColorRedScale,
		   XmNvalue, contour_color.red>>8,
		   NULL );
   XtVaSetValues ( editColorGreenScale,
		   XmNvalue, contour_color.green>>8,
		   NULL );
   XtVaSetValues ( editColorBlueScale,
		   XmNvalue, contour_color.blue>>8,
		   NULL );                          
   XtManageChild( editColorSlidersRowColumn ); 

   DEBUG_TRACE_OUT printf( "Leaving ColorSliderCallback\n" );   
}



void ColorListCallback (Widget w, XtPointer client, XtPointer call)
{
   DEBUG_TRACE_IN printf( "Entering ColorListCallback\n" );   

   XtVaSetValues( editColorRowColumn,
      XmNresizeHeight, False,
      XmNresizeWidth, False,
      NULL );

   XtUnmanageChild( editColorSlidersRowColumn );
   XtManageChild( editColorScrolledWindow );

   DEBUG_TRACE_OUT printf( "Leaving ColorListCallback\n" );   
}



/*
 * contour_levels_callback -- This callback is called when the user selects
 * the button "Contour Levels" from the main menu.  This callback displays
 * the current contour levels at which lines are to be drawn.  Currently, 
 * these levels cannot be changed in this window but must be changed in the
 * file 'contour_levels.txt' in the resources directory and, after being 
 * changed, the XContours program must be executed again.  
 * 
 * David Helzer 6-9-97
 */

void contour_levels_callback(void)
{
   extern Widget textWidgetForm;
   Widget        contour_level_form, contour_level_label;
   char          contour_level_string[MAX_LENGTH_CONTOUR_LEVEL_STRING];
   char          contour_levels_text[MAX_LENGTH_CONTOUR_LEVEL_STRING];
   XmString      message_to_display;
   int           count;

   DEBUG_TRACE_IN printf( "Entering contour_levels_callback\n" );   

  /*
   * Put a label on the text widget 
   */

   XtVaSetValues(textDialogShell,
		 XmNtitle, "Contour Levels",
		 NULL);

   message_to_display = XmStringCreateLtoR(CONTOUR_LEVEL_LABEL_MESSAGE, XmFONTLIST_DEFAULT_TAG);
   
   XtVaSetValues (textWidgetLabel,
		  XmNlabelString, message_to_display,
	 	  NULL);

   XmStringFree (message_to_display);
	     
		  
  /* 
   * Convert the float and int values of the global structure contour_levels.rlevel[]
   * to strings and copy each to the contour_levels_text string to be 
   * displayed by the text widget 
   */
   for (count = 0; count < contour_levels.number_levels; count++) {
     sprintf (contour_level_string, "%d\n", contour_levels.rlevel[count]);
     if (count == 0) 
       strcpy (contour_levels_text, contour_level_string);
     else 
       strcat (contour_levels_text, contour_level_string);
   }
      
   XmTextSetString (textWidgetTextArea, contour_levels_text);
   
   XtManageChild( textWidgetForm );	

   DEBUG_TRACE_OUT printf( "Leaving contour_levels_callback\n" );   
}


/*
 * David Helzer -- June 11, 1997
 * Apply_Contour_Levels_Callback -- invoked when the user clicks on the 
 * "Apply" button of the Contour Density widget.  This procedure examines each string 
 * in the widget and checks to make sure that it is a valid floating point number.  
 * If all are okay, the new levels are stored in the global data structure contour_levels.
 * If the are not all okay, an error dialog is created and no changes are updated.  
 */

void Apply_Contour_Levels_Callback (void)
{
  char         *contour_levels;
  FILE         *contour_levels_file;
  char          all_levels_text[MAX_LENGTH_CONTOUR_LEVEL_STRING];
  char          file_with_contour_levels[MAX_LENGTH_STRING];
  Widget        Bad_Character_Found_Dialog;  
  int           text_is_okay;

  DEBUG_TRACE_IN printf( "Entering Apply_Contour_Levels_Callback\n" );   

  contour_levels = XmTextGetString (textWidgetTextArea);

  check_contour_level_values (contour_levels, all_levels_text, &text_is_okay);

  /*
   * If all characters read are okay, load the levels in the string into
   * the global data structure using the procedure load_contour_levels_from_string
   * in the functions.c file.
   */

  if (text_is_okay)  {  
    load_contour_levels_from_string(all_levels_text);
    sort_contour_levels();
    
   /*
    * Update the contour levels legend & the colorwash_values
    */
    update_colorwash_values();
    update_legend();
    
    update_small_images (image_matrix.active_picture);
  }


  /* 
   * If there were errors in the text, display a dialog box stating so. 
   */
  else {      
    Bad_Character_Found_Dialog = XmCreateErrorDialog ( xcontoursTopLevelShell,
						       "Bad_Character_Found_Dialog",
						       NULL, 0);
    /*
     * Remove the Cancel and Help buttons.
     * Also, declare the string to be displayed and center it
     */
    XtUnmanageChild ( XmMessageBoxGetChild ( Bad_Character_Found_Dialog,
					     XmDIALOG_CANCEL_BUTTON ) );
    
    XtUnmanageChild ( XmMessageBoxGetChild ( Bad_Character_Found_Dialog,
					     XmDIALOG_HELP_BUTTON ) );
    
    XtVaSetValues (Bad_Character_Found_Dialog,
		   XtVaTypedArg, XmNmessageString, XmRString,
		   BAD_CHARACTER_FOUND_MESSAGE, strlen (BAD_CHARACTER_FOUND_MESSAGE)+1,
		   XmNmessageAlignment, XmALIGNMENT_CENTER,
		   XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
		   NULL );
    
    XtManageChild (Bad_Character_Found_Dialog);
  }

  DEBUG_TRACE_OUT printf( "Leaving Apply_Contour_Levels_Callback\n" );   
} 



/*
 * ScalePopupCallback() - Displays a scale bar widget to adjust various
 * controls
 */
 
void ScalePopupCallback (Widget w, XtPointer client, XtPointer call)
{
   int whichOne = (int) client;
   extern Widget      scaleWidgetForm;

   DEBUG_TRACE_IN printf( "Entering ScalePopupCallback\n" );   

   XtRemoveAllCallbacks( scaleWidget, XmNvalueChangedCallback );  /* remove old callbacks */

   if ( whichOne == 1 )
   {     
      XtVaSetValues( scaleDialogShell,
         XmNtitle, "Reserve Colors",
         NULL );

      XtVaSetValues( scaleWidget,
         XmNdecimalPoints, 0,
	 XmNmaximum, MAXCOLORS - NUM_RESCOL, 
	 XmNminimum, 1,
         XmNvalue, num_colors_avail + 1, /* off by 1 again ??? */	
         NULL );

      XtAddCallback( scaleWidget, XmNvalueChangedCallback,
         ReserveColorCallback, (caddr_t) 0 ); /* add new callback */
   }
      
   else
   {
      XtVaSetValues( scaleDialogShell,
         XmNtitle, "Contour Smoothing",
         NULL );

      XtVaSetValues( scaleWidget,
         XmNdecimalPoints, 2,
         XmNmaximum, 500,
         XmNminimum, 0,
         XmNvalue, (int) (smoothingValue * 100.0),
         NULL );

      XtAddCallback( scaleWidget, XmNvalueChangedCallback,
		     (XtCallbackProc)ContourSmoothCallback, 
		     (caddr_t) 0 ); /* add new callback */
   }

   XtManageChild( scaleWidgetForm );

   DEBUG_TRACE_OUT printf( "Leaving ScalePopupCallback\n" );   
}



void DosePopupCallback (Widget w, XtPointer client, XtPointer call)
{
   DEBUG_TRACE_IN printf( "Entering DosePopupCallback\n" );   
   XtManageChild( doseSelectionBox );
   DEBUG_TRACE_OUT printf( "Leaving DosePopupCallback\n" );   
}



void SelectColormapCallback (Widget w, XtPointer client, XtPointer call)
{
   DEBUG_TRACE_IN printf( "Entering SelectColormapCallback\n" );   
   XtManageChild( colormapSelectionBox );
   DEBUG_TRACE_OUT printf( "Leaving SelectColormapCallback\n" );   
}



void ReserveColorCallback(Widget w, XtPointer client, XtPointer call)
{
   XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) call;

   DEBUG_TRACE_IN printf( "Entering ReserveColorCallback\n" );   

#if defined (MORE_VERBOSE)
   printf( "   num_colors_avail = %d\n", num_colors_avail );
#endif /* MORE_VERBOSE */

   XtVaSetValues(masksReplaceImagesButton,
		 XmNset, False,
		 NULL);
   
   num_colors_avail = cbs->value - 1;  /* mwf 7-14-95:  -1 since off by 1 again */
   load_colormap( get_color_info()->cmap, 0, MAX_GRAY_INDEX - num_colors_avail - 1);
   colormap_loadrgb();
   recalc_pixels();
   load_imageEH(mainWindowDrawingArea, 0, &SureEvent);
   update_cbar();

   DEBUG_TRACE_OUT printf( "Leaving ReserveColorCallback\n" );   
}

void EditColorNameCallback(Widget w, XtPointer client, XtPointer call)
{
   XColor exact, closest;
   char  *colorName;
   XmSelectionBoxCallbackStruct *sbcbs = (XmSelectionBoxCallbackStruct *) call;

   DEBUG_TRACE_IN printf( "Entering EditColorNameCallback\n" );   

   /* get selection from the list. */
   XmStringGetLtoR(sbcbs->value, XmSTRING_DEFAULT_CHARSET, &colorName);

#if defined (MORE_VERBOSE)
   printf( "   colorName = \"%s\"\n", colorName );
   printf("Pixel value : %d\n", contour_color.pixel); 
#endif /* MORE_VERBOSE */

   XLookupColor(di, get_color_info()->cmap , colorName, &exact, &closest);

   contour_color.red = exact.red;         /* begin mwf 7-14-95 */
   contour_color.green = exact.green;
   contour_color.blue = exact.blue;
   myXStoreColor(di, get_color_info()->cmap, &contour_color);
   XtVaSetValues(editColorDrawnButton,              /* probably not necessary */
		 XmNbackground, get_color_info()->truecolors[contour_color.pixel],
		 NULL);                             /* end mwf */

#ifdef WHEN_YOU_HAVE_MORE_COLORS_AVAILABLE
   XmGetColors( XtScreen( editColorDrawnButton ), image_matrix.cmap, contour_color.pixel,
      &fg_color, &top_shadow, &btm_shadow, &select_color );

   XtVaSetValues( editColorDrawnButton, XmNtopShadowColor, top_shadow,
      XmNbottomShadowColor, btm_shadow,
      XmNselectColor, select_color,
      XmNarmColor, select_color,
      XmNborderColor, fg_color,
      NULL );
#endif  /* WHEN_YOU_HAVE_MORE_COLORS_AVAILABLE */

   DEBUG_TRACE_OUT printf( "Leaving EditColorNameCallback\n" );   
}


/*
 * ContourColorsPopupCallback() -- This callback is invoked when the user clicks on
 * the "Contour Colors" button of the XContours window.  This callback causes a 
 * widget containing the list of contour levels.  These levels can be selected and
 * colors can be provided for each.  If desired, the changes can be saved to an 
 * external file
 *
 * David Helzer   7-8-97
 */
 
void ContourColorsPopupCallback (void)
{
   extern Widget   contourColorsForm;
   XmString        message_to_display;
   XmString       *contour_levels_for_list;
   long            count;
   char            contour_level_string[MAX_LENGTH_STRING];
 
   DEBUG_TRACE_IN printf( "Entering ContourColorsPopupCallback\n" );   

   /*
    * Add the label to the top of the Contour Colors Popup Widget 
    */
    
   message_to_display = XmStringCreateLtoR (CONTOUR_COLORS_MESSAGE, XmFONTLIST_DEFAULT_TAG);
   
   XtVaSetValues (contourColorsLabel, 
                  XmNlabelType, XmSTRING,
                  XmNlabelString, message_to_display,
                  NULL);
   
   XmStringFree (message_to_display);
   
 
   /*
    * Put the contour levels on the list widget (as well as the "Max Values" item)
    */
    
   contour_levels_for_list = (XmString *) MT_malloc (sizeof (XmString) * 
                                                     (contour_levels.number_levels + 1));
    
   for (count = 0; count < contour_levels.number_levels; count ++) {
      sprintf (contour_level_string, "%d", contour_levels.rlevel[count]);
      contour_levels_for_list[count] = XmStringCreateLtoR (contour_level_string, 
                                                           XmFONTLIST_DEFAULT_TAG);
   }

   contour_levels_for_list[contour_levels.number_levels] = XmStringCreateLtoR ("Max Values",
                                                                               XmFONTLIST_DEFAULT_TAG);
    
   XtVaSetValues (contourColorsList,
                  XmNitems, contour_levels_for_list,
                  XmNitemCount, contour_levels.number_levels + 1,
                  NULL);
                   
   MT_free ((void *)contour_levels_for_list);
                    
   XtManageChild (contourColorsForm);

   DEBUG_TRACE_OUT printf( "Leaving ContourColorsPopupCallback\n" );   
}



/*
 * Select_Contour_Level_Callback -- This callback is invoked when the user 
 * highlights one of the contour levels in the widget used to select colors
 * for the contour levels.  This procedure causes the current color of that 
 * contour level to be activated.  All other colors are deactivated.  This 
 * procedure also sets the global value "currently_selected_contour_level"
 * so that various other procedures know which level is currently selected.  
 *
 * David Helzer -- 7/8/97
 */
 
void Select_Contour_Level_Callback (Widget w, XtPointer clientData, XtPointer callData)
{
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) callData;
   Boolean result;
   char *text;
   short count;
   short current_color;
   char current_color_string[MAX_LENGTH_STRING];

   DEBUG_TRACE_IN printf( "Entering Select_Contour_Level_Callback\n" );   

   /* 
    * Retrieve the character data from the compound string
    */ 
   if ((result = XmStringGetLtoR (cbs -> item, XmFONTLIST_DEFAULT_TAG, &text)) == TRUE) {

      /* Copy the string to the global string */
      strcpy (currently_selected_contour_level, text);

      /* 
       * Determine the current color of the selected contour level and
       * activate that color on the radio box of colors (deselect all others)
       */
       
      if (strcmp ("Max Values", text) == 0)
         current_color = max_contour_value_color;
      else
      {
         for (count = 0; count < contour_levels.number_levels; count ++) {
            if (contour_levels.rlevel[count] == atof (text)) 
               current_color = contour_levels.colors[count];
         }
      }

      switch (current_color) {
         case RESERVED_WHITE:    strcpy (current_color_string, "white");
                                 break;
         case RESERVED_BLACK:    strcpy (current_color_string, "black");
                                 break;
         case RESERVED_CYAN:     strcpy (current_color_string, "cyan");
                                 break;
         case RESERVED_MAGENTA:  strcpy (current_color_string, "magenta");
                                 break;
         case RESERVED_BLUE:     strcpy (current_color_string, "blue");
                                 break;
         case RESERVED_RED:      strcpy (current_color_string, "red");
                                 break;
         case RESERVED_YELLOW:   strcpy (current_color_string, "yellow");
                                 break;
         case RESERVED_GREEN:    strcpy (current_color_string, "green");
                                 break;
      }    

      for (count = 0; count < NUMBER_CONTOUR_COLORS; count ++) {
         if (strcmp (current_color_string, XtName(contourColorsSelectionButtons[count])) == 0) {
            XtVaSetValues (contourColorsSelectionButtons[count],
                           XmNset, 1,
                           NULL);
         }
         else 
            XtVaSetValues (contourColorsSelectionButtons[count],
                           XmNset, 0,
                           NULL);
      }
   }
   DEBUG_TRACE_OUT printf( "Leaving Select_Contour_Level_Callback\n" );   
}


/*
 * Select_Contour_Color_Callback() -- This callback is invoked when the user clicks 
 * on one of the color buttons in the widget used to choose color values for contour
 * levels.  This procedures checks to ensure that one of the contour levels has 
 * been highlighted and, if so, changes the color of that contour level.  
 *
 * David Helzer -- 7/8/97
 */

void Select_Contour_Color_Callback (Widget w, XtPointer clientData, XtPointer callData)
{
   XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
   short count;
   short new_color;

   DEBUG_TRACE_IN printf( "Entering Select_Contour_Color_Callback\n" );   

   if ((*cbs).set) {
   
      /*
       * Determine, based on the string of the button pressed, what color value
       * the contour level should receive
       */
       
       if (strcmp (XtName (w), "white") == 0)
          new_color = RESERVED_WHITE;
       else if (strcmp (XtName (w), "black") == 0)
          new_color = RESERVED_BLACK;
       else if (strcmp (XtName (w), "cyan") == 0)
          new_color = RESERVED_CYAN;
       else if (strcmp (XtName (w), "magenta") == 0)
          new_color = RESERVED_MAGENTA;
       else if (strcmp (XtName (w), "yellow") == 0)
          new_color = RESERVED_YELLOW;
       else if (strcmp (XtName (w), "blue") == 0)
          new_color = RESERVED_BLUE;
       else if (strcmp (XtName (w), "red") == 0)
          new_color = RESERVED_RED;
       else 
          new_color = RESERVED_GREEN;


      /* 
       * Determine the currently active contour level (if one exists) and
       * change its corresponding color to that selected
       */
       
       if (strcmp (currently_selected_contour_level, "Max Values") == 0)
          max_contour_value_color = new_color;
       else {
          for (count = 0; count < contour_levels.number_levels; count ++) {
             if (contour_levels.rlevel[count] == atof (currently_selected_contour_level)) 
                contour_levels.colors[count] = new_color;
          }
       }
    }
   DEBUG_TRACE_OUT printf( "Leaving Select_Contour_Color_Callback\n" );   
}



/*
 * Apply_Contour_Colors_Callback() -- This callback is invoked when the user
 * clicks on the "Apply" button of the widget used to choose color values
 * for the contour levels.  This procedure simply updates the image containing
 * the contour lines (if an image exists).
 *
 * David Helzer -- 7/8/97
 */
 
void Apply_Contour_Colors_Callback (void)
{  
   DEBUG_TRACE_IN printf( "Entering Apply_Contour_Color_Callback\n" );   

   /*
    * Update the contour levels legend and colorwash values
    */
   update_colorwash_values();
   update_legend();
   
   /*
    * If an image is currently displayed, update it with the new
    * contour levels
    */
   update_small_images(image_matrix.active_picture); 

   DEBUG_TRACE_OUT printf( "Leaving Apply_Contour_Color_Callback\n" );   
}



void AnalysisCallback(Widget w, XtPointer client, XtPointer call)
{
    int whichOne = (int) client;
 
    DEBUG_TRACE_IN printf( "Entering AnalysisCallback\n" );   

    switch (whichOne)
    {
        case 0 :
            /* do something wonderful */;
            break;
        case 1 :
            /* do something wonderful */;
            break;
        case 2 :
            /* do something wonderful */;
            break;
        default :
            printf( "Ooops! You fell through the case\n" );
            exit( 1 );
    }
    DEBUG_TRACE_OUT printf( "Leaving AnalysisCallback\n" );   
}


void EditParametersCallback(Widget w, XtPointer client, XtPointer call)
{
    /*SUPPRESS 594*/XmAnyCallbackStruct *acs=(XmAnyCallbackStruct*)call;

    DEBUG_TRACE_IN printf( "Entering EditParametersCallback\n" );
    DEBUG_TRACE_OUT printf( "Leaving EditParametersCallback\n" );   
}



void ExitCallback (Widget w, XtPointer client, XtPointer call)
{
   DEBUG_TRACE_IN printf( "Entering ExitCallback\n" );
   XtCloseDisplay( XtDisplay( w ) );

   /* Clean up any temparary files */
   /*remove_temp_contour_files ( );*/
   remove_temp_mask_files ( );
   
   exit( 0 );
}



void PopdownCallback(Widget w, XtPointer client, XtPointer call)
{
   Widget shell = (Widget) client;

   DEBUG_TRACE_IN printf( "Entering PopdownCallback\n" );

   /* pop the passed widget down */
   XtPopdown( shell );

   DEBUG_TRACE_OUT printf( "Leaving PopdownCallback\n" );
}


void PopupCallback(Widget w, XtPointer client, XtPointer call)
{
   Widget shell = (Widget) client;

   DEBUG_TRACE_IN printf( "Entering PopupCallback\n" );

   /* pop up the passed widget */
   XtPopup( shell, XtGrabNone );

   /* bring the dialog to the top (front) of the stacking order */
   XRaiseWindow( XtDisplay( shell ), XtWindow( shell ) );

   DEBUG_TRACE_OUT printf( "Leaving PopupCallback\n" );
}

/* ARGSUSED */
void FilePopupCallback(Widget w, XtPointer client, XtPointer call)
{
   extern Widget      fileSelectionBox;
   int whichOne = (int) client;
   extern Widget      fileSelectionDialogShell;

   DEBUG_TRACE_IN printf( "Entering FilePopupCallback\n" );

   XtRemoveAllCallbacks( fileSelectionBox, XmNokCallback );  /* remove old callbacks */
   XtRemoveAllCallbacks( fileSelectionBox, XmNcancelCallback );  /* remove old callbacks */
   XtAddCallback(fileSelectionBox, XmNcancelCallback, FilePopupCallback, (caddr_t) 3 );

   switch (whichOne)
   {
	case 0: 	/* load image */
	  /* XtVaSetValues(fileSelectionDialogShell, XmNtitle, "Select Image File", NULL);
	  /* XtAddCallback(fileSelectionBox, XmNokCallback, FileSelectionCallback, (caddr_t) 0 ); 
	   */
	   break;
	case 1:		/* load dose */
      	   XtVaSetValues(fileSelectionDialogShell, XmNtitle, "Select Dose File", NULL);
     	   XtAddCallback(fileSelectionBox, XmNokCallback, 
			 (XtCallbackProc)dose_fileCB, (caddr_t) 0 );
	   break;
	case 2:		/* load mask */
	   XtVaSetValues( fileSelectionDialogShell, XmNtitle, "Select Mask File", NULL);
	   XtAddCallback(fileSelectionBox, XmNokCallback, FileMaskCallback, (caddr_t) 0);
	   break;
        case 3:         /* cancel */
	   XtUnmanageChild( fileSelectionBox );
	   break;
	default:
	   printf("Should never. FilePopupCallback()\n");
	
   }
   if (whichOne!=3)
     XtManageChild( fileSelectionBox );

   DEBUG_TRACE_OUT printf( "Leaving FilePopupCallback\n" );
}


/* ARGSUSED */
void HelpMenuCallback(Widget w, XtPointer client, XtPointer call)
{
   /*SUPPRESS 594*/XmAnyCallbackStruct *acs=(XmAnyCallbackStruct*)call;

   DEBUG_TRACE_IN printf( "Entering HelpMenuCallback\n" );
   DEBUG_TRACE_OUT printf( "Leaving HelpMenuCallback\n" );
}

/* ARGSUSED */
void
ImageFormat(Widget w, XtPointer client, XtPointer call)
{
   /*SUPPRESS 594*/XmAnyCallbackStruct *acs=(XmAnyCallbackStruct*)call;

   DEBUG_TRACE_IN printf( "Entering ImageFormat\n" );
   DEBUG_TRACE_OUT printf( "Leaving ImageFormat\n" );
}

void DoseOptionCallback(Widget w, XtPointer client, XtPointer call)
{
   XmSelectionBoxCallbackStruct *sbcbs = (XmSelectionBoxCallbackStruct *) call;

   DEBUG_TRACE_IN printf( "Entering DoseOptionCallback\n" );

   XmStringGetLtoR(sbcbs->value, XmSTRING_DEFAULT_CHARSET, &doseString);

#if defined (MORE_VERBOSE)
   printf( "   doseString = \"%s\"\n", doseString );
#endif /* MORE_VERBOSE */

   if ( strcmp( doseString, "Boron Dose" )==0 )
       doseFlag = 0;
   else if ( strcmp( doseString, "Gamma Dose" )==0 )
       doseFlag = 1;
   else if ( strcmp( doseString, "Nitrogen Dose" )==0 )
       doseFlag = 2;
   else if ( strcmp( doseString, "Fast Dose" )==0 )
       doseFlag = 3;
   else if ( strcmp( doseString, "Group 1 Fluence" )==0 )
       doseFlag = 4;
   else if ( strcmp( doseString, "Group 2 Fluence" )==0 )
       doseFlag = 5;
   else if ( strcmp( doseString, "Thermal Fluence" )==0 )
       doseFlag = 6;
   else if ( strcmp( doseString, "Other Dose" )==0 )
       doseFlag = 7;
   else if ( strcmp( doseString, "Total Dose" )==0 )
       doseFlag = 8;
   else
   {
      error_popup ("Internal error - bad dose type in DoseOptionCallback");
   }
/*
       printf( "%s:  what's going on here.  the dose should have matched\n" ,doseString);
*/

   update_small_images(image_matrix.active_picture);

   DEBUG_TRACE_OUT printf( "Leaving DoseOptionCallback\n" );
}


void colorCB(Widget w, XtPointer client, XtPointer call)
{
  Arg wargs[10];
  int whichOne = (int) client;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) call;

  DEBUG_TRACE_IN printf( "Leaving colorCB\n" );

  if (cbs->reason == XmCR_VALUE_CHANGED || cbs->reason == XmCR_DRAG)
  {
      switch(whichOne)
      {
      case DoRed:
	contour_color.red = (cbs->value << 8);
	break;
      case DoGreen:
	contour_color.green = (cbs->value << 8);
	break;
      case DoBlue:
	contour_color.blue = (cbs->value << 8);
      }

   myXStoreColor(di,get_color_info()->cmap,&contour_color);

   XtVaSetValues(editColorDrawnButton,    /* mwf 7-14-95 -> probably not necessary */
       XmNbackground, get_color_info()->truecolors[contour_color.pixel],
       NULL);
  }
  DEBUG_TRACE_IN printf( "Leaving colorCB\n" );
}


void dose_fileCB(Widget w, int select, XmAnyCallbackStruct *c_d)
{
    float z_value;
    XmFileSelectionBoxCallbackStruct *s =
	(XmFileSelectionBoxCallbackStruct *) c_d;

    DEBUG_TRACE_IN printf( "Entering dose_fileCB\n" );

    XtUnmanageChild( fileSelectionBox );

    if (select == OK)
    {
        if (dose_file_name != NULL)
        {
            XtFree((char *)dose_file_name);
            dose_file_name = NULL;
        }
        XmStringGetLtoR(s->value, XmSTRING_DEFAULT_CHARSET, &dose_file_name);
        if ((dose_file = fopen(dose_file_name,"r")) == NULL)
        {
            printf ("Unable to open file!! %s\n", dose_file_name);
            XBell(di,100);
        }
        else
        {   
            fclose(dose_file);
            /* Check to see if file name is "legal" */
            if (!is_dose_file(dose_file_name)) {
                if (!confirm_popup("The file you have selected does not appear to be a valid dose file.\nA valid dose file typically ends in ',contour'.\nAre you sure you want to try to load this file?\n")) {
                    XtManageChild( fileSelectionBox );
                    DEBUG_TRACE_OUT printf( "Leaving dose_fileCB\n" );
                    return;
                }
            }
#if defined (MORE_VERBOSE)
            printf("dose file name: %s next\n",dose_file_name);
#endif /* MORE_VERBOSE */
            load_dose(dose_file_name, &dose_data, NULL, &z_value);
            dosage_is_there = TRUE;
            addto_dosage_list(dose_file_name, image_matrix.active_picture, z_value);
            contours_are_current = FALSE;
            load_imageEH(mainWindowDrawingArea,0,&SureEvent);
        }
    }
    else if (select == CLEAR)
    {
        dosage_is_there = FALSE;
        load_imageEH(mainWindowDrawingArea,0,&SureEvent);
    }

    DEBUG_TRACE_OUT printf( "Leaving dose_fileCB\n" );
}



void ContourSmoothCallback (Widget w, int select,
                            XmScaleCallbackStruct *c_d)
{
   DEBUG_TRACE_IN printf( "Entering ContoursSmoothCallback\n" );

   if (c_d->reason == XmCR_VALUE_CHANGED )
   {
      smoothingValue = (float)c_d->value / 100.0;

      #if defined (MORE_VERBOSE)
         printf( "   smoothingValue = \"%f\"\n", smoothingValue );
      #endif  /* MORE_VERBOSE */

      update_small_images(image_matrix.active_picture);
   }

   DEBUG_TRACE_OUT printf( "Leaving ContoursSmoothCallback\n" );
}



/* 
 * clickCB() - handles mouse clicks in the drawing area
 */
 
void clickCB(Widget w, caddr_t client_data, XEvent *event)
{
   XButtonEvent *bevent = (XButtonEvent *)event;

   DEBUG_TRACE_IN printf( "Entering clickCB\n" );

   if (XmToggleButtonGadgetGetState(locaterButton))
   {
      if (bevent -> type == ButtonPress)
      {
         measure_data . mode = True;
         measure_data . firstx = bevent -> x;
         measure_data . firsty = bevent -> y;
         measure_data . lastx = bevent -> x;
         measure_data . lasty = bevent -> y;
      }
      
      if (bevent -> type == ButtonRelease)
      {
         MeasureDrawLine (w, measure_data . firstx, measure_data . firsty,
                          measure_data . lastx, measure_data . lasty);
         measure_data . mode = False;
      }
   }

   DEBUG_TRACE_OUT printf( "Leaving clickCB\n" );
}



/* 
 * unclickCB() - Handles the disarm or button release in the image area.
 */
void unclickCB (Widget w, caddr_t client_data, XEvent *event)
{
   DEBUG_TRACE_IN printf( "Entering unclickCB\n" );

   if (! measure_data . mode)
   {
      DEBUG_TRACE_OUT printf( "Leaving unclickCB\n" );
      return;
   }

   /*
    * Draw the line to remove it from the screen and leave measure mode.
    */

   MeasureDrawLine (w, measure_data . firstx, measure_data . firsty,
         measure_data . lastx, measure_data . lasty);

   measure_data . mode = False;

   DEBUG_TRACE_OUT printf( "Leaving unclickCB\n" );
   return;
}



void update_locater (Widget w, caddr_t client_data, XEvent *event)
{
  int x, y;
  float DistancePerPixel = measure_data.fov / 512;
  float xmm, ymm,dose_val;
  Dimension win_size_x,win_size_y;
  float length, angle;
  Arg wargs[5];
  char str[256];
  XmString xmstr;

  /*
   * Make sure that there is an active picture too.
   * Otherwise we will get a core dump becuase the index
   * into img_arr will be out of range. mbr -> 12-09-98
   */

  if(XmToggleButtonGadgetGetState(locaterButton) && image_matrix.active_picture < MAX_PICS )
  {
    y = event->xmotion.y;
    x = event->xmotion.x;
    /*
     * Handle for measure mode or not.  Locate string is a simple location,
     * while measure provides the scaled length and angle of the line.
     */

    /*
     * Axis ( made consistent with seraModel ) 7-06-99 MBR
     *           |x +
     *           |
     *           |
     *    -------|-------
     *    y -    |     y +
     *           |
     *           |x -
     */

    if (! measure_data . mode)
    {
      ymm = (x - 256) * DistancePerPixel;
      xmm = (256 - y) * DistancePerPixel;

      if (image_matrix.img_arr[image_matrix.active_picture].contours_in_use){
	dose_val = get_dose_value_under_mouse(x,y,512,512);
	sprintf(str," X:%5.1f  Y:%5.1f  D: %5.1f", xmm, ymm,dose_val);
      }else
	sprintf(str," X:%5.1f  Y:%5.1f", xmm, ymm);
    }
    else
    {
      MeasureLineDrag (w, x, y, &length, &angle);
      length = length * DistancePerPixel;
      sprintf(str," L:%.1f  A:%.2f",length, angle);
    }
   
  }
  else
  {
    strcpy(str,"                      ");
  }
  
  xmstr = XmStringCreateLtoR (str, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues (locaterLabel, XmNlabelString, xmstr, NULL);	 
  XmStringFree (xmstr);  

}



/* =======================================================> ReportToggleArmCB
 *
 * When the Locate toggle button is armed, set the measure mode to
 * false to prevent premature measuring, which cannot take place before
 * a mouse button is pressed.
 *
 * ===========================================================================
 */

void ReportToggleArmCB (Widget w, char *client_data, 
                        XmToggleButtonCallbackStruct *call_data)
{
   DEBUG_TRACE_IN printf( "Entering ReportToggleArmCB\n" );

   measure_data . mode = False;
   if (measure_data . fov == 0)
   {
	/* need to get a FOV from the user */
        FOVchangeCallback(XtParent(w), client_data, (XmAnyCallbackStruct *)
			  call_data);
   }
   if (!crosshairCursor)
   	crosshairCursor = XCreateFontCursor(di, XC_tcross);
   if (!XmToggleButtonGadgetGetState(locaterButton))
	XDefineCursor(di, XtWindow(mainWindowDrawingArea), crosshairCursor);
   else
	XUndefineCursor(di, XtWindow(mainWindowDrawingArea));

   DEBUG_TRACE_OUT printf( "Leaving ReportToggleArmCB\n" );
   return;
}



void FOVchangeCallback(Widget w, char *client_data, XmAnyCallbackStruct *call_data)
{
    Arg    	args[20];
    Boolean   	argok;
    Cardinal   	argcnt;
    char 	slice_label[80];
    Widget 	newshell, mainform, label, ok_button;

   DEBUG_TRACE_IN printf( "Entering FOVchangeCallback\n" );

    if (fovDisplayed == TRUE)
    {
        DEBUG_TRACE_OUT printf( "Leaving FOVchangeCallback\n" );
	return;  /* already an instance of fov widget */
    }
    else
	fovDisplayed = TRUE;

    newshell = XtCreatePopupShell("Change Field Of View",
		topLevelShellWidgetClass,
		w,
		NULL,
		0);

    /* form container widget */
    mainform = XtVaCreateManagedWidget("mainform", xmFormWidgetClass,
		newshell, NULL, 0);
    	

    label = XtVaCreateManagedWidget("Enter FOV (in mm):", 
		xmLabelGadgetClass, mainform,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNalignment, XmALIGNMENT_BEGINNING,
		NULL);

    if (measure_data.fov == 0)
	sprintf(slice_label, "%.2f", 250.0);
    else
    	sprintf(slice_label, "%.2f", measure_data.fov);

    image_info = XtVaCreateManagedWidget("image info 1", 
		xmTextWidgetClass, mainform,
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, label,
		XmNcolumns, 5,
		XmNvalue, slice_label,
		NULL);

    ok_button = XtVaCreateManagedWidget("OK",
		xmPushButtonWidgetClass, mainform,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);

    XtAddCallback(ok_button, XmNactivateCallback, 
		  (XtCallbackProc)newFOVCB, (Widget) newshell);
    XtAddCallback(image_info, XmNactivateCallback, 
		  (XtCallbackProc)newFOVCB, (Widget) newshell);

    XtVaSetValues(label, XmNbottomAttachment, XmATTACH_WIDGET,
			 XmNbottomWidget, ok_button,
			 NULL);

    XtVaSetValues(image_info, XmNbottomAttachment, XmATTACH_WIDGET,
			 XmNbottomWidget, ok_button,
			 NULL);

    XtPopup(newshell, XtGrabNone);

    DEBUG_TRACE_OUT printf( "Leaving FOVchangeCallback\n" );
}



void newFOVCB(Widget w, Widget parent_shell, XtPointer call)
{
   char *argv[10];
   int argc = 9;
   char sxdim[10], sydim[10], send[10], sstart[10];
   char *s;

   DEBUG_TRACE_IN printf( "Entering newFOVCB\n" );

   s = XmTextGetString(image_info);
   sscanf(s, "%f", &measure_data.fov); 
   XtFree((char *)s);

   /* sanity checks */
   if (measure_data.fov <= 0) measure_data.fov = 250;

   fovDisplayed = FALSE;
 
   XtDestroyWidget(parent_shell);

   DEBUG_TRACE_OUT printf( "Leaving newFOVCB\n" );
}   



void rawImageFileLoaderCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  static Widget input_dialog = NULL;

  DEBUG_TRACE_IN printf( "Entering rawImageFileLoaderCB\n" );

  if ( !input_dialog ) {
    input_dialog = 
      XmCreateFileSelectionDialog 
      ( w, "input_dialog",
	NULL, 0 );
    XtVaSetValues( XtParent(input_dialog), XmNtitle, "Select Image File", NULL);

    make_colormap_window_children(input_dialog, get_color_info()->cmap);
  }

  XtRemoveAllCallbacks( input_dialog, XmNokCallback );      /* remove old callbacks */
  XtRemoveAllCallbacks( input_dialog, XmNcancelCallback );  /* remove old callbacks */

  switch ((int)client_data)
    {
    case 0: 	        /* load image */
      XtVaSetValues(input_dialog,
		    XmNcancelLabelString, XmStringCreateLtoR("Cancel", char_set),
		    XmNokLabelString, XmStringCreateLtoR("OK", char_set),
		    NULL);
      XtAddCallback ( input_dialog, XmNokCallback,
		      rawImageFileLoaderDoneCB, NULL );
      XtAddCallback ( input_dialog, XmNcancelCallback,
		      rawImageFileLoaderDoneCB, (XtPointer) 1 );
      XtManageChild ( input_dialog );
      break;
    case 1:            /* Smart Load Images */
      XtVaSetValues(input_dialog,
		    XmNcancelLabelString, XmStringCreateLtoR("Done", char_set),
		    XmNokLabelString, XmStringCreateLtoR("Add", char_set),
		    NULL);
      XtAddCallback ( input_dialog, XmNcancelCallback,
		      smartLoaderDoneCB, (XtPointer) 1 );
      XtAddCallback( input_dialog, XmNokCallback, smartLoaderDoneCB, (caddr_t) 0 ); 
      XtManageChild ( input_dialog );
      break;
    default:
      printf("Bad case.\n");
    }

  DEBUG_TRACE_OUT printf( "Leaving rawImageFileLoaderCB\n" );
}


void rawImageFileLoaderDoneCB(Widget w, XtPointer clientData, XtPointer callData) 
{
    XmFileSelectionBoxCallbackStruct *cbs = 
                         ( XmFileSelectionBoxCallbackStruct * ) callData;
    char *fileName;
    int whichcase, ival;
    static int i;

    DEBUG_TRACE_IN printf("Entering rawImageFileLoaderDoneCB\n");

    whichcase = (int) clientData;

    ival = image_matrix.num_pics; /* num_pics specifies the index of the next picture
				   * slot we can use up
				   */
    switch (whichcase)
    {      
      case 0: /* for a single file */
	/*
	 * Retrieve the character string from the compound string format.
	 */
	
	XmStringGetLtoR ( cbs->value, XmFONTLIST_DEFAULT_TAG, &fileName );
	if (!(image_matrix.img_arr[ival].fname_data=(char*)MT_malloc(strlen(fileName)+1))) {
	  printf("Malloc error.\n");
	  exit(13);
	}
	strcpy(image_matrix.img_arr[ival].fname_data, fileName);

	if (image_matrix.img_arr[ival].raw_data=read_raw(fileName, &qhd_data, ival)) {

	  qsh_PIC_init(ival);
	  toggle_on(ival, 1);
          highlight_active_picture ( );
          
	  /* For first successful image, turn on some things */
	  if (ival==0) {
	    char fsb_path[256]="\0";
	    char *tmp_ptr;
	    XmString tmp_string;
	    
	    /* Do some special initializations for first picture loaded */
	    XtVaSetValues(loadDosePushButton,
			  XmNsensitive, True,
			  NULL);
	    XtVaSetValues(loadMaskPushButton,
			  XmNsensitive, True,
			  NULL);

	    /* set up the path for dose and mask to the current path */
	    strcpy(fsb_path, fileName);
	    if (tmp_ptr = strrchr(fsb_path, '/'))
	      tmp_ptr[1]='\0'; /* terminate before the file name */
	    tmp_string = XmStringCreateLtoR(fsb_path, char_set);
	    XtVaSetValues(fileSelectionBox,
			  XmNdirectory, tmp_string,
			  NULL);
	    XmStringFree(tmp_string);
	  }
	} else {
	  MT_free((void*)image_matrix.img_arr[ival].fname_data);
	  image_matrix.img_arr[ival].fname_data=NULL;
	  return;
	}
	/* Set the active picture to red since PIC_init occurs every time
	 * and keeps setting them all to black
	 */
	XtVaSetValues ( image_matrix.img_arr[ival].draw_area, 
			XtVaTypedArg, XmNborderColor, XtRString, "red", 4,
			NULL );
	XtUnmanageChild(w);
	break;
      case 1:
	XtUnmanageChild(w);
	break;
      case 2: /* destroy (unload) ALL images (and masks and contours) */
	/*
	 * Retrieve the character string from the compound string format.
	 */
	
	destroypix();
	XtVaSetValues(loadDosePushButton,
		      XmNsensitive, False,
		      NULL);
	XtVaSetValues(loadMaskPushButton,
		      XmNsensitive, False,
		      NULL);

	/* Turn off the multiple load dose and mask buttons */
        /* MTC 7/7/98 */
	XtVaSetValues (qshDoseLoadCascadeButton, XmNsensitive, FALSE, NULL);
	XtVaSetValues (qshMaskLoadCascadeButton, XmNsensitive, FALSE, NULL);
	XtVaSetValues (qshRemoveImagesWODoseButton, XmNsensitive, FALSE, NULL);
	
	/* Remove any possible contour labelling */
	labelContours("");

        /* Free qsh info structure and set to NULL */
        if ( qsh_info )
        {
            MT_free ((void *)qsh_info->images);
            MT_free ((void *)qsh_info);
            qsh_info = NULL;
        }
        
        break;
      }
        
    DEBUG_TRACE_OUT printf("Leaving rawImageFileLoaderDoneCB\n");
}


void smartLoaderDoneCB(Widget w, XtPointer clientData, XtPointer callData) 
{
    XmFileSelectionBoxCallbackStruct *cbs = 
        ( XmFileSelectionBoxCallbackStruct * ) callData;
    static int first_call=1;
    static Widget image_loader_widget;
    char *fileName;
    int whichcase, ival;
    FILE *mask_file;
    static int i,jj,kk,k,found;
    static char tmp_filename[256];
    static char dose_path[256]="\0";
    static char mask_path[256]="\0";
    static char dose_ext[256]=",contour\0";
    static char mask_ext[256]=",mask\0";
    static char stripname[256]="\0";
    static char *ch_ptr;
    int please_draw;
    Boolean masks_replace_images;
    float z_value;
  
    DEBUG_TRACE_IN printf("Entering smartLoaderDoneCB\n");

    XtVaGetValues(masksReplaceImagesButton,
                  XmNset, &masks_replace_images,
                  NULL);

    
    whichcase = (int) clientData;


    if ( first_call )
    {
        image_loader_widget = w;
        first_call=0;
    }

    /* num_pics specifies the index of the next picture
     * slot we can use up */
    ival = image_matrix.num_pics; 

    switch (whichcase)
    {
        /* add */
        case 0: /* Done every time a raw file is selected */
            /* Don't allow the user to toggle between images while smartLoader is running */
            image_matrix.toggle_enable = 0;
      
            /* Call the regular rawImageFileLoaderDoneCB */
            rawImageFileLoaderDoneCB(w, clientData, callData);
      
            /* Check for success; if not, the filename will be NULL */
            if (image_matrix.img_arr[ival].fname_data==NULL)
                break;
 
            image_matrix.active_picture = 0;
            
            /* load the contours */
            XtRemoveAllCallbacks( fileSelectionBox, XmNokCallback ); /* remove old callbacks */
            XtRemoveAllCallbacks( fileSelectionBox, XmNcancelCallback ); /* remove old callbacks */
            XtVaSetValues(fileSelectionDialogShell, XmNtitle, "Select Dose File", NULL);
            XtAddCallback(fileSelectionBox, XmNokCallback, smartLoaderDoneCB, (caddr_t) 3 );
            XtAddCallback(fileSelectionBox, XmNcancelCallback, smartLoaderDoneCB, (caddr_t) 4 );
            XtManageChild( fileSelectionBox );
                
            /* load the mask and other cleanup handled by the above callback */
            toggle_on ( ival, 0 );
            highlight_active_picture ( );

            break;
            /* done */
        case 1: /* Chose "cancel=rewritten as done" --> indicates done loading images */
            /* Allow toggling between images again */
            image_matrix.toggle_enable = 1;
            /* Get rid of the file selection widget */
            XtUnmanageChild(w);
            break;
        case 2: /* destroy (unload) ALL images (and masks and contours) */
            /* Actually, this is never called.  See, instead, case 2 of rawImageFileLoaderDoneCB */
            break;
        case 3:		/* load dose --> falls through to 4 for mask selection */
            dose_fileCB(w, OK, callData); /* mwf simulates a call to dose_fileCB */
            /* keep retrying until a dose does get loaded */
            if (!image_matrix.img_arr[ival-1].contours_in_use)
            {
                XtManageChild( fileSelectionBox );
                break;
            } /* else we fall through and then load a mask */
        case 4:           /* set things up to load a mask --> file selection widget */
            XtRemoveAllCallbacks( fileSelectionBox, XmNokCallback ); /* remove old callbacks */
            XtRemoveAllCallbacks( fileSelectionBox, XmNcancelCallback ); /* remove old callbacks */
            XtVaSetValues(fileSelectionDialogShell, XmNtitle, "Select Mask File", NULL);
            XtAddCallback(fileSelectionBox, XmNokCallback, smartLoaderDoneCB, (caddr_t) 5 );
            XtAddCallback(fileSelectionBox, XmNcancelCallback, smartLoaderDoneCB, (caddr_t) 6 );
            XtManageChild( fileSelectionBox );
            break;
        case 5:           /* load mask --> falls through to 6 */
            FileMaskCallback(w, (caddr_t) 0, callData); /* mwf simulates a call to FileMaskCallback */
            /* keep retrying until a mask does get loaded */
            if (!image_matrix.img_arr[ival-1].mask_in_use)
            {
                XtManageChild( fileSelectionBox );
                break;
            } /* else we fall through and then load a mask */
        case 6:           /* potential mask and dose are loaded -- store paths, etc. */
            /* Be sure the mask file selection window goes away */
            XtUnmanageChild( fileSelectionBox );
            /* remanage the image loader since more files will probably be selected */
            XtManageChild(image_loader_widget);
      
            break;
    }

    DEBUG_TRACE_OUT printf("Leaving smartLoaderDoneCB\n");
}



void masksReplaceImagesCallback (Widget w, XtPointer client, XmAnyCallbackStruct *cbs)
{
   DEBUG_TRACE_IN printf("Entering masksReplaceImagesCallback\n");

   if (image_matrix.active_picture < XtNumber(image_matrix.img_arr))
       update_small_images(image_matrix.active_picture);

   DEBUG_TRACE_OUT printf("Leaving masksReplaceImagesCallback\n");
}



void Confirm_dialog_CB (Widget w, XtPointer client_data, XmAnyCallbackStruct *call_data)
{
   DEBUG_TRACE_IN printf("Entering Confirm_dialog_CB\n");

   if (call_data->reason == XmCR_OK) 
      CONFIRM = OK;
   else
      CONFIRM = CANCEL;

   /* make the dialog box invisible */
   XtUnmanageChild(w);

   DEBUG_TRACE_OUT printf("Leaving Confirm_dialog_CB\n");
}



void view_contour_lines_callback (Widget w, XtPointer clientData, XtPointer callData)
{
    XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
    Boolean reveal_legend;
   
    DEBUG_TRACE_IN printf("Entering view_contour_lines_callback\n"); 

    /* If view contour lines */
    if ((*cbs).set)
    {
        set_gamma_colormap ( );
        
        XtVaSetValues (view_contour_colorwash_button,
                       XmNset, FALSE,
                       NULL);
                     
        /* Masks Replace Images is re-enabled */
        XtVaSetValues (masksReplaceImagesButton,
                       XmNsensitive, TRUE, 
                       NULL);

        /* commented this out because I removed this button */               
        /* MTC 5/24/99                                      */
        /*XtVaSetValues (generate_colorwash_file_button,
          XmNsensitive, FALSE,
          NULL);*/

        XtVaGetValues (reveal_legend_button,
                       XmNset, &reveal_legend,
                       NULL);
                     
        if (reveal_legend) 
            update_legend();
                
        update_small_images (image_matrix.active_picture); 
    }
    else 
        XtVaSetValues (view_contour_lines_button,
                       XmNset, TRUE,
                       NULL);

    DEBUG_TRACE_OUT printf("Leaving view_contour_lines_callback\n"); 
}



void view_contour_colorwash_callback (Widget w, XtPointer clientData, XtPointer callData)
{
    XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
    Boolean reveal_legend;
   
    DEBUG_TRACE_IN printf("Entering view_contour_colorwash_callback\n"); 

    /* If view colorwash */
    if ((*cbs).set)
    {
        set_colorwash_colormap ( );
       
        XtVaSetValues (view_contour_lines_button,
                       XmNset, FALSE,
                       NULL);
                     
        /* "Masks Replace Images" is disabled while colorwash is viewed */
        XtVaSetValues (masksReplaceImagesButton,
                       XmNsensitive, FALSE, 
                       NULL);
   
        XtVaSetValues (masksReplaceImagesButton,
                       XmNset, FALSE,
                       NULL);
                     
        /* commented this out because I removed this button */
        /* MTC 5/24/99                                      */
        /*XtVaSetValues (generate_colorwash_file_button,
          XmNsensitive, TRUE,
          NULL);*/

        XtVaGetValues (reveal_legend_button,
                       XmNset, &reveal_legend,
                       NULL);

        if (reveal_legend) 
            update_legend();
               
        update_small_images (image_matrix.active_picture);
    }
    else
    {
        XtVaSetValues (view_contour_colorwash_button, 
                       XmNset, TRUE,
                       NULL);
    }
   
    DEBUG_TRACE_OUT printf("Leaving view_contour_colorwash_callback\n"); 
}



/* 
 * reveal_legend_callback() - A callback that is invoked when the user 
 * clicks on the "Reveal Legend" button.  If the button is "set," the
 * appropriate legend is updated and managed.  If the button is "unset,"
 * the legend is unmanaged.
 *
 * David Helzer 7/97
 */
 
void reveal_legend_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
   
   DEBUG_TRACE_IN printf("Entering reveal_legend_callback\n"); 

   if ((*cbs).set) 
      update_legend();
   else
      XtUnmanageChild (contour_legend_widget);

   DEBUG_TRACE_OUT printf("Leaving reveal_legend_callback\n"); 
}


/* 
 * reveal_info_callback() - A callback that is invoked when the user 
 * clicks on the "Reveal Info" button.  If the button is "set," the
 * information panel managed.  If the button is "unset,"
 * the panel is unmanaged.
 *
 * Matt Cohen 8/20/99
 */
 
void reveal_info_callback ( Widget w, XtPointer clientData, XtPointer callData )
{
   XmToggleButtonCallbackStruct *cbs = ( XmToggleButtonCallbackStruct * ) callData;
   
   DEBUG_TRACE_IN printf("Entering reveal_info_callback\n"); 

   if ( cbs->set ) 
      XtManageChild ( contour_info_widget );
   else
      XtUnmanageChild ( contour_info_widget );

   DEBUG_TRACE_OUT printf("Leaving reveal_legend_callback\n"); 
}



/*
 * contour_labels_options_callback() - A callback that is invoked
 * when the user selects the "View Contour Labels" button.  A
 * popup widget is managed that displays the options for the various 
 * contour labels.  
 *
 * David Helzer 8/97
 */
 
void contour_labels_options_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   extern Widget contour_labels_options_form;
   
   DEBUG_TRACE_IN printf("Entering contour_labels_options_callback\n"); 
   
   XtVaSetValues (large_image_labels_scale,
                  XmNvalue, large_image_label_size,
                  NULL);
                  
   XtVaSetValues (preview_image_labels_scale,
                  XmNvalue, preview_image_label_size,
                  NULL);
   
   XtManageChild (contour_labels_options_form);

   DEBUG_TRACE_OUT printf("Leaving contour_labels_options_callback\n"); 
}


void large_labels_scale_valchg_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;

   DEBUG_TRACE_IN printf("Entering large_labels_scale_valchg_callback\n");   
   large_image_label_size = (*cbs).value;
   DEBUG_TRACE_OUT printf("Leaving large_labels_scale_valchg_callback\n"); 
}


void preview_labels_scale_valchg_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;

   DEBUG_TRACE_IN printf("Entering preview_labels_scale_valchg_callback\n");    
   preview_image_label_size = (*cbs).value;
   DEBUG_TRACE_OUT printf("Leaving preview_labels_scale_valchg_callback\n"); 
}


void update_label_size_callback (Widget w, XtPointer clientData, XtPointer callData)
{
   DEBUG_TRACE_IN printf("Entering update_label_size_callback\n"); 
   update_small_images (image_matrix.active_picture);
   DEBUG_TRACE_OUT printf("Leaving update_label_size_callback\n"); 
}



void checkVersionCallback (Widget w, XtPointer clientData, XtPointer callData)
{
    DEBUG_TRACE_IN printf("Entering checkVersionCallback\n"); 
    CT_check_version (w, "seraDose");
    DEBUG_TRACE_OUT printf("Leaveing checkVersionCallback\n"); 
}


void launchAppCallback(Widget w, XtPointer clientdata,XtPointer calldata)
{
    DEBUG_TRACE_IN printf( "Entering launchAppCallback\n" );

    if (strstr(XtName(w),"Sera"))
        system("../../Bnct3D/sera3d &");
    else if (strstr(XtName(w),"Image"))
        system("../../ImageTools/Editor/image_tools &");

    DEBUG_TRACE_OUT printf( "Leaving launchAppCallback\n" );
}
