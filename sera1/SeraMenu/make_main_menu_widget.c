/******************************************************************************/
/*  Function Name:  make_main_menu_widget()                                   */
/*                                                                            */
/*  Input:   None                                                             */
/*  Output:  None                                                             */
/*                                                                            */
/*  Return:  None                                                             */
/*                                                                            */
/*  Description:                                                              */
/*     This function creates the menu widget.  This includes creating a main  */
/*  menu bar and all pulldown submenus necessary to run bnct_edit.  Once the  */
/*  menu bar is attached to the is created and attached to the form, the      */
/*  function make_menu() is called to create the submenus of the menu bar.    */
/******************************************************************************/

#include "seramenu.h"

#define MENU_LABEL "Center for Advanced\nRadiation Therapies\n\nBNCT Simulation Environment\nfor\nRadiotherapy Applications"


void make_main_menu_widget ( main_gui_t * gui )
{

   DEBUG_TRACE_IN  printf("Entering make_main_menu_widget\n");

   /* 
    * Create a form widget for the main menu.
    * This form widget is a child of row_column (global)
    * and in turn a grandchild of toplevel which were created
    * by intialize_top_level() in file m_tools.c
    */

   gui->containers.main_frame = 
     XtVaCreateManagedWidget("main_frame",
			     xmFrameWidgetClass, gui->containers.top_level_row_column,
			     XmNshadowType, XmSHADOW_ETCHED_IN,
			     XmNshadowThickness, 8,
			     NULL);
                                   

   gui->containers.main_form = 
     XtVaCreateManagedWidget("main_menu", xmFormWidgetClass,
			     gui->containers.main_frame,
			     XmNhorizontalSpacing, 3,
			     NULL);


   gui->containers.logo = (Widget) InstallLogo (gui->containers.main_form);


   gui->containers.label_frame = 
     XtVaCreateManagedWidget ("frame",
			      xmFrameWidgetClass, gui->containers.main_form,
			      XmNshadowType, XmSHADOW_ETCHED_IN,
			      XmNshadowThickness, 1,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, gui->containers.logo,
			      XmNrightAttachment, XmATTACH_FORM,
			      XmNleftAttachment, XmATTACH_FORM,
			      NULL);

   gui->labels.menu_label = 
     XtVaCreateManagedWidget ("menu_label",
			      xmLabelWidgetClass, gui->containers.label_frame,
			      XtVaTypedArg, XmNlabelString, XmRString,
			      MENU_LABEL, strlen( MENU_LABEL ) + 1,
			      NULL);


   gui->containers.separators[0] = 
     XtVaCreateManagedWidget ("separator",
			      xmSeparatorWidgetClass, gui->containers.main_form,
			      XmNseparatorType, XmDOUBLE_LINE,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, gui->containers.label_frame,
			      XmNrightAttachment, XmATTACH_FORM,
			      XmNleftAttachment, XmATTACH_FORM,
			      NULL);


   /*  Create the buttons on the main menu*/
   create_main_menu( gui ); 

   DEBUG_TRACE_OUT("Leaving make_main_menu_widget\n");
}
