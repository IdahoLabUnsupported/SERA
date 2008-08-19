#include "sera3d.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_colorwash_levels_popup(main_gui_t *gui)
{
  gui->colorwash_level_popup.shell = XtCreatePopupShell("Enter Levels Wanted",
							topLevelShellWidgetClass,gui->toplevel,
							NULL,0);

  XtPopdown(gui->colorwash_level_popup.shell);
  gui->colorwash_level_popup.form = XtVaCreateManagedWidget("form",
						       xmFormWidgetClass, gui->colorwash_level_popup.shell,
						       NULL);
  gui->colorwash_level_popup.text_box = XtVaCreateManagedWidget("text box",
							   xmTextWidgetClass,gui->colorwash_level_popup.form,
							   XmNeditMode, XmMULTI_LINE_EDIT,
							   XmNtopAttachment, XmATTACH_FORM,
							   XmNleftAttachment, XmATTACH_FORM,
							   XmNrightAttachment, XmATTACH_FORM,
							   XmNbottomAttachment, XmATTACH_NONE,
							   XmNheight, 250,
							   XmNcolumns,3,
							   NULL);
  gui->colorwash_level_popup.apply_button = XtVaCreateManagedWidget("Apply",
							       xmPushButtonWidgetClass, gui->colorwash_level_popup.form,
							       XmNtopAttachment, XmATTACH_WIDGET,
							       XmNtopWidget, gui->colorwash_level_popup.text_box,
							       XmNtopOffset, 5,
							       XmNrightAttachment, XmATTACH_FORM,
							       XmNrightOffset, 5,
							       XmNbottomAttachment, XmATTACH_FORM,
							       XmNbottomOffset, 5,
							       NULL);
  XtAddCallback(gui->colorwash_level_popup.apply_button, XmNactivateCallback,
		Colorwash_Levels_Popup_ApplyCB,(XtPointer)gui);
  
  gui->colorwash_level_popup.cancel_button = XtVaCreateManagedWidget("Cancel",
								xmPushButtonWidgetClass, gui->colorwash_level_popup.form,
								XmNtopAttachment, XmATTACH_WIDGET,
								XmNtopWidget, gui->colorwash_level_popup.text_box,
								XmNtopOffset, 5,
								XmNrightAttachment, XmATTACH_WIDGET,
								XmNrightWidget, gui->colorwash_level_popup.apply_button,
								XmNrightOffset, 5,
								XmNbottomAttachment, XmATTACH_FORM,
								XmNbottomOffset, 5,
								NULL);
  XtAddCallback(gui->colorwash_level_popup.cancel_button, XmNactivateCallback,
		Colorwash_Levels_Popup_CancelCB,(XtPointer)gui);
}

void Colorwash_Levels_Popup_CancelCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  /*colorwash_level_popup_t *clp = (colorwash_level_popup_t *)clientdata;*/

  XtPopdown(gui->colorwash_level_popup.shell);
}
void Colorwash_Levels_Popup_ApplyCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  /*colorwash_level_popup_t *clp = (colorwash_level_popup_t *)clientdata;*/

  rebuild_contour_colorwash_legend(gui);
  
  XtPopdown(gui->colorwash_level_popup.shell);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Show_Colorwash_Levels_PopupCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;

  XtPopup(gui->colorwash_level_popup.shell,XtGrabNone);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Colorwash_Legend_Color_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XColor new_color;
  
  if (!Select_Color(gui,&new_color)) return;

  XtVaSetValues(w,XmNbackground,new_color.pixel,NULL);

  /*printf("the color was : red : %d, green : %d, blue : %d\n",new_color.red,new_color.green,new_color.blue);*/
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Apply_Colorwash_Legend_ColorsCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  DEBUG_TRACE_IN printf("Entered Apply_Colorwash_Legend_ColorsCB\n");


  if (!gui->images_loaded){
    DT_error(gui->toplevel,"You must first load an image set",NULL,NULL);
    return;
  }

  DisplayBusyCursor(gui->mainwindow);
  rebuild_contour_colormap(gui);
  rebuild_3d_texture_from_texture_volume(gui);
  draw_all(gui);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Apply_Colorwash_Legend_ColorsCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void rebuild_contour_colorwash_legend(main_gui_t *gui)
{
    int i;
    char c;
    int temp_int;
    int valid_levels;
    char temp_string[256];
    char *level_string,*ptr;
    
    level_string = XmTextGetString(gui->colorwash_level_popup.text_box);
    
    if (strlen(level_string)<2) return;  /* nothing entered */
    
    ptr = level_string;
    while( !isdigit( (int) *ptr ) && *ptr != '\0' )
        ptr++;
    if( *ptr == '\0' ) return;          /* no digits entered */

    valid_levels = 0;
    while (sscanf(ptr,"%d",&temp_int))
    { 
        if( temp_int > 0 && temp_int < 100 )
            valid_levels++;
        
        while (isdigit(*ptr)) ptr++;

        while ((!isdigit(*ptr)) && (*ptr != '\0')) ptr ++;

        if (*ptr == '\0') break;
    }

    if( valid_levels == 0 ) return; /* No valid numbers entered */

    if (gui->legend.num_levels != 0)
    {
        if (gui->legend.rc) XtDestroyWidget(gui->legend.rc);
        if (gui->legend.color_box) MT_free( (void *)gui->legend.color_box);
        if (gui->legend.label)     MT_free( (void *)gui->legend.label);
        if (gui->legend.levels)    MT_free( (void *)gui->legend.levels);
    }
    gui->legend.num_levels = valid_levels;

    gui->legend.color_box = (Widget *)MT_malloc(sizeof(Widget)*gui->legend.num_levels);
    gui->legend.label     = (Widget *)MT_malloc(sizeof(Widget)*gui->legend.num_levels);
    gui->legend.levels    = (int *)MT_malloc(sizeof(int)*gui->legend.num_levels);
    
    i = 0;
    ptr = level_string;
    while ((!isdigit(*ptr)) && (*ptr != '\0')) ptr ++;
    while (sscanf(ptr,"%d", &temp_int))
    {
        if( temp_int > 0 && temp_int < 100 )
        {
            gui->legend.levels[i] = temp_int;
            i++;
        }

        while (isdigit(*ptr)) ptr++;
        while ((!isdigit(*ptr)) && (*ptr != '\0')) ptr ++;
        if (*ptr == '\0') break;
    }

    bubble_sort_int_array(gui->legend.levels, gui->legend.num_levels);

  /*printf("here are %d the sorted levels : ",legend.num_levels);
  for (i=0;i<legend.num_levels;i++)
    printf(" %d", legend.levels[i]);
  printf("\n");
  */

  gui->legend.rc = XtVaCreateManagedWidget("rc",
					   xmRowColumnWidgetClass, gui->legend.form,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNrightAttachment, XmATTACH_FORM,
					   XmNleftAttachment, XmATTACH_FORM,
					   XmNtopWidget, gui->legend.main_label,
					   XmNpacking, XmPACK_COLUMN,
					   XmNorientation, XmHORIZONTAL,
					   XmNnumColumns, gui->legend.num_levels,
					   NULL);

  /*printf("going to build the %d widgets\n",legend.num_levels);*/
  
  for (i=0;i<gui->legend.num_levels;i++){
    /*printf("building level : %d\n",legend.levels[i]);*/
    gui->legend.color_box[i] = XtVaCreateManagedWidget("color box",
						       xmDrawnButtonWidgetClass, gui->legend.rc,
						       XmNbackground, gui->hl,
						       XmNwidth, 60,
						       XmNheight, 10,
						       NULL);

    if (gui->contour_pref[gui->legend.levels[i]].filled)
      XtVaSetValues(gui->legend.color_box[i],XmNbackground,gui->contour_pref[gui->legend.levels[i]].color.pixel,NULL);

    XtAddCallback(gui->legend.color_box[i],XmNactivateCallback,
		  Colorwash_Legend_Color_ChangedCB,(XtPointer)gui);

    sprintf(temp_string,"%d",gui->legend.levels[i]);
    gui->legend.label[i] = XtVaCreateManagedWidget( temp_string,
						    xmLabelWidgetClass, gui->legend.rc,
						    XmNleftAttachment, XmATTACH_FORM,
						    XmNleftOffset, 15,
						    XmNtopAttachment, XmATTACH_FORM,
						    XmNtopOffset, 5,
						    NULL);
  }
}
