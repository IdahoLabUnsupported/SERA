#include "toqsh.h"

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
void resize_images(main_gui_t *gui)
{
  int i;
  int new_image_size;
  int old_image_size;
  int new_image_dimension;
  int old_image_dimension;
  int full_image_set_old;
  int full_image_set_new;
  int num_images;
  unsigned char *new_images,*ptr;
  unsigned char *old_images;

  char temp[15];

  DEBUG_TRACE_IN printf("Entered resize_images\n");
/* printf("RESIZING THE IMAGES\n"); */
  if (!gui->images_loaded) return;

  /*sprintf(temp,"%d",gui->qsh_gui->qsh_info->size_of_dimension[1]);
    XmTextSetString(gui->ir.new_size_tb,temp);*/
  

  new_image_dimension = atoi((char *)XmTextGetString(gui->manip_gui.scale_panel.size_tb));
/* printf("the new_image_dimension is : %d\n",new_image_dimension); */
  old_image_dimension = gui->qsh_gui->qsh_info->size_of_dimension[1];
/* printf("the old_image_dimension is : %d\n",old_image_dimension); */
  new_image_size = new_image_dimension * new_image_dimension;
/* printf("the new_image_size is : %d\n",new_image_size); */
  old_image_size = old_image_dimension * old_image_dimension;
/* printf("the old_image_size is : %d\n",old_image_size); */
  num_images = gui->qsh_gui->qsh_info->size_of_dimension[0];
  full_image_set_old = num_images * old_image_size;
/* printf("the old_image_set is : %d\n", full_image_set_old); */
  full_image_set_new = num_images * new_image_size;
  /*printf("the new_image_set is : %d\n", full_image_set_new);*/
  
  new_images = (unsigned char *) MT_malloc(full_image_set_new);
  
  old_images = gui->qsh_gui->qsh_info->images;

  /*printf("copying and resizing the %d images\n",num_images);*/
  for (i=0;i<num_images;i++){
    generic_resize_image(&old_images[i*old_image_size], &new_images[i*new_image_size],
			 old_image_dimension, old_image_dimension,
        		 new_image_dimension, new_image_dimension,
			 1);
  }

  /*printf("done copying and resizing\n");*/

  /*printf("freeing the old images\n"); */
  MT_free((void *)old_images);
  /*printf("done\n");*/
  gui->qsh_gui->qsh_info->images = new_images;
  gui->qsh_gui->qsh_info->size_of_dimension[1] = new_image_dimension;
  gui->qsh_gui->qsh_info->size_of_dimension[2] = new_image_dimension;

  /*printf("set the images to %d x %d\n",new_image_dimension,new_image_dimension);*/


  DEBUG_TRACE_OUT printf("Done with resize_images\n");
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
/*
void Resize_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata){
int i;
int new_image_size;
int old_image_size;
int new_image_dimension;
int old_image_dimension;
int full_image_set_old;
int full_image_set_new;
int num_images;
unsigned char *new_images,*ptr;
unsigned char *old_images;

char temp[15];
main_gui_t *gui = (main_gui_t *)clientdata;

if (!gui->images_loaded) return;

sprintf(temp,"%d",gui->qsh_gui->qsh_info->size_of_dimension[1]);
XmTextSetString(gui->ir.new_size_tb,temp);

gui->ir.finished = 0;
XtPopup(gui->ir.shell,XtGrabNone);

while ( !gui->ir.finished )
XtAppProcessEvent ( gui->app, XtIMAll );

if (gui->ir.cancelled) return;

DisplayBusyCursor(gui->mainwindow);

new_image_dimension = atoi((char *)XmTextGetString(gui->ir.new_size_tb));
old_image_dimension = gui->qsh_gui->qsh_info->size_of_dimension[1];
new_image_size = new_image_dimension * new_image_dimension;
old_image_size = old_image_dimension * old_image_dimension;
num_images = gui->qsh_gui->qsh_info->size_of_dimension[0];
full_image_set_old = num_images * old_image_size;
full_image_set_new = num_images * new_image_size;

if (!(new_images = (unsigned char *)malloc(full_image_set_new))){
printf("couldn't allocate enough room to copy over the new images\n");
printf("tried to allocate %d bytes\n",full_image_set_new);
return;
}

old_images = gui->qsh_gui->qsh_info->images;

for (i=0;i<num_images;i++){
generic_resize_image(&old_images[i*old_image_size], &new_images[i*new_image_size],
old_image_dimension, old_image_dimension,
new_image_dimension, new_image_dimension,
1);
}

free(old_images);

gui->qsh_gui->qsh_info->images = new_images;
gui->qsh_gui->qsh_info->size_of_dimension[1] = new_image_dimension;
gui->qsh_gui->qsh_info->size_of_dimension[2] = new_image_dimension;

destroy_and_rebuild_images(gui);

RemoveBusyCursor(gui->mainwindow);
}
*/


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_unknown_raw_popup_shell
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void build_resize_images_popup_shell(main_gui_t *gui){
XmString xmstr;

DEBUG_TRACE_IN printf("Entered build_resize_images_popup_shell\n");

gui->ir.shell = XtCreatePopupShell("Resize Images",
topLevelShellWidgetClass,gui->toplevel,
NULL,0);
XtPopdown(gui->ir.shell);

gui->ir.form = XtVaCreateManagedWidget("ctform",
xmFormWidgetClass, gui->ir.shell,
NULL);

gui->ir.title = XtVaCreateManagedWidget("Enter the new dimension for the images",
xmLabelWidgetClass, gui->ir.form,
XmNtopAttachment, XmATTACH_FORM,
XmNtopOffset, 10,
XmNleftAttachment, XmATTACH_FORM,
XmNleftOffset, 25,
NULL);
gui->ir.sep1 = XtVaCreateManagedWidget("sep1",
xmSeparatorWidgetClass, gui->ir.form,
XmNleftAttachment, XmATTACH_FORM,
XmNleftOffset, 10,
XmNrightAttachment, XmATTACH_FORM,
XmNrightOffset, 10,
XmNtopAttachment, XmATTACH_WIDGET,
XmNtopWidget, gui->ir.title,
XmNtopOffset, 10,
NULL);

gui->ir.new_size_label = XtVaCreateManagedWidget("Image Dimension:",
xmLabelWidgetClass,gui->ir.form,
XmNtopAttachment, XmATTACH_WIDGET,
XmNtopWidget, gui->ir.sep1,
XmNtopOffset, 20,
XmNleftAttachment, XmATTACH_FORM,
XmNleftOffset, 10,
NULL);
gui->ir.new_size_tb = XtVaCreateManagedWidget("New Size TB",
xmTextFieldWidgetClass,gui->ir.form,
XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
XmNtopWidget, gui->ir.new_size_label,
XmNleftAttachment, XmATTACH_FORM,
XmNleftOffset, 200,
XmNrightAttachment, XmATTACH_FORM,
XmNrightOffset, 10,
NULL);

gui->ir.sep2 = XtVaCreateManagedWidget("sep2",
xmSeparatorWidgetClass, gui->ir.form,
XmNleftAttachment, XmATTACH_FORM,
XmNleftOffset, 10,
XmNrightAttachment, XmATTACH_FORM,
XmNrightOffset, 10,
XmNtopAttachment, XmATTACH_WIDGET,
XmNtopWidget, gui->ir.new_size_label,
XmNtopOffset, 20,
NULL);  


gui->ir.apply = XtVaCreateManagedWidget("Apply",
xmPushButtonWidgetClass, gui->ir.form,
XmNtopAttachment, XmATTACH_WIDGET,
XmNtopWidget, gui->ir.sep2,
XmNtopOffset, 5,
XmNrightAttachment, XmATTACH_FORM,
XmNrightOffset, 5,
XmNbottomAttachment, XmATTACH_FORM,
XmNbottomOffset, 5,
NULL);
XtAddCallback(gui->ir.apply, XmNactivateCallback,
Resize_Images_ApplyCB,(XtPointer)gui);

gui->ir.cancel = XtVaCreateManagedWidget("Cancel",
xmPushButtonWidgetClass, gui->ir.form,
XmNtopAttachment, XmATTACH_WIDGET,
XmNtopWidget, gui->ir.sep2,
XmNtopOffset, 5,
XmNrightAttachment, XmATTACH_WIDGET,
XmNrightWidget, gui->ir.apply,
XmNrightOffset, 5,
XmNbottomAttachment, XmATTACH_FORM,
XmNbottomOffset, 5,
NULL);
XtAddCallback(gui->ir.cancel, XmNactivateCallback,
Resize_Images_CancelCB,(XtPointer)gui);

gui->ir.finished = 0;

DEBUG_TRACE_OUT printf("Done with build_unknown_raw_popup_shell\n");
}
*/

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
/*
void Resize_Images_CancelCB(Widget w,XtPointer clientdata, XtPointer calldata){
main_gui_t *gui = (main_gui_t *)clientdata;

DEBUG_TRACE_IN printf("Entered Resize_Images_CancelCB\n");

XtPopdown(gui->ir.shell);
gui->ir.cancelled = 1;
gui->ir.finished = 1;

DEBUG_TRACE_OUT printf("Done with Resize_Images_CancelCB\n")
}
*/

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
/*
void Resize_Images_ApplyCB(Widget w,XtPointer clientdata, XtPointer calldata){
main_gui_t *gui = (main_gui_t *)clientdata;

DEBUG_TRACE_IN printf("Entered Resize_Images_ApplyCB\n");

if (!confirm(gui,"You will be altering the original images,\n  Are you Sure you want to do this?")){
DEBUG_TRACE_OUT printf("Done with Resize_Images_ApplyCB\n");
return;
}

XtPopdown(gui->ir.shell);
gui->ir.cancelled = 0;
gui->ir.finished = 1;

DEBUG_TRACE_OUT printf("Done with Resize_Images_ApplyCB\n");
}
*/
