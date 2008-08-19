#include "sera3d.h"
/*
GLUquadricObj *intersect_obj;
Ray_t Rays[10000];
int num_rays;


void draw_intersection_object();
void SelectRayTrackFileCallback(Widget w, XtPointer clientData, XtPointer callData);
void RayTrackOKCallback (Widget w, XtPointer clientData, XtPointer callData);
void RayTrackCancelCallback(Widget w, XtPointer clientData, XtPointer callData);
void Ray_Num_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);


void draw_tracked_rays(main_gui_t *gui)
{
  float di,dj,dk;
  int i,j, lights_on=0;

  if (glIsEnabled(GL_LIGHTING)){
    glDisable(GL_LIGHTING); gui->lights_on = 1;
  }

  glBegin(GL_POINTS);

  for (j=0;j<gui->num_rays;j++){
    for (i=1;i<gui->Rays[j].num_subrays;i++){
      glColor3f(i*.2,1.0 - i*.2,0);      

      glVertex3f(gui->Rays[j].subray[i].start_x*10.0,
		 gui->Rays[j].subray[i].start_z*10.0,
		 gui->Rays[j].subray[i].start_y*10.0);
    }
  }

  glEnd();
  
  if (lights_on) glEnable(GL_LIGHTING);

}

void draw_tracked_ray(int j)
{
  float di,dj,dk;
  int i;

  glBegin(GL_LINES);

    for (i=0;i<gui->Rays[j].num_subrays;i++){
      
      di = gui->Rays[j].subray[i].i * gui->Rays[j].subray[i].dist;
      dj = gui->Rays[j].subray[i].j * gui->Rays[j].subray[i].dist;
      dk = gui->Rays[j].subray[i].k * gui->Rays[j].subray[i].dist;

      glColor3f(i*.2,1.0 - i*.2,0);      
      
      glVertex3f(gui->Rays[j].subray[i].start_x*10.0,
		 gui->Rays[j].subray[i].start_z*10.0,
		 gui->Rays[j].subray[i].start_y*10.0);
      
      glVertex3f((gui->Rays[j].subray[i].start_x + di)*10.0,
		 (gui->Rays[j].subray[i].start_z + dk)*10.0,
		 (gui->Rays[j].subray[i].start_y + dj)*10.0);
    }
  glEnd();

  
    for (i=1;i<gui->Rays[j].num_subrays;i++){
      glColor3f(i*.2,1.0 - i*.2,0);      
      glPushMatrix();
      glTranslatef(gui->Rays[j].subray[i].start_x*10,
		   gui->Rays[j].subray[i].start_z*10,
		   gui->Rays[j].subray[i].start_y*10);
      draw_intersection_object(gui);
      glPopMatrix();
    }
  
}
*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Show_ray_tracking_dialogCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: brings up the ray tracking dialog box
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*void Show_ray_tracking_dialogCB(Widget w, XtPointer clientData, XtPointer callData){
  Boolean tf_inter, tf_planes;
  int val,i,j;
  Widget dialog = (Widget)clientData;

  XtManageChild(dialog);
  }*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_ray_tracking_dialog
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent
%%%
%%%  Purpose: builds the dialog for tracking the rays
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*Widget build_ray_tracking_dialog(Widget parent){
  XmString xmstr;
  Widget dialog,main_form,
  start_label,dir_label,body_label,dist_label,num_vox_label,
  ray_num_label,
  ray_num_slider,ray_num_divider,ray_frame,ray_form,
  ray_divider,ray_swin,ray_swinform;
  
  dialog = (Widget)XmCreateMessageDialog(parent, "Detailed Clipping",NULL,0);
  
  XtUnmanageChild((Widget)XmMessageBoxGetChild(dialog,XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(dialog,XmDIALOG_MESSAGE_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(dialog,XmDIALOG_HELP_BUTTON));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(dialog,XmDIALOG_CANCEL_BUTTON));

  xmstr = XmStringCreateLocalized("Done");
  XtVaSetValues((Widget)XmMessageBoxGetChild(dialog,XmDIALOG_OK_BUTTON),
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);

  main_form = XtVaCreateManagedWidget("Main",
				      xmFormWidgetClass, dialog,
				      NULL);

  ray_num_label = XtVaCreateManagedWidget("Ray #",
					  xmLabelWidgetClass, main_form,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNleftOffset, 15,
	 				  XmNtopAttachment, XmATTACH_FORM,
					  XmNtopOffset, 15,
					  NULL);
  
  ray_num_slide r = (Widget)XtVaCreateManagedWidget(
	       	   "Rayslider",xmScaleWidgetClass,
			   main_form,
			   XmNrightAttachment, XmATTACH_NONE,
			   XmNleftAttachment, XmATTACH_WIDGET,
			   XmNleftWidget, ray_num_label,
			   XmNleftOffset, 10,
			   XmNtopAttachment, XmATTACH_FORM,
			   XmNtopOffset, 10,
			   XmNvalue, 0,
			   XmNminimum, 0,
			   XmNmaximum, 100,
			   XmNscaleWidth, 150,
			   XmNscaleHeight, 15,
			   XmNshowValue, TRUE,
			   XmNorientation, XmHORIZONTAL,
			   NULL);
  XtAddCallback(ray_num_slider, XmNdragCallback,
		Ray_Num_ChangedCB, NULL);
  

		ray_num_divider = XtVaCreateManagedWidget(
		"ray_divider",
		xmSeparatorWidgetClass,main_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftOffset, 10,
		XmNrightOffset,10,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, ray_num_slider,
		XmNtopOffset, 10,
		NULL);
		
		ray_frame = XtVaCreateManagedWidget(
		"ray_frame",
		xmFrameWidgetClass,main_form,
		XmNshadowType, XmSHADOW_ETCHED_IN,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, ray_num_divider,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftOffset, 10,
		XmNrightOffset, 10,
		XmNtopOffset, 10,
		XmNbottomOffset, 10,
		NULL);
		
		ray_form = XtVaCreateManagedWidget(
		"ray_form",
		xmFormWidgetClass, ray_frame,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
		
		
		start_label = XtVaCreateManagedWidget("From",
		xmLabelWidgetClass, ray_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		NULL);
		
		dir_label = XtVaCreateManagedWidget("Dir",
		xmLabelWidgetClass, ray_form,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, start_label,
		XmNleftOffset, 45,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		NULL);
		body_label = XtVaCreateManagedWidget("Body",
		xmLabelWidgetClass, ray_form,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, dir_label,
		XmNleftOffset, 45,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		NULL);
		
		dist_label = XtVaCreateManagedWidget("Dist",
		xmLabelWidgetClass, ray_form,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, body_label,
		XmNleftOffset, 25,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		NULL);
		
		num_vox_label = XtVaCreateManagedWidget("# Vox",
		xmLabelWidgetClass, ray_form,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, dist_label,
		XmNleftOffset, 15,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 25,
		NULL);
		
		
		ray_divider = XtVaCreateManagedWidget(
		"ray_divider",
		xmSeparatorWidgetClass,ray_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, num_vox_label,
		XmNtopOffset, 2,
		NULL);
		
		ray_swin = XtVaCreateManagedWidget("Scrolled Win",
		xmScrolledWindowWidgetClass, ray_form,
		XmNscrollingPolicy, XmAUTOMATIC,
		XmNscrollBarDisplayPolicy, XmSTATIC,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, ray_divider,
		XmNtopOffset, 2,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
		ray_swinform = XtVaCreateManagedWidget(
		"scroll_win_form",
		xmFormWidgetClass, ray_swin,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		NULL);
		
		
		
		return dialog;
		
		}
*/
		
/*		
		void init_intersection_object()
		{
		intersect_obj = gluNewQuadric();
		}
		
		
		void draw_intersection_object()
		{
		gluSphere(intersect_obj,3,3,3);
		}
*/

/*

  void read_ray_file(char *ray_filename){
  int i=0;
  FILE *ray_file;
  char junk_string[256];
  int junk_int,cur_ray=0,cur_subray=0;
  int done _with_ray;
  int done = 0,test;

printf("the filename is : %s\n",ray_filename);
  if (!(ray_file = fopen(ray_filename,"r")))
    printf("couldn't open the file: %s\n",ray_filename);
    
    
    while(!done)
    {
    done_with_ray = 0;
    cur_subray = 0;
    
    if (cur_ray == 0)
    {
    fscanf(ray_file,"%d",&junk_int);
    }
    fscanf(ray_file,"%f %f %f",
    &Rays[cur_ray].subray[cur_subray].start_x,
    &Rays[cur_ray].subray[cur_subray].start_y,
    &Rays[cur_ray].subray[cur_subray].start_z);
    
    printf("%f %f %f  ",
    Rays[cur_ray].subray[cur_subray].start_x,
    Rays[cur_ray].subray[cur_subray].start_y,
    Rays[cur_ray].subray[cur_subray].start_z);
    
    
    fscanf(ray_file,"%f %f %f",
    &Rays[cur_ray].subray[cur_subray].i,
    &Rays[cur_ray].subray[cur_subray].j,
    &Rays[cur_ray].subray[cur_subray].k);
    
    printf("%f %f %f  ",
    Rays[cur_ray].subray[cur_subray].i,
    Rays[cur_ray].subray[cur_subray].j,
    Rays[cur_ray].subray[cur_subray].k);
    
    
    fscanf(ray_file,"%f", &Rays[cur_ray].subray[cur_subray].dist);
    
    printf("%f  ", Rays[cur_ray].subray[cur_subray].dist);      
    
    
    fscanf(ray_file,"%s", junk_string);
    
    printf("%s  ", junk_string);
    
    
    fscanf(ray_file,"%s", Rays[cur_ray].subray[cur_subray].material);
    
    printf("%s ", Rays[cur_ray].subray[cur_subray].material);
    
    
    fscanf(ray_file,"%d", &Rays[cur_ray].subray[cur_subray].num_voxels);
    
    printf("%d\n", Rays[cur_ray].subray[cur_subray].num_voxels);
    jump_to_next_line(ray_file);
    
    
    cur_subray++;
    while (!done_with_ray){
    test = fscanf(ray_file,"%d",&junk_int);
    
    
    if (test == EOF){
    done = 1;break;
    }	
    if (junk_int != 0){
    done_with_ray = 1;
    
    continue;
    }
    fscanf(ray_file,"%f %f %f",
    &Rays[cur_ray].subray[cur_subray].start_x,
    &Rays[cur_ray].subray[cur_subray].start_y,
    &Rays[cur_ray].subray[cur_subray].start_z);
    printf("%f %f %f  ",
    Rays[cur_ray].subray[cur_subray].start_x,
    Rays[cur_ray].subray[cur_subray].start_y,
    Rays[cur_ray].subray[cur_subray].start_z);
    
    fscanf(ray_file,"%f %f %f",
    &Rays[cur_ray].subray[cur_subray].i,
    &Rays[cur_ray].subray[cur_subray].j,
    &Rays[cur_ray].subray[cur_subray].k);
    printf("%f %f %f  ",
    Rays[cur_ray].subray[cur_subray].i,
    Rays[cur_ray].subray[cur_subray].j,
    Rays[cur_ray].subray[cur_subray].k);
    
    fscanf(ray_file,"%f", &Rays[cur_ray].subray[cur_subray].dist);
    printf("%f  ", Rays[cur_ray].subray[cur_subray].dist);      
    
    fscanf(ray_file,"%s", junk_string);
    printf("%s  ", junk_string);
    
    fscanf(ray_file,"%s", Rays[cur_ray].subray[cur_subray].material);
    printf("%s ", Rays[cur_ray].subray[cur_subray].material);
    
    fscanf(ray_file,"%d", &Rays[cur_ray].subray[cur_subray].num_voxels);
    printf("%d\n", Rays[cur_ray].subray[cur_subray].num_voxels);
    
    jump_to_next_line(ray_file);
    
    cur_subray++;
    }
    
    
    Rays[cur_ray].num_subrays = cur_subray;
    cur_ray++;
    num_rays++;
    }
    
    fclose(ray_file);
    
    loaded_ray_tracking = 1;
    }
*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : SelectRayTrackFileCallback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: pulls up the selectfiledialog to open files
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
  void SelectRayTrackFileCallback(Widget w, XtPointer clientData, 
  XtPointer callData)
  {
  Widget text = (Widget) clientData;
  static Widget dialog = NULL;
  
  if (!dialog)
  {
  dialog = (Widget)XmCreateFileSelectionDialog(
  w,"openRayTrackFileDialog",
  NULL,0);
  
  XtUnmanageChild((Widget)XmFileSelectionBoxGetChild(dialog,
  XmDIALOG_HELP_BUTTON) );
  
  XtAddCallback(dialog, XmNokCallback,
  RayTrackOKCallback,NULL);
  XtAddCallback(dialog, XmNcancelCallback,
  RayTrackCancelCallback,NULL);
  }
  XtManageChild(dialog);
  }
*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : RayTrackOKCallback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: when the file is chosen
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void RayTrackOKCallback (Widget w, XtPointer clientData, XtPointer callData){
 XmFileSelectionBoxCallbackStruct *cbs = 
 (XmFileSelectionBoxCallbackStruct *) callData;
 
 char *fileName;
 int i = 0,valid_file = 1;
 char c;
 
 DisplayBusyCursor(form);
 
 
 XtUnmanageChild(w);
 
 
 XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &fileName);
 
 printf("in the OK, the filename was : %s\n",fileName);
 
 read_ray_file(fileName);
 
 draw_all();
 RemoveBusyCursor(form);
 }
*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : RayTrackCancelCallback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
  void RayTrackCancelCallback(Widget w, XtPointer clientData, XtPointer callData)
  {
  DisplayBusyCursor(mainwindow);
  XtUnmanageChild(w);
  RemoveBusyCursor(mainwindow);
}

void Ray_Num_ChangedCB(Widget w, XtPointer clientData, XtPointer callData){
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct*) callData;

  current_tracked_ray = cbs->value; 

  draw_all();
}
*/
