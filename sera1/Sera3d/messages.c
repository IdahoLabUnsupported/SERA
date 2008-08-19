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
void PostMessage (main_gui_t *gui,char message[256])
{
  XmString xmstr;
  char *text;

  /*DEBUG_TRACE_IN printf("Entered PostMessage\n");*/

  gui->quick_messages_on = 0;

  if (gui->messages_on && !gui->rotating_messages)
    {
      XSynchronize(gui->display,TRUE);
      xmstr = (XmString)XmStringCreateLocalized((char *)message);
      XtVaSetValues(gui->message_panel.pad, XmNlabelString, xmstr, NULL);
     
      XFlush(gui->display); 
      wait_on_xserver(gui);
      XSynchronize(gui->display,FALSE);       

      XmStringFree(xmstr);
    }     

  /*DEBUG_TRACE_OUT printf("Done with PostMessage\n");*/
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
void PostQuickMessage(main_gui_t *gui,char message[256])
{
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered PostQuickMessage\n");

  xmstr = (XmString)XmStringCreateLocalized((char *)message);
  XtVaSetValues(gui->message_panel.pad, XmNlabelString, xmstr, NULL);
  XmStringFree(xmstr);          

  DEBUG_TRACE_OUT printf("Done with PostQuickMessage\n");
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
void RemoveMessage (main_gui_t *gui,char message[256])
{
  XmString xmstr;
 
  DEBUG_TRACE_IN printf("Entered RemoveMessage\n");

  if (gui->messages_on && !gui->rotating_messages)
    {
      xmstr = XmStringCreateLocalized("Sera3D");
      XtVaSetValues(gui->message_panel.pad, XmNlabelString, xmstr, NULL);
    }

  gui->quick_messages_on = 1;

  DEBUG_TRACE_OUT printf("Done with RemoveMessage\n");
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
void wait_on_xserver(main_gui_t *gui)
{
  XEvent event;
  static XtInputMask m;

  DEBUG_TRACE_IN printf("Entered wait_on_xserver\n");

  while (XtAppPending(gui->app))
    {
      XtAppNextEvent(gui->app,&event);
      XtDispatchEvent(&event);
    }

  DEBUG_TRACE_OUT printf("Done with wait_on_xserver\n");
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
void set_uv_name_in_message_bar(main_gui_t *gui)
{
  char *uv_name;
  char the_message[256];
  XmString xmstr;

  DEBUG_TRACE_IN printf("entering set_uv_name_in_message_bar\n");

  if (strstr(gui->uvfile,(char *)"/")){
    uv_name = strrchr(gui->uvfile,'/');
    uv_name++;
    strcpy(the_message,uv_name);
  }else{
    strcpy(the_message,gui->uvfile);
  }
  
  strcat(the_message,(char *)" ");

  xmstr = XmStringCreateLocalized(the_message);
  XtVaSetValues(gui->message_panel.uv_pad, XmNlabelString, xmstr, NULL);
  XmStringFree(xmstr);

  DEBUG_TRACE_OUT printf("leaving set_uv_name_in_message_bar\n");
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
void register_message_handlers_for_children(main_gui_t *gui,Widget w)
{  
  int i;

  /******** NEW  NEW  NEW  NEW ****************************/
  static int initial_call = 1;
  
  /*DEBUG_TRACE_IN printf("Entered register_message_handlers_for_children\n");*/

  if (XtIsComposite(w) == FALSE)
    {
      if (XtIsRealized(w)) {
        /*printf("Registering %s\n",XtName(w));*/
          XtAddEventHandler(w, EnterWindowMask, False, ReportMessageForWidget, (XtPointer)gui);
      }
      return;
    }
  else
    {
      WidgetList theList;
      Cardinal listCount;

      if (XtIsRealized(w))
        {
          /*printf("Registering %s\n",XtName(w));*/
          XtAddEventHandler(w, EnterWindowMask, False, ReportMessageForWidget, (XtPointer)gui);
        }
        else
          /********************************************************/
          {
          }

      XtVaGetValues(w, XmNnumChildren, &listCount, XmNchildren, &theList, NULL); 
      for (i = 0; i < listCount; i ++ )
        if (XtIsWidget(theList[i]))
          register_message_handlers_for_children(gui,theList[i]);
    }   

  /*DEBUG_TRACE_OUT printf("Done with register_message_handlers_for_children\n");*/
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
void ReportMessageForWidget(Widget w, XtPointer clientData, XEvent *event, Boolean *flag)
{
  char text[256];
  main_gui_t *gui = (main_gui_t *)clientData;
  /*DEBUG_TRACE_IN printf("Entered ReportMessageForWidget\n");*/

 if (gui->messages_on && gui->quick_messages_on)
 {
   /********************* Main messages ************************/
   if(strcmp(XtName(w),"glxarea") == 0)
     PostQuickMessage(gui,"Main drawing area, right click -> mouse controls ");
   else if(strcmp(XtName(w),"messages") == 0)
     PostQuickMessage(gui,"Pointer info");
   else if(strcmp(XtName(w),"Body List") == 0)
     PostQuickMessage(gui,"click on the body name to toggle the body on or off");

   else if(strcmp(XtName(w),"IS axis") == 0)
     PostQuickMessage(gui,"Rotate around the Z axis");
   else if(strcmp(XtName(w),"PA axis") == 0)
     PostQuickMessage(gui,"Rotate around the Y axis");
   else if(strcmp(XtName(w),"RL axis") == 0)
     PostQuickMessage(gui,"Rotate around the X axis");

   else if(strcmp(XtName(w),"gl_lview_area") == 0)
     PostQuickMessage(gui,"view through the left camera");
   else if(strcmp(XtName(w),"gl_rview_area") == 0)
     PostQuickMessage(gui,"view through the right camera");
   else if(strcmp(XtName(w),"gl_tview_area") == 0)
     PostQuickMessage(gui,"view through the top camera");

   /************ Control Panel Buttons  messages *********************/
   else if(strcmp(XtName(w),"cp0") == 0)
     PostQuickMessage(gui,"The Viewing Control Panel");
   else if(strcmp(XtName(w),"cp1") == 0)
     PostQuickMessage(gui,"The Body Color Control Panel");
   else if(strcmp(XtName(w),"cp2") == 0)
     PostQuickMessage(gui,"The Body Tranparency Control Panel");
   else if(strcmp(XtName(w),"cp3") == 0)
     PostQuickMessage(gui,"The Lighting Control Panel");
   else if(strcmp(XtName(w),"cp4") == 0)
     PostQuickMessage(gui,"The Axis Control Panel");
   else if(strcmp(XtName(w),"cp5") == 0)
     PostQuickMessage(gui,"The Clipping Control Panel");
   else if(strcmp(XtName(w),"cp6") == 0)
     PostQuickMessage(gui,"The Nurbs Control Panel");
   else if(strcmp(XtName(w),"cp7") == 0)
     PostQuickMessage(gui,"The Particles Control Panel");
   else if(strcmp(XtName(w),"cp8") == 0)
     PostQuickMessage(gui,"The Slice Control Panel");
   else if(strcmp(XtName(w),"cp9") == 0)
     PostQuickMessage(gui,"The Contours Control Panel");
   else if(strcmp(XtName(w),"cp10") == 0)
     PostQuickMessage(gui,"The Mouse Control Panel");
   else if(strcmp(XtName(w),"cp11") == 0)
     PostQuickMessage(gui,"The Viewing Parameters Control Panel");
   else if(strcmp(XtName(w),"cp12") == 0)
     PostQuickMessage(gui,"The Beam Controls Control Panel");
   /*else if(strcmp(XtName(w),"cp13") == 0)
     PostQuickMessage(gui,"The Ray Trackrng Control Panel");*/
   else if(strcmp(XtName(w),"cp14") == 0)
     PostQuickMessage(gui,"The Texture Mapping Control Panel");


   /********************* View Form  messages ************************/
   else if(strcmp(XtName(w),"vs0") == 0)
     PostQuickMessage(gui,"Wireframe view style");
   else if(strcmp(XtName(w),"vs1") == 0)
     PostQuickMessage(gui,"Outline view style");
   else if(strcmp(XtName(w),"vs2") == 0)
     PostQuickMessage(gui,"Solid view style");
   else if(strcmp(XtName(w),"vs3") == 0)
     PostQuickMessage(gui,"Polygonal view style");

/*
   else if(strcmp(XtName(w),"Anterior") == 0)
     PostQuickMessage(gui,"change the main window view to the anterior camera");
   else if(strcmp(XtName(w),"Posterior") == 0)
     PostQuickMessage(gui,"change the main window view to the posterior camera ");
   else if(strcmp(XtName(w),"Left") == 0)
     PostQuickMessage(gui,"change the main window view to the left camera ");
   else if(strcmp(XtName(w),"Right") == 0)
     PostQuickMessage(gui,"change the main window view to the right camera ");
   else if(strcmp(XtName(w),"Superior") == 0)
     PostQuickMessage(gui,"change the main window view to the top camera ");
*/ 
   else if(strcmp(XtName(w),"Scalingslider") == 0)
     PostQuickMessage(gui,"scale the bodies");

   else if(strcmp(XtName(w),"auto_rotate_left") == 0)
     PostQuickMessage(gui,"Auto rotate the rendering left");
   else if(strcmp(XtName(w),"auto_rotate_right") == 0)
     PostQuickMessage(gui,"Auto rotate the rendering right");
   else if(strcmp(XtName(w),"auto_rotate_up") == 0)
     PostQuickMessage(gui,"Auto rotate the rendering up");
   else if(strcmp(XtName(w),"auto_rotate_down") == 0)
     PostQuickMessage(gui,"Auto rotate the rendering down");
   else if(strcmp(XtName(w),"stop_rotate") == 0)
     PostQuickMessage(gui,"Stop the auto rotate");
   else if(strcmp(XtName(w),"deg_up") == 0)
     PostQuickMessage(gui,"Increase the auto rotate degrees by 5");
   else if(strcmp(XtName(w),"deg_down") == 0)
     PostQuickMessage(gui,"Decrease the auto rotate degrees by 5");

   else if(strcmp(XtName(w),"RESET VIEWING") == 0)
     PostQuickMessage(gui,"Reset all the viewing options to the defaults (excluding the view style)");


   /********************* Color Form  messages ************************/
   else if(strcmp(XtName(w),"Body_menu") == 0)
     PostQuickMessage(gui,"select which body to change the color of");
   else if(strcmp(XtName(w),"ColorPresetsList") == 0)
     PostQuickMessage(gui,"Quick Preset Colors");
   else if(strcmp(XtName(w),"color_swatch") == 0)
     PostQuickMessage(gui,"Color Preview");
   else if(strcmp(XtName(w)," R ") == 0)
     PostQuickMessage(gui,"change the red color component for the selected body");
   else if(strcmp(XtName(w)," G ") == 0)
     PostQuickMessage(gui,"change the green color component for the selected body ");
   else if(strcmp(XtName(w)," B ") == 0)
     PostQuickMessage(gui,"change the blue color component for the selected body ");
   else if(strcmp(XtName(w),"Apply to Body") == 0)
     PostQuickMessage(gui,"Apply the settings to the selected body and redraw");


   /********************* Transparency Form  messages *****************/
   else if(strcmp(XtName(w),"T_menu") == 0)
     PostQuickMessage(gui,"select which body to change the transparency for");
   else if(strcmp(XtName(w),"Opacity Presets") == 0)
     PostQuickMessage(gui,"Quick Preset Opacities");
   else if(strcmp(XtName(w),"t_slider") == 0)
     PostQuickMessage(gui,"change the opacity of the selected body");
   

   /********************* Lighting Form  messages *********************/
   else if(strcmp(XtName(w),"Front Right") == 0)
     PostQuickMessage(gui,"Toggle the front right light on or off ");
   else if(strcmp(XtName(w),"Front Left") == 0)
     PostQuickMessage(gui,"Toggle the front left light on or off");
   else if(strcmp(XtName(w),"Back Left") == 0)
     PostQuickMessage(gui,"Toggle the back left light on or off ");
   else if(strcmp(XtName(w),"Back Right") == 0)
     PostQuickMessage(gui,"Toggle the back right light on or off ");
   else if(strcmp(XtName(w),"Top") == 0)
     PostQuickMessage(gui,"Toggle the top light on or off ");
   else if(strcmp(XtName(w),"Ambientslider") == 0)
     PostQuickMessage(gui,"adjust the ambient lighting (between 0(off) and 1(fully on)");
   else if(strcmp(XtName(w),"DEFAULT") == 0)
     PostQuickMessage(gui,"set the lighting controls to their defaults ");


   /********************* Axis Form  messages   ***********************/
   else if(strcmp(XtName(w),"at1") == 0)
     PostQuickMessage(gui,"display full axes");
   else if(strcmp(XtName(w),"at2") == 0)
     PostQuickMessage(gui,"display thin positive axes");
   else if(strcmp(XtName(w),"at3") == 0)
     PostQuickMessage(gui,"display thick positive axes");
   else if(strcmp(XtName(w),"at0") == 0)
     PostQuickMessage(gui,"display no axes");
   else if(strcmp(XtName(w),"Axes Labels") == 0)
     PostQuickMessage(gui,"toggle the axes labels");

   /********************* Clipping Form  messages  ********************/
   else if(strcmp(XtName(w),"Set Clipping") == 0)
     PostQuickMessage(gui,"brings up the clipping dialog box to set the clipping");
   else if(strcmp(XtName(w),"Clipped Bodies") == 0)
     PostQuickMessage(gui,"toggle specific bodies from being clipped or not");


   /********************* Nurb Form  messages  ************************/
   else if(strcmp(XtName(w),"Order") == 0)
     PostQuickMessage(gui,"choose the nurb order : Linear, Quadratic, Cubic ");
   else if(strcmp(XtName(w),"Quality") == 0)
     PostQuickMessage(gui,"Choose the nurb quality : Low, Medium, or High ");
   /*else if(strcmp(XtName(w),"Sampling") == 0)
     PostQuickMessage("Choose the Control Point Sampling Method  : Direct or by Degrees");*/
   else if(strcmp(XtName(w),"Type") == 0)
     PostQuickMessage(gui,"Choose the nurb display type  : Solid or Mesh");
   else if(strcmp(XtName(w),"# Ctl Points") == 0)
     PostQuickMessage(gui,"Set the # of control points extracted to provide the nurb framework");
   else if(strcmp(XtName(w),"Draw Ctl Points") == 0)
     PostQuickMessage(gui,"Displays the control points selected (after nurb rebuild)");
   else if(strcmp(XtName(w),"Rebuild Nurbs") == 0)
     PostQuickMessage(gui,"Recontructs the Nurbs bodies with the new settings");


   /********************* Particle Path Form  messages ****************/
   else if(strcmp(XtName(w),"Load Paths") == 0)
     PostQuickMessage(gui,"load a particle path file (.pp)");
   else if(strcmp(XtName(w),"Display Particles") == 0)
     PostQuickMessage(gui,"draw the loaded particles paths in the image");
   else if(strcmp(XtName(w),"Antialiasing") == 0)
     PostQuickMessage(gui,"smooth out the particle lines");
   else if(strcmp(XtName(w),"gl_color") == 0)
     PostQuickMessage(gui,"adjust the color and line style for the Low energy Gamma paths");
   else if(strcmp(XtName(w),"gm_color") == 0)
     PostQuickMessage(gui,"adjust the color and line style for the Medium energy Gamma paths");
   else if(strcmp(XtName(w),"gh_color") == 0)
     PostQuickMessage(gui,"adjust the color and line style for the High energy Gamma paths");
   else if(strcmp(XtName(w),"nl_color") == 0)
     PostQuickMessage(gui,"adjust the color and line style for the Low energy Neutron paths");
   else if(strcmp(XtName(w),"nm_color") == 0)
     PostQuickMessage(gui,"adjust the color and line style for the Medium energy Neutron paths");
   else if(strcmp(XtName(w),"nh_color") == 0)
     PostQuickMessage(gui,"adjust the color and line style for the High energy Neutron paths");
   else if(strcmp(XtName(w),"beam_color") == 0)
     PostQuickMessage(gui,"adjust the color and line style for the Beam");
   else if(strcmp(XtName(w),"lost_color") == 0)
     PostQuickMessage(gui,"adjust the color and line style for the lost particles");
   else if(strcmp(XtName(w),"Gamma Low") == 0)
     PostQuickMessage(gui,"Toggle the Low energy Gamma particle paths");
   else if(strcmp(XtName(w),"Gamma Medium") == 0)
     PostQuickMessage(gui,"Toggle the Medium energy Gamma particle paths");
   else if(strcmp(XtName(w),"Gamma High") == 0)
     PostQuickMessage(gui,"Toggle the High energy Gamma particle paths");
   else if(strcmp(XtName(w),"Neutron Low") == 0)
     PostQuickMessage(gui,"Toggle the Low energy Neutron particle paths");
   else if(strcmp(XtName(w),"Neutron Medium") == 0)
     PostQuickMessage(gui,"Toggle the Medium energy Neutron particle paths");
   else if(strcmp(XtName(w),"Neutron High") == 0)
     PostQuickMessage(gui,"Toggle the High energy Neutron particle paths");
   else if(strcmp(XtName(w),"Beam") == 0)
     PostQuickMessage(gui,"Toggle the Beam");
   else if(strcmp(XtName(w),"Lost") == 0)
     PostQuickMessage(gui,"Toggle the lost particle paths");
   else if(strcmp(XtName(w),"Defaults") == 0)
     PostQuickMessage(gui,"Reset the particle line types to their defaults");


   /********************* Slice Form messages *************************/
   else if(strcmp(XtName(w),"Load Raw Images") == 0)
     PostQuickMessage(gui,"Load a .qim file which contains the corresponding raw images");
   else if(strcmp(XtName(w),"Inlay Slice") == 0)
     PostQuickMessage(gui,"Inlay a slice into the rendering");
   else if(strcmp(XtName(w),"Axial Slice") == 0)
     PostQuickMessage(gui,"Make the inlaid slice an axial slice");
   else if(strcmp(XtName(w),"Coronal Slice") == 0)
     PostQuickMessage(gui,"Make the inlaid slice a coronal slice");
   else if(strcmp(XtName(w),"Sagittal Slice") == 0)
     PostQuickMessage(gui,"Make the inlaid slice a sagittal slice");
   else if(strcmp(XtName(w),"External Window") == 0)
     PostQuickMessage(gui,"brings up an external window for viewing the inlaid slice");
   else if(strcmp(XtName(w),"Inlaid Slice Slider") == 0)
     PostQuickMessage(gui,"changes the position of the currently inlaid slice");
   /********************* Contours Form messages **********************/

   /********************* Mouse Form  messages ************************/
   else if(strcmp(XtName(w),"mb1") == 0)
     PostQuickMessage(gui,"Control Rotation by Pulling Axis Method");
   else if(strcmp(XtName(w),"mb3") == 0)
     PostQuickMessage(gui,"Control Rotation by Mouse Buttons : 1->X axis  2->Y axis");
   else if(strcmp(XtName(w),"mb2") == 0)
     PostQuickMessage(gui,"Control Rotation by Sliders");
   else if(strcmp(XtName(w),"Lock Bodies") == 0)
     PostQuickMessage(gui,"Locks the Bodies from being rotated in the rendering");
   else if(strcmp(XtName(w),"Lock Clip Planes") == 0)
     PostQuickMessage(gui,"Locks the Clipping planes from being rotated");
   /********************* View Params  Form  messages******************/
   else if(strcmp(XtName(w),"Double Buffered") == 0)
     PostQuickMessage(gui,"draws to back buffer first for smooth animation");
   else if(strcmp(XtName(w),"Single Buffered") == 0)
     PostQuickMessage(gui,"draws directly to screen for watching the drawing");
   else if(strcmp(XtName(w),"Show Frame Rate") == 0)
     PostQuickMessage(gui,"toggles the frame rate on/off in the bottom left of the main window");
   else if(strcmp(XtName(w),"Test Polygon Speed") == 0)
     PostQuickMessage(gui,"Runs a speed test and reports the rendering speed of the system");
   /********************* Beam  Form  messages ************************/
   else if(strcmp(XtName(w),"Beam in Use") == 0)
     PostQuickMessage(gui,"toggles between the beam from the file and an interactive beam");
   else if(strcmp(XtName(w),"Beam Line View") == 0)
     PostQuickMessage(gui,"Toggles the beam's eye view camera on/off");
   else if(strcmp(XtName(w),"Beam_Slider") == 0)
     PostQuickMessage(gui,"slides the beam camera along the beam line"); 
   else if(strcmp(XtName(w),"Beam_Slice_Slider") == 0)
     PostQuickMessage(gui,"Slides the inlaid beam slice along the beam line");
   else if(strcmp(XtName(w),"Test Polygon Speed") == 0)
     PostQuickMessage(gui,"Runs a speed test and reports the rendering speed of the system");
   else if(strcmp(XtName(w),"ch0") == 0)
     PostQuickMessage(gui,"Displays no crosshairs");
   else if(strcmp(XtName(w),"ch1") == 0)
     PostQuickMessage(gui,"Displays thin crosshairs");
   else if(strcmp(XtName(w),"ch2") == 0)
     PostQuickMessage(gui,"Displays scope crosshairs");
   else if(strcmp(XtName(w),"ch3") == 0)
     PostQuickMessage(gui,"Displays diamond centered crosshairs");
   else if(strcmp(XtName(w),"Inlay Beam Slice") == 0)
     PostQuickMessage(gui,"inlays a slice perpendicular to the beam");
   else if(strcmp(XtName(w),"Clip With Beam Slice") == 0)
     PostQuickMessage(gui,"will clip bodies with the beam slice");
   else if(strcmp(XtName(w),"I-Beam Controls") == 0)
     PostQuickMessage(gui,"brings up the controls for moving the interactive beam");
   else if(strcmp(XtName(w),"Use Aperture") == 0)
     PostQuickMessage(gui,"changes the view to include a view aperture and a ring aperture");
   else if(strcmp(XtName(w),"Aperture Settings") == 0)
     PostQuickMessage(gui,"brings up the Aperture control window");
  /********************* Ray Track  Form  messages *******************/


   /********************* Texture  Form  messages ************************/
   else if(strcmp(XtName(w),"NEAREST") == 0)
     PostQuickMessage(gui,"when texturing, just picks the nearest value");
   else if(strcmp(XtName(w),"LINEAR") == 0)
     PostQuickMessage(gui,"when texturing, linear interpolates between 4 neighbors");
   else if(strcmp(XtName(w),"REPEAT") == 0)
     PostQuickMessage(gui,"texture repeats when an object goes out of bounds");
   else if(strcmp(XtName(w),"CLAMP") == 0)
     PostQuickMessage(gui,"texture clamps to edge value when an object goes out of bounds");
   else if(strcmp(XtName(w),"Alpha Culling") == 0)
     PostQuickMessage(gui,"toggles alpha culling (throwing out certain values) on/off");
   else if(strcmp(XtName(w),"alpha_slider") == 0)
     PostQuickMessage(gui,"values less than the slider value are thrown out (with Alpha Culling)");
   else if(strcmp(XtName(w),"Volume Render") == 0)
     PostQuickMessage(gui,"volume renders the raw images");
   else if(strcmp(XtName(w),"Texture the Bodies") == 0)
     PostQuickMessage(gui,"toggles texture mappping of the nurb bodies with the raw images");

   else
     PostQuickMessage (gui,"-----");
 }

 /*DEBUG_TRACE_OUT printf("Done with ReportMessageForWidget\n");*/
}
