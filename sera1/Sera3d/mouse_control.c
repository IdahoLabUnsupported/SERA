#include <stdio.h>
#include "sera3d.h"


static void Slider_rotateCB(Widget w, XtPointer clientData, XtPointer callData);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : center_rotation_under_360
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: rotation degrees
%%%
%%%  Purpose: keeps the rotation within the 0-360 range
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int center_rotation_under_360(int rotation)
{
    DEBUG_TRACE_IN printf("Entered center_rotation_under_360\n");
    
    if (rotation < -180)
        while (rotation < -180) rotation += 360;
    else if (rotation > 180)
        while (rotation > 180) rotation -= 360;
  
    DEBUG_TRACE_OUT printf("Done with center_rotation_under_360, returning : %d\n",rotation);
    return rotation;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Reset_positions
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (event handler)
%%%
%%%  Purpose: to reset the old mouse positions to the
%%%           current position, to ensure the rotation
%%%           is correct.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Reset_positions (Widget w, XtPointer clientData,
		      XEvent *event, Boolean *flag)
{
    main_gui_t *gui = (main_gui_t *)clientData;
    int centered_x, centered_y;
  
    DEBUG_TRACE_IN printf("Entered Reset_positions\n");

    /*************************************************/
    /** Must save the position of the cursor when */
    /** the mouse button was pressed to ensure    */
    /** correct rotation                          */
    /*************************************************/
    gui->oldx = event->xbutton.x;
    gui->oldy = event->xbutton.y;
  
    centered_x = event->xbutton.x - 350;
    centered_y = -(event->xbutton.y - 350);
  
    gui->click_angle = calculate_angle(centered_x,centered_y);
  
    switch(gui->pickedaxis){
        case 1: gui->click_difference = gui->click_angle - gui->rotation_x; break;
        case 2: gui->click_difference = gui->click_angle - gui->rotation_y; break;
        case 3: gui->click_difference = gui->click_angle - gui->rotation_z; break;
    }
  
    gui->highlight_on = 1;

    DEBUG_TRACE_OUT printf("Done with Reset_positions\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_the_high_lists()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: toggles the high lists (wireframe) for faster
%%%           rotations
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_the_high_lists(main_gui_t *gui)
{
    DEBUG_TRACE_IN printf("Entered draw_the_high_lists\n");

    gui->draw_high_lists = 1;
    /*if (gui->view_style == VIEW_STYLE_UNIVELS){
      glDisable(GL_BLEND);
      glDisable(GL_LIGHTING);
      }
    */
    gui->rotating_messages = 1;
  
    draw_all(gui);
    /*if (gui->view_style == VIEW_STYLE_UNIVELS) glEnable(GL_LIGHTING);*/  

    DEBUG_TRACE_OUT printf("Done with draw_the_high_lists\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Unset_High_Lists()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (eh)
%%%
%%%  Purpose: turns off the high lists from being rendered
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Unset_High_Lists (Widget w, XtPointer clientData, XEvent *event, Boolean *flag)
{
    main_gui_t *gui = (main_gui_t *)clientData;

    DEBUG_TRACE_IN  printf("Entered Unset_High_Lists\n");


    DisplayBusyCursor(gui->mainwindow);
    gui->draw_high_lists = 0;
    gui->rotating_messages = 0;
    /*if (gui->view_style == VIEW_STYLE_UNIVELS){
      glEnable(GL_BLEND);
      glEnable(GL_LIGHTING);
      }*/
    draw_all(gui);
    RemoveBusyCursor(gui->mainwindow);

    DEBUG_TRACE_OUT  printf("Done with Unset_High_Lists\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Set_High_Lists()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (eh)
%%%
%%%  Purpose: turns on the high lists
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Set_High_Lists (Widget w, XtPointer clientData, XEvent *event, Boolean *flag)
{  
    main_gui_t *gui = (main_gui_t *)clientData;
  
    DEBUG_TRACE_IN printf("Entered Set_High_Lists\n");


    /*printf("calling DisplayBusyCursor\n");
      printf("gui is : %d\n",gui);*/
    DisplayBusyCursor(gui->mainwindow);
    /*printf("done\n");*/
    gui->draw_high_lists = 1;
    gui->rotating_messages = 1;

    /*glDisable(GL_BLEND);*/
    /*if (gui->view_style == VIEW_STYLE_UNIVELS) glDisable(GL_LIGHTING);*/
    /*printf("calling draw_all\n");*/
    draw_all(gui);
    /*printf("done\n");*/

    /*glEnable(GL_BLEND);*/

    RemoveBusyCursor(gui->mainwindow);

    DEBUG_TRACE_OUT printf("Done with Set_High_Lists\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Move_with_mouse
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (eh)
%%%
%%%  Purpose: used to be used to translate the objects, currently
%%%           it is not allowed
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Move_with_mouse(Widget w,XtPointer clientData, 
                     XEvent *event, Boolean *flag)
{
    Widget position = (Widget)clientData;
    int diff_x, diff_y, centered_x, centered_y;
  
    DEBUG_TRACE_IN printf("Entered Move_with_Mouse\n");
    /*************************************************/
    /** If the motion was done with the first mouse  */
    /** button, we want to translate (x-z plane)     */
    /*************************************************/
    centered_x = event->xbutton.x - 350;
    centered_y = -(event->xbutton.y - 350);
  
    if (event->xmotion.state & Button2Mask){
        /*
          diff_x = (event->xmotion.x )  - oldx;
          diff_y = (event->xmotion.y )  - oldy;
      
          cam_pos_x -= diff_x;
          at_pos_x -= diff_x;
          cam_pos_z -= diff_y;
          at_pos_z -= diff_y;
      
          oldx = event->xmotion.x;
          oldy = event->xmotion.y;
        */
    }
    /*************************************************/
    /** If the motion was done with the first mouse  */
    /** button, rotate around the picked axis        */          
    /*************************************************/
    else if (event->xmotion.state & Button1Mask){
        /*
          if (pickedaxis != 0 ){
          switch (pickedaxis){
          case 1:
          rotation_x = calculate_angle(centered_x,centered_y);
          rotation_x -= click_difference;
          rotation_x = center_rotation_under_360(rotation_x);
          break;
          case 2:
          rotation_y = calculate_angle(centered_x,centered_y);
          rotation_y -= click_difference;
          rotation_y = center_rotation_under_360(rotation_y);
          break;
          case 3:
          rotation_z = calculate_angle(centered_x,centered_y);
          rotation_z -= click_difference;
          rotation_z = center_rotation_under_360(rotation_z);
          break;
          }
	
          if (pickedaxis == 1)	      
          {
	  if(  (((rotation_y >0)&&(rotation_y<180)) &&
          (((rotation_z >=0)&&(rotation_z<90)) ||
          ((rotation_z >270)&&(rotation_z<=360)) )) ||
          ((rotation_y >180)&&(rotation_y<360)) &&
          ((rotation_z >90)&&(rotation_z<270))  )
          {
          rotation_x = 360-rotation_x;
          }
          }
          else if (pickedaxis == 2)
          {
	  if(  (((rotation_x >90)&&(rotation_x<270)) &&
          (((rotation_z >=0)&&(rotation_z<90)) ||
          ((rotation_z >270)&&(rotation_z<=360))) )
          ||
          (((rotation_x >=0)&&(rotation_x<90))  ||
          ((rotation_x >270) &&(rotation_x<=360))) &&
          ((rotation_z >90)&&(rotation_z<270))     )
          {		
          rotation_y = 360-rotation_y;
          }
          }
          else if (pickedaxis == 3)
          {
	  if(  (((rotation_x >180)&&(rotation_x<360)) &&
          (((rotation_y >=0)&&(rotation_y<90)) ||
          ((rotation_y >270)&&(rotation_y<=360)) )) ||
          ((rotation_x >0)&&(rotation_x<180)) &&
          ((rotation_y >90)&&(rotation_y<270))  )
          {
          rotation_z = 360-rotation_z;
          }
          }	     
          }
        */
    }
    /*************************************************/
    /** Redraw with the new changes                  */
    /*************************************************/
    /*draw_all();*/

    DEBUG_TRACE_OUT printf("Done with Move_with_Mouse\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Mouse_control_changedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: changes the mouse control methods between
%%%           pulling axis, mouse, and sliders 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Mouse_control_changedCB(Widget w, XtPointer clientData, XtPointer callData)
{
    main_gui_t *gui = (main_gui_t *)clientData;
    int  ok = 1;
    XmString xmstr;
    char text[50];
    static int old_method=MOUSE;
    int new_method;
  

    DEBUG_TRACE_IN printf("Entered Mouse_control_changedCB\n");

    if (w == gui->popup_menu.button[1] || w == gui->mouse_panel.mouse_b[0]){
        new_method = SLIDER;
        /*printf("sliders have been selected, the old_methodis : %d, new : %d\n",old_method,new_method);*/
    }else{ 
        new_method = MOUSE;
        /*printf("mouse_button have been selected, the old_methodis : %d, new : %d\n",old_method,new_method);*/
    }
    gui->mouse_control_method = new_method;

    if (new_method != old_method){
        XtVaSetValues(gui->mouse_panel.mouse_b[new_method-1],XmNshadowType, XmSHADOW_IN,NULL);
        XtVaSetValues(gui->mouse_panel.mouse_b[old_method-1],XmNshadowType, XmSHADOW_OUT,NULL);
    
        switch(new_method){
            /*case 1:
              if (!gui->axis_on) gui->axis_on = 1;	
              XtAddEventHandler(gui->glxarea,ButtonPressMask, FALSE,pickAxis, (XtPointer)gui);
              XtAddEventHandler(gui->glxarea,ButtonReleaseMask,FALSE,Unhighlight, (XtPointer)gui);
              XtAddEventHandler(gui->glxarea, ButtonMotionMask, FALSE, Move_with_mouse, (XtPointer)gui); 
              break;*/
            case SLIDER:
                auto_set_window_sizes(gui,1);
                ok = 1;
                XtVaSetValues(gui->toplevel, XmNallowShellResize, TRUE, NULL);
                XtManageChild(gui->slider_panel.slider_divider);
                XtManageChild(gui->slider_panel.form);
                XtVaSetValues(gui->toplevel, XmNallowShellResize, FALSE, NULL);
                add_slider_callbacks(gui);
      
                sprintf(text,"%d",gui->rotation_x);
                xmstr = XmStringCreateLocalized(text);
                XtVaSetValues(gui->slider_panel.slider_x_numeric_label,XmNlabelString, xmstr,NULL);
                XmStringFree(xmstr);
                sprintf(text,"%d",gui->rotation_z);
                xmstr = XmStringCreateLocalized(text);
                XtVaSetValues(gui->slider_panel.slider_y_numeric_label,XmNlabelString, xmstr,NULL);
                XmStringFree(xmstr);
                sprintf(text,"%d",gui->rotation_y);
                xmstr = XmStringCreateLocalized(text);
                XtVaSetValues(gui->slider_panel.slider_z_numeric_label,XmNlabelString, xmstr,NULL);
                XmStringFree(xmstr);
      
                XmScaleSetValue(gui->slider_panel.slider_x, gui->rotation_x);
                XmScaleSetValue(gui->slider_panel.slider_y, gui->rotation_z);
                XmScaleSetValue(gui->slider_panel.slider_z, gui->rotation_y);
                break;
            case MOUSE:
                XtAddEventHandler(gui->glxarea, ButtonMotionMask, FALSE, Rotate_with_mouse, (XtPointer)gui);
      
                if (gui->fast_rotation_type != 3){
                    XtAddEventHandler(gui->glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)gui); 
                    XtAddEventHandler(gui->glxarea,ButtonReleaseMask, FALSE, Unset_High_Lists, (XtPointer)gui); 	
                }
                break;
        }

        switch(old_method){
            /*case 1:
              XtRemoveEventHandler(gui->glxarea,ButtonPressMask, FALSE,  pickAxis, (XtPointer)gui);  
              XtRemoveEventHandler(gui->glxarea,ButtonReleaseMask,FALSE, Unhighlight, (XtPointer)gui);
              XtRemoveEventHandler(gui->glxarea, ButtonMotionMask, FALSE, Move_with_mouse, (XtPointer)gui);
              break;*/
            case SLIDER:
                XtRemoveCallback(gui->slider_panel.slider_x, XmNvalueChangedCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
                XtRemoveCallback(gui->slider_panel.slider_y, XmNvalueChangedCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
                XtRemoveCallback(gui->slider_panel.slider_z, XmNvalueChangedCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
      
                XtRemoveCallback(gui->slider_panel.slider_x, XmNdragCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
                XtRemoveCallback(gui->slider_panel.slider_y, XmNdragCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
                XtRemoveCallback(gui->slider_panel.slider_z, XmNdragCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
      
                XtRemoveCallback(gui->slider_panel.slider_x, XmNdragCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
                XtRemoveCallback(gui->slider_panel.slider_y, XmNdragCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
                XtRemoveCallback(gui->slider_panel.slider_z, XmNdragCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
      
                XtAddCallback(gui->slider_panel.slider_x, XmNvalueChangedCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
                XtAddCallback(gui->slider_panel.slider_y, XmNvalueChangedCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
                XtAddCallback(gui->slider_panel.slider_z, XmNvalueChangedCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
      
                XtRemoveCallback(gui->slider_panel.slider_x,XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists,(XtPointer)gui);
                XtRemoveCallback(gui->slider_panel.slider_y,XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists,(XtPointer)gui);
                XtRemoveCallback(gui->slider_panel.slider_z,XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists,(XtPointer)gui);

                XtVaSetValues(gui->toplevel, XmNallowShellResize, TRUE, NULL);
                XtUnmanageChild(gui->slider_panel.slider_divider);
                XtUnmanageChild(gui->slider_panel.form);
                XtVaSetValues(gui->toplevel, XmNallowShellResize, FALSE, NULL);
                break;
            case MOUSE:
                XtRemoveEventHandler(gui->glxarea, ButtonMotionMask, FALSE, Rotate_with_mouse, (XtPointer)gui);
                if (gui->fast_rotation_type != 3){
                    XtRemoveEventHandler(gui->glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)gui); 
                    XtRemoveEventHandler(gui->glxarea,ButtonReleaseMask, FALSE, Unset_High_Lists, (XtPointer)gui);
                } 
                break;
        }
    
        old_method = gui->mouse_control_method;
    }
    draw_all(gui);

    DEBUG_TRACE_OUT printf("Done with Mouse_control_changedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Slider_rotateCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the rotation according to the slider value
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void Slider_rotateCB(Widget w, XtPointer clientData, XtPointer callData)
{
    main_gui_t *gui = (main_gui_t *)clientData;
    XmString xmstr;
    int temp_value;
    char out_string[50];
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;

    DEBUG_TRACE_IN printf("Entered Slider_rotatedCB\n");

    XtVaGetValues (w, XmNvalue, &temp_value, NULL);

    if (w == gui->slider_panel.slider_x){
        gui->rotation_x = cbs->value;
        sprintf(out_string,"%d",temp_value);
        xmstr = XmStringCreateLocalized(out_string);
        XtVaSetValues(gui->slider_panel.slider_x_numeric_label, XmNlabelString, xmstr, NULL);
    }else if (w == gui->slider_panel.slider_y){
        gui->rotation_z = cbs->value;
        sprintf(out_string,"%d",temp_value);
        xmstr = XmStringCreateLocalized(out_string);
        XtVaSetValues(gui->slider_panel.slider_y_numeric_label, XmNlabelString, xmstr, NULL);
    }else if (w == gui->slider_panel.slider_z){
        gui->rotation_y = cbs->value;
        sprintf(out_string,"%d",temp_value);
        xmstr = XmStringCreateLocalized(out_string);
        XtVaSetValues(gui->slider_panel.slider_z_numeric_label, XmNlabelString, xmstr, NULL);
    }
  
    /*
    if (gui->fast_rotation_type != 3)
        draw_the_high_lists(gui);
    else{
        draw_all(gui);
	}*/
    DEBUG_TRACE_OUT printf("Done with Slider_rotatedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Rotate_with_Mouse
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (eh)
%%%
%%%  Purpose: sets the global rotations according to the mouse
%%%           buttons
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Rotate_with_mouse(Widget w,XtPointer clientData, XEvent *event, Boolean *flag)
{
    /*Widget position = (Widget)clientData;*/
    main_gui_t *gui = (main_gui_t *)clientData;
    int diff_x, diff_y;
    int centered_x, centered_y, correct_x, correct_y;

    DEBUG_TRACE_IN printf("Entered Rotate_with_mouse\n");

    /*************************************************/
    /** If the motion was done with the first mouse  */
    /** button, we want to rotate around x axis      */
    /*************************************************/
    diff_x = (event->xmotion.x )  - gui->oldx;
    diff_y = (event->xmotion.y )  - gui->oldy;
  
    if (event->xmotion.state & Button1Mask){
        gui->rotation_x += diff_y;
        gui->rotation_x = center_rotation_under_360(gui->rotation_x);
        gui->oldy = event->xmotion.y;
    }
    /*************************************************/
    /** If the motion was done with the first mouse  */
    /** button, rotate around the Y axis             */     
    /*************************************************/
    else if (event->xmotion.state & Button2Mask){
        gui->rotation_y += diff_x;
        gui->rotation_y = center_rotation_under_360(gui->rotation_y);
        gui->oldx = event->xmotion.x;
    }
    /*************************************************/
    /** Redraw with the new changes                  */
    /*************************************************/
    draw_all(gui);

    DEBUG_TRACE_OUT printf("Done with Rotate_with_mouse\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Update_slider_messageCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: updates the slider message to the slider value
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Update_slider_messageCB(Widget w, XtPointer clientData, XtPointer callData)
{
    main_gui_t *gui = (main_gui_t *)clientData;
    XmString xmstr;
    int temp_value;
    char out_string[50];
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct*) callData;
  
    DEBUG_TRACE_IN printf("Entered Update_slider_messageCB\n");

    XtVaGetValues (w, XmNvalue, &temp_value, NULL);

    if (strcmp(XtName(w), "Xslider") == 0){
        sprintf(out_string,"%d",temp_value);
        xmstr = XmStringCreateLocalized(out_string);
        XtVaSetValues(gui->slider_panel.slider_x_numeric_label, XmNlabelString, xmstr, NULL);
    }else if (strcmp(XtName(w),"Yslider") == 0){
        sprintf(out_string,"%d",temp_value);
        xmstr = XmStringCreateLocalized(out_string);
        XtVaSetValues(gui->slider_panel.slider_y_numeric_label, XmNlabelString, xmstr, NULL);
    }else if (strcmp(XtName(w),"Zslider") == 0){
        sprintf(out_string,"%d",temp_value);
        xmstr = XmStringCreateLocalized(out_string);
        XtVaSetValues(gui->slider_panel.slider_z_numeric_label, XmNlabelString, xmstr, NULL);
    }

    DEBUG_TRACE_OUT printf("Done with Update_slider_messageCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : remove_slider_callbacks()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: adds all the callbacks for the rotational sliders
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void remove_slider_callbacks(main_gui_t *gui)
{
    DEBUG_TRACE_IN printf("Entered remove_slider_callbacks\n");
  
    XtRemoveCallback(gui->slider_panel.slider_x, XmNvalueChangedCallback, (XtCallbackProc)Slider_rotateCB, (XtPointer)gui);
    XtRemoveCallback(gui->slider_panel.slider_y, XmNvalueChangedCallback, (XtCallbackProc)Slider_rotateCB, (XtPointer)gui);
    XtRemoveCallback(gui->slider_panel.slider_z, XmNvalueChangedCallback, (XtCallbackProc)Slider_rotateCB, (XtPointer)gui);
  
    /*XtRemoveCallback(gui->slider_panel.slider_x, XmNdragCallback, (XtCallbackProc)Slider_rotateCB, (XtPointer)gui);
    XtRemoveCallback(gui->slider_panel.slider_y, XmNdragCallback, (XtCallbackProc)Slider_rotateCB, (XtPointer)gui);
    XtRemoveCallback(gui->slider_panel.slider_z, XmNdragCallback, (XtCallbackProc)Slider_rotateCB, (XtPointer)gui);*/
  
    XtRemoveCallback(gui->slider_panel.slider_x, XmNdragCallback, (XtCallbackProc)Update_slider_messageCB, (XtPointer)gui);
    XtRemoveCallback(gui->slider_panel.slider_y, XmNdragCallback, (XtCallbackProc)Update_slider_messageCB, (XtPointer)gui);
    XtRemoveCallback(gui->slider_panel.slider_z, XmNdragCallback, (XtCallbackProc)Update_slider_messageCB, (XtPointer)gui);
  
    XtAddCallback(gui->slider_panel.slider_x, XmNvalueChangedCallback, (XtCallbackProc)Update_slider_messageCB, (XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_y, XmNvalueChangedCallback, (XtCallbackProc)Update_slider_messageCB, (XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_z, XmNvalueChangedCallback, (XtCallbackProc)Update_slider_messageCB, (XtPointer)gui);
  
    XtRemoveCallback(gui->slider_panel.slider_x,XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists, (XtPointer)gui);
    XtRemoveCallback(gui->slider_panel.slider_y,XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists, (XtPointer)gui);
    XtRemoveCallback(gui->slider_panel.slider_z,XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists, (XtPointer)gui);

    DEBUG_TRACE_OUT printf("Done with remove_slider_callbacks\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : add_slider_callbacks()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: adds all the callbacks for the rotational sliders
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void add_slider_callbacks(main_gui_t *gui)
{
    DEBUG_TRACE_IN printf("Entered add_slider_callbacks\n");

    XtAddCallback(gui->slider_panel.slider_x, XmNdragCallback, (XtCallbackProc)Update_slider_messageCB, (XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_y, XmNdragCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_z, XmNdragCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
  
    XtAddCallback(gui->slider_panel.slider_x, XmNvalueChangedCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_y, XmNvalueChangedCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_z, XmNvalueChangedCallback, (XtCallbackProc)Update_slider_messageCB,(XtPointer)gui);
  
    XtAddCallback(gui->slider_panel.slider_x, XmNvalueChangedCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_y, XmNvalueChangedCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_z, XmNvalueChangedCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
  
    /*XtAddCallback(gui->slider_panel.slider_x, XmNdragCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_y, XmNdragCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_z, XmNdragCallback, (XtCallbackProc)Slider_rotateCB,(XtPointer)gui);	   */
  
    XtAddCallback(gui->slider_panel.slider_x,XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists,(XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_y,XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists,(XtPointer)gui);
    XtAddCallback(gui->slider_panel.slider_z,XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists,(XtPointer)gui);

    DEBUG_TRACE_OUT printf("Done with add_slider_callbacks\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : lock_all_rotations()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: prevents any rotation from taking place.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void lock_all_rotations(main_gui_t *gui)
{
    DEBUG_TRACE_IN printf("Entered lock_all_rotations\n");

    /*printf("in lock_all_rotations, mouse_control_method is : %d\n",gui->mouse_control_method);*/
    if (gui->mouse_control_method == MOUSE){
        XtRemoveEventHandler(gui->glxarea, ButtonMotionMask, FALSE, Rotate_with_mouse, (XtPointer)gui);
    }else if (gui->mouse_control_method == SLIDER){
        XtVaSetValues(gui->slider_panel.slider_x,XmNsensitive,FALSE,NULL);
        XtVaSetValues(gui->slider_panel.slider_y,XmNsensitive,FALSE,NULL);
        XtVaSetValues(gui->slider_panel.slider_z,XmNsensitive,FALSE,NULL);
    }

    DEBUG_TRACE_OUT printf("Done with lock_all_rotations\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : unlock_all_rotations()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: restores rotations.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void unlock_all_rotations(main_gui_t *gui)
{
    DEBUG_TRACE_IN printf("Entered unlock_all_rotations\n");
  
    if (gui->mouse_control_method == MOUSE)
        XtAddEventHandler(gui->glxarea, ButtonMotionMask, FALSE, Rotate_with_mouse, (XtPointer)gui);
    else if (gui->mouse_control_method == SLIDER){
        XtVaSetValues(gui->slider_panel.slider_x,XmNsensitive,TRUE,NULL);
        XtVaSetValues(gui->slider_panel.slider_y,XmNsensitive,TRUE,NULL);
        XtVaSetValues(gui->slider_panel.slider_z,XmNsensitive,TRUE,NULL);
    }

    DEBUG_TRACE_OUT printf("Done with unlock_all_rotations\n");
}


