#include "sera3d.h"


void draw_pulling_axis(GLenum mode);
void draw_axis_type(main_gui_t *gui,GLUquadricObj *obj);
void processHits (GLint hits, GLuint buffer[]);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : AxisLabelToggleCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: toggles the axis labels on, off
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void AxisLabelToggleCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  DEBUG_TRACE_IN printf("Entered AxisLabelToggleCB\n");

  DisplayBusyCursor(gui->mainwindow);
  if (gui->axis_labels_on) gui->axis_labels_on = 0;
  else gui->axis_labels_on = 1;
  
  draw_all(gui);

  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with AxisLabelToggleCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Axis_typeCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb), new axis type passed through clientData
%%%
%%%  Purpose: switches the axis type to the new type
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Axis_typeCB(Widget w, XtPointer clientData,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int type = -1;
  int i;
  static int old_type = 0;

  DEBUG_TRACE_IN printf("Entered Axis_typeCB\n");
  
  for (i=0;i<4;i++)
    if (w == gui->axis_panel.axis_b[i])
      type = i;

  if (type == -1){
    DEBUG_TRACE_OUT printf("Done with Axis_typeCB\n");
    return;
  }

  if (type != old_type)
    {
      DisplayBusyCursor(gui->mainwindow);

      XtVaSetValues(gui->axis_panel.axis_b[old_type],XmNshadowType, XmSHADOW_OUT,NULL);
      XtVaSetValues(w,XmNshadowType, XmSHADOW_IN,NULL);
      gui->axis_type = type;
      old_type = type;

      draw_all(gui);
      RemoveBusyCursor(gui->mainwindow);
    }
  DEBUG_TRACE_OUT printf("Done with Axis_typeCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_generic_axis
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: draws the axis, based on the axis_type.
%%%           if axis_labels is on, it will draw the x+,x-, ...
%%%
%%%    New 9-19-97: based on the coordinate system, the x & y 
%%%                 axis will be switched.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_generic_axis(main_gui_t *gui)
{
  GLUquadricObj *temp_x,*temp_y;
  int x_color, y_color;
  int x_pos,x_neg,y_pos,y_neg;
  char label[8];

  DEBUG_TRACE_IN printf("Entered draw_generic_axis\n");
  
  /*   if (coord_sys == 1){*/
     x_pos = X3d_pos; x_neg = X3d_neg;
     y_pos = Y3d_pos; y_neg = Y3d_neg;
     x_color = X_AXIS_COLOR;y_color = Y_AXIS_COLOR;
     temp_x = gui->x_axis; temp_y = gui->y_axis;
     /*}else{
       x_pos = Y3d_pos; x_neg = Y3d_neg;
       y_pos = X3d_pos; y_neg = X3d_neg;
       x_color = Y_AXIS_COLOR;y_color = X_AXIS_COLOR;
       temp_x = y_axis; temp_y = x_axis;
     }*/
     
     /*if(gui->view_style >= VIEW_STYLE_NURBS && !gui->draw_high_lists)
       set_material(gui,x_color,1);*/
     /*else*/ set_color(x_color);

     glPushMatrix();
      glTranslatef(5,0,0);
       glPushMatrix();
        glRotatef(90,0,1,0);
        draw_axis_type(gui,temp_x);

        if (gui->axis_type == 1)
	  {
	    glPushMatrix();
	    glRotatef(180,0,1,0);
	    draw_axis_type(gui,temp_x);
	    glPopMatrix();
	    if (gui->axis_labels_on)
	      {
		glPushMatrix();
		glRotatef(-90,0,1,0);

                /* Get the label for this axis */
                sprintf( label, "%c", gui->axisLabels[COLUMN_AXIS].first );
		display_string(label,-165,0,0);
		glPopMatrix();
	      }
	  }
	glPopMatrix();
	
	if (gui->axis_labels_on){
	  glPushMatrix();

          /* Get the label for this axis */
          sprintf( label, "%c", gui->axisLabels[COLUMN_AXIS].last );          
	  display_string(label,165,0,0);
	  glPopMatrix();
	}

	glPopMatrix();
	
	/*if(gui->view_style >= VIEW_STYLE_UNIVELS && !gui->draw_high_lists)
	  set_material(gui,y_color,1);
	  else*/
    set_color(y_color);
  
  glPushMatrix();
    glTranslatef(0,0,5);
    glPushMatrix();
      draw_axis_type(gui,gui->y_axis);
      if (gui->axis_type == 1)
	{
	  glPushMatrix();
	  glRotatef(180,1,0,0);
	  draw_axis_type(gui,temp_y);
	  glPopMatrix();
      
      if (gui->axis_labels_on)
	{
		glPushMatrix();

                /* Get the label for this axis */
                sprintf( label, "%c", gui->axisLabels[ROW_AXIS].last );
		display_string(label,0,0,-165);
		glPopMatrix();
	     }
	 }
       glPopMatrix();

       if (gui->axis_labels_on)
	 {
	   glPushMatrix();

           /* Get the label for this axis */
           sprintf( label, "%c", gui->axisLabels[ROW_AXIS].first );
	   display_string(label,0,0,165);
	   glPopMatrix();
	 }

     glPopMatrix();

     /*if(gui->view_style >= VIEW_STYLE_UNIVELS && !gui->draw_high_lists)
    set_material(gui,Z_AXIS_COLOR,1);
    else*/
    set_color(Z_AXIS_COLOR);

     glPushMatrix();
      glTranslatef(0,5,0);
      glPushMatrix();
        glRotatef(-90,1,0,0);
	draw_axis_type(gui,gui->z_axis);
	if (gui->axis_type == 1)
	  {
	    glPushMatrix();
	    glRotatef(180,0,1,0);
	    draw_axis_type(gui,gui->z_axis);
	    glPopMatrix();
	  
	    if (gui->axis_labels_on)
	      {
		glPushMatrix();
		glRotatef(90,1,0,0);

                /* Get the label for this axis */
                sprintf( label, "%c", gui->axisLabels[SLICE_AXIS].first );
		display_string(label,0,-165,0);
		glPopMatrix();
	      }
	  }
	glPopMatrix();
	
	if (gui->axis_labels_on)
	  {
		glPushMatrix();

                /* Get the label for this axis */
                sprintf( label, "%c", gui->axisLabels[SLICE_AXIS].last );
		display_string(label,0,165,0);
		glPopMatrix();
	  }
   glPopMatrix();	

  DEBUG_TRACE_OUT printf("Done with draw_generic_axis\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_axis_type
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the glu object (the axis)
%%%
%%%  Purpose: the bottom level axis drawing, calls the gluCylinder
%%%           to draw a cylindrical axis.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_axis_type(main_gui_t *gui,GLUquadricObj *obj)
{
  DEBUG_TRACE_IN printf("Entered draw_axis_type\n");

  switch(gui->axis_type)
    {
    case 0:       
            break;
    case 1:
    case 2:
            gluCylinder(obj,1,.5,150,10,10);break;   
    case 3:
            gluCylinder(obj,2,1.5,150,10,10); break;
 /* case 4:
            gluPartialDisk(obj,140,142,50,50,0,360);
	    break;*/
    } 
  DEBUG_TRACE_OUT printf("Done with draw_axis_type\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_axis
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: initializes the axis (glu) data types.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_axis(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered build_axis\n");

  gui->x_axis = gluNewQuadric();
  gui->y_axis = gluNewQuadric();
  gui->z_axis = gluNewQuadric();

  gui->x_sphere = gluNewQuadric();  
  gui->y_sphere = gluNewQuadric();  
  gui->z_sphere = gluNewQuadric();

  DEBUG_TRACE_OUT printf("Done with build_axis\n");
}




/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_pulling_axis
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the GL rendering mode
%%%
%%%  Purpose: displays the axis designed for grabbing & 
%%%           pulling with the mouse for rotation.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void draw_pulling_axis(GLenum mode){
DEBUG_TRACE_IN printf("Entered draw_pulling_axis\n");


if(view_style >= VIEW_STYLE_UNIVELS && !draw_high_lists)
set_material(X_AXIS_COLOR,1);
else
set_color(X_AXIS_COLOR);

if (mode != GL_SELECT){
glPushMatrix();
glTranslatef(5,0,0);
glRotatef(90,0,1,0);
gluCylinder(x_axis,1,.5,150,10,10);
glPopMatrix();
}

glPushMatrix();

glTranslatef(155,0,0);

if(view_style >= VIEW_STYLE_UNIVELS && !draw_high_lists)
set_material(Z_AXIS_COLOR,1);
else
set_color(Z_AXIS_COLOR);

if(mode == GL_SELECT)
glLoadName(2);     

glPushMatrix(); 
if ((highlight_on) && (pickedaxis == 2))
glScalef(2,2,2);
glTranslatef(0,0,3);
gluSphere(y_sphere,3,10,10);
glPopMatrix();

if(view_style >= VIEW_STYLE_UNIVELS && !draw_high_lists)
set_material(Y_AXIS_COLOR,1);
else
set_color(Y_AXIS_COLOR);

if(mode == GL_SELECT)
glLoadName(3);     

glPushMatrix();
if ((highlight_on) && (pickedaxis == 3))
glScalef(2,2,2);
glTranslatef(0,0,-3);
gluSphere(z_sphere,3,10,10);
glPopMatrix();

glPopMatrix();
 
if(view_style >= VIEW_STYLE_UNIVELS && !draw_high_lists)
set_material(Y_AXIS_COLOR,1);
else
set_color(Y_AXIS_COLOR);

if (mode != GL_SELECT){
glPushMatrix();
glTranslatef(0,0,5);

gluCylinder(z_axis,1,.5,150,10,10);

glPopMatrix();
}

glPushMatrix();

glTranslatef(0,0,155);

if(view_style >= VIEW_STYLE_UNIVELS && !draw_high_lists)
set_material(X_AXIS_COLOR,1);
else
set_color(X_AXIS_COLOR);

if(mode == GL_SELECT)
glLoadName(1);     

glPushMatrix(); 
if ((highlight_on) && (pickedaxis == 1))
glScalef(2,2,2);
glTranslatef(3,0,0);
gluSphere(x_sphere,3,10,10);
glPopMatrix();


if(view_style >= VIEW_STYLE_UNIVELS && !draw_high_lists)
set_material(Z_AXIS_COLOR,1);
else
set_color(Z_AXIS_COLOR);

if(mode == GL_SELECT)
glLoadName(2);     

glPushMatrix();
if ((highlight_on) && (pickedaxis == 2))
glScalef(2,2,2);
glTranslatef(-3,0,0);
gluSphere(y_sphere,3,10,10);
glPopMatrix();

glPopMatrix();

if(view_style >= VIEW_STYLE_UNIVELS && !draw_high_lists)
set_material(Z_AXIS_COLOR,1);
else
set_color(Z_AXIS_COLOR);

if (mode != GL_SELECT){
glPushMatrix();
glTranslatef(0,5,0);
glRotatef(-90,1,0,0);
gluCylinder(y_axis,1,.5,150,10,10);
glPopMatrix();
}

glPushMatrix();

glTranslatef(0,155,0);

if(view_style >= VIEW_STYLE_UNIVELS && !draw_high_lists)
set_material(X_AXIS_COLOR,1);
else
set_color(X_AXIS_COLOR);

if(mode == GL_SELECT)
glLoadName(1);     

glPushMatrix(); 
if ((highlight_on) && (pickedaxis == 1))
glScalef(2,2,2);
glTranslatef(3,0,0);
gluSphere(x_sphere,3,10,10);
glPopMatrix();


if(view_style >= VIEW_STYLE_UNIVELS && !draw_high_lists)
set_material(Y_AXIS_COLOR,1);
else
set_color(Y_AXIS_COLOR);

if(mode == GL_SELECT)
glLoadName(3);     

glPushMatrix();
if ((highlight_on) && (pickedaxis == 3))
glScalef(2,2,2);
glTranslatef(-3,0,0);
gluSphere(z_sphere,3,10,10);
glPopMatrix();

glPopMatrix();

DEBUG_TRACE_OUT printf("Done with draw_pulling_axis\n");
}
*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : rotate_around_picked_axis
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void rotate_around_picked_axis(){
DEBUG_TRACE_IN printf("Entered rotate_around_picked_axis\n");

switch (pickedaxis){
case 1:
glRotatef(rotation_y,0,1,0);
glRotatef(rotation_z,0,0,1);
glRotatef(rotation_x,1,0,0);      
break;
case 2:
glRotatef(rotation_x,1,0,0);
glRotatef(rotation_z,0,0,1);    
glRotatef(rotation_y,0,1,0);
break;
case 3:
glRotatef(rotation_x,1,0,0);
glRotatef(rotation_y,0,1,0);
glRotatef(rotation_z,0,0,1);
break;
}

DEBUG_TRACE_OUT printf("Done with rotate_around_picked_axis\n");
}
*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : pickAxis
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (ev)
%%%
%%%  Purpose: using GL SELECT rendering mode, this procedure
%%%           determines which axis was selected by the mouse
%%%           and sets that axis to the current axis for 
%%%           rotation.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void pickAxis(Widget w, XtPointer clientData,
	      XEvent *event, Boolean *flag)
{
  int hits;
  GLint selectBuf[BUFSIZE];
  GLint viewport[4];
  int x,y;

  DEBUG_TRACE_IN printf("Entered pickAxis\n");

  x = event->xbutton.x;
  y = event->xbutton.y;

  glGetIntegerv(GL_VIEWPORT,(GLint *)viewport);


  glSelectBuffer((GLint)BUFSIZE,(GLuint *)selectBuf);

  (void) glRenderMode (GL_SELECT);

  glInitNames();
  glPushName(-1);

  glPushMatrix();
  glMatrixMode(GL_PROJECTION);
     glScalef(scaling, scaling, scaling);

   glLoadIdentity();
   gluPickMatrix((GLdouble)x,(GLdouble)(viewport[3] - y), 5,5,
		 (GLint *)viewport);
   glFrustum (-20, 20, -20, 20,50.0, 800.0);

   glMatrixMode(GL_MODELVIEW);

   glClearColor(0,0,0,0);
   glClear(GL_COLOR_BUFFER_BIT);

   rotate_around_picked_axis();

   draw_pulling_axis(GL_SELECT);


   glPopMatrix();
   
   glFlush();
 
  
  hits = glRenderMode(GL_RENDER);

  processHits(hits, (GLuint *)selectBuf);



  glMatrixMode (GL_PROJECTION);	
  glLoadIdentity ();	
  glFrustum (-20, 20, -20, 20,50.0, 800.0);	
  glMatrixMode (GL_MODELVIEW);	

  glViewport(0, 0, viewport[2], viewport[3]);

  DEBUG_TRACE_OUT printf("Done with pickAxis\n");
}
*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : processHits
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the # of hits, and a buffer that holds them
%%%
%%%  Purpose: when the mouse is pressed using GL_SELECT mode,
%%%           hits are generated if the mouse is over a gl object.
%%%           by checking through the hit list, we can determine
%%%           which axis was hit.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void processHits (GLint hits, GLuint buffer[])
{
  unsigned int i, j;
  GLuint ii, jj, names, *ptr;
  
  DEBUG_TRACE_IN printf("Entered processHits\n");

  ptr = (GLuint *) buffer;
  
  for (i = 0; i < hits; i++) {	
    names = *ptr;
    printf (" number of names for this hit = %d\n", names); ptr++;
    printf ("  z1 is %u;", *ptr); ptr++;
    printf ("  z2 is %u\n", *ptr); ptr++;
    for (j=0;j<names;j++)
      {
	pickedaxis = *ptr;
	printf("The Axis picked is : %d\n",pickedaxis);
	ptr++;
      }
  }
  DEBUG_TRACE_OUT printf("Done with processHits\n");
}
*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Unhighlight
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (eh)
%%%
%%%  Purpose: simply shuts off highlighting of the picked axis.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void Unhighlight(Widget w,XtPointer clientData, 
		       XEvent *event, Boolean *flag)
{
  DEBUG_TRACE_IN printf("Entered Unhighlight\n");

  highlight_on = 0;
  draw_all();

  DEBUG_TRACE_OUT printf("Done with Unhighlight\n");
}
*/
