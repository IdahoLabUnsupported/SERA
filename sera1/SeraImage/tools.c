#include "toqsh.h"
#include <X11/X.h>  /* for Cursor type */

#define PIXEL_RANGE 20
#define TIME_RANGE  400

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : DisplayBusyCursor
%%%
%%%  Written by: Youngs book
%%%
%%%  Parameters: widget
%%%
%%%  Purpose: turns the cursor to a watch
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void DisplayBusyCursor(Widget w)
{
  static Cursor cursor = (Cursor) NULL;
  if (!cursor) cursor = XCreateFontCursor(XtDisplay (w), XC_watch);
  XDefineCursor (XtDisplay(w), XtWindow (w), cursor);
  XFlush(XtDisplay (w));
  XtAppAddWorkProc (XtWidgetToApplicationContext (w),
		    (XtWorkProc)RemoveBusyCursor, (XtPointer)w);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : RemoveBusyCursor
%%%
%%%  Written by: Youngs book
%%%
%%%  Parameters: widget
%%%
%%%  Purpose: removes the watch cursor
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
Boolean RemoveBusyCursor (Widget w)
{
  XUndefineCursor(XtDisplay(w), XtWindow (w));
  return (TRUE);
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


Boolean detect_double_click ( main_gui_t *gui, XButtonEvent *event )
{
    DEBUG_TRACE_IN printf ( "Entering detect_double_click\n" );

    /* Check on double-click */
    /* press, release, press all in the same area, at same time */
    if ( ( gui->manip_gui.double_click.button == event->button ) &&
	 ( event->x < ( gui->manip_gui.double_click.x + PIXEL_RANGE ) ) &&
	 ( event->x > ( gui->manip_gui.double_click.x - PIXEL_RANGE ) ) &&
	 ( event->y < ( gui->manip_gui.double_click.y + PIXEL_RANGE ) ) &&
	 ( event->y > ( gui->manip_gui.double_click.y - PIXEL_RANGE ) ) &&
	 ( event->time < ( gui->manip_gui.double_click.time + TIME_RANGE ) ) &&
	 ( event->time < ( gui->manip_gui.double_click.time + TIME_RANGE ) ) )
    {
        /* zero out values for next check */
        gui->manip_gui.double_click.x      = ( -10 );
	gui->manip_gui.double_click.y      = ( -10 );
	gui->manip_gui.double_click.time   = ( Time ) 0;
	gui->manip_gui.double_click.button = ( -1 );

	DEBUG_TRACE_OUT  printf( "Leaving detect_double_click returning True\n" );
	return True;
    }
    else
    {
        /* save current values for next time */
        gui->manip_gui.double_click.x      = event->x;
	gui->manip_gui.double_click.y      = event->y;
	gui->manip_gui.double_click.time   = event->time;
	gui->manip_gui.double_click.button = event->button;

	DEBUG_TRACE_OUT printf( "Leaving detect_double_click returning False\n" );
	return False;
    }

}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     set_mouse_button_function
%% 
%% Purpose:      Change the label corresponding to a mouse button in the 
%%               mouse functions section of the controls frame.
%% 
%% Parameters:   gui          -> The address of a main_gui_t
%%               which_button -> An int representing the mouse button:
%%                                   0 = Left Mouse Button
%%                                   1 = Middle Mouse Button
%%                                   2 = Right Mouse Button
%%               function     -> A char *, the new function.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_mouse_button_function( main_gui_t * gui, int which_button, char * function )
{
  XmString xmstr;
  char local_string[64];

  DEBUG_TRACE_IN printf("Entering set_mouse_button_function\n");

  /* Change the label of the correct button */
  if( which_button >= 0 && which_button <= 2 ) 
  {
    strcpy( gui->mouse_section.elements[ which_button ].function, function );
    sprintf( local_string, "%-45s", gui->mouse_section.elements[ which_button ].function );
    xmstr = XmStringCreateLocalized( local_string );
    XtVaSetValues( gui->mouse_section.elements[ which_button ].function_label,
		   XmNlabelString, xmstr, 
		   NULL );

    XmStringFree( xmstr );
  }
  else
    printf("Sending invalid button number to set_mouse_button_function\n");

  DEBUG_TRACE_OUT printf("Leaving set_mouse_button_function\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     check_for_int_input
%% 
%% Purpose:      Check that input into a text widget contains only integers.
%%               Spaces, letters, and symbols will not be accepted. This 
%%               function should be registered with a Text or TextField widget
%%               as a XmNmodifyVerifyCallback.
%% 
%% Parameters:   Callback parameters. Nothing passed through clientData.
%% 
%% Return Value: If the user enters a number, the input will be added to the
%%               widget, otherwise the input will not be added.
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void check_for_int_input( Widget w, XtPointer clientData, XtPointer callData )
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) callData;
  char * local_ptr;

  if( cbs->text->ptr )      /*user didn't hit a backspace*/ 
  {
    local_ptr = cbs->text->ptr;
    if( !isdigit( (int) *local_ptr ) ||
	isspace ( (int) *local_ptr ) )
      cbs->doit = False;
  }
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     check_for_float_input
%% 
%% Purpose:      Check that input into a text widget contains only float values.
%%               Spaces, letters, and symbols will not be accepted. This 
%%               function should be registered with a Text or TextField widget
%%               as a XmNmodifyVerifyCallback.
%%
%%               Valid input is of the form [-]digit[digits].[digits]
%% 
%% Parameters:   Callback parameters. Nothing passed through clientData.
%% 
%% Return Value: If the user enters a number, the input will be added to the
%%               widget, otherwise the input will not be added.
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void check_for_float_input( Widget w, XtPointer clientData, XtPointer callData )
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) callData;
  char * local_ptr;
  int allow_dot = 0;
  int allow_minus = 0;
  char * ptr;

  if( cbs->text->length == 1 )            /* the text didn't come from a cut and paste */
  {
    if( cbs->text->ptr )                  /* user didn't hit a backspace */ 
    {
      ptr = XmTextGetString( w );         /* get the current text box's string */
    
      if( strchr( ptr, '.' ) == NULL )    /* don't allow more than one decimal point */
	allow_dot = 1;

      if( strlen( ptr ) == 0 )            /* if currently empty, we can allow a minus sign */
      {                                   /* however, don't allow .1234 */
	allow_minus = 1;                
	allow_dot = 0;
      }

      if( strlen( ptr ) == 1 && *ptr == '-' )
	allow_dot = 0;                    /* don't allow something like -.1234 */

      XtFree( ptr );

      local_ptr = cbs->text->ptr;

      if( allow_dot )
      {
	if( isdigit( (int)*local_ptr ) || *local_ptr == '.' )
	  cbs->doit = True;
	else
	  cbs->doit = False;
      }
      else
      {
	if( allow_minus )
        {
	  if( isdigit( (int) *local_ptr ) || *local_ptr == '-' )
	    cbs->doit = True;
	  else
	    cbs->doit = False;
	}
	else
        {
	  if( isdigit( (int)*local_ptr ) )
	    cbs->doit = True;
	  else
	    cbs->doit = False;
	}
      }
    }
  }
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     set_move_images_label()
%% 
%% Purpose:      Change the label of the move_images_button, which is 
%%               determined by its current state.
%% 
%% Parameters:   mi_button -> A ptr to the move_images_button member of
%%               the main_gui structure.
%% 
%% Return Value: none
%%  
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_move_images_label( move_images_t * mi_button )
{
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entering set_move_images_label\n");

  xmstr = XmStringCreateLocalized( mi_button->labels[mi_button->state] );
  XtVaSetValues( mi_button->button,
		 XmNlabelString, xmstr,
		 NULL );

  XmStringFree( xmstr );

  DEBUG_TRACE_OUT printf("Leaving set_move_images_label\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     is_allowed_callback
%% 
%% Purpose:      Determine if a button can process a callback. Right now
%%               this is just checking to see if the user is in the 
%%               Move/Remove mode.
%% 
%% Parameters:   gui -> A pointer to the main_gui_t structure.
%% 
%% Return Value: 1 if not in Move/Remove mode 
%%               0 if in Move_Reove mode
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int is_allowed_callback( main_gui_t * gui )
{
  DEBUG_TRACE_IN printf("Entering is_allowed_callback\n");

  if( gui->move_images_button.state == START_IMAGE_MOVE )
  {
      DEBUG_TRACE_OUT printf("Leaving is_allowed_callback, callback is allowed\n");      
      return( 1 );
  }
  else
  {
      DT_inform( gui->mainwindow, "You must first press the End Move/Remove button.", NULL, NULL );
      DEBUG_TRACE_OUT printf("Leaving is_allowed_callback, callback not allowed\n");
      return( 0 );
  }
}
