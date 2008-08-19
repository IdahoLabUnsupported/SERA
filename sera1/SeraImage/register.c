/* =========================================================================
 *
 * File:    register.c
 *
 * Purose:  Image registration routines
 *
 * History: Harkin, 6/99
 *
 * ==========================================================================
 */

#include <math.h>
#include "toqsh.h"
#include "strings.h"

/*
 *  Local prototypes
 */

static void BuildPopupMenu (register_gui_t *, register_image_t *, int);

static void RegisterFreeUndoMemory (register_gui_t *);
static int RegisterLoadQsh (register_gui_t *, char *, int);
static int QshBuildFilenames (char *, char *, char *);
static void UnloadImages (register_gui_t *, int);
static void UnloadStrip (image_set_t *);
static void BuildImageDisplay (register_gui_t *, image_set_t *, int, int);


static void RegisterColormap (register_gui_t *,Widget, Colormap);
static int InitColormap (register_gui_t *); 
static void IncreaseColorDepth (register_gui_t *, unsigned char *, unsigned int); 

static void InitializeGui (register_gui_t *);

static Boolean  InImageSet (Widget, image_set_t *);
register_image_t *GetImage ( Widget, register_gui_t *, int *, image_set_t **);
register_image_t *GetImageByMenu 
(  Widget, 
   register_gui_t *, 
   int *, 
   image_set_t **
);

static int  InRightSet (Widget, register_gui_t *);

static void ImageExposeCB (Widget, XtPointer, XtPointer);
static void ImageClickEH ( Widget, XtPointer, XEvent *, Boolean *);
static void HighlightImageEH ( Widget, XtPointer, XEvent *, Boolean *);
static void DehighlightImageEH ( Widget, XtPointer, XEvent *, Boolean *);

static void LeftButtonClick 
   (XButtonEvent *, register_gui_t *, image_set_t *, register_image_t *, int);

static void RightButtonClick 
   (XButtonEvent *, register_gui_t *, register_image_t *);

void DrawCross (register_gui_t *, int, int, register_image_t *);
void UndrawCross (register_gui_t *, int, int, register_image_t *);
void DrawPoint (register_gui_t *, int, int, register_image_t *);
void UndrawPoint (register_gui_t *, int, int, register_image_t *);

static void LinkImageCB (Widget, XtPointer, XtPointer);
static void LinkCancelCB (Widget, XtPointer, XtPointer);
static void LinkClickEH ( Widget, XtPointer, XEvent *, Boolean *);
static int ProcessLinking (register_gui_t *, image_set_t *, int);
static void StartLink (register_gui_t *, image_set_t *, int);

static void StartAlternatePoint (register_gui_t *, image_set_t *, int);
/* static void AltCancelCB (Widget, XtPointer, XtPointer); */
static int StopAlternatePoint (register_gui_t *, image_set_t *, int);

static void ZoomImageCB (Widget, XtPointer, XtPointer);
static void RegisterImageCB (Widget, XtPointer, XtPointer);

unsigned char *RegisterOneImage 
   (  register_gui_t *, 
      register_image_t *, int, int,
      register_image_t *, int, int
   );
static unsigned char RegisterAllImages 
   (register_gui_t *, image_set_t *, image_set_t *);

static void ClearPointsCB (Widget, XtPointer, XtPointer);
static void RemoveMarkCB (Widget, XtPointer, XtPointer);
void RemoveMark (register_gui_t *, image_set_t *, register_image_t *, int);

static void UndoPointsCB (Widget, XtPointer, XtPointer);
static void UndoMark (register_gui_t *, register_image_t *);
static void UnlinkImagesCB (Widget, XtPointer, XtPointer);

void RefreshImageSet (register_gui_t *, image_set_t *);
void ChangeMarkColor (register_gui_t *, int);

static int LinkImages 
   (  register_gui_t *, image_set_t *, register_image_t *, int, 
      image_set_t *, register_image_t *, int
   );
static void UnlinkImage 
   (register_gui_t *, image_set_t *, register_image_t *, int);

static void ClearPoints (register_gui_t *, register_image_t *);

static void CenterMarkCB (Widget, XtPointer, XtPointer);
static void OutlineMarkCB (Widget, XtPointer, XtPointer);
static unsigned char *ExtractSubimage 
(  register_gui_t *, image_set_t *, register_image_t *,
   int, int, int
);

image_set_t *OtherImageSet (register_gui_t *, image_set_t *);

/*
 * Local definitions
 */

void MenuPopup (Widget, XEvent *, String *, Cardinal);

/*
 * Callbacks first, then supporting routines.
 */

/* =========================================================================
 *
 * Function:   Starting point for registration.  Pop up the widget and
 *             process user requests.
 *
 * Syntax:     void RegisterImages (widget, client_data, call_data)
 *                Standard X callback.
 *
 * History:    Harkin, 6/99
 *
 * Notes:
 *
 * ==========================================================================
 */

void RegisterImagesCB 
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *register_gui = (register_gui_t *) client_data;

   DEBUG_TRACE_IN printf ( "Entering RegisterImagesCB\n" ); 
   
   /*
    * Initialize variables as needed
    */
   InitializeGui (register_gui);

   register_gui->user_done = False;
   register_gui->left.loaded = False;
   register_gui->right.loaded = False;
   register_gui->linking = False;
   register_gui->alternating = False;

   if (InitColormap (register_gui) == 0)
      return;

   /*
    * Manage the widget and process events
    */

   XtManageChild (register_gui->shell);  

   while (!register_gui->user_done)
   {
      XtAppProcessEvent (register_gui->app, XtIMAll);
   }

   DEBUG_TRACE_OUT printf ( "RegisterImagesCB\n" ); 

}

/* =========================================================================
 *
 * Function:   Open a file - not currently used
 *
 * Syntax:     void RegisterOpenFileCB (widget, client_data, call_data)
 *                Standard X callback.
 *
 * History:    Harkin, 6/99
 *
 * Notes:    
 *
 * ==========================================================================
 */

void RegisterOpenFileCB
(
   Widget    w, 
   XtPointer client_data,
   XtPointer call_data            
)
{
   DEBUG_TRACE_IN printf ("Entering RegisterOpenFileB\n"); 
   DEBUG_TRACE_OUT printf ("Leaving RegisterOpenFileB\n"); 

   return;
}


/* =========================================================================
 *
 * Function:   Open a file for the image strips - left or right.
 *
 * Syntax:     void RegisterOpenLeftFileCB (widget, client_data, call_data)
 *             void RegisterOpenRightFileCB (widget, client_data, call_data)
 *                Standard X callback.
 *
 * History:    Harkin, 6/99
 *
 * Notes:    
 *
 * ==========================================================================
 */

void RegisterOpenLeftFileCB
(
   Widget    w, 
   XtPointer client_data,
   XtPointer call_data            
)
{
   char         filename [256];
   register_gui_t   *register_gui = (register_gui_t *) client_data;

   DEBUG_TRACE_IN printf ("Entering RegisterOpenLeftFileB\n"); 


   if (register_gui->left.loaded)
      UnloadImages (register_gui, LEFT_SIDE);

   /*
    * Go get the name and if OK, load the file.
    */

   if (DT_select_file (register_gui->toplevel, register_gui->app, filename, "Load QSH")) 
   {
      if (FT_fileExists (filename))
      {
         if (is_a_valid_qsh_file (filename))
            DisplayBusyCursor (register_gui->mainwindow);
            DisplayBusyCursor (register_gui->toplevel);

            strcpy (register_gui->left.name, filename);
            if (RegisterLoadQsh (register_gui, filename, LEFT_SIDE) == 0)
               register_gui->left.loaded = False;

            RemoveBusyCursor (register_gui->mainwindow);
            RemoveBusyCursor (register_gui->toplevel);
      }
   }
   else
   {
      DT_error 
      (  register_gui->toplevel, 
         "Not a valid qsh file", 
         "File Error", 
         NULL
      );
      return;
   }
 
   DEBUG_TRACE_OUT printf ( "Leaving RegisterOpenLeftFileCB\n" );  

   return;
}

void RegisterOpenRightFileCB
(
   Widget    w, 
   XtPointer client_data,
   XtPointer call_data            
)
{
   char         filename [256];
   register_gui_t   *register_gui = (register_gui_t *) client_data;

   DEBUG_TRACE_IN printf ("Entering RegisterOpenRightFileB\n"); 


   if (register_gui->right.loaded)
      UnloadImages (register_gui, RIGHT_SIDE);

   /*
    * Go get the name and if OK, load the file.
    */

   if (DT_select_file (register_gui->toplevel, register_gui->app, filename, "Load QSH")) 
   {
      if (FT_fileExists (filename))
      {
         if (is_a_valid_qsh_file (filename))
            DisplayBusyCursor (register_gui->mainwindow);
            DisplayBusyCursor (register_gui->toplevel);

            strcpy (register_gui->left.name, filename);
            if (RegisterLoadQsh (register_gui, filename, RIGHT_SIDE) == 0)
               register_gui->left.loaded = False;

            RemoveBusyCursor (register_gui->mainwindow);
            RemoveBusyCursor (register_gui->toplevel);
      }
   }
   else
   {
      DT_error 
      (  register_gui->toplevel, 
         "Not a valid qsh file", 
         "File Error", 
         NULL
      );
      return;
   }
 

   DEBUG_TRACE_OUT printf ( "Leaving RegisterOpenRightFileCB\n" );  

   return;
}

/* =========================================================================
 *
 * Function:   Close a file for the image strips - left or right.
 *
 * Syntax:     void RegisterCloseLeftFileCB (widget, client_data, call_data)
 *             void RegisterCloseRightFileCB (widget, client_data, call_data)
 *                Standard X callback.
 *
 * History:    Harkin, 6/99
 *
 * Notes:    
 *
 * ==========================================================================
 */

void RegisterCloseLeftFileCB
(
   Widget    w, 
   XtPointer client_data,
   XtPointer call_data            
)
{
   register_gui_t   *register_gui = (register_gui_t *) client_data;

   DEBUG_TRACE_IN printf ("Entering RegisterOpenFileB\n"); 

   UnloadImages (register_gui, LEFT_SIDE);

   DEBUG_TRACE_OUT printf ( "Leaving RegisterOpenFileCB\n" );  

   return;
}

void RegisterCloseRightFileCB
(
   Widget    w, 
   XtPointer client_data,
   XtPointer call_data            
)
{
   register_gui_t   *register_gui = (register_gui_t *) client_data;

   DEBUG_TRACE_IN printf ("Entering RegisterOpenFileB\n"); 

   UnloadImages (register_gui, RIGHT_SIDE);

   DEBUG_TRACE_OUT printf ( "Leaving RegisterOpenFileCB\n" );  

   return;
}


/* =========================================================================
 *
 * Function:   Save a file from the registration process.
 *
 * Syntax:     void RegisterSaveFileCB (widget, client_data, call_data)
 *                Standard X callback.
 *
 * History:    Harkin, 6/99
 *
 * Notes:    
 *
 * ==========================================================================
 */

void RegisterSaveFileCB
(
   Widget    w, 
   XtPointer client_data,
   XtPointer call_data            
)
{
   register_gui_t   *register_gui = (register_gui_t *) client_data;

   DEBUG_TRACE_IN printf ("Entering RegisterSaveFileB\n"); 


   DEBUG_TRACE_OUT printf ( "Leaving RegisterSaveFileCB\n" );  

   return;
}

/* =========================================================================
 *
 * Function:   Change the image size.
 *
 * Syntax:     void RegisterSizeChangedCB (widget, client_data, call_data)
 *                Standard X callback.
 *
 * Notes:    
 *
 * ==========================================================================
 */

void RegisterSizeChangeCB
(
   Widget    w,
   XtPointer client_data,
   XtPointer call_data
)
{
   register_gui_t   *reg_gui = (register_gui_t *) client_data;
   image_set_t      *image_set;
   int              sizex, sizey, ct, imagenum;
   float            ratio;

   DEBUG_TRACE_IN printf ( "Entering RegisterSizeChangeCB\n" );  

   if (w == reg_gui->menubar.size_128)
      reg_gui->default_width = 128;
   else if (w == reg_gui->menubar.size_256)
      reg_gui->default_width = 256;
   else if (w == reg_gui->menubar.size_512)
      reg_gui->default_width = 512;
   else
      return;

   if (reg_gui->left.loaded)
   {
      UnloadStrip(&(reg_gui->left));
      XtVaSetValues 
      (  reg_gui->lstrip_form,
         XmNwidth, reg_gui->default_width + 40,
         NULL
      );
      image_set = &(reg_gui->left);


      sizex = image_set->info->size_of_dimension [1];
      sizey = image_set->info->size_of_dimension [2];
      ratio = (float) sizex / (float) sizey;
      image_set->di_width = reg_gui->default_width;
      image_set->di_height = (int) (image_set->di_width * ratio + 0.5);
      for (ct = 0; ct < image_set->numimages; ct ++)
      {
         /*
          * Initialize some image data values.
          */
  
         image_set->images[ct].zoom_factor = 1;
         image_set->images[ct].xoffset = 0;
         image_set->images[ct].yoffset = 0;

         BuildImageDisplay (reg_gui, image_set, ct, LEFT_SIDE);
      }
   }
   
   if (reg_gui->right.loaded)
   {
      UnloadStrip(&(reg_gui->right));
      XtVaSetValues 
      (  reg_gui->rstrip_form,
         XmNwidth, reg_gui->default_width + 40,
         NULL
      );
      image_set = &(reg_gui->left);

      sizex = image_set->info->size_of_dimension [1];
      sizey = image_set->info->size_of_dimension [2];
      ratio = (float) sizex / (float) sizey;
      image_set->di_width = reg_gui->default_width;
      image_set->di_height = (int) (image_set->di_width * ratio + 0.5);
      for (ct = 0; ct < image_set->numimages; ct ++)
      {
         /*
          * Initialize some image data values.
          */
  
         image_set->images[ct].zoom_factor = 1;
         image_set->images[ct].xoffset = 0;
         image_set->images[ct].yoffset = 0;

         BuildImageDisplay (reg_gui, image_set, ct, RIGHT_SIDE);
      }
   }

   DEBUG_TRACE_OUT printf ( "Leaving RegisterSizeChangeCB\n" );  
}

/* =========================================================================
 *
 * Function:   Change the registration type.
 *
 * Syntax:     void RegisterModelChangeCB (widget, client_data, call_data)
 *                Standard X callback.
 *
 * Notes:    
 *
 * ==========================================================================
 */

void RegisterModelChangeCB
(
   Widget    w,
   XtPointer client_data,
   XtPointer call_data
)
{
   register_gui_t   *register_gui = (register_gui_t *) client_data;
   DEBUG_TRACE_IN printf ( "Entering RegisterModelChangeCB\n" );  

   if (w == register_gui->menubar.model_lreg)
      register_gui->menubar.model = LREG;
   else
      register_gui->menubar.model = LREG;

   DEBUG_TRACE_OUT printf ( "Leaving RegisterModelChangeCB\n" );  
}


/* =========================================================================
 *
 * Function:   Change the point correlation method.
 *
 * Syntax:     void RegisterMethodChangeCB (widget, client_data, call_data)
 *                Standard X callback.
 *
 * Notes:    
 *
 * ==========================================================================
 */

void RegisterMethodChangeCB
(
   Widget    w,
   XtPointer client_data,
   XtPointer call_data
)
{
   register_gui_t   *register_gui = (register_gui_t *) client_data;

   DEBUG_TRACE_IN printf ( "Entering RegisterMethodChangeCB\n" );  

   if (w == register_gui->menubar.method_alternate)
      register_gui->menubar.method = ALT_POINTS;
   else if (w == register_gui->menubar.method_link)
      register_gui->menubar.method= LINKING;
   else if (w == register_gui->menubar.method_overlay)
      register_gui->menubar.method= OVERLAY;
   else
      register_gui->menubar.method = ALT_POINTS;

   DEBUG_TRACE_OUT printf ( "Leaving RegisterMethodChangeCB\n" );  
}



/* =========================================================================
 *
 * Function:   Exit from the registration process
 *
 * Syntax:     void RegisterSaveFileCB (widget, client_data, call_data)
 *                Standard X callback.
 *
 * History:    Harkin, 6/99
 *
 * Notes:    
 *
 * ==========================================================================
 */

void RegisterExitCB
(
   Widget    w,
   XtPointer client_data,
   XtPointer call_data
)
{
   int          dismiss_confirmed;

   register_gui_t   *register_gui = (register_gui_t *) client_data;


   DEBUG_TRACE_IN printf ("Entering RegisterExitCB\n");

   /*
    * Check to see if changes are to be discarded.
    */

/*
   if (gui->register_gui->undo_list->next_undo != NULL)
   {
      if (DT_decide(gui->toplevel,gui->app,
                    CONFIRM_RESLICE_DISMISS,NULL,NULL,NULL))
         dismiss_confirmed = 1;
       else
         dismiss_confirmed = 0;
   }
*/

   if (!dismiss_confirmed)
      return;

   /*
    * Clear the undo list and make undo button insensitive.
    */

/*
   if (gui->register_gui->undo_list != NULL)
      RegisterFreeUndoMemory ( gui );

   XtVaSetValues 
   (  gui->register_gui->button_panel.undo,
      XmNsensitive,
      FALSE, NULL
   );
*/

  
   /*
    * Set user done for event processing and pop down.
    */

   register_gui->user_done = 1;

   if (register_gui->left.loaded)
      UnloadImages (register_gui, LEFT_SIDE);
   if (register_gui->right.loaded)
      UnloadImages (register_gui, RIGHT_SIDE);

   XtUnmanageChild (register_gui->shell);


   DEBUG_TRACE_OUT printf ( "Leaving RegisterExitCB\n" );

   return;
}


/* =========================================================================
 *
 * Function:   The application to all or one image method changed callback.
 *
 * Syntax:     void RegisterSaveFileCB (widget, client_data, call_data)
 *                Standard X callback.
 *
 * History:    Harkin, 6/99
 *
 * Notes:    
 *
 * ==========================================================================
 */

void RegisterApplyMethodChangeCB 
(
   Widget    w, 
   XtPointer client_data,
   XtPointer call_data            
)
{
   register_gui_t   *register_gui = (register_gui_t *) client_data;

   DEBUG_TRACE_IN printf ( "Entering RegisterApplyMethodChangeCB\n" ); 

   DEBUG_TRACE_OUT printf ( "Leaving RegisterApplyMethodChangeCB\n" ); 

   return;
}

/* ========================================================================
 *
 * Function: Handle context sensitive help hints for registration.
 *
 * Syntax:   RegisterInfoEH (w, clientdata, event, flag)
 *              Standard event handler syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
*/

void RegisterInfoEH 
(
   Widget       w,
   XtPointer    clientdata,
   XEvent       *event,
   Boolean      *flag
)
{
   register_gui_t     *register_gui = (register_gui_t *)clientdata;
   XmString       xmstr;

   DEBUG_TRACE_IN printf ( "Entering RegisterInfoEH\n" ); 

   /*
    * Start with the general message.
    */

   if (w == register_gui->form)
      xmstr = XmStringCreateLocalized ("             ");

   /*
    * Strip buttons
    */

   else if (w == register_gui->lstrip_buttons.open)
      xmstr = XmStringCreateLocalized (LSTRIP_OPEN);
   else if (w == register_gui->lstrip_buttons.close)
      xmstr = XmStringCreateLocalized (LSTRIP_CLOSE);
   else if (w == register_gui->rstrip_buttons.open)
      xmstr = XmStringCreateLocalized (RSTRIP_OPEN);
   else if (w == register_gui->rstrip_buttons.close)
      xmstr = XmStringCreateLocalized (RSTRIP_CLOSE);

   else if (w == register_gui->menubar.model_menu)
      xmstr = XmStringCreateLocalized (MODEL_MENU);
   else if (w == register_gui->menubar.model_lreg)
      xmstr = XmStringCreateLocalized (LREG_MENU);

   else if (w == register_gui->menubar.method_menu)
      xmstr = XmStringCreateLocalized (METHOD_MENU);
   else if (w == register_gui->menubar.method_alternate)
      xmstr = XmStringCreateLocalized (ALT_PTS);

   else if (w == register_gui->menubar.cmd_menu)
      xmstr = XmStringCreateLocalized (CMD_MENU);
   else if (w == register_gui->menubar.apply_to_all)
      xmstr = XmStringCreateLocalized (APPLYALL_CMD);
   else if (w == register_gui->menubar.revert)
      xmstr = XmStringCreateLocalized (RESET_CMD);
   else if (w == register_gui->menubar.remove_all)
      xmstr = XmStringCreateLocalized (REMOVEALL_CMD);
   else if (w == register_gui->menubar.interpolate_marks)
      xmstr = XmStringCreateLocalized (INTERPOLATE_CMD);

   /*
    * images
    */

   else if (InImageSet (w, &(register_gui->left)))
      xmstr = XmStringCreateLocalized (LSTRIP_IMAGE);
   else if (InImageSet (w, &(register_gui->right)))
      xmstr = XmStringCreateLocalized (RSTRIP_IMAGE);
   
   /*
    * Nothing to hint for.
    */

   else
      xmstr = XmStringCreateLocalized ("               ");

   XtVaSetValues 
   (  register_gui->information,
      XmNlabelString, xmstr, NULL
   );
   XmStringFree (xmstr);

   DEBUG_TRACE_OUT printf ( "Leaving RegisterInfoEH\n" );
   return;
}


/* ========================================================================
 *
 * Function: Free memory for the undo list.
 *
 * Syntax:   RegisterFreeUndoMemory (register_gui_t *gui)
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
*/

void RegisterFreeUndoMemory (register_gui_t *register_gui)
{
   DEBUG_TRACE_IN printf ( "Entering RegisterImagesCB\n" ); 
   DEBUG_TRACE_OUT printf ( "Leaving RegisterExitCB\n" );
   return;
}


/* ========================================================================
 *
 * Function: Load a qsh file into the qsh_info_t structure and
 *           build the image structures for registration.
 *
 * Syntax:   err = RegisterLoadQsh (gui, filename, side)
 *              Load filename if possible for registration part side.
 *              Return 0 for an error.
 *
 * History:  Harkin, 6/99
 *
 * Notes:    Relies heavily on the information in libqsh.h and the code
 *           in libqsh.c.
 *
 * =========================================================================
*/

int RegisterLoadQsh 
(
   register_gui_t   *reg_gui,
   char         *filename,
   int          side
)
{
   char               qim_filename [256], qhd_filename [256];
   int                err, ct;
   qsh_info_t         *qsh_info;
   image_set_t        *image_set;
   float              ratio;
   int                sizex, sizey;

   DEBUG_TRACE_IN printf ( "Entering RegisterLoadQsh\n" ); 

   qsh_info = (qsh_info_t *) MT_malloc (sizeof (qsh_info_t));
   if (side == LEFT_SIDE) 
       reg_gui->left.info = qsh_info;
   else
       reg_gui->right.info = qsh_info;

   err = read_qsh_pair
   (  qsh_info,
      filename,
      reg_gui->toplevel,
      reg_gui -> app
   );
   if (err == 0)
   {
      DT_error 
      (  reg_gui->toplevel, 
         "Could not open the file", 
         "File Error", 
         NULL
      );
      return (0);
   }


   /*
    * We have read the file into a qsh_info_t structure.  Build the 
    * images.  This involves building the image structure and the
    * drawing area widgets for display.
    */

   if (side == LEFT_SIDE) 
   {
      image_set = &(reg_gui->left);

      XtVaSetValues 
      (  reg_gui->lstrip_form,
         XmNwidth, reg_gui->default_width + 40,
         NULL
      );
   }
   else
   {
      image_set = &(reg_gui->right);
      XtVaSetValues 
      (  reg_gui->rstrip_form,
         XmNwidth, reg_gui->default_width + 40,
         NULL
      );
   }

   strcpy (image_set->name, filename);

   image_set->numimages = image_set->info->size_of_dimension[0]; 
   image_set->loaded = True;
   image_set->images = (register_image_t *) malloc 
         (image_set->numimages * sizeof (register_image_t));
   if (image_set->images == NULL)
   {
      DT_error 
      (  reg_gui->toplevel, 
         "Insufficient memory", 
         "Memory Error", 
         NULL
      );

      Free_Qsh (image_set->info);
      return (0);
   }

   sizex = image_set->info->size_of_dimension [1];
   sizey = image_set->info->size_of_dimension [2];
   ratio = (float) sizex / (float) sizey;

   image_set->di_width = reg_gui->default_width;
   image_set->di_height = (int) (image_set->di_width * ratio + 0.5);

   for (ct = 0; ct < image_set->numimages; ct ++)
   {
      /*
       * Initialize some image data values.
       */
   
      image_set->images[ct].nmark = 0;
      image_set->images[ct].zvalue = 
            image_set->info->image_location[ct];
      image_set->images[ct].zoom_factor = 1;
      image_set->images[ct].xoffset = 0;
      image_set->images[ct].yoffset = 0;
      image_set->images[ct].linked = False;
      image_set->images[ct].registered = False;
      image_set->images[ct].image = 
         image_set->info->images + ct * sizex * sizey;

      BuildImageDisplay (reg_gui, image_set, ct, side);
   }
   
   DEBUG_TRACE_OUT printf ( "Leaving RegisterLoadQsh\n" );
   return (1);
}


void BuildImageDisplay 
(  
   register_gui_t   *reg_gui,
   image_set_t      *image_set,
   int              imagenum,
   int              side
)
{
   char           zlabel [16];
   XmString       xmstr;
   unsigned char  *newimage, *ip; 
   float          ratio;
   int            ct, height; 
   Dimension        sizex, sizey;
   Colormap       cmap;

   EventMask      mask;

   DEBUG_TRACE_IN printf ( "Entering BuildImageDisplay\n" ); 

   sizex = image_set->info->size_of_dimension [1];
   sizey = image_set->info->size_of_dimension [2];


   /*
    * Build the  widget to contain the image.
    */


   image_set->images[imagenum].frame = XtVaCreateManagedWidget
   (
      "frame",
      xmFrameWidgetClass, image_set->rowcol,

      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_OUT,

      NULL
   );

   image_set->images[imagenum].form = XtVaCreateManagedWidget
   (
      "form",
        xmFormWidgetClass, image_set->images[imagenum].frame,
      NULL
   );

   XtAddEventHandler
   (  image_set->images[imagenum].form,
      EnterWindowMask,
      FALSE,
      HighlightImageEH,
      ( XtPointer ) reg_gui
   );

   XtAddEventHandler
   (  image_set->images[imagenum].form,
      LeaveWindowMask,
      FALSE,
      DehighlightImageEH,
      ( XtPointer ) reg_gui
   );


   sprintf (zlabel, "Z=%-4.4f", image_set->images[imagenum].zvalue);
   xmstr = XmStringCreateLocalized (zlabel);
   image_set->images[imagenum].label = XtVaCreateManagedWidget
   (
      "label",
      xmLabelWidgetClass, image_set->images[imagenum].form,
      XmNlabelString, xmstr,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_FORM,
      NULL
   );
   XmStringFree (xmstr); 

   image_set->images[imagenum].drawing_area = XtVaCreateManagedWidget
   (
      "darea",
      xmDrawingAreaWidgetClass, image_set->images[imagenum].form,
      XmNwidth, image_set->di_width,
      XmNheight, image_set->di_height,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_WIDGET,
      XmNtopWidget, image_set->images[imagenum].label,
      XmNtopOffset, 2,
      NULL
   );

   RegisterColormap 
   (  reg_gui, image_set->images[imagenum].drawing_area, 
      reg_gui->color_info.cmap  
   ); 
   /*
    * Create a properly sized image.  Note that this creates a unique
    * image area that is different from that loaded as qsh data.  It
    * is freed in UnloadImages.
    */

   CreateImage 
   (  reg_gui,
      image_set,
      imagenum,
      (unsigned char *) image_set->info->images + imagenum * sizex * sizey,
      sizex, sizey
   );

 
   /*
    * Add a popdown menu and the event handlers.
    */

   BuildPopupMenu (reg_gui, &(image_set->images[imagenum]), side);

   XtAddCallback
   (
      image_set->images[imagenum].drawing_area,
      XmNexposeCallback,
      ImageExposeCB,
      (XtPointer)reg_gui
   );

   XtAddEventHandler
   (  image_set->images[imagenum].drawing_area,
      EnterWindowMask,
      FALSE,
      RegisterInfoEH,
      ( XtPointer ) reg_gui
   );
   XFlush (reg_gui->display);


   DEBUG_TRACE_OUT printf ( "BuildImageDisplay\n" );
}


void CreateImage 
(  
   register_gui_t   *gui,
   image_set_t      *image_set,
   int              imagenum,
   unsigned char    *rawdata,
   int              rwidth,
   int              rheight
)
{
   float            ratio;
   unsigned char    *newimage, *ip;
   int              ct, iwidth, iheight;
   register_image_t *image;

   DEBUG_TRACE_IN printf ( "Entering CreateImage\n" );

   /*
    * Create a properly sized image.  Note that this creates a unique
    * image area that is different from that loaded as qsh data.  It
    * is freed in UnloadImages.
    */


   iwidth = image_set->di_width;
   iheight = image_set->di_height;

   newimage = (unsigned char *) malloc (iwidth * iheight);

   generic_resize_image 
   (  rawdata, 
      newimage, 
      rwidth, rheight, 
      iwidth, iheight, 1
   );


   /*
    * Map the image to the colormap.
    */

   image = &(image_set->images[imagenum]);
   ip = newimage;
   for (ct = 0; ct < iwidth * iheight; ct ++)
   {
      *ip = gui->gray_colormapping [*ip];
      ip ++;
   }

   image->ximage = XCreateImage
   (  gui->display, XDefaultVisual (gui->display, gui->screen),
      8, ZPixmap, 0, 
      (char *)newimage,
      iwidth, iheight,
      8, 0
   );

   
   DEBUG_TRACE_OUT printf ( "Leaving CreateImage\n" );
}

void UnloadImages 
(
   register_gui_t   *register_gui,
   int              side
)
{
   image_set_t      *image_set;
   int              ct;

   DEBUG_TRACE_IN printf ( "Entering UnloadImages\n" ); 

   /*
    * Remove the images from the side, releasing all memory and clean
    * up the drawing area widgets.
    */

   if (side == LEFT_SIDE)
      image_set = &(register_gui->left);
   else
      image_set = &(register_gui->right);
   
   
   if (!image_set->loaded)
      return;

   /*
    * Unload the image display and then the image data.
    */

   UnloadStrip (image_set);

   image_set->numimages = 0;
   image_set->loaded = False;


   Free_Qsh (image_set->info);

   free (image_set->images);

   DEBUG_TRACE_OUT printf ( "Leaving UnloadImages\n" );
  
   return;   
}

void UnloadStrip (image_set_t *image_set)
{
   int     ct;

   DEBUG_TRACE_IN printf ( "Entering UnloadImages\n" );

   for (ct = 0; ct < image_set->numimages; ct ++)
   {
      XDestroyImage (image_set->images [ct].ximage);
      XtDestroyWidget (image_set->images [ct].drawing_area);
      XtDestroyWidget (image_set->images [ct].label);
      XtDestroyWidget (image_set->images [ct].form);
      XtDestroyWidget (image_set->images [ct].frame);
   }

   DEBUG_TRACE_OUT printf ( "Leaving UnloadImages\n" );
}


void RegisterColormap (register_gui_t *gui,Widget w, Colormap cmap)
{
   static int initial_call = 1;	
   int maxHWcmaps;
   Window wi;


   DEBUG_TRACE_IN printf ( "Entering RegisterColormap\n" ); 

   if (gui->color_info.colortype != PseudoColor)
   {
      return;
   }

   if (initial_call) 
   {
      if (gui->color_info.maxHWcolormaps == 1)
      {
         XInstallColormap(gui->display, cmap);
      }
      initial_call = 0;
   }

    
   if (XtIsRealized(w)) 
   {
      if (gui->color_info.maxHWcolormaps == 1)
      {
         XtAddEventHandler
         (  w, EnterWindowMask, False,
            (XtEventHandler)Install_ColormapEH, (XtPointer)gui
         );
      }
      else 
      {
         wi = XtWindow(w);

	 XSetWMColormapWindows(gui->display, XtWindow(gui->toplevel), &wi, 1);
	 XSetWindowColormap(gui->display, XtWindow(w), gui->color_info.cmap);
      }
   }
   else
      if (XtIsComposite(w)) 
      {
         WidgetList theList;
         Cardinal listCount;
         int i;
      
         XtVaGetValues
         (  w, XmNnumChildren, &listCount,
            XmNchildren, &theList, 
            NULL
         ); 
         for (i = 0; i < listCount; i ++ )
	    if (XtIsWidget(theList[i])){ 
	       RegisterColormap (gui,theList[i], gui->color_info.cmap);
      }
   }

   DEBUG_TRACE_OUT printf ( "Leaving RegisterColormap\n" );
}


int InitColormap (register_gui_t *gui) 
{
   int       ct;
   static Colormap default_cmap;
   static int first_call = 1;
   static XColor def_colors[256];
   Display *display;
   int screen_num;
   int default_depth;
   int i, success = 0;
   XColor temp_color_cell, color;
   int ncolors, num_desired=256;
   unsigned long colors[256], plane_masks[1]={0};
 
   DEBUG_TRACE_IN printf("Entered InitColormap\n");
 
   gui->color_info.display = gui->display;
   gui->color_info.screen = gui->screen;
 
   gui->color_info.background = 0;
   gui->color_info.saturation = 255;
   gui->color_info.offset = 0;
   gui->color_info.gamma = 20;
 
   gui->color_info.maxHWcolormaps = 
      MaxCmapsOfScreen(DefaultScreenOfDisplay(gui->display));
 
   /*
    * Grab the current colormap for later use.
    */


/*
   gui->color_info.cmap = DefaultColormap(gui->display,gui->screen);
*/
   for (ct=0; ct < 256; ct++) 
   {
     gui->color_info.cmap_pixels[ct]=ct;
   }
 
   gui->color_info.cmap  = (Colormap)NULL;
   gui->color_info.depth = DefaultDepth(gui->display, gui->screen);
 
   switch(gui->color_info.depth)
   {
      /* supported depths */
      case 8:
      case 16:
      case 24:
      case 32:
        if ((gui->color_info.depth == 8 ) && 
 	   (XMatchVisualInfo
            (  gui->display, gui->screen, 
               gui->color_info.depth, PseudoColor, 
               &(gui->color_info.visual_info)
            ))) 
        {
 	   gui->color_info.colortype=PseudoColor;
 	   success = 1;
        } 
        else 
           if (XMatchVisualInfo
               (  gui->display, 
                  gui->screen, 
                  gui->color_info.depth, 
                  TrueColor, 
                  &(gui->color_info.visual_info)
               )) 
           {
 	       gui->color_info.colortype=TrueColor;
 	       success = 1;
           }
           break;

        default:
           break;
   }  /* end switch */
 
   if (!success) 
   {
      DT_error 
      (  gui->toplevel, 
         "Unsupported graphics device type", 
         "DeviceError", 
         NULL
      );
      return (0); 
   } 

   if (!success)
   {
      DT_error 
      (  gui->toplevel, 
         "Could not initialize color map.", 
         "System Error", 
         NULL
      );
      return(0);
   }
 
   /*
    * We are going to use the default colormap for the system, but with
    * a special colormap for the images.  Set it up with the lower part
    * the same as the default (to avoid silly changes), the middle part
    * for the image gray scale, and finally, the upper part for special
    * colors.
    */

   if (first_call) 
   {
      first_call = 0;
      if (gui->color_info.colortype==PseudoColor) 
      {
         default_cmap = DefaultColormap(gui->display, gui->screen);
         for (ct = 0; ct < 256; ct++) 
         {
 	    def_colors[ct].pixel = (unsigned long)ct;
         }
         XQueryColors(gui->display, default_cmap, def_colors, num_desired);
         XFreeColormap(gui->display, default_cmap);
         gui->color_info.cmap =XCreateColormap
         (  gui->display, 
            RootWindow(gui->display, gui->screen),
 	    gui->color_info.visual_info.visual,
 	    AllocAll
         );
         CsStoreColors 
         (  &(gui->color_info), 
            gui->color_info.cmap, 
            def_colors, num_desired
         );
      }
   }

   /*
    * Allocate the reserved colors.
    */

   
   color.pixel = RESERVED_RED;
   color.red = 65000;
   color.green = 0;
   color.blue = 0;
   CsStoreColor (&(gui->color_info), gui->color_info.cmap, &color);

   color.pixel = RESERVED_GREEN;
   color.red = 0;
   color.green = 65000;
   color.blue = 0;
   CsStoreColor (&(gui->color_info), gui->color_info.cmap, &color);

   color.pixel = RESERVED_BLUE;
   color.red = 0;
   color.green = 0;
   color.blue = 65000;
   CsStoreColor (&(gui->color_info), gui->color_info.cmap, &color);

   color.pixel = RESERVED_YELLOW;
   color.red = 50000;
   color.green = 50000;
   color.blue = 0;
   CsStoreColor (&(gui->color_info), gui->color_info.cmap, &color);

   color.pixel = RESERVED_CYAN;
   color.red = 0;
   color.green = 50000;
   color.blue = 50000;
   CsStoreColor (&(gui->color_info), gui->color_info.cmap, &color);

   color.pixel = RESERVED_MAGENTA;
   color.red = 50000;
   color.green = 0;
   color.blue = 50000;
   CsStoreColor (&(gui->color_info), gui->color_info.cmap, &color);

   /* 
    * Build a colormapping array to modify images to fit map to the
    * correct colormap locations.
    */

   for (ct = 0; ct < 256; ct ++)
      gui->gray_colormapping [ct] = (int) ((float) ct * 
         (float) REG_NUM_GRAYS/ (float) 256) + REG_MIN_GRAY;
 
   CsLoadGamma (&(gui->color_info), gui->color_info.cmap_values,
      gui->color_info.gamma/10.0);
   CsLoadRGB (&(gui->color_info));

   DEBUG_TRACE_OUT printf("Done with InitColors\n");
   return( 1 ); /* success */
}
 

/* ========================================================================
 *
 * Function: Handle an image exposure.
 *
 * Syntax:   ImageExposeCB (w, clientdata, call_data)
 *              Standard callback syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void ImageExposeCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t    *gui = (register_gui_t *)client_data;
   XmDrawingAreaCallbackStruct *cbs = (XmDrawingAreaCallbackStruct *)call_data;

   register_image_t  *image;
   int               mark, ct, sizex, sizey, image_size;
   float             truezoom;
   XExposeEvent      *event;
   image_set_t       *image_set;

   DEBUG_TRACE_IN printf("Entering  ImageExposeCB\n");

   event =  (XExposeEvent *) cbs->event;

   /*
    * Which image is it??.
    */

   image = GetImage (w, gui, NULL, &image_set);
   
   if (image == NULL)
   {
   /*
         DT_error 
      (  register_gui->toplevel, 
         "Internal error - image exposure failure", 
         "System Error", 
         NULL
      );
   */
      DEBUG_TRACE_OUT printf("Leaving  ImageExposeCB\n");
      return;
   }

   /*
    * Must have the pointer now.
    */

   /* Just do the default if the  color depth is 8 since we already have byte 
    * data.  Otherwise, create a temporary image that is the proper depth
    * to display and then destroy it.
    */

   sizex = image->ximage->width;
   sizey = image->ximage->height;
   image_size = sizex * sizey;

/*
   switch (gui->color_info.depth)
   {
      case 8:
         XPutImage
         (  gui->display, 
            XtWindow (w), gui->gc, 
            image->ximage,
	    event->x, event ->y, 
	    event->x, event ->y, 
            event->width, event->height
         );
         break;

      case 16:
         deep_image = (char *)malloc (image_size * 2);
         memcpy (deep_image, image->ximage->data, image_size * 2);

         IncreaseColorDepth
         (  gui,
            (unsigned char *) deep_image,
            image_size
         );


         new_ximage = XCreateImage
         (  gui->display, XDefaultVisual (gui->display, gui->screen),
            gui->color_info.depth, 
            ZPixmap, 0, 
            (char *)deep_image,
            sizex, sizey,
            BitmapPad (gui->display), sizex*4
         );
         break;

      case 24:
         deep_image = (char *)malloc (image_size * 3);
         memcpy (deep_image, image->ximage->data, image_size * 3);

         IncreaseColorDepth
         (  gui,
            (unsigned char *) deep_image,
            image_size
         );


         new_ximage = XCreateImage
         (  gui->display, XDefaultVisual (gui->display, gui->screen),
            gui->color_info.depth, 
            ZPixmap, 0, 
            (char *)deep_image,
            sizex, sizey,
            BitmapPad (gui->display), sizex*4
         );
         break;

      case 32:
         deep_image = (char *)malloc (image_size * 4);
         memcpy (deep_image, image->ximage->data, image_size);

         IncreaseColorDepth
         (  gui,
            (unsigned char *) deep_image,
            image_size
         );


         new_ximage = XCreateImage
         (  gui->display, XDefaultVisual (gui->display, gui->screen),
            gui->color_info.depth, 
            ZPixmap, 0, 
            (char *)deep_image,
            sizex, sizey,
            BitmapPad (gui->display), sizex*4
         );
   }
*/

/*
         XPutImage
         (  gui->display, 
            XtWindow (w), gui->gc, 
            new_ximage,
	    event->x, event ->y, 
	    event->x, event ->y, 
            event->width, event->height
         );
*/
         CsPutImage 
         (  &(gui->color_info),
            XtWindow (w),
            gui->gc,
            image->ximage,
            event->x, event->y,
            event->x, event->y,
            event->width, event->height
         );
   
   /*
    * Draw the fiducial markers on the image.
    */


   sizex = image_set->info->size_of_dimension[1];
   truezoom = (float)image->zoom_factor * 
        (float) image_set->di_width /  (float) sizex; 

   for (mark = 0; mark < image->nmark; mark ++)
      for (ct = 0; ct < image->mset[mark].npoint; ct ++)
         if (image->mset[mark].points[ct].type == PT_TYPE_CROSS)
            DrawCross 
         (  gui, 
            (image->mset[mark].points[ct].x - image->xoffset) * truezoom,
            (image->mset[mark].points[ct].y - image->yoffset) * truezoom,
            image
         );
      else
         DrawPoint 
         (  gui, 
            (image->mset[mark].points[ct].x - image->xoffset) * truezoom,
            (image->mset[mark].points[ct].y - image->yoffset) * truezoom,
            image
         );

   DEBUG_TRACE_OUT printf("Leaving  ImageExposeCB\n");

   return;
}

/* ========================================================================
 *
 * Function: ZoomImageCB
 *
 * Purpose:  Zoom in on the image for better resolution of marking
 *
 * Syntax:   ZoomImageCB (w, clientdata, call_data)
 *              Standard callback syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void ZoomImageCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *gui = (register_gui_t *) client_data;
   XmString         xmstr, newstring;
   image_set_t      *image_set;
   register_image_t *image;
   int              sizex, sizey, imagenum, zwidth, zheight;
   float            truezoom, ratio;
   Widget           da_widget;
   unsigned char    *newimage;

   DEBUG_TRACE_IN printf("Entering ZoomImageCB\n");

   /*
    * Get the rowcol parent of Zoom and then find the corresponding image.
    */
   da_widget = XtParent (w);
   image = GetImageByMenu (da_widget, gui, &imagenum, &image_set);
   if (image == NULL)
      return;

   sizex = image_set->info->size_of_dimension [1];
   sizey = image_set->info->size_of_dimension [2];


   /* 
    * Change the menu label and build a new image for display.
    */

   XtVaGetValues 
   (
      image->popup_menu.zoom,
      XmNlabelString, &xmstr,
      NULL
   );

   /*if (XmStringLength (xmstr) == 4) */
   if (image->zoom_factor == 1)
   {
      /* Currently not zoomed, so zoom */
      newstring = XmStringCreateLocalized ("Unzoom");
      XtVaSetValues
      (  image->popup_menu.zoom,
         XmNlabelString, newstring,
         NULL
      );
      XmStringFree (newstring);

      /*
       * Get the required part of the image centered at the cursor
       * Then build a new image and let expose take care of the rest of it.
       */

      
      image->zoom_factor = gui->menubar.default_zoom;
      truezoom = (float) image->zoom_factor *
        (float) image_set->di_width /  (float) sizex; 
         

      zwidth = sizex / image -> zoom_factor;
      zheight = sizey / image -> zoom_factor;
/*
      zwidth = (int) image_set-> di_width / image->zoom_factor;
      zheight = (int) image_set-> di_height / image->zoom_factor;
*/

      newimage = ExtractSubimage 
      (  gui, image_set, image, imagenum,
         zwidth, zheight
      );
   
      XDestroyImage (image->ximage);

      CreateImage (gui, image_set, imagenum, newimage, zwidth, zheight);

/*
      XmRedisplayWidget (image->drawing_area);
*/
      XClearArea 
      (  gui->display, 
         XtWindow (image->drawing_area),
         0, 0,
         image_set->di_width, image_set->di_height,
         True
      );
     

   }
   else
   {
      /* Currently zoomed, so unzoom */

      newstring = XmStringCreateLocalized ("Zoom");
      XtVaSetValues
      (  image->popup_menu.zoom,
         XmNlabelString, newstring,
         NULL
      );
      XmStringFree (newstring);

      
      /*
       * Reset things for unzoomed.
       */

      image->zoom_factor = 1;
      image->xoffset = 0;
      image->yoffset = 0;


      XDestroyImage (image->ximage);

      CreateImage 
      (  gui, image_set, imagenum, 
         image_set->info->images + imagenum * sizex * sizey,         
         sizex, sizey 
      );
         
/*
      XmRedisplayWidget (image->drawing_area);
*/
      XClearArea 
      (  gui->display, 
         XtWindow (image->drawing_area),
         0, 0,
         image_set->di_width, image_set->di_height,
         True
      );
   }

   DEBUG_TRACE_OUT printf("Leaving  ZoomImageCB\n");
}

/* ========================================================================
 *
 * Function: RegisterImageCB
 *
 * Purpose:  Register an image individually
 *
 * Syntax:   RegisterImageCB (w, clientdata, call_data)
 *              Standard callback syntax
 *
 * Notes:
 *
 * =========================================================================
 */

void RegisterImageCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *gui = (register_gui_t *) client_data;
   XmString         xmstr, newstring;
   image_set_t      *var_set, *fixed_set;
   register_image_t *vimage, *fimage;
   int              imagenum, fsizex, fsizey, vsizex, vsizey;
   unsigned char    *newimage;
   Widget           da_widget;

   DEBUG_TRACE_IN printf("Entering RegisterImageCB\n");

   /*
    * Get the image and image set pointers.
    */

   da_widget = XtParent (w);
   vimage = GetImageByMenu (da_widget, gui, &imagenum, &var_set);
   if (vimage == NULL)
      return;
   fixed_set = &(gui->left);

   fimage = &(fixed_set->images[vimage->linkid]); 

   /*
    * If the image isn't linked or has no points, don't go any further.
    */

   if (! vimage->linked)
   {
      DT_error 
      (  gui->toplevel, 
         "The image is not linked and cannot be registered", 
         "Registration Error", 
         NULL
      );
      return;
   }

   if (vimage->nmark == 0)
   {
      DT_error 
      (  gui->toplevel, 
         "The image has no marks and cannot be registered", 
         "Registration Error", 
         NULL
      );
      return;
   }

   /*
    * Set up the necessary variables.
    */

   fixed_set = OtherImageSet (gui, var_set);

   vsizex = var_set->info->size_of_dimension[1];
   vsizey = var_set->info->size_of_dimension[2];
   fsizex = fixed_set->info->size_of_dimension[1];
   fsizey = fixed_set->info->size_of_dimension[2];

   /*
    * Two choice - register or unregister.  Unregister is easy.
    */

   if (vimage->registered)
      UnregisterImage (gui, var_set, imagenum);
/*

      newstring = XmStringCreateLocalized ("Register");
      XtVaSetValues
      (  vimage->popup_menu.reg_image,
         XmNlabelString, newstring,
         NULL
      );
      XmStringFree (newstring);
 
      vimage->registered = False;

      XDestroyImage (vimage->ximage);

      CreateImage 
      (  gui, var_set, imagenum, 
         var_set->info->images + imagenum * vsizex * vsizey,         
         vsizex, vsizey 
      );
         
      XClearArea 
      (  gui->display, 
         XtWindow (vimage->drawing_area),
         0, 0,
         var_set->di_width, var_set->di_height,
         True
      );
*/
   else
   {
      /* 
       * Currently not registered,
       */

      vimage->registered = True;

      newstring = XmStringCreateLocalized ("Unregister");
      XtVaSetValues
      (  vimage->popup_menu.reg_image,
         XmNlabelString, newstring,
         NULL
      );
      XmStringFree (newstring);
 
      /*
       * Create a new image and then clear.
       */

      newimage = RegisterOneImage 
      (  gui, 
         fimage,
         fsizex, fsizey,
         vimage,
         vsizex, vsizey
      );
      
      XDestroyImage (vimage->ximage);

      CreateImage (gui, var_set, imagenum, newimage, vsizex, vsizey);

      XClearArea 
      (  gui->display, 
         XtWindow (vimage->drawing_area),
         0, 0,
         var_set->di_width, var_set->di_height,
         True
      );
     
   }

   DEBUG_TRACE_OUT printf("Leaving  RegisterImageCB\n");
}


/* ========================================================================
 *
 * Function: ExtractSubimage
 *
 * Purpose:  Pull out part of an image for zooming.
 *
 * Syntax:   ExtractSubimage (gui, image_set, image, imagenum)
 *            
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * ========================================================================
 */

unsigned char *ExtractSubimage 
(  register_gui_t     *gui, 
   image_set_t        *image_set, 
   register_image_t   *image,
   int                imagenum,
   int                zwidth,
   int                zheight
)
{
   int              row, col, ct, sizex, sizey, x, y;
   int              startx, starty, endx, endy;
   float            ratio;
   unsigned char    *ip, *newimage, *np;

   DEBUG_TRACE_IN printf("Entering ExtractSubimage\n");

   sizex = image_set->info->size_of_dimension [1];
   sizey = image_set->info->size_of_dimension [2];

   newimage = (unsigned char *) malloc (zwidth * zheight);
   if (newimage == NULL)
      return (NULL);
   
   x = image->popup_menu.x;
   y = image->popup_menu.y;

   /*
    * Figure out where to get the section from.  Use (x,y) as the
    * center.  However, if the display images are a different size
    * than the stored images, adjust x and y to compensate.
    */

   if (sizex != image_set->di_width)
      x = (int) (x * ((float) sizex / (float) image_set->di_width));
   if (sizey != image_set->di_height)
      y = (int) (y * ((float) sizey / (float) image_set->di_height));

   startx = x - zwidth/2;
   if (startx < 0)
      startx = 0;

   endx = startx + zwidth;
   if (endx > sizex)
   {
      endx = sizex;
      startx = endx - zwidth;
   }

   starty = y - zheight/2;
   if (starty < 0)
      starty = 0;
   endy = starty + zheight;
   if (endy > sizey)
   {
      endy = sizey;
      starty = endy - zheight;
   }


   image->xoffset = startx;
   image->yoffset = starty;

   /*
    * Rip out the part of the image desired.
    */

   ip = image_set->info->images + imagenum * sizex * sizey;
   np = newimage;
   for (row = starty; row < endy; row++)
   {
      ip = image_set->info->images + imagenum * sizex * sizey + 
           row * sizex + startx;
      for (col = starty; col < endy; col ++)
         *np++ = *ip++;
   }

   DEBUG_TRACE_OUT printf("Leaving  ExtractSubimage\n");
   return (newimage);
}

/* ========================================================================
 *
 * Function: ClearPointsCB
 *
 * Purpose:  Clear the points from an image.
 *
 * Syntax:   ClearPointsCB (w, clientdata, call_data)
 *              Standard callback syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void ClearPointsCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *gui = (register_gui_t *) client_data;
   XmString         xmstr;
   register_image_t *image;
   Widget           da_widget;
   image_set_t      *image_set, *link_set;
   int              image_num;

   DEBUG_TRACE_IN printf("Entering ClearPointsCB\n");

   /*
    * Just set the number of points to zero.
    */

   da_widget = XtParent (w);
   image = GetImageByMenu (da_widget, gui, &image_num, &image_set);

   link_set = OtherImageSet (gui, image_set);

   image->nmark = 0;

   XClearArea 
   (  gui->display, 
      XtWindow (image->drawing_area),
      0, 0,
      image_set->di_width, image_set->di_height,
      True
   );

   /*
    * If linked, do the same for the linked image.
    */

   if (! image->linked)
      return;

   image->linked = False;

   image = &(link_set->images[image->linkid]);
   image->nmark = 0;
   image->linked = False;

   XClearArea 
   (  gui->display, 
      XtWindow (image->drawing_area),
      0, 0,
      link_set->di_width, link_set->di_height,
      True
   );

   
   DEBUG_TRACE_OUT printf("Leaving ClearPointsCB\n");

   return;
}


/* ========================================================================
 *
 * Purpose:  Clear a point from an image based on a button click.
 *
 * Syntax:   RemoveMarkCB (w, clientdata, call_data)
 *              Standard callback syntax
 *           RemoveMark (reg_gui, image_set, image, mark_num)
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void RemoveMarkCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *reg_gui = (register_gui_t *) client_data;
   XmString         xmstr;
   register_image_t *image, *limage;
   Widget           da_widget;
   image_set_t      *image_set, *link_set;
   int              image_num, ct, pt, dist, mindist, minpoint;

   XmPushButtonCallbackStruct  *cbs = (XmPushButtonCallbackStruct *) call_data;
   XEvent           *xevent;

   DEBUG_TRACE_IN printf("Entering RemoveMarkCB\n");

   /*
    * Figure out which mark we are talking about.  This is based on
    * the city-block distance from the button press to the marks.
    */

   xevent = cbs -> event;

   da_widget = XtParent (w);
   image = GetImageByMenu (da_widget, reg_gui, &image_num, &image_set);

   link_set = OtherImageSet (reg_gui, image_set);


   /*
    * We need the location of the drawing area window with respect
    * to the root window, which is used by the XEvent structure.
    */


   mindist = 1000000;
   minpoint = 0;
   for (ct = 0; ct < image->nmark; ct ++)
   {
      dist = abs(image->popup_menu.x - image->mset[ct].points[0].x) + 
             abs(image->popup_menu.y - image->mset[ct].points[0].y);
      if (dist < mindist)
      {
         mindist = dist;
         minpoint = ct;
      }
   }

   RemoveMark (reg_gui, image_set, image, minpoint);

   /*
    * If linked, do the same for the linked image.
    */

   if (! image->linked)
      return;

   limage = &(link_set->images[image->linkid]);

   RemoveMark (reg_gui, image_set, limage, minpoint);

   if (image->nmark == 0)
   {
      image->linked = False;
      limage->linked = False;
   }
   
   DEBUG_TRACE_OUT printf("Leaving RemoveMarkCB\n");

   return;
}

void RemoveMark
(
   register_gui_t    *reg_gui,
   image_set_t       *image_set,
   register_image_t  *image,
   int               mark_num
)
{
   int               ct, pt;

   /*
    * Copy down the points that follow and change the number of points.
    */

   for (ct = mark_num; ct < image->nmark-1; ct ++)
   {
      image->mset[ct].npoint = image->mset[ct+1].npoint;
      for (pt = 0; pt < image->mset[ct].npoint; pt ++)
      {
          image->mset[ct].points[pt].x = image->mset[ct+1].points[pt].x;
          image->mset[ct].points[pt].y = image->mset[ct+1].points[pt].y;
      }
   }

   image->nmark --;

   /*
    * With the point gone, redisplay.
    */

   XClearArea 
   (  reg_gui->display, 
      XtWindow (image->drawing_area),
      0, 0,
      image_set->di_width, image_set->di_height,
      True
   );
}


/* ========================================================================
 *
 * Function: UnlinkImagesCB
 *
 * Purpose:  Unlink an image.
 *
 * Syntax:   UnlinkImagesCB (w, clientdata, call_data)
 *              Standard callback syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void UnlinkImagesCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *gui = (register_gui_t *) client_data;
   XmString         xmstr;
   register_image_t *image;
   Widget           da_widget;
   image_set_t      *image_set, *other_set;
   int              image_num;

   DEBUG_TRACE_IN printf("Entering UnlinkImagesCB\n");

   /*
    * Just set the number of points to zero.
    */

   da_widget = XtParent (w);
   image = GetImageByMenu (da_widget, gui, &image_num, &image_set);

   if (! image->linked)
      return;

   other_set = OtherImageSet (gui, image_set);

   image->nmark = 0;

   if (image->linked)
   {
      other_set->images[image->linkid].nmark = 0;
      other_set->images[image->linkid].linked = False;
      image->linked = False;
   }

/*
   XmRedisplayWidget (image->drawing_area);
*/
   XClearArea 
   (  gui->display, 
      XtWindow (image->drawing_area),
      0, 0,
      image->ximage->width, image->ximage->height,
      True
   );
   
   DEBUG_TRACE_OUT printf("Leaving UnlinkImagesCB\n");

   return;
}

/* ========================================================================
 *
 * Function: UndoPointsCB
 *
 * Purpose:  Undo the points from an image.
 *
 * Syntax:   UndoPointsCB (w, clientdata, call_data)
 *              Standard callback syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void UndoPointsCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *gui = (register_gui_t *) client_data;
   XmString         xmstr;
   register_image_t *image;
   Widget           da_widget;

   DEBUG_TRACE_IN printf("Entering UndoPointsCB\n");

   /*
    * Remove all points concerned with the last mark set.
    */

   da_widget = XtParent (w);
   image = GetImageByMenu (da_widget, gui, NULL, NULL);

   UndoMark (gui, image);

   DEBUG_TRACE_OUT printf("Leaving UndoPointsCB\n");

   return;
}

void UndoMark
(  register_gui_t     *gui,
   register_image_t   *image
)
{
   DEBUG_TRACE_OUT printf("Entering UndoMark\n");

   if (image->nmark == 0)
      return;

   /* 
    * Remove the last marker set
    */

   image->nmark --;

/*
   XmRedisplayWidget (image->drawing_area);
*/
   XClearArea 
   (  gui->display, 
      XtWindow (image->drawing_area),
      0, 0,
      image->ximage->width, image->ximage->height,
      True
   );

   DEBUG_TRACE_OUT printf("Leaving UndoMark\n");
}



int ProcessLinking 
(  register_gui_t   *gui, 
   image_set_t      *image_set, 
   int              image_num
)
{
   image_set_t       *linking_set;
   int               inum;

   DEBUG_TRACE_IN printf("Entering ProcessLinking\n");

   /*
    * First check the sides for reasonableness and set up a pointer for the
    * other side.
    */

   if (gui->link_side == LEFT_SIDE) 
      if (image_set != &(gui->left)) 
         return (-1);
      else
         linking_set = &(gui->right);

   if (gui->link_side == RIGHT_SIDE) 
      if (image_set != &(gui->right)) 
         return (-1);
      else
         linking_set = &(gui->left);

   /*
    * If already linked, give the user a choice to relink or
    * cancel out.
    */

   if (image_set->images[image_num].linked)
   {
      if (  !DT_decide 
            (  gui->toplevel, 
               gui->app, 
               "Image already linked - continue",
               "Attempt to relink",
               "Yes", "No"
            )
         ) 
         return (-1);
      else
      {
         inum = image_set->images[image_num].linkid;
         linking_set->images[inum].linked = False;   
      }

   }

   gui->linking = False;
   linking_set->images[gui->link_inum].linked = True;   
   linking_set->images[gui->link_inum].linkid = image_num;
   image_set->images[image_num].linked = True;
   image_set->images[image_num].linkid = gui->link_inum;

   XUndefineCursor (gui->display, XtWindow (gui->mainwindow));

   DEBUG_TRACE_OUT printf("Leaving ProcessLinking\n");

   return (0);
}



/* ========================================================================
 *
 * Function: LinkImageCB 
 *
 * Purpose:  Link a fixed and variable image together for registration.
 *
 * Syntax:   LinkImageCB (w, clientdata, call_data)
 *              Standard callback syntax
 *
 * Notes:
 *
 * =========================================================================
 */

void LinkImageCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *gui = (register_gui_t *) client_data;
   register_image_t *image;
   int              imagenum;
   image_set_t      *image_set;
   Widget           da_widget;

   DEBUG_TRACE_IN printf("Entering LinkImageCB\n");

   /*
    * Find out which image we're in. The images to link to are the opposite
    * side.
    */

   da_widget = XtParent (w);
   image = GetImageByMenu (da_widget, gui, &imagenum, &image_set);

   StartLink (gui, image_set, imagenum);
 
   return;
}


void StartLink 
(
   register_gui_t  *gui,
   image_set_t     *image_set,
   int             image_num
)
{
   XmString         xmstr;
   int              side, x, y, width;

   /*
    * If already linked, give the user a choice to relink or
    * cancel out.
    */

   if (image_set->images[image_num].linked)
   {
      if (  !DT_decide 
            (  gui->toplevel, 
               gui->app, 
               "Image already linked - continue",
               "Attempt to relink",
               "Yes", "No"
            )
         ) 
         return;
   }

   gui->linking = True;
   gui->link_inum = image_num;

   if (image_set == &(gui->left))
   {
      gui->link_side = RIGHT_SIDE;
      image_set = &(gui->right);
   }
   else
   {
      gui->link_side = LEFT_SIDE;
      image_set = &(gui->left);
   }

   /*
    * Change the cursor and force the user to select an image in the
    * other set.  This is done by grabbing the mouse and forcing the
    * next click to be in the image to link.
    */

   XDefineCursor (gui->display, XtWindow (gui->mainwindow), gui->link_cursor);

   XtVaGetValues
   (
      gui->form,
      XmNx, &x,
      XmNy, &y,
      XmNwidth, &width,
      NULL
   );

   XtManageChild (gui->message_box);

   if (gui->alt_side == RIGHT_SIDE)
      if (gui->menubar.method == ALT_POINTS)
         xmstr = XmStringCreateLocalized ("Click on the corresponding point\n \
            in the variable image set");
      else
         xmstr = XmStringCreateLocalized ("Click on the Variable image to \
            link");
   else
      if (gui->menubar.method == ALT_POINTS)
         xmstr = XmStringCreateLocalized ("Click on the corresponding point\n \
            in the fixed image set");
      else
         xmstr = XmStringCreateLocalized ("Click on the Fixed image to link");

   XtVaSetValues
   (  gui->message_box,
      XmNx, x + width,
      XmNy, y + 50,
      XmNmessageString, xmstr,
      NULL
   );
   XmStringFree (xmstr);
 
   DEBUG_TRACE_OUT printf("Leaving LinkImageCB\n");

   return;
}


void LinkCancelCB 
(  Widget       w, 
   XtPointer    client_data, 
   XtPointer    call_data
)
{
   register_gui_t   *gui = (register_gui_t *) client_data;
   image_set_t      *image_set;
   int              ct;

   DEBUG_TRACE_IN printf("Entering LinkCancelCB\n");

   if (gui->link_side = LEFT_SIDE)
      image_set = &(gui->left);
   else
      image_set = &(gui->right);

   gui->linking = False;

   XUndefineCursor (gui->display, XtWindow (gui->mainwindow));

   XtUnmanageChild (gui->message_box);

   DEBUG_TRACE_OUT printf("Leaving LinkCancelCB\n");
}

void LinkClickEH 
(
   Widget       w,
   XtPointer    clientdata,
   XEvent       *event,
   Boolean      *flag
)
{
   register_gui_t   *gui = (register_gui_t *) clientdata; 
   XButtonEvent     *bevent = (XButtonEvent *)event;
   register_image_t *image;
   image_set_t      *image_set;
   int              ct;

   DEBUG_TRACE_IN printf("Entering LinkClickEH\n");

   /*
    * Check to see if the click was in the right place - e.g. an 
    * image in the correct image strip.
    */
   
   /*
    * Cancel everything.
    */

   if (gui->link_side = LEFT_SIDE)
      image_set = &(gui->left);
   else
      image_set = &(gui->right);

   gui->linking = False;

   XtUnmanageChild (gui->message_box);

   DEBUG_TRACE_OUT printf("Leaving LinkClickEH\n");
}


void StartAlternatePoint
(
   register_gui_t  *gui,
   image_set_t     *image_set,
   int             image_num
)
{
   XmString         xmstr;
   int              x, y, width;

   DEBUG_TRACE_IN printf("Entering StartAlternatePoint\n");

   /*
    * Check to see if both image sets are loaded.  If not, stop here.
    */

   if (image_set == &(gui->left))
   {
      gui->alt_side = RIGHT_SIDE;
      if (! gui->right.loaded)
      {
         DT_error 
         (  gui->toplevel, 
            "Variable image set not loaded.", 
            "Point Error", 
            NULL
         );
         return;
      }
   }
   else
   {
      gui->alt_side = LEFT_SIDE;
      if (! gui->right.loaded)
      {
         DT_error 
         (  gui->toplevel, 
            "Fixed image set not loaded.", 
            "Point Error", 
            NULL
         );
         return;
      }
   }

   gui->alternating = True;
   gui->alt_inum = image_num;

   /*
    * Change the cursor and force the user to select an image in the
    * other set.  
    */

   XDefineCursor (gui->display, XtWindow (gui->mainwindow), gui->link_cursor);

   /*
    * Place the message box on the edge of the main window.
    */

   XtVaGetValues
   (
      gui->form,
      XmNx, &x,
      XmNy, &y,
      XmNwidth, &width,
      NULL
   );

   XtManageChild (gui->message_box);

   if (gui->alt_side == RIGHT_SIDE)
      xmstr = XmStringCreateLocalized ("Click on the corresponding point\n \
 in the VARIABLE image set");
   else
      xmstr = XmStringCreateLocalized ("Click on the corresponding point\n \
 in the FIXED imageset ");

   XtVaSetValues
   (  gui->message_box,
      XmNx, x + width,
      XmNy, y + 50,
      XmNmessageString, xmstr,
      NULL
   );
   XmStringFree (xmstr);
 
/*
   XtAddCallback
   (  gui->message_box,
      XmNcancelCallback, AltCancelCB,
      (XtPointer) gui
   );
*/

   /*
    * If the image is unlinked, set up linking.
    */

   
   if (!image_set->images[image_num].linked)
      gui->alt_linking = True;

   DEBUG_TRACE_OUT printf("Leaving StartAlternatePoint\n");

   return;
}

int StopAlternatePoint
(
   register_gui_t   *gui,
   image_set_t      *image_set,
   int              image_num
)
{
   image_set_t      *link_set;
   register_image_t *image;
   int              inum;
 
   DEBUG_TRACE_IN printf("Entering StopAlternatePoint\n");

   if (gui->alt_side == LEFT_SIDE)
      link_set = &(gui->right);
   else
      link_set = &(gui->left);

   if (image_set == link_set)
   {
      DT_error 
      (  gui->toplevel, 
         "Wrong image set for second point", 
         "Point Error", 
         NULL
      );

      return (-1);
   }


   /*
    * If linking, set it up.  If already linked, make sure this is the
    * right image.
    */

   if (gui->alt_linking)
   {
      if (image_set->images[image_num].linked)
      {
         DT_error
         (  gui->toplevel, 
            "Variable image already linked differently.",
            "Point Error",
            NULL
         );
         return (-1);
      }
      else
      {
         image_set->images[image_num].linked = True;
         image_set->images[image_num].linkid = gui->alt_inum;
         link_set->images[gui->alt_inum].linked = True;
         link_set->images[gui->alt_inum].linkid = image_num;
         gui->alt_linking = False;
      }
   }
   else
   {
      /*
       * Not in linking mode, so the start image is already linked.
       * If this image is the right one, great, otherwise, it's an
       * error.  This could be either that the ending image is not
       * linked, or that it is the wrong image.
       */

      if (! image_set->images[image_num].linked)
      {
         DT_error 
         (  gui->toplevel, 
            "Initial image already linked elsewhere.", 
            "Point Error", 
            NULL
         );
         return (-1);
      }
      
      if (image_set->images[image_num].linkid != gui->alt_inum)
      {
         DT_error 
         (  gui->toplevel, 
            "These images not linked - try another", 
            "Point Error", 
            NULL
         );

         return (-1);
      }
   }

   gui->alternating  = False;

   XUndefineCursor (gui->display, XtWindow (gui->mainwindow));

   XtUnmanageChild (gui->message_box);

   DEBUG_TRACE_OUT printf("Leaving StopAlternatePoint\n");

   return (0);
}

void AltCancelCB 
(  Widget       w, 
   XtPointer    client_data, 
   XtPointer    call_data
)
{
   register_gui_t   *gui = (register_gui_t *) client_data;
   image_set_t      *image_set;
   int              ct;
   register_image_t *image;
   XmAnyCallbackStruct  *cbs = (XmAnyCallbackStruct *) call_data;
   XEvent           *event;

   DEBUG_TRACE_IN printf("Entering AltCancelCB\n");

   event = cbs->event;

   if (gui->alt_side == RIGHT_SIDE){
     image_set = &(gui->left);
   } else if(gui->alt_side == LEFT_SIDE) {
     image_set = &(gui->right);
   } else {
     printf("Side not specififed in AltCancelCB\n");
     return;
   }

   image = &(image_set->images[gui->alt_inum]);
   UndoMark (gui, image);

   gui->alternating = False;
   XUndefineCursor (gui->display, XtWindow (gui->mainwindow));
   XtUnmanageChild (gui->message_box);


   DEBUG_TRACE_OUT printf("Leaving AltCancelCB\n");
}

/* ========================================================================
 *
 * Function: CenterMarkCB
 *
 * Purpose:  Center a mark in the apparent fiducial marker
 *
 * Syntax:   CenterMarkCB (w, clientdata, call_data)
 *              Standard callback syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void CenterMarkCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *gui = (register_gui_t *) client_data;
   XmString         xmstr;

   DEBUG_TRACE_IN printf("Entering CenterMarkCB\n");
   DEBUG_TRACE_OUT printf("Leaving CenterMarkCB\n");

   return;
}

/* ========================================================================
 *
 * Function: OutlineMarkCB
 *
 * Purpose:  Outline a mark in the apparent fiducial marker
 *
 * Syntax:   OutlineMarkCB (w, clientdata, call_data)
 *              Standard callback syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void OutlineMarkCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *gui = (register_gui_t *) client_data;
   XmString         xmstr;

   DEBUG_TRACE_IN printf("Entering OutlineMarkCB\n");
   DEBUG_TRACE_OUT printf("Leaving OutlineMarkCB\n");
}


/* ========================================================================
 *
 * Function: Create an image of 16 or 32 bits in depth.
 *
 * Syntax:   IncreaseColorDepth (color_info_t  *cinfo, unsigned char
 *              Standard event handler syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void IncreaseColorDepth
(  register_gui_t   *gui,
   unsigned char    *image,
   unsigned int     memsize
) 
{
   unsigned int   i, top;
   unsigned short *sdata;
   unsigned long  *ldata;
   unsigned char  *data;
   unsigned long  *truecolors;

   DEBUG_TRACE_IN printf("Entered IncreaseColorDepth\n");

   truecolors = gui->color_info.cmap_pixels;
   top = memsize-1;
   data = image+top;

   switch(gui->color_info.depth)
   {
      case 8:
         break;
      case 16:
         sdata = ((unsigned short *)image) + top;
         for (i=0; i<memsize; i++) 
         {
            *sdata-- = (unsigned short)(truecolors[*data--]);
         }
      break;

      case 24:
      case 32:
         ldata = ((unsigned long *)image) + top;
         for (i=0; i<memsize; i++) 
         {
            *ldata-- = truecolors[*data--];
         }
         break;
      default:
         break;
   }
   DEBUG_TRACE_OUT printf("Leaving IncreaseColorDepth\n");
}


/* ========================================================================
 *
 * Function: Handle a click event in an image.
 *
 * Syntax:   ImageClickEH (w, clientdata, event, flag)
 *              Standard event handler syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:    
 *
 * =========================================================================
 */

void ImageClickEH
(
   Widget       w,
   XtPointer    clientdata,
   XEvent       *event,
   Boolean      *flag
)
{
   register_gui_t   *gui = (register_gui_t *) clientdata; 
   XButtonEvent     *bevent = (XButtonEvent *)event;
   register_image_t *image;
   image_set_t      *image_set;
   int              image_num;

   DEBUG_TRACE_IN printf("Entering  ImageClick\n");

   image = GetImage (w, gui, &image_num, &image_set);
  
   if (image == NULL)
      return;       /* this is an internal error of some kind. */ 


   /*
    * Check to see if we're linking.  If so, things are different.
    * Do the linking and then process the press.  Treat a press with
    * either button the same.
    */

   if (gui->linking)
   {
      if (ProcessLinking (gui, image_set, image_num) < 0)
         return;

      XUndefineCursor (gui->display, XtWindow (gui->mainwindow));
      XtUnmanageChild (gui->message_box);

      /* LeftButtonClick (bevent, gui, image_set, image, image_num); */
      return;
   }

   /*
    * We've got the image.  It could be a press or a release, and
    * it could be the right or left button.
    */

   if (bevent -> button == 3)
   {
      RightButtonClick (bevent, gui, image);
      return;
   }
   else if (bevent -> button == 1)
   {
      LeftButtonClick (bevent, gui, image_set, image, image_num);
      return;
   }

   DEBUG_TRACE_OUT printf("Leaving  ImageClick\n");
}

/* ==========================================================================
 *
 * Function:     LeftButtonClick
 *
 * Purpose:      Handle a left button click in an image
 *
 * Syntax:       LeftButtonClick (bevent, register_gui, image, image_num)
 *
 * ==========================================================================
 */

static void LeftButtonClick 
(  XButtonEvent     *bevent, 
   register_gui_t   *gui, 
   image_set_t      *image_set,
   register_image_t *image,
   int              image_num
)
{
   float        truezoom;
   markerset_t  *mark;

   DEBUG_TRACE_IN printf("Entering LeftButtonClick\n");

   if (bevent -> type == ButtonRelease)
      return;

   /*
    * If we are in alternating mode, then this is either the first or
    * the second in a pair.  If the first, start an alternating process,
    * if the second, end one.
    */

   if (gui->menubar.method == ALT_POINTS)
      if (gui->alternating)
         if (StopAlternatePoint (gui, image_set, image_num) < 0)
         {
            return;
         }
         else
            ;
      else
         StartAlternatePoint (gui, image_set, image_num);

   /*
    * This is for a single click which marks a fiducial point.
    * A fiducial point could be single or multiple, depending on the
    * setting for encapsulate setting. 
    */

   /*
    * Store the data for the image.
    */

   if (image->nmark == MAX_MARKS)
   {
      DT_error 
      (  gui->toplevel, 
         "Too many marks for an image", 
         "Event Error", 
         NULL
      );

      return;
   }

   mark = &(image->mset[image->nmark]);
   image->nmark ++;
   mark->npoint = 0;


   /*
    * Grab the point and convert to the stored image coordinate
    * system.
    */

   truezoom = (float)image->zoom_factor * (float) image_set->di_width /
      (float) image_set->info->size_of_dimension[1]; 

   mark->points[mark->npoint].x = image->xoffset + bevent->x / truezoom;
   mark->points[mark->npoint].y = image->yoffset + bevent->y / truezoom;

   mark->points[mark->npoint].type = PT_TYPE_CROSS;
   mark->npoint ++;
   
   /*
    * Force a redraw.
    */

   XClearArea 
   (  gui->display, 
      XtWindow (image->drawing_area),
      bevent->x-10, bevent->y-10,
      20, 20,
      True
   );


   /*
    * If we are in autolink mode and this image is unlinked
    * so start up a link process.
    */

   if (gui->menubar.method == LINKING)
      if (!image->linked)
      {
         StartLink (gui, image_set, image_num);
         return;
      }

   DEBUG_TRACE_OUT printf("Entering LeftButtonClick\n");
}

/* ==========================================================================
 *
 * Function:     RightButtonClick
 *
 * Purpose:      Handle a right button click in an image
 *
 * Syntax:       RightButtonClick (bevent, register_gui, image)
 *
 * ==========================================================================
 */

static void RightButtonClick 
(  XButtonEvent    *bevent, 
   register_gui_t  *gui, 
   register_image_t *image
)
{

   DEBUG_TRACE_IN printf("Entering RightButtonClick\n");

   /*
    * Pop up a menu of options.
    *    zoom in on the fiducial marker
    *    find the center of the marker
    *    outline the marker
    *
    * If its a button release, pop down the menu.
    */

   if (bevent -> type == ButtonRelease)
   {
      XtUnmanageChild (image->popup_menu.rowcol);
      /* XtPopdown (image->popup_menu.shell); */
      return;
   }
   else
   {
      XmMenuPosition (image->popup_menu.rowcol, bevent);
      image->popup_menu.x = bevent->x;
      image->popup_menu.y = bevent->y;
      XtManageChild (image->popup_menu.rowcol);
      /* XtPopup (image->popup_menu.shell, XtGrabNone); */
   }
   
   DEBUG_TRACE_OUT printf("Leaving RightButtonClick\n");
 
}



/* ========================================================================
 *
 * Function: The following functions perform the drawing of
 *           the fiducial marks on the image.  The two types are a
 *           cross mark and a simple point mark.
 *
 * Syntax:   void DrawCross (register_gui_t *gui, int x, int y, 
 *                           register_image_t *image)
 *              Using the GC from gui, draw a cross on the image at x,y.
 *
 *           void DrawPoint (register_gui_t *gui, int x, int y, 
 *                           register_image_t *image)
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */
void DrawCross 
(  register_gui_t     *gui, 
   int                x, 
   int                y, 
   register_image_t   *image
)
{
   DEBUG_TRACE_IN  printf("Entering DrawCross\n");

   XDrawLine 
   (  gui->display, 
      XtWindow (image->drawing_area), 
      gui->mark_gc,
      x - CROSS_SIZE, y,
      x + CROSS_SIZE, y
   );

   XDrawLine 
   (  gui->display, 
      XtWindow (image->drawing_area), 
      gui->mark_gc,
      x, y - CROSS_SIZE,
      x, y + CROSS_SIZE
   );
   
   DEBUG_TRACE_OUT  printf("Leaving DrawCross\n");
}

void DrawPoint
(  register_gui_t     *gui, 
   int                x, 
   int                y, 
   register_image_t   *image
)
{
   DEBUG_TRACE_IN  printf("Entering DrawPoint\n");

   XDrawPoint
   (  gui->display, 
      XtWindow (image->drawing_area), 
      gui->mark_gc,
      x, y
   );

   DEBUG_TRACE_OUT  printf("Leaving DrawPoint\n");
}



/* ========================================================================
 *
 * Function: Handle the highlighting of the current image.
 *
 * Syntax:   HighlightImageEH (w, clientdata, event, flag)
 *              Standard event handler syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void HighlightImageEH
(
   Widget       w,
   XtPointer    clientdata,
   XEvent       *event,
   Boolean      *flag
)
{
   register_gui_t     *gui = (register_gui_t *) clientdata;
   image_set_t        *image_set;
   int                imagenum;
   register_image_t   *image;

   DEBUG_TRACE_IN printf("Entering  HighlightImageEH\n");
 
   image = GetImage (w, gui, &imagenum, &image_set);

   if (image == NULL)
      return;

   XtVaSetValues
   (
      image->frame,
      XmNtopShadowColor, gui->red_pixel,
      XmNbottomShadowColor, gui->red_pixel,
      NULL
   );

   DEBUG_TRACE_OUT printf("Leaving  HighlightImageEH\n");
}

/* ========================================================================
 *
 * Function: De-highlight an image when the cursor leaves.
 *
 * Syntax:   DehighlightImageEH (w, clientdata, event, flag)
 *              Standard event handler syntax
 *
 * History:  Harkin, 6/99
 *
 * Notes:
 *
 * =========================================================================
 */

void DehighlightImageEH
(
   Widget       w,
   XtPointer    clientdata,
   XEvent       *event,
   Boolean      *flag
)
{
   register_gui_t     *gui = (register_gui_t *) clientdata;
   image_set_t        *image_set;
   int                imagenum;
   register_image_t   *image;

   DEBUG_TRACE_IN printf("Entering  HighlightImageEH\n");
 
   image = GetImage (w, gui, &imagenum, &image_set);

   if (image == NULL)
      return;

   XtVaSetValues
   (
      image->frame,
      XmNtopShadowColor, gui->ts_color.pixel,
      XmNbottomShadowColor, gui->bs_color.pixel,
      NULL
   );
   DEBUG_TRACE_OUT printf("Leaving  DehighlightImageEH\n");
}


/* ==========================================================================
 *
 * Function:     InitializeGui
 *
 * Purpose:      Initialize as necessary for the register gui at the
 *               time of popup.
 *
 * Syntax:       void InitializeGui (gui *)
 *                  register_gui_t *gui  - pointer to the main register gui
 *                                     structure.
 *
 * ==========================================================================
 */

void InitializeGui (register_gui_t *register_gui)
{
   XGCValues           gcvalues;
   XColor              red_pixel, yellow_pixel, exact;

   XtTranslations     trans_table;

   DEBUG_TRACE_IN printf ("Entering InitializeRegisterGui\n");

   register_gui->menubar.default_zoom = 2;
/*
   XtAddActions (actionsTable, XtNumber(actionsTable));
   trans_table = XtParseTranslationTable (defaultTranslations);
   XtAugmentTranslations (register_gui->shell, trans_table);
*/

   register_gui->user_done = 1;
   register_gui->default_width = 256;

   register_gui->left.numimages = 0;
   register_gui->right.numimages = 0;

   gcvalues.function = GXcopy;
   register_gui -> gc = XCreateGC
   (  register_gui->display,
      XtWindow (register_gui->toplevel),
      GCFunction, &gcvalues
   );

   gcvalues.function = GXcopy;
   gcvalues.foreground = register_gui->yellow_pixel;
   register_gui -> mark_gc = XCreateGC
   (  register_gui->display,
      XtWindow (register_gui->toplevel),
      GCFunction | GCForeground, &gcvalues
   );


   DEBUG_TRACE_OUT printf ( "Leaving InitializeRegisterGui\n" );
   return;
}


/* ==========================================================================
 *
 * Function:     InImageSet
 *
 * Purpose:      Determine which of the images in the right or left set
 *               matches the passed widget.
 *
 * Syntax:       Boolean InImageSet (Widget, image_set_t *)
 *
 * ==========================================================================
 */

Boolean InImageSet
(
   Widget           w,
   image_set_t      *image_set
)
{
   int        ct;

   DEBUG_TRACE_IN printf ( "Entering InLeftSet\n" );

   for (ct = 0; ct < image_set->numimages; ct ++)
      if (w == image_set->images [ct].drawing_area)
         return (True);

   DEBUG_TRACE_OUT printf ( "Leaving InLeftSet\n" );

   return (False);
}

/* ==========================================================================
 *
 * Function:     GetImage
 *
 * Purpose:      Return a pointer to the image that correlates to widget w.
 *
 * Syntax:       int GetImage (Widget, register_gui_t *)
 *
 * ==========================================================================
 */

register_image_t *GetImage
(
   Widget           w,
   register_gui_t   *gui, 
   int              *imagenum,
   image_set_t      **image_set
)
{
   int        ct;

   DEBUG_TRACE_IN printf ( "Entering InLeftSet\n" );

   for (ct = 0; ct < gui->left.numimages; ct ++)
      if (w == gui->left.images [ct].drawing_area)
      {
         if (imagenum != NULL)       
         {
            *imagenum = ct;
         }
         if (image_set != NULL)
         {
            *image_set = &(gui->left);
         }
         return (&(gui->left.images[ct]));
      }

   for (ct = 0; ct < gui->right.numimages; ct ++)
      if (w == gui->right.images [ct].drawing_area)
      {
         if (imagenum != NULL)       
         {
            *imagenum = ct;
         }
         if ((void *) image_set != NULL)
         {
            *image_set = &(gui->right);
         }
         return (&(gui->right.images[ct]));
      }

   DEBUG_TRACE_OUT printf ( "Leaving InLeftSet\n" );

   return (NULL);
}


/* ==========================================================================
 *
 * Function:     GetImageByMenu
 *
 * Purpose:      Return a pointer to the image that correlates to popup
 *               menu rowcol widget w.
 *
 * Syntax:       int GetImageByMenu (Widget, register_gui_t *)
 *
 * ==========================================================================
 */

register_image_t *GetImageByMenu
(
   Widget           w,
   register_gui_t   *gui, 
   int              *imagenum,
   image_set_t      **image_set
)
{
   int                ct;

   DEBUG_TRACE_IN printf ( "Entering InLeftSet\n" );

   for (ct = 0; ct < gui->left.numimages; ct ++)
      if (w == gui->left.images [ct].popup_menu.rowcol)
      {
         if (imagenum != NULL)       
         {
            *imagenum = ct;
         }
         if (image_set != NULL)
         {
            *image_set = &(gui->left);
         }
         return (&(gui->left.images[ct]));
      }

   for (ct = 0; ct < gui->right.numimages; ct ++)
      if (w == gui->right.images [ct].popup_menu.rowcol)
      {
         if (imagenum != NULL)       
         {
            *imagenum = ct;
         }
         if ((void *) image_set != NULL)
         {
            *image_set = &(gui->right);
         }
         return (&(gui->right.images[ct]));
      }

   DEBUG_TRACE_OUT printf ( "Leaving InLeftSet\n" );

   return (NULL);
}

/*
void MenuPopup (Widget w, XEvent *event, String *params, Cardinal nparam)
{
      XmMenuPosition (gui->popup_menu.menu, event);
      XtManageChild (gui->popup_menu.menu); 
};
*/


/* ==========================================================================
 *
 * Function:     BuildPopupMenu
 *
 * Purpose:      Build the right mouse button popup menu.
 *
 * Syntax:       void BuildPopupMenu (gui, image)
 *                  register_gui_t *gui  - pointer to the main register gui
 *                                     structure.
 *                  register_image_t *image  - pointer to the image
 *
 * ==========================================================================
 */

void BuildPopupMenu 
(  register_gui_t  *gui,
   register_image_t *image,
   int              side
)
{
   ArgList       args;
   XColor        blue_pixel, exact;

   DEBUG_TRACE_IN printf ("Entering BuildPopupMenu\n");

 
   image->popup_menu.rowcol = XmCreatePopupMenu
   (
      image->drawing_area,
      "menushell",
      NULL,
      0
   );

   XtAddEventHandler
   (
      image->drawing_area,
      ButtonReleaseMask | ButtonPressMask,
      False,
      ImageClickEH,
      (XtPointer) gui
   );


   image->popup_menu.label = XtVaCreateManagedWidget
   (
      "Options",
      xmLabelWidgetClass, 
      image->popup_menu.rowcol,
      XmNforeground, gui->blue_pixel,
      NULL
   );

   image->popup_menu.zoom = XtVaCreateManagedWidget
   (
      "Zoom",
      xmPushButtonWidgetClass,
      image->popup_menu.rowcol,
      NULL
   );

   XtAddCallback 
   (  image->popup_menu.zoom,
      XmNactivateCallback,
      ZoomImageCB,
      (XtPointer) gui
   );

   image->popup_menu.remove = XtVaCreateManagedWidget
   (
      "Remove Point",
      xmPushButtonWidgetClass,
      image->popup_menu.rowcol,
      NULL
   );

   XtAddCallback 
   (  image->popup_menu.remove,
      XmNactivateCallback,
      RemoveMarkCB,
      (XtPointer) gui
   );

   image->popup_menu.clear = XtVaCreateManagedWidget
   (
      "Clear All",
      xmPushButtonWidgetClass,
      image->popup_menu.rowcol,
      NULL
   );

   XtAddCallback 
   (  image->popup_menu.clear,
      XmNactivateCallback,
      ClearPointsCB,
      (XtPointer) gui
   );

/*
   image->popup_menu.link = XtVaCreateManagedWidget
   (
      "Link",
      xmPushButtonWidgetClass,
      image->popup_menu.rowcol,
      NULL
   );

   XtAddCallback 
   (  image->popup_menu.link,
      XmNactivateCallback,
      LinkImageCB,
      (XtPointer) gui
   );
*/

   if (side == RIGHT_SIDE)
   {
      image->popup_menu.reg_image = XtVaCreateManagedWidget
      (
         "Register",
         xmPushButtonWidgetClass,
         image->popup_menu.rowcol,
         NULL
      );

      XtAddCallback 
      (  image->popup_menu.reg_image,
         XmNactivateCallback,
         RegisterImageCB,
         (XtPointer) gui
      );
   }

/*
   image->popup_menu.center = XtVaCreateManagedWidget
   (
      "Center",
      xmPushButtonWidgetClass,
      image->popup_menu.rowcol,
      NULL
   );

   XtAddCallback 
   (  image->popup_menu.center,
      XmNactivateCallback,
      CenterMarkCB,
      (XtPointer) gui
   );

   image->popup_menu.outline = XtVaCreateManagedWidget
   (
      "Outline",
      xmPushButtonWidgetClass,
      image->popup_menu.rowcol,
      NULL
   );

   XtAddCallback 
   (  image->popup_menu.outline,
      XmNactivateCallback,
      CenterMarkCB,
      (XtPointer) gui
   );
*/

   DEBUG_TRACE_OUT printf ("Leaving BuildPopupMenu\n");
 
   return;
}

image_set_t *OtherImageSet 
(  register_gui_t   *gui,
   image_set_t      *image_set
)
{
   DEBUG_TRACE_IN printf ("Entering BuildPopupMenu\n");

   if (image_set == &(gui->left))
      return (&(gui->right));
   else if (image_set == &(gui->right))
      return (&(gui->left));
   else
      return (NULL);

}


/* ==========================================================================
 *
 * Function:     LinkImages
 *
 * Purpose:      Build the link for two images in different sets.
 *
 * Syntax:       int LinkImages 
 *                  (gui, image1_set, image1, image1_num, 
 *                        image2set, image2, image2_num)
 *
 * ==========================================================================
 */

int LinkImages 
(  register_gui_t   *register_gui, 
   image_set_t      *image1_set, 
   register_image_t *image1, 
   int              image1_num,
   image_set_t      *image2_set, 
   register_image_t *image2, 
   int              image2_num
)
{
   int      fnum, vnum;

   DEBUG_TRACE_IN printf ("Entering LinkImages\n");

   /*
    * If the images are from the same set, that is an error.
    */

   if (image1_set == image2_set)
   {
      DT_error 
      (  register_gui->toplevel, 
         "Second mark must be in the  other image set", 
         "Registration Error", 
         NULL
      );
  
      DEBUG_TRACE_OUT printf ("Leaving LinkImages\n");

      return (-1);
   }

   /*
    * If one of the images is already linked, that is an error.
    */

   if (image1->linked)
   {
      DT_error 
      (  register_gui->toplevel, 
         "Source image already linked - must unlink first", 
         "Registration Error", 
         NULL
      );
  
      DEBUG_TRACE_OUT printf ("Leaving LinkImages\n");

      return (-1);
   }

   if (image2->linked)
   {
      DT_error 
      (  register_gui->toplevel, 
         "Destination image already linked - must unlink first", 
         "Registration Error", 
         NULL
      );
  
      return (-1);
   }

   /*
    * Set up the variables.
    */

   image1->linked = True;
   image1->linkid = image2_num;
   image2->linked = True;
   image2->linkid = image1_num;

   DEBUG_TRACE_OUT printf ("Leaving LinkImages\n");

   return (0);
}

/* ==========================================================================
 *
 * Function:     UnlinkImage
 *
 * Purpose:      Destroy the link for two images in different sets.
 *
 * Syntax:       void UnlinkImage
 *                  (gui, image1_set, image1, image1_num, 
 *                        image2set, image2, image2_num)
 *
 * ==========================================================================
 */

void UnlinkImage 
(  register_gui_t   *gui, 
   image_set_t      *image1_set, 
   register_image_t *image1, 
   int              image1_num
)
{
   DEBUG_TRACE_IN printf ("Entering UnlinkImage\n");

   /*
    * 
    */

   DEBUG_TRACE_OUT printf ("Leaving UnlinkImage\n");
}

unsigned char *RegisterOneImage 
(  register_gui_t      *gui, 
   register_image_t    *fimage, 
   int                 fsizex,
   int                 fsizey,
   register_image_t    *vimage,
   int                 vsizex,
   int                 vsizey
)
{
   unsigned char       *newimage;
   
   DEBUG_TRACE_IN printf ("Entering RegisterOneImage\n");

   /*
    * First, decide on the standard width and height.  Until now,
    * they could be different, but not during registration.   To avoid
    * potential reduction in image quality, modify the fixed 
    * image size if necessary.
    */

   newimage = (unsigned char *) malloc (vsizex * vsizey);
   switch (gui->menubar.model)
   {
      case LREG:
         if (RegisterByLreg (fimage, vimage, vsizex, vsizey, newimage) < 0)
         {
            DT_error 
            (  gui->toplevel, 
               "Failed to register", 
               "File Error", 
               NULL
            );
            return (NULL);
         }
         break;

      default:
         newimage = NULL;
   }

   DEBUG_TRACE_OUT printf ("Leaving RegisterOneImage\n");

   return (newimage);
}

/* ========================================================================
 *
 * Function: Routines to handle changing the marker point color.
 *
 * Syntax:   PointColorChangeCB (w, clientdata, call_data)
 *              Standard callback syntax
 *
 * Notes:
 *
 * =========================================================================
 */

void PointColorChangeCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *reg_gui = (register_gui_t *) client_data;
   int              color;


   DEBUG_TRACE_IN printf ("Entering PointColorChangeCB\n");

   /*
    * What is the new color - find out by looking at the widget id
    */

   if (w == reg_gui->menubar.point_yellow)
      color = 0;
   else if (w == reg_gui->menubar.point_red)
      color = 1;
   else if (w == reg_gui->menubar.point_blue)
      color = 2;
   else if (w == reg_gui->menubar.point_green)
      color = 3;
   else if (w == reg_gui->menubar.point_magenta)
      color = 4;
   else if (w == reg_gui->menubar.point_cyan)
      color = 5;
   else
      return;

   ChangeMarkColor (reg_gui, color);

   if (reg_gui->left.loaded)
      RefreshImageSet (reg_gui, &(reg_gui->left));

   if (reg_gui->right.loaded)
      RefreshImageSet (reg_gui, &(reg_gui->right));

   DEBUG_TRACE_OUT printf ("Leaving PointColorChangeCB\n");
}

void ChangeMarkColor (register_gui_t *reg_gui, int color)
{
   XColor     pixel, exact;
   XGCValues  gcvalues;

   DEBUG_TRACE_IN printf ("Entering ChangeMarkColor \n");

   if ((color < 0) || (color > 5))
      return;

   switch (color)
   {
      case 0:
         gcvalues.foreground = reg_gui->yellow_pixel;
         break;

      case 1:
         gcvalues.foreground = reg_gui->red_pixel;
         break;

      case 2:
         gcvalues.foreground = reg_gui->blue_pixel;
         break;

      case 3:
         gcvalues.foreground = reg_gui->green_pixel;
         break;

      case 4:
         gcvalues.foreground = reg_gui->magenta_pixel;
         break;

      case 5:
         gcvalues.foreground = reg_gui->cyan_pixel;
         break;

   }

   gcvalues.function = GXcopy;
   reg_gui -> mark_gc = XCreateGC
   (  reg_gui->display,
      XtWindow (reg_gui->toplevel),
      GCFunction | GCForeground, &gcvalues
   );

   DEBUG_TRACE_OUT printf ("Leaving ChangeMarkColor \n");

}

void RefreshImageSet 
(  register_gui_t *reg_gui, 
   image_set_t    *image_set
)
{
   int         ct;

   DEBUG_TRACE_IN printf ("Entering RefreshImageSet\n");

   for (ct = 0; ct < image_set->numimages; ct ++)
      XClearArea 
      (  reg_gui->display, 
         XtWindow (image_set->images[ct].drawing_area),
         0, 0,
         image_set->images[ct].ximage->width, 
         image_set->images[ct].ximage->height,
         True
      );

   DEBUG_TRACE_OUT printf ("Leaving RefreshImageSet\n");

}
