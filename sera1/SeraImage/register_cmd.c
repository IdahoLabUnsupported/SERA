/* =========================================================================
 *
 * Callbacks and support for the registration commands menu.
 *
 * History:    Harkin, 7/99
 *
 * =========================================================================
 */

#include <strings.h>
#include "toqsh.h"
#include "matrix.h"

int Interpolate (register_gui_t *, int, MBASE_TYPE [2][2]);

int RegisterGuiImage 
(  register_gui_t *, register_image_t *, register_image_t *, 
   int, MBASE_TYPE [2][2]
);

unsigned char *RegisterOneImage ();



/* =========================================================================
 *
 * Function:   Apply to all command.  Using the marks currently
 *             on the images, perform a registration of all images.
 *
 * Notes:      The success of this registration depends largely on 
 *             the amount of distortion and the marks available for
 *             determining the undistorting function.  
 *
 * ==========================================================================
 */

void RegisterApplyToAllCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *reg_gui = (register_gui_t *) client_data;
   int              ct;
   image_set_t      *var_set, *fixed_set;
   register_image_t *vimage, *fimage;
   char             msg [64];
   MBASE_TYPE       T[2][2];
   XmString         xstr;

   DEBUG_TRACE_IN printf ( "Entering RegisterApplyToAllCB\n" );
   
   /*
    * First, make sure that all the images are linked.  This is
    * done by first calling the Interpolate routine, which will
    * create points for any image that isn't currently linked.
    * If there is insufficient data for interpolation, it
    * will tell us.
    */

   fixed_set = &(reg_gui->left);
   var_set = &(reg_gui->right);
   for (ct = 0; ct < var_set->numimages; ct++)
   {
      vimage = &(var_set->images[ct]);
      fimage = &(fixed_set->images[vimage->linkid]);

      if (! vimage->linked)
         if (Interpolate (reg_gui, ct, T) < 0)
         {
            sprintf (msg, "Insufficient data to interpolate for Z = %.4f", 
               vimage->zvalue);
            DT_error
            (  reg_gui->toplevel,
               msg,
               "Registration Error",
               NULL
            );
         return;
         }
         else ;
      else
         CalcLregCoefficients (fimage, vimage, T);

      if (RegisterGuiImage (reg_gui, fimage, vimage, ct, T) < 0)
      {
         sprintf (msg, "Error registering image at Z = %.4f", vimage->zvalue);
         DT_error
         (  reg_gui->toplevel,
            msg,
            "Registration Error",
            NULL
         );
         return;
      }
      xstr = XmStringCreateLocalized ("Unregister");
      XtVaSetValues
      (  vimage->popup_menu.reg_image,
         XmNlabelString, xstr,
         NULL
      );
      XmStringFree (xstr);


   }

   DEBUG_TRACE_OUT printf ( "Leaving RegisterApplyToAllCB\n" );

}

/* =========================================================================
 *
 * Function:   Revert the image sets back to their original displays without
 *             marks and without registration. This is Unregister All.
 *
 * Notes:      
 *
 * ==========================================================================
 */

void RegisterRevertCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *reg_gui = (register_gui_t *) client_data;
   image_set_t      *iset;
   register_image_t *image;
   int              ct, sizex, sizey;
   XmString         xstr;

   DEBUG_TRACE_IN printf ( "Entering RegisterRevertCB\n" );

   iset = &(reg_gui->right);

   for (ct = 0; ct < iset->numimages; ct ++)
      if (image->registered)
         UnregisterImage (reg_gui, iset, ct);

   DEBUG_TRACE_OUT printf ( "Leaving RegisterRevertCB\n" );

}

/* =========================================================================
 *
 * Function:   Interpolate a set of marks for each image based on what is
 *             marked on one or more images.
 *
 * Notes:      This could be tricky, depending on what has been marked.  If
 *             two images in the middle are marked, the interpolation
               on the ends could be very poor.
 *
 * ==========================================================================
 */

void RegisterInterpolateCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *reg_gui = (register_gui_t *) client_data;
   image_set_t      *fixed_set, *var_set;
   register_image_t *vimage, *fimage;
   int              ct, endnum, startnum;
   MBASE_TYPE       T1[2][2], T2[2][2], T[2][2];
   float            factor;


   DEBUG_TRACE_IN printf ( "Entering RegisterInterpolateCB\n" );
   DEBUG_TRACE_OUT printf ( "Leaving RegisterInterpolateCB\n" );

}

int Interpolate 
(  register_gui_t *reg_gui, 
   int imagenum, 
   MBASE_TYPE T[2][2]
)
{
   MBASE_TYPE       T1 [2][2], T2[2][2];
   int              ct, startnum, endnum;
   Boolean          leader, follower, t1_valid;
   image_set_t      *var_set, *fixed_set;
   register_image_t *vimage, *fimage;
   float            factor;
      

   DEBUG_TRACE_IN printf ( "Entering Interpolate\n" );

   var_set = &(reg_gui->right);
   fixed_set = &(reg_gui->left);

   if (var_set->images[imagenum].linked)
      return;

   /*
    * Search for an image on each side that is linked.  Look backwards
    * to the first and then forward to the last.
    */

  
   ct = imagenum;
   startnum = -1;
   while (--ct >= 0)
      if (var_set->images[ct].linked)
      {
         startnum = ct;
         break;
      }

   ct = imagenum;
   endnum = -1;
   while (++ct < var_set->numimages)
      if (var_set->images[ct].linked)
      {
         endnum = ct;
         break;
      }

   /*
    * If the startnum is missing, we need two on the other side and
    * conversely.
    */

   if (startnum < 0) 
      if (endnum < 0)
         return (-1);
      else
      {
         ct = endnum;
         startnum = endnum;
         endnum = -1;
         while (++ct < var_set->numimages)
            if (var_set->images[ct].linked)
            {
               endnum = ct;
               break;
            }

         if (endnum < 0)
            return (-1);
      }
   else
      if (endnum < 0)
      {
         ct = startnum;
         endnum = startnum;
         startnum = -1;
         while (--ct >= 0)
            if (var_set->images[ct].linked)
            {
               startnum = ct;
               break;
            }

         if (startnum < 0)
            return (-1);
      }
   

   /*
    * We have two images.  Calculate the coefficients for each.
    */

   CalcLregCoefficients 
         (&(fixed_set->images[startnum]), &(var_set->images[startnum]), T1);
   CalcLregCoefficients 
         (&(fixed_set->images[endnum]), &(var_set->images[endnum]), T2);

   factor = (float)(imagenum - startnum) / (float)(endnum - startnum);

   T[0][0] = T1[0][0] + factor * (T2[0][0] - T1[0][0]);
   T[0][1] = T1[0][1] + factor * (T2[0][1] - T1[0][1]);
   T[1][0] = T1[1][0] + factor * (T2[1][0] - T1[1][0]);
   T[1][1] = T1[1][1] + factor * (T2[1][1] - T1[1][1]);

   
   DEBUG_TRACE_OUT printf ( "Leaving Interpolate\n" );
}


/* =========================================================================
 *
 * Function:   Remove all marks from all images.
 *
 * Notes:      
 *
 * ==========================================================================
 */

void RegisterRemoveAllCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *reg_gui = (register_gui_t *) client_data;
   image_set_t      *iset;
   register_image_t *image;
   int              ct, sizex, sizey;
   XmString         xstr;

   DEBUG_TRACE_IN printf ( "Entering RegisterRemoveAllCB\n" );

   iset = &(reg_gui->right);
   sizex = iset->info->size_of_dimension[1];
   sizey = iset->info->size_of_dimension[2];

   for (ct = 0; ct < iset->numimages; ct ++)
   {
      image = &(iset->images[ct]);

      image->nmark = 0;
      image->linked = False;

      if (image->registered)
         UnregisterImage (reg_gui, iset, ct);
      else
         XClearArea
         (  reg_gui->display,
            XtWindow (image->drawing_area),
            0, 0,
            iset->di_width, iset->di_height,
            True
         );
   }

   iset = &(reg_gui->left);
   sizex = iset->info->size_of_dimension[1];
   sizey = iset->info->size_of_dimension[2];

   for (ct = 0; ct < iset->numimages; ct ++)
   {
      image = &(iset->images[ct]);

      image->nmark = 0;
      image->linked = False;

      XClearArea
      (  reg_gui->display,
         XtWindow (image->drawing_area),
         0, 0,
         iset->di_width, iset->di_height,
         True
      );
   }
   DEBUG_TRACE_OUT printf ( "Leaving RegisterRemoveAllCB\n" );

}

/* ========================================================================
 *
 * Register an individual image.
 *
 * ========================================================================
 */

int RegisterGuiImage 
(  register_gui_t     *reg_gui, 
   register_image_t   *fimage, 
   register_image_t   *vimage,
   int                imagenum,
   MBASE_TYPE         T[2][2]
)
{
   int            fsizex, fsizey, vsizex, vsizey;
   XmString       newstring;
   unsigned char  *newimage;
   image_set_t    *var_set;

   DEBUG_TRACE_IN printf ( "Entering RegisterGuiImage\n" );

   var_set = &(reg_gui->right);

   /*
    * Get the sizes for the images.
    */

   vsizex = reg_gui->right.info->size_of_dimension[1];
   vsizey = reg_gui->right.info->size_of_dimension[2];
   fsizex = reg_gui->left.info->size_of_dimension[1];
   fsizey = reg_gui->left.info->size_of_dimension[2];

   newimage = (unsigned char *) malloc (vsizex * vsizey);

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

/*
   newimage = RegisterOneImage
   (  reg_gui,
      fimage,
      fsizex, fsizey,
      vimage,
      vsizex, vsizey
   );
*/

   TransformPixels (vimage->image, newimage, vsizex, vsizey, T);

   XDestroyImage (vimage->ximage);

   CreateImage (reg_gui, var_set, imagenum, newimage, vsizex, vsizey);

   XClearArea
   (  reg_gui->display,
      XtWindow (vimage->drawing_area),
      0, 0,
      var_set->di_width, var_set->di_height,
      True
   );

   vimage->registered = True;

   DEBUG_TRACE_OUT printf ( "Leaving RegisterGuiImage\n" );

   return (0);
}

void UnregisterImage
(  register_gui_t    *reg_gui,
   image_set_t       *iset,
   int               imagenum
)
{
   int               sizex, sizey;
   XmString          newstring;
   register_image_t  *image;

   DEBUG_TRACE_IN printf ( "Entering UnregisterImage\n" );

   image = &(iset->images[imagenum]);
   sizex = iset->info->size_of_dimension[1];
   sizey = iset->info->size_of_dimension[2];

   newstring = XmStringCreateLocalized ("Register");
   XtVaSetValues
   (  image->popup_menu.reg_image,
      XmNlabelString, newstring,
      NULL
   );
   XmStringFree (newstring);

   /*
    * Reset things for unregistered.
    */

   image->registered = False;
   XDestroyImage (image->ximage);

   CreateImage
   (  reg_gui, iset, imagenum,
      iset->info->images + imagenum * sizex * sizey,
      sizex, sizey
   );

   XClearArea
   (  reg_gui->display,
      XtWindow (image->drawing_area),
      0, 0,
      iset->di_width, iset->di_height,
      True
   );
 
   DEBUG_TRACE_OUT printf ( "Leaving UnregisterImage\n" );
}


void RegisterPrintCB
(  Widget      w,
   XtPointer   client_data,
   XtPointer   call_data
)
{
   register_gui_t   *reg_gui = (register_gui_t *) client_data;
   image_set_t      *iset;
   register_image_t *image;
   int              ct, sizex, sizey;
   XmString         xstr;

   DEBUG_TRACE_IN printf ( "Entering RegisterPrintCB\n" );
   
   /*
    * Make sure we have an image to print.
    */
   if( reg_gui->left.numimages > 0 )
       PT_print (reg_gui->shell, reg_gui->left.images[0].drawing_area);

   DEBUG_TRACE_OUT printf("Leaving RegisterPrintCB\n");

}



