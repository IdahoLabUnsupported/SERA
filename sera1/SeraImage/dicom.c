
/* ================================================================
 *  
 *   Procedure : Save_DicomCB
 * 
 *   Written by: Harkin
 * 
 *   Parameters: Save the current image set in dicom format.
 * 
 *   Purpose: 
 * 
 * =================================================================
*/

#include "toqsh.h"
#include "dicom.h"
#include "dcm2qsh.h"
#include "libdcm.h"

/*
 * Read a single Dicom file.
 */

Boolean InitImages (main_gui_t *gui, qsh_data_T *qsh_data)
{
   unsigned int    xsize, ysize, ct;

   DEBUG_TRACE_IN printf("Entered InitImages\n");


   if ((! qsh_data -> qd_valid.va_width) ||
       (! qsh_data -> qd_valid.va_height))
      return (False);
   
   xsize = qsh_data -> qd_width;
   ysize = qsh_data -> qd_height;
   gui->qsh_gui->qsh_info->images = (unsigned char *)MT_malloc(xsize * ysize);
   if (gui->qsh_gui->qsh_info->images == NULL)
      return (False);

   init_qsh_info_validity(gui->qsh_gui->qsh_info);

   gui->qsh_gui->qsh_info->valid.number_of_dimensions = 1;
   gui->qsh_gui->qsh_info->number_of_dimensions = 3;
    
   gui->qsh_gui->qsh_info->size_of_dimension[0] = 0;
   gui->qsh_gui->qsh_info->valid.size_of_dimension[0] = 1;
   gui->qsh_gui->qsh_info->size_of_dimension[1] = xsize;
   gui->qsh_gui->qsh_info->valid.size_of_dimension[1] = 1;
   gui->qsh_gui->qsh_info->size_of_dimension[2] = ysize;
   gui->qsh_gui->qsh_info->valid.size_of_dimension[2] = 1;
   gui->qsh_gui->qsh_info->image_referencing = 2;

   /*
    * Patient name
    */

   if (qsh_data -> qd_valid.va_name)
   {
      strcpy (gui->qsh_gui->qsh_info -> patient_name, qsh_data->qd_name);
      gui->qsh_gui->qsh_info->valid.patient_name = 1;
   }

   /*
    * Slice orientation
    */

   if (qsh_data -> qd_valid.va_orientation)
   {
      gui->qsh_gui->qsh_info->valid.slice_orientation = 1;
      switch (qsh_data->qd_orientation)
      {
          case Axial:
            strcpy (gui->qsh_gui->qsh_info -> slice_orientation, "Transverse");
            break;
          case Coronal:
            strcpy (gui->qsh_gui->qsh_info -> slice_orientation, "Coronal");
            break;
          case Sagittal:
            strcpy (gui->qsh_gui->qsh_info -> slice_orientation, "Sagittal");
            break;
      }
   }
   else
      strcpy (gui->qsh_gui->qsh_info -> slice_orientation, "Unknown");

   /*
    * Dimensionality
    */

   if (qsh_data -> qd_valid.va_dimensionality)
   {
      gui->qsh_gui->qsh_info->valid.dimensionality = 1;
      switch (qsh_data->qd_dimensionality)
      {
          case CM:
            strcpy (gui->qsh_gui->qsh_info -> slice_orientation, "cm");
            break;
          case MM:
            strcpy (gui->qsh_gui->qsh_info -> slice_orientation, "mm");
            break;
      }
   }
   else
      strcpy (gui->qsh_gui->qsh_info -> slice_orientation, "Unknown");

   /*
    * Bytes per pixel
    * Currently, everything is done as 1 pixel format.
    */

   gui->qsh_gui->qsh_info->valid.bytes_per_pixel = 1;
   gui->qsh_gui->qsh_info->byte_referencing = 1;

   /*
    * For later if needed - GJH
    * 
   if (qsh_data -> qd_valid.va_bytesperpixel)
   {
      gui->qsh_gui->qsh_info->valid.bytes_per_pixel = 1;
      gui->qsh_gui->qsh_info -> bytes_per_pixel = qsh_data->qd_bytesperpixel;
   }
   else
      gui->qsh_gui->qsh_info -> bytes_per_pixel = 1;
    
   if (gui->qsh_gui->qsh_info->bytes_per_pixel == 1)
      gui->qsh_gui->byte_referencing = 1;
   else
      gui->qsh_gui->byte_referencing = 2;
   */
      gui->qsh_gui->qsh_info -> bytes_per_pixel = 1;

   /*
    * Byte order
    * Doesn't matter if bpp is 1.
    */

   /*
   if (qsh_data -> qd_valid.va_byteorder)
   {
      gui->qsh_gui->qsh_info->valid.byte_order= 1;
      if (qsh_data -> qd_byteorder == BigEndian)
         strcpy (gui->qsh_gui->qsh_info -> byte_order, "big endian"); 
      else
         strcpy (gui->qsh_gui->qsh_info -> byte_order, "little endian"); 
   }
   */


   /*
    * Pixel sizes
    */

   if (qsh_data -> qd_valid.va_xpixel_size)
   {
      gui->qsh_gui->qsh_info->valid.x_pixel_size = 1;
      gui->qsh_gui->qsh_info -> x_pixel_size = qsh_data -> qd_xpixel_size;
   }

   if (qsh_data -> qd_valid.va_ypixel_size)
   {
      gui->qsh_gui->qsh_info->valid.y_pixel_size = 1;
      gui->qsh_gui->qsh_info -> y_pixel_size = qsh_data -> qd_ypixel_size;
   }


   /*  
    * initialize the image locations to all zero for images 
    */

   for(ct = 0; ct < MAX_QSH_SLICES; ct++ )
      gui->qsh_gui->qsh_info->image_location[ct] = 0.00;
    
   /*
    * Make sure we tell libqsh that there are no key alias mappings, 
    * otherwise it will try to free them 
    */

    gui->qsh_gui->num_mappings = 0;


   DEBUG_TRACE_OUT printf("Done with InitImages\n");

   return (True);
}

Boolean ReadDicomImage (main_gui_t *gui, char *filename, qsh_data_T *qinfo)
{
   FILE         *dfile;

   DEBUG_TRACE_IN printf("Entered ReadDicomImage\n");

   /*
    * Open the file and convert to the qsh structure.
    */

   dfile = DcmOpenInputFile (filename);
   if (dfile == NULL)
      return (False);

   if (Dcm2Qsh (dfile, qinfo) < 0)
      return (False);

   fclose (dfile);

   DEBUG_TRACE_OUT printf("Done with ReadDicomImage\n");
   return (True);
}

void NormalizeTwoByteImage (qsh_data_T *qdata)
{
   unsigned int    ct, width, height, min, max;
   unsigned long   temp, range;
   unsigned char   *ip, *np;

   DEBUG_TRACE_IN printf("Entered NormalizeTwoByteImage\n");

   /*
    * Convert from 2 bytes to 1, but accompany the conversion
    * with a histogram stretching the maximized the contrast in
    * the image.
    */

   /*
    * First, determine the minimum and maximum value in the image,
    * keeping the byte order in mind.  Most DICOM images are
    * in little endian order.
    */

   min = 65535;
   max = 0;
   width = qdata -> qd_width;
   height = qdata -> qd_height;
   ip = qdata -> qd_pixels;
   for (ct = 0; ct < width * height; ct ++)
   {
      if (qdata -> qd_byteorder == BigEndian)
      {
         temp = *ip++;
         temp = temp << 8;
         temp += *ip++;
      }
      else
      {
         temp = *(ip+1);
         temp = temp << 8;
         temp += *ip++;
         ip ++;
      }

      if (temp < min)
         min = temp;
      if (temp > max)
         max = temp;
   }

   /* 
    * Now convert and store.
    */

   range = max - min;

   ip = qdata -> qd_pixels;
   np = qdata -> qd_pixels;
   for (ct = 0; ct < width * height; ct ++)
   {
      if (qdata -> qd_byteorder == BigEndian)
      {
         temp = *ip++;
         temp = temp << 8;
         temp += *ip++;
      }
      else
      {
         temp = *(ip+1);
         temp = temp << 8;
         temp += *ip++;
         ip ++;
      }
      *np ++ = (unsigned char) ((float) (temp - min) * 255.0 /
         (float)range);
   }

   DEBUG_TRACE_OUT printf("Leaving NormalizingTwoByteImage\n");
   return;
}

/*
 * Convert an image to desired format
 */

void ConvertImage (main_gui_t *gui, qsh_data_T *qdata)
{
   unsigned int     xsize, ysize;
   unsigned char    *resized_image, *new_images, *ptr;
   unsigned int     sizeof_image, old_sizeof_set, new_sizeof_set;
   
   DEBUG_TRACE_IN printf("Entered ConvertImage\n");

   /*
    * All images are converted to single byte.  This requires
    * a conversion if the dicom file is two bytes.   Note that this
    * could change in the future if we decide to handle two byte
    * images.  For now, this saves some space.
    */

   if (qdata->qd_bytesperpixel == 2)
   {
      NormalizeTwoByteImage (qdata);
      gui->qsh_gui->qsh_info->valid.bytes_per_pixel = 1;
   }
    
   /*
    *  Check to see if this image is the same size as the ones loaded
    *  if not, resize it before adding it. 
    */

   xsize =  gui->qsh_gui->qsh_info->size_of_dimension[1];
   ysize =  gui->qsh_gui->qsh_info->size_of_dimension[2];
   sizeof_image = xsize * ysize;
   old_sizeof_set = sizeof_image * 
         gui->qsh_gui->qsh_info->size_of_dimension[0];
   new_sizeof_set = sizeof_image * 
         (gui->qsh_gui->qsh_info->size_of_dimension[0] + 1);
   if ((qdata -> qd_width != xsize) ||
	(qdata -> qd_height != ysize))
   {

      resized_image = (unsigned char *)MT_malloc(sizeof_image);

      generic_resize_image
      (   qdata->qd_pixels, resized_image,
          qdata -> qd_width, qdata -> qd_height,
	  xsize,ysize,
          1
      );
      MT_free ((void *) qdata->qd_pixels);
      qdata -> qd_pixels = resized_image;
   }


  /*
   *  Add the image to the qsh structure.
   */

   if (gui->qsh_gui->qsh_info->size_of_dimension [0] > 0)
   {

      /*
       * There are already some images, so add it to the structure.
       * Create a new memory structure and copy everything over.
       */

       new_images = (unsigned char *) MT_malloc (new_sizeof_set);
  
       memcpy(new_images, gui->qsh_gui->qsh_info->images, old_sizeof_set);
       ptr = &new_images[old_sizeof_set];
       memcpy(ptr, qdata -> qd_pixels, sizeof_image);
    
       MT_free((void *)gui->qsh_gui->qsh_info->images);
       gui->qsh_gui->qsh_info->images = new_images;

       gui->qsh_gui->qsh_info->size_of_dimension[0]++;
    }
    else 
    {
       /*
        * First image in the set.
        */

       gui->image_block.width = 128;
       gui->image_block.height = 128;
    
       memcpy(gui->qsh_gui->qsh_info->images,qdata -> qd_pixels, 
          sizeof_image);
       gui->qsh_gui->qsh_info->size_of_dimension[0] = 1;
  }


   DEBUG_TRACE_OUT printf ("Done with ConvertImage\n");
   return;
}

/*
 * Read a set of Dicom images
 */

int LoadDicom (main_gui_t *gui)
{
   int          ct, nfile_loaded;
   char         msg [256];
   qsh_data_T   qsh_data;

   static unsigned int    fixed_width, fixed_height;
   static float           fixed_xpixelsize, fixed_ypixelsize;
   static byteorder_T     fixed_byteorder; 
   static int             fixed_bytesperpixel; 
   
   DEBUG_TRACE_IN printf("Entered LoadDicom\n");

   /*
    * Load all the images that are specified in the multi_file_select
    * structure.
    *
    * If any errors are found, those images are eliminated.
    */

   nfile_loaded = 0;
   for (ct = 0;ct < gui->mfb.num_files; ct++)
   {
      if (!ReadDicomImage (gui,gui->mfb.files[ct], &qsh_data))
      {
         sprintf (msg, "Could not open \n%s\n   Continue?", 
            gui->mfb.files[ct]);
         if (!DT_decide 
               (  gui->toplevel, gui->app, msg, 
                  "Dicom File Error", "Continue", "Abort"
               ))
         {
            nfile_loaded = 0;
            DEBUG_TRACE_OUT printf("Done with LoadDicom\n");
            return (0);
         }
         else
            continue;
      }

      /* 
       * If images are not already loaded and this is the first image,
       * initialize the structures.
       */

      if ((!gui->images_loaded) && (ct == 0))
      {
         gui->qsh_gui->qsh_info = 
            (qsh_info_t *)MT_malloc(sizeof(qsh_info_t));
         if (gui->qsh_gui->qsh_info == NULL)
         {
            DT_error( gui->toplevel, "Error creating qsh structure", 
               NULL, NULL );
            return (0);
         }
   
         if (! InitImages (gui, &qsh_data))
         {
            DT_error( gui->toplevel, "Error creating image structure", 
               NULL, NULL );
            return (0);
         }
      }

      /*
       * Successfully read - store the information.
       */

      if (ct == 0)
      {

         /*
          * Set up fixed properties for checking against
          * further images.
          */
           
         fixed_width = qsh_data . qd_width;
         fixed_height = qsh_data . qd_height;
         fixed_xpixelsize = qsh_data . qd_xpixel_size;
         fixed_ypixelsize = qsh_data . qd_ypixel_size;

         /*
          * Set, but ignored for now.
         */
         
         fixed_bytesperpixel = qsh_data . qd_bytesperpixel;
         fixed_byteorder = qsh_data . qd_byteorder;
      }
         
      /*
       * Every image reaches here.  First check the required
       * matching elements to make sure that we don't get an inconsistent
       * image set with different sizes and so on.  Some of this could
       * be fixed by converting images on the fly ... someday.
       */

      if ((!qsh_data . qd_valid . va_width) ||
          (!qsh_data . qd_valid . va_height) ||
          (qsh_data . qd_width != fixed_width) ||
          (qsh_data . qd_height != fixed_height))
      {
         sprintf (msg, "Incompatible image dimensions in %s", 
               gui->mfb.files[ct]);
         DT_error( gui->toplevel, msg, NULL, NULL );
         continue;
      }

      if (qsh_data . qd_valid . va_xpixel_size) 
         if ((qsh_data . qd_xpixel_size != fixed_xpixelsize) ||
             (qsh_data . qd_ypixel_size != fixed_ypixelsize))
         {
            sprintf (msg, "Incompatible pixel size in %s", 
                  gui->mfb.files[ct]);
            DT_error( gui->toplevel, msg, NULL, NULL );
            continue;
         }

      /*
      if (qsh_data . qd_valid . va_bytesperpixel) 
         if (qsh_data . qd_bytesperpixel != fixed_bytesperpixel)
         {
            sprintf (msg, "Incompatible bytes per pixel in %s", 
                  gui->mfb.files[ct]);
            DT_error( gui->toplevel, msg, NULL, NULL );
            continue
         }
       */

      /*
      if (qsh_data . qd_valid . va_byteorder) 
         if (qsh_data . qd_byteorder != fixed_byteorder)
         {
            sprintf (msg, "Incompatible bytesorder in %s", 
                  gui->mfb.files[ct]);
            DT_error( gui->toplevel, msg, NULL, NULL );
            continue
         }
       */

      /*
       * Store the per image data.
       */

      gui->qsh_gui->qsh_info->image_location [nfile_loaded] = 
         qsh_data . qd_location;

      
      ConvertImage (gui, &qsh_data);
      nfile_loaded ++;

   }
               
   DEBUG_TRACE_OUT printf ("Done with LoadDicom\n");
   return (nfile_loaded);
}

Boolean is_a_valid_dicom_file (char *filename)
{
   return (True);
}

Boolean write_dicom
(  qsh_info_t     *info,
   char           *filename
)
{
   return (True);
} 
   
void Save_DicomCB (Widget w, XtPointer clientdata, XtPointer calldata)
{
   main_gui_t *gui = (main_gui_t *)clientdata;
   char filename[256];
   char warning_string[256];
   int ok_to_save;
   int i;
  
  
   DEBUG_TRACE_IN printf("Entered Save_DicomCB\n");

   if( is_allowed_callback( gui ) )
   {
      if (!gui->images_loaded)
      {
        DEBUG_TRACE_OUT printf("Done with Save_DicomCB\n");
        return;
      }

      if( gui->specify_ils_when_saving )
      {
         get_image_location_values( gui );
        
         while( gui->specify_ils_when_saving == 1 )
            XtAppProcessEvent( gui->app, XtIMAll );
     }
    
     if (!get_file_name(gui,filename))
     {
        DEBUG_TRACE_OUT printf("Done with Save_DicomCB\n");
        return;
     }

     /*
      * We want the size_of_dimension[0] to match the number
      * of image locations, so make any extra image_locations
      * passed size_of_dimension[0] invalid here.
      */

     for( i = gui->qsh_gui->qsh_info->size_of_dimension[0]; 
          i < MAX_QSH_SLICES; i++ )
     {
        gui->qsh_gui->qsh_info->valid.image_location[i] = 0;
     }
    
    
     ok_to_save = 0;
     
     if (strstr(filename,".qhd") || strstr(filename,".qim")) 
        filename[strlen(filename)-4] = '\0';
      
     strcat(filename,".dcm");
    
     /*
      * See if the file already exists.
      * Ask the user if we can overwrite it if it does.
      */

     if( FT_fileExists( filename ) )
     {
        sprintf( warning_string, "OK to overwrite %s?", filename );
        
        if( DT_decide(gui->toplevel, gui->app, warning_string,
                      "File Already Exists", "YES", "NO") )
        {
           ok_to_save = 1;
        }
     }
     else
     {
        ok_to_save = 1;
     }

      if( ok_to_save )
      {
         DisplayBusyCursor(gui->mainwindow);
        
         if (!write_dicom (gui->qsh_gui->qsh_info,filename))
         {
            DT_error( gui->toplevel, "Error writing the dicom file!", 
               NULL, NULL );
         }
        
         RemoveBusyCursor(gui->mainwindow);
      }
   }
  
   DEBUG_TRACE_OUT printf("Done with Save_QshCB\n");
  
   return;
}


/* ================================================================
 *  
 *   Procedure : Load_DicomCB
 * 
 *   Written by: Harkin
 * 
 *   Parameters: Read dicom files
 * 
 *   Purpose: 
 * 
 * ================================================================
 */

void Load_DicomCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
   main_gui_t *gui = (main_gui_t *)clientdata;
   char       filename[256];
   int        ct, nfile_loaded;

   DEBUG_TRACE_IN printf("Entered Load_DicomCB\n");

   /*
    * Use the multiple file dialog to get a list of file names.
    * The list of files is in gui->mfb->files.
    */

   if (!get_multiple_files(gui->toplevel,gui->app,&gui->mfb))
   {
      DEBUG_TRACE_OUT printf("Done with Load_DicomCB\n");
      return;
   }

   /*
    * Read the list of files, returning the number that
    * are successfully read.
    */

   nfile_loaded = LoadDicom (gui);
   if (nfile_loaded == 0)
   {
      DEBUG_TRACE_OUT printf("Done with Load_DicomCB\n");
      return;
   };


  DisplayBusyCursor(gui->mainwindow);  

  destroy_and_rebuild_images(gui);  
  adjust_manip_window_to_program_changes ( gui );

  if( gui->move_images_button.state == END_IMAGE_MOVE )
     gui->move_images_button.images_were_added = 1;

  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Load_Single_Images_AppendCB\n");
}


