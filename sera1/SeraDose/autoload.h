#ifndef _AUTOLOAD_H
#define _AUTOLOAD_H

#define MAX_LENGTH_FILES 8192
#define MAX_NUMBER_FILES 256
#define LOAD_ALL_MESSAGE "Would you like to\nload all related images\nin that directory?"
#define AUTOLOADER_ERROR_MARGIN 0.001

float get_autoload_scale_factor ( float val );
int dose_goes_with_image ( char *dose_filename, char *image_filename );
int mask_goes_with_image ( char *mask_filename, char *image_filename );
float get_image_z_value_from_filename ( char *filename );

float get_dose_or_mask_z_value_from_filename ( char *filename );
void auto_loader_callback(Widget w, XtPointer clientData, XtPointer callData);
void load_all_images_callback(Widget w, XtPointer clientData, XtPointer callData);
void load_one_image_callback(Widget w, XtPointer clientData, XtPointer callData);

void autoload_image (char *filename_to_load);
void autoload_dose (char *filename_to_load);
void autoload_mask (char *filename_to_load);

void display_images_dosages_masks (void);


#endif
