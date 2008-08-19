#ifndef PICSHELLH
#define PICSHELLH

#ifdef DECLARATION
#define externOrNot /* nil */
#else
#define externOrNot extern
#endif  /* DECLARATION */

#define MAX_PICS 1024              /* no more than 1024 pics loaded at once          */
#define MAX_INFO_LENGTH 65536      /* Max amount of info displayed in the info text field */

typedef struct _img_arr_type
{
  Widget rc;                       /* a row column to hold the XImage and Z-value    */
  Widget window;                   /* scrolled window for an XImage                  */
  Widget draw_area;                /* each drawing area holds one XImage for display */
  Widget pic_label;                /* The z-value label for each picture             */
  Widget dose_val_button;          /* CLA - 5-14-98 will report value under mouse    */
  GC gc;                           /* each XImage gets a graphics context            */
  XImage * image;                  /* XImages for the filmstrip preview              */
  XImage * contoured_image;        /* XImages with contour lines                     */
  XImage * colorwashed_image;      /* XImages with colorwashed regions               */
  int    colorwash_present;        /* Boolean - has colorwash been generated         */
  int    contoured_present;

  /****** The rest is added for xcontours ********************************************/
  unsigned char * raw_data;        /* The raw data for the image                     */
           char * fname_data;      /* The filename of the raw image                  */
           char * fname_mask;      /* The filename of the mask                       */
           char * fname_contours;  /* The filename of the contours                   */
  unsigned char * masked_array;    /* True/False array of areas masked               */
  unsigned char * mask_buffer;     /* corresponds to 'buffer' for areas masked       */
  int             mask_in_use;     /* 1 if a mask has been loaded                    */
  int             contours_in_use; /* 1 if contours for image have been loaded       */
  float           dose_z_value;
} img_arr_type;

typedef struct _image_matrix_type
{
  Widget shell;                    /* same as xcontoursTopLevelShell -- used once    */
                                   /* for make_colormap_window_children              */
  Widget window;                   /* scrolled window inside of shell for form       */
  Widget rc;                       /* row/column that holds all the pictures         */
  int size;                        /* maximum number of XImages I can hold           */
                                   /* ** See MAX_PICS above                          */
  int num_pics;                    /* number of XImages currently in memory          */
  int num_dosages;                 /* Number of dosages that have been specified     */
  img_arr_type img_arr[MAX_PICS];  /* several images -> need arrays of things        */
  int active_picture;              /* out of range if none, else currently selected  */
  Display *dpy;                    /*These three are set once at program initialization*/
  int screen;
  int maxHWcmaps;
  int pic_width;                   /* preview width                                  */
  int pic_height;                  /* preview height                                 */
  int rc_width;                    /* rc width   (rc holds all pictures and          */
                                   /*             is in a scrolled window)           */
  int rc_height;                   /* rc height  (rc holds all pictures and          */
                                   /*             is in a scrolled window)           */
  int toggle_enable;               /* turn on to allow the active (red) picture to
				    * change -- turned off by SmartLoader*/
  int image_set;                   /* increments every time you click on unload images */
} image_matrix_type;

externOrNot image_matrix_type image_matrix;



/****************************************************************************************/
/* FUNCTION PROTOTYPES                                                                  */
/****************************************************************************************/
/* initializes some values in the image_matrix structure */
externOrNot void image_matrix_init(void);

/* simply creates a scrolled window given a parent       */
externOrNot Widget make_pic_window(Widget parent);

/* updates the filename labels in the image matrix    */
externOrNot void make_window_filename_labels(void);

/*externOrNot void make_window_dose_buttons();*/

externOrNot void PIC_init(void);                    /* if previewable pixmaps are available, this function places the
					         * pixmaps into scrolled windows for viewing when the user selects
					         * to preview the output
					         */

externOrNot void destroypix(void);                  /* removes all previewable pixmaps from the viewing area */

/* -> need to redraw part of the image                   */
externOrNot void PictureExposedCB(Widget w, XtPointer clientData, XmDrawingAreaCallbackStruct *cbs);

/* -> image has been clicked, (un)select image           */
externOrNot void PictureToggledCB(Widget w, XtPointer clientData, XtPointer callData);

/* -> image has been clicked, (un)select image           */
externOrNot void toggle_on(int i, int dodraw);

/* associate picture to a dosage                        */
externOrNot void addto_dosage_list(char *dose_file_name, int picture_index, float z_val);

/* remove picture/dosage association                    */
externOrNot void removefrom_dosage_list(int picutre_index);

/* associate picture to a mask                          */
externOrNot void addto_mask_list(char *mask_file_name, int picture_index);

/* remove picture/mask association                      */
externOrNot void removefrom_mask_list(int picture_index);
/****************************************************************************************/

#endif

