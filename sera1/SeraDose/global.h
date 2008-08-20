/*
 * global.h 
 * 
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 *
 * This file is necessary for all global variables, definitions, 
 * and prototypes used by various XContours procedures.
 *
 */
  
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "debug_tools.h" 
#include "dialog_tools.h"
#include "libqsh.h"
#include "contours_calc.h"
#include <Xm/Xm.h>

#define TEMP_CONTOUR_FILE_DIRECTORY ".~~temp_contour_files~~/"
#define TEMP_MASK_FILE_DIRECTORY ".~~temp_mask_files~~/"

#define DoseDisplayResourceDir "/SeraDose"
#define DoseDisplayResourceFile "/seradose_prefs.rsc"
#define DoseDisplayContourLevelsFile "/contour_levels.txt"
#define DoseDisplayColorwashFile "/colorwash.cmap"
#define SharedColormapDir "/Shared/colormaps"
#define DoseDisplayGammaFile "/gamma.cmap"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define DATA_WIDTH 256
#define DATA_HEIGHT 256
#define WINDOW_WIDTH 512
#define WINDOW_HEIGHT 512
#define NUMAPPL 10

#define BUTTON	0
#define SCALE	1

#define OK	0
#define CANCEL	1
#define CLEAR	2

#define MAXCOLORS   256

#define DEFAULT_NUM_GRAYS 128    /* default number of grays in gamma_map */
#define NUM_RESCOL 12            /* number of reserved color spots set aside
				  * 12 := 1 variable contour color
				  * + 3 (psbly var) 
				      bnct_rtpe colors (white, yellow, red)
				  * + 8 colors we can count on:
				  *   RESERVED_RED
				  *   RESERVED_GREEN
				  *   RESERVED_BLUE
				  *   RESERVED_CYAN
				  *   RESERVED_YELLOW
				  *   RESERVED_MAGENTA
				  *   RESERVED_BLACK
				  *   RESERVED_WHITE
				  */
				      
/* set up indices in colormap - from 0 to 255 they go something like:
 * 0 to (DEFAULT_MIN_GRAY_INDEX - 1)        
                           0-127:  Standard Colormap
 * DEFAULT_MIN_GRAY_INDEX to MAX_GRAY_INDEX 
                           128-243:  Black -> shades of gray -> white
 * MIN_RESCOL_INDEX to MAX_RESCOL_INDEX         
                           244-255:  Colors set aside and needed by program
 */
#define MAX_RESCOL_INDEX (MAXCOLORS - 1)
#define MIN_RESCOL_INDEX (MAX_RESCOL_INDEX - NUM_RESCOL + 1)
#define MAX_GRAY_INDEX (MIN_RESCOL_INDEX - 1)
#define DEFAULT_MIN_GRAY_INDEX (MAX_GRAY_INDEX - DEFAULT_NUM_GRAYS + 1)

#define CONTOUR_INDEX      (MAX_GRAY_INDEX + 4)
#define RESERVED_RED       (CONTOUR_INDEX + 1)
#define RESERVED_GREEN     (CONTOUR_INDEX + 2)
#define RESERVED_BLUE      (CONTOUR_INDEX + 3)
#define RESERVED_CYAN      (CONTOUR_INDEX + 4)
#define RESERVED_YELLOW    (CONTOUR_INDEX + 5)
#define RESERVED_MAGENTA   (CONTOUR_INDEX + 6)
#define RESERVED_BLACK     (CONTOUR_INDEX + 7)
#define RESERVED_WHITE     (CONTOUR_INDEX + 8)


/*
 * Information for the colormap used
 * for the contour colorwash.
 *  David Helzer 8/97
 */
#define NUMBER_SHADES_PER_COLOR 18
#define COLORS_STARTING_POSITION_IN_CMAP 116
#define MASKED_COLORWASH_COLOR 25  /* chosen arbitrarily, must be
                                      a value that is not capable of being used
                                      as a contour line color */

/* 
 * number of reserved colors -- as long as the program uses only 256 colors, 
 * this value should remain 
 * David Helzer   7-8-97
 */
#define NUMBER_CONTOUR_COLORS 8


/*Defines for set cursor functions. gjh.*/ 
#define CURSOR_DEFAULT  0
#define CURSOR_CLOCK    1
#define CURSOR_ERROR    2

#ifndef M_PI
#define M_PI  (double) 3.14159265358979323846
#endif

/* global macro */
#define min(x, y) (((x)<=(y))?(x):(y))
#define max(x, y) (((x)>=(y))?(x):(y))
#define reverse(value, limit) ( limit - value - 1 )
/***** if DECLARATION is defined in the source
	then "externOrNot"s interpreted as (nil), otherwise extern... *****/

#ifdef DECLARATION
#define externOrNot /* nil */
#else
#define externOrNot extern
#endif  /* DECLARATION */


/***** X11/Motif globals *****/

externOrNot Widget preferences_shell;
externOrNot Widget contour_labels_options_shell;
externOrNot Widget use_scalable_fonts_toggle;
externOrNot Widget view_large_image_labels_toggle;
externOrNot Widget view_preview_image_labels_toggle;
externOrNot Widget large_image_labels_scale;
externOrNot Widget preview_image_labels_scale;
externOrNot Widget scaleDialogShell;
externOrNot Widget scaleWidgetForm;
externOrNot Widget scaleWidget;
externOrNot Widget textDialogShell;    
externOrNot Widget textWidgetTextArea; 
externOrNot Widget textWidgetLabel;    
externOrNot Widget contourColorsLabel;
externOrNot Widget contourColorsList;
externOrNot Widget contourColorsSelectionButtons[NUMBER_CONTOUR_COLORS];
externOrNot Widget contour_legend_label;
externOrNot Widget contour_legend_widget;
externOrNot Widget contour_info_widget;
externOrNot Widget contour_info_text;
externOrNot Widget contour_legend;
externOrNot Widget editColorBlueScale;
externOrNot Widget editColorDrawnButton;
externOrNot Widget editColorGreenScale;
externOrNot Widget editColorRedScale;
externOrNot Widget editColorRowColumn;
externOrNot Widget editColorScrolledWindow;
externOrNot Widget editColorSlidersRowColumn;
externOrNot Widget fileSelectionBox;
externOrNot Widget fileSelectionDialogShell;
externOrNot Widget generate_colorwash_file_button;
externOrNot Widget helpMainWindow;
externOrNot Widget MainWindow;
externOrNot Widget helpTopLevelShell;
externOrNot Widget doseSelectionBox;
externOrNot Widget colormapSelectionBox;
externOrNot Widget sep[2];
externOrNot Widget shell[5], form[2], scale[2], label[2], button[2];
externOrNot Widget AppShell, mainWindowDrawingArea, shellWindowForm;
externOrNot Widget contourTextLabel;
externOrNot Widget xcontoursTopLevelShell, mainWindowScrolledWindow;
externOrNot Widget locaterButton, locaterLabel, image_info;
/*externOrNot Colormap default_colormap;*/
externOrNot Widget loadDosePushButton;
externOrNot Widget qshDoseLoadPushButton;  /* MTC 6/16/98 */
externOrNot Widget qshDoseLoadRightMenu;  /* MTC 12/21/98 */
externOrNot Widget qshDoseLoadAllPushButton;  /* MTC 12/21/98 */
externOrNot Widget qshDoseLoadSinglePushButton;  /* MTC 12/21/98 */
externOrNot Widget qshDoseLoad3DPushButton;      /* MTC 5/13/99 */
externOrNot Widget qshDoseLoadCascadeButton;  /* MTC 12/21/98 */
externOrNot Widget qshMaskLoadPushButton;  /* MTC 6/16/98 */
externOrNot Widget qshMaskLoadRightMenu;  /* MTC 12/21/98 */
externOrNot Widget qshMaskLoadAllPushButton;  /* MTC 12/21/98 */
externOrNot Widget qshMaskLoadSinglePushButton;  /* MTC 12/21/98 */
externOrNot Widget qshMaskLoad3DPushButton;      /* MTC 5/14/99 */
externOrNot Widget qshMaskLoadCascadeButton;  /* MTC 12/21/98 */
externOrNot Widget qshRemoveImagesWODoseButton; /* MTC 8/11/98 */
externOrNot Widget loadMaskPushButton;
externOrNot Widget applyToAllButton;
externOrNot Widget applyToAllButton2;
externOrNot Widget masksReplaceImagesButton;
externOrNot Widget view_contour_lines_button;
externOrNot Widget view_contour_colorwash_button;
externOrNot Widget reveal_legend_button;
externOrNot Widget reveal_info_button;
externOrNot Widget contour_lines_legend_rowcol;
externOrNot Widget contour_colorwash_legend_rowcol;
externOrNot XColor contour_color;
externOrNot XColor grey[MAXCOLORS];
externOrNot XColor high_color;
externOrNot XColor norm_color;
externOrNot XColor point_color;

externOrNot Display *di;
externOrNot Window wi;
externOrNot GC gc, newGC, drag_gc;
externOrNot XImage *global_contoured_image;
externOrNot XImage *global_colorwashed_image;
externOrNot XEvent SureEvent;
externOrNot Cursor crosshairCursor;


/***** i/o globals *****/

externOrNot char *dose_file_name;
externOrNot char *doseString;
externOrNot char *image_file_name;
externOrNot unsigned char *dummy_buffer;
externOrNot unsigned char *dummy_masks;
externOrNot unsigned char values[WINDOW_HEIGHT*WINDOW_WIDTH];
externOrNot unsigned char orig_values[WINDOW_HEIGHT*WINDOW_WIDTH]; 

externOrNot FILE *dose_file;
externOrNot FILE *image_file;

externOrNot float xmin, xmax, ymin, ymax, zmin, zmax;
externOrNot float maxx,maxy,minx,miny;

/*
 * new_font is the font that is created by the procedure LoadQueryScalableFont.
 * It has been placed as a global variable so that it can be accessed easily
 * and so that the memory for it is not lost at any time throughout the run of t
he
 * program using this file.
 */

externOrNot Font new_font;

/*
 * Globals for autoload.c
 */

#define MAX_LENGTH_NAME      256
externOrNot char image_name[MAX_LENGTH_NAME];
externOrNot char image_directory[MAX_LENGTH_NAME];
externOrNot char dose_name[MAX_LENGTH_NAME];
externOrNot char dose_directory[MAX_LENGTH_NAME];
externOrNot char mask_name[MAX_LENGTH_NAME];
externOrNot char mask_directory[MAX_LENGTH_NAME];


#ifdef DECLARATION
float BoronFactor = 1.0;
float GammaFactor = 1.0;
float NitrogenFactor = 1.0;
float FastFactor = 1.0;
float OtherFactor = 1.0;
float BoronRef = 0;
float GammaRef = 0;
float NitrogenRef = 0;
float FastRef = 0;
float OtherRef = 0;
float BoronConc = 0;
float GammaConc = 0;
float NitrogenConc = 0;
float FastConc = 0;
float OtherConc = 0;
float CurFactors[] = { 1.0, 1.0, 1.0, 1.0, 1.0 };
float CurRefs[] = { 0, 0, 0, 0, 0, 0, 0, 0};
float CurConcs[] = { 0, 0, 0, 0, 0};
#else
externOrNot float BoronFactor;
externOrNot float GammaFactor;
externOrNot float NitrogenFactor;
externOrNot float FastFactor;
externOrNot float OtherFactor;
externOrNot float BoronRef;
externOrNot float GammaRef;
externOrNot float NitrogenRef;
externOrNot float FastRef;
externOrNot float OtherRef;
externOrNot float BoronConc;
externOrNot float GammaConc;
externOrNot float NitrogenConc;
externOrNot float FastConc;
externOrNot float OtherConc;
externOrNot float CurFactors[];
externOrNot float CurRefs[];
externOrNot float CurConcs[];
#endif


#ifdef DECLARATION
  char scalable_font_name[50] = "-*-courier-medium-r-normal-*-0-0-*-*-*-0-*-*";
#else
  extern char scalable_font_name[50];
#endif /* DECLARATION */
   
#ifdef DECLARATION
short large_image_label_size = 170;
short preview_image_label_size = 90;
#else
extern short large_image_label_size;
extern short preview_image_label_size;
#endif  /* DECLARATION */


#ifdef DECLARATION
float smoothingValue = 0.0;
#else
extern float smoothingValue;
#endif  /* DECLARATION */


externOrNot int color_index;
externOrNot int color_value[3];

#ifdef DECLARATION
int doseFlag = 8;           /* total dose is default */
int fovDisplayed = FALSE;   /* no Field Of View widget currently instantiated */
#else
extern int doseFlag;
extern int fovDisplayed;
#endif  /* DECLARATION */

externOrNot int is_argv;
externOrNot int ncolors;
externOrNot int num_colors_avail;
externOrNot int slice_is_there, dosage_is_there;
externOrNot int contours_are_current;
externOrNot int value[2];
/*externOrNot int contour_lines_legend_displayed;
externOrNot int contour_colorwash_legend_displayed;*/



/*
 * structure to store contour density levels. 
 *    david helzer:  6-9-97
 */

#define MAX_NUMBER_CONTOUR_LEVELS 300

#ifdef DECLARATION
int default_contour_color = RESERVED_YELLOW;
#else
externOrNot int default_contour_color;
#endif /* DECLARATION */

typedef struct _contour_levels_structure
{
    int   number_levels;
    int   rlevel[MAX_NUMBER_CONTOUR_LEVELS];
    int   colors[MAX_NUMBER_CONTOUR_LEVELS];
} contour_levels_structure;
externOrNot contour_levels_structure contour_levels;

externOrNot int max_contour_value_color;


/*
 * structure to store information for the 
 * colorwashes
 * David Helzer 7/97
 */
#define MAX_NUMBER_REGIONS 301
#define MAXIMUM_POSSIBLE_CONTOUR_VALUE 1000
#define MINIMUM_POSSIBLE_CONTOUR_VALUE 0
 
typedef struct _colorwash_legend_values_type
{
   int number_regions;
   int low_values[MAX_NUMBER_REGIONS];
   int high_values[MAX_NUMBER_REGIONS];
   int colors[MAX_NUMBER_REGIONS];
} colorwash_legend_values_type;
externOrNot colorwash_legend_values_type colorwash_legend_values;

/*externOrNot short colorwash_labels_color 1;*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  defines and structures for remembering files
%%%
%%%  Mark Rossmeier 9/3/1999
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#define FILES_TO_REMEMBER    10  /* keep track of last 10 filenames */
#define MAX_FILENAME_LENGTH  256 /* maximum length of filename */

#define NUM_FILE_TYPES       3
#define QSH_FILES            0
#define DOSE_FILES           1
#define MASK_FILES           2

typedef struct _remembered_files_popup_t
{
    Widget shell;
    Widget form;
    Widget frame;
    Widget label;
    Widget list;

} remembered_files_popup_t;

typedef struct _remembered_files_t
{
    char    files[FILES_TO_REMEMBER][MAX_FILENAME_LENGTH];
    char    filename[MAX_FILENAME_LENGTH];
    char    return_filename[MAX_FILENAME_LENGTH];
    char    file_type_suffix[64];
    
    int     num_files;
    int     need_to_build;

    int     user_done;
    int     save_file_present;
    int     file_loaded;

    XtAppContext rf_app;

    remembered_files_popup_t popup;

} remembered_files_t;

externOrNot remembered_files_t rememberedFiles[ NUM_FILE_TYPES ];

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes for remember_files.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_remembered_files                 ( remembered_files_t * rf );
int get_filename_from_remembered_files_list( remembered_files_t * rf, char * filename );
void add_to_saved_files                    ( remembered_files_t * rf, char * filename );

/*
 * defines data structure for dose factor stuff
 */
typedef struct {
  Widget factor[6], button[6], ref[6], label[4], conc[6], stepnumber;
  Widget name_text, bday_text, id_text, plan_text;
  char   TSNumber[4];
  char   stringName[5][50];
  char   *buttontext[6];
  char   *factortext[6];
  char   *reftext[6];
  char	 *conctext[6];
  int    buttonNumber;
  int    numButtons;
  int    numFactors;
  int    numRefs;
  int	 numConcs;
} driver_data;


/* stuff for contour masking */
typedef struct _mask_type
{
  int in_use;	       /* is the region select box displayed */
  unsigned char *masks;  /* array of mask on/off flags */
  unsigned char *buffer; /* current mask buffer */
  Widget shell;	       /* handle to region select widget */
  Widget maskScreen;     /* mwf -> used in call to mask_region_search */
  Pixmap mask_region;    /* Pixmaps for the actual data */
  unsigned char maskdata[DATA_WIDTH*DATA_HEIGHT];/* current mask region data */
  unsigned char imagedata[DATA_WIDTH*DATA_HEIGHT]; /* mwf:  added 7-11-95
		    * copy of maskdata that uses pixel values reserved in
		    * the gamma_colormap so we'll know what colors we'll
		    * get and we'll know the masks will be contrasting - 
		    * this is the mask image that gets displayed in the window
		    */
} mask_type;

#ifdef DECLARATION
mask_type mask_pack = {FALSE, };
#else
extern mask_type mask_pack;
#endif

externOrNot Widget dose_factor_shell;


typedef struct{
		float x, y, z;
		float totalDose;
		float boronDose;
		float gammaDose;
		float nitrogenDose;
		float fastDose;
		float group1Fluence;
		float group2Fluence;
		float thermalFluence;
		float otherDose;
	       } *floyd_data_ptr, floyd_data;

externOrNot floyd_data_ptr dose_data;

/*#ifdef DECLARATION
floyd_data_ptr original_data;
#else
externOrNot floyd_data_ptr original_data;
#endif*/


externOrNot driver_data current_driver_data;

/* taken from BNCT and used by the confirm dialog */
externOrNot int   CONFIRM;         /* mwf added 1/13/1997 --> = OK or CANCEL */
externOrNot Widget Confirm_dialog; /* mwf added 1/13/1997 --> confirm dialog */

externOrNot int   NUMPOINTS, NUM_X_POINTS, NUM_Y_POINTS;
externOrNot float ZVAL, FOV;

#define LINESIZE 255


/* window definition for postscript outputs */
typedef struct
{
   float xlo, ylo, xhi, yhi;
   char title[80];
   int tloc;
} WM_WINDOW_T;

/* structure for ruler drawing information */
typedef struct _measure_data_t
{
   int        mode;              /* Are we in measure mode or not. */
   float      fov;               /* field of view */
   Position   firstx, firsty;
   Position   lastx, lasty;
} measure_data_t;
externOrNot measure_data_t measure_data;

/* QSH Structure information MTC 7/15/99 */
externOrNot qsh_info_t *qsh_info;


externOrNot void allocColors();
externOrNot void AnalysisCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void masksReplaceImagesCallback(Widget w, XtPointer client, XmAnyCallbackStruct *cbs);
externOrNot void view_contour_lines_callback(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void view_contour_colorwash_callback(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void contour_labels_options_callback(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void large_labels_scale_valchg_callback(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void preview_labels_scale_valchg_callback(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void update_label_size_callback(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void Confirm_dialog_CB(Widget w, XtPointer client_data, XmAnyCallbackStruct *call_data); 
externOrNot void colorCB(Widget w, XtPointer client, XtPointer call);
externOrNot void ColorListCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void ColorSlidersCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void contour_levels_callback(void);
externOrNot void Apply_Contour_Levels_Callback(void);
externOrNot void Apply_Contour_Colors_Callback(void);
externOrNot void Select_Contour_Level_Callback(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void Select_Contour_Color_Callback(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void reveal_legend_callback(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void reveal_info_callback(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void ScalePopupCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void ContourSmoothCallback(Widget w, int select, XmScaleCallbackStruct *c_d);
externOrNot void convert_command();
externOrNot void apply_mask_to_all_CB ( Widget, XtPointer, XtPointer );
externOrNot void clear_masksCB(Widget w, XtPointer client, XmAnyCallbackStruct *cbs);
externOrNot void clickCB(Widget w, caddr_t client_data, XEvent *event);
externOrNot void init_dose_factors(void);
externOrNot void create_contour_cm(Colormap *colormap);
externOrNot void create_default_cm(void);
externOrNot void colormap_install(Widget w, caddr_t client_data, XEvent *event); 
externOrNot Widget create_defaultdose_button();
externOrNot Widget create_dismiss_button(Widget, Widget);
externOrNot void create_gabor_cm();
externOrNot void add_guaranteed_colors(Colormap *);  
externOrNot Widget create_select_button(Widget parent, driver_data *data); 
externOrNot void dose_fileCB(Widget w, int select, XmAnyCallbackStruct *c_d);
externOrNot void defaultdoseCB();
externOrNot void DoseOptionCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void DosePopupCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void SelectColormapCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void DoseFactorCallback(Widget, XtPointer, XtPointer);
externOrNot Widget make_dose_factor_shell(Widget);
externOrNot void setfactors(driver_data *data, floyd_data_ptr original_data);
externOrNot void EditColorDismissCallback();
externOrNot void EditColorNameCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void EditColorPopupCallback();
externOrNot void ContourColorsPopupCallback(void);
externOrNot void EditParametersCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void ExitCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void FilePopupCallback(Widget w, XtPointer client, XtPointer call);
/* externOrNot void FileSelectionCallbackXXX(Widget w, XtPointer client, XtPointer call); */
externOrNot void FileMaskCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void FOVchangeCallback(Widget w, char *client_data, XmAnyCallbackStruct *call_data);
externOrNot void HelpMenuCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void ImageFormat(Widget w, XtPointer client, XtPointer call);
externOrNot void init_colors_old(void);
externOrNot Widget InstallLogo(Widget);
externOrNot void load_colormap(Colormap whichColormap, int low_index, int high_index);
externOrNot void add_dose_to_memory(int i,floyd_data_ptr data, 
				    floyd_data_ptr original_data, 
				    int numpoints, float z_value);
externOrNot void load_dose(char *,floyd_data_ptr *,floyd_data_ptr *, float *);
externOrNot void load_image(FILE *file, Widget w);
externOrNot void load_image_BNCT(Drawable);
externOrNot void load_imageEH(Widget w, caddr_t client_data, XEvent *event);
externOrNot void load_contour_levels_from_string(char *contour_levels_to_load); 
externOrNot void sort_contour_level_values();
externOrNot void check_contour_levels();
externOrNot void update_legend(void);
externOrNot void updateInfoWindow ( int );
externOrNot void update_contour_lines_legend(void);
externOrNot void update_contour_colorwash_legend(void);
externOrNot void update_colorwash_values(void);
externOrNot void set_colorwash_colormap(void);
externOrNot void set_gamma_colormap(void);
externOrNot void make_colormap_window_children(Widget, Colormap);
externOrNot void mask_imageEH(Widget w, caddr_t client_data, XEvent *event);
externOrNot void mask_region_search(Widget, char *);
externOrNot void MeasureDrawLine(Widget, int, int, int, int);
externOrNot void MeasureLineDrag(Widget, int, int, float *, float *);
externOrNot void newFOVCB(Widget w, Widget parent_shell, XtPointer call);
externOrNot void PopdownCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void PopupCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void process_region(unsigned int, unsigned char);
externOrNot void ps_display(WM_WINDOW_T *, int [], float [], unsigned char *, int);
externOrNot void region_selectCB(Widget w, XtPointer client, XmDrawingAreaCallbackStruct *cbs);
externOrNot void ReportToggleArmCB(Widget w, char *client_data, XmToggleButtonCallbackStruct *call_data);
externOrNot void ReserveColorCallback(Widget w, XtPointer client, XtPointer call);
/* externOrNot void resize_image(unsigned char *values); */
externOrNot void recalc_pixels(void);
externOrNot void set_masksCB(Widget w, XtPointer client, XmAnyCallbackStruct *cbs);
externOrNot void setup_gc(Widget w);
externOrNot void UnmanageCallback(Widget w, XtPointer client, XtPointer call);
externOrNot void unclickCB(Widget w, caddr_t client_data, XEvent *event);
externOrNot void update_locater(Widget, caddr_t, XEvent *);
externOrNot void XsMakeButtons();
externOrNot void XsMakeMenubar();
externOrNot void XsPolyLine();

externOrNot void generic_resize_image(unsigned char *data, unsigned char *output, unsigned int inwidth, unsigned int inheight, unsigned int outwidth, unsigned int outheight);
externOrNot void recalc_one_image_pixels(unsigned char *orig_values, unsigned char *values, unsigned int width, unsigned int height);
externOrNot void draw_preview(int ival);
externOrNot void draw_large_image(void);
externOrNot void rawImageFileLoaderCB(Widget w, XtPointer client_data, XtPointer call_data);
externOrNot void rawImageFileLoaderDoneCB(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void smartLoaderCB();
externOrNot void smartLoaderDoneCB(Widget w, XtPointer clientData, XtPointer callData);
externOrNot void update_small_images(int active);
externOrNot void update_masks(int active, unsigned int value, int set_to);
externOrNot void set_unset_masks(int active, int set_to);
externOrNot void labelContours(char *text);
externOrNot void reload_image(int picnum);
externOrNot int  confirm_popup(char *message);          /* added by mwf 1-13-97 */
externOrNot void error_popup (char *message);           /* added by gjh 2-21-97 */
externOrNot void create_confirm_popup(void);   /* added by mwf 1-13-97 */
externOrNot int is_image_file(char *str);           /* mwf added 1-13-97 */
externOrNot int is_mask_file(char *str);            /* mwf added 1-13-97 */
externOrNot int is_dose_file(char *str);            /* mwf added 1-13-97 */
externOrNot XtAppContext context;          /* mwf made global on 1-13-97 */
                         
/* Added by MTC 6/11/98 */
externOrNot void qshLoadCallback (Widget, XtPointer, XtPointer);
externOrNot void qshDoseLoadCallback (Widget, XtPointer, XtPointer);
externOrNot void qshMaskLoadCallback (Widget, XtPointer, XtPointer);
externOrNot void checkVersionCallback (Widget, XtPointer, XtPointer);
externOrNot void launchAppCallback (Widget, XtPointer, XtPointer);
externOrNot void fillPatientName ( char * );
externOrNot void removePatientName ( void );
externOrNot void remove_images_without_dose_CB ( Widget, XtPointer, XtPointer );
externOrNot void load_single_dose_on_qsh (char ** dose_files, int numDoseFiles);
externOrNot void load_single_mask_on_qsh (char ** mask_files, int numMaskFiles);
externOrNot void find_slice              ( float, int );

/*
 * Print functions.
 */
externOrNot void PrintImageCallback(Widget wcall, XtPointer ptype, XmAnyCallbackStruct *c_d);
externOrNot int  PsWrite (FILE *, char *, unsigned char *, int, int, int,
                          float, float);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% added by CLA for measuring the dose based on
%% the mouse position
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
float get_dose_value_under_mouse(int x,int y, int ncols, int nrows);
void toggle_dose_locatorCB(Widget w, XtPointer clientdata, XtPointer calldata);
void MeasureDoseEH (Widget w, XtPointer clientData,
		      XEvent *event, Boolean *flag);

/*=========================================================*/
/* added by MTC for loading 3D contour files               */
/*=========================================================*/
void load_concatenated_contour_file_CB ( Widget w, XtPointer clientData, XtPointer callData );
void load_concatenated_mask_file_CB ( Widget w, XtPointer clientData, XtPointer callData );

void remove_temp_contour_files ( void );
void remove_temp_mask_files ( void );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes for functions.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void load_mask ( char *local_image_file_name );
void check_contour_level_values (char *contour_levels, 
                                 char *all_levels_text, int *text_is_okay);
void sort_contour_levels (void);



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes for load_qsh.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void qsh_PIC_init            ( int );
void highlight_active_picture( void );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes for contour_calc.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int ContoursCalculate( floyd_data_ptr fdata, contour_data_type * contours,
                       int ncntr, int which_dose );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  prototypes for file_select.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_file_name(XtAppContext app, Widget w, char *name, char *title);
