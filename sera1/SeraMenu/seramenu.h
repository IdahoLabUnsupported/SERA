#ifndef  SERAMENU_H
#define  SERAMENU_H

/*%%%%%%%%%%%%%%%%%%%%%%%%%
 * INCLUDES
 *%%%%%%%%%%%%%%%%%%%%%%%%%*/
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <signal.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <wait.h>

/*%%%%%%%%%%%%%%%%%%%%%%%%%
 * INCLUDES FOR WIDGETS
 *%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
#include <Xm/Xm.h>
#include <Xm/ArrowB.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MessageB.h>
#include <Xm/MwmUtil.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 * LOCAL INCLUDES
 *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#include "debug_tools.h"
#include "dialog_tools.h"
#include "memory_tools.h"
#include "libhelp.h"
#include "environment_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 * DEFINES FOR EXECUTABLE NAMES
 *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#define SERA_IMAGE  "seraImage"
#define SERA_MODEL  "seraModel"
#define SERA_3D     "sera3d"
#define SERA_DOSE   "seraDose"
#define SERA_PLOT   "seraPlot"
#define SERA_CALC   "seraCalc"
#define SERA_PLAN   "seraPlan"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 * DEFINES FOR PREFERENCES
 *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#define NUM_PREFERENCES 4
#define COLORS          0
#define FONTS           1
#define MISC            2
#define SIZES           3

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 * DEFINES FOR PROCESS ID'S
 *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#define MAX_NUMBER_PIDS 100
#define MAX_LENGTH_NAME 30

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 * DEFINES FOR ADVANCED MDOE
 *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#define ADVANCED_MODE_STRING_LENGTH 50



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% Structure definitions for the 
  %% components of the gui
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

typedef struct _menu_containers_t
{

  Widget top_level_row_column;
  Widget main_frame;
  Widget main_form;
  Widget logo;
  Widget label_frame;
  Widget separators[4];

} menu_containers_t;


typedef struct _menu_buttons_t
{

  Widget proj_directory_button;
  Widget global_preferences_button;
  Widget seraImage_button;
  Widget seraModel_button;
  Widget sera3d_button;
  Widget seraDose_button;
  Widget seraPlot_button;
  Widget seraCalc_button;
  Widget seraPlan_button;
  Widget close_apps_button;
  Widget help_button;
  Widget exit_button;

} menu_buttons_t;


typedef struct _preferences_button_t
{
  
  Widget button;
  Widget form;
  char title[64];

} preferences_button_t;

typedef struct _preferences_popup_t
{

  Widget shell;
  Widget form;
  Widget frame;
  Widget frame_label;

  Widget inner_form;

  preferences_button_t buttons[NUM_PREFERENCES];

  int active;

} preferences_popup_t;

typedef struct _close_apps_popup_t
{

  Widget shell;
  Widget list;

} close_apps_popup_t;

typedef struct _advanced_mode_popup_t
{

  Widget shell;
  Widget rc;
  Widget display_label;
  Widget parameters_label;
  Widget display_text;
  Widget parameters_text;

} advanced_mode_popup_t;

typedef struct _advanced_mode_t
{

  Widget toggle;
  char program_to_load[256];
  advanced_mode_popup_t popup;

} advanced_mode_t;

typedef struct _popups_t
{
    
  preferences_popup_t preferences;
  close_apps_popup_t  close_apps;

} popups_t;

typedef struct _labels_t
{

  Widget menu_label;
  Widget helpbar;

} labels_t;

typedef struct _pid_t_ 
{

  int pid;
  char pid_name[MAX_LENGTH_NAME];

} _pid_t; /* pid_t is already defined somewhere */

typedef struct _pids_t
{

  _pid_t elements[MAX_NUMBER_PIDS];
  int pids_recorded;

} pids_t;

typedef struct main_gui_t
{

  XtAppContext app;
  Widget toplevel;

  menu_containers_t containers;
  menu_buttons_t    buttons;
  advanced_mode_t   advanced_mode;
  popups_t          popups;
  labels_t          labels;

  pids_t            pids;

  Position x;
  Position y;
  Dimension width;
  Dimension height;

} main_gui_t; 


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Prototypes for InstallLogo.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
Widget InstallLogo ( Widget parent );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Prototypes for callbacks.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void close_apps_cb            ( Widget w, XtPointer clientData, XtPointer callData );
void exit_main_menu_cb        ( Widget w, XtPointer clientData, XtPointer callData );
void set_project_directory_cb ( Widget w, XtPointer clientData, XtPointer callData );
void execute_programCB        ( Widget w, XtPointer clientData, XtPointer callData );
void execute_program          ( main_gui_t * gui, Widget w, char * program_name );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Prototypes for initialize_top_level.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void initialize_top_level ( main_gui_t * gui, int * argc, char * argv[] );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Prototypes for make_main_menu_widget.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void make_main_menu_widget ( main_gui_t * gui );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Prototypes for create_main_menu.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void create_main_menu ( main_gui_t * gui );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Prototypes for load_program.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int load_program ( Widget top_parent, char ** argv );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Prototypes for pids.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void add_pid               ( main_gui_t * gui, int pid_to_add, char *program_name );
void kill_all_pids         ( main_gui_t * gui );
void kill_pid              ( main_gui_t * gui, XmString pid_to_kill );
void get_pids_and_names    ( main_gui_t * gui, XmString *list );
void update_process_ids_cb ( Widget w, XtPointer clientData, XtPointer callData );  


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Prototypes for popup_references.c 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void widget_entered ( Widget w, XtPointer clientData, XEvent *event, Boolean *flag );
void widget_left    ( Widget w, XtPointer clientData, XEvent *event, Boolean *flag );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Prototypes for preferences.c       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_global_preferences_cb ( Widget w, XtPointer clientData, XtPointer callData );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Prototypes for advanced_mode.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void reveal_advanced_mode_menu ( main_gui_t * gui );

#endif
