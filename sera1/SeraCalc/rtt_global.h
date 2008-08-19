#ifdef DECLARATION
#define externOrNot /* nil */
#else
#define externOrNot extern
#endif  /* DECLARATION */

#define MAX_NHIST 2000

#include "views.h"

/*****************************************************************************/
/*                  Global variables used for X window stuff                 */
/*****************************************************************************/
externOrNot Display      *display;     /* default display of the machine */
externOrNot Cursor       cursor;       /* the Cursor for xImageWidget windows */


externOrNot XtAppContext rtt_app;
externOrNot Widget  rtt_top;

externOrNot string_type SERA_PATH;
externOrNot string_type SERA_RESOURCES;
externOrNot string_type SERA_BIN;

externOrNot Widget rtt_edit_MenuBar;

/* LLV added 5-27-96 */
externOrNot int CONFIRM;
externOrNot RttEditPopupSt *rtt_file_data;
externOrNot RttRunSt * rtt_run_widgets;

/* MBR added 8-23-99 */
externOrNot viewGui_t * viewGui;

externOrNot char * rtt_opt[MAX_RTT_OPTIONS]; /* see bnct_define.h */
externOrNot Widget rtt_editpu_popup;
externOrNot Widget rtt_excluded_popup;
externOrNot Widget rtt_transbs_popup;
externOrNot Widget rtt_iop_popup;
externOrNot Widget rtt_isotopes_popup;
