#ifndef INCLUDE
#define INCLUDE

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/stat.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <Xm/ArrowB.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MessageB.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/MwmUtil.h>
#include <Xm/ScrollBar.h>
#include <Xm/Xm.h>

#include "memory_tools.h"
#include "connection_tools.h"

/*%%%% Used for check_version %%%%%%%*/
#define SERA_MODEL "seraModel"
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#define MY_CHARSET XmSTRING_DEFAULT_CHARSET

#ifdef DEBUG
#define debug printf
#else
void dummyfcn(char *, ...);
#define debug dummyfcn
#endif

#define AUTOSAVE_REGIONS_NAME "autobak_regions"

#define max(x,y) (x)>(y)?(x):(y)
#define min(x,y) (x)<(y)?(x):(y)
#define MAXCOLORS   256

#define BORDER_MASK 128
#define REGION_MASK 127
#define MAXNUMBODIES 64
#define DEFAULT_NAMES [MAXNUMBODIES]={\
				      "buffer",\
				      "scalp",\
				      "skull",\
				      "brain",\
				      "target",\
				      "tumor",\
				      ""}
#define DEFAULT_NUM_GRAYS 128        /* default number of grays in gamma_map */
#define NUM_RESCOL 12                /* number of reserved color spots set aside
				      * 12 := 1 variable contour color
				      * + 3 (psbly var) bnct_rtpe colors (white, yellow, red)
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
 * 0 to (DEFAULT_MIN_GRAY_INDEX - 1)              0-127:  Standard Colormap
 * DEFAULT_MIN_GRAY_INDEX to MAX_GRAY_INDEX     128-243:  Black -> shades of gray -> white
 * MIN_RESCOL_INDEX to MAX_RESCOL_INDEX         244-255:  Colors set aside and needed by program
 */
#define MAX_RESCOL_INDEX (MAXCOLORS - 1)
#define MIN_RESCOL_INDEX (MAX_RESCOL_INDEX - NUM_RESCOL + 1)
#define MAX_GRAY_INDEX (MIN_RESCOL_INDEX - 1)
#define DEFAULT_MIN_GRAY_INDEX (MAX_GRAY_INDEX - DEFAULT_NUM_GRAYS + 1)
#define DEFAULT_MIN_BODY_COLOR_INDEX (DEFAULT_MIN_GRAY_INDEX - MAXNUMBODIES)
#define THRESHOLD_INDEX (DEFAULT_MIN_BODY_COLOR_INDEX-1)

#define CONTOUR_INDEX      (MAX_GRAY_INDEX + 4)
#define BORDER_COLOR       CONTOUR_INDEX
#define RESERVED_RED       (CONTOUR_INDEX + 1)
#define RESERVED_GREEN     (CONTOUR_INDEX + 2)
#define RESERVED_BLUE      (CONTOUR_INDEX + 3)
#define RESERVED_CYAN      (CONTOUR_INDEX + 4)
#define RESERVED_YELLOW    (CONTOUR_INDEX + 5)
#define RESERVED_MAGENTA   (CONTOUR_INDEX + 6)
#define RESERVED_BLACK     (CONTOUR_INDEX + 7)
#define RESERVED_WHITE     (CONTOUR_INDEX + 8)
/* mwf:  end changes *********/

#define GRAYS_AVAILABLE DEFAULT_NUM_GRAYS
#define MIN_GRAY_INDEX (DEFAULT_MIN_GRAY_INDEX)

#endif
