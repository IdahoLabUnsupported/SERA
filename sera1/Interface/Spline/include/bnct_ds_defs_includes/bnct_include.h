/* Include files ********************************************/

#ifndef BNCT_INCLUDE
#define BNCT_INCLUDE

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>

#include  <X11/Xlib.h>
#include  <X11/Xutil.h>
#include  <X11/Xos.h>
#include  <X11/Intrinsic.h>
#include  <X11/IntrinsicP.h>
#include  <X11/StringDefs.h>
#include  <X11/cursorfont.h>
#include  <X11/Shell.h>
#include <X11/Core.h>
#include <X11/Constraint.h>
#include <X11/Composite.h>

#include  <Xm/Xm.h>

#include <Xm/ArrowB.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/DialogS.h>
#include <Xm/DrawnB.h>
#include <Xm/DrawingA.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/SelectioB.h>
#include <Xm/Separator.h>
#include <Xm/SeparatoG.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>

/* RSB 120595
#include  "misc.h"
#include  "Display.h"
#include  "Inel.h"
#include  "list.h"
*/

/* The bds.h include file without the Xinclude entry follows:*/
#include "BDSdefine.h"
#include "nurb.h"
#include "surface_structure_defs.h"
#include "BDStypedef.h"
#include "BDSproto.h"
#include "image_data.h"
#include "util.h"

/* LLV added 5-27-96 for viewer code */
#include <string.h>
#include <sys/stat.h>
#include <Xm/PanedW.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>

#endif
