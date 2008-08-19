/*
 * $Id: callbacks.h,v 1.1 1998/01/18 14:23:33 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: callbacks.h,v $
 * Revision 1.1  1998/01/18 14:23:33  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:52  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:34  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.1.1.1  1996/02/12  23:29:12  babcock
 * Imported sources
 *
 * Revision 1.3  1994/12/30  23:07:57  astrakan
 * Changed includes to reflect segregated *.h heirarchy.
 *
 * Revision 1.2  1994/07/05  22:04:45  astrakan
 * Minor changes to allow for capping, implementation of drawing list.
 *
 * Revision 1.1  1994/06/29  16:15:17  astrakan
 * Initial revision
 *
 * Revision 1.2  1994/06/09  22:28:22  astrakan
 * Conditional inclusion of bnct_defs.h
 *
 * Revision 1.4  1994/05/30  19:05:18  astrakan
 * unknown
 *
 * Revision 1.3  1994/05/27  13:41:00  astrakan
 * putting all into single data structure
 *
 * Revision 1.2  1994/05/26  15:25:47  astrakan
 * added headers for button callbacks
 *
 * Revision 1.1  1994/05/26  14:43:32  astrakan
 * Initial revision
 *
 *
 */

#ifndef CALLBACKS_H
#define CALLBACKS_H

#ifndef MASTER_LIST_DEFS_H
#include "master_list_defs.h"
#endif

void exposeCB (Widget, ll_start_type*, XmDrawingAreaCallbackStruct*);
void menuCB (Widget, int, XmAnyCallbackStruct*);



#endif
