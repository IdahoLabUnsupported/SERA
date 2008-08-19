/*
 * $Id: button_callbacks.h,v 1.1 1998/01/18 14:23:33 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: button_callbacks.h,v $
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
 * Revision 1.5  1994/12/30  23:07:57  astrakan
 * Changed includes to reflect segregated *.h heirarchy.
 *
 * Revision 1.4  1994/09/28  06:00:53  astrakan
 * added handle_print callback
 *
 * Revision 1.3  1994/07/05  22:04:45  astrakan
 * Minor changes to allow for capping, implementation of drawing list.
 *
 * Revision 1.2  1994/06/30  22:33:09  astrakan
 * Minor changes.
 *
 * Revision 1.1  1994/06/29  16:15:17  astrakan
 * Initial revision
 *
 *
 */


#ifndef BUTTON_CALLBACKS_H
#define BUTTON_CALLBACKS_H


#ifndef MASTER_LIST_DEFS_H
#include "master_list_defs.h"
#endif

void handle_brl_oslo (Widget, ll_start_type *, XmPushButtonCallbackStruct *);
void handle_clear (Widget, XtPointer, XmPushButtonCallbackStruct);
void handle_eval_draw (Widget, ll_start_type *, XmPushButtonCallbackStruct *);
void handle_exit (Widget w, XtPointer, XmPushButtonCallbackStruct);
void handle_fire_uv (Widget w, ll_start_type *master_list, 
		  XmPushButtonCallbackStruct *callback_struct);
void handle_flat_cap (Widget, ll_start_type *, XmPushButtonCallbackStruct *);
void handle_free_subsrfs (Widget, ll_start_type *, XmPushButtonCallbackStruct *);
void handle_peterson_ray (Widget w, ll_start_type *head, 
			  XmPushButtonCallbackStruct *button);
void handle_point_cap (Widget, ll_start_type *, XmPushButtonCallbackStruct *);
void handle_point_eval (Widget w, 
			ll_start_type *master_list_head, 
			XmPushButtonCallbackStruct *callback_struct);
void handle_print (Widget, ll_start_type*, XmPushButtonCallbackStruct*);
void handle_ray (Widget, ll_start_type *, XmPushButtonCallbackStruct *);
void handle_selective_draw (Widget, ll_start_type *, XmPushButtonCallbackStruct *);
void handle_stick_draw (Widget, ll_start_type *, XmAnyCallbackStruct *);
void handle_subdivide (Widget, ll_start_type *, XmPushButtonCallbackStruct *);
void handle_split_cap (Widget, ll_start_type *, XmPushButtonCallbackStruct *);
void handle_test (Widget w, ll_start_type *master_list, 
		  XmPushButtonCallbackStruct *callback_struct);

static int do_if_hit ( register struct application *, struct partition * );
static int do_if_miss ( register struct application*, struct partition* );

#endif
