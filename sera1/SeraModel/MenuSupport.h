/********************************************************************
 *         This example code is from the book:
 *
 *           The X Window System: Programming and Applications with Xt
 *           Second OSF/Motif Edition
 *         by
 *           Douglas Young
 *           Prentice Hall, 1994
 *
 *         Copyright 1994 by Prentice Hall
 *         All Rights Reserved
 *
 *  Permission to use, copy, modify, and distribute this software for 
 *  any purpose except publication and without fee is hereby granted, provided 
 *  that the above copyright notice appear in all copies of the software.
 * *****************************************************************************/

/*********************************************************
 * MenuSupport.h: Definitions for a simple menu package.
 *********************************************************/
#ifndef MENUSUPPORT_H
#define MENUSUPPORT_H
#include <Xm/Xm.h>

/*
 * Declare some symbols that can be used to identify
 * the loginal elements of a menu.
 */

typedef enum _menu_type { END,
                          POPUP,  
                          MENUBAR,
                          OPTION,
                          PULLDOWN,
                          PULLDOWN_TEAROFF,
                          RADIOPULLDOWN,
			  RADIOPULLDOWN_TEAROFF,
                          HELPPANE,
                          BUTTON,
                          LABEL,
                          TOGGLE,
                          SEPARATOR } MenuType;

/*
 * Declare a data structure that contains the information
 * required to describe a menu hierarchy.
 */

typedef struct _menu_desc {
    MenuType             type;
    char*                name;     /* name of the button */
    XtCallbackProc       func;     /* Callback to be invoked */
    XtPointer            data;     /* Data for the callback */
    struct _menu_desc    *subMenu;  /* data for submenu of this menu  */
    char*                accelerator;      /* String to represent accelerator */
    char*                acceleratorText;  /* String to represent accelerator text */
} MenuDescription;

/*
 * A function that creates a menu from a MenuDescription structure.
 */

Widget CreateMyMenu ( MenuType         menuType,
                    char            *name,
                    Widget           parent,
                    MenuDescription *desc,
                    XtPointer        defaultClientData );

#endif
