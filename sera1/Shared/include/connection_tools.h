/******************************************************************************
 * connection_tools.h                                                         *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Include this file in any software module in which checking the most recent *
 * version is desired.                                                        *
 *                                                                            *
 * Use:  Call CT_check_version from the callback of a button.                 *
 *                                                                            *
 *       The parameters for CT_check_version are:                             *
 *	          Widget w:  A parent widget for the dialog.                  *
 *            char *module:  The string representing the software module.     *
 *                                                                            *
 * NOTE 1: A resource file called ctrc is used to store the values of the     *
 *         host address and port number.  If the file is missing or cannot be *
 *	   openned, the address defaults to esus.cs.montana.edu and the port  *
 *	   number defaults to 32351.                                          *
 *                                                                            *
 * NOTE 2: connection_tools uses keyval_tools.  Therefore, keyval_tools must  *
 *         linked into the software module using connection_tools.            *
 *                                                                            *
 * - Matt Cohen 12/22/98                                                      *
 *****************************************************************************/
#ifndef CONNECTION_TOOLS_H
#define CONNECTION_TOOLS_H

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include "keyval_tools.h"
#include "debug_tools.h"

/* Main structure for the version checker */
typedef struct _main_checker_t 
{
    Widget parent;
    Widget shell;
    Widget main_rowcol;
    Widget info_rowcol;
    Widget sw;
    Widget info_text;
    Widget this_version_label;
    Widget this_version;
    Widget recent_version_label;
    Widget recent_version;

    char   module[256];
    char   version_number[256];
    char   version_info[256];
    char   address[256];
    char   version_string[256];

    int    port_number;

} main_checker_t;

/* Functions for connection_tools.c */
void CT_check_version ( Widget, char * );

#endif  /* CONNECTION_TOOLS_H */
