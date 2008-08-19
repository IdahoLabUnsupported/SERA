/*#include "bnct.h" --David Helzer */
#include "histo.h"

/*****************************************************************************
 * climb widget tree until we get to the top.  Return the Shell
 */

Widget GetTopShell( Widget w )
{
    while ( w && !XtIsWMShell( w ) )
        w = XtParent( w );
    return ( w );
}

