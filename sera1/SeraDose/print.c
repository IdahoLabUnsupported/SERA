#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/FileSB.h>
#include <X11/Xutil.h>
#include <X11/Shell.h>
#include <Xm/DialogS.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawingA.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/SelectioB.h>
#include <Xm/TextF.h>
#include <Xm/Separator.h>
#include <Xm/ArrowB.h>
#include <Xm/ToggleB.h>

/* local header files */
#include "global.h"
#include "picshell.h"
#include "read_raw.h"
#include "common.h"
#include "color.h"
#include "print_tools.h"

/* 
 * Output image in PS format.
 */

int PrintExtractData(int);


void PrintImageCallback 
(
    Widget                wcall,
    XtPointer             ptype,
    XmAnyCallbackStruct   *c_d
    )
{
    if (PrintExtractData ((int) ptype) < 0)
    {
        error_popup ("Unable to extract image data");
        return;
    }
    return;
} 

int PrintExtractData
(
    int               type
    )
{
    img_arr_type      *img_arr;
    int current_pic;

    /*
     * Get the ximage structure for the thing to be printed.
     */
  
    img_arr = image_matrix . img_arr;

    current_pic = image_matrix . active_picture;
    switch (type)
    {
        case 0:      /* Print selected from image matrix */
            if( image_matrix.num_pics > 0 )
                PT_print(image_matrix.shell, img_arr [current_pic].window);
            else
                return( -1 );
            break;

	
        case 1:      /* Print all from image matrix, Not used anywhere*/
            PT_print(image_matrix.shell, image_matrix.window);
            break;
	 
        case 2:      /* Print drawing area image */
            /*
             * What is displayed depends on which widget is used as the
             * source of the ximage.  shellWindow gives the entire
             * widget.  mainWindowDrawingArea shows only the image.
             */

            PT_print(image_matrix.shell, shellWindowForm);
            break;

        case 3:      /* Print only viewable part of image matrix */
            PT_print(image_matrix.shell,image_matrix . window);
            break;
    }
    return 0;
}
