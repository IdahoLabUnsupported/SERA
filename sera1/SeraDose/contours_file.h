#include <stdio.h>
#include <Xm/Xm.h>


#define MAX_NUM_FILES 100

void generate_contours_file_callback(Widget w, XtPointer clientData, XtPointer callData);
void ok_input_files_callback(Widget w, XtPointer clientData, XtPointer callData);
void cancel_generate_contours_file_callback(Widget w, XtPointer clientData, XtPointer callData);
