
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

   file XImage.h: is the Public header file for the xImageWidget.  
 
   classname:     xImageWidgetClass.

   create as:     XtCreate<Managed>Widget(name, xImageWidgetClass, 
                                          parentWidgetname, arguments, 
                                          number_of_arguments); 

* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

#ifndef _XImage_h_
#define _XImage_h_


#undef XtCVisual 
#undef XtNmaxWidth 
#undef XtCMaxWidth 
#undef XtNminWidth 
#undef XtCMinWidth 
#undef XtNmaxHeight 
#undef XtCMaxHeight 
#undef XtNminHeight 
#undef XtCMinHeight 

 
#define XtNimage      "image"
#define XtNxoffset    "xoffset"
#define XtNyoffset    "yoffset"
#define XtNmaxWidth   "maxWidth"
#define XtNminWidth   "minWidth"
#define XtNmaxHeight  "maxHeight"
#define XtNminHeight  "minHeight"

#define XtCImage      "Image"
#define XtCVisual     "Visual"
#define XtCXoffset    "Xoffset"
#define XtCYoffset    "Yoffset"
#define XtCMaxWidth   "MaxWidth"
#define XtCMinWidth   "MinWidth"
#define XtCMaxHeight  "MaxHeight"
#define XtCMinHeight  "MinHeight"


extern WidgetClass xImageWidgetClass;

typedef struct _XImageClassRec *XImageWidgetClass;
typedef struct _XImageRec      *XImageWidget;

#endif
/* End of file XImage.h */ 
