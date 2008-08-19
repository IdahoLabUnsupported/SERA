/*
 * preferences.c
 *
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 * 
 * David Helzer
 * Created August, 1997
 * Last Modified January, 1998
 */
 

#include "include.h"
#include "global.h"
#include "preferences.h"


#define MAX_LENGTH_STRING 50
/*#define PREFERENCE_PRINTS*/

typedef struct _preferences_type
{
   /* Values Loaded */
   Boolean view_contour_lines;
   Boolean masks_replace_images;
   char dose_component[30];
   int number_contour_levels;
   int contour_level_values[300];
   int contour_level_colors[300];
   int max_contour_level_color;
   int maximum_contour_value_color;
   int default_contour_color;
   Boolean attempt_to_use_scalable_fonts;
   Boolean view_large_image_labels;
   int large_image_labels_size;
   Boolean view_preview_image_labels;
   int preview_image_labels_size;
} preferences_type;   

preferences_type default_preferences;



/*
 * Prototypes
 */
void save_preferences_callback (Widget w, XtPointer callData, XtPointer clientData);
Widget create_contour_lines_labels_preferences_form (Widget parent);
Widget create_defaults_preferences_form (Widget parent);
Widget create_contour_levels_preferences_form (Widget parent);
Widget create_miscellaneous_preferences_form (Widget parent);
void update_defaults_preferences_form (void);
void update_contour_lines_labels_preferences_form (void);
void update_contour_levels_preferences_form (void);
void update_miscellaneous_preferences_form (void);
void save_defaults_preferences (FILE *preferences_file);
void save_contour_lines_labels_preferences (FILE *preferences_file);
void save_contour_levels_preferences (FILE *preferences_file);
void save_miscellaneous_preferences (FILE *preferences_file);
void break_into_key_and_value(char * br, char * s,
                              char ** key, char ** value);
int trim_string(char * s);


/*
 * Globals
 */ 

Dimension button_height;
Widget defaults_button, contour_levels_button;
Widget contour_lines_labels_button, miscellaneous_button;
Widget defaults_form, contour_levels_form;
Widget contour_lines_labels_form, miscellaneous_form;
Widget save_button, cancel_button;

Widget save_view_contour_lines, save_masks_replace_images, save_dose_component;

Widget save_contour_levels, save_default_contour_color;

Widget save_attempt_to_use_scalable_fonts, save_large_image_labels, save_preview_image_labels;
Widget save_large_image_labels_size, save_preview_image_labels_size;





#define PREFERENCES_TITLE "BNCT seraDose Preferences\n\nThe following settings\ncan be saved to the file\nseradose_prefs.rsc\nin the resources directory"



void set_default_preferences (void)
{
   short count;
   
   default_preferences.view_contour_lines = 1;
   default_preferences.masks_replace_images = 0;
   strcpy (default_preferences.dose_component, "Total Dose");
   
   default_preferences.number_contour_levels = 10;
   for (count = 0; count < 10; count ++)
      default_preferences.contour_level_values[count] = count * 10;

   default_preferences.contour_level_colors[0] = RESERVED_RED;
   default_preferences.contour_level_colors[1] = RESERVED_GREEN;
   default_preferences.contour_level_colors[2] = RESERVED_BLUE;
   default_preferences.contour_level_colors[3] = RESERVED_CYAN;
   default_preferences.contour_level_colors[4] = RESERVED_MAGENTA;
   default_preferences.contour_level_colors[5] = RESERVED_YELLOW;
   default_preferences.contour_level_colors[6] = RESERVED_RED;
   default_preferences.contour_level_colors[7] = RESERVED_GREEN;
   default_preferences.contour_level_colors[8] = RESERVED_BLUE;
   default_preferences.contour_level_colors[9] = RESERVED_CYAN;
   default_preferences.max_contour_level_color = RESERVED_MAGENTA;
   
   default_preferences.default_contour_color = RESERVED_YELLOW;
   
   default_preferences.attempt_to_use_scalable_fonts = 1;
   default_preferences.view_large_image_labels = 1;
   default_preferences.large_image_labels_size = 120;
   default_preferences.view_preview_image_labels = 0;
   default_preferences.preview_image_labels_size = 50;
}



/*
 * create_preferences_shell
 *
 * David Helzer 8/13/97
 */
 
Widget create_preferences_shell (Widget parent)
{
   Widget preferences_form;
   Widget preferences_label, select_label;
   XmString message_to_display;


   XmRegisterConverters();

   preferences_shell = XtVaCreatePopupShell ("preferences_shell", 
                                             xmDialogShellWidgetClass, parent,
                                             NULL);

   preferences_form = XtVaCreateWidget("preferences_form",
				       xmFormWidgetClass, preferences_shell,
				       XmNwidth, 280,
				       XmNheight, 400,
				       NULL);
				       
				       
   message_to_display = XmStringCreateLtoR(PREFERENCES_TITLE, XmFONTLIST_DEFAULT_TAG);
   				       
   preferences_label = XtVaCreateManagedWidget ("preferences_label",
                                                xmLabelWidgetClass, preferences_form,
                                                XmNlabelString, message_to_display,
                                                XmNtopAttachment, XmATTACH_FORM,
                                                XmNleftAttachment, XmATTACH_FORM,
                                                XmNrightAttachment, XmATTACH_FORM,
                                                NULL);
                                                
                                                
   message_to_display  = XmStringCreateLtoR ("Defaults", XmFONTLIST_DEFAULT_TAG);
   
   defaults_button = XtVaCreateManagedWidget ("defaults_button",
                                              xmToggleButtonWidgetClass, preferences_form,
                                              XmNlabelString, message_to_display,
                                              XmNtopAttachment, XmATTACH_WIDGET,
                                              XmNtopWidget, preferences_label,
                                              XmNleftAttachment, XmATTACH_FORM,
                                              NULL);
                                              
   XtAddCallback (defaults_button, XmNvalueChangedCallback, preferences_selected_callback, NULL);
                                              
                                              
   message_to_display = XmStringCreateLtoR ("Contour Levels",
                                            XmFONTLIST_DEFAULT_TAG);
                                            
   contour_levels_button = XtVaCreateManagedWidget ("contour_levels_button",
                                                    xmToggleButtonWidgetClass, preferences_form,
                                                    XmNlabelString, message_to_display,
                                                    XmNtopAttachment, XmATTACH_WIDGET,
                                                    XmNtopWidget, preferences_label,
                                                    XmNleftAttachment, XmATTACH_WIDGET,
                                                    XmNleftWidget, defaults_button,
                                                    NULL);
                                                          
   XtAddCallback (contour_levels_button, XmNvalueChangedCallback, 
                  preferences_selected_callback, NULL);

                                                          
                                                          
   message_to_display = XmStringCreateLtoR ("Contour Lines' Labels", 
                                            XmFONTLIST_DEFAULT_TAG);
                                            
   contour_lines_labels_button = XtVaCreateManagedWidget ("contour_lines_labels_button", 
                                                          xmToggleButtonWidgetClass, preferences_form,
                                                          XmNlabelString, message_to_display,
                                                          XmNtopAttachment, XmATTACH_WIDGET,
                                                          XmNtopWidget, defaults_button,
                                                          XmNleftAttachment, XmATTACH_FORM,
                                                          NULL);
                                                          
   XtAddCallback (contour_lines_labels_button, XmNvalueChangedCallback, 
                  preferences_selected_callback, NULL);

                                       

   message_to_display = XmStringCreateLtoR ("Miscellaneous",
                                            XmFONTLIST_DEFAULT_TAG);
                                            
   miscellaneous_button = XtVaCreateManagedWidget ("miscellaneous_button",
                                                   xmToggleButtonWidgetClass, preferences_form,
                                                   XmNlabelString, message_to_display,
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, contour_levels_button,
                                                   XmNleftAttachment, XmATTACH_WIDGET,
                                                   XmNleftWidget, contour_lines_labels_button,
                                                   NULL);
                                                   
   XtAddCallback (miscellaneous_button, XmNvalueChangedCallback, 
                  preferences_selected_callback, NULL);
                  
                 
   /*
    * Get the height of a button, for later use
    */
    
   XtVaGetValues (miscellaneous_button,
                  XmNheight, &button_height,
                  NULL);
                  
                  
   message_to_display = XmStringCreateLtoR ("\nSelect the Options to Save\n\nThe defaults of those not\nselected will be saved.\n",
                                             XmFONTLIST_DEFAULT_TAG);
                                             
   select_label = XtVaCreateManagedWidget ("select_label", 
                                           xmLabelWidgetClass, preferences_form,
                                           XmNlabelString, message_to_display,
                                           XmNleftAttachment, XmATTACH_FORM,
                                           XmNtopAttachment, XmATTACH_WIDGET,
                                           XmNtopWidget, contour_lines_labels_button,
                                           XmNrightAttachment, XmATTACH_FORM,
                                           NULL);



   message_to_display = XmStringCreateLtoR ("Save", XmFONTLIST_DEFAULT_TAG);

   save_button = XtVaCreateManagedWidget ("save_button",
                                          xmPushButtonWidgetClass, preferences_form,
                                          XmNlabelString, message_to_display,
                                          XmNleftAttachment, XmATTACH_FORM,
                                          XmNbottomAttachment, XmATTACH_FORM,
                                          NULL);
                                          
   XtAddCallback (save_button, XmNactivateCallback, save_preferences_callback, NULL);


   message_to_display = XmStringCreateLtoR ("Cancel", XmFONTLIST_DEFAULT_TAG);

   cancel_button = XtVaCreateManagedWidget ("cancel_button", 
                                            xmPushButtonWidgetClass, preferences_form,
                                            XmNlabelString, message_to_display,
                                            XmNrightAttachment, XmATTACH_FORM,
                                            XmNbottomAttachment, XmATTACH_FORM,
                                            NULL);
                                            

   defaults_form = create_defaults_preferences_form (preferences_form);   
   
   XtVaSetValues (defaults_form,
                  XmNtopAttachment, XmATTACH_WIDGET,
                  XmNtopWidget, select_label,
                  XmNleftAttachment, XmATTACH_FORM,
                  XmNrightAttachment, XmATTACH_FORM,
                  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget, save_button,
                  NULL);
   
                     
   contour_levels_form = create_contour_levels_preferences_form (preferences_form);
   
   XtVaSetValues (contour_levels_form,
                  XmNtopAttachment, XmATTACH_WIDGET,
                  XmNtopWidget, select_label,
                  XmNleftAttachment, XmATTACH_FORM,
                  XmNrightAttachment, XmATTACH_FORM,
                  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget, save_button,
                  NULL);
                  
                                                              
   contour_lines_labels_form = create_contour_lines_labels_preferences_form (preferences_form);
   
   XtVaSetValues (contour_lines_labels_form,
                  XmNtopAttachment, XmATTACH_WIDGET,
                  XmNtopWidget, select_label,
                  XmNleftAttachment, XmATTACH_FORM,
                  XmNrightAttachment, XmATTACH_FORM,
                  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget, save_button,
                  NULL);
                  
                  
   miscellaneous_form = create_miscellaneous_preferences_form (preferences_form);
   
   XtVaSetValues (miscellaneous_form,
                  XmNtopAttachment, XmATTACH_WIDGET,
                  XmNtopWidget, select_label,
                  XmNleftAttachment, XmATTACH_FORM,
                  XmNrightAttachment, XmATTACH_FORM,
                  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget, save_button,
                  NULL);            
                            
                                                                        
   return preferences_form;                                      
}



Widget create_defaults_preferences_form (Widget parent)
{
   Widget form;
   XmString message_to_display;
   
   
   form = XtVaCreateWidget ("form",
                            xmFormWidgetClass, parent,
                            NULL);
                            
                            
   message_to_display = XmStringCreateLtoR ("View Contour Lines (not colorwash)",
                                             XmFONTLIST_DEFAULT_TAG);
                                             
   save_view_contour_lines = XtVaCreateManagedWidget ("save_view_contour_lines",
                                                      xmToggleButtonWidgetClass, form,
                                                      XmNlabelString, message_to_display,
                                                      XmNleftAttachment, XmATTACH_FORM,
                                                      XmNtopAttachment, XmATTACH_FORM,
                                                      NULL);
                                                      
                                                      
   message_to_display = XmStringCreateLtoR ("Masks Replace Images", 
                                            XmFONTLIST_DEFAULT_TAG);
                                                      
   save_masks_replace_images = XtVaCreateManagedWidget ("save_masks_replace_images",
                                                        xmToggleButtonWidgetClass, form,
                                                        XmNlabelString, message_to_display,
                                                        XmNleftAttachment, XmATTACH_FORM,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, save_view_contour_lines,
                                                        NULL);
                                                        
                                                        
   message_to_display = XmStringCreateLtoR ("Dose Component",
                                            XmFONTLIST_DEFAULT_TAG);
                                            
   save_dose_component = XtVaCreateManagedWidget ("save_dose_component",
                                                   xmToggleButtonWidgetClass, form,
                                                   XmNlabelString, message_to_display,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, save_masks_replace_images,
                                                   NULL);
                                                   
   XmStringFree (message_to_display);
   return form;
}




Widget create_contour_levels_preferences_form (Widget parent)
{
   Widget form;
   XmString message_to_display;
   
   
   form = XtVaCreateWidget ("form",
                            xmFormWidgetClass, parent,
                            NULL);
                                   
   message_to_display = XmStringCreateLtoR ("Default Contour Color",
                                            XmFONTLIST_DEFAULT_TAG);
                                            
   save_default_contour_color = XtVaCreateManagedWidget ("save_default_contour_color",
                                                         xmToggleButtonWidgetClass, form,
                                                         XmNlabelString, message_to_display,
                                                         XmNleftAttachment, XmATTACH_FORM,
                                                         XmNtopAttachment, XmATTACH_FORM,
                                                         NULL);
                                                         
                                   
   message_to_display = XmStringCreateLtoR ("Contour Levels",
                                            XmFONTLIST_DEFAULT_TAG);
                                            
   save_contour_levels = XtVaCreateManagedWidget ("save_contour_levels",
                                                  xmToggleButtonWidgetClass, form,
                                                  XmNlabelString, message_to_display,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, save_default_contour_color,
                                                  NULL);
                                                       
   XmStringFree (message_to_display);
   return form;
}



Widget create_miscellaneous_preferences_form (Widget parent)
{
   Widget form;
   XmString message_to_display;
   
   
   form = XtVaCreateManagedWidget ("form",
                                   xmFormWidgetClass, parent,
                                   NULL);
   return form;
}



Widget create_contour_lines_labels_preferences_form (Widget parent)
{
   Widget form;
   XmString message_to_display;
   
   
   form = XtVaCreateWidget ("form", 
                            xmFormWidgetClass, parent,
                            NULL);                            
                                   
                       
   message_to_display = XmStringCreateLtoR ("Attempt To Use Scalable Fonts", 
                                            XmFONTLIST_DEFAULT_TAG);
        
   save_attempt_to_use_scalable_fonts = XtVaCreateManagedWidget ("save_attempt_to_use_scalable_fonts", 
                                                                 xmToggleButtonWidgetClass, form,
                                                                 XmNlabelString, message_to_display,
                                                                 XmNleftAttachment, XmATTACH_FORM,
                                                                 XmNtopAttachment, XmATTACH_FORM,
                                                                 NULL);
                                         
                                         
   message_to_display = XmStringCreateLtoR ("View Large Image Labels",
                                            XmFONTLIST_DEFAULT_TAG);
                                            
   save_large_image_labels = XtVaCreateManagedWidget ("save_large_image_labels", 
                                                      xmToggleButtonWidgetClass, form,
                                                      XmNlabelString, message_to_display,
                                                      XmNleftAttachment, XmATTACH_FORM,
                                                      XmNtopAttachment, XmATTACH_WIDGET,
                                                      XmNtopWidget, save_attempt_to_use_scalable_fonts,
                                                      NULL);
                                                      
                                                      
   message_to_display = XmStringCreateLtoR ("Large Image Labels' Size",
                                            XmFONTLIST_DEFAULT_TAG);
                                            
   save_large_image_labels_size = XtVaCreateManagedWidget ("save_large_image_labels_size", 
                                                           xmToggleButtonWidgetClass, form,
                                                           XmNlabelString, message_to_display,
                                                           XmNleftAttachment, XmATTACH_FORM,
                                                           XmNtopAttachment, XmATTACH_WIDGET,
                                                           XmNtopWidget, save_large_image_labels,
                                                           NULL);
                                                      
                                                      
   message_to_display = XmStringCreateLtoR ("View Preview Image Labels",
                                            XmFONTLIST_DEFAULT_TAG);
                                            
   save_preview_image_labels = XtVaCreateManagedWidget ("save_preview_image_labels", 
                                                        xmToggleButtonWidgetClass, form,
                                                        XmNlabelString, message_to_display,
                                                        XmNleftAttachment, XmATTACH_FORM,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, save_large_image_labels_size,
                                                        NULL);
                                                        
                                                        
   message_to_display = XmStringCreateLtoR ("Preview Image Labels' Size", 
                                            XmFONTLIST_DEFAULT_TAG);
                                            
   save_preview_image_labels_size = XtVaCreateManagedWidget ("save_preview_image_labels_size", 
                                                             xmToggleButtonWidgetClass, form,
                                                             XmNlabelString, message_to_display,
                                                             XmNleftAttachment, XmATTACH_FORM,
                                                             XmNtopAttachment, XmATTACH_WIDGET,
                                                             XmNtopWidget, save_preview_image_labels,
                                                             NULL);

   XmStringFree (message_to_display);                                                        
   return form;
}



void preferences_callback (Widget w, XtPointer callData, XtPointer clientData)
{
   extern Widget preferences_form;
   XtManageChild (preferences_form);
}



void preferences_selected_callback (Widget w, XtPointer callData, XtPointer clientData)
{
   /*
    * Unmanage all preference forms
    */
    
   XtUnmanageChild (defaults_form);
   XtUnmanageChild (contour_levels_form);
   XtUnmanageChild (contour_lines_labels_form);
   XtUnmanageChild (miscellaneous_form);


   /* Set all of the toggle buttons to unset and then reset that specified */
   XtVaSetValues (defaults_button, 
                  XmNset, FALSE,
                  NULL);
                                    
   XtVaSetValues (contour_levels_button,
                  XmNset, FALSE,
                  NULL);
                  
   XtVaSetValues (contour_lines_labels_button,
                  XmNset, FALSE,
                  NULL);
                  
   XtVaSetValues (miscellaneous_button,
                  XmNset, FALSE,
                  NULL);
      
   XtVaSetValues (w,
                  XmNset, TRUE,
                  NULL);
                  
       
   if (strcmp (XtName (w), "contour_lines_labels_button") == 0) {
      update_contour_lines_labels_preferences_form();
      XtManageChild (contour_lines_labels_form);
   }
   else if (strcmp (XtName (w), "contour_levels_button") == 0) {
      update_contour_levels_preferences_form();
      XtManageChild (contour_levels_form);
   }
   else if (strcmp (XtName (w), "defaults_button") == 0) {
      update_defaults_preferences_form();
      XtManageChild (defaults_form);
   }
   else {
      update_miscellaneous_preferences_form();
      XtManageChild (miscellaneous_form);
   }
}



void update_defaults_preferences_form (void)
{
   Boolean masks_replace_images, view_contour_lines;
   XmString message_to_display;
   Widget view_contour_lines_label, masks_replace_images_label;
   Widget dose_component_label;
   
   
   XtVaGetValues (view_contour_lines_button, 
                  XmNset, &view_contour_lines,
                  NULL);
                  
   if (view_contour_lines)
      message_to_display = XmStringCreateLtoR ("True", XmFONTLIST_DEFAULT_TAG);
   else
      message_to_display = XmStringCreateLtoR ("False", XmFONTLIST_DEFAULT_TAG);
      
   view_contour_lines_label = XtVaCreateManagedWidget ("view_contour_lines_label",
                                                       xmLabelWidgetClass, defaults_form,
                                                       XmNlabelString, message_to_display,
                                                       XmNheight, button_height,
                                                       XmNleftAttachment, XmATTACH_WIDGET,
                                                       XmNleftWidget, save_view_contour_lines,
                                                       XmNtopAttachment, XmATTACH_FORM,
                                                       NULL);
                                                       
                                                       
   XtVaGetValues (masksReplaceImagesButton,
                  XmNset, &masks_replace_images,
                  NULL);
                  
   if (masks_replace_images)
      message_to_display = XmStringCreateLtoR ("True", XmFONTLIST_DEFAULT_TAG);
   else
      message_to_display = XmStringCreateLtoR ("False", XmFONTLIST_DEFAULT_TAG);
      
   masks_replace_images_label = XtVaCreateManagedWidget 
   (
      "masks_replace_images_label",
       xmLabelWidgetClass, defaults_form,
       XmNlabelString, message_to_display,
       XmNheight, button_height,
       XmNleftAttachment, XmATTACH_WIDGET,
       XmNleftWidget, save_masks_replace_images,
       XmNtopAttachment, XmATTACH_WIDGET,
       XmNtopWidget, view_contour_lines_label,
       NULL
   );
                                                         
                                                         
   if (doseFlag == 0)
      message_to_display = XmStringCreateLtoR ("Boron Dose",
                                               XmFONTLIST_DEFAULT_TAG);
   else if (doseFlag == 1)
      message_to_display = XmStringCreateLtoR ("Gamma Dose",
                                               XmFONTLIST_DEFAULT_TAG);
   else if (doseFlag == 2)
      message_to_display = XmStringCreateLtoR ("Nitrogen Dose",
                                               XmFONTLIST_DEFAULT_TAG);
   else if (doseFlag == 3)
      message_to_display = XmStringCreateLtoR ("Fast Dose",
                                               XmFONTLIST_DEFAULT_TAG);
   else if (doseFlag == 4)
      message_to_display = XmStringCreateLtoR ("Group 1 Fluence",
                                               XmFONTLIST_DEFAULT_TAG);
   else if (doseFlag == 5)
      message_to_display = XmStringCreateLtoR ("Group 2 Fluence",
                                               XmFONTLIST_DEFAULT_TAG);
   else if (doseFlag == 6)
      message_to_display = XmStringCreateLtoR ("Thermal Fluence",
                                               XmFONTLIST_DEFAULT_TAG);
   else if (doseFlag == 7)
      message_to_display = XmStringCreateLtoR ("Other Dose",
                                               XmFONTLIST_DEFAULT_TAG);
   else
      message_to_display = XmStringCreateLtoR ("Total Dose",
                                               XmFONTLIST_DEFAULT_TAG);
                                               
   dose_component_label = XtVaCreateManagedWidget 
   (
      "dose_component_label",
      xmLabelWidgetClass, defaults_form,
      XmNlabelString, message_to_display,
      XmNheight, button_height,
      XmNleftAttachment, XmATTACH_WIDGET,
      XmNleftWidget, save_dose_component,
      XmNtopAttachment, XmATTACH_WIDGET,
      XmNtopWidget, masks_replace_images_label,
      NULL
   );
   
   XmStringFree (message_to_display);                  
}



void update_contour_levels_preferences_form (void)
{
   XmString message_to_display;
   Widget contour_level_color_label;
   Widget contour_colors_scroll;
   Widget contour_colors_label;
   char single_contour_color[20];
   char contour_colors[500];
   short count;
   
   
   if (default_contour_color == RESERVED_RED)
      message_to_display = XmStringCreateLtoR ("Red", 
                                               XmFONTLIST_DEFAULT_TAG);
   else if (default_contour_color == RESERVED_GREEN)
      message_to_display = XmStringCreateLtoR ("Green", 
                                               XmFONTLIST_DEFAULT_TAG);
   else if (default_contour_color == RESERVED_BLUE)
      message_to_display = XmStringCreateLtoR ("Blue", 
                                               XmFONTLIST_DEFAULT_TAG);
   else if (default_contour_color == RESERVED_CYAN)
      message_to_display = XmStringCreateLtoR ("Cyan", 
                                               XmFONTLIST_DEFAULT_TAG);
   else if (default_contour_color == RESERVED_MAGENTA)
      message_to_display = XmStringCreateLtoR ("Magenta", 
                                               XmFONTLIST_DEFAULT_TAG);
   else if (default_contour_color == RESERVED_YELLOW)
      message_to_display = XmStringCreateLtoR ("Yellow", 
                                               XmFONTLIST_DEFAULT_TAG);
   else if (default_contour_color == RESERVED_BLACK)
      message_to_display = XmStringCreateLtoR ("Black", 
                                               XmFONTLIST_DEFAULT_TAG);
   else
      message_to_display = XmStringCreateLtoR ("White", 
                                               XmFONTLIST_DEFAULT_TAG);

   contour_level_color_label = XtVaCreateManagedWidget ("contour_level_color_label",
                                                        xmLabelWidgetClass, contour_levels_form,
                                                        XmNlabelString, message_to_display,
                                                        XmNheight, button_height,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, save_default_contour_color,
                                                        XmNtopAttachment, XmATTACH_FORM,
                                                        NULL);

   contour_colors_scroll = XtVaCreateManagedWidget ("contour_colors_scroll",
                                                    xmScrolledWindowWidgetClass, contour_levels_form,
                                                    XmNscrollingPolicy, XmAUTOMATIC,
                                                    XmNscrollingPolicy, XmSTATIC,
                                                    XmNheight, button_height * 3,
                                                    XmNleftAttachment, XmATTACH_WIDGET,
                                                    XmNleftWidget, save_contour_levels,
                                                    XmNtopAttachment, XmATTACH_WIDGET,
                                                    XmNtopWidget, contour_level_color_label,
                                                    XmNrightAttachment, XmATTACH_FORM,
                                                    XmNbottomAttachment, XmATTACH_FORM,
                                                    NULL);
   
   
   for (count = 0; count < contour_levels.number_levels; count ++) {
      sprintf (single_contour_color, "%d - ", contour_levels.rlevel[count]);
      
      if (contour_levels.colors[count] == RESERVED_RED)
         strcat (single_contour_color, "Red\n");
      else if (contour_levels.colors[count] == RESERVED_GREEN)
         strcat (single_contour_color, "Green\n");
      else if (contour_levels.colors[count] == RESERVED_BLUE)
         strcat (single_contour_color, "Blue\n");
      else if (contour_levels.colors[count] == RESERVED_CYAN)
         strcat (single_contour_color, "Cyan\n");
      else if (contour_levels.colors[count] == RESERVED_MAGENTA)
         strcat (single_contour_color, "Magenta\n");
      else if (contour_levels.colors[count] == RESERVED_YELLOW)
         strcat (single_contour_color, "Yellow\n");
      else if (contour_levels.colors[count] == RESERVED_BLACK)
         strcat (single_contour_color, "Black\n");
      else
         strcat (single_contour_color, "White\n");
   
      if (count == 0)
         strcpy (contour_colors, single_contour_color);
      else
         strcat (contour_colors, single_contour_color);
   }
   
   strcpy (single_contour_color, "Max - ");
   
   if (max_contour_value_color == RESERVED_RED)
      strcat (single_contour_color, "Red\n");
   else if (max_contour_value_color == RESERVED_GREEN)
      strcat (single_contour_color, "Green\n");
   else if (max_contour_value_color == RESERVED_BLUE)
      strcat (single_contour_color, "Blue\n");
   else if (max_contour_value_color == RESERVED_CYAN)
      strcat (single_contour_color, "Cyan\n");
   else if (max_contour_value_color == RESERVED_MAGENTA)
      strcat (single_contour_color, "Magenta\n");
   else if (max_contour_value_color == RESERVED_YELLOW)
      strcat (single_contour_color, "Yellow\n");
   else if (max_contour_value_color == RESERVED_BLACK)
      strcat (single_contour_color, "Black\n");
   else
      strcat (single_contour_color, "White\n");
      
   strcat (contour_colors, single_contour_color);
   
   
   message_to_display = XmStringCreateLtoR (contour_colors, XmFONTLIST_DEFAULT_TAG);
   contour_colors_label = XtVaCreateManagedWidget ("contour_colors_label",
                                                   xmLabelWidgetClass, contour_colors_scroll,
                                                   XmNlabelString, message_to_display,
                                                   NULL);
                                                                                                        
   XmStringFree (message_to_display);
}



void update_contour_lines_labels_preferences_form (void)
{ 
   Boolean view_large_image_labels, view_preview_image_labels, attempt_to_use_scalable_fonts;
   char label_size[5];
   Widget use_scalable_fonts_label, view_large_labels_label, view_preview_labels_label;
   Widget large_labels_size_label, preview_labels_size_label;
   XmString message_to_display;


   XtVaGetValues (use_scalable_fonts_toggle,
                  XmNset, &attempt_to_use_scalable_fonts,
                  NULL);
                  
   if (attempt_to_use_scalable_fonts)
      message_to_display = XmStringCreateLtoR ("True", XmFONTLIST_DEFAULT_TAG);
   else
      message_to_display = XmStringCreateLtoR ("False", XmFONTLIST_DEFAULT_TAG);
                  
   use_scalable_fonts_label = XtVaCreateManagedWidget ("font_label", 
                                                       xmLabelWidgetClass, contour_lines_labels_form,
                                                       XmNlabelString, message_to_display, 
                                                       XmNheight, button_height,
                                                       XmNleftAttachment, XmATTACH_WIDGET,
                                                       XmNleftWidget, save_attempt_to_use_scalable_fonts,
                                                       XmNtopAttachment, XmATTACH_FORM,
                                                       NULL);
                                                       

   XtVaGetValues (view_large_image_labels_toggle,
                  XmNset, &view_large_image_labels,
                  NULL);
  
  
   if (view_large_image_labels)
      message_to_display = XmStringCreateLtoR ("True", XmFONTLIST_DEFAULT_TAG);
   else 
      message_to_display = XmStringCreateLtoR ("False", XmFONTLIST_DEFAULT_TAG);
      
   view_large_labels_label = XtVaCreateManagedWidget ("view_large_labels_label", 
                                                      xmLabelWidgetClass, contour_lines_labels_form,
                                                      XmNlabelString, message_to_display,
                                                      XmNheight, button_height,
                                                      XmNleftAttachment, XmATTACH_WIDGET,
                                                      XmNleftWidget, save_large_image_labels,                                                      XmNtopAttachment, XmATTACH_WIDGET,
                                                      XmNtopWidget, use_scalable_fonts_label,
                                                      NULL);
                                                         
                                                         
   sprintf (label_size, "%d", large_image_label_size);                                                   
   message_to_display = XmStringCreateLtoR (label_size, XmFONTLIST_DEFAULT_TAG);
    
   large_labels_size_label = XtVaCreateManagedWidget ("large_labels_size_label",
                                                      xmLabelWidgetClass, contour_lines_labels_form,
                                                      XmNlabelString, message_to_display,
                                                      XmNheight, button_height,
                                                      XmNleftAttachment, XmATTACH_WIDGET,
                                                      XmNleftWidget, save_large_image_labels_size,
                                                      XmNtopAttachment, XmATTACH_WIDGET,
                                                      XmNtopWidget, view_large_labels_label,
                                                      NULL);             
                                                                                                                  
                                                      
   XtVaGetValues (view_preview_image_labels_toggle,
                  XmNset, &view_preview_image_labels,
                  NULL);  
  
   if (view_preview_image_labels)
      message_to_display = XmStringCreateLtoR ("True", XmFONTLIST_DEFAULT_TAG);
   else 
      message_to_display = XmStringCreateLtoR ("False", XmFONTLIST_DEFAULT_TAG);
      
   view_preview_labels_label = XtVaCreateManagedWidget ("view_preview_labels_label", 
                                                        xmLabelWidgetClass, contour_lines_labels_form,
                                                        XmNlabelString, message_to_display,
                                                        XmNheight, button_height,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, save_preview_image_labels,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, large_labels_size_label,
                                                        NULL);


   sprintf (label_size, "%d", preview_image_label_size);                                                   
   message_to_display = XmStringCreateLtoR (label_size, XmFONTLIST_DEFAULT_TAG);
    
   preview_labels_size_label = XtVaCreateManagedWidget ("preview_labels_size_label",
                                                        xmLabelWidgetClass, contour_lines_labels_form,
                                                        XmNlabelString, message_to_display,
                                                        XmNheight, button_height,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, save_preview_image_labels_size,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, view_preview_labels_label,
                                                        NULL);
}



void update_miscellaneous_preferences_form (void)
{
}



void save_preferences_callback (Widget w, XtPointer callData, XtPointer clientData)
{
   FILE *preferences_file;
   char file_to_open[256];
   Boolean set;
   
   strcpy (file_to_open, getenv ("SERA_RESOURCES"));
   strcat (file_to_open, DoseDisplayResourceDir);
   strcat (file_to_open, DoseDisplayResourceFile);
   
   preferences_file = fopen (file_to_open, "w+");
   
   if (!preferences_file)
   {
      printf ("Error creating/opening %s.  Aborting.\n", file_to_open);
      exit (0);
   }
   
   save_defaults_preferences (preferences_file);
   save_contour_levels_preferences (preferences_file);
   save_contour_lines_labels_preferences (preferences_file);
   save_miscellaneous_preferences (preferences_file);
         
   fclose (preferences_file);
}



void save_defaults_preferences (FILE *preferences_file)
{
   Boolean attribute_button_set;
   Boolean save_button_set;
   
   
   XtVaGetValues (save_view_contour_lines,
                  XmNset, &save_button_set,
                  NULL);
   
   if (save_button_set) 
   {
      XtVaGetValues (view_contour_lines_button,
                     XmNset, &attribute_button_set, 
                     NULL);
      if (attribute_button_set)
         fprintf (preferences_file, "view contour lines:True\n");
      else
         fprintf (preferences_file, "view contour lines:False\n");
   }
   else
   {
      if (default_preferences.view_contour_lines)
         fprintf (preferences_file, "view contour lines:True\n");
      else 
         fprintf (preferences_file, "view contour lines:False\n");\
   }
  
  
  XtVaGetValues (save_masks_replace_images,
                 XmNset, &save_button_set,
                 NULL);
                 
  if (save_button_set)
  {
     XtVaGetValues (masksReplaceImagesButton,
                    XmNset, &attribute_button_set,
                    NULL);
     if (attribute_button_set)
        fprintf (preferences_file, "masks replace images:True\n");
     else
        fprintf (preferences_file, "masks replace images:False\n");
  }
  else 
  {
     if (default_preferences.masks_replace_images)
        fprintf (preferences_file, "masks replace images:True\n");
     else
        fprintf (preferences_file, "masks replace images:False\n");
  }
  
  
  XtVaGetValues (save_dose_component,
                 XmNset, &save_button_set, 
                 NULL);
                 
  if (save_button_set)
  {
     if (doseFlag == 0)
        fprintf (preferences_file, "dose component:Boron Dose\n");
     else if (doseFlag == 1)
        fprintf (preferences_file, "dose component:Gamma Dose\n");
     else if (doseFlag == 2)
        fprintf (preferences_file, "dose component:Nitrogen Dose\n");
     else if (doseFlag == 3)
        fprintf (preferences_file, "dose component:Fast Dose\n");
     else if (doseFlag == 4)
        fprintf (preferences_file, "dose component:Group 1 Fluence\n");
     else if (doseFlag == 5)
        fprintf (preferences_file, "dose component:Group 2 Fluence\n");
     else if (doseFlag == 6)
        fprintf (preferences_file, "dose component:Thermal Fluence\n");
     else if (doseFlag == 7)
        fprintf (preferences_file, "dose component:Other Dose\n");
     else 
        fprintf (preferences_file, "dose component:Total Dose\n");
  }
  else
  {
     if (strcmp (default_preferences.dose_component, "Boron Dose") == 0)
        fprintf (preferences_file, "dose component:Boron Dose\n");
     else if (strcmp (default_preferences.dose_component, "Gamma Dose") == 0)
        fprintf (preferences_file, "dose component:Gamma Dose\n");
     else if (strcmp (default_preferences.dose_component, "Nitrogen Dose") == 0)
        fprintf (preferences_file, "dose component:Nitrogen Dose\n");
     else if (strcmp (default_preferences.dose_component, "Fast Dose") == 0)
        fprintf (preferences_file, "dose component:Fast Dose\n");
     else if (strcmp (default_preferences.dose_component, "Group 1 Fluence") == 0)
        fprintf (preferences_file, "dose component:Group 1 Fluence\n");
     else if (strcmp (default_preferences.dose_component, "Group 2 Fluence") == 0)
        fprintf (preferences_file, "dose component:Group 2 Fluence\n");
     else if (strcmp (default_preferences.dose_component, "Thermal Fluence") == 0)
        fprintf (preferences_file, "dose component:Thermal Fluence\n");
     else if (strcmp (default_preferences.dose_component, "Other Dose") == 0)
        fprintf (preferences_file, "dose component:Other Dose\n");
     else
        fprintf (preferences_file, "dose component:Total Dose\n");
  }
}



void save_contour_lines_labels_preferences (FILE *preferences_file)
{
   Boolean attribute_button_set;
   Boolean save_button_set;
   char font_line_to_save[100];
   
   
   XtVaGetValues (save_attempt_to_use_scalable_fonts,
                  XmNset, &save_button_set,
                  NULL);
                 
   if (save_button_set)
   {
      XtVaGetValues (use_scalable_fonts_toggle,
                     XmNset, &attribute_button_set,
                     NULL);
                     
      if (attribute_button_set)
         fprintf (preferences_file, "attempt to use scalable fonts:True\n");
      else
         fprintf (preferences_file, "attempt to use scalable fonts:False\n");
   }
   else
   {
      if (default_preferences.attempt_to_use_scalable_fonts)
         fprintf (preferences_file, "attempt to use scalable fonts:True\n");
      else
         fprintf (preferences_file, "attempt to use scalable fonts:False\n");
   }
   
   
   XtVaGetValues (save_large_image_labels,
                  XmNset, &save_button_set,
                  NULL);
                  
   if (save_button_set)
   {
      XtVaGetValues (view_large_image_labels_toggle,
                     XmNset, &attribute_button_set,
                     NULL);
                     
      if (attribute_button_set)
         fprintf (preferences_file, "view large image labels:True\n");
      else 
         fprintf (preferences_file, "view large image labels:False\n");
   }
   else
   {
      if (default_preferences.view_large_image_labels)
         fprintf (preferences_file, "view large image labels:True\n");
      else
         fprintf (preferences_file, "view large image labels:False\n");
   }
   
   
   XtVaGetValues (save_large_image_labels_size,
                  XmNset, &save_button_set,
                  NULL);
                  
   if (save_button_set)
      fprintf (preferences_file, "large image labels size:%d\n", large_image_label_size);
   else
      fprintf (preferences_file, "large image labels size:%d\n", 
               default_preferences.large_image_labels_size);
      
   
   
   XtVaGetValues (save_preview_image_labels,
                  XmNset, &save_button_set,
                  NULL);
                  
   if (save_button_set)
   {
      XtVaGetValues (view_preview_image_labels_toggle,
                     XmNset, &attribute_button_set,
                     NULL);
                     
      if (attribute_button_set)
         fprintf (preferences_file, "view preview image labels:True\n");
      else
         fprintf (preferences_file, "view preview image labels:False\n");
   }
   else
   {
      if (default_preferences.view_preview_image_labels)
         fprintf (preferences_file, "view preview image labels:True\n");
      else
         fprintf (preferences_file, "view preview image labels:False\n");
   }


   XtVaGetValues (save_preview_image_labels_size,
                  XmNset, &save_button_set,
                  NULL);
                  
   if (save_button_set)
      fprintf (preferences_file, "preview image labels size:%d\n", preview_image_label_size);
   else
      fprintf (preferences_file, "preview image labels size:%d\n", 
               default_preferences.preview_image_labels_size);

}



void save_contour_levels_preferences (FILE *preferences_file)
{
   Boolean attribute_button_set;
   Boolean save_button_set;
   short count;


   XtVaGetValues (save_default_contour_color, 
                  XmNset, &save_button_set,
                  NULL);
                  
   if (save_button_set) 
   {
      if (default_contour_color == RESERVED_RED)
         fprintf (preferences_file, "default contour color:Red\n");
      else if (default_contour_color == RESERVED_GREEN)
         fprintf (preferences_file, "default contour color:Green\n");
      else if (default_contour_color == RESERVED_BLUE)
         fprintf (preferences_file, "default contour color:Blue\n");
      else if (default_contour_color == RESERVED_CYAN)
         fprintf (preferences_file, "default contour color:Cyan\n");
      else if (default_contour_color == RESERVED_MAGENTA)
         fprintf (preferences_file, "default contour color:Magenta\n");
      else if (default_contour_color == RESERVED_YELLOW)
         fprintf (preferences_file, "default contour color:Yellow\n");
      else if (default_contour_color == RESERVED_BLACK)
         fprintf (preferences_file, "default contour color:Black\n");
      else
         fprintf (preferences_file, "default contour color:White\n");         
   }
   else
   {
      if (default_preferences.default_contour_color == RESERVED_RED)
         fprintf (preferences_file, "default contour color:Red\n");
      else if (default_preferences.default_contour_color == RESERVED_GREEN)
         fprintf (preferences_file, "default contour color:Green\n");
      else if (default_preferences.default_contour_color == RESERVED_BLUE)
         fprintf (preferences_file, "default contour color:Blue\n");
      else if (default_preferences.default_contour_color == RESERVED_CYAN)
         fprintf (preferences_file, "default contour color:Cyan\n");
      else if (default_preferences.default_contour_color == RESERVED_MAGENTA)
         fprintf (preferences_file, "default contour color:Magenta\n");
      else if (default_preferences.default_contour_color == RESERVED_YELLOW)
         fprintf (preferences_file, "default contour color:Yellow\n");
      else if (default_preferences.default_contour_color == RESERVED_BLACK)
         fprintf (preferences_file, "default contour color:Black\n");
      else
         fprintf (preferences_file, "default contour color:White\n");            
   }
   
   
   XtVaGetValues (save_contour_levels,
                  XmNset, &save_button_set,
                  NULL);
                  
   if (save_button_set) 
   {
      fprintf (preferences_file, "contour levels:\n");
      for (count = 0; count < contour_levels.number_levels; count ++) 
      {
         fprintf (preferences_file, "%d:", contour_levels.rlevel[count]);
         
         if (contour_levels.colors[count] == RESERVED_RED)
            fprintf (preferences_file, "Red\n");
         else if (contour_levels.colors[count] == RESERVED_GREEN)
            fprintf (preferences_file, "Green\n");
         else if (contour_levels.colors[count] == RESERVED_BLUE)
            fprintf (preferences_file, "Blue\n");
         else if (contour_levels.colors[count] == RESERVED_CYAN)
            fprintf (preferences_file, "Cyan\n");
         else if (contour_levels.colors[count] == RESERVED_MAGENTA)
            fprintf (preferences_file, "Magenta\n");
         else if (contour_levels.colors[count] == RESERVED_YELLOW)
            fprintf (preferences_file, "Yellow\n");
         else if (contour_levels.colors[count] == RESERVED_BLACK)
            fprintf (preferences_file, "Black\n");
         else 
            fprintf (preferences_file, "White\n");
      }
      if (max_contour_value_color == RESERVED_RED)
         fprintf (preferences_file, "Max:Red\n");
      else if (max_contour_value_color == RESERVED_GREEN)
         fprintf (preferences_file, "Max:Green\n");
      else if (max_contour_value_color == RESERVED_BLUE)
         fprintf (preferences_file, "Max:Blue\n");
      else if (max_contour_value_color == RESERVED_CYAN)
         fprintf (preferences_file, "MaxCyan\n");
      else if (max_contour_value_color == RESERVED_MAGENTA)
         fprintf (preferences_file, "Max:Magenta\n");
      else if (max_contour_value_color == RESERVED_YELLOW)
         fprintf (preferences_file, "Max:Yellow\n");
      else if (max_contour_value_color == RESERVED_BLACK)
         fprintf (preferences_file, "Max:Black\n");
      else
         fprintf (preferences_file, "Max:White\n");
   }
   else
   {
      for (count = 0; count < default_preferences.number_contour_levels; count ++)
      {
         fprintf (preferences_file, "%d:", default_preferences.contour_level_values[count]);
         
         if (default_preferences.contour_level_colors[count] == RESERVED_RED)
            fprintf (preferences_file, "Red\n");
         else if (default_preferences.contour_level_colors[count] == RESERVED_GREEN)
            fprintf (preferences_file, "Green\n");
         else if (default_preferences.contour_level_colors[count] == RESERVED_BLUE)
            fprintf (preferences_file, "Blue\n");
         else if (default_preferences.contour_level_colors[count] == RESERVED_CYAN)
            fprintf (preferences_file, "Cyan\n");
         else if (default_preferences.contour_level_colors[count] == RESERVED_MAGENTA)
            fprintf (preferences_file, "Magenta\n");
         else if (default_preferences.contour_level_colors[count] == RESERVED_YELLOW)
            fprintf (preferences_file, "Yellow\n");
         else if (default_preferences.contour_level_colors[count] == RESERVED_BLACK)
            fprintf (preferences_file, "Black\n");
         else 
            fprintf (preferences_file, "White\n");
      }
      if (default_preferences.max_contour_level_color == RESERVED_RED)
         fprintf (preferences_file, "Max:Red\n");
      else if (default_preferences.max_contour_level_color == RESERVED_GREEN)
         fprintf (preferences_file, "Max:Green\n");
      else if (default_preferences.max_contour_level_color == RESERVED_BLUE)
         fprintf (preferences_file, "Max:Blue\n");
      else if (default_preferences.max_contour_level_color == RESERVED_CYAN)
         fprintf (preferences_file, "MaxCyan\n");
      else if (default_preferences.max_contour_level_color == RESERVED_MAGENTA)
         fprintf (preferences_file, "Max:Magenta\n");
      else if (default_preferences.max_contour_level_color == RESERVED_YELLOW)
         fprintf (preferences_file, "Max:Yellow\n");
      else if (default_preferences.max_contour_level_color == RESERVED_BLACK)
         fprintf (preferences_file, "Max:Black\n");
      else
         fprintf (preferences_file, "Max:White\n");
   }
}



void save_miscellaneous_preferences (FILE *preferences_file)
{
}




void load_preferences (void)
{
   FILE *in;
   char file_to_open[256];
   int  contour_level_value;
   char contour_level[50];
   int  color_number;
   char buffer[100],*test;
   char *key,*value;
  
   strcpy (file_to_open, getenv ("SERA_RESOURCES"));
   strcat (file_to_open, DoseDisplayResourceDir);
   strcat (file_to_open, DoseDisplayResourceFile);
   
   in = fopen (file_to_open, "r");
   
   if (!in){
     printf ("Error opening %s.  Aborting.\n", file_to_open);
     exit (0);
   }

   
   fgets(buffer,100,in);
   trim_string(buffer);
   break_into_key_and_value(":",buffer,&key,&value);
#ifdef PREFERENCE_PRINTS
   printf("read in : %s and %s\n",key,value);
#endif

   while (strlen(buffer) > 1){

#ifdef PREFERENCE_PRINTS
     printf("the strlen of buffer is : %d\n",strlen(buffer));
     printf("In the While read in : %s and %s\n",key,value);
#endif

       /*
	* Determine which preference is to be set and act accordingly
	*/       
       if (strcmp (key, "view contour lines") == 0){
	 if (strcmp (value, "False") == 0){
	   XtVaSetValues (view_contour_lines_button,XmNset, FALSE,NULL); 
	   XtVaSetValues (view_contour_colorwash_button,XmNset, TRUE,NULL);
	   XtVaSetValues (masksReplaceImagesButton,XmNset, FALSE,NULL);
	 }
	 else if (strcmp (value, "True") != 0){
	   printf ("Error in resources file!  Aborting program!\n");
	   exit (0); 
	 }
       }else if (strcmp (key, "masks replace images") == 0){
	 if (strcmp (value, "True") == 0){
	   XtVaSetValues (masksReplaceImagesButton,XmNset, TRUE,NULL);
	   XtVaSetValues (view_contour_lines_button,XmNset, TRUE,NULL);
	   XtVaSetValues (view_contour_colorwash_button,XmNset, FALSE,NULL);
	 }
	 else if (strcmp (value, "False") != 0){
	   printf ("Error in resources file!  Aborting program!\n");
	   exit (0);
	 }
       }else if (strcmp (key, "dose component") == 0){
	 if (strcmp (value, "Boron Dose") == 0) doseFlag = 0;
	 else if (strcmp (value, "Gamma Dose") == 0) doseFlag = 1;
	 else if (strcmp (value, "Nitrogen Dose") == 0) doseFlag = 2;
	 else if (strcmp (value, "Fast Dose") == 0) doseFlag = 3;
	 else if (strcmp (value, "Group 1 Fluence") == 0) doseFlag = 4;
	 else if (strcmp (value, "Group 2 Fluence") == 0) doseFlag = 5;
	 else if (strcmp (value, "Thermal Fluence") == 0) doseFlag = 6;
	 else if (strcmp (value, "Other Dose") == 0) doseFlag = 7;
	 else if (strcmp (value, "Total Dose") == 0) doseFlag = 8;
	 else{
	   printf ("Error in resources file!  Aborting program!\n");
	   exit (0);
	 }
       }else if (strcmp (key, "default contour color") == 0){
	 if (strcmp (value, "Red") == 0)
	   default_contour_color = RESERVED_RED;
	 else if (strcmp (value, "Blue") == 0)
	   default_contour_color = RESERVED_BLUE;
	 else if (strcmp (value, "Green") == 0)
	   default_contour_color = RESERVED_GREEN;
	 else if (strcmp (value, "Cyan") == 0)
	   default_contour_color = RESERVED_CYAN;
	 else if (strcmp (value, "Magenta") == 0)
	   default_contour_color = RESERVED_MAGENTA;
	 else if (strcmp (value, "Yellow") == 0)
	   default_contour_color = RESERVED_YELLOW;
	 else if (strcmp (value, "Black") == 0)
	   default_contour_color = RESERVED_BLACK;
	 else if (strcmp (value, "White") == 0)
	   default_contour_color = RESERVED_WHITE;
	 else{
	   printf ("Error in resources file!  Aborting program!\n");
	   exit (0);
	 }
       }else if (strcmp (key, "contour levels") == 0){
	 contour_levels.number_levels = 0;
	 do{
	   
	   fgets(buffer,100,in);
	   trim_string(buffer);
	   break_into_key_and_value(":",buffer,&key,&value);
	   
	   if ((isdigit(key[0])) || (strcmp (key, "Max") == 0)){
	     if (!strcmp (key, "Max") == 0)
	       contour_level_value = atoi (key);

	     if (strcmp (value, "Red") == 0)
	       color_number = RESERVED_RED;
	     else if (strcmp (value, "Green") == 0)
	       color_number = RESERVED_GREEN;
	     else if (strcmp (value, "Blue") == 0)
	       color_number = RESERVED_BLUE;
	     else if (strcmp (value, "Cyan") == 0)
	       color_number = RESERVED_CYAN;
	     else if (strcmp (value, "Magenta") == 0)
	       color_number = RESERVED_MAGENTA;
	     else if (strcmp (value, "Yellow") == 0)
	       color_number = RESERVED_YELLOW;
	     else if (strcmp (value, "White") == 0)
	       color_number = RESERVED_WHITE;
	     else if (strcmp (value, "Black") == 0)
	       color_number = RESERVED_BLACK;
	     else{
	       printf ("DError in resources file!  Aborting program!\n");
	       exit (0);
	     }
	     
	     if (strcmp (key, "Max") == 0)
	       max_contour_value_color = color_number;
	     else {
	       contour_levels.colors[contour_levels.number_levels] = 
		 color_number;
	       contour_levels.rlevel[contour_levels.number_levels] = 
		 contour_level_value;
	       contour_levels.number_levels ++;
	     }
	   }
	 }
	 while (strcmp(key,"Max") != 0);


       }else if (strcmp (key, "attempt to use scalable fonts") == 0){
	 if (strcmp (value, "True") == 0)
	   XtVaSetValues (use_scalable_fonts_toggle,XmNset, TRUE,NULL);
	 else if (strcmp (value, "False") != 0){
	   printf ("Error in resources file!  Aborting program!\n");
	   exit (0);
	 }
       }else if (strcmp (key, "view large image labels") == 0){
	 if (strcmp (value, "True") == 0)
	   XtVaSetValues (view_large_image_labels_toggle,XmNset, TRUE,NULL);
	 else if (strcmp (value, "False") != 0) {
	   printf ("Error in resources file!  Aborting program!\n");
	   exit (0);
	 }
       }else if (strcmp (key, "large image labels size") == 0){
	 if (value)
	   large_image_label_size = atoi (value);
	 else{
	   printf ("Error in resources file!  Aborting program!\n");
	   exit (0);
	 }
       }else if (strcmp (key, "view preview image labels") == 0){
	 if (strcmp (value, "True") == 0)
	   XtVaSetValues (view_preview_image_labels_toggle,XmNset, TRUE,NULL);
	 else if (strcmp (value, "False") != 0){
	   printf ("Error in resources file!  Aborting program!\n");
	   exit (0);
	 }
       }else if (strcmp (key, "preview image labels size") == 0){
	 if (value)
	   preview_image_label_size = atoi (value);
	 else{
	   printf ("Error in resources file!  Aborting program!\n");
	   exit (0);
	 }
     }

       test = fgets(buffer,100,in);       
       if (test != buffer) break;
       trim_string(buffer);
       break_into_key_and_value(":",buffer,&key,&value);
   }
     
   fclose (in);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Procedure :
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: none
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/******************************************************************
 * break_into_key_and_value
 ******************************************************************
 * Looks for first br to break the string in two (if none, all
 * of string is key)
 *   throws out the br portion
 *   'trims' each remaining string
 *   converts the key to lowercase
 ******************************************************************/

void break_into_key_and_value(char * br, char * s,
                              char ** key, char ** value) {
  char * wherebreak;

  if (wherebreak=strstr(s, br)) {
    *value = wherebreak + strlen(br);
    trim_string(*value);
    wherebreak[0] = '\0';
  } else {
    *value = s+strlen(s); /* The '\0' at the end */
  }
  *key = s;  
  trim_string(*key);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Procedure :
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: none
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/******************************************************************
 * trim_string
 ****************************************************************** 
 * Based on length of passed string, removes all 'isspace'
 * characters at end of string and then those at the beginning.
 * If it removes characters at the beginning, it must translate
 * the string backwards.  Returns the length of the 'trimmed'
 * string.
 ******************************************************************/
int trim_string(char * s) {
  int i, length, first_non_space; 
  
  length = strlen(s);
    
  /* Trim the end of the string */
  while((length>0)&&(isspace(s[length-1]))) {
    length--;
    s[length] = '\0';
  }
   
  /* Now, trim the beginning of the string -- this will consist of
   * translating the string backwards a few bytes, as necessary
   */
  if (length>0) {
    first_non_space = 0;
 
    while((s[first_non_space]!='\0')&&(isspace(s[first_non_space])))
      first_non_space++;
 
    if (first_non_space>0) {
      for (i=first_non_space; i<=length; i++) {
        s[i-first_non_space] = s[i];
      }
      length -= first_non_space;  
    }    
  }  
  return(length);
}
