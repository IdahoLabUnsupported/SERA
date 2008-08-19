/*
 * preferences.h
 *
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 * 
 * David Helzer
 * August, 1997
 */
 
 
Widget create_preferences_shell (Widget parent);
void preferences_callback (Widget w, XtPointer callData, XtPointer clientData);
void preferences_selected_callback (Widget parent, XtPointer callData,
                                    XtPointer clientData);
void set_default_preferences (void);

void load_preferences (void);
