#include "choose_text_files.h"
#include "functions.h"
#include "debug_tools.h"
#include "dialog_tools.h"
#include "file_tools.h"
#include "libsz.h"

static int countValidFiles( char * filename );

/*** Build the popup shell and its children ***/
void build_popup_pair( Widget parent, popup_pair_t * popup_pair,
		       char * top_label, char * bottom_label,
		       char * dialog_title, XtCallbackProc callback ) {
  XmString xmstr;


  DEBUG_TRACE_IN printf("Entering build_popup_pair\n");

  /*
   * Create a popup shell which will contain three different sections.
   * The three sections will be scrolled lists allowing the user to
   * choose from different files, or use a file selection box.
   *
   * NOTE: Get rid of the "-" in the upper left hand corner so the 
   *       dialog must be dismissed by hitting the Cancel button or
   *       the Load Files button.
   */

  popup_pair->shell = XtVaCreatePopupShell( "text_file_popup",
					    xmDialogShellWidgetClass,
					    parent,
					    XmNtitle, dialog_title,
					    XmNmwmDecorations, MWM_DECOR_ALL|MWM_DECOR_MENU,
                                            XmNallowShellResize, TRUE,
                                            XmNdeleteResponse, XmDO_NOTHING,
					    NULL );

  /*
   * Put a main form inside this shell so we can manage attachments
   */

  popup_pair->main_form = XtVaCreateWidget( "main_form",
					    xmFormWidgetClass,
					    popup_pair->shell,
					    XmNhorizontalSpacing, 5,
					    XmNverticalSpacing,   10,
					    XmNautoUnmanage, FALSE,
					    NULL );

  /*
   * Create a label telling the user what to do
   */
  xmstr = XmStringCreateLtoR("Please select which files you would like to use",
			     MY_CHARSET);
  popup_pair->message_label = 
    XtVaCreateManagedWidget( "message_label",
			     xmLabelWidgetClass,
			     popup_pair->main_form,
			     XmNlabelString, xmstr, 
			     XmNalignment, XmALIGNMENT_BEGINNING,
  			     XmNtopAttachment, XmATTACH_FORM,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL );
  XmStringFree( xmstr );

  /*
   * Now create the two main sections of the popup
   */

  make_popup_section( popup_pair->main_form, 
		      &(popup_pair->top_section), top_label );
  make_popup_section( popup_pair->main_form,
		      &(popup_pair->bottom_section), bottom_label );

  /*
   * Attach the different sections to each other
   */

  XtVaSetValues( popup_pair->top_section.frame,
		 XmNtopAttachment, XmATTACH_WIDGET,
		 XmNtopWidget,     popup_pair->message_label,
		 NULL );

  XtVaSetValues( popup_pair->bottom_section.frame,
		 XmNtopAttachment, XmATTACH_WIDGET, 
		 XmNtopWidget,     popup_pair->top_section.frame,     
		 NULL );

  /*
   * Add a separator and a load button to the bottom of the popup.
   * The load button will take the selected items from each scrolled
   * list and open and process those files
   */

  popup_pair->separator =
    XtVaCreateManagedWidget( "separator",
			     xmSeparatorWidgetClass,
			     popup_pair->main_form,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, popup_pair->bottom_section.frame,
			     XmNtopOffset, 15,
			     XmNleftAttachment,  XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL );

  xmstr = XmStringCreateLtoR( "Load Files", MY_CHARSET );
  popup_pair->load_button =
    XtVaCreateManagedWidget( "load_button",
			     xmPushButtonWidgetClass,
                             popup_pair->main_form,
			     XmNlabelString, xmstr,
			     XmNrightAttachment, XmATTACH_FORM,
			     XmNtopAttachment, XmATTACH_WIDGET,
			     XmNtopWidget, popup_pair->separator,
			     XmNbottomAttachment, XmATTACH_FORM,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( popup_pair->load_button, XmNactivateCallback,
		 callback, (XtPointer) popup_pair );

  /*
   * Assume the widget will be dismissed due to hitting the
   * load files button.
   */
  popup_pair->cancel_button_pressed = 0;

  xmstr = XmStringCreateLtoR( "Cancel", MY_CHARSET );
  popup_pair->cancel_button =
    XtVaCreateManagedWidget( "cancel_button", 
			     xmPushButtonWidgetClass,
			     popup_pair->main_form,
			     XmNlabelString, xmstr,
			     XmNrightAttachment, XmATTACH_WIDGET,
			     XmNrightWidget,     popup_pair->load_button,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       popup_pair->separator,
			     XmNbottomAttachment,XmATTACH_FORM,
			     NULL );
  XmStringFree( xmstr );

  XtAddCallback( popup_pair->cancel_button, XmNactivateCallback,
		 popup_cancel_cb, (XtPointer) popup_pair );

  /*
   * Manage the form inside of the shell
   */

  XtManageChild( popup_pair->main_form );

  /* 
   * NOTE: We haven't popped up the shell in this function.
   * The shell must be popped up in "main" after the call
   * to build_popup_pair()
   */

  DEBUG_TRACE_OUT printf("Leaving build_popup_pair\n");
}


/*** Make a section of the popup ***/
void make_popup_section( Widget parent,
			 popup_section_t * section, char * name ) {

  XmString xmstr;
  Arg al[1];
  
  DEBUG_TRACE_IN printf("Entering make_popup_section\n");

  /*
   * First make a frame, with a frame label of name. Then we'll put a form
   * inside the frame to manage the other widgets
   */

  section->frame =
    XtVaCreateManagedWidget( "frame",
			     xmFrameWidgetClass,
			     parent,
			     XmNleftAttachment, XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL );

  xmstr = XmStringCreateLtoR( name, MY_CHARSET );
  section->frame_label =
    XtVaCreateManagedWidget( "frame_label",
			     xmLabelWidgetClass,
			     section->frame,
			     XmNlabelString, xmstr,
			     XmNchildType, XmFRAME_TITLE_CHILD,
			     NULL );
  XmStringFree( xmstr );

  section->form =
    XtVaCreateManagedWidget( "form",
			     xmFormWidgetClass,
			     section->frame,
			     XmNverticalSpacing, 5,
			     XmNhorizontalSpacing, 5,
			     NULL );

  /*
   * Make a scrolled list as a child of this form
   */
  XtSetArg( al[0], XmNlistSizePolicy, XmVARIABLE );
  
  section->scrolled_list =
      XmCreateScrolledList( section->form,
                            "scrolled_list", 
                            al, 1 );
  
  XtVaSetValues( XtParent(section->scrolled_list),
		 XmNtopAttachment,    XmATTACH_FORM,
		 XmNleftAttachment,   XmATTACH_FORM,
		 XmNrightAttachment,  XmATTACH_FORM,
		 NULL );
  
  XtManageChild( section->scrolled_list );

  /*
   * Add a separator and a FSB button
   */

  section->separator =
    XtVaCreateManagedWidget( "separator",
			     xmSeparatorWidgetClass,
			     section->form,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       XtParent(section->scrolled_list),
			     XmNleftAttachment,  XmATTACH_FORM,
			     XmNrightAttachment, XmATTACH_FORM,
			     NULL );

  xmstr = XmStringCreateLtoR( "Use FSB", MY_CHARSET );
  section->file_selection_button =
    XtVaCreateManagedWidget( "file_selection_button",
			     xmPushButtonWidgetClass,
			     section->form,
			     XmNlabelString, xmstr,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       section->separator,
			     XmNrightAttachment, XmATTACH_FORM,
			     XmNbottomAttachment, XmATTACH_FORM,
			     NULL );
  XmStringFree( xmstr );

  /*
   * Add a callback for the FSB button
   */

  XtAddCallback( section->file_selection_button, XmNactivateCallback,
		 display_fsb_cb, (XtPointer) section );
   

  /*
   * Before returning, fill the scrolled list from a file
   * and set the scrolled list to display all the files found
   */

  fill_scrolled_list_from_file( section );

  XtVaSetValues( section->scrolled_list, 
		 XmNvisibleItemCount, 5,
		 NULL );

  DEBUG_TRACE_OUT printf("Leaving build_popup_section\n");
}


/*** Initialize the saved_file_name for each section ***/
void initialize_saved_file_names( choose_files_t * popup ) {

  DEBUG_TRACE_IN printf("Entering initialize_saved_file_names\n");

  strcpy( popup->marker_popup.top_section.saved_file_name, 
	  "fiducial_marker_files.sav" );
  
  strcpy( popup->marker_popup.bottom_section.saved_file_name, 
	  "constraint_marker_files.sav" );

  strcpy( popup->marker_popup.name, "Marker Files Popup" );

  popup->marker_popup.rebuild = 1; /* need to rebuild before displaying */

  strcpy( popup->body_and_material_popup.top_section.saved_file_name,
	  "body_data_files.sav" );

  strcpy( popup->body_and_material_popup.bottom_section.saved_file_name,
	  "material_files.sav" );

  strcpy( popup->body_and_material_popup.name, "Body Data Popup" );

  popup->body_and_material_popup.rebuild = 1; /*need to rebuild before displaying*/

  DEBUG_TRACE_OUT printf("Leaving initialize_saved_file_names\n");
}


/*** Write the contents of the remembered_files member to a file ***/
void save_saved_files( popup_section_t * section ) {

  FILE * fptr;
  int i;
  char path_name[256];

  DEBUG_TRACE_IN printf("Entering save_saved_files\n");

  get_image_editor_resource_path_name( path_name, section->saved_file_name );

  if( !( fptr = fopen( path_name, "w" ) ) ) {
    fprintf( stderr, "Could not open %s for writing\n", path_name );
    DEBUG_TRACE_OUT printf("Leaving save_saved_files, couldn't open file\n");
    return;

  } else {
    for( i = 0; i < section->num_remembered_files; i++ )
      fprintf( fptr, "%s\n", section->remembered_files[i] );
  }
  fclose( fptr ); 

  DEBUG_TRACE_OUT printf("Leaving save_saved_files\n");
}


/*** Add a given path name to the list of remembered files ***/
void add_to_saved_files( popup_section_t * section, char * file_name ) {
  int i, j;

  DEBUG_TRACE_IN printf("Entering add_to_saved_files\n");

  /* Delete the filename from the list if already present */
  for ( i = 0; i < section->num_remembered_files; i++ ) {
    if ( !strcmp(file_name, section->remembered_files[i])) {
      for ( j = i+1; j < section->num_remembered_files; j++ ) {
	strcpy(section->remembered_files[j-1], section->remembered_files[j]);
      }
      section->num_remembered_files--;
    }
  }
  /* Now, make room for the new first entry */
  if ( section->num_remembered_files == FILES_TO_REMEMBER ) {
    /* If already filled, will need to delete 1 */
    section->num_remembered_files--;
  }
  for ( i = section->num_remembered_files-1; i >= 0; i--) {
    strcpy(section->remembered_files[i+1], section->remembered_files[i]);
  }
  strcpy(section->remembered_files[0], file_name);

  /* Added 1 so be sure to increment count */
  section->num_remembered_files++;
  /* Now, be sure to save the new list */
  save_saved_files( section );

  DEBUG_TRACE_OUT printf("Leaving add_to_saved_files\n");
}


/*** Populate a section's scrolled list from a file ***/
void fill_scrolled_list_from_file( popup_section_t * section ) {

  FILE * fptr;
  int i, size;
  char file_name[256];
  XmString xmstr;
 
  DEBUG_TRACE_IN printf("Entering fill_scrolled_list_from_file\n");

  get_image_editor_resource_path_name( file_name, section->saved_file_name );  
  section->num_remembered_files = 0;

  /*
   * Check to see if the file exists, AND has some valid files in it.
   * If not create the file with a default entry in it.
   */
  if( countValidFiles( file_name ) == 0 )
  {
      fptr = fopen( file_name, "w" );

      if( fptr == NULL )
      {
          fprintf( stderr, "Could not open %s for writing\n", file_name );
          DEBUG_TRACE_OUT printf("Leaving fill_scrolled_list_from_file, file not opened\n");
          return;
      } 
      else 
      {
          if( !strcmp( section->saved_file_name, "body_data_files.sav" ) ) 
          {
              fprintf( fptr, "%s\n", get_body_data_fname() );
          }
          else if( !strcmp( section->saved_file_name, "fiducial_marker_files.sav" ) ) 
          {
              fprintf( fptr, "%s\n", get_fiducial_markers_fname() );
          }
          else if( !strcmp( section->saved_file_name, "constraint_marker_files.sav" ) )
          {
              fprintf( fptr, "%s\n", get_constraint_markers_fname() );
          }
          else
          {
              fprintf( fptr, "%s\n", get_materials_fname() );
          }
          fclose( fptr );
      }
  }

  /*
   * At this point, we have a file with at least one valid filename in it.
   * So read in the file.
   */
  fptr = fopen( file_name, "r" );

  if( fptr != NULL )
  {
      for( i = 0; i < FILES_TO_REMEMBER; i++ )
      {
          size = readln3( fptr, 
                          section->remembered_files[section->num_remembered_files], 
                          256 );

          if( size == -1 ) break; /* exit loop when eof is reached */
    
          if( size > 0 ) {
              /* If the file has been deleted don't add it to the list */
              if( FT_fileExists( section->remembered_files[section->num_remembered_files] ) ){
                  section->num_remembered_files++;
              }
          }
      }
      fclose( fptr );
  }
  else
  {
      fprintf( stderr, "Could not open %s for writing\n", file_name );
      DEBUG_TRACE_OUT printf("Leaving fill_scrolled_list_from_file, file not opened\n");
      return;
  }

  /*
   * Now add the elements of the remembered_files member to the scrolled list
   */
  
  for( i = 0; i < section->num_remembered_files; i++ ) {
    xmstr = XmStringCreateLtoR( section->remembered_files[i], MY_CHARSET );
    XmListAddItem( section->scrolled_list, xmstr, i + 1 );
    
    if( i == 0 ) {
      XmListSelectItem( section->scrolled_list, xmstr, i + 1 );
    }
    XmStringFree( xmstr );
  }
  DEBUG_TRACE_OUT printf("Leaving fill_scrolled_list_from_file\n");
}

/*** Display the Fiducial and Constraint Markers popup ***/
void display_marker_popup( ) {

  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf("Entering display_marker_popup\n");

  image_matrix_ptr = get_image_matrix();

  if( image_matrix_ptr->choose_files.marker_popup.rebuild ) {
    
    build_popup_pair( image_matrix_ptr->toplevel,
		      &(image_matrix_ptr->choose_files.marker_popup),
		      "Fiducial Marker Files", "Constraint Marker Files",
		      "Choose Fiducial and Constraint Marker Files",
		      (XtCallbackProc) load_marker_files_cb);

    XtPopup( image_matrix_ptr->choose_files.marker_popup.shell, XtGrabExclusive );
    image_matrix_ptr->choose_files.marker_popup.rebuild = 0;
  }
  DEBUG_TRACE_OUT printf("Leaving display_marker_popup\n");
}

/*** Display the Body Data and Materials popup ***/
void display_body_and_material_popup( ) {

  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf("Entering display_body_and_material_popup\n");

  image_matrix_ptr = get_image_matrix();

  if( image_matrix_ptr->choose_files.body_and_material_popup.rebuild ) {

    build_popup_pair( image_matrix_ptr->toplevel,
		      &(image_matrix_ptr->choose_files.body_and_material_popup),
		      "Body Data Files", "Material Files",
		      "Choose Body Data and Material Files",
		      (XtCallbackProc) load_body_and_material_files_cb);

    XtPopup( image_matrix_ptr->choose_files.body_and_material_popup.shell, XtGrabExclusive );
    image_matrix_ptr->choose_files.body_and_material_popup.rebuild = 0;
  }
  DEBUG_TRACE_OUT printf("Leaving display_body_and_material_popup\n");
}

/*#####################################################################
##               CALLBACK SECTION FOR THE TEXT FILE POPUP            ##
#######################################################################*/

/*** Build and display a file selection box ***/
void display_fsb_cb( Widget w, XtPointer clientData, XtPointer callData ) {

  popup_section_t * section = (popup_section_t *) clientData;
  image_matrix_type * image_matrix_ptr;
  int successful;
  char file_to_add[256];
  
  DEBUG_TRACE_IN printf("Entering display_fsb_cb\n");

  image_matrix_ptr = get_image_matrix();
  successful = DT_select_file( image_matrix_ptr->toplevel,
			       image_matrix_ptr->app,
			       file_to_add,
			       "Choose a Text File to Use" );
  /*
   * DT_select_file will return 1 if a file was chosen from
   * the file selection dialog.  So if a file was chosen, make
   * sure it is valid before adding it to the scrolled list.
   * If the file is not valid tell the user.
   */

  if( successful ) 
  {
    
    if( FT_fileExists( file_to_add ) && SZ_IsASeraZippedFile( file_to_add ) ) 
    {
      add_to_saved_files( section, file_to_add );

      /* Add file to the scrolled list only if it is still managed */
      if( XtIsManaged( section->scrolled_list ) )
      {

	XmListDeleteAllItems( section->scrolled_list );
	fill_scrolled_list_from_file( section );
      }
    }
    else /* Not a valid file name, don't add to the scrolled list */
    {
      DT_inform( image_matrix_ptr->toplevel,
		 "The selected file is not a valid .sz file name",
		 "Invalid File",
		 NULL );
    }
  }

  DEBUG_TRACE_OUT printf("Leaving display_fsb_cb\n");
}


/*** Load the two markers files from the popup ***/
void load_marker_files_cb( Widget w, XtPointer clientData, XtPointer callData ){
  
    popup_pair_t * popup_pair = (popup_pair_t *) clientData;
    image_matrix_type * image_matrix_ptr;
    char * fid_file_name;
    char * const_file_name;
    XmStringTable fid_selected;
    XmStringTable const_selected;
    int num_fid_selected;
    int num_const_selected;
  

    DEBUG_TRACE_IN printf("Entering load_marker_files_cb\n");

    image_matrix_ptr = get_image_matrix();

    /*
     * Make sure that we have some selected items.
     */
    XtVaGetValues( popup_pair->top_section.scrolled_list, XmNselectedItemCount, &num_fid_selected, NULL );
    XtVaGetValues( popup_pair->bottom_section.scrolled_list, XmNselectedItemCount, &num_const_selected, NULL );

    if( (num_fid_selected > 0) && (num_const_selected > 0) )
    {
        /*
         * We are loading new files so deallocate the memory used
         * by the old ones first.
         */
        clean_marker_info( image_matrix_ptr );

        /*
         * Get the compound strings of each of the selected file names in the three lists
         */

        XtVaGetValues( popup_pair->top_section.scrolled_list,
                       XmNselectedItems, &fid_selected, NULL );
        XtVaGetValues( popup_pair->bottom_section.scrolled_list,
                       XmNselectedItems, &const_selected, NULL );
		 
        /*
         * Now get the char * equivalants of these compound strings
         */

        XmStringGetLtoR( fid_selected[0], MY_CHARSET, &fid_file_name );
        XmStringGetLtoR( const_selected[0], MY_CHARSET, &const_file_name );

        /*
         * Make sure these files are in the correct format (.sz).
         */
        if( SZ_IsASeraZippedFile( fid_file_name ) &&
            SZ_IsASeraZippedFile( const_file_name ) )
        {
            /*
             * Make the currently selected item the first entry in the file
             * where the recent file names are saved so it will be the default
             * entry the next time the user runs the program.
             */

            add_to_saved_files( &(popup_pair->top_section), fid_file_name );
            add_to_saved_files( &(popup_pair->bottom_section), const_file_name );

            /*
             * Now read the files and mark them as read
             */

            read_valid_markers( image_matrix_ptr, fid_file_name, const_file_name );
            image_matrix_ptr->choose_files.constraint_read = 1;
            image_matrix_ptr->choose_files.fiducial_read = 1;

            /*
             * These have been moved from main because we don't know
             * how many fiducial markers and constraint markers we have
             * until this point. These functions need to know those numbers
             * so they can build the widget correctly.
             */
            image_matrix_ptr->choose_files.edit_fiducial_shell =
                make_markers_shell( "Edit Fiducial Markers", FIDUCIAL );
            
            image_matrix_ptr->choose_files.edit_constraint_shell =
                make_markers_shell( "Edit Constraint Markers", CONSTRAINT );
            
            /*
             * We've built them, but they still aren't realized
             */
            image_matrix_ptr->choose_files.fid_built_not_realized = 1;
            image_matrix_ptr->choose_files.const_built_not_realized = 1;
            
            XtPopdown( popup_pair->shell );
            XtDestroyWidget( popup_pair->shell );
            popup_pair->rebuild = 1;
        }
        else
        {
            DT_error( w, "One or both of your selected files\nare not in the correct format",
                      "Incorrectly Formatted Files", NULL );
        }
        
        XtFree( (char *) fid_file_name );
        XtFree( (char *) const_file_name );
    }
    
    DEBUG_TRACE_OUT printf("Leaving load_marker_files_cb\n");
} 

void load_body_and_material_files_cb( Widget w, XtPointer clientData, XtPointer callData ) {
 
    popup_pair_t * popup_pair = (popup_pair_t * ) clientData;
    image_matrix_type * image_matrix_ptr;
    char * body_data_file_name;
    char * materials_file_name;
    XmStringTable body_data_selected;
    XmStringTable materials_selected;
    int num_bd_selected;
    int num_mat_selected;
    Widget tempwidget;
  
    DEBUG_TRACE_IN printf("Entering load_body_and_material_files_cb\n");

    image_matrix_ptr = get_image_matrix();

    /*
     * First make sure that we have some selected files.
     */
    XtVaGetValues( popup_pair->top_section.scrolled_list, XmNselectedItemCount, &num_bd_selected, NULL );
    XtVaGetValues( popup_pair->bottom_section.scrolled_list, XmNselectedItemCount, &num_mat_selected, NULL );
    
    if( (num_bd_selected > 0) && (num_mat_selected > 0) )
    {
        /*
         * Get the compound strings of each of the selected file names in the two
         * lists. These return pointers to the actual strings, NOT COPIES, so do
         * not free these.
         */

        XtVaGetValues( popup_pair->top_section.scrolled_list,
                       XmNselectedItems, &body_data_selected, NULL );
        XtVaGetValues( popup_pair->bottom_section.scrolled_list,
                       XmNselectedItems, &materials_selected, NULL );

        /*
         * Now get the char * equivalants of these compound strings
         */

        XmStringGetLtoR( body_data_selected[0], MY_CHARSET, &body_data_file_name );
        XmStringGetLtoR( materials_selected[0], MY_CHARSET, &materials_file_name );

        /*
         * Make sure these files are in the correct format (.sz).
         */
        if( SZ_IsASeraZippedFile( body_data_file_name ) &&
            SZ_IsASeraZippedFile( materials_file_name ) )
        {
            /*
             * Make the currently selected item the first entry in the file
             * where the recent file names are saved so it will be the default
             * entry the next time the user runs the program.
             */
            
            add_to_saved_files( &(popup_pair->top_section), body_data_file_name );
            add_to_saved_files( &(popup_pair->bottom_section), materials_file_name );
            
            /*
             * Read the body data file.
             * Set the Load Body Data and Materials button on
             * the menubar to insensitive; only allow one body
             * data file at a time.
             */
            
            read_body_data( body_data_file_name );
            tempwidget = XtNameToWidget( image_matrix_ptr->toplevel, "*Load Body Data and Material Files" );
            if( tempwidget )
                XtSetSensitive( tempwidget, False );
                        
            /*
             * Copy the materials file name and the body data file into the 
             * choose files structure  so that it can be used at a later time
             */
            strcpy( image_matrix_ptr->choose_files.materials_file_name,
                    materials_file_name );
            strcpy( image_matrix_ptr->choose_files.body_data_file_name,
                    body_data_file_name );
            
            /*
             * Mark these files as being read
             */
            image_matrix_ptr->choose_files.body_data_read = 1;
            image_matrix_ptr->choose_files.materials_read = 1;
            
            XtPopdown( popup_pair->shell );
            XtDestroyWidget( popup_pair->shell );
            popup_pair->rebuild = 1;
        }
        else /* Files are in the wrong format */
        {
            DT_error( w, "One or both of your selected files\nare not in the correct format",
                      "Incorrectly Formatted Files", NULL );
        }

        XtFree( (char *) body_data_file_name );
        XtFree( (char *) materials_file_name );        
    }
    
    DEBUG_TRACE_OUT printf("Leaving load_body_and_material_files_cb\n");
}


/*** Destroy a popup shell and its children ***/
void popup_cancel_cb( Widget w, XtPointer clientData, XtPointer callData ) {

  popup_pair_t * popup_pair = (popup_pair_t *) clientData;
  program_defaults_type * pdt_ptr;
  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf("Entering popup_cancel_cb\n");

  image_matrix_ptr = get_image_matrix();
  pdt_ptr = get_program_defaults();

  /*
   * If the user has EditRegionsViewable set to true in their itrc file
   * the edit regions pane will appear when the program starts up.
   * Because a body data file must be loaded before editing regions, 
   * we must make sure that a body data file is loaded. With this if
   * statement, the dialog cannot be dismissed until the user loads
   * a body data file.
   */
  if( (pdt_ptr->EditRegionsViewable == 1) &&
      (image_matrix_ptr->choose_files.body_data_read == 0 ) )
  {
      DEBUG_TRACE_OUT printf("Leaving popup_cancel_cb\n");
      return;
  }

  /*
   * Mark the correct cancel button as being pressed
   */
  popup_pair->cancel_button_pressed = 1;

  XtPopdown( popup_pair->shell );
  XtDestroyWidget( popup_pair->shell );
  popup_pair->rebuild = 1;

  DEBUG_TRACE_OUT printf("Leaving popup_cancel_cb\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     countValidFiles
%%%
%%%  Purpose:      Given the name of a file, with filenames in it, count
%%%                the number of filenames that currently exist on the system.
%%%
%%%  Parameters:   filename -> A char *, the name of the file to read.
%%%
%%%  Returns:      The number of filenames in the file that exist.
%%%                A return of zero means that filename does not exist,
%%%                or contains no valid files.
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int countValidFiles( char * filename )
{
    int fileCount = 0;
    FILE * filePtr;
    char buffer[256];
    int length;
    
    DEBUG_TRACE_IN printf("Entering countValidFiles, looking in %s\n", filename);

    filePtr = fopen( filename, "r" );

    if( filePtr != NULL )
    {
        while( fgets( buffer, 255, filePtr ) )
        {
            length = strlen( buffer );
            buffer[ length - 1 ] = '\0';

            if( FT_fileExists( buffer ) )
                fileCount++;
        }
        
        fclose( filePtr );
    }
    
    DEBUG_TRACE_OUT printf("Leaving countValidFiles, found %d files\n", fileCount);
    return( fileCount );
}
