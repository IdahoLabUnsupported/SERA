/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Written By:     Mark Rossmeier
%% First Written:  October 24, 1998
%% Last Modified:  January 21, 1999
%%
%% Purpose:  The default .txt files that we have been using, namely
%%           body_data.txt, Fiducial.txt, Constraint.txt, and material.txt
%%           will no longer be the only files that the user can use.
%%           Because of this, we need some way for the user to 
%%           select which files they would like to use. The basic
%%           setup will look something like this:  a popup shell
%%           that contains scrolled lists that the user will
%%           be allowed to select from.  Each time a different file 
%%           is used, it will be saved in a file in 
%%           /Resources/SeraModel/, and this path will appear in
%%           the scrolled list for the user to select the next time.
%%           The files in /Resources/SeraModel will be called:
%%           body_data_files.sav,
%%           fiducial_marker_files.sav,
%%           constraint_marker_files.sav,
%%           material_files.sav.
%%
%%                      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


#ifndef _CHOOSE_TEXT_FILES_H
#define _CHOOSE_TEXT_FILES_H

#include "include.h"

#define FILES_TO_REMEMBER 10


/*
 * Define a structure which we can use for each of the four sections.
 */

typedef struct _popup_section_t {

  Widget frame;                 /* frame to hold this section */
  Widget frame_label;           /* label for the frame */
  Widget form;                  /* main form inside of the frame */
  Widget separator;             /* to separate the list from the button */
  Widget scrolled_list;         /* list containing file names */
  Widget file_selection_button; /* allows user to use a file selection box */
  char remembered_files[FILES_TO_REMEMBER][256];
  int num_remembered_files;
  char saved_file_name[30];     /* where the saved info is located */ 
  
} popup_section_t;

/*
 * The sections of the popup will appear in pairs.  Body data files and 
 * materials  will appear together, and fiducial and constraint markers 
 * will appear together.  Each popup will look like the following structure.
 */

typedef struct _popup_pair_t {

  popup_section_t top_section;          /* the two sections of the popup */
  popup_section_t bottom_section;

  Widget shell;          /* the popup shell */
  Widget main_form;      /* main form to hold the two sections */
  Widget message_label;  /* message to the user telling them what to do */
  Widget separator;     
  Widget load_button;    /* loads the selected files */
  Widget cancel_button;  /* unmanages the popup shell */

  int rebuild;           /* do we need to rebuild the widget? */
  char name[128];        /* name of this popup pair */
  int cancel_button_pressed; /* dismissed because of the cancel button */

} popup_pair_t; 


/*
 * For good encapsulation, we'll put these two structures of pairs into
 * one structure.  Now that the user can choose which constraint and
 * fiducial marker files to use, the dependency on those files and the
 * edit constraint and edit fiducial widgets is much higher.  Because of
 * this, we will put the shells for those two widgets in this 
 * structure too.  That way we can always get to these shells and easily
 * determine if they need to be rebuilt.
 */

typedef struct _choose_files_t {

  popup_pair_t marker_popup;
  popup_pair_t body_and_material_popup;

  Widget edit_fiducial_shell;
  Widget edit_constraint_shell;

  int fid_built_not_realized;    /* fiducial widget built, not realized? */
  int const_built_not_realized;  /* constraint widget built, not realized? */
  int body_data_read;            /* has a body data file been read yet? */
  int materials_read;            /* has a materials file been read yet? */
  int constraint_read;           /* has a constraint file been read yet? */
  int fiducial_read;             /* has a fiducial file been read yet? */

  char materials_file_name[256]; /* file name for the materials file */
  char body_data_file_name[256]; /* file name for the body data file */

} choose_files_t;


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    build_popup_pair
%% Purpose:     Build a popup with two sections
%% Parameters:  parent      -> A Widget, the parent of this popup shell
%%              popup_pair  -> A ptr to a popup_pair_t
%%              top_label   -> The label for the top section frame
%%              bottom_label-> The label for the bottom section frame
%%              dialog_title-> The title for the shell
%%              callback    -> The callback to register with the Load button
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_popup_pair( Widget parent, popup_pair_t * popup_pair,
		       char * top_label, char * bottom_label,
		       char * dialog_title, XtCallbackProc callback );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    make_popup_section
%% Purpose:     Create a section of the popup shell.
%% Parameters:  parent  -> A Widget, the parent of this section
%%              section -> A ptr to a popup_section structure
%%              name    -> The label for the frame of this section
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void make_popup_section( Widget parent, 
			 popup_section_t * section, char * name );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    initialize_saved_file_names
%% Purpose:     Initialize the saved_file_name member of each section
%%              of the popup.
%% Parameters:  popup -> A ptr to a choose_files_t structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void initialize_saved_file_names( choose_files_t * popup ); 


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    save_saved_files
%% Purpose:     Write the contents of the remembered_files member of a 
%%              popup_section structure to its saved file.
%% Parameters   section -> A ptr to a popup_section structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void save_saved_files( popup_section_t * section );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    add_to_saved_files
%% Purpose:     Add a given path name to the list of saved files in the
%%              remembered_files member of a popup_section_t.
%% Parameters   section   -> A ptr to a popup_section structure
%%              file_name -> The file name to add to the list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void add_to_saved_files( popup_section_t * section, char * file_name );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    fill_scrolled_list_from_file
%% Purpose:     Get a list of the most recently used files and put them
%%              in a list from which the user can select.
%% Parameters   section -> A ptr to a popup_section structure.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void fill_scrolled_list_from_file( popup_section_t * section );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    display_marker_popup
%% Purpose:     Popup the choose fiducial and constraint marker widget.
%% Parameters:  none
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void display_marker_popup( );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    display_body_and_material_popup
%% Purpose:     Popup the choose body data and materials files widget.
%% Parameters:  none
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void display_body_and_material_popup( );


/*#####################################################################
##         CALLBACKS USED WITH THE CHOOSE TEXT FILES POPUP           ##
#######################################################################*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    display_fsb_cb
%% Purpose:     Build and display a file selection box, from which the
%%              user will select a file to use for either a body data
%%              file, fiducial marker file, constraint marker file or a 
%%              materials file
%% Parameters:  Callback parameters, a ptr to a popup section will be
%%              passed through clientData
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void display_fsb_cb( Widget w, XtPointer clientData, XtPointer callData ); 


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    load_marker_files_cb
%% Purpose:     Load the two selected files, a fiducial marker file, 
%%              and a constraint marker file from
%%              the choose_text_files popup
%% Parameters:  Callback parameters, a ptr to a popup_pair_t
%%              will be passed as clientData.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void load_marker_files_cb( Widget w, XtPointer clientData, XtPointer callData);


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    load_body_and_material_files_cb
%% Purpose:     Load the two selected files, a body data file and a
%%              materials file from the choose body data and materials
%%              popup
%% Parameters:  Callback parameters, a ptr to a popup_pair_t
%%              will be passed as clientData
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void load_body_and_material_files_cb( Widget w, XtPointer clientData, XtPointer callData );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function:    popup_cancel_cb
%% Purpose:     Destroy a popup shell and its children
%% Parameters:  Callback parameters, a ptr to a popup_pair_t will be 
%%              passed as clientData.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void popup_cancel_cb( Widget w, XtPointer clientData, XtPointer callData);


#endif
