#include "rtt.h"

void new_rtt_popup()
{
    Widget rtt_Form;

    Arg al[20];
    int ac;
    int i;
    char * ptr;
    
    DEBUG_TRACE_IN printf("Entering new_rtt_popup\n");
    
    rtt_file_data   = (RttEditPopupSt *) MT_malloc(sizeof(RttEditPopupSt));
    rtt_run_widgets = (RttRunSt *)       MT_malloc(sizeof(RttRunSt));

    viewGui = (viewGui_t *) MT_malloc( sizeof( viewGui_t ) );
    
    /* initialize button names */
    rtt_file_data->buttons[0] = "file_button1";
    rtt_file_data->buttons[1] = "file_button2";
    rtt_file_data->buttons[2] = "file_button3";
    rtt_file_data->buttons[3] = "file_button4";
    rtt_file_data->buttons[4] = "file_button5";
    rtt_file_data->buttons[5] = "file_button6";
    rtt_file_data->buttons[6] = "file_button7";
    rtt_file_data->buttons[7] = "file_button8";
    rtt_file_data->buttons[8] = "file_button9";
    rtt_file_data->buttons[9] = "file_button10";
    rtt_file_data->buttons[10] = "file_button11";
    rtt_file_data->buttons[11] = "file_button12";

    /* initialize field names */
    rtt_file_data->fields[0] = "field1";
    rtt_file_data->fields[1] = "field2";
    rtt_file_data->fields[2] = "field3";
    rtt_file_data->fields[3] = "field4";
    rtt_file_data->fields[4] = "field5";
    rtt_file_data->fields[5] = "field6";
    rtt_file_data->fields[6] = "field7";
    rtt_file_data->fields[7] = "field8";
    rtt_file_data->fields[8] = "field9";
    rtt_file_data->fields[9] = "field10";
    rtt_file_data->fields[10] = "field11";
    rtt_file_data->fields[11] = "field12";
    rtt_file_data->numFields = 9;

    /* initialize file names */
    rtt_file_data->fileName[0] = (char *) MT_malloc( 256 * sizeof( char ) );
    ptr = getenv( "PWD" );
    if( ptr != NULL ) {
        sprintf( rtt_file_data->fileName[0], "%s/", ptr );
        strcpy ( rtt_file_data->saveDirectory, rtt_file_data->fileName[0] );
        rtt_file_data->saveDirectoryValid = 1;
    }
    else {
        sprintf( rtt_file_data->fileName[0], "/" );
        rtt_file_data->saveDirectory[0] = '\0';
        rtt_file_data->saveDirectoryValid = 0;
    }

    rtt_file_data->originalInputFile[0] = '\0';
    rtt_file_data->originalInputFileValid = 0;
    
    rtt_file_data->fileName[1] = "cg.geom";
    rtt_file_data->fileName[2] = "";
    rtt_file_data->fileName[3] = "";
    rtt_file_data->fileName[4] = "sera.rst";
    rtt_file_data->fileName[5] = "";
    rtt_file_data->fileName[6] = "$SERA_HOME/Resources/SeraMC/seraMC.mat";
    rtt_file_data->fileName[7] = "$SERA_HOME/Resources/SeraMC/seraMC.sigma";
    rtt_file_data->fileName[8] = "run_seraMC";
    rtt_file_data->fileName[9] = "$SERA_HOME/Resources/SeraMC/range.seraMC";
    rtt_file_data->fileName[10] = "$SERA_HOME/Resources/SeraMC/ultradir";
    rtt_file_data->fileName[11] = "$SERA_HOME/Resources/SeraMC/ultralib";

    /* seraMC not running yet */
    rtt_file_data->rttRunInProgress = 0;

    /* initialize iop stuff */
    rtt_file_data->rtt_iop_saveflag = 0;
    rtt_file_data->rtt_iop_plusflag = 0;
    rtt_file_data->rtt_iop_button   = 1;
    rtt_file_data->iopInfoInFile    = 0;

    ac = 0;
    XtSetArg(al[ac], XmNx, 10); ac++;
    XtSetArg(al[ac], XmNy, 200); ac++;
    XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
    rtt_file_data->rtt_shell = XtCreatePopupShell("seraCalc", topLevelShellWidgetClass,
                                                  rtt_top, al, ac);
    ac = 0;
    rtt_Form =  XmCreateForm(rtt_file_data->rtt_shell, "Rtt_Form", al, ac);

    /*
     *  Set up some default values for parameters that are not allowed to be changed
     *  in the widget
     */

/*  rtt_file_data->edit_voxel_dimension = 30; */
    rtt_file_data->number_energy_bins = 3;
    rtt_file_data->break_point1 = 32;
    rtt_file_data->break_point2 = 72;
    rtt_file_data->break_point3 = 94;
    rtt_file_data->random_seed = 1;

    /* Create the MenuBar for the view popup */

    create_rtt_forms(rtt_Form, rtt_file_data);

    create_rtt_monitor();

    create_rtt_edit_menus(rtt_edit_MenuBar, rtt_file_data->rtt_shell);

    /* set display views flag */
    viewGui->displayViewWindow = 1;
    XmToggleButtonSetState( viewGui->viewToggle, True, False );

    /* Initialize ximages */
    for( i = 0; i < NUM_VIEWS; i++ )
    {
        viewGui->views[i].ximage = NULL;
        viewGui->views[i].imageData = NULL;
    }
    
    /* get some display values */
    viewGui->display = XtDisplay    ( rtt_top );
    viewGui->screen  = DefaultScreen( viewGui->display );

    /* fill the gray colormap */
    for ( i = 0; i < 256; i++ )
    {
        viewGui->gray_colormapping[i] = (int)((float)i*(float)(MAX_GRAY - MIN_GRAY + 1)/256.0 + (float)MIN_GRAY);
    }

    /* Initialize colors and the colormap */
    init_colors( viewGui );
    add_guaranteed_colors( viewGui );
    load_gamma_colormap( viewGui, viewGui->color_info.cmap_values, (float)(viewGui->color_info.gamma)/10.0 );
    colormap_load_rgb( viewGui );
    initViewGraphicsContext( );
    
    /* create the window for displaying the view.* files */
    createViewWindow( rtt_file_data->rtt_shell );

    /* Manage the main form last */
    XtManageChild(rtt_Form);
    
    DEBUG_TRACE_OUT printf("Leaving new_rtt_popup\n");
    
} /* end new_rtt_popup */
