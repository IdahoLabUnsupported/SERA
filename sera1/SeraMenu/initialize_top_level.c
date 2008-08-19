#include "seramenu.h"


void initialize_top_level( main_gui_t * gui, int * argc, char * argv[] )

{

  gui->toplevel = XtVaAppInitialize( &gui->app,
				     "SeraMenu",
				     options,
				     XtNumber( options ),
				     argc, 
				     argv,
				     NULL,
				     NULL);

   /*
    * Needed for debugging purposes.
    * See sera1/Shared/include/debug_tools.h for more info.
    * NOTE the different parameters to XtVaAppInitialize above.
    */

   set_debug_values( argv[0], gui->toplevel );

   if( *argc > 1 )
     debug_syntax( *argc, argv );
   
   gui->containers.top_level_row_column = 
     XtVaCreateManagedWidget("top_level_row_column", 
			     xmRowColumnWidgetClass,
			     gui->toplevel, NULL);

}
