#include "rtt_define.h"
#include "rtt_include.h"
#include "rtt_typedef.h"
/* define DECLARATION to force the externOrNot to be nil in rtt_global.h */
#define DECLARATION

#include "rtt_global.h"
#include "rtt_proto.h"
#include "debug_tools.h"
#include "environment_tools.h"

int main(int argc, char ** argv)
{
    static String fallback_resources[]={
        /* A resource file will override any of these defaults */
        "*fontList: -*-Fixed-Medium-R-*-*-*-120-*-*-*-*-*-*",
        "*background:                    SkyBlue",
        "*borderWidth:                   0",
        "*foreground:                    Black",
        NULL
    };
    
    ET_checkEnvironment( ); /* Check environment variables */

    rtt_top = XtVaAppInitialize(&rtt_app, "SeraCalc", options, XtNumber(options),
                                &argc, argv,
                                fallback_resources,
                                NULL);

    set_debug_values( argv[0], rtt_top );
    if( argc > 1 )
        debug_syntax( argc, argv );

    DEBUG_TRACE_IN printf("Entering main\n");

    rtt_file_data = NULL;
    rtt_run_widgets = NULL;
    viewGui = NULL;
    
    new_rtt_popup();
    rtt_mc();

    XtAppMainLoop(rtt_app);
    return( 0 );
}
