#include "environment_tools.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Local Prototypes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void initializeVariables( variable_t * variables );
static void printResults( variable_t * variables );
static int  validVariable( char * var, variable_t * variable );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:      ET_checkEnvironment      
%%% 
%%% Purpose:       Check all of the environment variables required by the
%%%                SERA modules. Print out variables that are incorrect.
%%%                If a required environment variable is not found or
%%%                incorrect, a warning will be issued.
%%% 
%%% Parameters:    none
%%%                
%%% Return Value:  none
%%% 
%%% Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ET_checkEnvironment( void )
{
    variable_t variables[NUM_ENVIRONMENT_VARIABLES];
    char * ptr;
    int i;
    int all_valid = 1;

    /* Initialize properties for each variable */
    initializeVariables( variables );

    for( i = 0; i < NUM_ENVIRONMENT_VARIABLES; i++ )
    {
        ptr = (char *) getenv( variables[i].variableName );

        if( ptr != NULL ) /* the variable has been set... */
        {
            if( validVariable( ptr, &variables[i] ) ) /*... but is it valid */
            {
                variables[i].valid = 1;
            }
            else
            {
                strcpy( variables[i].actual, ptr );
                all_valid = 0;
            }
        }
        else /* the variable hasn't been set at all */
        {
            strcpy( variables[i].actual, "Not Set" );
            all_valid = 0;
        }
    }

    if( !all_valid )
    {
        printResults( variables );  /* print what we found */
    }
}


void initializeVariables( variable_t * variables )
{
    strcpy( variables[seraHome].variableName, "SERA_HOME" );
    strcpy( variables[seraHome].shouldBe, "$HOME/sera1" );
    strcpy( variables[seraHome].required, "/sera1" );
    variables[seraHome].valid = 0;

    strcpy( variables[seraResources].variableName, "SERA_RESOURCES" );
    strcpy( variables[seraResources].shouldBe, "$SERA_HOME/Resources" );
    strcpy( variables[seraResources].required, "/sera1/Resources" );
    variables[seraResources].valid = 0;

    strcpy( variables[seraHelp].variableName, "SERA_HELP" );
    strcpy( variables[seraHelp].shouldBe, "$SERA_HOME/Docs/dialog_help" );
    strcpy( variables[seraHelp].required, "/sera1/Docs/dialog_help" );
    variables[seraHelp].valid = 0;

    strcpy( variables[filesManualPath].variableName, "FILES_MANUAL_PATH" );
    strcpy( variables[filesManualPath].shouldBe, "$SERA_HOME/Docs/Manuals/sera1/" );
    strcpy( variables[filesManualPath].required, "/sera1/Docs/Manuals/sera1/" );
    variables[filesManualPath].valid = 0;

    strcpy( variables[netManualPath].variableName, "NET_MANUAL_PATH" );
    strcpy( variables[netManualPath].shouldBe, "http://www.cs.montana.edu/~bnct/manual/" );
    strcpy( variables[netManualPath].required, "http://www.cs.montana.edu/~bnct/manual/" );
    variables[netManualPath].valid = 0;

    strcpy( variables[seramc].variableName, "SERAMC" );
    strcpy( variables[seramc].shouldBe, "SeraMC" );
    strcpy( variables[seramc].required, "SeraMC" );
    variables[seramc].valid = 0;

    strcpy( variables[seramcPath].variableName, "SERAMC_PATH" );
    strcpy( variables[seramcPath].shouldBe, "$SERA_HOME/$SERAMC" );
    strcpy( variables[seramcPath].required, "/sera1/SeraMC" );
    variables[seramcPath].valid = 0;
}



void printResults( variable_t * variables )
{
    int i;

    fprintf( stderr, "\n!!!  WARNING  !!!\n");
    fprintf( stderr, "\nThe following environment variables have not been set correctly." );
    fprintf( stderr, "\nThese must be set before the SERA modules will run properly.\n\n" );

    fprintf( stderr, "%-17s %-45s %-s\n", "Variable", "Current", "Should Be" );

    for( i = 0; i < NUM_ENVIRONMENT_VARIABLES; i++ )
    {
        if( variables[i].valid == 0 )
        {
            fprintf( stderr, "%-17s %-45s %-s\n",
                     variables[i].variableName,
                     variables[i].actual,
                     variables[i].shouldBe );
        }
    }
}


int validVariable( char * var, variable_t * variable )
{
    char * ptr;
    char i;
    int returnValue = 0;
    int lengthOfActual;
    int lengthOfRequired;

    lengthOfActual   = strlen( var );
    lengthOfRequired = strlen( variable->required );
    
    /* Find the portion of var to compare to what's required */
    if( lengthOfActual >= lengthOfRequired )
    {
        ptr = &var[ lengthOfActual ];

        for( i = 0; i < lengthOfRequired; i++ )
            ptr--;

        /* Compare the two for equality */
        if( strcmp( ptr, variable->required ) == 0 )
            returnValue = 1;
    }
    
    return( returnValue );
}
