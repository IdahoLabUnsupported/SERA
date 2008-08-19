/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  environment_tools.h
%%%
%%%  Purpose:  The purpose of environment tools is to make sure that
%%%            all of the environment variables required by SERA have
%%%            been set properly.
%%%
%%%  Usage:    The function ET_checkEnvironment() should be the first
%%%            thing called in a module's "main" program. If any
%%%            incorrect variables are found, they will be reported,
%%%            and a warning will be issued.
%%%
%%%  Written By: Mark Rossmeier
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#ifndef ENVIRONMENT_TOOLS_H
#define ENVIRONMENT_TOOLS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*%%%%%%%%%%%%%%%%%%%%%%
%%% Defines
%%%%%%%%%%%%%%%%%%%%%%%%*/

/* Change this if a new variable is added */
#define NUM_ENVIRONMENT_VARIABLES 7

/* Add a new variable to this list */
enum variables
{
    seraHome,
    seraResources,
    seraHelp,
    filesManualPath,
    netManualPath,
    seramc,
    seramcPath
};


typedef struct _variable_t
{
    char variableName[256];
    char actual[256];
    char shouldBe[256];
    char required[256];
    int valid;
    
} variable_t;


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Prototypes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void ET_checkEnvironment( void );

#endif
