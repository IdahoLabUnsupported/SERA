/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  File:        check_files.h
%%%
%%%  Purpose:     Allow preliminary checking of .rst files before actual
%%%               processing takes place. Two routines are provided,
%%%               one does the checking of the file, and the other displays
%%%               the results of the check. Both functions use the masks
%%%               defined below.
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#ifndef __CHECK_FILES__H
#define __CHECK_FILES__H

#include <Xm/Xm.h>
/*%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Defines
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*
 * If more (less) masks become needed
 * increment (decrement) NUM_CHECK_MASKS
 * and add (delete) the appropriate mask below.
 * Masks must be unique, and a power of 2.
 */
#define NUM_CHECK_MASKS     9

#define FILE_NO_EXIST_MASK  ((unsigned int) 0x0001)  /* 1    */
#define FORMAT_MASK         ((unsigned int) 0x0002)  /* 2    */
#define PLAN_FILE_MASK      ((unsigned int) 0x0004)  /* 4    */
#define TITLE_MASK          ((unsigned int) 0x0008)  /* 8    */
#define GEOM_FILE_MASK      ((unsigned int) 0x0010)  /* 16   */  
#define UV_FILE_MASK        ((unsigned int) 0x0020)  /* 32   */
#define NO_UVH_MASK         ((unsigned int) 0x0040)  /* 64   */
#define MAT_FILE_MASK       ((unsigned int) 0x0080)  /* 128  */
#define SIGMA_FILE_MASK     ((unsigned int) 0x0100)  /* 256  */


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Prototypes
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    checkFilenamesInRstFile()
%%%
%%%  Purpose:     Do a preliminary check of the filenames in the given
%%%               .rst file. 
%%%
%%%  Parameters:  filename   -> The file to check.
%%%               flags      -> The address of an unsigned int; the results
%%%                             of the check are returned here.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:       The parameters here should be the same supplied to
%%%               displayResultsOfRstCheck().
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void checkFilenamesInRstFile( char * filename,  unsigned int * flags );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    displayResultsOfRstCheck()
%%%
%%%  Purpose:     Display a dialog telling the user the problems with
%%%               their given .rst file. No dialog appears if no bits
%%%               in flags are set.
%%%
%%%  Parameters:  parent    -> A parent widget.
%%%               filename  -> The name of the file checked.
%%%               flags     -> The flags returned from checkFilenamesInRstFile
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void displayResultsOfRstCheck( Widget parent, char * filename, unsigned int flags );


#endif
