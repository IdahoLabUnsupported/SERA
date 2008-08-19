/*
 *  Includes
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <X11/Intrinsic.h>
#include <X11/Xutil.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>

#include "debug_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Prototypes
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     CheckEntry
%%%
%%%  Purpose:      Check the characters entered into a text widget
%%%                Only allows digits and decimal points
%%%
%%%  Parameters:   None, besides normal text widget callback arguments
%%%
%%%  Returns:      None
%%%
%%%  Notes:        modifyVerifyCallback for Text or TextField
%%%
%%%  Written By:   Chuck Wemple
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CheckEntry ( Widget, XtPointer, XmTextVerifyCallbackStruct * );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     CheckTextEntry
%%%
%%%  Purpose:      Check the characters entered into a text widget
%%%                Allows everything except space characters, including
%%%                printing and non-printing control characters
%%%
%%%  Parameters:   None, besides normal text widget callback arguments
%%%
%%%  Returns:      None
%%%
%%%  Notes:        modifyVerifyCallback for Text or TextField
%%%
%%%  Written By:   Chuck Wemple
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CheckTextEntry ( Widget, XtPointer, XmTextVerifyCallbackStruct * );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     CheckMultiTextEntry
%%%
%%%  Purpose:      Check the characters entered into a multi-line text widget
%%%                Allows alphanumerics, spaces, and punctuation (no controls)
%%%
%%%  Parameters:   None, besides normal text widget callback arguments
%%%
%%%  Returns:      None
%%%
%%%  Notes:        modifyVerifyCallback for Text only
%%%
%%%  Written By:   Chuck Wemple
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CheckMultiTextEntry ( Widget, XtPointer, XmTextVerifyCallbackStruct * );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     NNumbersOnlyCB
%%%
%%%  Purpose:      Check the characters entered into a text widget
%%%                Permits N space-separated, numeric entries (including
%%%                decimals and exponentials)
%%%
%%%  Parameters:   Integer for number of numeric entries as clientData 
%%%
%%%  Returns:      None
%%%
%%%  Notes:        modifyVerifyCallback for Text or TextField
%%%
%%%  Written By:   Chuck Wemple
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void NNumbersOnlyCB ( Widget, int *, XmTextVerifyCallbackStruct * );

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     integersOnlyCB
%%%
%%%  Purpose:      Check the characters entered into a text widget
%%%                Only allows integer values (no exponential or decimal)
%%%
%%%  Parameters:   None, besides normal text widget callback arguments
%%%
%%%  Returns:      None
%%%
%%%  Notes:        modifyVerifyCallback for Text or TextField
%%%
%%%  Written By:   Chuck Wemple
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void integersOnlyCB ( Widget, XtPointer, XmTextVerifyCallbackStruct * );
