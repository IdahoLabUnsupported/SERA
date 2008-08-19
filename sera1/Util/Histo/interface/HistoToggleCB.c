#include "histo.h"

/*******************************************************************************
 * Callbacks for the toggle buttons in do_histo 
 */

void HistoToggleCB ( Widget w, XtPointer clientData, XtPointer callData )
{

   XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
   histo_struct * histo_struct_var = (histo_struct *) clientData;
       
   char * name;            /* Name of the toggle button that was activated */
   int    i;
  
   name = (char *) malloc (strlen(XtName(w))+1);
   strcpy (name, XtName(w));
 
 /* Note: if the toggle buttons' names change in do_histo, they must also change here. */
   if (!strcmp(name, "total dose"))
      i = 0;
   else if (!strcmp(name, "b10 dose"))
      i = 1;
   else if (!strcmp(name, "gamma dose"))
      i = 2;
   else if (!strcmp(name, "n-14 dose"))
      i = 3;
   else if (!strcmp(name, "hydrogen dose"))
      i = 4;
   else if (!strcmp(name, "fast fluence"))
      i = 5;
   else if (!strcmp(name, "thermal fluence"))
      i = 6;
   else if (!strcmp(name, "other dose"))
      i = 7;

   if (cbs->set)
      {
      histo_struct_var->options[i] = 1;
      }
   else
      {
      histo_struct_var->options[i] = 0;
      }
   free ((void*) name);
}
