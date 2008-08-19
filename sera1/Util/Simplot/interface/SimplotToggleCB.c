/*******************************************************************************
 * Callbacks for the toggle buttons in do_simplot 
 */
#include "simplot.h"

void SimplotToggleCB ( Widget w, XtPointer clientData, XtPointer callData )
{
   XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
   simplot_struct * simplot_struct_var = (simplot_struct *) clientData;

   char * name;            /* Name of the toggle button that was activated */
   int    i;
  
   name = (char *) MT_malloc (strlen(XtName(w))+1);
   strcpy (name, XtName(w));
 
   /* Note: if the toggle buttons' names change in do_simplot, they must also change here. */
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
   else if (!strcmp(name, "other dose"))
      i = 5;
   else if (!strcmp(name, "Gp 1 fluence"))
      i = 6;
   else if (!strcmp(name, "Gp 2 fluence"))
      i = 7;
   else if (!strcmp(name, "Thermal fluence"))
      i = 8;
   else if (!strcmp(name, "Gamma production"))
      i = 9;
   else if (!strcmp(name, "Ultrafast Gamma"))
      i = 10;
   else if (!strcmp(name, "Reaction Rate 1"))
      i = 11;
   else if (!strcmp(name, "Reaction Rate 2"))
      i = 12;

   if (cbs->set)
      {
      simplot_struct_var->options[i] = 1;
      }
   else
      {
      simplot_struct_var->options[i] = 0;
      }

   MT_free ( (void *) name);
}
