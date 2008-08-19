/*
 * Main procedure for the bnct_rtpe program
 */
 
#include "seramenu.h" 


int main (int argc, char *argv[])
{

  main_gui_t gui;

  ET_checkEnvironment( ); /* Make sure environment variables have been set */

  fprintf (stderr,"\n");
  fprintf (stderr,"SERA 1B0:  Simulation Environment for Radiotherapy Applications\n");
  fprintf (stderr,"\n");
  fprintf (stderr,"Developed under Government Contract No. DE-AC07-76ID01570 at the\n");
  fprintf (stderr,"INEEL National Engineering and Environmental Laboratory and Montana State University\n");
  fprintf (stderr,"and is subject to Limited Government License. Neither the United\n");
  fprintf (stderr,"States nor the United States Government nor any of their employees\n");
  fprintf (stderr,"or subcontractors, makes any warranty, express or implied, or assumes\n");
  fprintf (stderr,"any legal liability or responsibility for the accuracy. completeness, or\n");
  fprintf (stderr,"usefulness of any information, apparatus, product, or process disclosed,\n");
  fprintf (stderr,"or represents that its use would not infringe privately-owned rights.\n");
  fprintf (stderr,"\n");
   
  initialize_top_level  ( &gui, &argc, argv );
  make_main_menu_widget ( &gui );

  /* Put towards the upper left hand corner */
  XtVaSetValues( gui.toplevel, 
		 XmNx, 25,
		 XmNy, 25, 
		 NULL );

  XtRealizeWidget (gui.toplevel);

   /*
    * Get the current height and width of the Main Menu.
    * Then set the max and min dimensions so that the 
    * Main Menu cannot be resized.
    */

   XtVaGetValues( gui.toplevel, 
		  XmNwidth,   &gui.width,
		  XmNheight,  &gui.height,
		  NULL );
   

   XtVaSetValues( gui.toplevel,
		  XmNminWidth,  (int) gui.width,
		  XmNmaxWidth,  (int) gui.width,
		  XmNminHeight, (int) gui.height,
		  XmNmaxHeight, (int) gui.height,
		  NULL );

   XtAppMainLoop ( gui.app );
   return( 0 );
}


