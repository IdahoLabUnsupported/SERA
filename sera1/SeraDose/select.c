/*
 *  select.c - selects the application with the user
 *          selected dose factors.
 */
#include <Xm/Text.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include "global.h"
#include "picshell.h"
#include <errno.h>
/* #include <unistd.h> */
#include <sys/types.h>


void selectFACTORCB(Widget w, driver_data *data, XmAnyCallbackStruct *call_data);

/* 
 * 
 */
Widget create_select_button(Widget parent, driver_data *data)
{
   Widget     w;

   w = XtCreateManagedWidget("Update Contours",
                      xmPushButtonWidgetClass,
                      parent, NULL, 0);
   XtAddCallback(w, XmNactivateCallback, 
                 (XtCallbackProc)selectFACTORCB, data);
     
   return (w);
}


void selectFACTORCB(Widget w,
                driver_data     *data,
                XmAnyCallbackStruct *call_data)

{
    /* mwf 2-14-96 --> take out load_imageEH and instead call the update_small_images function
     * so that all preview images are updated, if so selected by the user
     */
    /*    load_imageEH(mainWindowDrawingArea, 0, &SureEvent);*/
    update_small_images(image_matrix.active_picture);
}



/* This new function used to be in selectFACTORCB
 * Since there are now multiple images and since the doses are reloaded whenever
 * a new picture is activated, we need to not only call setfactors when the
 * Dose Factor widget is applied, but also every time an image is 'activated' (provided
 * the activated image has a dose loaded).
 * mwf 5/11/96
 */
 
void setfactors(driver_data *data, floyd_data_ptr original_data) 
{
    char *s;
    int   i, error;
    static char  *argv[256];
    static float Factor[] = {1.0,1.0,1.0,1.0,1.0};
    static float Conc[] = {1.0, 1.0, 1.0, 1.0, 1.0};
    static float boron, gamma, nitrogen, fast, other, total;
    static float boronref, gammaref, nitrogenref, fastref, otherref, totalref;
    static float new_factor_conc[5], old_factor_conc[5];


    /*
     * extract input factors from defaultdose widget
     * and put into argv buffer that will be passed to
     * the child.
     */
    for(i = 0; i < data->numFactors - 1; i ++)
    {
	s = XmTextGetString(data->factor[i]);
	sscanf(s, "%f", &Factor[i]);
	CurFactors[i] = Factor[i];
	XtFree(s);
    }	
    for(i = 0; i < data->numConcs - 1; i ++)
    {
	s = XmTextGetString(data->conc[i]);
	sscanf(s, "%f", &Conc[i]);
	CurConcs[i] = Conc[i];
	XtFree(s);
    }	

    for (i = 0; i < data->numFactors - 1; i ++)
    {
	new_factor_conc[i] = Factor[i] * Conc[i];
    }
    old_factor_conc[0] = BoronFactor * BoronConc;
    old_factor_conc[1] = GammaFactor * GammaConc;
    old_factor_conc[2] = NitrogenFactor * NitrogenConc;
    old_factor_conc[3] = FastFactor * FastConc;    
    old_factor_conc[4] = OtherFactor * OtherConc;

    CurRefs[0] = boronref = (old_factor_conc[0] == 0) ? 0 : BoronRef * new_factor_conc[0] / 
	old_factor_conc[0];
    CurRefs[1] = gammaref = (old_factor_conc[1] == 0) ? 0 : GammaRef * new_factor_conc[1] / 
	old_factor_conc[1];
    CurRefs[2] = nitrogenref = (old_factor_conc[2] == 0) ? 0 : NitrogenRef * new_factor_conc[2] / 
	old_factor_conc[2];
    CurRefs[3] = fastref = (old_factor_conc[3] == 0) ? 0 : FastRef * new_factor_conc[3] / 
	old_factor_conc[3];
    CurRefs[4] = otherref = (old_factor_conc[4] == 0) ? 0 : OtherRef * new_factor_conc[4] / 
	old_factor_conc[4];
    totalref = boronref + gammaref + nitrogenref + fastref + otherref;

    for (i = 0; i < NUMPOINTS; i ++)
    {
	boron = (old_factor_conc[0] == 0) ? 0 : original_data[i].boronDose *
 		new_factor_conc[0] / old_factor_conc[0];
	gamma = (old_factor_conc[1] == 0) ? 0 : original_data[i].gammaDose * 
 		new_factor_conc[1] / old_factor_conc[1];
	nitrogen = (old_factor_conc[2] == 0) ? 0 : original_data[i].nitrogenDose * 
 		new_factor_conc[2] / old_factor_conc[2];
	fast = (old_factor_conc[3] == 0) ? 0 : original_data[i].fastDose * 
 		new_factor_conc[3] / old_factor_conc[3];
	other = (old_factor_conc[4] == 0) ? 0 : original_data[4].otherDose * 
 		new_factor_conc[4] / old_factor_conc[4];
	total = boron + gamma + nitrogen + fast + other;
	dose_data[i].totalDose = total * 100 / totalref;  
	/*What is this for?  It sometimes causes crashes -David*/
	/* put back in by MC 6-1-98 */
    }


/*
    BoronFactor = Factor[0];
    GammaFactor = Factor[1];
    NitrogenFactor = Factor[2];
    FastFactor = Factor[3];
    BoronConc = Conc[0];
    GammaConc = Conc[1];
    NitrogenConc = Conc[2];
    FastConc = Conc[3];
*/
}
