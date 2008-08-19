#include <stdlib.h>
#include <ctype.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include "sliderText.h"

/* These callbacks don't have to be seen anywhere else */
void SetSliderValue(Widget slider, char * textbox_text, XtPointer ClientData);
void SetTextboxValue(Widget textbox, int value, int num_decimals);
void SliderAdjustCB(Widget w, XtPointer ClientData, XtPointer CallData);
void TextEnteredCB(Widget w, XtPointer ClientData, XtPointer CallData);

void SetSliderValue(Widget slider, char * textbox_text,
		    XtPointer ClientData) {
  int new_value, is_negative=0;
  double dbl_value, mult_factor;
  int min, max, val;
  int i;
  short num_decimals;

  XtVaGetValues(slider,
		XmNmaximum, &max,
		XmNminimum, &min,
		XmNvalue, &val,
		XmNdecimalPoints, &num_decimals,
		NULL);

  dbl_value = atof(textbox_text);
  if (dbl_value<0.0) {
    dbl_value = -dbl_value;
    is_negative = 1;
  }
  mult_factor = 1.0;
  for (i=0; i<num_decimals; i++) {
    mult_factor*=10.0;
  }
  new_value = (int)(dbl_value*mult_factor+0.5);
  if (is_negative) {
    new_value = -new_value;
  }
  if (new_value<min) new_value=min;
  if (new_value>max) new_value=max;
  if (new_value==val) return;
  XtVaSetValues(slider,
		XmNvalue, new_value,
		NULL);
  XtCallCallbacks(slider, XmNvalueChangedCallback, ClientData);
}

void SetTextboxValue(Widget textbox, int value, int num_decimals) {
  char textbox_text[256]; /* assumes will fit in 256 chars */
  int i, factor=1;
  char a_digit[2];

  if (num_decimals<=0) {
    sprintf(textbox_text, "%d", value);
  } else /* num_decimals>0 */ {
    for (i=1; i<=num_decimals; i++) {
      factor*=10;
    }
    if (value<0) {
      value = -value;
      sprintf(textbox_text, "-%d.", value/factor);
    } else {
      sprintf(textbox_text, "%d.", value/factor);
    }
    /* Pick off 1 digit at a time for each digit after decimal */
    for (i=1; i<=num_decimals; i++) {
      value -= (value/factor)*factor;
      factor/=10;
      sprintf(a_digit, "%d", value/factor);
      strcat(textbox_text, a_digit);
    }
  }

  /* now, set the textbox */
  XtVaSetValues(textbox,
		XmNvalue, textbox_text,  
		XmNcursorPosition, strlen(textbox_text),
		NULL);
  /*XmTextFieldSetString(textbox, textbox_text);
    XmTextSetCursorPosition(textbox, strlen(textbox_text));*/
}

void SliderAdjustCB(Widget w, XtPointer ClientData, XtPointer CallData) {
  int value;
  short num_decimals;
  Widget textbox;

  textbox = (Widget)ClientData;
  XtVaGetValues(w,
		XmNvalue, &value,
		XmNdecimalPoints, &num_decimals,
		NULL);
  SetTextboxValue(textbox, value, num_decimals);
}

void TextEnteredCB(Widget w, XtPointer ClientData, XtPointer CallData) {
  Widget slider;
  char * textbox_text;

  slider = (Widget)ClientData;  

  textbox_text = XmTextGetString(w);
  
  SetSliderValue(slider, textbox_text, (XtPointer)w);

  XtFree(textbox_text);
}

Widget CreateSliderText(Widget * slider, Widget * textbox,
			Widget parent, char * label, int showvalue,
			int num_decimals, int min, int max, int val) {
  Widget form;
  XmString xms;

  form = XmCreateForm(parent, "form", NULL, 0);

  *slider = XmCreateScale(form, "slider", NULL, 0);
  xms = XmStringCreateLtoR(label, XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues(*slider,
		XmNtitleString, xms,
		XmNshowValue, (Boolean)(showvalue!=0),
		XmNorientation, XmHORIZONTAL,
		XmNminimum, min,
		XmNmaximum, max,
		XmNvalue, val,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_POSITION,
		XmNrightPosition, 75,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNdecimalPoints, num_decimals,
		NULL);
  XtManageChild(*slider);
  XmStringFree(xms);

  *textbox = XmCreateTextField(form, "textbox", NULL, 0);
  XtVaSetValues(*textbox,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, *slider,
		XmNleftOffset, 5,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNtopPosition, 20,
		XmNbottomAttachment, XmATTACH_POSITION,
		XmNbottomPosition, 80,
		NULL);
  SetTextboxValue(*textbox, val, num_decimals);
  XtManageChild(*textbox);

  XtAddCallback(*slider, XmNdragCallback, SliderAdjustCB, (XtPointer)(*textbox));
  XtAddCallback(*slider, XmNvalueChangedCallback, SliderAdjustCB, (XtPointer)(*textbox));
  XtAddCallback(*textbox, XmNactivateCallback, TextEnteredCB, (XtPointer)(*slider));
  XtAddCallback(*textbox, XmNmodifyVerifyCallback, VerifyNumericModifiedCallback, (XtPointer)NULL);

  return(form);
}

/* Here, the value will also depend on the number of decimals */
int SetSliderText(Widget slider, Widget textbox, int value) {
  short num_decimals;
  int min, max, val;
  int retval = 1;

  XtVaGetValues(slider,
		XmNdecimalPoints, &num_decimals,
		XmNminimum, &min,
		XmNmaximum, &max,
		XmNvalue, &val,
		NULL);
  if (val!=value) {
    if (value<min) {
      retval = 0;
      value = min;
    }
    if (value>max) {
      retval = 0;
      value = max;
    }
    if (val!=value) {
      XtVaSetValues(slider,
		    XmNvalue, value,
		    NULL);
    }
  }
  SetTextboxValue(textbox, value, num_decimals);

  return(retval);
}

void VerifyNumericModifiedCallback(Widget w, XtPointer clientData, 
				    XtPointer callData) {
  int has_dot_already = 0;
  XmTextVerifyCallbackStruct *cbs = 
    (XmTextVerifyCallbackStruct *) callData;
  
  if (cbs->text->ptr) {
    char *string = cbs->text->ptr;
    int i;
    
    for (i=0; i<cbs->text->length; i++) {
      if ((!isdigit(string[i]))&&
	  (string[i]!='-')&&
	  (string[i]!='.'))
	{
	  cbs->doit = FALSE;
	  break;
	}
      if (string[i]=='.') has_dot_already = 1;
    }
  }
}
