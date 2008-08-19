#ifndef SLIDERTEXT_H
#define SLIDERTEXT_H

#include <Xm/Xm.h>

Widget CreateSliderText(Widget * slider, Widget * textbox,
			Widget parent, char * label, int showvalue,
			int num_decimals, int min, int max, int val);
int SetSliderText(Widget slider, Widget textbox, int value);
void VerifyNumericModifiedCallback(Widget, XtPointer, XtPointer);

#endif
