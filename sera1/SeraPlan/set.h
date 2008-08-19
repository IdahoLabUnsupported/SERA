#ifndef SET_H
#define SET_H
extern plan_data   data;
extern panel_data  *panel;

void create_scrolled_panel ( );
void create_fraction ( int );
void create_field ( Widget *, int, int, Widget );
void create_frac_summ ( Widget, summ_struct *, summ_struct *, int );
void SetSlideCallback ( Widget, XtPointer, XtPointer );
void SetTextValue ( Widget, char *, XtPointer );
/*void SetFileTextValue ( Widget, char *, XtPointer );*/
void SetB10Value ( Widget, XtPointer, XtPointer );
void SetExpValue ( Widget, XtPointer, XtPointer );
void SetGammaValue ( Widget, XtPointer, XtPointer );
void b10calc_avg ( int );
void CheckEntry ( Widget, XtPointer, XmTextVerifyCallbackStruct * );
void SetActive ( Widget, XtPointer, XmToggleButtonCallbackStruct * );
void FileSelect ( Widget, XtPointer, XtPointer );
void SetFrac ( Widget, XtPointer, XmToggleButtonCallbackStruct * );

#endif /* ifndef SET_H */
