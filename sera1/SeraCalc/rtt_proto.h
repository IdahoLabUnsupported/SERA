#ifndef RTT_PROTO_H
#define RTT_PROTO_H

#include "rtt_include.h"


void    rtt_selectCB();
void    rtt_cancelCB();
void    rtt_KillCB(Widget, char *, XmAnyCallbackStruct *);
void    new_rtt_popup();
void    rtt_mc();

void    create_rtt_forms( Widget, RttEditPopupSt * );
void    create_rtt_monitor( void );
void    create_rtt_edit_menus( Widget, Widget );

char *  blank_trim( char * );

void RttSelectCB(Widget, char *, XmFileSelectionBoxCallbackStruct *);
void RttCancelCB(Widget, char *, XmFileSelectionBoxCallbackStruct *);

void rtt_edit_buttonCB(Widget, char *, XmAnyCallbackStruct *);
void change_textCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_directive_add_buttonCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_directive_clear_buttonCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_editdir_buttonCB(Widget, char *, XmAnyCallbackStruct *);

void rtt_excluded_saveCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_excluded_resetCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_excluded_cancelCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_excluded_buttonCB(Widget, char *, XmAnyCallbackStruct *);

void rtt_transbs_buttonCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_transbs_saveCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_transbs_resetCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_transbs_cancelCB(Widget, char *, XmAnyCallbackStruct *);

void rtt_iop_buttonCB ( Widget w, XtPointer clientData, XtPointer callData );
void rtt_iop_changedCB( Widget w, XtPointer clientData, XtPointer callData );
void rtt_iop_plusCB   ( Widget w, XtPointer clientData, XtPointer callData );
void rtt_iop_saveCB   ( Widget w, XtPointer clientData, XtPointer callData );
void rtt_iop_resetCB  ( Widget w, XtPointer clientData, XtPointer callData );
void rtt_iop_cancelCB ( Widget w, XtPointer clientData, XtPointer callData );

void rtt_isotopes_buttonCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_isotopes_saveCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_isotopes_resetCB(Widget, char *, XmAnyCallbackStruct *);
void rtt_isotopes_cancelCB(Widget, char *, XmAnyCallbackStruct *);

void DoseMenuCB(Widget, XtPointer, XmAnyCallbackStruct *);

/* LLV added 5-27-96 for viewer code */

#ifdef DECLARATION
#define externOrNot /* nil */
#else
#define externOrNot extern
#endif  /* DECLARATION */

/** added by CLA **/
void read_and_display(XtPointer, XtIntervalId *);

/* Added by CAW, 5/13/98 */
void RunDirsOnlyCB (Widget, XtPointer, XmTextVerifyCallbackStruct *);
void AddUltraCB (Widget, RttEditPopupSt *, XtPointer);
void nhistCheckCB (Widget, XtPointer, XmTextVerifyCallbackStructWcs *);
void makeAllCapsCB( Widget w, XtPointer clientData, XtPointer callData );
void checkGridCB (Widget, XtPointer, XmTextVerifyCallbackStruct *);


/* Added by MBR, 8/17/99 */
int checkRttFiles      ( void );
int isADirectory       ( char * filename );
int getValueFromTextBox( Widget textbox, char * value, int expand );

#endif
