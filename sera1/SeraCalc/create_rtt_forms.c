#include "rtt.h"
#include <time.h>

static int ONE=1, THREE=3;

Widget	rtt_edit_mon_form;

/* Functions found in rtt_mc.c */
extern void buttonPressedEH ( Widget w, XtPointer clientData, XEvent * event, Boolean * flag );
extern void buttonReleasedEH( Widget w, XtPointer clientData, XEvent * event, Boolean * flag );

void create_rtt_option_strings(void);
void create_rtt_editpu_buttons(Widget, RttEditPopupSt *);
char * allocate_rtt_option(char *);

void
create_rtt_forms(Widget local_form, RttEditPopupSt * data)
{
    int    i;
    int    len_token;
    Arg    al[20];
    int    ac;

    Widget above_text_widget;

    Widget rtt_status_form;
    Widget rtt_status_label;
    Widget rtt_edit_form2;
    Widget rtt_edit_form3;
    Widget rtt_edit_frame1;
    Widget rtt_title_form1;
    Widget rtt_title_label;
    Widget rtt_edit_frame2;
    Widget rtt_edit_fileform;
    Widget rtt_file_frame1;
    Widget rtt_file_label1;

    Widget rtt_edit_frame3;
    Widget rtt_edit_controlform;
    Widget rtt_control_frame1;
    Widget rtt_control_label1;
    Widget rtt_control_form1;
    Widget rtt_ctl_text_label1;
    Widget rtt_ctl_text_label2;
    Widget rtt_ctl_text_label3;
    Widget rtt_ctl_text_label4;
    Widget rtt_edit_frame4;

    Widget rtt_edit_positionform;
    Widget rtt_position_frame1;
    Widget rtt_position_label1;
    Widget rtt_position_form1;
    Widget rtt_pos_text_label1;
    Widget rtt_pos_text_label2;
    Widget rtt_pos_text_label3;
    Widget rtt_pos_text_label4;

    Widget rtt_edit_frame5;
    Widget rtt_edit_tallyform;
    Widget rtt_tally_frame1;
    Widget rtt_tally_label1;
    Widget rtt_tally_form1;
    Widget rtt_tal_text_label1;
    Widget rtt_tal_text_label2a;
    Widget rtt_tal_text_label2;
    Widget rtt_tal_text_label3;
    Widget rtt_edit_frame7;
    Widget rtt_edit_preprocrc;

    Widget rtt_iop_button;
    Widget rtt_iop_form;
    Widget rtt_iop_radiobox;
    Widget rtt_iop_label1;
    Widget rtt_iop_label2;
    Widget rtt_iop_label3;
    Widget rtt_iop_label4;
    Widget rtt_iop_label5;
    Widget rtt_iop_save;
    Widget rtt_iop_reset;
    Widget rtt_iop_cancel;

    Widget rtt_isotopes_button;
    Widget rtt_isotopes_label1;
    Widget rtt_isotopes_label1a;
    Widget rtt_isotopes_label2a;
    Widget rtt_isotopes_label3a;
    Widget rtt_isotopes_label4a;
    Widget rtt_isotopes_label5a;
    Widget rtt_isotopes_label6a;
    Widget rtt_isotopes_label7a;
    Widget rtt_isotopes_form;
    Widget rtt_isotopes_save;
    Widget rtt_isotopes_reset;
    Widget rtt_isotopes_cancel;

    Widget rtt_directive_buttonrc;
    Widget rtt_directive_add_button;
    Widget rtt_directive_clear_button;
    Widget rtt_directive_frame1;
    Widget rtt_directive_form1;
    Widget rtt_directive_label1;

    Widget rtt_editpu_rc;

    char   tod_date[12];
    time_t timer;

    DEBUG_TRACE_IN printf("Entering create_rtt_forms\n");
    
    above_text_widget = NULL;
    data->button[data->numFields] = NULL;

    /* create the file selection widget to be realized later */
    ac = 0;
    XtSetArg(al[ac],XmNdirMask, XmStringCreateLtoR("*.input",
                                                   XmSTRING_DEFAULT_CHARSET)); ac++;
    data->rtt_file_select = 
        XmCreateFileSelectionDialog(rtt_top, "rttfileselect", al, ac);
    XtAddCallback(data->rtt_file_select, XmNokCallback, 
                  (XtCallbackProc)RttSelectCB, data->fname_in);
    XtAddCallback(data->rtt_file_select, XmNcancelCallback, 
                  (XtCallbackProc)RttCancelCB, data->fname_in);


    /* Create the MenuBar for the rtt_edit popup */
    ac = 0;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
    rtt_edit_MenuBar = XmCreateMenuBar(local_form,
                                       "rtteditMenuBar", al, ac);
    XtManageChild(rtt_edit_MenuBar);

    /* Create the form widget that manages status and title  */
    rtt_edit_form3 = XtVaCreateManagedWidget("rtteditform3",
                                             xmFormWidgetClass, local_form,
                                             XmNtopAttachment, XmATTACH_WIDGET,
                                             XmNtopWidget, rtt_edit_MenuBar,
                                             XmNleftAttachment, XmATTACH_FORM,
                                             XmNrightAttachment, XmATTACH_FORM,
                                             XmNbottomAttachment, XmATTACH_NONE,
                                             NULL);

    rtt_edit_frame1 = XtVaCreateManagedWidget("rtteditframe1",
                                              xmFrameWidgetClass, rtt_edit_form3,
                                              XmNtopAttachment, XmATTACH_FORM,
                                              XmNleftAttachment, XmATTACH_FORM,
                                              XmNrightAttachment, XmATTACH_FORM,
                                              XmNbottomAttachment, XmATTACH_FORM,
                                              NULL);

    rtt_title_form1 = XtVaCreateManagedWidget("rtttitleform1",
                                              xmFormWidgetClass, rtt_edit_frame1,
                                              NULL);

    rtt_title_label = XtVaCreateManagedWidget("rttstatuslabel",
                                              xmLabelWidgetClass, rtt_title_form1,
                                              XmNlabelString,
                                              XmStringCreateLtoR("Run Title:",
                                                                 XmSTRING_DEFAULT_CHARSET),
                                              XmNalignment, XmALIGNMENT_END,
                                              XmNtopAttachment, XmATTACH_FORM,
                                              XmNtopOffset, 3,
                                              XmNleftAttachment, XmATTACH_FORM,
                                              XmNrightAttachment, XmATTACH_POSITION,
                                              XmNrightPosition, 20,
                                              XmNbottomAttachment, XmATTACH_FORM,
                                              NULL);

    data->rtt_title_text = XtVaCreateManagedWidget("rttedittitle",
                                                   xmTextWidgetClass, rtt_title_form1,
                                                   XmNtopAttachment, XmATTACH_FORM,
                                                   XmNleftAttachment, XmATTACH_WIDGET,
                                                   XmNleftWidget, rtt_title_label,
                                                   XmNrightAttachment, XmATTACH_FORM,
                                                   XmNbottomAttachment, XmATTACH_FORM,
                                                   XmNvalue,"Descriptive Title For This Run",
                                                   NULL);

    /* Create the form widget that manages the editor forms */
    rtt_edit_form2 = XtVaCreateManagedWidget("rtteditform2",
                                             xmFormWidgetClass, local_form,
                                             XmNtopAttachment, XmATTACH_WIDGET,
                                             XmNtopWidget, rtt_edit_form3,
                                             XmNleftAttachment, XmATTACH_FORM,
                                             XmNrightAttachment, XmATTACH_FORM,
                                             XmNbottomAttachment, XmATTACH_FORM,
                                             NULL);

    rtt_edit_frame2 = XtVaCreateManagedWidget("rtteditframe2",
                                              xmFrameWidgetClass, rtt_edit_form2,
                                              XmNtopAttachment, XmATTACH_FORM,
                                              XmNleftAttachment, XmATTACH_FORM,
                                              XmNrightAttachment, XmATTACH_NONE,
                                              XmNbottomAttachment, XmATTACH_NONE,
                                              NULL);

    rtt_edit_fileform = XtVaCreateManagedWidget("rtteditfileform",
                                                xmFormWidgetClass, rtt_edit_frame2,
                                                NULL);

    rtt_edit_frame3 = XtVaCreateManagedWidget("rtteditframe3",
                                              xmFrameWidgetClass, rtt_edit_form2,
                                              XmNtopAttachment, XmATTACH_FORM,
                                              XmNleftAttachment, XmATTACH_WIDGET,
                                              XmNleftWidget, rtt_edit_frame2,
                                              XmNwidth, 185,
                                              XmNheight, 180,
                                              XmNrightAttachment, XmATTACH_POSITION,
                                              XmNrightPosition, 60,
                                              XmNbottomAttachment, XmATTACH_NONE,
                                              NULL);

    rtt_edit_controlform = XtVaCreateManagedWidget("rtteditcontrolform",
                                                   xmFormWidgetClass, rtt_edit_frame3,
                                                   XmNtopAttachment, XmATTACH_FORM,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_FORM,
                                                   XmNbottomAttachment, XmATTACH_FORM,
                                                   NULL);

    rtt_edit_frame4 = XtVaCreateManagedWidget("rtteditframe4",
                                              xmFrameWidgetClass, rtt_edit_form2,
                                              XmNtopAttachment, XmATTACH_FORM,
                                              XmNleftAttachment, XmATTACH_WIDGET,
                                              XmNleftWidget, rtt_edit_frame3,
                                              XmNwidth, 160,
                                              XmNheight, 180,
                                              XmNrightAttachment, XmATTACH_POSITION,
                                              XmNrightPosition, 80,
                                              XmNbottomAttachment, XmATTACH_NONE,
                                              NULL);

    rtt_edit_positionform = XtVaCreateManagedWidget("rtteditpositionform",
                                                    xmFormWidgetClass, rtt_edit_frame4,
                                                    NULL);

    rtt_edit_frame5 = XtVaCreateManagedWidget("rtteditframe5",
                                              xmFrameWidgetClass, rtt_edit_form2,
                                              XmNtopAttachment, XmATTACH_FORM,
                                              XmNleftAttachment, XmATTACH_WIDGET,
                                              XmNleftWidget, rtt_edit_frame4,
                                              XmNwidth, 160,
                                              XmNheight, 180,
                                              XmNrightAttachment, XmATTACH_FORM,
                                              XmNbottomAttachment, XmATTACH_NONE,
                                              NULL);

    rtt_edit_tallyform = XtVaCreateManagedWidget("rttedittallyform",
                                                 xmFormWidgetClass, rtt_edit_frame5,
                                                 NULL);

    rtt_edit_frame7 = XtVaCreateManagedWidget("rtteditframe7",
                                              xmFrameWidgetClass, rtt_edit_form2,
                                              XmNtopAttachment, XmATTACH_WIDGET,
                                              XmNtopWidget, rtt_edit_frame3,
                                              XmNleftAttachment, XmATTACH_WIDGET,
                                              XmNleftWidget, rtt_edit_frame2,
                                              XmNrightAttachment, XmATTACH_FORM,
                                              XmNbottomAttachment, XmATTACH_NONE,
                                              NULL);

    rtt_edit_preprocrc = XtVaCreateManagedWidget("rtteditpreprocrc",
                                                 xmRowColumnWidgetClass, rtt_edit_frame7,
                                                 XmNorientation, XmHORIZONTAL,
                                                 XmNpacking, XmPACK_COLUMN,
                                                 NULL);


    /** file form **/
    rtt_file_frame1 =  XtVaCreateManagedWidget("rttfileframe1",
                                               xmFrameWidgetClass, rtt_edit_fileform,
                                               XmNtopAttachment, XmATTACH_FORM,
                                               XmNleftAttachment, XmATTACH_FORM,
                                               XmNrightAttachment, XmATTACH_FORM,
                                               XmNbottomAttachment, XmATTACH_NONE,
                                               NULL);

    rtt_file_label1 =  XtVaCreateManagedWidget("rttfilelabel1",
                                               xmLabelWidgetClass, rtt_file_frame1,
                                               XmNlabelString,
                                               XmStringCreateLtoR("Files",XmSTRING_DEFAULT_CHARSET),
                                               NULL);

    data->rtt_file_form1 = XtVaCreateManagedWidget("rttfileform1",
                                                   xmFormWidgetClass, rtt_edit_fileform,
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, rtt_file_frame1,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_NONE,
                                                   XmNbottomAttachment, XmATTACH_NONE,
                                                   NULL);

    for(i=0; i<data->numFields; i++)
    {
        if(above_text_widget == NULL)
        {
            /* do the first button/text widget in the column */
            data->button[i] = XtVaCreateManagedWidget(data->buttons[i],
                                                      xmPushButtonWidgetClass, data->rtt_file_form1,
                                                      XmNtopAttachment, XmATTACH_FORM,
                                                      XmNtopOffset, 4,
                                                      XmNleftAttachment, XmATTACH_FORM,
                                                      XmNrightAttachment, XmATTACH_POSITION,
                                                      XmNrightPosition, 50,
                                                      XmNbottomAttachment, XmATTACH_NONE,
                                                      XmNuserData, i,
                                                      XmNsensitive, True,
                                                      NULL);

            XtAddCallback(data->button[i], XmNactivateCallback, 
                          (XtCallbackProc)rtt_edit_buttonCB, data);

            data->textinfo[i] = XtVaCreateManagedWidget(data->fields[i],
                                                        xmTextWidgetClass, data->rtt_file_form1,
                                                        XmNtopAttachment, XmATTACH_FORM,
                                                        XmNleftAttachment, XmATTACH_FORM,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, data->button[i],
                                                        XmNrightAttachment, XmATTACH_FORM,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNuserData, i,
                                                        XmNsensitive, True,
                                                        NULL);

            XmTextSetString(data->textinfo[i], data->fileName[i] );
            XtAddCallback ( data->textinfo[i], XmNlosingFocusCallback,
                            (XtCallbackProc) change_textCB, data );
     
            above_text_widget = data->textinfo[i];
        }
        else
        {
            /* do the rest of button/text widgets in the column */
            data->button[i] = XtVaCreateManagedWidget(data->buttons[i],
                                                      xmPushButtonWidgetClass, data->rtt_file_form1,
                                                      XmNtopAttachment, XmATTACH_WIDGET,
                                                      XmNtopWidget, above_text_widget,
                                                      XmNtopOffset, 4,
                                                      XmNleftAttachment, XmATTACH_FORM,
                                                      XmNleftAttachment, XmATTACH_FORM,
                                                      XmNrightAttachment, XmATTACH_POSITION,
                                                      XmNrightPosition, 50,
                                                      XmNbottomAttachment, XmATTACH_NONE,
                                                      XmNuserData, i,
                                                      XmNsensitive, True,
                                                      NULL);

            XtAddCallback(data->button[i], XmNactivateCallback, 
                          (XtCallbackProc) rtt_edit_buttonCB, data);

            data->textinfo[i] = XtVaCreateManagedWidget(data->fields[i],
                                                        xmTextWidgetClass, data->rtt_file_form1,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, above_text_widget,
                                                        XmNleftAttachment, XmATTACH_FORM,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, data->button[i],
                                                        XmNrightAttachment, XmATTACH_FORM,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNuserData, i,
                                                        XmNsensitive, True,
                                                        NULL);

            XmTextSetString(data->textinfo[i], data->fileName[i] );
            XtAddCallback ( data->textinfo[i], XmNlosingFocusCallback,
                            (XtCallbackProc) change_textCB, data );
            
            
            above_text_widget = data->textinfo[i];
     
        };

        /* If button 3 is pressed over one of the buttons, the full text value will be displayed. */
        XtAddEventHandler( data->button[i], ButtonPressMask,   False, buttonPressedEH,  (XtPointer) i );
        XtAddEventHandler( data->button[i], ButtonReleaseMask, False, buttonReleasedEH, (XtPointer) i );

        /* Don't allow spaces! Added 10-01-99 MBR 
           Changed to CheckTextEntry 12-17-99 CAW */
        XtAddCallback( data->textinfo[i], XmNmodifyVerifyCallback, (XtCallbackProc) CheckTextEntry,
                       NULL );
        
    }; /* end of for on numFields */

    /* Add label and text widget for rtt running status */

    rtt_status_form = XtVaCreateManagedWidget("rttstatusform",
                                              xmFormWidgetClass, rtt_edit_form2,
                                              XmNtopAttachment, XmATTACH_WIDGET,
                                              XmNtopWidget, rtt_edit_frame2,
                                              XmNleftAttachment, XmATTACH_FORM,
                                              XmNrightAttachment, XmATTACH_NONE,
                                              XmNbottomAttachment, XmATTACH_NONE,
                                              NULL);

    rtt_status_label = XtVaCreateManagedWidget("rttstatuslabel",
                                               xmLabelWidgetClass, rtt_status_form,
                                               XmNlabelString,
                                               XmStringCreateLtoR("seraMC Status:",
                                                                  XmSTRING_DEFAULT_CHARSET),
                                               XmNalignment, XmALIGNMENT_END,
                                               XmNtopAttachment, XmATTACH_FORM,
                                               XmNleftAttachment, XmATTACH_FORM,
                                               XmNrightAttachment, XmATTACH_NONE,
                                               XmNbottomAttachment, XmATTACH_NONE,
                                               NULL);

    data->rtt_status_text = XtVaCreateManagedWidget("rttstatustext",
                                                    xmLabelWidgetClass, rtt_status_form,
                                                    XmNlabelString,
                                                    XmStringCreateLtoR("NOT RUNNING",
                                                                       XmSTRING_DEFAULT_CHARSET),
                                                    XmNalignment, XmALIGNMENT_BEGINNING,
                                                    XmNtopAttachment, XmATTACH_FORM,
                                                    XmNleftAttachment, XmATTACH_WIDGET,
                                                    XmNleftWidget, rtt_status_label,
                                                    XmNrightAttachment, XmATTACH_FORM,
                                                    XmNbottomAttachment, XmATTACH_NONE,
                                                    NULL);

    rtt_edit_mon_form = XtVaCreateManagedWidget("rtt_edit_mon_form",
                                                xmFormWidgetClass, rtt_edit_form2,
                                                XmNtopAttachment, XmATTACH_WIDGET,
                                                XmNtopWidget, rtt_status_form,
                                                XmNleftAttachment, XmATTACH_FORM,
                                                XmNrightAttachment, XmATTACH_NONE,
                                                XmNbottomAttachment, XmATTACH_NONE,
                                                NULL);

    /** control form **/
 
    rtt_control_frame1 =  XtVaCreateManagedWidget("rttcontrolframe1",
                                                  xmFrameWidgetClass, rtt_edit_controlform,
                                                  XmNtopAttachment, XmATTACH_FORM,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_FORM,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  NULL);

    rtt_control_label1 =  XtVaCreateManagedWidget("rttcontrollabel1",
                                                  xmLabelWidgetClass, rtt_control_frame1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("Control",XmSTRING_DEFAULT_CHARSET),
                                                  NULL);

    rtt_control_form1 = XtVaCreateManagedWidget("rttcontrolrowcol1",
                                                xmFormWidgetClass, rtt_edit_controlform,
                                                XmNtopAttachment, XmATTACH_WIDGET,
                                                XmNtopWidget, rtt_control_frame1,
                                                XmNleftAttachment, XmATTACH_FORM,
                                                XmNrightAttachment, XmATTACH_FORM,
                                                XmNbottomAttachment, XmATTACH_FORM,
                                                NULL);

    rtt_ctl_text_label1 = XtVaCreateManagedWidget("rttctltextlabel1",
                                                  xmLabelWidgetClass, rtt_control_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("nbatch",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_FORM,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 40,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_control_text1 = XtVaCreateManagedWidget("rttcontroltext1",
                                                      xmTextWidgetClass, rtt_control_form1,
                                                      XmNtopAttachment, XmATTACH_FORM,
                                                      XmNleftAttachment, XmATTACH_WIDGET,
                                                      XmNleftWidget, rtt_ctl_text_label1,
                                                      XmNrightAttachment, XmATTACH_FORM,
                                                      XmNbottomAttachment, XmATTACH_NONE,
                                                      XmNvalue,"50",
                                                      NULL);
    XtAddCallback ( data->rtt_control_text1, XmNmodifyVerifyCallback, (XtCallbackProc)
                    integersOnlyCB, NULL );

    rtt_ctl_text_label2 = XtVaCreateManagedWidget("rttctltextlabel2",
                                                  xmLabelWidgetClass, rtt_control_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("nhist",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_control_text1,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 40,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_control_text2 = XtVaCreateManagedWidget("rttcontroltext2",
                                                      xmTextWidgetClass, rtt_control_form1,
                                                      XmNtopAttachment, XmATTACH_WIDGET,
                                                      XmNtopWidget, data->rtt_control_text1,
                                                      XmNleftAttachment, XmATTACH_WIDGET,
                                                      XmNleftWidget, rtt_ctl_text_label2,
                                                      XmNrightAttachment, XmATTACH_FORM,
                                                      XmNbottomAttachment, XmATTACH_NONE,
                                                      XmNvalue,"100",
                                                      NULL);
    XtAddCallback ( data->rtt_control_text2, XmNmodifyVerifyCallback, (XtCallbackProc)
                    integersOnlyCB, NULL );
    XtAddCallback ( data->rtt_control_text2, XmNmodifyVerifyCallbackWcs, (XtCallbackProc)
                    nhistCheckCB, NULL );

    rtt_ctl_text_label3 = XtVaCreateManagedWidget("rttctltextlabel3",
                                                  xmLabelWidgetClass, rtt_control_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("run dir",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_control_text2,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 40,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_control_text3 = XtVaCreateManagedWidget("rttcontroltext3",
                                                      xmTextWidgetClass, rtt_control_form1,
                                                      XmNtopAttachment, XmATTACH_WIDGET,
                                                      XmNtopWidget, data->rtt_control_text2,
                                                      XmNleftAttachment, XmATTACH_WIDGET,
                                                      XmNleftWidget, rtt_ctl_text_label3,
                                                      XmNrightAttachment, XmATTACH_FORM,
                                                      XmNbottomAttachment, XmATTACH_NONE,
                                                      XmNvalue,"NFGD",
                                                      NULL);
    XtAddCallback ( data->rtt_control_text3, XmNmodifyVerifyCallback, (XtCallbackProc) RunDirsOnlyCB,
                    NULL );
    XtAddCallback ( data->rtt_control_text3, XmNactivateCallback, (XtCallbackProc) AddUltraCB,
                    data );
    XtAddCallback ( data->rtt_control_text3, XmNlosingFocusCallback, (XtCallbackProc) AddUltraCB,
                    data );
    XtAddCallback ( data->rtt_control_text3, XmNvalueChangedCallback, (XtCallbackProc) AddUltraCB,
                    data );

    rtt_ctl_text_label4 = XtVaCreateManagedWidget("rttctltextlabel4",
                                                  xmLabelWidgetClass, rtt_control_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("run date",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_control_text3,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 40,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    timer = time ( NULL );
    (void) strftime ( tod_date, 12, "%d_%b_%Y", localtime ( &timer ) );
    data->rtt_control_text4 = XtVaCreateManagedWidget("rttcontroltext4",
                                                      xmTextWidgetClass, rtt_control_form1,
                                                      XmNtopAttachment, XmATTACH_WIDGET,
                                                      XmNtopWidget, data->rtt_control_text3,
                                                      XmNleftAttachment, XmATTACH_WIDGET,
                                                      XmNleftWidget, rtt_ctl_text_label4,
                                                      XmNrightAttachment, XmATTACH_FORM,
                                                      XmNbottomAttachment, XmATTACH_NONE,
                                                      XmNvalue, tod_date,
                                                      NULL);

    /** position form **/
 
    rtt_position_frame1 =  XtVaCreateManagedWidget("rttpositionframe1",
                                                   xmFrameWidgetClass, rtt_edit_positionform,
                                                   XmNtopAttachment, XmATTACH_FORM,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_FORM,
                                                   XmNbottomAttachment, XmATTACH_NONE,
                                                   NULL);

    rtt_position_label1 =  XtVaCreateManagedWidget("rttpositionlabel1",
                                                   xmLabelWidgetClass, rtt_position_frame1,
                                                   XmNlabelString,
                                                   XmStringCreateLtoR("Positioning",XmSTRING_DEFAULT_CHARSET),
                                                   NULL);

    rtt_position_form1 = XtVaCreateManagedWidget("rttpositionform1",
                                                 xmFormWidgetClass, rtt_edit_positionform,
                                                 XmNtopAttachment, XmATTACH_WIDGET,
                                                 XmNtopWidget, rtt_position_frame1,
                                                 XmNleftAttachment, XmATTACH_FORM,
                                                 XmNrightAttachment, XmATTACH_FORM,
                                                 XmNbottomAttachment, XmATTACH_FORM,
                                                 NULL);

    rtt_pos_text_label1 = XtVaCreateManagedWidget("rttpostextlabel1",
                                                  xmLabelWidgetClass, rtt_position_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("T",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_FORM,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 30,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_position_text1 = XtVaCreateManagedWidget("rttpositiontext1",
                                                       xmTextWidgetClass, rtt_position_form1,
                                                       XmNtopAttachment, XmATTACH_FORM,
                                                       XmNleftAttachment, XmATTACH_WIDGET,
                                                       XmNleftWidget, rtt_pos_text_label1,
                                                       XmNrightAttachment, XmATTACH_FORM,
                                                       XmNbottomAttachment, XmATTACH_NONE,
                                                       XmNvalue,"0.0 0.0 0.0",
                                                       NULL);
    XtAddCallback ( data->rtt_position_text1, XmNmodifyVerifyCallback,
                    (XtCallbackProc) NNumbersOnlyCB, &THREE );

    rtt_pos_text_label2 = XtVaCreateManagedWidget("rttpostextlabel2",
                                                  xmLabelWidgetClass, rtt_position_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("Zb",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_position_text1,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 30,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_position_text2 = XtVaCreateManagedWidget("rttpositiontext2",
                                                       xmTextWidgetClass, rtt_position_form1,
                                                       XmNtopAttachment, XmATTACH_WIDGET,
                                                       XmNtopWidget, data->rtt_position_text1,
                                                       XmNleftAttachment, XmATTACH_WIDGET,
                                                       XmNleftWidget, rtt_pos_text_label2,
                                                       XmNrightAttachment, XmATTACH_FORM,
                                                       XmNbottomAttachment, XmATTACH_NONE,
                                                       XmNvalue,"0",
                                                       NULL);
    XtAddCallback ( data->rtt_position_text2, XmNmodifyVerifyCallback, (XtCallbackProc)
                    CheckEntry, NULL );

    rtt_pos_text_label3 = XtVaCreateManagedWidget("rttpostextlabel3",
                                                  xmLabelWidgetClass, rtt_position_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("phi",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_position_text2,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 30,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_position_text3 = XtVaCreateManagedWidget("rttpositiontext3",
                                                       xmTextWidgetClass, rtt_position_form1,
                                                       XmNtopAttachment, XmATTACH_WIDGET,
                                                       XmNtopWidget, data->rtt_position_text2,
                                                       XmNleftAttachment, XmATTACH_WIDGET,
                                                       XmNleftWidget, rtt_pos_text_label3,
                                                       XmNrightAttachment, XmATTACH_FORM,
                                                       XmNbottomAttachment, XmATTACH_NONE,
                                                       XmNvalue,"0",
                                                       NULL);
    XtAddCallback ( data->rtt_position_text3, XmNmodifyVerifyCallback, (XtCallbackProc)
                    CheckEntry, NULL );

    rtt_pos_text_label4 = XtVaCreateManagedWidget("rttpostextlabel4",
                                                  xmLabelWidgetClass, rtt_position_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("theta",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_position_text3,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 30,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_position_text4 = XtVaCreateManagedWidget("rttpositiontext4",
                                                       xmTextWidgetClass, rtt_position_form1,
                                                       XmNtopAttachment, XmATTACH_WIDGET,
                                                       XmNtopWidget, data->rtt_position_text3,
                                                       XmNleftAttachment, XmATTACH_WIDGET,
                                                       XmNleftWidget, rtt_pos_text_label4,
                                                       XmNrightAttachment, XmATTACH_FORM,
                                                       XmNbottomAttachment, XmATTACH_NONE,
                                                       XmNvalue,"0",
                                                       NULL);
    XtAddCallback ( data->rtt_position_text4, XmNmodifyVerifyCallback, (XtCallbackProc)
                    CheckEntry, NULL );
    
    /** tally form **/
 
    rtt_tally_frame1 =  XtVaCreateManagedWidget("rtttallyframe1",
                                                xmFrameWidgetClass, rtt_edit_tallyform,
                                                XmNtopAttachment, XmATTACH_FORM,
                                                XmNleftAttachment, XmATTACH_FORM,
                                                XmNrightAttachment, XmATTACH_FORM,
                                                XmNbottomAttachment, XmATTACH_NONE,
                                                NULL);

    rtt_tally_label1 =  XtVaCreateManagedWidget("rtttallylabel1",
                                                xmLabelWidgetClass, rtt_tally_frame1,
                                                XmNlabelString,
                                                XmStringCreateLtoR("Tally",XmSTRING_DEFAULT_CHARSET),
                                                NULL);

    rtt_tally_form1 = XtVaCreateManagedWidget("rtttallyrowcol1",
                                              xmFormWidgetClass, rtt_edit_tallyform,
                                              XmNtopAttachment, XmATTACH_WIDGET,
                                              XmNtopWidget, rtt_tally_frame1,
                                              XmNleftAttachment, XmATTACH_FORM,
                                              XmNrightAttachment, XmATTACH_FORM,
                                              XmNbottomAttachment, XmATTACH_FORM,
                                              NULL);

    rtt_tal_text_label1 = XtVaCreateManagedWidget("rtttaltextlabel1",
                                                  xmLabelWidgetClass, rtt_tally_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("origin",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_FORM,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 40,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_tally_text1 = XtVaCreateManagedWidget("rtttallytext1",
                                                    xmTextWidgetClass, rtt_tally_form1,
                                                    XmNtopAttachment, XmATTACH_FORM,
                                                    XmNleftAttachment, XmATTACH_WIDGET,
                                                    XmNleftWidget, rtt_tal_text_label1,
                                                    XmNrightAttachment, XmATTACH_FORM,
                                                    XmNbottomAttachment, XmATTACH_NONE,
                                                    XmNvalue,"-15 -15 -15",
                                                    NULL);
    XtAddCallback ( data->rtt_tally_text1, XmNmodifyVerifyCallback, (XtCallbackProc)
                    NNumbersOnlyCB, &THREE );

    rtt_tal_text_label2a = XtVaCreateManagedWidget("rtt_tal_text_label2a",
                                                  xmLabelWidgetClass, rtt_tally_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("nedit2",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_tally_text1,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 40,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_tally_text2a = XtVaCreateManagedWidget("rtttallytext2a",
                                                    xmTextWidgetClass, rtt_tally_form1,
                                                    XmNtopAttachment, XmATTACH_WIDGET,
                                                    XmNtopWidget, data->rtt_tally_text1,
                                                    XmNleftAttachment, XmATTACH_WIDGET,
                                                    XmNleftWidget, rtt_tal_text_label2a,
                                                    XmNrightAttachment, XmATTACH_FORM,
                                                    XmNbottomAttachment, XmATTACH_NONE,
                                                    XmNvalue,"30",
                                                    NULL);
    XtAddCallback ( data->rtt_tally_text2a, XmNmodifyVerifyCallback, (XtCallbackProc)
                    CheckEntry, NULL );

    rtt_tal_text_label2 = XtVaCreateManagedWidget("rtt_tal_text_label2",
                                                  xmLabelWidgetClass, rtt_tally_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("delw",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_tally_text2a,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 40,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_tally_text2 = XtVaCreateManagedWidget("rtttallytext2",
                                                    xmTextWidgetClass, rtt_tally_form1,
                                                    XmNtopAttachment, XmATTACH_WIDGET,
                                                    XmNtopWidget, data->rtt_tally_text2a,
                                                    XmNleftAttachment, XmATTACH_WIDGET,
                                                    XmNleftWidget, rtt_tal_text_label2,
                                                    XmNrightAttachment, XmATTACH_FORM,
                                                    XmNbottomAttachment, XmATTACH_NONE,
                                                    XmNvalue,"1.0",
                                                    NULL);
    XtAddCallback ( data->rtt_tally_text2, XmNmodifyVerifyCallback, (XtCallbackProc)
                    CheckEntry, NULL );
    XtAddCallback ( data->rtt_tally_text2, XmNlosingFocusCallback, (XtCallbackProc) checkGridCB,
                    NULL );

    rtt_tal_text_label3 = XtVaCreateManagedWidget("rtt_tal_text_label3",
                                                  xmLabelWidgetClass, rtt_tally_form1,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("wncut",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_tally_text2,
                                                  XmNtopOffset, 7,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 40,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    data->rtt_tally_text3 = XtVaCreateManagedWidget("rtttallytext3",
                                                    xmTextWidgetClass, rtt_tally_form1,
                                                    XmNtopAttachment, XmATTACH_WIDGET,
                                                    XmNtopWidget, data->rtt_tally_text2,
                                                    XmNleftAttachment, XmATTACH_WIDGET,
                                                    XmNleftWidget, rtt_tal_text_label3,
                                                    XmNrightAttachment, XmATTACH_FORM,
                                                    XmNbottomAttachment, XmATTACH_NONE,
                                                    XmNvalue,"0.01",
                                                    NULL);
    XtAddCallback ( data->rtt_tally_text3, XmNmodifyVerifyCallback, (XtCallbackProc)
                    CheckEntry, NULL );

    /* pre-processing area */

    rtt_iop_button =  XtVaCreateManagedWidget("iop",
                                              xmPushButtonWidgetClass, rtt_edit_preprocrc,
                                              XmNalignment, XmALIGNMENT_CENTER,
                                              NULL);

    XtAddCallback(rtt_iop_button, XmNactivateCallback, 
                  (XtCallbackProc) rtt_iop_buttonCB, (XtPointer) data);

    rtt_isotopes_button =  XtVaCreateManagedWidget("isotopes",
                                                   xmPushButtonWidgetClass, rtt_edit_preprocrc,
                                                   XmNalignment, XmALIGNMENT_CENTER,
                                                   NULL);

    XtAddCallback(rtt_isotopes_button, XmNactivateCallback, 
                  (XtCallbackProc) rtt_isotopes_buttonCB, data);

    /** edit directives form **/
 
    rtt_directive_frame1 =  XtVaCreateManagedWidget("rttdirectiveframe1",
                                                    xmFrameWidgetClass, rtt_edit_form2,
                                                    XmNtopAttachment, XmATTACH_WIDGET,
                                                    XmNtopWidget, rtt_edit_frame7,
                                                    XmNleftAttachment, XmATTACH_WIDGET,
                                                    XmNleftWidget, rtt_edit_frame2,
                                                    XmNrightAttachment, XmATTACH_FORM,
                                                    XmNbottomAttachment, XmATTACH_FORM,
                                                    NULL);

    rtt_directive_form1 =  XtVaCreateManagedWidget("rttdirectiveform1",
                                                   xmFormWidgetClass, rtt_directive_frame1,
                                                   XmNtopAttachment, XmATTACH_FORM,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_FORM,
                                                   XmNbottomAttachment, XmATTACH_FORM,
                                                   NULL);

    ac = 0;
    XtSetArg(al[ac], XmNdefaultPosition, False); ac++;
    XtSetArg(al[ac], XmNx, 580); ac++;
    XtSetArg(al[ac], XmNy, 660); ac++;
    rtt_editpu_popup = XtCreatePopupShell("edit-directives", 
                                          topLevelShellWidgetClass, rtt_top, al, ac);

    rtt_editpu_rc = XtVaCreateManagedWidget("rtteditpurc",
                                            xmRowColumnWidgetClass, rtt_editpu_popup, 
                                            XmNpacking, XmPACK_COLUMN, 
                                            XmNnumColumns, 5,
                                            NULL);


    rtt_directive_label1 =  XtVaCreateManagedWidget("rttdirectivelabel1",
                                                    xmLabelWidgetClass, rtt_directive_form1,
                                                    XmNtopAttachment, XmATTACH_FORM,
                                                    XmNtopOffset, 7,
                                                    XmNleftAttachment, XmATTACH_FORM,
                                                    XmNrightAttachment, XmATTACH_NONE,
                                                    XmNbottomAttachment, XmATTACH_NONE,
                                                    XmNlabelString,
                                                    XmStringCreateLtoR("Edit Directives",XmSTRING_DEFAULT_CHARSET),
                                                    NULL);

    rtt_directive_buttonrc = XtVaCreateManagedWidget("rttdirectivebuttonrc",
                                                     xmRowColumnWidgetClass, rtt_directive_form1,
                                                     XmNorientation, XmHORIZONTAL,
                                                     XmNpacking, XmPACK_COLUMN,
                                                     XmNtopAttachment, XmATTACH_FORM,
                                                     XmNleftAttachment, XmATTACH_WIDGET,
                                                     XmNleftWidget, rtt_directive_label1,
                                                     XmNrightAttachment, XmATTACH_NONE,
                                                     XmNbottomAttachment, XmATTACH_NONE,
                                                     NULL);

    /* add edit directives popup button */
    rtt_directive_add_button= XtVaCreateManagedWidget("rtteditbutton",
                                                      xmPushButtonWidgetClass, rtt_directive_buttonrc,
                                                      XmNlabelString, XmStringCreateSimple("Add"),
                                                      NULL);


    rtt_directive_clear_button = XtVaCreateManagedWidget("rttdirectiveclearbutton",
                                                         xmPushButtonWidgetClass, rtt_directive_buttonrc,
                                                         XmNlabelString, XmStringCreateSimple("Clear"),
                                                         NULL);

    ac=0;
    XtSetArg(al[ac], XmNeditMode, XmMULTI_LINE_EDIT); ac++;
    data->rtt_directive_text1 = XmCreateScrolledText(rtt_directive_form1,
                                                     "rttdirectivetext1", al, ac);
    XtManageChild(data->rtt_directive_text1);

    ac = 0;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNtopWidget, rtt_directive_buttonrc); ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
    XtSetValues(XtParent(data->rtt_directive_text1), al, ac);

    XtAddCallback(rtt_directive_add_button, XmNactivateCallback, 
                  (XtCallbackProc) rtt_directive_add_buttonCB, data);

    XtAddCallback(rtt_directive_clear_button, XmNactivateCallback, 
                  (XtCallbackProc) rtt_directive_clear_buttonCB, data);

    create_rtt_option_strings();
    create_rtt_editpu_buttons(rtt_editpu_rc, data);
  
    ac = 0;
    XtSetArg(al[ac], XmNuserData, data->rtt_directive_text1); ac++;
    XtSetValues(XtParent(rtt_directive_add_button), al, ac);

    /* Set up the FileSelectDialog widget with callback to rtt_selectCB which
       sets up file name string */

    ac=0;
    XtSetArg(al[ac], XmNselectionLabelString,
             XmStringCreateLtoR("Enter file name. ",
                                XmSTRING_DEFAULT_CHARSET) ); ac++;
    XtSetArg(al[ac], XmNsensitive, True); ac++;
    data->select_file = XmCreateFileSelectionDialog(local_form, "rtt_select",
                                                    al, ac);
    XtAddCallback (data->select_file, XmNokCallback, rtt_selectCB, data);
    XtAddCallback (data->select_file, XmNcancelCallback, rtt_cancelCB, data);
    /* LYNN is adding this to get the help to work on the rtt FileSelectDialog widget */
    /*XtAddCallback (data->select_file, XmNhelpCallback, 
      (XtCallbackProc)ContextHelpCB, data);*/

    /* There isn't any help for the button, so making it insensitive. MBR 5-05-99 */
    XtSetSensitive( XmFileSelectionBoxGetChild(data->select_file, XmDIALOG_HELP_BUTTON), False );
 

    rtt_iop_popup = XtCreatePopupShell("iop_popup", 
                                       topLevelShellWidgetClass, rtt_top, al, ac);

    rtt_iop_form = XtVaCreateManagedWidget("rttiopform",
                                           xmFormWidgetClass, rtt_iop_popup,
                                           NULL);

    rtt_iop_label1 = XtVaCreateManagedWidget("rttioplabel1",
                                             xmLabelWidgetClass, rtt_iop_form,
                                             XmNtopAttachment, XmATTACH_FORM,
                                             XmNtopOffset, 7,
                                             XmNleftAttachment, XmATTACH_FORM,
                                             XmNrightAttachment, XmATTACH_NONE,
                                             XmNbottomAttachment, XmATTACH_NONE,
                                             XmNalignment, XmALIGNMENT_END,
                                             XmNlabelString, XmStringCreateSimple("iop"),
                                             NULL);

    ac = 0;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNleftWidget, rtt_iop_label1); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
    XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
    XtSetArg(al[ac], XmNradioBehavior, True ); ac++;
    
    rtt_iop_radiobox = XmCreateRowColumn(rtt_iop_form, "rttiopradiobox", al, ac);

    data->rtt_iop_button = 1;
    
    data->rtt_iop_buttons[0] = XtVaCreateManagedWidget("1", 
                                                       xmToggleButtonWidgetClass, rtt_iop_radiobox,
                                                       XmNset, TRUE,
                                                       NULL);
    XtAddCallback(data->rtt_iop_buttons[0], XmNvalueChangedCallback,
                  (XtCallbackProc) rtt_iop_changedCB, (XtPointer) data);

    data->rtt_iop_buttons[1] = XtVaCreateManagedWidget("2", 
                                                       xmToggleButtonWidgetClass, rtt_iop_radiobox, NULL);
    XtAddCallback(data->rtt_iop_buttons[1], XmNvalueChangedCallback,
                  (XtCallbackProc) rtt_iop_changedCB, (XtPointer) data);

    data->rtt_iop_buttons[2] = XtVaCreateManagedWidget("3", 
                                                       xmToggleButtonWidgetClass, rtt_iop_radiobox, NULL);
    XtAddCallback(data->rtt_iop_buttons[2], XmNvalueChangedCallback,
                  (XtCallbackProc) rtt_iop_changedCB, (XtPointer) data);

    data->rtt_iop_buttons[3] = XtVaCreateManagedWidget("4", 
                                                       xmToggleButtonWidgetClass, rtt_iop_radiobox, NULL);
    XtAddCallback(data->rtt_iop_buttons[3], XmNvalueChangedCallback,
                  (XtCallbackProc) rtt_iop_changedCB, (XtPointer) data);

    data->rtt_iop_buttons[4] = XtVaCreateManagedWidget("5", 
                                                       xmToggleButtonWidgetClass, rtt_iop_radiobox, NULL);
    XtAddCallback(data->rtt_iop_buttons[4], XmNvalueChangedCallback,
                  (XtCallbackProc) rtt_iop_changedCB, (XtPointer) data);

    XtManageChild(rtt_iop_radiobox);

    data->rtt_iop_plus = XtVaCreateManagedWidget ("Adjust zsep", xmToggleButtonWidgetClass,
                                                  rtt_iop_form,
                                                  XmNrightAttachment,   XmATTACH_FORM,
                                                  XmNtopAttachment,     XmATTACH_FORM,
                                                  XmNleftAttachment,    XmATTACH_NONE,
                                                  XmNbottomAttachment,  XmATTACH_NONE,
                                                  XmNmarginTop,         7,
                                                  XmNmarginRight,       7,
                                                  XmNsensitive,         False,
	                                          NULL );

    XtAddCallback( data->rtt_iop_plus, XmNvalueChangedCallback, rtt_iop_plusCB, (XtPointer) data );
    
    rtt_iop_label2 = XtVaCreateManagedWidget("rttioplabel2",
                                             xmLabelWidgetClass, rtt_iop_form,
                                             XmNtopAttachment, XmATTACH_WIDGET,
                                             XmNtopWidget, rtt_iop_radiobox,
                                             XmNtopOffset, 7,
                                             XmNleftAttachment, XmATTACH_FORM,
                                             XmNrightAttachment, XmATTACH_NONE,
                                             XmNbottomAttachment, XmATTACH_NONE,
                                             XmNalignment, XmALIGNMENT_END,
                                             XmNlabelString, XmStringCreateSimple("regions"),
                                             NULL);

    data->rtt_iop_text2 = XtVaCreateManagedWidget("rttioptext2",
                                                  xmTextFieldWidgetClass, rtt_iop_form,
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, rtt_iop_radiobox,
                                                  XmNleftAttachment, XmATTACH_WIDGET,
                                                  XmNleftWidget, rtt_iop_label2,
                                                  XmNrightAttachment, XmATTACH_NONE,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, FALSE,
                                                  NULL);
    XtAddCallback( data->rtt_iop_text2, XmNmodifyVerifyCallback, makeAllCapsCB, NULL );

    data->rtt_iop_text3 = XtVaCreateManagedWidget("rttioptext3",
                                                  xmTextFieldWidgetClass, rtt_iop_form,
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, rtt_iop_radiobox,
                                                  XmNleftAttachment, XmATTACH_WIDGET,
                                                  XmNleftWidget, data->rtt_iop_text2,
                                                  XmNrightAttachment, XmATTACH_FORM,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, FALSE,
                                                  NULL);
    XtAddCallback( data->rtt_iop_text3, XmNmodifyVerifyCallback, makeAllCapsCB, NULL );

    rtt_iop_label3 = XtVaCreateManagedWidget("rttioplabel3",
                                             xmLabelWidgetClass, rtt_iop_form,
                                             XmNtopAttachment, XmATTACH_WIDGET,
                                             XmNtopWidget, data->rtt_iop_text2,
                                             XmNtopOffset, 7,
                                             XmNleftAttachment, XmATTACH_FORM,
                                             XmNrightAttachment, XmATTACH_NONE,
                                             XmNbottomAttachment, XmATTACH_NONE,
                                             XmNalignment, XmALIGNMENT_END,
                                             XmNlabelString, XmStringCreateSimple("zsep"),
                                             NULL);

    data->rtt_iop_text4 = XtVaCreateManagedWidget("rttioptext4",
                                                  xmTextFieldWidgetClass, rtt_iop_form,
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_iop_text2,
                                                  XmNleftAttachment, XmATTACH_WIDGET,
                                                  XmNleftWidget, rtt_iop_label3,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 50,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, FALSE,
                                                  NULL);
    XtAddCallback ( data->rtt_iop_text4, XmNmodifyVerifyCallback, (XtCallbackProc) CheckEntry,
                    NULL );

    rtt_iop_label4 = XtVaCreateManagedWidget("rttioplabel4",
                                             xmLabelWidgetClass, rtt_iop_form,
                                             XmNtopAttachment, XmATTACH_WIDGET,
                                             XmNtopWidget, data->rtt_iop_text4,
                                             XmNtopOffset, 7,
                                             XmNleftAttachment, XmATTACH_FORM,
                                             XmNrightAttachment, XmATTACH_NONE,
                                             XmNbottomAttachment, XmATTACH_NONE,
                                             XmNalignment, XmALIGNMENT_END,
                                             XmNlabelString, XmStringCreateSimple("beamline"),
                                             NULL);

    data->rtt_iop_text5 = XtVaCreateManagedWidget("rttioptext5",
                                                  xmTextFieldWidgetClass, rtt_iop_form,
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_iop_text4,
                                                  XmNleftAttachment, XmATTACH_WIDGET,
                                                  XmNleftWidget, rtt_iop_label4,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 75,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, FALSE,
                                                  NULL);
    XtAddCallback ( data->rtt_iop_text5, XmNmodifyVerifyCallback, (XtCallbackProc) NNumbersOnlyCB,
                    &THREE );

    rtt_iop_save = XtVaCreateManagedWidget("rttiopsave",
                                           xmPushButtonWidgetClass, rtt_iop_form,
                                           XmNtopAttachment, XmATTACH_WIDGET,
                                           XmNtopWidget, data->rtt_iop_text5,
                                           XmNleftAttachment, XmATTACH_POSITION,
                                           XmNleftPosition, 10,
                                           XmNrightAttachment, XmATTACH_POSITION,
                                           XmNrightPosition, 30,
                                           XmNbottomAttachment, XmATTACH_NONE,
                                           XmNlabelString, XmStringCreateSimple("Save"),
                                           XmNsensitive, True,
                                           NULL);

    XtAddCallback(rtt_iop_save, XmNactivateCallback,
                  (XtCallbackProc) rtt_iop_saveCB, (XtPointer) data);

    rtt_iop_reset = XtVaCreateManagedWidget("rttiopreset",
                                            xmPushButtonWidgetClass, rtt_iop_form,
                                            XmNtopAttachment, XmATTACH_WIDGET,
                                            XmNtopWidget, data->rtt_iop_text5,
                                            XmNleftAttachment, XmATTACH_POSITION,
                                            XmNleftPosition, 40,
                                            XmNrightAttachment, XmATTACH_POSITION,
                                            XmNrightPosition, 60,
                                            XmNbottomAttachment, XmATTACH_NONE,
                                            XmNlabelString, XmStringCreateSimple("Reset"),
                                            XmNsensitive, True,
                                            NULL);

    XtAddCallback(rtt_iop_reset, XmNactivateCallback,
                  (XtCallbackProc) rtt_iop_resetCB, (XtPointer) data);

    rtt_iop_cancel = XtVaCreateManagedWidget("rttiopcancel",
                                             xmPushButtonWidgetClass, rtt_iop_form,
                                             XmNtopAttachment, XmATTACH_WIDGET,
                                             XmNtopWidget, data->rtt_iop_text5,
                                             XmNleftAttachment, XmATTACH_POSITION,
                                             XmNleftPosition, 70,
                                             XmNrightAttachment, XmATTACH_POSITION,
                                             XmNrightPosition, 90,
                                             XmNbottomAttachment, XmATTACH_NONE,
                                             XmNlabelString, XmStringCreateSimple("Close"),
                                             XmNsensitive, True,
                                             NULL);

    XtAddCallback(rtt_iop_cancel, XmNactivateCallback,
                  (XtCallbackProc) rtt_iop_cancelCB, (XtPointer) data);

    rtt_isotopes_popup = XtVaCreatePopupShell("isotopes_popup", 
                                              topLevelShellWidgetClass, rtt_top, NULL );

    rtt_isotopes_form = XtVaCreateManagedWidget("rttisotopesform",
                                                xmFormWidgetClass, rtt_isotopes_popup,
                                                XmNwidth, 300,
                                                NULL);

    rtt_isotopes_label1 = XtVaCreateManagedWidget("rttisotopeslabel1",
                                                  xmLabelWidgetClass, rtt_isotopes_form,
                                                  XmNlabelString,
                                                  XmStringCreateLtoR("Edit Isotopes",XmSTRING_DEFAULT_CHARSET),
                                                  XmNtopAttachment, XmATTACH_FORM,
                                                  XmNleftAttachment, XmATTACH_FORM,
                                                  XmNrightAttachment, XmATTACH_FORM,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNsensitive, True,
                                                  NULL);

    rtt_isotopes_label1a = XtVaCreateManagedWidget("rttisotopeslabel1a",
                                                   xmLabelWidgetClass, rtt_isotopes_form,
                                                   XmNlabelString,
                                                   XmStringCreateLtoR("Boron-10",XmSTRING_DEFAULT_CHARSET),
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, rtt_isotopes_label1,
                                                   XmNtopOffset, 7,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_POSITION,
                                                   XmNrightPosition, 35,
                                                   XmNbottomAttachment, XmATTACH_NONE,
                                                   XmNsensitive, True,
                                                   NULL);
    data->rtt_isotopes_text1a = XtVaCreateManagedWidget("rttisotopestext1a",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, rtt_isotopes_label1,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, rtt_isotopes_label1a,
                                                        XmNrightAttachment, XmATTACH_POSITION,
                                                        XmNrightPosition, 55,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"521",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text1a, XmNmodifyVerifyCallback,
                    (XtCallbackProc) integersOnlyCB, NULL );
    sscanf (XmTextGetString(data->rtt_isotopes_text1a), "%d", &data->nuclide_id[0]);

    data->rtt_isotopes_text1b = XtVaCreateManagedWidget("rttisotopestext1b",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, rtt_isotopes_label1,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, data->rtt_isotopes_text1a,
                                                        XmNrightAttachment, XmATTACH_FORM,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"6.01426E-8",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text1b, XmNmodifyVerifyCallback,
                    (XtCallbackProc) NNumbersOnlyCB, &ONE );
    sscanf (XmTextGetString(data->rtt_isotopes_text1b), "%lf", &data->nuclide_density[0]);

    rtt_isotopes_label2a = XtVaCreateManagedWidget("rttisotopeslabel2a",
                                                   xmLabelWidgetClass, rtt_isotopes_form,
                                                   XmNlabelString,
                                                   XmStringCreateLtoR("Hydrogen",XmSTRING_DEFAULT_CHARSET),
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, data->rtt_isotopes_text1a,
                                                   XmNtopOffset, 7,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_POSITION,
                                                   XmNrightPosition, 35,
                                                   XmNbottomAttachment, XmATTACH_NONE,
                                                   XmNsensitive, True,
                                                   NULL);
    data->rtt_isotopes_text2a = XtVaCreateManagedWidget("rttisotopestext2a",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text1a,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, rtt_isotopes_label2a,
                                                        XmNrightAttachment, XmATTACH_POSITION,
                                                        XmNrightPosition, 55,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"508",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text2a, XmNmodifyVerifyCallback,
                    (XtCallbackProc) integersOnlyCB, NULL );
    sscanf (XmTextGetString(data->rtt_isotopes_text2a), "%d", &data->nuclide_id[1]);

    data->rtt_isotopes_text2b = XtVaCreateManagedWidget("rttisotopestext2b",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text1b,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, data->rtt_isotopes_text2a,
                                                        XmNrightAttachment, XmATTACH_FORM,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"6.315E-2",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text2b, XmNmodifyVerifyCallback,
                    (XtCallbackProc) NNumbersOnlyCB, &ONE );
    sscanf (XmTextGetString(data->rtt_isotopes_text2b), "%lf", &data->nuclide_density[1]);

    rtt_isotopes_label3a = XtVaCreateManagedWidget("rttisotopeslabel3a",
                                                   xmLabelWidgetClass, rtt_isotopes_form,
                                                   XmNlabelString,
                                                   XmStringCreateLtoR("Nitrogen",XmSTRING_DEFAULT_CHARSET),
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, data->rtt_isotopes_text2a,
                                                   XmNtopOffset, 7,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_POSITION,
                                                   XmNrightPosition, 35,
                                                   XmNbottomAttachment, XmATTACH_NONE,
                                                   XmNsensitive, True,
                                                   NULL);
    data->rtt_isotopes_text3a = XtVaCreateManagedWidget("rttisotopestext3a",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text2a,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, rtt_isotopes_label3a,
                                                        XmNrightAttachment, XmATTACH_POSITION,
                                                        XmNrightPosition, 55,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"513",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text3a, XmNmodifyVerifyCallback,
                    (XtCallbackProc) integersOnlyCB, NULL );
    sscanf (XmTextGetString(data->rtt_isotopes_text3a), "%d", &data->nuclide_id[2]);

    data->rtt_isotopes_text3b = XtVaCreateManagedWidget("rttisotopestext3b",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text2b,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, data->rtt_isotopes_text3a,
                                                        XmNrightAttachment, XmATTACH_FORM,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"7.911E-4",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text3b, XmNmodifyVerifyCallback,
                    (XtCallbackProc) NNumbersOnlyCB, &ONE );
    sscanf (XmTextGetString(data->rtt_isotopes_text3b), "%lf", &data->nuclide_density[2]);

    rtt_isotopes_label4a = XtVaCreateManagedWidget("rttisotopeslabel4a",
                                                   xmLabelWidgetClass, rtt_isotopes_form,
                                                   XmNlabelString,
                                                   XmStringCreateLtoR("Carbon",XmSTRING_DEFAULT_CHARSET),
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, data->rtt_isotopes_text3a,
                                                   XmNtopOffset, 7,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_POSITION,
                                                   XmNrightPosition, 35,
                                                   XmNbottomAttachment, XmATTACH_NONE,
                                                   XmNsensitive, True,
                                                   NULL);
    data->rtt_isotopes_text4a = XtVaCreateManagedWidget("rttisotopestext4a",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text3a,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, rtt_isotopes_label4a,
                                                        XmNrightAttachment, XmATTACH_POSITION,
                                                        XmNrightPosition, 55,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"512",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text4a, XmNmodifyVerifyCallback,
                    (XtCallbackProc) integersOnlyCB, NULL );
    sscanf (XmTextGetString(data->rtt_isotopes_text4a), "%d", &data->nuclide_id[3]);

    data->rtt_isotopes_text4b = XtVaCreateManagedWidget("rttisotopestext4b",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text3b,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, data->rtt_isotopes_text4a,
                                                        XmNrightAttachment, XmATTACH_FORM,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"7.270E-3",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text4b, XmNmodifyVerifyCallback,
                    (XtCallbackProc) NNumbersOnlyCB, &ONE );
    sscanf (XmTextGetString(data->rtt_isotopes_text4b), "%lf", &data->nuclide_density[3]);

    rtt_isotopes_label5a = XtVaCreateManagedWidget("rttisotopeslabel5a",
                                                   xmLabelWidgetClass, rtt_isotopes_form,
                                                   XmNlabelString,
                                                   XmStringCreateLtoR("Oxygen",XmSTRING_DEFAULT_CHARSET),
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, data->rtt_isotopes_text4a,
                                                   XmNtopOffset, 7,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_POSITION,
                                                   XmNrightPosition, 35,
                                                   XmNbottomAttachment, XmATTACH_NONE,
                                                   XmNsensitive, True,
                                                   NULL);
    data->rtt_isotopes_text5a = XtVaCreateManagedWidget("rttisotopestext5a",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text4a,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, rtt_isotopes_label5a,
                                                        XmNrightAttachment, XmATTACH_POSITION,
                                                        XmNrightPosition, 55,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"505",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text5a, XmNmodifyVerifyCallback,
                    (XtCallbackProc) integersOnlyCB, NULL );
    sscanf (XmTextGetString(data->rtt_isotopes_text5a), "%d", &data->nuclide_id[4]);

    data->rtt_isotopes_text5b = XtVaCreateManagedWidget("rttisotopestext5b",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text4b,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, data->rtt_isotopes_text5a,
                                                        XmNrightAttachment, XmATTACH_FORM,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"2.680E-2",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text5b, XmNmodifyVerifyCallback,
                    (XtCallbackProc) NNumbersOnlyCB, &ONE );
    sscanf (XmTextGetString(data->rtt_isotopes_text5b), "%lf", &data->nuclide_density[4]);

    rtt_isotopes_label6a = XtVaCreateManagedWidget("rttisotopeslabel6a",
                                                   xmLabelWidgetClass, rtt_isotopes_form,
                                                   XmNlabelString,
                                                   XmStringCreateLtoR("Edit isotope 1",XmSTRING_DEFAULT_CHARSET),
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, data->rtt_isotopes_text5a,
                                                   XmNtopOffset, 7,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_POSITION,
                                                   XmNrightPosition, 35,
                                                   XmNbottomAttachment, XmATTACH_NONE,
                                                   XmNsensitive, True,
                                                   NULL);
    data->rtt_isotopes_text6a = XtVaCreateManagedWidget("rttisotopestext6a",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text5a,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, rtt_isotopes_label6a,
                                                        XmNrightAttachment, XmATTACH_POSITION,
                                                        XmNrightPosition, 55,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"0",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text6a, XmNmodifyVerifyCallback,
                    (XtCallbackProc) integersOnlyCB, NULL );
    sscanf (XmTextGetString(data->rtt_isotopes_text6a), "%d", &data->nuclide_id[5]);

    data->rtt_isotopes_text6b = XtVaCreateManagedWidget("rttisotopestext6b",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text5b,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, data->rtt_isotopes_text6a,
                                                        XmNrightAttachment, XmATTACH_FORM,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"0.0",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text6b, XmNmodifyVerifyCallback,
                    (XtCallbackProc) NNumbersOnlyCB, &ONE );
    sscanf (XmTextGetString(data->rtt_isotopes_text6b), "%lf", &data->nuclide_density[5]);

    rtt_isotopes_label7a = XtVaCreateManagedWidget("rttisotopeslabel7a",
                                                   xmLabelWidgetClass, rtt_isotopes_form,
                                                   XmNlabelString,
                                                   XmStringCreateLtoR("Edit isotope 2",XmSTRING_DEFAULT_CHARSET),
                                                   XmNtopAttachment, XmATTACH_WIDGET,
                                                   XmNtopWidget, data->rtt_isotopes_text6a,
                                                   XmNtopOffset, 7,
                                                   XmNleftAttachment, XmATTACH_FORM,
                                                   XmNrightAttachment, XmATTACH_POSITION,
                                                   XmNrightPosition, 35,
                                                   XmNbottomAttachment, XmATTACH_NONE,
                                                   XmNsensitive, True,
                                                   NULL);
    data->rtt_isotopes_text7a = XtVaCreateManagedWidget("rttisotopestext7a",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text6a,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, rtt_isotopes_label7a,
                                                        XmNrightAttachment, XmATTACH_POSITION,
                                                        XmNrightPosition, 55,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"0",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text7a, XmNmodifyVerifyCallback,
                    (XtCallbackProc) integersOnlyCB, NULL );
    sscanf (XmTextGetString(data->rtt_isotopes_text7a), "%d", &data->nuclide_id[6]);

    data->rtt_isotopes_text7b = XtVaCreateManagedWidget("rttisotopestext7b",
                                                        xmTextFieldWidgetClass, rtt_isotopes_form,
                                                        XmNtopAttachment, XmATTACH_WIDGET,
                                                        XmNtopWidget, data->rtt_isotopes_text6b,
                                                        XmNleftAttachment, XmATTACH_WIDGET,
                                                        XmNleftWidget, data->rtt_isotopes_text7a,
                                                        XmNrightAttachment, XmATTACH_FORM,
                                                        XmNbottomAttachment, XmATTACH_NONE,
                                                        XmNvalue,"0.0",
                                                        NULL);
    XtAddCallback ( data->rtt_isotopes_text7b, XmNmodifyVerifyCallback,
                    (XtCallbackProc) NNumbersOnlyCB, &ONE );
    sscanf (XmTextGetString(data->rtt_isotopes_text7b), "%lf", &data->nuclide_density[6]);

    rtt_isotopes_save = XtVaCreateManagedWidget("rttisotopessave",
                                                xmPushButtonWidgetClass, rtt_isotopes_form,
                                                XmNtopAttachment, XmATTACH_WIDGET,
                                                XmNtopWidget, data->rtt_isotopes_text7a,
                                                XmNleftAttachment, XmATTACH_POSITION,
                                                XmNleftPosition, 0,
                                                XmNrightAttachment, XmATTACH_POSITION,
                                                XmNrightPosition, 30,
                                                XmNbottomAttachment, XmATTACH_NONE,
                                                XmNlabelString, XmStringCreateSimple("Save"),
                                                XmNsensitive, True,
                                                NULL);

    XtAddCallback(rtt_isotopes_save, XmNactivateCallback,
                  (XtCallbackProc) rtt_isotopes_saveCB, data);

    rtt_isotopes_reset = XtVaCreateManagedWidget("rttisotopesreset",
                                                 xmPushButtonWidgetClass, rtt_isotopes_form,
                                                 XmNtopAttachment, XmATTACH_WIDGET,
                                                 XmNtopWidget, data->rtt_isotopes_text7b,
                                                 XmNleftAttachment, XmATTACH_POSITION,
                                                 XmNleftPosition, 35,
                                                 XmNrightAttachment, XmATTACH_POSITION,
                                                 XmNrightPosition, 65,
                                                 XmNbottomAttachment, XmATTACH_NONE,
                                                 XmNlabelString, XmStringCreateSimple("Reset"),
                                                 XmNsensitive, True,
                                                 NULL);

    XtAddCallback(rtt_isotopes_reset, XmNactivateCallback,
                  (XtCallbackProc) rtt_isotopes_resetCB, data);

    rtt_isotopes_cancel = XtVaCreateManagedWidget("rttisotopescancel",
                                                  xmPushButtonWidgetClass, rtt_isotopes_form,
                                                  XmNtopAttachment, XmATTACH_WIDGET,
                                                  XmNtopWidget, data->rtt_isotopes_text7b,
                                                  XmNleftAttachment, XmATTACH_POSITION,
                                                  XmNleftPosition, 70,
                                                  XmNrightAttachment, XmATTACH_POSITION,
                                                  XmNrightPosition, 100,
                                                  XmNbottomAttachment, XmATTACH_NONE,
                                                  XmNlabelString, XmStringCreateSimple("Cancel"),
                                                  XmNsensitive, True,
                                                  NULL);

    XtAddCallback(rtt_isotopes_cancel, XmNactivateCallback,
                  (XtCallbackProc) rtt_isotopes_cancelCB, data);

    DEBUG_TRACE_OUT printf("Leaving create_rtt_forms\n");

}

void
create_rtt_option_strings(void)
{
    int i = 0;

    DEBUG_TRACE_IN printf("Entering create_rtt_option_strings\n");
    
    rtt_opt[i++] = allocate_rtt_option("#patient John Doe");
    rtt_opt[i++] = allocate_rtt_option("#date Jan_07_2001");
    rtt_opt[i++] = allocate_rtt_option("#beam BMRR_12_CM");

    rtt_opt[i++] = allocate_rtt_option(" refvol 1.0 0");
    rtt_opt[i++] = allocate_rtt_option(" refpoint 1.0 1.0 1.0");
    rtt_opt[i++] = allocate_rtt_option(" refdepth 2.5");
    rtt_opt[i++] = allocate_rtt_option(" ref_reg brain tumor");
    rtt_opt[i++] = allocate_rtt_option(" more_ref 12 13");
    rtt_opt[i++] = allocate_rtt_option(" ref_b10 1.0");
    rtt_opt[i++] = allocate_rtt_option(" ref_rbe 1.0 1.0 1.0 1.0 1.0 1.0");

    rtt_opt[i++] = allocate_rtt_option(" mw_min 1.0");
    rtt_opt[i++] = allocate_rtt_option(" mw 3.0");
    rtt_opt[i++] = allocate_rtt_option(" b10_blood 1.0");
    rtt_opt[i++] = allocate_rtt_option(" b10_ratio brain 1.0 tumor 3.5");
    rtt_opt[i++] = allocate_rtt_option(" delta 0.25");
    rtt_opt[i++] = allocate_rtt_option(" eps 0.05");
    rtt_opt[i++] = allocate_rtt_option(" in_reg brain tumor");
    rtt_opt[i++] = allocate_rtt_option(" more_reg 12 13");
    rtt_opt[i++] = allocate_rtt_option(" nbin_DV 20");
    rtt_opt[i++] = allocate_rtt_option(" N_avg 3");
    rtt_opt[i++] = allocate_rtt_option(" rbe 1.0 1.0 1.0 1.0 1.0 1.0");
    rtt_opt[i++] = allocate_rtt_option(" images 24.0  2 1 3   40 40");
    rtt_opt[i++] = allocate_rtt_option(" ap 0.0 0.0 0.0");
    rtt_opt[i++] = allocate_rtt_option(" bp 1.0 1.0 1.0");
    rtt_opt[i++] = allocate_rtt_option(" cp 0.0 0.0 0.0");
    rtt_opt[i++] = allocate_rtt_option(" crosshair 10.0");
    rtt_opt[i++] = allocate_rtt_option(" skinentry 0.1");

    rtt_opt[i++] = allocate_rtt_option(" point 1.0 1.0 1.0  optic_chiasm");
    rtt_opt[i++] = allocate_rtt_option(" line 0.0 0.0 0.0   0.0 0.0 10.0");
    rtt_opt[i++] = allocate_rtt_option(" contour filename 1.0");
    rtt_opt[i++] = allocate_rtt_option(" box -1.0 1.0   -1.0 1.0   -1.0 1.0");
    rtt_opt[i++] = allocate_rtt_option(" fiducial 1.0 1.0 1.0 Posterior");
    rtt_opt[i++] = allocate_rtt_option(" ras -10.0 10.0   -10.0 10.0   1.0 1.0");
    rtt_opt[i++] = allocate_rtt_option(" DVbs");
    rtt_opt[i++] = allocate_rtt_option(" ottocon 40 40 scalp");
    rtt_opt[i++] = allocate_rtt_option(" beamplt 40 40  -3.0  5.0 bplane");
    rtt_opt[i++] = allocate_rtt_option(" px  1.0  -2.0 2.0   -2.0 2.0");
    rtt_opt[i++] = allocate_rtt_option(" py  2.0   1.0 1.0   -2.0 2.0");
    rtt_opt[i++] = allocate_rtt_option(" pz  2.0  -2.0 2.0    1.0 1.0");

    DEBUG_TRACE_OUT printf("Leaving create_rtt_option_strings\n");
}

char *
allocate_rtt_option(char * option)
{
    char *temp;

    DEBUG_TRACE_IN printf("Entering allocate_rtt_option\n");
    
    temp = (char *) MT_malloc((int)(sizeof(char)*(strlen(option)+1)));
    strcpy(temp,option);

    DEBUG_TRACE_OUT printf("Leaving allocate_rtt_option\n");
    return(temp);

}

#define SEPCHARS " "
void
create_rtt_editpu_buttons(Widget parent, RttEditPopupSt *data)
{
    int i;
    int len_token;
    char * token;
    char * name;
    char * local_label;
    Widget button;

    DEBUG_TRACE_IN printf("Entering create_rtt_editpu_buttons\n");
    
    for(i=0; i<MAX_RTT_OPTIONS; i++)
    {
        local_label = (char *) MT_malloc((int)(strlen(rtt_opt[i])+1)*sizeof(char));
  
        strcpy(local_label, rtt_opt[i]);
        /* get first token from rtt_option label */
        token = strtok(local_label, SEPCHARS);
        len_token = strlen(token);

        if(strcmp(token,"#separator") != 0)
        {
            /* create a widget name */
            name = (char *) MT_malloc((int)(sizeof(char)*(len_token+1)));
            strcpy(name, token);

            button = XtVaCreateManagedWidget(name,
                                             xmPushButtonWidgetClass, parent,
                                             XmNlabelString, XmStringCreateSimple(name),
                                             XmNuserData, i,
                                             NULL);
            XtAddCallback(button, XmNactivateCallback, 
                          (XtCallbackProc)rtt_editdir_buttonCB, data);
        } else {
            /* create a separator widget name */
            name = (char *) MT_malloc((int)(sizeof(char)*(len_token+1)));
            strcpy(name, token);
            button = XtVaCreateManagedWidget(name,
                                             xmSeparatorWidgetClass, parent,
                                             XmNlabelString, XmStringCreateSimple(name),
                                             XmNuserData, i,
                                             XmNseparatorType, XmNO_LINE,
                                             NULL);

        };

        MT_free((void *)name);
        MT_free((void *)local_label);

    };

    /* Create one more button Close */

    /* create a widget name */
    name = (char *) MT_malloc((int)(sizeof(char)*14));
    strcpy(name, "CloseEditDir");

    button = XtVaCreateManagedWidget(name,
                                     xmPushButtonWidgetClass, parent,
                                     XmNlabelString, XmStringCreateSimple("Close"),
                                     XmNuserData, -1,
                                     NULL);

    XtAddCallback(button, XmNactivateCallback, 
                  (XtCallbackProc)rtt_editdir_buttonCB, NULL);

    MT_free((char *)name);

    DEBUG_TRACE_OUT printf("Leaving create_rtt_editpu_buttons\n");
}
