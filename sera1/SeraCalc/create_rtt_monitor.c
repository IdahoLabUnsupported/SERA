#include "rtt.h"

extern Widget rtt_edit_mon_form;

void
create_rtt_monitor()
{
    Arg al[10];
    int ac = 0;
    Widget rtt_mon_rowcol2, rtt_mon_rowcol3;
    Widget rtt_mon_label10, rtt_mon_label11, rtt_mon_label12;
    Widget rtt_mon_label20;
    Widget rtt_mon_label30;
    Widget rtt_mon_label40;
    Widget rtt_mon_label50;
    Widget rtt_mon_label5, rtt_mon_label6, rtt_mon_label6x, 
        rtt_mon_label6y, rtt_mon_label6z;
    Widget rtt_mon_label7;

    DEBUG_TRACE_IN printf("Entering create_rtt_monitor\n");
    
    rtt_mon_rowcol2 = XtVaCreateManagedWidget("rttrowcol2",
                                              xmRowColumnWidgetClass, rtt_edit_mon_form,
                                              XmNtopAttachment, XmATTACH_FORM,
                                              XmNleftAttachment, XmATTACH_FORM,
                                              XmNrightAttachment, XmATTACH_FORM,
                                              XmNbottomAttachment, XmATTACH_NONE,
                                              XmNorientation, XmHORIZONTAL,
                                              XmNpacking, XmPACK_COLUMN,
                                              XmNnumColumns, 5,
                                              XmNadjustLast, True,
                                              XmNisAligned, True,
                                              XmNentryAlignment, XmALIGNMENT_END,
                                              NULL);

    rtt_mon_label10 = XtVaCreateManagedWidget("rttmonlabel10",
                                              xmLabelWidgetClass, rtt_mon_rowcol2,
                                              XmNlabelString,
                                              XmStringCreateLtoR("",XmSTRING_DEFAULT_CHARSET),
                                              NULL);
    rtt_mon_label11 = XtVaCreateManagedWidget("rttmonlabel11",
                                              xmLabelWidgetClass, rtt_mon_rowcol2,
                                              XmNlabelString,
                                              XmStringCreateLtoR("# Processed",XmSTRING_DEFAULT_CHARSET),
                                              NULL);
    rtt_mon_label12 = XtVaCreateManagedWidget("rttmonlabel12",
                                              xmLabelWidgetClass, rtt_mon_rowcol2,
                                              XmNlabelString,
                                              XmStringCreateLtoR("# Lost",XmSTRING_DEFAULT_CHARSET),
                                              NULL);

    rtt_mon_label20 = XtVaCreateManagedWidget("rttmonlabel20",
                                              xmLabelWidgetClass, rtt_mon_rowcol2,
                                              XmNlabelString,
                                              XmStringCreateLtoR("neutron",XmSTRING_DEFAULT_CHARSET),
                                              NULL);
    rtt_file_data->rtt_mon_label21 = XtVaCreateManagedWidget("rttmonlabel21",
                                                             xmLabelWidgetClass, rtt_mon_rowcol2,
                                                             XmNlabelString,
                                                             XmStringCreateLtoR("0",XmSTRING_DEFAULT_CHARSET),
                                                             NULL);
    rtt_file_data->rtt_mon_label22 = XtVaCreateManagedWidget("rttmonlabel22",
                                                             xmLabelWidgetClass, rtt_mon_rowcol2,
                                                             XmNlabelString,
                                                             XmStringCreateLtoR("0",XmSTRING_DEFAULT_CHARSET),
                                                             NULL);

    rtt_mon_label30 = XtVaCreateManagedWidget("rttmonlabel30",
                                              xmLabelWidgetClass, rtt_mon_rowcol2,
                                              XmNlabelString,
                                              XmStringCreateLtoR("fast",XmSTRING_DEFAULT_CHARSET),
                                              NULL);
    rtt_file_data->rtt_mon_label31 = XtVaCreateManagedWidget("rttmonlabel31",
                                                             xmLabelWidgetClass, rtt_mon_rowcol2,
                                                             XmNlabelString,
                                                             XmStringCreateLtoR("0",XmSTRING_DEFAULT_CHARSET),
                                                             NULL);
    rtt_file_data->rtt_mon_label32 = XtVaCreateManagedWidget("rttmonlabel32",
                                                             xmLabelWidgetClass, rtt_mon_rowcol2,
                                                             XmNlabelString,
                                                             XmStringCreateLtoR("0",XmSTRING_DEFAULT_CHARSET),
                                                             NULL);

    rtt_mon_label40 = XtVaCreateManagedWidget("rttmonlabel40",
                                              xmLabelWidgetClass, rtt_mon_rowcol2,
                                              XmNlabelString,
                                              XmStringCreateLtoR("gamma",XmSTRING_DEFAULT_CHARSET),
                                              NULL);
    rtt_file_data->rtt_mon_label41 = XtVaCreateManagedWidget("rttmonlabel41",
                                                             xmLabelWidgetClass, rtt_mon_rowcol2,
                                                             XmNlabelString,
                                                             XmStringCreateLtoR("0",XmSTRING_DEFAULT_CHARSET),
                                                             NULL);
    rtt_file_data->rtt_mon_label42 = XtVaCreateManagedWidget("rttmonlabel42",
                                                             xmLabelWidgetClass, rtt_mon_rowcol2,
                                                             XmNlabelString,
                                                             XmStringCreateLtoR("0",XmSTRING_DEFAULT_CHARSET),
                                                             NULL);

    rtt_mon_label50 = XtVaCreateManagedWidget("rttmonlabel50",
                                              xmLabelWidgetClass, rtt_mon_rowcol2,
                                              XmNlabelString,
                                              XmStringCreateLtoR("ultrafast",XmSTRING_DEFAULT_CHARSET),
                                              NULL);
    rtt_file_data->rtt_mon_label51 = XtVaCreateManagedWidget("rttmonlabel51",
                                                             xmLabelWidgetClass, rtt_mon_rowcol2,
                                                             XmNlabelString,
                                                             XmStringCreateLtoR("0",XmSTRING_DEFAULT_CHARSET),
                                                             NULL);
    rtt_file_data->rtt_mon_label52 = XtVaCreateManagedWidget("rttmonlabel52",
                                                             xmLabelWidgetClass, rtt_mon_rowcol2,
                                                             XmNlabelString,
                                                             XmStringCreateLtoR("0",XmSTRING_DEFAULT_CHARSET),
                                                             NULL);

    rtt_mon_label5 = XtVaCreateManagedWidget("rttmonlabel5",
                                             xmLabelWidgetClass, rtt_edit_mon_form,
                                             XmNtopAttachment, XmATTACH_WIDGET,
                                             XmNtopWidget, rtt_mon_rowcol2,
                                             XmNleftAttachment, XmATTACH_FORM,
                                             XmNrightAttachment, XmATTACH_FORM,
                                             XmNbottomAttachment, XmATTACH_NONE,
                                             XmNlabelString,
                                             XmStringCreateLtoR("Thermal Peak",XmSTRING_DEFAULT_CHARSET),
                                             NULL);

    rtt_mon_rowcol3 = XtVaCreateManagedWidget("rttrowcol3",
                                              xmRowColumnWidgetClass, rtt_edit_mon_form,
                                              XmNtopAttachment, XmATTACH_WIDGET,
                                              XmNtopWidget, rtt_mon_label5,
                                              XmNleftAttachment, XmATTACH_FORM,
                                              XmNrightAttachment, XmATTACH_FORM,
                                              XmNbottomAttachment, XmATTACH_NONE,
                                              XmNorientation, XmHORIZONTAL,
                                              XmNpacking, XmPACK_TIGHT,
                                              XmNadjustLast, False,
                                              XmNisAligned, False,
                                              XmNnumColumns, 1,
                                              NULL);


    rtt_mon_label6 = XtVaCreateManagedWidget("rttmonlabel6",
                                             xmLabelWidgetClass, rtt_mon_rowcol3,
                                             XmNlabelString,
                                             XmStringCreateLtoR("  Location",XmSTRING_DEFAULT_CHARSET),
                                             NULL);

/* Peak X Location */
    rtt_mon_label6x = XtVaCreateManagedWidget("rttmonlabel6x",
                                              xmLabelWidgetClass, rtt_mon_rowcol3,
                                              XmNalignment, XmALIGNMENT_END,
                                              XmNlabelString,
                                              XmStringCreateLtoR("X:",XmSTRING_DEFAULT_CHARSET),
                                              NULL);
    rtt_file_data->rtt_mon_label6xv = XtVaCreateManagedWidget("rttmonlabel6xv",
                                                              xmLabelWidgetClass, rtt_mon_rowcol3,
                                                              XmNalignment, XmALIGNMENT_BEGINNING,
                                                              XmNlabelString,
                                                              XmStringCreateLtoR("0.0",XmSTRING_DEFAULT_CHARSET),
                                                              NULL);

/* Peak Y Location */
    rtt_mon_label6y = XtVaCreateManagedWidget("rttmonlabel6y",
                                              xmLabelWidgetClass, rtt_mon_rowcol3,
                                              XmNalignment, XmALIGNMENT_END,
                                              XmNlabelString,
                                              XmStringCreateLtoR("Y:",XmSTRING_DEFAULT_CHARSET),
                                              NULL);
    rtt_file_data->rtt_mon_label6yv = XtVaCreateManagedWidget("rttmonlabel6yv",
                                                              xmLabelWidgetClass, rtt_mon_rowcol3,
                                                              XmNalignment, XmALIGNMENT_BEGINNING,
                                                              XmNlabelString,
                                                              XmStringCreateLtoR("0.0",XmSTRING_DEFAULT_CHARSET),
                                                              NULL);

/* Peak Z Location */
    rtt_mon_label6z = XtVaCreateManagedWidget("rttmonlabel6z",
                                              xmLabelWidgetClass, rtt_mon_rowcol3,
                                              XmNalignment, XmALIGNMENT_END,
                                              XmNlabelString,
                                              XmStringCreateLtoR("Z:",XmSTRING_DEFAULT_CHARSET),
                                              NULL);
    rtt_file_data->rtt_mon_label6zv = XtVaCreateManagedWidget("rttmonlabel6zv",
                                                              xmLabelWidgetClass, rtt_mon_rowcol3,
                                                              XmNalignment, XmALIGNMENT_BEGINNING,
                                                              XmNlabelString,
                                                              XmStringCreateLtoR("0.0",XmSTRING_DEFAULT_CHARSET),
                                                              NULL);

    rtt_mon_label7 = XtVaCreateManagedWidget("rttmonlabel7",
                                             xmLabelWidgetClass, rtt_edit_mon_form,
                                             XmNtopAttachment, XmATTACH_WIDGET,
                                             XmNtopWidget, rtt_mon_rowcol3,
                                             XmNleftAttachment, XmATTACH_FORM,
                                             XmNrightAttachment, XmATTACH_NONE,
                                             XmNbottomAttachment, XmATTACH_NONE,
                                             XmNlabelString,
                                             XmStringCreateLtoR("  Value:",XmSTRING_DEFAULT_CHARSET),
                                             NULL);

    rtt_file_data->rtt_mon_label8 = XtVaCreateManagedWidget("rttmonlabel8",
                                                            xmLabelWidgetClass, rtt_edit_mon_form,
                                                            XmNtopAttachment, XmATTACH_WIDGET,
                                                            XmNtopWidget, rtt_mon_rowcol3,
                                                            XmNleftAttachment, XmATTACH_WIDGET,
                                                            XmNleftWidget, rtt_mon_label7,
                                                            XmNrightAttachment, XmATTACH_FORM,
                                                            XmNbottomAttachment, XmATTACH_NONE,
                                                            XmNlabelString,
                                                            XmStringCreateLtoR("0",XmSTRING_DEFAULT_CHARSET),
                                                            NULL);

    DEBUG_TRACE_OUT printf("Leaving create_rtt_monitor\n");
}
