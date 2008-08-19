#include "rtt.h"


void
read_and_display(XtPointer clientData, XtIntervalId *id)
{
    char status[256];
    char * s1;
    char * s2;
    char * s3;
    char dummy[10];
    FILE * fptr;
    int done_flag;
    char seraMonPath[256];
    char * saveDirectory;

    static int run_flag=0;

    DEBUG_TRACE_IN printf("Entering read_and_display\n");

    sprintf( seraMonPath, "%s%s", rtt_file_data->saveDirectory, "sera.mon" );

    if ((fptr = fopen ( seraMonPath, "r")) == NULL)
    {
        if ( !run_flag ) fprintf(stderr, "Waiting... for a sera.mon file\n");
        rtt_file_data->interval_id = XtAppAddTimeOut(rtt_app, 
                                                     RTT_RUN_TIMEOUT, read_and_display, NULL);
        DEBUG_TRACE_OUT printf("Leaving read_and_display\n");
        return;
    }

    s1 = (char *) MT_malloc(120*sizeof(char));
    s2 = (char *) MT_malloc(120*sizeof(char));
    s3 = (char *) MT_malloc(120*sizeof(char));

    fscanf(fptr, "%s\n", status);
    XtVaSetValues(rtt_file_data->rtt_status_text,
                  XmNlabelString, XmStringCreateLtoR(status,XmSTRING_DEFAULT_CHARSET),
                  NULL);

    done_flag = FALSE; 
    /* Check to see if next run has not started yet.  If so, just
       restore the timer and return */
    if(strncmp(status,"INACTIVE",8) == 0)
    {
        fclose(fptr);
        fprintf(stderr, "Waiting... for new sera.mon file\n");
        rtt_file_data->interval_id = XtAppAddTimeOut(rtt_app, 
                                                     RTT_RUN_TIMEOUT, read_and_display, NULL);
        DEBUG_TRACE_OUT printf("Leaving read_and_display\n");
        return;
    };
    
    if(strncmp(status,"CRASHED",7) == 0) done_flag = TRUE;
    if(strncmp(status,"KILLED",6) == 0) done_flag = TRUE;


    if( strncmp(status,"RUNNING",7) == 0 || strncmp(status,"EDITING",7) == 0) {
      if( done_flag == FALSE ) {
        run_flag = 1;
        fscanf(fptr, "%s %s %s\n", dummy, s1, s2);
        XtVaSetValues(rtt_file_data->rtt_mon_label21,
                      XmNlabelString, XmStringCreateLtoR(s1,XmSTRING_DEFAULT_CHARSET),
                      NULL);
        XtVaSetValues(rtt_file_data->rtt_mon_label22,
                      XmNlabelString, XmStringCreateLtoR(s2,XmSTRING_DEFAULT_CHARSET),
                      NULL);

        fscanf(fptr, "%s %s %s\n", dummy, s1, s2);
        XtVaSetValues(rtt_file_data->rtt_mon_label31,
                      XmNlabelString, XmStringCreateLtoR(s1,XmSTRING_DEFAULT_CHARSET),
                      NULL);
        XtVaSetValues(rtt_file_data->rtt_mon_label32,
                      XmNlabelString, XmStringCreateLtoR(s2,XmSTRING_DEFAULT_CHARSET),
                      NULL);

        fscanf(fptr, "%s %s %s\n", dummy, s1, s2);
        XtVaSetValues(rtt_file_data->rtt_mon_label41,
                      XmNlabelString, XmStringCreateLtoR(s1,XmSTRING_DEFAULT_CHARSET),
                      NULL);
        XtVaSetValues(rtt_file_data->rtt_mon_label42,
                      XmNlabelString, XmStringCreateLtoR(s2,XmSTRING_DEFAULT_CHARSET),
                      NULL);

        fscanf(fptr, "%s %s %s\n", dummy, s1, s2);
        XtVaSetValues(rtt_file_data->rtt_mon_label51,
                      XmNlabelString, XmStringCreateLtoR(s1,XmSTRING_DEFAULT_CHARSET),
                      NULL);
        XtVaSetValues(rtt_file_data->rtt_mon_label52,
                      XmNlabelString, XmStringCreateLtoR(s2,XmSTRING_DEFAULT_CHARSET),
                      NULL);

        fscanf(fptr, "%s %s %s %s\n", dummy, s1, s2, s3);
        XtVaSetValues(rtt_file_data->rtt_mon_label6xv,
                      XmNlabelString, XmStringCreateLtoR(s1,XmSTRING_DEFAULT_CHARSET),
                      NULL);
        XtVaSetValues(rtt_file_data->rtt_mon_label6yv,
                      XmNlabelString, XmStringCreateLtoR(s2,XmSTRING_DEFAULT_CHARSET),
                      NULL);
        XtVaSetValues(rtt_file_data->rtt_mon_label6zv,
                      XmNlabelString, XmStringCreateLtoR(s3,XmSTRING_DEFAULT_CHARSET),
                      NULL);

        fscanf(fptr, "%s %s\n", dummy, s1);
        XtVaSetValues(rtt_file_data->rtt_mon_label8,
                      XmNlabelString, XmStringCreateLtoR(s1,XmSTRING_DEFAULT_CHARSET),
                      NULL);

        fclose(fptr);
      }
    }
  
    if(strncmp(status,"DONE",4) == 0) done_flag = TRUE;
    if(done_flag)
    {
	XtVaSetValues(rtt_run_widgets->RunW, XmNsensitive, TRUE, NULL);
	XtVaSetValues(rtt_run_widgets->TestW, XmNsensitive, TRUE, NULL);
        rtt_file_data->rttRunInProgress = 0;
        
        DEBUG_TRACE_OUT printf("Leaving read_and_display\n");
        return;
    } else {
        rtt_file_data->interval_id = XtAppAddTimeOut(rtt_app,
                                                     RTT_RUN_TIMEOUT, read_and_display, NULL);
    };

    MT_free((void *)s1);
    MT_free((void *)s2);
    MT_free((void *)s3);

    DEBUG_TRACE_OUT printf("Leaving read_and_display\n");
}  
