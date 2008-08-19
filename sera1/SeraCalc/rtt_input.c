/* rtt_input.c - A function to read the input file
 *                for the Monte Carlo transport code.
 *  
 *  by: Ray S. Babcock, Montana State University
 *      January 9, 1997
 */

#include "rtt.h"
#include "rtt_mc.h"
#include "dialog_tools.h"
#include "file_tools.h"
#include <errno.h>
#include <sys/types.h>

/* local prototypes */
int get_and_store(FILE *, Widget);
int get_and_store_field(FILE *, Widget);
int get_trans_bs(FILE *);
void get_edit_dir(FILE *);
int get_iop(FILE *);
int get_isotopes(FILE *);
void get_exclude(char *);
void read_rtt_file(FILE *);
char * get_non_blank_line(FILE *);
int string_is_blank(char *);


void
rtt_input()
{
    char *slash_char;
    char * ptr;
    char s1[256];
    FILE * i_ptr;

    DEBUG_TRACE_IN printf("Entering rtt_input\n");
    
    /* Get file name from File Selection */
    CONFIRM = 1;
    XtManageChild(rtt_file_data->rtt_file_select);

    while (CONFIRM == 1)
        XtAppProcessEvent(rtt_app, XtIMAll);

    /*
     *  This prevents the code from trying to read a file called CANCEL
     *  in the event that the Cancel button is hit on the FSB
     */
    if (CONFIRM == 0) /* The cancel button wasn't pressed */
    {
        if( FT_filenameEndsIn( rtt_file_data->fname_in, ".input" ) && FT_fileExists( rtt_file_data->fname_in ) )
        {
            /* open file */
            i_ptr = fopen( rtt_file_data->fname_in, "r" );

            if( i_ptr != NULL )
            {
                /* Also keep the whole name of the original file */
                strcpy( rtt_file_data->originalInputFile, rtt_file_data->fname_in );
                rtt_file_data->originalInputFileValid = 1;
                
                slash_char = strrchr(rtt_file_data->fname_in, '/');

                if( slash_char != NULL )
                {
                    slash_char++;
                    *slash_char = '\0';                            /* get the directory */
                    strcpy( rtt_file_data->saveDirectory, rtt_file_data->fname_in );
                    rtt_file_data->saveDirectoryValid = 1;
                    
                    XmTextSetString(rtt_file_data->textinfo[0], rtt_file_data->saveDirectory);
                }
                rtt_file_data->iopInfoInFile = 0; /* assume no iop stuff until found */
                read_rtt_file(i_ptr);
            }
            else
            {
                sprintf(s1,"Could not input file %s\n", rtt_file_data->fname_in);
                DT_error ( rtt_file_data->rtt_shell, s1, "File error", NULL );
                DEBUG_TRACE_OUT printf("Leaving rtt_input\n");
                return;
            }
        }
        else {
            DT_error( rtt_file_data->rtt_shell, "You must select an existing .input file.", "File Error", NULL );
            DEBUG_TRACE_OUT printf("Leaving rtt_input\n");
            return;
        }
    }
    
    DEBUG_TRACE_OUT printf("Leaving rtt_input\n");
}

void 
read_rtt_file(FILE * i_ptr)
{
    int i;
    int nbatch, nhist, ntrk, nedit2;
    double wncut;
    char * s1;
    char * s2;
    char * sinp;
    char * ptr;
    char * textValue;
    double Xp,Yp,Zp;
    double Zb, phi, theta;
    double x1, x2, x3;
    Widget edit_text;
    
    DEBUG_TRACE_IN printf("Entering read_rtt_file\n");
    
    s1 = (char *) MT_malloc(80*sizeof(char));
    s2 = (char *) MT_malloc(120*sizeof(char));
    sinp = (char *) MT_malloc(100*sizeof(char));

    /* read file */

    /* Record 1 : title line */
    if ( get_and_store(i_ptr, rtt_file_data->rtt_title_text) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 1 (title line)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }

    /* Record 2 : nbatch, nhist, wncut (default 0.01) */
    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 2 (batch info)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }
    sscanf(sinp, "%d %d %le", &nbatch, &nhist, &wncut);

    sprintf(s1, "%d", nbatch);
    XmTextSetString(rtt_file_data->rtt_control_text1, s1);

    sprintf(s1, "%d", nhist);
    XmTextSetString(rtt_file_data->rtt_control_text2, s1);

    sprintf(s1, "%g", wncut);
    XmTextSetString(rtt_file_data->rtt_tally_text3, s1);

    /* Record 3 : Target Point */

    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 3 (target point)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }
    sscanf(sinp, "%lg %lg %lg", &Xp, &Yp, &Zp);
    sprintf(s1, "%g %g %g", Xp, Yp, Zp);
    XmTextSetString(rtt_file_data->rtt_position_text1, s1);

    /* Record 4 : Zb, phi, theta */
    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 4 (beam angles)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }
    sscanf(sinp, "%le %le %le", &Zb, &phi, &theta);
    
    sprintf(s1, "%lg", Zb);
    XmTextSetString(rtt_file_data->rtt_position_text2, s1);

    sprintf(s1, "%lg", phi);
    XmTextSetString(rtt_file_data->rtt_position_text3, s1);

    sprintf(s1, "%lg", theta);
    XmTextSetString(rtt_file_data->rtt_position_text4, s1 );
    
    /* Record 5 : CG geometry file name */
    if ( get_and_store(i_ptr, rtt_file_data->textinfo[1]) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 5 (CG geometry file)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }

    /* Record 6 : univel file name */
    if ( get_and_store(i_ptr, rtt_file_data->textinfo[2]) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 6 (univel file)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }

    /* Record 7 : edit_voxel_dimension number_energy_bins  */
    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 7 (voxel dimensions)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }
    sscanf(sinp, "%d %d", &nedit2, &(rtt_file_data->number_energy_bins));
    sprintf(s1, "%ld", nedit2);
    XmTextSetString(rtt_file_data->rtt_tally_text2a, s1);

    /* Record 8 : break points */
    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 8 (energy bins)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }
    sscanf(sinp, "%d %d %d", 
           &(rtt_file_data->break_point1),
           &(rtt_file_data->break_point2),
           &(rtt_file_data->break_point3));

    /* Record 9 : Low coordinates of subelement mesh */
    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 9 (subelement mesh)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }
    sscanf(sinp, "%le %le %le", &x1, &x2, &x3);
    sprintf(s1, "%g %g %g", x1, x2, x3);
    XmTextSetString(rtt_file_data->rtt_tally_text1, s1);

    /* Record 10: delw */
    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 10 (mesh size)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }
    sscanf(sinp, "%le %d", &x1, &rtt_file_data->ntrk);
    rtt_file_data->ntrk = 0;
    sprintf(s1, "%g", x1);
    XmTextSetString(rtt_file_data->rtt_tally_text2, s1);

    /* Record 11: old restart file or none */
    if ( get_and_store(i_ptr, rtt_file_data->textinfo[3]) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 11 (old restart file)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }

    /* Record 12: new restart file or none */
    if ( get_and_store(i_ptr, rtt_file_data->textinfo[4]) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 12 (new restart file)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }
    /*
     * We only want the filename for the new restart file, because
     * we are going to append it to the save directory.
     */
    textValue = XmTextGetString( rtt_file_data->textinfo[4] );
    if( strlen( textValue ) > 0 && strcmp( textValue, "none" ) != 0 )
    {
        ptr = strrchr( textValue, '/' );
        if( ptr != NULL )
        {
            ptr++;
            XmTextSetString( rtt_file_data->textinfo[4], ptr );
        }
    }
    XtFree( textValue );

    /* Record 13: beam source or dose table file name or none*/
    if ( get_and_store(i_ptr, rtt_file_data->textinfo[5]) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 13 (source file)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }

    /* Record 14: random_seed and debug_flag */
    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 14 (random seed)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }
    sscanf(sinp, "%d %d",
           &(rtt_file_data->random_seed),
           &(rtt_file_data->debug_flag));

    /* Record 15: run_dir run_date */
    XmTextSetString ( rtt_file_data->rtt_control_text3, "" );
    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 15 (run directives)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }
    sscanf(sinp, "%s %s", s1, s2);
    XmTextSetString(rtt_file_data->rtt_control_text3, s1);
    XmTextSetString(rtt_file_data->rtt_control_text4, s2);

    /* Record 16: material file name or none */
    if ( get_and_store(i_ptr, rtt_file_data->textinfo[6]) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 16 (material file)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }

    /* Record 17: cross section file name or none */
    if ( get_and_store(i_ptr, rtt_file_data->textinfo[7]) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 17 (cross section file)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }

    /* Record 18: */
    if ( get_isotopes ( i_ptr ) ) return;

    /* Record 19: code_vers */
    if ( get_and_store(i_ptr, rtt_file_data->textinfo[8]) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 19 (run script file)",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
        return;
    }

    /* Records 20-22: ultrafast data files */
    if ( strchr(s1, 'U') || strchr(s1, 'P') ) {
        if ( get_and_store(i_ptr, rtt_file_data->textinfo[9]) ) {
            DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 20 (ultrafast range file)",
                       "File read error", NULL );
            DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
            return;
        }
        if ( get_and_store(i_ptr, rtt_file_data->textinfo[10]) ) {
            DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 21 (ultrafast XS directory)",
                       "File read error", NULL );
            DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
            return;
        }
        if ( get_and_store(i_ptr, rtt_file_data->textinfo[11]) ) {
            DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading Record 22 (ultrafast XS file)",
                       "File read error", NULL );
            DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
            return;
        }
    }

    /* clear out any existing edit directives */
    edit_text = rtt_file_data->rtt_directive_text1;
    XmTextSetSelection(edit_text, 0, XmTextGetLastPosition(edit_text), (Time)0);
    XmTextRemove(edit_text);

    /* get preprocessor directives, if any */

    s1 = get_non_blank_line(i_ptr);

    while(! feof(i_ptr) ) {

        if(strncmp(s1, "#",1) == 0) {
            s1 = get_non_blank_line(i_ptr);
            continue;
        }

        if(strncmp(s1, " iop",4) == 0 || strncmp(s1, "iop", 3) == 0) {
            if (strncmp(s1," iop+",4) == 0 || strncmp(s1,"iop+",4) == 0) {
                rtt_file_data->rtt_iop_plusflag = 1;
                /*XmToggleButtonSetState ( rtt_file_data->rtt_iop_plus, TRUE, TRUE );*/
            }
            if ( get_iop(i_ptr) ) return;
            s1 = get_non_blank_line(i_ptr);
            continue;
        }

        if(strncmp(s1, " trans_bs",9) == 0 || strncmp(s1, "trans_bs",8) == 0) {
            DT_warn ( rtt_file_data->rtt_shell, "The trans_bs directive is no longer used in seraMC.\nThe trans_bs input data specified in your file is being ignored.", NULL, NULL );
            if ( get_trans_bs(i_ptr) ) return;
            s1 = get_non_blank_line(i_ptr);
            continue;
        }

        if(strncmp(s1, " exclude",8) == 0 || strncmp(s1, "exclude", 7) == 0) {
            DT_warn ( rtt_file_data->rtt_shell, "The exclude directive is no longer used in seraMC.\nThe exclude input data specified in your file is being ignored.", NULL, NULL );
            get_exclude(s1);
            s1 = get_non_blank_line(i_ptr);
            continue;
        }

        if(strncmp(s1, " edit_dir",9) == 0 || strncmp(s1, "edit_dir",8) == 0) {
            break;
        }
    
        sprintf(s2, "Skipping the following unrecognized pre-processor record.\n%s", s1);
        DT_warn ( rtt_file_data->rtt_shell, s2, "File read warning", NULL );

        s1 = get_non_blank_line(i_ptr);
    }  /* while */

    get_edit_dir(i_ptr);

    /* close file */
    fclose(i_ptr);

    MT_free((void *)s1);
    MT_free((void *)s2);

    DEBUG_TRACE_OUT printf("Leaving read_rtt_file\n");
    return;

} /* end of read_rtt_file */

int
get_and_store(FILE * fptr, Widget w)
{
    char * istring;
    int nread=0;

    DEBUG_TRACE_IN printf("Entering get_and_store\n");
    
    istring = (char *) MT_malloc(100*sizeof(char));
    if(fgets(istring, 100, fptr) == NULL)
        nread = 1;

    XmTextSetString(w, (char *) blank_trim(istring));
    MT_free((void *)istring);

    DEBUG_TRACE_OUT printf("Leaving get_and_store\n");
    return (nread);
} 

int
get_and_store_field(FILE * fptr, Widget w)
{
    char * istring;
    int nread=0;

    DEBUG_TRACE_IN printf("Entering get_and_store_field\n");
    
    istring = (char *) MT_malloc(100*sizeof(char));
    if(fgets(istring, 100, fptr) == NULL)
        nread = 1;

    XmTextFieldSetString(w, (char *) blank_trim(istring));
    MT_free((void *)istring);

    DEBUG_TRACE_OUT printf("Leaving get_and_store_field\n");
    return (nread);
} 

char *
get_non_blank_line(FILE * fptr)
{
    char * local_string;
    int i;
    char * sptr;

    DEBUG_TRACE_IN printf("Entering get_non_blank_line\n");
    
    local_string = (char *) MT_malloc(120*sizeof(char));
    sptr = local_string;

    for (i=0; i<120; i++)
    {
        *sptr = '\0';
        sptr++;
    }; 

    fgets(local_string, 120, fptr);
    while(!feof(fptr) && string_is_blank(local_string))
        fgets(local_string, 120, fptr);

    DEBUG_TRACE_OUT printf("Leaving get_non_blank_line\n");
    return (local_string);

}

int
string_is_blank(char * s)
{
    char * sptr;
    int length;
    int i;

    DEBUG_TRACE_IN printf("Entering string_is_blank\n");
    
    /* check string for all blanks */
    sptr = s;
    length = strlen(s);

    for(i=0; i<length; i++) {
        if(*sptr == '\0') {
            DEBUG_TRACE_OUT printf("Leaving string_is_blank\n");
            return(True);
        }

        if(*sptr != '\n' && *sptr != ' ') {
            DEBUG_TRACE_OUT printf("Leaving string_is_blank\n");
            return(False);
        }

        sptr++;
    }
    DEBUG_TRACE_OUT printf("Leaving string_is_blank\n");
    return(True);
}

int get_iop(FILE * i_ptr)
{
    int i;
    int iop;
    char * s1;
    char * s2;
    char * sinp;

    DEBUG_TRACE_IN printf("Entering get_iop\n");
    
    s1 = (char *) MT_malloc(120*sizeof(char));
    s2 = (char *) MT_malloc(120*sizeof(char));
    sinp = (char *) MT_malloc(100*sizeof(char));

    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading iop records block.",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving get_iop\n");
        return 1;
    }
    sscanf(sinp, "%d %s %s", &iop, s1, s2);

    rtt_file_data->rtt_iop_button = iop;
    strcpy( rtt_file_data->rtt_iop_regions[0], s1 );
    strcpy( rtt_file_data->rtt_iop_regions[1], s2 );

    if( !fgets( sinp, 100, i_ptr ) )
    {
        DT_error( rtt_file_data->rtt_shell,
                  "Fatal error while reading seraMC input file\nPresently reading zsep from iop block.",
                  "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving get_iop\n");
        return 1;
    }
    sscanf( sinp, "%f", &(rtt_file_data->rtt_iop_zsep) );

    /* get beamline coords for iop = 2 */
    if( iop == 2 )
    {
        if( !fgets( sinp, 100, i_ptr ) )
        {
            DT_error( rtt_file_data->rtt_shell,
                      "Fatal error while reading seraMC input file\nPresently reading beamline coords for iop = 2.",
                      "File read error", NULL );
            DEBUG_TRACE_OUT printf("Leaving get_iop\n");
            return 1;
        }
        sscanf( sinp, "%f %f %f",
                &(rtt_file_data->rtt_iop_beamline[0]),
                &(rtt_file_data->rtt_iop_beamline[1]),
                &(rtt_file_data->rtt_iop_beamline[2]) );
    }

    MT_free((void *)s1);
    MT_free((void *)s2);
    MT_free((void *)sinp);

    rtt_file_data->rtt_iop_saveflag = TRUE;
    rtt_file_data->iopInfoInFile = 1;
    
    DEBUG_TRACE_OUT printf("Leaving get_iop\n");

    return 0;
}

int get_trans_bs(FILE * i_ptr) 
{
    double e1, e2, e3;
    char *s1, *sinp;

    DEBUG_TRACE_IN printf("Entering get_trans_bs\n");
    
    s1 = (char *) MT_malloc(120*sizeof(char));
    sinp = (char *) MT_malloc(100*sizeof(char));

    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading trans_bs block record 1.",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving get_trans_bs\n");
        return 1;
    }
    sscanf(sinp, "%le %le %le\n", &e1, &e2, &e3);

/*
 *  Commenting out the following, as there isn't a trans_bs widget anymores
 */

/*  sprintf(s1, "%g", e1);
    XmTextFieldSetString(rtt_file_data->rtt_transbs_matrix[0], s1 );

    sprintf(s1, "%g", e2);
    XmTextFieldSetString(rtt_file_data->rtt_transbs_matrix[1], s1 );

    sprintf(s1, "%g", e3);
    XmTextFieldSetString(rtt_file_data->rtt_transbs_matrix[2], s1 ); */

    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading trans_bs block record 2.",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving get_trans_bs\n");
        return 1;
    }
    sscanf(sinp, "%le %le %le\n", &e1, &e2, &e3);

/*
 *  Commenting out the following, as there isn't a trans_bs widget anymores
 */

/*  sprintf(s1, "%g", e1);
    XmTextFieldSetString(rtt_file_data->rtt_transbs_matrix[3], s1 );

    sprintf(s1, "%g", e2);
    XmTextFieldSetString(rtt_file_data->rtt_transbs_matrix[4], s1 );

    sprintf(s1, "%g", e3);
    XmTextFieldSetString(rtt_file_data->rtt_transbs_matrix[5], s1 ); */

    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading trans_bs block record 3.",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving get_trans_bs\n");
        return 1;
    }
    sscanf(sinp, "%le %le %le\n", &e1, &e2, &e3);

/*
 *  Commenting out the following, as there isn't a trans_bs widget anymores
 */

/*  sprintf(s1, "%g", e1);
    XmTextFieldSetString(rtt_file_data->rtt_transbs_matrix[6], s1 );

    sprintf(s1, "%g", e2);
    XmTextFieldSetString(rtt_file_data->rtt_transbs_matrix[7], s1 );

    sprintf(s1, "%g", e3);
    XmTextFieldSetString(rtt_file_data->rtt_transbs_matrix[8], s1 ); */

    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading trans_bs block record 4.",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving get_trans_bs\n");
        return 1;
    }
    sscanf(sinp, "%le\n", &e1); 
/*
 *  Commenting out the following, as there isn't a trans_bs widget anymores
 */
/*  sprintf(s1, "%g", e1);
    XmTextFieldSetString(rtt_file_data->rtt_transbs_scale, s1);

    if ( get_and_store_field(i_ptr, rtt_file_data->rtt_transbs_reindex) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading trans_bs block record 4.",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving get_trans_bs\n");
        return 1;
    } */
/*
 *  Replacement code to read last record in trans_bs block - delete
 *  if resurrecting this section
 */
    if ( !fgets ( sinp, 100, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading trans_bs block record 5.",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving get_trans_bs\n");
        return 1;
    }

    rtt_file_data->rtt_transbs_saveflag = TRUE;

    MT_free((void *)s1);
    MT_free((void *)sinp);

    DEBUG_TRACE_OUT printf("Leaving get_trans_bs\n");

    return 0;
}

void get_exclude(char * s1)
{
    char * local_s;

    DEBUG_TRACE_IN printf("Entering get_exclude\n");
    
    local_s = (char *) MT_malloc((strlen(s1)+1)*sizeof(char));
    strcpy(local_s, s1+8);

/*
 *  Commenting this out, because there isn't an exclude widget anymore
 */

/*  XmTextFieldSetString(rtt_file_data->rtt_excluded_text1, 
                         (char *) blank_trim(local_s)); */
    rtt_file_data->rtt_excluded_saveflag = TRUE;
    MT_free((void *)local_s);

    DEBUG_TRACE_OUT printf("Leaving get_exclude\n");
}

int get_isotopes(FILE * i_ptr)
{
    char *s1, *s2, *sinp;
    int  j=0, k=0;

    DEBUG_TRACE_IN printf("Entering get_isotopes\n");
    
    s1 = (char *) MT_malloc(120*sizeof(char));
    s2 = (char *) MT_malloc(80*sizeof(char));
    sinp = (char *) MT_malloc(120*sizeof(char));

    if ( !fgets ( sinp, 120, i_ptr ) ) {
        DT_error ( rtt_file_data->rtt_shell, "Fatal error while reading seraMC input file\nPresently reading isotopes and densities.",
                   "File read error", NULL );
        DEBUG_TRACE_OUT printf("Leaving get_isotopes\n");
        return 1;
    }

/*
 *  Count number of entries on record.  Half that is the
 *  number of isotopes actually supplied.
 */
    s2 = strtok ( sinp, " " );
    while ( s2 != NULL ) {
        if ( strchr( s2, '.' ) ) {
            rtt_file_data->nuclide_density[k] = strtod( s2, NULL );
            k++;
        }
        else {
            rtt_file_data->nuclide_id[j] = atoi( s2 );
            j++;
        }
        s2 = strtok ( NULL, " " );
    }

    sprintf(s1, "%d", rtt_file_data->nuclide_id[0]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text1a, s1);
    sprintf(s1, "%g", rtt_file_data->nuclide_density[0]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text1b, s1);

    sprintf(s1, "%d", rtt_file_data->nuclide_id[1]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text2a, s1);
    sprintf(s1, "%g", rtt_file_data->nuclide_density[1]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text2b, s1);

    sprintf(s1, "%d", rtt_file_data->nuclide_id[2]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text3a, s1);
    sprintf(s1, "%g", rtt_file_data->nuclide_density[2]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text3b, s1);

    sprintf(s1, "%d", rtt_file_data->nuclide_id[3]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text4a, s1);
    sprintf(s1, "%g", rtt_file_data->nuclide_density[3]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text4b, s1);

    sprintf(s1, "%d", rtt_file_data->nuclide_id[4]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text5a, s1);
    sprintf(s1, "%g", rtt_file_data->nuclide_density[4]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text5b, s1);

    sprintf(s1, "%d", rtt_file_data->nuclide_id[5]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text6a, s1);
    sprintf(s1, "%g", rtt_file_data->nuclide_density[5]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text6b, s1);

    sprintf(s1, "%d", rtt_file_data->nuclide_id[6]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text7a, s1);
    sprintf(s1, "%g", rtt_file_data->nuclide_density[6]);
    XmTextFieldSetString(rtt_file_data->rtt_isotopes_text7b, s1);

    rtt_file_data->rtt_isotopes_saveflag = TRUE;

    MT_free((void *)s1);
    MT_free((void *)s2);
    MT_free((void *)sinp);

    DEBUG_TRACE_OUT printf("Leaving get_isotopes\n");

    return 0;
}

void get_edit_dir(FILE * i_ptr)
{
    char * s1;
    Widget w_id;

    DEBUG_TRACE_IN printf("Entering get_edit_dir\n");
    
    s1 = (char *) MT_malloc(120*sizeof(char));
    w_id = rtt_file_data->rtt_directive_text1;

    fgets(s1, 120, i_ptr);
    while (! feof(i_ptr)) {
        XmTextInsert(w_id, XmTextGetLastPosition(w_id), (char *) blank_trim(s1));
        if(!string_is_blank(s1))
            XmTextInsert(w_id, XmTextGetLastPosition(w_id), "\n");
        fgets(s1, 120, i_ptr);
    }

    MT_free((void *)s1);

    DEBUG_TRACE_OUT printf("Leaving get_edit_dir\n");
    return;

}
