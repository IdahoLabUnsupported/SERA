/* Structures and typedefs for bnct_rtpe 
 *
 * August 11, 1994
 * Ray S. Babcock
 */

/* added 12/5/95 to complete removal of utah code from callbacks.c */
typedef char * string_type;

#ifndef SINGLE_FLOATS     /* cc arg -DSINGLE_FLOATS disables double floats. */
typedef double real_type;
#else
typedef float real_type;
#endif /* SINGLE_FLOATS */

/* Structure containing rtt Run menu widgets for callbacks */
typedef struct _RttRunSt
{
    Widget RunW;
    Widget TestW;
    Widget KillW;
} RttRunSt;

/* Structure containing rtt popup data for a rtt_edit_popup window */
typedef struct _RttEditPopupSt
{
    Widget rtt_shell,
	rtt_file_select,
        rtt_file_form1,
	textinfo[12],
	button[12],
	select_file,
	rtt_title_text,
 	rtt_control_text1,
	rtt_control_text2,
	rtt_control_text3,
	rtt_control_text4,
	rtt_position_text1,
	rtt_position_text2,
	rtt_position_text3,
	rtt_position_text4,
	rtt_tally_text1,
	rtt_tally_text2a,
	rtt_tally_text2,
	rtt_tally_text3,
        rtt_excluded_text1,
	rtt_transbs_matrix[9],
	rtt_transbs_scale,
	rtt_transbs_reindex,
        rtt_iop_buttons[5],
        rtt_iop_plus,
	rtt_iop_text2,
	rtt_iop_text3,
	rtt_iop_text4,
	rtt_iop_text5,
	rtt_isotopes_text1a,
	rtt_isotopes_text1b,
	rtt_isotopes_text2a,
	rtt_isotopes_text2b,
	rtt_isotopes_text3a,
	rtt_isotopes_text3b,
	rtt_isotopes_text4a,
	rtt_isotopes_text4b,
	rtt_isotopes_text5a,
	rtt_isotopes_text5b,
	rtt_isotopes_text6a,
	rtt_isotopes_text6b,
	rtt_isotopes_text7a,
	rtt_isotopes_text7b,
        rtt_directive_text1,
	rtt_status_text,
	rtt_mon_label21,
	rtt_mon_label22,
	rtt_mon_label31,
	rtt_mon_label32,
	rtt_mon_label41,
	rtt_mon_label42,
	rtt_mon_label51,
	rtt_mon_label52,
	rtt_mon_label6xv,
	rtt_mon_label6yv,
	rtt_mon_label6zv,
	rtt_mon_label8;
    
    int	rtt_isotopes_saveflag,
   	rtt_transbs_saveflag,
	rtt_excluded_saveflag,
        rtt_run_timeout,
        rttRunInProgress;

    /* data from the iop widget */
    int   rtt_iop_saveflag;
    int   rtt_iop_plusflag;
    int   rtt_iop_button;  /* 1..5  NOT  0..4 */  
    char  rtt_iop_regions[2][256];
    float rtt_iop_zsep;
    float rtt_iop_beamline[3];
    int   iopInfoInFile;
    int   rtt_iop_prompt_for_apply;

    XtIntervalId interval_id;
    char   stringName[12][50];
    char   *buttons[12];
    char   *fields[12];
    char   *fileName[12];
    char   fname_in[256];
    
    char   originalInputFile[256];/* file loaded with Open, used for subsequent Saves */
    int    originalInputFileValid;/* becomes valid with Open or Save As */
    
    char   saveDirectory[256];    /* base directory to save files to */
    int    saveDirectoryValid;    /* becomes valid with Open, Save, or Save As */

    int    buttonNumber;
    int    numFields;
    /* locations for values not in rtt popup widget
     * but in input file 
     */
    int    edit_voxel_dimension; /* Record 7 */
    int    number_energy_bins;   /*   "    " */
    int    break_point1;         /* Record 8 */
    int    break_point2;         /*   "    " */
    int    break_point3;         /*   "    " */
    int    ntrk;                 /* Record 10 */
    int    random_seed;          /* Record 14 */
    int    debug_flag;           /*   "     " */
    int    nuclide_id[7];          /* Record 18 */
    double nuclide_density[7];     /*   "     " */
} RttEditPopupSt;
