void set_up_stuff ( );
void get_in_file_name ( char* );
void get_out_file_name ( char* );
void get_sample_rate ( int * );
void get_norm_factor ( float * );
void get_boron_conc ( float * );
void write_standard_commands ( FILE*, char * );
void position_fp_to_start_of_numbers ( FILE*, int*, char * );
void read_in_cols ( FILE*, col_struct*, int );
void read_in_ultra_cols ( FILE*, col_struct*, int, int* );
void write_axis_label ( col_struct*, FILE*, float );
void write_cols_to_file ( FILE*, col_struct* );		
void write_col (FILE *, float *,  float *, int, int);
void deactivate_sets (FILE *, col_struct *);
void process_variable_enablers (int, char**, col_struct *, int );
void check_if_logarithmic (col_struct*);

