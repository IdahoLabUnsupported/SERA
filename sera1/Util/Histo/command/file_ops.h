void set_up_stuff ( );
void get_in_file_name ( char* );
void get_out_file_name ( char* );
void write_standard_commands ( FILE* );
void write_titles_to ( FILE*, int );
void position_fp_to_start_of_numbers ( FILE*, int*, int* );
void read_in_cols ( FILE*, dose_vol_struct*, int );
void read_in_reference ( FILE*, dose_vol_struct*, int, int );
void write_cols_to_file ( FILE*, dose_vol_struct*, int, int );
void write_col (FILE *, bin *,  float *, int);
void write_col2 (FILE *, bin *,  float *, int, float);
void deactivate_sets (FILE *, dose_vol_struct *);
int process_variable_enablers (int, char**, dose_vol_struct *, int);
int read_norm_by_total ( int, char**, int );

