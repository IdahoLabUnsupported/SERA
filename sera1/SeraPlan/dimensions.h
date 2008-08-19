#ifndef DIMENSIONS_H 
#define DIMENSIONS_H
#include "dose.h"

extern plan_data         data;
extern edit_data_struct  edit_data;

void  initialize_data ( );
void  set_up_mem ( panel_data * );
void  set_up_dose_mem ( dose_struct *, int );
void  set_up_edit_mem ( edit_data_struct * );
void  edit_panel_mem_setup ( edit_panel_struct * );
void  results_mem_setup ( edit_results_struct *, int, int, int, int );

#endif /* ifndef DIMENSIONS_H */
