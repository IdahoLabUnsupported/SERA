#ifndef UNDO_H
#define UNDO_H
#include "image_matrix.h"

void save_for_undo_no_restores(int);
void save_for_undo(int);
void undo(void);
void restore_undo (undo_type *);
void undo_on_imageCB(Widget, XtPointer, XtPointer);
void restoreCB(Widget, XtPointer, XtPointer);
void compress_undo_data(undo_type * undo_ptr);
void uncompress_undo_data(undo_type * undo_ptr);
unsigned char * get_undo_data(undo_type *);
void set_undo_invalid(undo_type *);
void start_undo_set(void);
void end_undo_set(void);
#endif
