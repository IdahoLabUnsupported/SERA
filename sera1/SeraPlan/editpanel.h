/*
 *   Widgets for the edit input popups
 */

typedef struct {

   Widget popup, form, numlab, numframe, numform, numtext, label;

} inp_struct;

typedef struct {

   Widget  frame, form, label, datframe, data;

} point_inp;

/*
 *  Base widgets in popups
 */
typedef struct {

   Widget but_frame, but_form, done_fr, done_but, save_fr, save_but;

} base_struct;

/*
 *  Point edit popup
 */
typedef struct {

   inp_struct  *number;
   point_inp   *location;
   base_struct *base;

} point_panel_struct;


/*
 *  Line edit popup
 */
typedef struct {

   inp_struct  *number;
   point_inp   *delta, *begin, *end;
   base_struct *base;

} line_panel_struct;


/*
 *  Box edit popup
 */
typedef struct {

   inp_struct  *number;
   point_inp   *bodies;
   base_struct *base;

} box_panel_struct;


/*
 *  Contour edit popup
 */
typedef struct {

   inp_struct  *number;
   point_inp   *file, *point, *vector1, *vector2;
   base_struct *base;

} contour_panel_struct;


typedef struct {

   point_panel_struct    *point;

   line_panel_struct     *line;

   box_panel_struct      *box;

   contour_panel_struct  *contour;

} edit_panel_struct;
