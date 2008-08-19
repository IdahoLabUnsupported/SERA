#define MAX_NUMBER_POINTS 500
#define MAX_NUMBER_LINES 500
#define MAX_LENGTH_LABEL_STRING 64
#define MAX_LENGTH_STRING 256

#define MAX_NUMBER_MAXIMUM_POINTS 400 /* CLA-> set to 400 from 15, 4-7-98*/



/* 
 * Points structure.  This is basically a 3-D array.  The first component
 * represents a given line segment.  The second component is the 
 * x-components of a given point and the third is the y-components.  
 */
 
typedef struct line_type_ {
   float points[MAX_NUMBER_POINTS][2];
   short number_points;
   short color;
   char label[MAX_LENGTH_LABEL_STRING];
   float value;
} LINE_TYPE;
 
typedef struct contour_lines_type_ {
   short number_lines;
   LINE_TYPE lines[MAX_NUMBER_LINES];
} CONTOUR_LINES_TYPE;


typedef struct value_color_record_struct 
{
   float contour_value;
   int line_color;
} value_color_record_type;



void draw_contours (floyd_data_ptr data, int whichDose, 
                    Boolean recalculate_contours, Boolean draw_lines,
                    Boolean draw_colorwash, Boolean draw_large_labels,
                    Boolean draw_preview_labels, Boolean scale_font);
