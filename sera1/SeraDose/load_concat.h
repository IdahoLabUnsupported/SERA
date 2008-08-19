#ifndef LOAD_CONCAT_H
#define LOAD_CONCAT_H

#include "global.h"
#include "commonfcns.h"
#include "picshell.h"
#include "keyval_tools.h"
#include "dialog_tools.h"
#include "libsz.h"
#include <Xm/MessageB.h>

typedef struct _dose_concat_t
{
    float FOV;
    float xmin;
    float xmax;
    float ymin;
    float ymax;

    int x_in_field;
    int y_in_field;
    int z_in_field;
    int num_cols;
    int num_rows;
    int num_points;
    int num_planes;
    
    float totalConc;
    float boronConc;
    float gammaConc;
    float nitrogenConc;
    float fastConc;
    float fastFluenceConc;
    float epithermalFluenceConc;
    float thermalFluenceConc;
    float otherConc;
    
    float totalRBE;
    float boronRBE;
    float gammaRBE;
    float nitrogenRBE;
    float fastRBE;
    float fastFluenceRBE;
    float epithermalFluenceRBE;
    float thermalFluenceRBE;
    float otherRBE;
    
    float totalRef;
    float boronRef;
    float gammaRef;
    float nitrogenRef;
    float fastRef;
    float fastFluenceRef;
    float epithermalFluenceRef;
    float thermalFluenceRef;
    float otherRef;
    
    int abscissa;
    int ordinate;

    float *z_val;
    char **filename;

    char run_title[256];
    
} dose_concat_t;

int read_concatenated_header_file ( char *headerFile, dose_concat_t *concat_data );

void load_concatenated_contour_file ( char *headerFilename, char *contourFilename );
void load_concatenated_mask_file ( char *headerFilename, char *maskFilename );

int load_3d_contour_file ( char *contourFilename, dose_concat_t *concat_data );

int create_temp_mask_files ( char *maskFilename, dose_concat_t *concat_data );

int create_single_temp_mask_file ( char *filename, unsigned char **fileArray, dose_concat_t *concat_data );

void load_temp_contour_files ( dose_concat_t *concat_data );
void load_temp_mask_files ( dose_concat_t *concat_data );

void free_concat_data ( dose_concat_t *concat_data );


#endif /* LOAD_CONCAT_H */
