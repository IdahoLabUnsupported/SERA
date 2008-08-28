
/* ===================================================> bnct_dicom.h
 *
 * Definitions for the dicom support in bnct.
 *
 * ====================================================================
 */

/* #include <values.h> */
#include <limits.h>

#define MAXSHORT SHRT_MAX

#define FALSE     0
#define TRUE      ~FALSE

/*
 * UID constants
 */

#define DEFAULT_TRANSFER_UID          "1.2.840.10008.1.2"
#define EXPLICIT_LITTLE_ENDIAN_UID    "1.2.840.10008.1.2.1"
#define EXPLICIT_BIG_ENDIAN_UID       "1.2.840.10008.1.2.2"
#define LOSSLESS_JPEG_UID             "1.2.840.10008.1.2.4.70"
#define LOSSY_JPEG_8BIT_UID           "1.2.840.10008.1.2.4.50"
#define LOSSY_JPEG_12BIT_UID          "1.2.840.10008.1.2.4.51"


#define UNKNOWN_PHOTOINTERP           0
#define GRAY_PHOTOINTERP              1
#define RGB_PHOTOINTERP               2

/*
 * File types
 */

enum MACHINE_TYPE {BIG_ENDIAN_TYPE, LITTLE_ENDIAN_TYPE};
typedef enum MACHINE_TYPE machine_type_T;

enum FILE_TYPE {DICOM_FILE, NEMA_FILE};
typedef enum FILE_TYPE file_type_T;

enum ENCODING_TYPE {IMPLICIT, EXPLICIT};
typedef enum ENCODING_TYPE encoding_type_T;

enum JPEG_TYPE {NOJPEG, LOSSLESS, LOSSY_8BIT, LOSSY12BIT};
typedef enum JPEG_TYPE jpeg_type_T;

enum TRANSFER_TYPE {IMP_LITTLE_ENDIAN, EXP_LITTLE_ENDIAN, EXP_BIG_ENDIAN};
typedef enum TRANSFER_TYPE transfer_type_T;


/*
 * DICOM3 Value Representation types, plus XX for bad type.
 */

enum              VRTYPE   {AE, AS, AT, CS, DA, DL, DS, DT, FL, FD, IS, LO, 
                            LT, NA, OB, OW, OX, PN, SH, SL, SQ, SS, ST, TM, UI, 
                            UL, US, XS, XX, NONE};

typedef enum VRTYPE  VRTYPE_T;  


/*
 * Structures for the internal dictionary of group and element names,
 * VR's and string identifiers.
 */

typedef char      DE_NAME [64];
#define VM_MAX    MAXSHORT

typedef struct
{
   short       dd_group;
   short       dd_element;
   char        *dd_vrep;
   short       dd_multiplicity_min;
   short       dd_multiplicity_max;
   char        *dd_owner;
   char        *dd_name;
} DD_ENTRY_T;

typedef struct 
{
   DD_ENTRY_T    de_entry;
   int         de_len;
} DELEMENT_T;


/*
 * definitions for passing data between applications and dicom
 * library files.
 */

#define UNKNOWN     0xFFFF
#define CHAR_TYPE        0
#define INT_TYPE         1
#define SHORT_TYPE       2
#define FLOAT_TYPE      10 
#define DECIMAL_TYPE    20
#define DATAVAL_TYPE    30 

typedef union datatype_U
{
   char              *dv_char;
   unsigned short    *dv_ushort;
   unsigned long     *dv_ulong;
   float             *dv_float;
   double            *dv_double;
} datatype_T;

typedef struct
{
   int        dv_type;
   int        dv_num;
   void       *dv_value;
} dataval_T;         

/*
 * Special group and element pairs for qsh conversion.
 */

#define DCM_CREATION_DATE_GRP           0x0008
#define DCM_CREATION_DATE_ELM           0x0012

#define DCM_STUDY_DATE_GRP              0x0008
#define DCM_STUDY_DATE_ELM              0x0020

#define DCM_SOP_CLASS_GRP               0x0008
#define DCM_SOP_CLASS_ELM               0x0016

#define DCM_MODALITY_GRP                0x0008
#define DCM_MODALITY_ELM                0x0060  /* CR, CT, MR, ... */

#define DCM_PATIENT_NAME_GRP            0x0010
#define DCM_PATIENT_NAME_ELM            0x0010

#define DCM_PATIENT_BIRTHDATE_GRP       0x0010
#define DCM_PATIENT_BIRTHDATE_ELM       0x0030

#define DCM_SLICE_THICKNESS_GRP         0x0018
#define DCM_SLICE_THICKNESS_ELM         0x0050

#define DCM_ECHO_NUMBER_GRP             0x0018
#define DCM_ECHO_NUMBER_ELM             0x0086

#define DCM_SLICE_SPACING_GRP           0x0018
#define DCM_SLICE_SPACING_ELM           0x0088

#define DCM_PATIENT_POSITION_GRP        0x0018
#define DCM_PATIENT_POSITION_ELM        0x5100

#define DCM_PIXEL_X_SIZE_GRP            0x0018
#define DCM_PIXEL_X_SIZE_ELM            0x6028

#define DCM_PIXEL_Y_SIZE_GRP            0x0018
#define DCM_PIXEL_Y_SIZE_ELM            0x602A

#define DCM_IMAGE_POSITION_GRP          0x0020
#define DCM_IMAGE_POSITION_ELM          0x0032

#define DCM_ORIENTATION_GRP             0x0020
#define DCM_ORIENTATION_ELM             0x0037

#define DCM_FRAMEOFREF_GROUP            0x0020
#define DCM_FRAMEOFREF_ELM              0x0052

#define DCM_FRAMEOFREFPOS_GROUP         0x0020
#define DCM_FRAMEOFREFPOS_ELM           0x1040

#define DCM_SLICE_LOCATION_GRP          0x0020
#define DCM_SLICE_LOCATION_ELM          0x1041

#define DCM_SAMPLES_PER_PIXEL_GRP       0x0028
#define DCM_SAMPLES_PER_PIXEL_ELM       0x0002

#define DCM_PHOTMETRIC_INTERP_GRP       0x0028
#define DCM_PHOTMETRIC_INTERP_ELM       0x0004

#define DCM_PLANAR_CONFIG_GRP           0x0028
#define DCM_PLANAR_CONFIG_ELM           0x0006

#define DCM_ROWS_GRP                    0x0028
#define DCM_ROWS_ELM                    0x0010

#define DCM_COLS_GRP                    0x0028
#define DCM_COLS_ELM                    0x0011

#define DCM_PIXEL_SPACING_GRP           0x0028
#define DCM_PIXEL_SPACING_ELM           0x0030

#define DCM_PIXEL_ASPECT_RATIO_GRP      0x0028
#define DCM_PIXEL_ASPECT_RATIO_ELM      0x0034

#define DCM_BITS_ALLOCATED_GRP          0x0028
#define DCM_BITS_ALLOCATED_ELM          0x0100

#define DCM_BITS_STORED_GRP             0x0028
#define DCM_BITS_STORED_ELM             0x0101

#define DCM_HIGH_BIT_GRP                0x0028
#define DCM_HIGH_BIT_ELM                0x0102

#define DCM_PIXEL_REP_GRP               0x0028
#define DCM_PIXEL_REP_ELM               0x0103

#define DCM_SMALLEST_PIXVAL_GRP         0x0028
#define DCM_SMALLEST_PIXVAL_ELM         0x0106

#define DCM_LARGEST_PIXVAL_GRP          0x0028
#define DCM_LARGEST_PIXVAL_ELM          0x0107

#define DCM_PIXEL_DATA_GRP              0x7FE0
#define DCM_PIXEL_DATA_ELM              0x0010

/* 
 * Prototypes
 */

FILE *DcmOpenInputFile (char *name);
int DcmInitSearch (FILE *);
int DcmReadTag (FILE *, void *, unsigned int, unsigned int, 
                unsigned short, unsigned short);
char *DcmReadPixelData (FILE *, int *, int *);

int DcmGetImageInfo (FILE *, long *, long *, long *, long *);

int DcmConvertToQsh (int, char **, char *);

int GetMachineType ();
int GetTransferSyntaxType ();

void DcmAbort (char *);
