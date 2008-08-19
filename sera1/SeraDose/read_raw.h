#ifndef READ_RAWH
#define READ_RAWH

#ifdef DECLARATION
#define externOrNot /* nil */
#else
#define externOrNot extern
#endif  /* DECLARATION */

typedef struct _qhd_data_type {
    int dimx;
    int dimy;
    int BPP;  /* bytes per pixel         */
    int swap; /* 1 if need to swap bytes */
} qhd_data_type;

externOrNot qhd_data_type qhd_data;

externOrNot unsigned char* read_raw(char *infilename, qhd_data_type *qhd_data, int ival);
#endif
