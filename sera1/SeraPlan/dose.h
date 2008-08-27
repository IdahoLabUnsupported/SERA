#ifndef DOSE_H
#define DOSE_H
/*
 *  Data structure for the dose data from the .rst file(s)
 *  Dynamically-allocated storage
 */

#define  MAX_RUNDIR    6
#define  MAX_VERS     12
#define  MAX_NNED     14
#define  LN_DATE      15
#define  MAX_NEDIT   120
#define  MAX_RTTFILE  80
#define  MAX_GROUPS    3

typedef struct {

   char    *run_dir, *date, *sourcefile, *new_rst;

   int     *nnhist, *nghist, *nfhist, *irand_num;
   int     *nbatch, *nhist, *id_b10, *id_h, *id_n, *id_c, *id_o, *id_rr1, *id_rr2;

   double  *rel_wt, *wn0, *s_tot, *gamratio, *xp, *yp, *zp, *zb, *phi, *theta;
   double  *wncut, *b10_dens, *h_dens, *n_dens, *c_dens, *o_dens, *rr1_dens, *rr2_dens;
   double  *entry, *bvec;

} set_struct;

typedef struct {

   int       nset;
   int       nedit, *nedt, *iged;

   char      *geomfile, *bsg_file, *matfile, *sigmafile, *code_vers;
   char      *plan_file_name, *title, *vers_stmp;
   
   double    *x0, *y0, *z0, *delw;

   set_struct  *sets;

   float     *bflux;

} dose_struct;

#endif /* ifndef DOSE_H */
