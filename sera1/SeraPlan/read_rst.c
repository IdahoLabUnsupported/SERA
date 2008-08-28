#include <stdio.h>
#include <stdlib.h>
#include "data.h"
#include "editdata.h"
#include "dose.h"

void set_up_dose_mem ( dose_struct *dose, int num_sets )

{

   int   i;

   dose->nset = 0;

   dose->nedt = (int *) malloc ( sizeof ( int ) );
   dose->iged = (int *) malloc ( MAX_GROUPS * sizeof ( int ) );


   dose->vers_stmp = (char *) malloc ( MAX_VERS * sizeof ( char ) );

   dose->plan_file_name = (char *) malloc ( MAX_FILE * sizeof ( char ) );

   dose->title = (char *) malloc ( MAX_FILE * sizeof ( char ) );
   dose->geomfile = (char *) malloc ( MAX_FILE * sizeof ( char ) );
   dose->bsg_file = (char *) malloc ( MAX_FILE * sizeof ( char ) );
   dose->matfile = (char *) malloc ( MAX_FILE * sizeof ( char ) );
   dose->sigmafile = (char *) malloc ( MAX_FILE * sizeof ( char ) );
   dose->code_vers = (char *) malloc ( MAX_VERS * sizeof ( char ) );


   dose->x0 = (double *) malloc ( sizeof ( double ) );
   dose->y0 = (double *) malloc ( sizeof ( double ) );
   dose->z0 = (double *) malloc ( sizeof ( double ) );
   dose->delw = (double *) malloc ( sizeof ( double ) );


   dose->sets = (set_struct *) malloc ( num_sets * MAX_RUNDIR * sizeof ( set_struct ) );
   for ( i = 0; i < num_sets * MAX_RUNDIR; i++ ) {

      dose->sets[i].run_dir = (char *) malloc ( MAX_RUNDIR * sizeof ( char ) );
      dose->sets[i].date = (char *) malloc ( LN_DATE * sizeof ( char ) );
      dose->sets[i].sourcefile = (char *) malloc ( MAX_FILE * sizeof ( char ) );
      dose->sets[i].new_rst = (char *) malloc ( MAX_FILE * sizeof ( char ) );

      dose->sets[i].nnhist = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].nghist = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].nfhist = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].irand_num = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].nbatch = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].nhist = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].id_b10 = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].id_h = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].id_n = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].id_c = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].id_o = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].id_rr1 = (int *) malloc ( sizeof ( int ) );
      dose->sets[i].id_rr2 = (int *) malloc ( sizeof ( int ) );

      dose->sets[i].rel_wt = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].wn0 = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].s_tot = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].gamratio = (double *) malloc ( sizeof ( double ) );

      dose->sets[i].xp = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].yp = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].zp = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].zb = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].phi = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].theta = (double *) malloc ( sizeof ( double ) );

      dose->sets[i].wncut = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].b10_dens = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].h_dens = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].n_dens = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].c_dens = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].o_dens = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].rr1_dens = (double *) malloc ( sizeof ( double ) );
      dose->sets[i].rr2_dens = (double *) malloc ( sizeof ( double ) );

      dose->sets[i].entry = (double *) malloc ( 6*sizeof ( double ) );
      dose->sets[i].bvec = (double *) malloc ( 3*sizeof ( double ) );
   }

   return;

}


void read_rst_file ( dose_struct *dose, FILE *file, int *irr )

{

  char test; //, tmpstr[MAX_FILE];
   int  bsize, i, j, nned;

/*
 *  Read number of data sets, case title, file names, and rtt_MC code version
 */


   *irr = 0;

   fscanf ( file, "%4d%12s\n", &dose->nset, dose->vers_stmp );
   fscanf ( file, "%80s\n", dose->plan_file_name );
   fscanf ( file, "%80c\n", dose->title );
   fscanf ( file, "%80s\n", dose->geomfile );
   fscanf ( file, "%80s\n", dose->bsg_file );
   fscanf ( file, "%80s\n", dose->matfile );
   fscanf ( file, "%80s\n", dose->sigmafile );
   fscanf ( file, "%12s\n", dose->code_vers );

/*
 *  Read edit mesh information and number of energy groups
 */


   fscanf ( file, "%15le", dose->x0);
   fscanf ( file, "%15le", dose->y0);
   fscanf ( file, "%15le", dose->z0);
   fscanf ( file, "%15le", dose->delw);
   fscanf ( file, "%8d", &dose->nedit);
   fscanf ( file, "%8d\n", dose->nedt);
   for ( i = 0; i < *dose->nedt; i++ ) {
      if ( (i+1)%10 && (i+1)%*dose->nedt )
         fscanf ( file, "%8d", &dose->iged[i] );
      else
         fscanf ( file, "%8d\n", &dose->iged[i] );
   }

/*
 *  Read edit directives, date, source info, number of histories run, 
 *  gamma ratio, sourcefile, and the starting random number
 */


   for ( i = 0; i < dose->nset; i++ ) {
      fscanf ( file, "%6c", dose->sets[i].run_dir );
      fscanf ( file, "%15c", dose->sets[i].date );
      fscanf ( file, "%15le", dose->sets[i].rel_wt );
      fscanf ( file, "%15le", dose->sets[i].wn0 );
      fscanf ( file, "%15le\n", dose->sets[i].s_tot );
      fscanf ( file, "%d", dose->sets[i].nnhist );
      fscanf ( file, "%d", dose->sets[i].nghist );
      fscanf ( file, "%d", dose->sets[i].nfhist );
      fscanf ( file, "%le\n", dose->sets[i].gamratio );
      fscanf ( file, "%80s\n", dose->sets[i].sourcefile );
      fscanf ( file, "%10d\n", dose->sets[i].irand_num );

/*
 *  Read source positioning parameters and restart file name (input)
 */

      fscanf ( file, "%15le", dose->sets[i].xp );
      fscanf ( file, "%15le", dose->sets[i].yp );
      fscanf ( file, "%15le", dose->sets[i].zp );
      fscanf ( file, "%15le", dose->sets[i].zb );
      fscanf ( file, "%15le", dose->sets[i].phi );
      fscanf ( file, "%15le\n", dose->sets[i].theta );
      fscanf ( file, "%80s\n", dose->sets[i].new_rst );

/*
 *  Read # batches, # histories, cutoff weight, nuclide IDs, and nuclide densities
 */

      fscanf ( file, "%8d", dose->sets[i].nbatch );
      fscanf ( file, "%8d", dose->sets[i].nhist );
      fscanf ( file, "%15le", dose->sets[i].wncut );
      fscanf ( file, "%8d", dose->sets[i].id_b10 );
      fscanf ( file, "%8d", dose->sets[i].id_h );
      fscanf ( file, "%8d", dose->sets[i].id_n );
      fscanf ( file, "%8d", dose->sets[i].id_c );
      fscanf ( file, "%8d", dose->sets[i].id_o );
      fscanf ( file, "%c", &test );
      if ( test == ' ' ) {
         *irr = 1;
         fscanf ( file, "%7d", dose->sets[i].id_rr1 );
         fscanf ( file, "%8d\n", dose->sets[i].id_rr2 );
      }
      fscanf ( file, "%15le", dose->sets[i].b10_dens );
      fscanf ( file, "%15le", dose->sets[i].h_dens );
      fscanf ( file, "%15le", dose->sets[i].n_dens );
      fscanf ( file, "%15le", dose->sets[i].c_dens );
      fscanf ( file, "%15le", dose->sets[i].o_dens );
      if ( *irr ) {
         fscanf ( file, "%15le", dose->sets[i].rr1_dens );
         fscanf ( file, "%15le\n", dose->sets[i].rr2_dens );
      }
      else {
         fscanf ( file, "\n" );
      }

/*
 *  Read the beam entry point, and the beam directional vector
 */

      for ( j = 0; j < 3; j++ )
         fscanf ( file, "%15le", &dose->sets[i].entry[j] );

      for ( j = 0; j < 2; j++ )
         fscanf ( file, "%15le", &dose->sets[i].bvec[j] );
      fscanf ( file, "%15le\n", &dose->sets[i].bvec[2] );
   }

/*
 *  Setup and read bflux array - dose data by physical dose component
 */


   nned = ( *irr ? MAX_NNED : MAX_NNED-2 );
   bsize = dose->nedit * dose->nedit * dose->nedit * nned;
   dose->bflux = (float *) malloc ( bsize * sizeof ( float ) );
   for ( i = 0; i < bsize; i++ ) {
      if ( (i+1)%6 && (i+1)%bsize )
         fscanf ( file, "%15e", &dose->bflux[i] );
      else
         fscanf ( file, "%15e\n", &dose->bflux[i] );
   }

   return;

}

int main(int argc, char ** argv) {
  dose_struct dose;
  int irr;
  int nned;
  if(argc == 2) {
    FILE * dose_file;
    int x,y,z,i;
    //printf("Reading %s\n",argv[1]);
    printf("#x y z gam hyd tot b10 gp n14 ff ef tf pr ugp oth uk rr1 rr2\n");
    dose_file = fopen(argv[1],"r");
    set_up_dose_mem(&dose, MAX_FIELDS);
    read_rst_file(&dose,dose_file,&irr);
    nned = ( irr ? MAX_NNED : MAX_NNED-2 );
    for(x = 0; x < dose.nedit; x++) {
      double xv = *dose.x0 + *dose.delw*x;
      for(y = 0; y < dose.nedit; y++) {
	double yv = *dose.y0 + *dose.delw*y;
	for(z = 0; z < dose.nedit; z++) {
	  double zv = *dose.z0 + *dose.delw*z;
	  int ad1 = nned*(z+dose.nedit*(y+x*dose.nedit));
	  printf("%e %e %e ",xv,yv,zv);
	  for(i = 0; i < nned; i++) {
	    printf("%e ",dose.bflux[ad1+i]);	    
	  }
	  printf("\n");
	}
      }
    }
  }
  return 0;
}
