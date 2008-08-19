#include <stdio.h>
#include <stdlib.h>

#include "transform.h"
#define LINESIZE 120

main(int argc, char *argv[])
{
    FILE  *file;
    int   numBEADS;
    int   count=1;
    char  line[LINESIZE];
    float coord[3], source[3],
        theta, phi,
        RadiansPerDegree = 1.5707963 / 90.0,
        thetaR, phiR,
        xp, yp, zp,
        xbs, ybs, zbs;

    if( argc == 2 )
    {
        if ((file = fopen(argv[1],"r")) == NULL)
        {
            printf ("Unable to open file %s\n", argv[1]);
            exit(1);
        }
    }
    else
    {
        printf ("USAGE:  position [file name]\n");
        exit(1);
    }

    fgets(line, LINESIZE - 1, file); /* skip by first line */
    fputs(line, stdout);
    fscanf(file,"%e %e %d\n", &theta, &phi, &numBEADS);
    printf(" theta = %f phi = %f numBEADS = %d \n", theta, phi, numBEADS);
    thetaR = theta * RadiansPerDegree;
    phiR   = phi   * RadiansPerDegree;

    fgets(line, LINESIZE - 1, file); /* skip by third line */
    fputs(line, stdout);
    fscanf(file,"%e %e %e\n", &xp, &yp, &zp);
    printf(" xp = %f yp = %f zp = %f \n", xp, yp, zp);

    fgets(line, LINESIZE - 1, file); /* skip by fifth line */
    fputs(line, stdout);
    fscanf(file,"%e %e %e\n", &xbs, &ybs, &zbs);
    printf(" xbs = %f ybs = %f zbs = %f \n", xbs, ybs, zbs);

    source[0] = xbs;
    source[1] = ybs;
    source[2] = zbs;
    translate (source, -xp, -yp, -zp);
    rotate_z (source, thetaR);
    rotate_x (source, phiR); 
    translate (source, xp, yp, zp);
    display (source, "re-rotated beam source");

    while (count <= numBEADS)
    {
        printf ("Input the coordinate.\n");
        scanf ("%f %f %f", &coord[0], &coord[1], &coord[2]);

        /* scale (coord, -1.0, 1.0, 1.0); */
        fswap (&coord[0], &coord[1]);
        translate (coord, -xp, -yp, -zp); /* target point now at origin */
        rotate_z (coord, thetaR);
        rotate_x (coord, phiR); 
        translate (coord, xp, yp, zp);

        translate (coord, -source[0], -source[1], -source[2]);
        display (coord, "final bead position");
        count++;

    }
    fclose(file);
}
