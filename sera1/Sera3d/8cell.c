#include "sera3d.h"


/***************************************************************************************/
/****** expecting the ordering of the corners of the eight cell to be as follows *******/

/*         4          7                  */
/*                                       */
/*     5           6                     */
/*                                       */
/*                                       */
/*        3           2                  */
/*                                       */
/*     0           1                     */
/*****************************************/
#define BIT_1   1
#define BIT_2   2
#define BIT_3   4
#define BIT_4   8
#define BIT_5   16
#define BIT_6   32
#define BIT_7   64
#define BIT_8   128

#define DN1 .7071
#define DN2 .57735

void determine_8_cell_vertex_ordering(main_gui_t *gui, Cell_Triangle_t *tris, int *num_triangles, unsigned char cell)
{
  int num_tris;
  
  switch ( cell )
    {
    case  0: 
      num_tris = 0; break;
    case  1:
      num_tris = 0; break;
    case  2:
      num_tris = 0; break;
    case  3:
      num_tris = 0; break;
    case  4:
      num_tris = 0; break;
    case  5:
      num_tris = 0; break;
    case  6:
      num_tris = 0; break;
    case  7:
      tris[0].a  =   0;  tris[0].b =  1;    tris[0].c = 2;
      tris[0].nx = 0.0;  tris[0].ny = 1.0;  tris[0].nz = 0.0;
      num_tris = 1; break;
    case  8:
      num_tris = 0; break;
    case  9:
      num_tris = 0; break;
    case  10:
      num_tris = 0; break;
    case  11:
      tris[0].a  =   0;  tris[0].b =  1;    tris[0].c = 3;
      tris[0].nx = 0.0;  tris[0].ny = 1.0;  tris[0].nz = 0.0;
      num_tris = 1; break;
    case  12:
      num_tris = 0; break;
    case  13:
      tris[0].a  =   0;  tris[0].b =  2;    tris[0].c = 3;
      tris[0].nx = 0.0;  tris[0].ny = 1.0;  tris[0].nz = 0.0;
      num_tris = 1; break;
    case  14:
      tris[0].a = 1;     tris[0].b = 2;     tris[0].c = 3;
      tris[0].nx = 0.0;  tris[0].ny = 1.0;  tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  15:	
      tris[0].a = 0;     tris[0].b = 1;     tris[0].c = 2;
      tris[0].nx = 0.0;  tris[0].ny = 1.0;  tris[0].nz = 0.0;
      tris[1].a = 0;     tris[1].b = 2;     tris[1].c = 3;
      tris[1].nx = 0.0;  tris[1].ny = 1.0;  tris[1].nz = 0.0;
      num_tris = 2; break;
    case  16:
      num_tris = 0; break;
    case  17:
      num_tris = 0; break;
    case  18:
      num_tris = 0; break;
    case  19:
      tris[0].a = 0;     tris[0].b = 1;     tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = DN1;  tris[0].nz = DN1; 
      num_tris = 1; break;
    case  20:
      num_tris = 0; break;
    case  21:
      tris[0].a = 0;     tris[0].b = 2;     tris[0].c = 4;
      tris[0].nx = DN2;  tris[0].ny = DN2;  tris[0].nz = DN2; 
      num_tris = 1; break;
    case  22:
      tris[0].a = 1;     tris[0].b = 2;     tris[0].c = 4;
      tris[0].nx = DN1;  tris[0].ny = DN1;  tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  23:	
      tris[0].a = 1;     tris[0].b = 2;      tris[0].c = 4;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 4;      tris[1].c = 2;
      tris[1].nx = -DN2; tris[1].ny = -DN2;  tris[1].nz = -DN2; 

      tris[2].a = 0;     tris[2].b = 1;      tris[2].c = 4;
      tris[2].nx = 0.0;  tris[2].ny = DN1;   tris[2].nz = DN1; 

      /*tris[3].a = 0;     tris[3].b = 2;      tris[3].c = 1;
	tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0;*/ 
      num_tris = 3; break;
    case  24:
      num_tris = 0; break;
    case  25:
      tris[0].a = 0;     tris[0].b = 3;     tris[0].c = 4;
      tris[0].nx = 1.0;  tris[0].ny = 0.0;  tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  26:
      tris[0].a = 1;     tris[0].b = 4;     tris[0].c = 3;
      tris[0].nx = -DN1;  tris[0].ny = 0.0;  tris[0].nz = DN1; 
      num_tris = 1; break;
    case  27:
      tris[0].a = 1;     tris[0].b = 3;      tris[0].c = 4;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 0;     tris[1].b = 1;      tris[1].c = 4;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 

      /*tris[2].a = 0;     tris[2].b = 4;      tris[2].c = 3;
      tris[2].nx = -1.0; tris[2].ny = 0.0;   tris[2].nz = 0.0; 

      tris[0].a = 1;     tris[0].b = 0;      tris[0].c = 3;
      tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0;*/ 
      num_tris = 2;	break;
    case  28:
      tris[0].a = 3;     tris[0].b = 2;     tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = 0.0;  tris[0].nz = 1.0; 
      num_tris = 1; break;
    case  29:
      tris[0].a = 0;     tris[0].b = 2;     tris[0].c = 4;
      tris[0].nx = DN2;  tris[0].ny = DN2;  tris[0].nz = DN2; 
      num_tris = 1; break;
    case  30:
      tris[0].a = 3;     tris[0].b = 1;      tris[0].c = 4;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 
      
      tris[1].a = 1;     tris[1].b = 2;      tris[1].c = 4;
      tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = 0.0; 

      /*tris[2].a = 2;     tris[2].b = 3;      tris[2].c = 4;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = -1.0; 
	
	tris[0].a = 1;     tris[0].b = 3;      tris[0].c = 2;
	tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0;*/       
      num_tris = 2;	break;
    case  31:	
      tris[0].a = 1;     tris[0].b = 2;      tris[0].c = 4;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 1;      tris[1].c = 4;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 

      /*tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 1;
	tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0;*/ 
      num_tris = 2;  break;
    case  32:
      num_tris = 0; break;
    case  33:
      num_tris = 0; break;
    case  34:
      num_tris = 0; break;
    case  35:
      tris[0].a = 0;     tris[0].b = 5;     tris[0].c = 1;
      tris[0].nx = 0.0;  tris[0].ny = 0.0;  tris[0].nz = -1.0; 
      num_tris = 1; break;      
    case  36:
      num_tris = 0; break;
    case  37:
      tris[0].a = 0;     tris[0].b = 2;     tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = 0;    tris[0].nz = DN1; 
      num_tris = 1; break;
    case  38:
      tris[0].a = 1;     tris[0].b = 2;     tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = DN1;  tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  39:	

      tris[0].a = 1;     tris[0].b = 2;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 2;     tris[1].b = 0;      tris[1].c = 5;
      tris[1].nx = -DN1; tris[1].ny = 0;     tris[1].nz = -DN1; 

      /*tris[3].a = 0;     tris[3].b = 2;      tris[3].c = 1;
	tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0; 
	
	tris[0].a = 0;     tris[0].b = 1;      tris[0].c = 5;
	tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = 1.0;*/ 
      num_tris = 2;	break;
    case  40:
      num_tris = 0; break;
    case  41:
      tris[0].a = 0;     tris[0].b = 3;     tris[0].c = 5;
      tris[0].nx = 1.0;  tris[0].ny = 0.0;  tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  42:
      tris[0].a = 1;     tris[0].b = 3;     tris[0].c = 5;
      tris[0].nx = DN2;  tris[0].ny = DN2;  tris[0].nz = -DN2; 
      num_tris = 1; break;
    case  43:
      tris[0].a = 1;     tris[0].b = 3;      tris[0].c = 5;
      tris[0].nx = DN2;  tris[0].ny = DN2;   tris[0].nz = -DN2; 
      num_tris = 1;	break;
    case  44:
      tris[0].a = 3;     tris[0].b = 5;     tris[0].c = 2;
      tris[0].nx = 0.0;  tris[0].ny = DN1;  tris[0].nz = -DN1; 
      num_tris = 1; break;
    case  45:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 
      
      tris[1].a = 3;     tris[1].b = 5;      tris[1].c = 2;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = -DN1; 
      /*      
	      tris[2].a = 0;     tris[2].b = 5;      tris[2].c = 3;
	      tris[2].nx = -1.0; tris[2].ny = 0.0;   tris[2].nz = 0.0; 
      
	      tris[3].a = 0;     tris[3].b = 3;      tris[3].c = 2;
	      tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  46:
      tris[0].a = 1;     tris[0].b = 5;      tris[0].c = 3;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = DN2; 
      
      tris[1].a = 1;     tris[1].b = 2;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = 0.0; 

      tris[2].a = 2;     tris[2].b = 3;      tris[2].c = 5;
      tris[2].nx = 0.0;  tris[2].ny = DN1;   tris[2].nz = -DN1; 
      /*
	tris[3].a = 1;     tris[3].b = 3;      tris[3].c = 2;
	tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  47:	
      tris[0].a = 1;     tris[0].b = 2;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 
      
      tris[1].a = 2;     tris[1].b = 3;      tris[1].c = 5;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = -DN1; 
      /*
	tris[2].a = 1;     tris[2].b = 3;      tris[2].c = 2;
	tris[2].nx = 0.0;  tris[2].ny = -1.0;  tris[2].nz = 0.0; 
      */
      num_tris = 2; break;
    case  48:
      num_tris = 0; break;
    case  49:
      tris[0].a = 0;     tris[0].b = 4;      tris[0].c = 5;
      tris[0].nx = 1.0;  tris[0].ny = 0.0;   tris[0].nz = 0.0; 
      num_tris = 1;  break;
    case  50:
      tris[0].a = 1;     tris[0].b = 4;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 
      num_tris = 1;  break;
    case  51:
      tris[0].a = 1;     tris[0].b = 4;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 
      
      tris[1].a = 1;     tris[1].b = 0;      tris[1].c = 4;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = -DN1; 
      /*
	tris[2].a = 0;     tris[2].b = 5;      tris[2].c = 4;
	tris[2].nx = -1.0; tris[2].ny = 0.0;   tris[2].nz = 0.0; 
	
	tris[0].a = 0;     tris[0].b = 1;      tris[0].c = 5;
	tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = 1.0; 
      */
      num_tris = 2;	break;
    case  52:
      tris[0].a = 2;     tris[0].b = 4;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 
      num_tris = 1;  break;
    case  53:
     tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 5;
     tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 
     
     tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 5;
     tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = 0.0; 
     
     tris[2].a = 0;     tris[2].b = 2;      tris[2].c = 4;
     tris[2].nx = DN2;  tris[2].ny = DN2;   tris[2].nz = DN2; 

     /*
       tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 4;
       tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
     */
     num_tris = 3;	break;
    case  54:
     tris[0].a = 1;     tris[0].b = 4;      tris[0].c = 5;
     tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 
     
     tris[1].a = 1;     tris[1].b = 2;      tris[1].c = 4;
     tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = 0.0; 
     num_tris = 2;break;
    case  55:	
      tris[0].a = 1;     tris[0].b = 4;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 1;     tris[1].b = 2;      tris[1].c = 4;
      tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = 0.0; 

      tris[2].a = 0;     tris[2].b = 4;      tris[2].c = 2;
      tris[2].nx = -DN2; tris[2].ny = -DN2;  tris[2].nz = -DN2; 

      /*
	tris[4].a = 0;     tris[4].b = 2;     tris[4].c = 1;
	tris[4].nx = 0.0;  tris[4].ny = -1.0; tris[4].nz = 0.0; 
	
	tris[5].a = 0;     tris[5].b = 5;    tris[5].c = 4;
	tris[5].nx = -1.0; tris[5].ny = 0.0; tris[5].nz = 0.0; 
	
	tris[0].a = 0;     tris[0].b = 1;      tris[0].c = 5;
	tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = 1.0; 
      */

      num_tris = 3;	break;
    case  56:
      tris[0].a = 3;     tris[0].b = 4;      tris[0].c = 5;
      tris[0].nx = 1.0;  tris[0].ny = 0.0;   tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  57:
      tris[0].a = 0;     tris[0].b = 4;      tris[0].c = 5;
      tris[0].nx = 1.0;  tris[0].ny = 0.0;   tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 3;      tris[1].c = 4;
      tris[1].nx = 1.0;  tris[1].ny = 0.0;   tris[1].nz = 0.0; 
      num_tris = 2; break;
    case  58:
      tris[0].a = 1;     tris[0].b = 5;      tris[0].c = 3;
      tris[0].nx = -DN2; tris[0].ny =-DN2;   tris[0].nz = DN2; 

      tris[1].a = 1;     tris[1].b = 4;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = 0.0; 

      tris[2].a = 1;     tris[2].b = 3;      tris[2].c = 4;
      tris[2].nx = DN1;  tris[2].ny = 0.0;   tris[2].nz = -DN1; 
      /*
	tris[3].a = 3;     tris[3].b = 5;      tris[3].c = 4;
	tris[3].nx = -1.0; tris[3].ny = 0.0;   tris[3].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  59:
      tris[0].a = 1;     tris[0].b = 4;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 1;     tris[1].b = 3;      tris[1].c = 4;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = -DN1; 
      /*
	tris[2].a = 3;     tris[2].b = 5;      tris[2].c = 4;
	tris[2].nx = -1.0; tris[2].ny = 0.0;   tris[2].nz = 0.0; 
      */
      num_tris = 2; break;
    case  60:
      tris[0].a = 2;     tris[0].b = 5;      tris[0].c = 3;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = DN1; 

      tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = 0.0; 

      /*
	tris[2].a = 2;     tris[2].b = 3;      tris[2].c = 4;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = -1.0; 
	
	tris[3].a = 3;     tris[3].b = 5;      tris[3].c = 4;
	tris[3].nx = -1.0; tris[3].ny = 0.0;   tris[3].nz = 0.0; 
      */ 
     num_tris = 2;	break;
    case  61:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 
      
      tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = 0.0; 
      num_tris = 2;  break;
    case  62:
      tris[0].a = 1;     tris[0].b = 2;      tris[0].c = 4;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 1;     tris[1].b = 4;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = 0.0; 

      tris[2].a = 3;     tris[2].b = 1;      tris[2].c = 5;
      tris[2].nx = -DN2; tris[2].ny = -DN2;  tris[2].nz = DN2; 

      /*
	tris[2].a = 1;     tris[2].b = 3;      tris[2].c = 2;
	tris[2].nx = 0;    tris[2].ny = -1.0;  tris[2].nz = 0.0; 
	
	tris[3].a = 3;     tris[3].b = 5;      tris[3].c = 4;
	tris[3].nx = -1.0; tris[3].ny = 0.0;   tris[3].nz = 0.0; 
	
	tris[4].a = 2;     tris[4].b = 3;      tris[4].c = 4;
	tris[4].nx = 0.0;  tris[4].ny = 0.0;   tris[4].nz = -1.0; 
      */
      num_tris = 3;	break;
    case  63:	
      tris[0].a = 1;     tris[0].b = 2;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = 0.0; 
      num_tris = 2; break;
    case  64:
      num_tris = 0; break;
    case  65:
      num_tris = 0; break;
    case  66:
      num_tris = 0; break;
    case  67:
      tris[0].a = 1;     tris[0].b = 0;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = -1.0; 
      num_tris = 1; break;
    case  68:
      num_tris = 0; break;
    case  69:
      tris[0].a = 2;     tris[0].b = 0;      tris[0].c = 6;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 
      num_tris = 1; break;
    case  70:
      tris[0].a = 1;     tris[0].b = 6;      tris[0].c = 2;
      tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  71:	
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 2;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 
      num_tris = 1; break;
    case  72:
      num_tris = 0; break;
    case  73:
      tris[0].a = 0;     tris[0].b = 3;      tris[0].c = 6;
      tris[0].nx = DN1; tris[0].ny = -DN1;   tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  74:
      tris[0].a = 1;     tris[0].b = 3;      tris[0].c = 6;
      tris[0].nx = DN1; tris[0].ny = 0.0;   tris[0].nz = -DN1; 
      num_tris = 1; break;
    case  75:
      tris[0].a = 1;     tris[0].b = 3;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = -DN1; 
      
      tris[1].a = 3;     tris[1].b = 0;      tris[1].c = 6;
      tris[1].nx = -DN1; tris[1].ny = DN1;   tris[1].nz = 0.0; 
      /*
	tris[0].a = 0;     tris[0].b = 3;      tris[0].c = 1;
	tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0; 
	
	tris[1].a = 0;     tris[1].b = 1;      tris[1].c = 6;
	tris[1].nx = 0.0;  tris[1].ny = 0.0;   tris[1].nz = 1.0; 
      */
      num_tris = 2;	break;
    case  76:
      tris[0].a = 2;     tris[0].b = 6;      tris[0].c = 3;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;   tris[0].nz = DN1; 
      num_tris = 1; break;
    case  77:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2; 
      
      tris[1].a = 2;     tris[1].b = 3;      tris[1].c = 6;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = -DN1; 
	
      tris[2].a = 3;     tris[2].b = 0;      tris[2].c = 6;
      tris[2].nx = -DN1; tris[2].ny = DN1;   tris[2].nz = 0.0; 
      /*
	tris[0].a = 0;     tris[0].b = 3;      tris[0].c = 2;
	tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0;       
      */
      num_tris = 3;	break;;
    case  78:
      tris[0].a = 2;     tris[0].b = 3;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = -DN1; 
      
      tris[1].a = 3;     tris[1].b = 1;      tris[1].c = 6;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = DN1; 
      /*
	tris[0].a = 1;     tris[0].b = 2;      tris[0].c = 6;
	tris[0].nx = 1.0;  tris[0].ny = 0.0;   tris[0].nz = 0.0; 
	
	tris[1].a = 3;     tris[1].b = 2;      tris[1].c = 1;
	tris[1].nx = 0.0;  tris[1].ny = -1.0;  tris[1].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  79:	
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 3;
      tris[0].nx = -DN1;  tris[0].ny = DN1;   tris[0].nz = 0.0; 
      
      tris[1].a = 2;     tris[1].b = 3;      tris[1].c = 6;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = -DN1; 
      num_tris = 2; break;
    case  80:
      num_tris = 0; break; 
    case  81:
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2;  tris[0].ny = DN2;   tris[0].nz = DN2; 
      num_tris = 1; break;
    case  82:
      tris[0].a = 1;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1;
      num_tris = 1; break;
    case  83:
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[2].a = 0;     tris[2].b = 4;      tris[2].c = 1;
      tris[2].nx = 0.0;  tris[2].ny = -DN1;  tris[2].nz = -DN1; 

      tris[3].a = 1;     tris[3].b = 4;      tris[3].c = 6;
      tris[3].nx = DN1;  tris[3].ny = 0.0;   tris[3].nz = -DN1; 
      /*
	tris[1].a = 0;     tris[1].b = 1;      tris[1].c = 6;
	tris[1].nx = 0.0;  tris[1].ny = 0.0;   tris[1].nz = 1.0; 
      */
      num_tris = 3;	break;
    case  84:
      tris[0].a = 2;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 
      num_tris = 1; break;
    case  85:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 6;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = -DN2; 

      tris[2].a = 0;     tris[2].b = 6;      tris[2].c = 4;
      tris[2].nx = -DN2; tris[2].ny = DN2;   tris[2].nz = DN2; 

      tris[3].a = 0;     tris[3].b = 4;      tris[3].c = 2;
      tris[3].nx = -DN2; tris[3].ny = -DN2;  tris[3].nz = -DN2; 
      num_tris = 4;	break;
    case  86:
      tris[0].a = 2;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = DN2;   tris[0].nz = -DN2; 

      tris[1].a = 2;     tris[1].b = 1;      tris[1].c = 4;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 

      tris[2].a = 4;     tris[2].b = 1;      tris[2].c = 6;
      tris[2].nx = -DN1; tris[2].ny = 0.0;   tris[2].nz = DN1; 
      /*
	tris[0].a = 1;     tris[0].b = 2;      tris[0].c = 6;
	tris[0].nx = 1.0;  tris[0].ny = 0.0;   tris[0].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  87:	
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 6;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = -DN2; 

      tris[2].a = 0;     tris[2].b = 4;      tris[2].c = 2;
      tris[2].nx = -DN2; tris[2].ny = -DN2;  tris[2].nz = -DN2; 
      num_tris = 3; break;
    case  88:
      tris[0].a = 3;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 
      num_tris = 1; break;
    case  89:
      tris[0].a = 3;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 0;     tris[1].b = 6;      tris[1].c = 4;
      tris[1].nx = -DN2; tris[1].ny = DN2;   tris[1].nz = DN2; 

      tris[2].a = 0;     tris[2].b = 3;      tris[2].c = 6;
      tris[2].nx = DN1;  tris[2].ny = -DN1;  tris[2].nz = 0.0; 
      /*
	tris[1].a = 0;     tris[1].b = 4;      tris[1].c = 3;
	tris[1].nx = -1.0; tris[1].ny = 0.0;   tris[1].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  90:
      tris[0].a = 3;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 1;      tris[1].c = 6;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = DN1; 
      num_tris = 2; break;
    case  91:
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[1].a = 1;     tris[1].b = 4;      tris[1].c = 6;
      tris[1].nx =DN1;   tris[1].ny = 0.0;   tris[1].nz = -DN1; 

      tris[2].a = 1;     tris[2].b = 3;      tris[2].c = 4;
      tris[2].nx = DN1;  tris[2].ny = 0;     tris[2].nz = -DN1; 
      /*
	tris[0].a = 0;     tris[0].b = 4;      tris[0].c = 3;
	tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
	
	tris[1].a = 0;     tris[1].b = 3;      tris[1].c = 1;
	tris[1].nx = 0.0;  tris[1].ny = -1.0;  tris[1].nz = 0.0; 
	
	tris[3].a = 0;     tris[3].b = 1;      tris[3].c = 6;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
      */

      num_tris = 3;	break;
    case  92:
      tris[0].a = 2;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = DN2;   tris[0].nz = -DN2; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 4;
      tris[1].nx = -DN2; tris[1].ny = -DN2;  tris[1].nz = DN2; 

      tris[2].a = 3;     tris[2].b = 2;      tris[2].c = 6;
      tris[2].nx = 0.0;  tris[2].ny = -DN1;  tris[2].nz = DN1; 
      /*
	tris[0].a = 2;     tris[0].b = 3;      tris[0].c = 4;
	tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = -1.0; 
      */
      num_tris = 3;	break;
    case  93:
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[1].a = 0;     tris[1].b = 2;      tris[1].c = 6;
      tris[1].nx = DN2;  tris[1].ny = -DN2;  tris[1].nz = DN2; 

      tris[2].a = 2;     tris[2].b = 4;      tris[2].c = 6;
      tris[2].nx = DN2; tris[2].ny = DN2;  tris[2].nz = -DN2; 
      num_tris = 3; break;
    case  94:
      tris[0].a = 1;     tris[0].b = 6;      tris[0].c = 3;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 4;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = DN1; 

      tris[2].a = 2;     tris[2].b = 4;      tris[2].c = 6;
      tris[2].nx = DN2;  tris[2].ny = DN2;   tris[2].nz = -DN2; 
      /*
	tris[0].a = 1;     tris[0].b = 3;      tris[0].c = 2;
	tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0; 
	
	tris[1].a = 1;     tris[1].b = 2;      tris[1].c = 6;
	tris[1].nx = 1.0;  tris[1].ny = 0.0;   tris[1].nz = 0.0; 
	
	tris[4].a = 2;     tris[4].b = 3;      tris[4].c = 4;
	tris[4].nx = 0.0;  tris[4].ny = 0.0;   tris[4].nz = -1.0; 
      */
      num_tris = 3;	break;
    case  95:	
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 6;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = -DN2;
      num_tris = 2; break;
    case  96:
      num_tris = 0; break;
    case  97:
      tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = -1.0; 
      num_tris = 1; break;
    case  98:
      tris[0].a = 1;     tris[0].b = 5;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = -1.0; 
      num_tris = 1; break;
    case  99:
      tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = -1.0; 

      tris[1].a = 0;     tris[1].b = 6;      tris[1].c = 1;
      tris[1].nx = 0.0;  tris[1].ny = 0.0;   tris[1].nz = -1.0;
      num_tris = 2; break;
    case  100:
      tris[0].a = 5;     tris[0].b = 2;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;   tris[0].nz = DN1; 
      num_tris = 1; break;
    case  101:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 0;     tris[1].b = 5;      tris[1].c = 2;
      tris[1].nx = -DN2; tris[1].ny = 0.0;   tris[1].nz = -DN2; 

      tris[2].a = 2;     tris[2].b = 5;      tris[2].c = 6;
      tris[2].nx = 0.0;  tris[2].ny = DN1;   tris[2].nz = -DN1; 
      /*
	tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 5;
	tris[0].nx = 0.0;  tris[0].ny = 0.0;  tris[0].nz = 1.0; 
      */
      num_tris = 3;	break;
    case  102:
      tris[0].a = 2;     tris[0].b = 5;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = -DN1; 

      tris[1].a = 1;     tris[1].b = 5;      tris[1].c = 2;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      /*
	tris[0].a = 1;     tris[0].b = 6;      tris[0].c = 5;
	tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = 1.0; 
	
	tris[1].a = 1;     tris[1].b = 2;      tris[1].c = 6;
	tris[1].nx = 1.0;  tris[1].ny = 0.0;   tris[1].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  103:	
      tris[0].a = 2;     tris[0].b = 0;      tris[0].c = 5;
      tris[0].nx = -DN1;  tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 2;     tris[1].b = 5;      tris[1].c = 6;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = -DN1;
      num_tris = 2; break;
    case  104:
      tris[0].a = 3;     tris[0].b = 6;      tris[0].c = 5;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = DN1; 
      num_tris = 1; break;
    case  105:
      tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 6;
      tris[0].nx = 0;    tris[0].ny = DN1;   tris[0].nz = -DN1; 

      tris[1].a = 0;     tris[1].b = 3;      tris[1].c = 6;
      tris[1].nx = DN1;  tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      /*
	tris[3].a = 0;     tris[3].b = 6;      tris[3].c = 5;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
	
	tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 3;
	tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
      */

      num_tris = 2;	break;
    case  106:
      tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = -DN1; 

      tris[1].a = 1;     tris[1].b = 3;      tris[1].c = 6;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = -DN1; 

      tris[2].a = 3;     tris[2].b = 1;      tris[2].c = 5;
      tris[2].nx = -DN2; tris[2].ny = -DN2;  tris[2].nz = DN2; 
      /*
	tris[2].a = 1;     tris[2].b = 6;      tris[2].c = 5;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = 1.0; 
      */
      num_tris = 3;	break;
    case  107:
      tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = -DN1; 

      tris[1].a = 1;     tris[1].b = 3;      tris[1].c = 6;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = -DN1; 
      num_tris = 2; break;
    case  108:
      tris[0].a = 2;     tris[0].b = 3;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = -DN1; 

      tris[1].a = 3;     tris[1].b = 5;      tris[1].c = 6;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = -DN1; 
      num_tris = 2; break;
    case  109:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 2;     tris[1].b = 3;      tris[1].c = 6;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = -DN1; 

      tris[2].a = 3;     tris[2].b = 5;      tris[2].c = 6;
      tris[2].nx = 0.0;  tris[2].ny = DN1;   tris[2].nz = -DN1; 
      /*
	tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 3;
	tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
	
	tris[1].a = 0;     tris[1].b = 3;      tris[1].c = 2;
	tris[1].nx = 0.0;  tris[1].ny = -1.0;  tris[1].nz = 0.0; 
	
	tris[3].a = 0;     tris[3].b = 6;      tris[3].c = 5;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
      */

      num_tris = 3;	break;
    case  110:
      tris[0].a = 2;     tris[0].b = 3;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = -DN1; 

      tris[1].a = 3;     tris[1].b = 5;      tris[1].c = 6;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = -DN1; 

      tris[2].a = 1;     tris[2].b = 5;      tris[2].c = 3;
      tris[2].nx = -DN2; tris[2].ny = -DN2;  tris[2].nz = DN2; 
      /*
	tris[5].a = 1;     tris[5].b = 6;      tris[5].c = 5;
	tris[5].nx = 0.0;  tris[5].ny = 0.0;   tris[5].nz = 1.0; 
	
	tris[2].a = 1;     tris[2].b = 2;      tris[2].c = 6;
	tris[2].nx = 1.0;  tris[2].ny = 0.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 1;     tris[3].b = 3;      tris[3].c = 2;
	tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  111:	
      tris[0].a = 2;     tris[0].b = 3;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = -DN1; 

      tris[1].a = 3;     tris[1].b = 5;      tris[1].c = 6;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = -DN1;
      num_tris = 2; break; 
    case  112:
      tris[0].a = 6;     tris[0].b = 5;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  113:
      tris[0].a = 0;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = -DN2; 
      num_tris = 1; break;
    case  114:
      tris[0].a = 1;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 1;     tris[1].b = 5;      tris[1].c = 4;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      /*
	tris[3].a = 1;     tris[3].b = 6;      tris[3].c = 5;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
	
	tris[0].a = 4;     tris[0].b = 5;      tris[0].c = 6;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  115:
      tris[0].a = 1;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 1;     tris[1].b = 0;      tris[1].c = 4;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = -DN1; 
      /*
	tris[2].a = 1;     tris[2].b = 6;      tris[2].c = 0;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = 1.0; 
      */
      num_tris = 2; break;
    case  116:
      tris[0].a = 2;     tris[0].b = 6;      tris[0].c = 5;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = DN1; 

      tris[1].a = 2;     tris[1].b = 5;      tris[1].c = 4;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 

      tris[2].a = 2;     tris[2].b = 4;      tris[2].c = 6;
      tris[2].nx = DN2;  tris[2].ny = DN2;   tris[2].nz = -DN2; 
      /*
	tris[1].a = 5;     tris[1].b = 6;      tris[1].c = 4;
	tris[1].nx = 0.0;  tris[1].ny = 1.0;   tris[1].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  117:
      tris[0].a = 2;     tris[0].b = 6;      tris[0].c = 0;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 6;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = -DN2; 

      tris[2].a = 2;     tris[2].b = 0;      tris[2].c = 4;
      tris[2].nx = -DN2; tris[2].ny = -DN2;  tris[2].nz = -DN2; 
      num_tris = 3; break;
    case  118:
      tris[0].a = 1;     tris[0].b = 5;      tris[0].c = 4;
      tris[0].nx = -DN1; tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 1;     tris[1].b = 4;      tris[1].c = 2;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 

      tris[2].a = 2;     tris[2].b = 4;      tris[2].c = 6;
      tris[2].nx = DN2;  tris[2].ny = DN2;   tris[2].nz = -DN2; 
      /*
	tris[2].a = 5;     tris[2].b = 6;      tris[2].c = 4;
	tris[2].nx = 0.0;  tris[2].ny = 1.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 1;     tris[3].b = 6;      tris[3].c = 5;
	tris[3].nx = 0.0; tris[3].ny = 0.0;   tris[3].nz = 1.0; 
	
	tris[4].a = 1;     tris[4].b = 2;      tris[4].c = 6;
	tris[4].nx = 1.0;  tris[4].ny = 0.0;   tris[4].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  119:	
      tris[0].a = 2;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN2; tris[0].ny = DN2;  tris[0].nz = -DN2; 

      tris[1].a = 2;     tris[1].b = 0;      tris[1].c = 4;
      tris[1].nx = -DN2; tris[1].ny = -DN2;  tris[1].nz = -DN2; 
      num_tris = 2; break;
    case  120:
      tris[0].a = 3;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 5;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = DN1; 
      /*
	tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 4;
	tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
	
	tris[1].a = 5;     tris[1].b = 6;      tris[1].c = 4;
	tris[1].nx = 0.0;  tris[1].ny = 1.0;   tris[1].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  121:
      tris[0].a = 0;     tris[0].b = 3;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 3;     tris[1].b = 4;      tris[1].c = 6;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz =-DN1;
      /*
	tris[2].a = 5;     tris[2].b = 6;      tris[2].c = 4;
	tris[2].nx = 0.0;  tris[2].ny = 1.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 0;     tris[3].b = 6;      tris[3].c = 5;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
      */ 
     num_tris = 2; break; 
    case  122:

      tris[0].a = 5;     tris[0].b = 3;      tris[0].c = 1;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 6;     tris[1].b = 3;      tris[1].c = 4;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = -DN1; 

      tris[2].a = 6;     tris[2].b = 1;      tris[2].c = 3;
      tris[2].nx = DN1;  tris[2].ny = 0.0;   tris[2].nz = -DN1; 

      /*
	tris[0].a = 4;     tris[0].b = 5;      tris[0].c = 6;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
	
	tris[1].a = 4;     tris[1].b = 3;      tris[1].c = 5;
	tris[1].nx = -1.0; tris[1].ny = 0.0;   tris[1].nz = 0.0; 
	
	tris[2].a = 5;     tris[2].b = 1;      tris[2].c = 6;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = 1.0; 
      */

      num_tris = 3;	break;
    case  123:
      tris[0].a = 6;     tris[0].b = 3;      tris[0].c = 4;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 6;     tris[1].b = 1;      tris[1].c = 3;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = -DN1; 
      num_tris = 2; break;
    case  124:
      tris[0].a = 2;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = DN1;   tris[0].nz = -DN1; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 5;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = DN1; 

      tris[2].a = 3;     tris[2].b = 2;      tris[2].c = 6;
      tris[2].nx = 0.0;  tris[2].ny = -DN1;  tris[2].nz = DN1; 

      /*
	tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 4;
	tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
	
	tris[1].a = 4;     tris[1].b = 5;      tris[1].c = 6;
	tris[1].nx = 0.0; tris[1].ny = 1.0;   tris[1].nz = 0.0; 
	
	tris[3].a = 2;     tris[3].b = 3;      tris[3].c = 4;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = -1.0; 
      */

      num_tris = 3;	break;
    case  125:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2; 
      
      tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 6;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = -DN2; 
      num_tris = 2; break;
    case  126:
      tris[0].a = 1;     tris[0].b = 5;      tris[0].c = 3;
      tris[0].nx = -DN1; tris[0].ny = -DN1;  tris[0].nz = DN1; 

      tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 6;
      tris[1].nx = DN1;  tris[1].ny = DN1;   tris[1].nz = -DN1; 
      /*
	tris[2].a = 1;     tris[2].b = 2;      tris[2].c = 6;
	tris[2].nx = 1.0;  tris[2].ny = 0.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 1;     tris[3].b = 6;      tris[3].c = 5;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
	
	tris[4].a = 5;     tris[4].b = 6;      tris[4].c = 4;
	tris[4].nx = 0.0;  tris[4].ny = 1.0;   tris[4].nz = 0.0; 
	
	tris[5].a = 5;     tris[5].b = 4;      tris[5].c = 3;
	tris[5].nx = -1.0; tris[5].ny = 0.0;   tris[5].nz = 0.0; 
	
	tris[6].a = 3;     tris[6].b = 4;      tris[6].c = 2;
	tris[6].nx = 0.0;  tris[6].ny = 0.0;   tris[6].nz = -1.0; 
	
	tris[7].a = 1;     tris[7].b = 3;      tris[7].c = 2;
	tris[7].nx = 0.0;  tris[7].ny = -1.0;  tris[7].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  127:
      tris[0].a = 2;     tris[0].b = 4;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = DN2;   tris[0].nz = -DN2; 
      num_tris = 1; break;
    case  128:
      num_tris = 0; break;
    case  129:
      num_tris = 0; break;
    case  130:
      num_tris = 0; break;
    case  131:
      tris[0].a = 0;     tris[0].b = 1;      tris[0].c = 7;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz =  DN1; 
      num_tris = 1; break;
    case  132:
      num_tris = 0; break;
    case  133:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 7;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 
      num_tris = 1; break;
    case  134:
      tris[0].a = 1;     tris[0].b = 7;      tris[0].c = 2;
      tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  135:
      tris[0].a = 0;     tris[0].b = 1;      tris[0].c = 7;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = DN1; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 2;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = -DN1; 
      /*
	tris[2].a = 1;     tris[2].b = 2;      tris[2].c = 7;
	tris[2].nx = 1.0;  tris[2].ny = 0.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 0;     tris[3].b = 2;      tris[3].c = 1;
	tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0; 
      */
      num_tris = 2; break;
    case  136:
      num_tris = 0; break;
    case  137:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 3;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  138:
      tris[0].a = 1;     tris[0].b = 7;      tris[0].c = 3;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 
      num_tris = 1; break;
    case  139:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 3;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 1;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 

      tris[2].a = 1;     tris[2].b = 3;      tris[2].c = 7;
      tris[2].nx = DN2;  tris[2].ny = -DN2;  tris[2].nz = -DN2; 

      /*
      tris[2].a = 0;     tris[2].b = 3;      tris[2].c = 1;
      tris[2].nx = 0.0;  tris[2].ny = -1.0;  tris[2].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  140:
      tris[0].a = 2;     tris[0].b = 7;      tris[0].c = 3;
      tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = 1.0; 
      num_tris = 1; break;
    case  141:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 3;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 2;      tris[1].c = 7;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = DN1; 

      /*
	tris[3].a = 0;     tris[3].b = 3;      tris[3].c = 2;
	tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0; 
	
	tris[1].a = 2;     tris[1].b = 3;      tris[1].c = 7;
	tris[1].nx = 0.0;  tris[1].ny = 0.0;   tris[1].nz = -1.0; 
      */

      num_tris = 2;	break;
    case  142:
      tris[0].a = 1;     tris[0].b = 7;      tris[0].c = 3;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 
      num_tris = 1; break;
    case  143:	
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 3;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 1;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 
      /*
	tris[2].a = 0;     tris[2].b = 3;      tris[2].c = 1;
	tris[2].nx = 0.0;  tris[2].ny = -1.0;  tris[2].nz = 0.0; 
      */
      num_tris = 2; break;
    case  144:
      num_tris = 0; break;
    case  145:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = DN1; 
      num_tris = 1; break;
    case  146:
      tris[0].a = 1;     tris[0].b = 7;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = DN1; 
      num_tris = 1; break;
    case  147:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = DN1; 

      tris[1].a = 0;     tris[1].b = 1;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 
      num_tris = 2; break;
    case  148:
      tris[0].a = 2;     tris[0].b = 7;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = 1.0; 
      num_tris = 1; break;
    case  149:
      tris[0].a = 2;     tris[0].b = 0;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = -DN2; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 4;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 
      /*
	tris[1].a = 2;     tris[1].b = 4;      tris[1].c = 7;
	tris[1].nx = 0.0;  tris[1].ny = 0.0;   tris[1].nz = -1.0; 
	
	tris[2].a = 0;     tris[2].b = 2;      tris[2].c = 7;
	tris[2].nx = DN1;  tris[2].ny = 0.0;   tris[2].nz = DN1; 
      */
      num_tris = 2;	break;
    case  150:
      tris[0].a = 1;     tris[0].b = 7;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = DN1; 

      tris[1].a = 2;     tris[1].b = 1;      tris[1].c = 4;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      /*
	tris[0].a = 2;     tris[0].b = 7;      tris[0].c = 1;
	tris[0].nx = 1.0;  tris[0].ny = 0.0;   tris[0].nz = 0.0; 
      */
      num_tris = 2; break;
    case  151:	

      tris[0].a = 2;     tris[0].b = 0;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = -DN2; 

      tris[1].a = 0;     tris[1].b = 1;      tris[1].c = 4;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 

      tris[2].a = 1;     tris[2].b = 7;      tris[2].c = 4;
      tris[2].nx = 0.0;  tris[2].ny = DN1;   tris[2].nz = DN1; 
      /*
	tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 1;
	tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0; 
	
	tris[1].a = 2;     tris[1].b = 7;      tris[1].c = 1;
	tris[1].nx = 1.0;  tris[1].ny = 0.0;   tris[1].nz = 0.0; 
	
	tris[2].a = 2;     tris[2].b = 4;      tris[2].c = 7;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = -1.0; 
      */
      num_tris = 3;	break;
    case 152:
      tris[0].a = 3;     tris[0].b = 7;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = 1.0; 
      num_tris = 1; break;
    case 153:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = DN1; 

      tris[1].a = 0;     tris[1].b = 3;      tris[1].c = 7;
      tris[1].nx = DN1;  tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      /*
	tris[0].a = 0;     tris[0].b = 3;      tris[0].c = 4;
	tris[0].nx = 1.0;  tris[0].ny = 0.0;   tris[0].nz = 0.0; 
	
	tris[2].a = 3;     tris[2].b = 7;      tris[2].c = 4;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = 1.0; 
      */
      num_tris = 2; break;
    case 154:
      tris[0].a = 3;     tris[0].b = 1;      tris[0].c = 4;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 3;
      tris[1].nx = -DN2; tris[1].ny = DN2;   tris[1].nz = DN2; 

      tris[2].a = 1;     tris[2].b = 7;      tris[2].c = 4;
      tris[2].nx = 0.0;  tris[2].ny = DN1;   tris[2].nz = DN1; 

      /*
	tris[2].a = 3;     tris[2].b = 7;      tris[2].c = 4;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = 1.0; 
      */
      num_tris = 3; break;
    case 155:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = DN1; 

      tris[1].a = 0;     tris[1].b = 1;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 

      tris[2].a = 1;     tris[2].b = 3;      tris[2].c = 7;
      tris[2].nx = DN2;  tris[2].ny = -DN2;  tris[2].nz = -DN2; 
      /*
	tris[0].a = 0;     tris[0].b = 4;      tris[0].c = 3;
	tris[0].nx = -1.0; tris[0].ny = 0.0;  tris[0].nz = 0.0; 
	
	tris[1].a = 0;     tris[1].b = 3;      tris[1].c = 1;
	tris[1].nx = 0.0;  tris[1].ny = -1.0;   tris[1].nz = 0.0; 
	
	tris[2].a = 4;     tris[2].b = 7;      tris[2].c = 3;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = -1.0; 
      */

      num_tris = 3;	break;
    case 156:
      tris[0].a = 3;     tris[0].b = 7;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = 1.0; 

      tris[1].a = 3;     tris[1].b = 2;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = 0.0;   tris[1].nz = 1.0; 

      num_tris = 2;	break;
    case 157:

      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 7;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 4;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 
      /*
	tris[0].a = 2;     tris[0].b = 4;      tris[0].c = 7;
	tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = -1.0; 
      */
      num_tris = 2; break;
    case  158:
      tris[0].a = 1;     tris[0].b = 4;      tris[0].c = 3;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 4;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 
      /*
	tris[2].a = 3;     tris[2].b = 4;      tris[2].c = 7;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = -1.0; 
      */
      num_tris = 2; break;
    case  159:
      tris[0].a = 0;     tris[0].b = 1;      tris[0].c = 7;
      tris[0].nx = 0.0;  tris[0].ny = DN1;   tris[0].nz = DN1; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 4;
      tris[1].nx = 0.0;  tris[1].ny = DN1;   tris[1].nz = DN1; 
      num_tris = 2; break;
    case 160:
      num_tris = 0; break;
    case  161:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 
      num_tris = 1; break;
    case  162:
      tris[0].a = 1;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = DN2;  tris[0].ny = DN2;   tris[0].nz = DN2; 
      num_tris = 1; break;
    case  163:
      tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 1;     tris[1].b = 5;      tris[1].c = 7;
      tris[1].nx = -DN2; tris[1].ny = -DN2;  tris[1].nz = -DN2; 

      tris[2].a = 0;     tris[2].b = 7;      tris[2].c = 1;
      tris[2].nx = 0.0;  tris[2].ny = -DN1;  tris[2].nz = -DN1; 
      /*
	tris[3].a = 0;     tris[3].b = 1;      tris[3].c = 5;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
      */
      num_tris = 3;	break;
    case  164:
      tris[0].a = 2;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 
      num_tris = 1; break;
    case  165:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 7;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = DN1; 
      num_tris = 2; break;
    case  166:	
      tris[0].a = 2;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 2;     tris[1].b = 1;      tris[1].c = 5;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 

      tris[2].a = 1;     tris[2].b = 7;      tris[2].c = 5;
      tris[2].nx = DN2;  tris[2].ny = DN2;   tris[2].nz = DN2; 
      /*
	tris[3].a = 1;     tris[3].b = 2;      tris[3].c = 7;
	tris[3].nx = 1.0;  tris[3].ny = 0.0;   tris[3].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  167:
      tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 2;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = -DN1; 

      tris[2].a = 1;     tris[2].b = 7;      tris[2].c = 5;
      tris[2].nx = DN2;  tris[2].ny = DN2;   tris[2].nz = DN2; 
      /*
	tris[5].a = 1;     tris[5].b = 2;      tris[5].c = 7;
	tris[5].nx = 1.0;  tris[5].ny = 0.0;   tris[5].nz = 0.0; 
	
	tris[2].a = 0;     tris[2].b = 2;      tris[2].c = 1;
	tris[2].nx = 0.0;  tris[2].ny = -1.0;  tris[2].nz = 0.0; 
	
	tris[3].a = 0;     tris[3].b = 1;      tris[3].c = 5;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
      */
      num_tris = 3;	break;
    case  168:
      tris[0].a = 3;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2; 
      num_tris = 1; break;
    case  169:
      tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 

      tris[1].a = 0;     tris[1].b = 3;      tris[1].c = 7;
      tris[1].nx = DN1;  tris[1].ny = -DN1;  tris[1].nz = 0.0; 

      tris[2].a = 0;     tris[2].b = 7;      tris[2].c = 5;
      tris[2].nx = DN1;  tris[2].ny = 0.0;   tris[2].nz = DN1; 
      /*
	tris[1].a = 3;     tris[1].b = 0;      tris[1].c = 5;
	tris[1].nx = -1.0; tris[1].ny = 0.0;   tris[1].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  170:
      tris[0].a = 3;     tris[0].b = 1;      tris[0].c = 5;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 3;     tris[1].b = 5;      tris[1].c = 7;
      tris[1].nx = -DN2; tris[1].ny = DN2;   tris[1].nz = -DN2; 

      tris[2].a = 1;     tris[2].b = 7;      tris[2].c = 5;
      tris[2].nx = DN2;  tris[2].ny = DN2;   tris[2].nz = DN2; 

      tris[3].a = 1;     tris[3].b = 3;      tris[3].c = 7;
      tris[3].nx = DN2;  tris[3].ny = -DN2;  tris[3].nz = -DN2; 
      num_tris = 4;	break;
    case  171:
      tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 

      tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = DN2; 

      tris[2].a = 1;     tris[2].b = 3;      tris[2].c = 7;
      tris[2].nx = DN2;  tris[2].ny = -DN2;  tris[2].nz = -DN2; 
      num_tris = 3; break;
    case  172:
      tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 
      
      tris[1].a = 3;     tris[1].b = 2;      tris[1].c = 5;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = DN1; 

      tris[2].a = 2;     tris[2].b = 7;      tris[2].c = 5;
      tris[2].nx = DN1;  tris[2].ny = 0.0;   tris[2].nz = DN1; 

      num_tris = 3; break;
    case  173:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 7;
      tris[0].nx = DN1;  tris[0].ny = 0.0;  tris[0].nz = DN1; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = DN1; 

      tris[2].a = 3;     tris[2].b = 5;      tris[2].c = 7;
      tris[2].nx = -DN2; tris[2].ny = DN2;   tris[2].nz = -DN2; 
      /*
	tris[5].a = 7;     tris[5].b = 2;      tris[5].c = 3;
	tris[5].nx = 0.0;  tris[5].ny = 0.0;   tris[5].nz = -1.0; 
	
	tris[2].a = 3;     tris[2].b = 2;      tris[2].c = 0;
	tris[2].nx = 0.0;  tris[2].ny = -1.0;  tris[2].nz = 0.0; 
	
	tris[3].a = 3;     tris[3].b = 0;      tris[3].c = 5;
	tris[3].nx = -1.0; tris[3].ny = 0.0;   tris[3].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  174:
      tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 

      tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = DN2; 

      tris[2].a = 3;     tris[2].b = 1;      tris[2].c = 5;
      tris[2].nx = -DN2;  tris[2].ny = -DN2;  tris[2].nz = DN2; 
      num_tris = 3; break;
    case  175:	
      tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 

      tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx =  DN2; tris[1].ny = DN2;   tris[1].nz = DN2; 
      num_tris = 2; break;
    case  176:
      tris[0].a = 4;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  177:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 0;     tris[1].b = 4;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = -DN1; 
      /*
	tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 4;
	tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
	
	tris[2].a = 4;     tris[2].b = 5;      tris[2].c = 7;
	tris[2].nx = 0.0;  tris[2].ny = 1.0;   tris[2].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  178:
      tris[0].a = 1;     tris[0].b = 5;      tris[0].c = 4;
      tris[0].nx = -DN1; tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = DN2; 

      tris[2].a = 1;     tris[2].b = 4;      tris[2].c = 7;
      tris[2].nx = 0.0;  tris[2].ny = -DN1;  tris[2].nz = -DN1; 
      /*
	tris[0].a = 4;     tris[0].b = 5;      tris[0].c = 7;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  179:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 1;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = -DN1; 

      tris[1].a = 0;     tris[1].b = 4;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;   tris[1].nz = -DN1; 

      tris[2].a = 1;     tris[2].b = 7;      tris[2].c = 5;
      tris[2].nx = DN2;  tris[2].ny = DN2;   tris[2].nz = DN2; 

      /*
	tris[3].a = 4;     tris[3].b = 5;      tris[3].c = 7;
	tris[3].nx = 0.0;  tris[3].ny = 1.0;   tris[3].nz = 0.0; 
	
	tris[5].a = 0;     tris[5].b = 1;      tris[5].c = 5;
	tris[5].nx = 0.0;  tris[5].ny = 0.0;   tris[5].nz = 1.0; 
	
	tris[2].a = 0;     tris[2].b = 5;      tris[2].c = 4;
	tris[2].nx = -1.0; tris[2].ny = 0.0;   tris[2].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  180:
      tris[0].a = 2;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = DN2;  tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[1].a = 2;     tris[1].b = 5;      tris[1].c = 4;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      /*
	tris[0].a = 4;     tris[0].b = 5;      tris[0].c = 7;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
	
	tris[2].a = 2;     tris[2].b = 4;      tris[2].c = 7;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = -1.0; 
      */
      num_tris = 2;	break;
    case  181:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 7;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = DN1;

      tris[2].a = 0;     tris[2].b = 4;      tris[2].c = 2;
      tris[2].nx = -DN2; tris[2].ny = -DN2;  tris[2].nz = -DN2; 

      /*
	tris[2].a = 0;     tris[2].b = 5;      tris[2].c = 4;
	tris[2].nx = -1.0; tris[2].ny = 0.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 4;     tris[3].b = 5;      tris[3].c = 7;
	tris[3].nx = 0.0;  tris[3].ny = 1.0;   tris[3].nz = 0.0; 
	
	tris[4].a = 2;     tris[4].b = 4;      tris[4].c = 7;
	tris[4].nx = 0.0;  tris[4].ny = 0.0;   tris[4].nz = -1.0; 
      */

      num_tris = 3;	break;
    case  182:
      tris[0].a = 1;     tris[0].b = 5;      tris[0].c = 4;
      tris[0].nx = -DN1; tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 1;     tris[1].b = 4;      tris[1].c = 2;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0;

      tris[2].a = 1;     tris[2].b = 7;      tris[2].c = 5;
      tris[2].nx = DN2;  tris[2].ny = DN2;   tris[2].nz = DN2; 
      /*
	tris[4].a = 1;     tris[4].b = 2;      tris[4].c = 7;
	tris[4].nx = 1.0;  tris[4].ny = 0.0;   tris[4].nz = 0.0; 
	
	tris[5].a = 2;     tris[5].b = 4;      tris[5].c = 7;
	tris[5].nx = 0.0;  tris[5].ny = 0.0;   tris[5].nz = -1.0; 
	
	tris[2].a = 4;     tris[2].b = 5;      tris[2].c = 7;
	tris[2].nx = 0.0;  tris[2].ny = 1.0;   tris[2].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  183:	
      tris[0].a = 2;     tris[0].b = 0;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = -DN2; 

      tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = DN2; 

      /*
	tris[7].a = 4;     tris[7].b = 5;      tris[7].c = 7;
	tris[7].nx = 0.0;  tris[7].ny = 1.0;   tris[7].nz = 0.0; 
	
	tris[4].a = 2;     tris[4].b = 4;      tris[4].c = 7;
	tris[4].nx = 0.0;  tris[4].ny = 0.0;   tris[4].nz = -1.0; 
	
	tris[5].a = 1;     tris[5].b = 2;      tris[5].c = 7;
	tris[5].nx = 1.0;  tris[5].ny = 0.0;   tris[5].nz = 0.0; 
	
	
	tris[0].a = 0;     tris[0].b = 1;      tris[0].c = 2;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;  tris[0].nz = 0.0; 
	
	tris[1].a = 0;     tris[1].b = 1;      tris[1].c = 5;
	tris[1].nx = 0.0;  tris[1].ny = 0.0;   tris[1].nz = 1.0;
	
	tris[2].a = 0;     tris[2].b = 5;      tris[2].c = 4;
	tris[2].nx = -1.0; tris[2].ny = 0.0;   tris[2].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  184:
      tris[0].a = 3;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2;

      /*
	tris[3].a = 3;     tris[3].b = 5;      tris[3].c = 4;
	tris[3].nx = -1.0; tris[3].ny = 0.0;   tris[3].nz = 0.0; 
	
	tris[4].a = 3;     tris[4].b = 4;      tris[4].c = 7;
	tris[4].nx = 0.0;  tris[4].ny = 0.0;   tris[4].nz = -1.0; 
	
	tris[5].a = 3;     tris[5].b = 7;      tris[5].c = 2;
	tris[5].nx = 0.0;  tris[5].ny = 0.0;   tris[5].nz = -1.0; 
	
	tris[0].a = 4;     tris[0].b = 5;      tris[0].c = 7;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
      */

      num_tris = 1;	break;
    case  185:
      tris[0].a = 0;     tris[0].b = 3;      tris[0].c = 7;
      tris[0].nx = DN1;  tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = DN1; 
      /*
	tris[0].a = 0;     tris[0].b = 4;      tris[0].c = 3;
	tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
	
	tris[1].a = 0;     tris[1].b = 5;      tris[1].c = 4;
	tris[1].nx = -1.0; tris[1].ny = 0.0;   tris[1].nz = 0.0;
	
	tris[2].a = 4;     tris[2].b = 5;      tris[2].c = 7;
	tris[2].nx = 0.0;  tris[2].ny = 1.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 3;     tris[3].b = 4;      tris[3].c = 7;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = -1.0; 
      */
      num_tris = 2;	break;
    case  186:
      tris[0].a = 1;     tris[0].b = 3;      tris[0].c = 7;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = -DN2; 

      tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = DN2;

      tris[2].a = 1;     tris[2].b = 5;      tris[2].c = 3;
      tris[2].nx = -DN2;  tris[2].ny = -DN2;  tris[2].nz = DN2; 
      num_tris = 3; break;
    case  187:
      tris[0].a = 1;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = DN2;  tris[0].ny = DN2;  tris[0].nz = DN2; 

      tris[1].a = 1;     tris[1].b = 3;      tris[1].c = 7;
      tris[1].nx = DN2;  tris[1].ny = -DN2;  tris[1].nz = -DN2;
      num_tris = 2; break;
    case  188:
      tris[0].a = 2;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 2;      tris[1].c = 5;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = DN1;
      num_tris = 2; break;
    case  189:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 5;
      tris[0].nx = DN1;  tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 2;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN1;  tris[1].ny = 0.0;   tris[1].nz = DN1;
      num_tris = 2; break;
    case  190:
      tris[0].a = 3;     tris[0].b = 1;      tris[0].c = 5;
      tris[0].nx = -DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 5;
      tris[1].nx = DN2;  tris[1].ny = DN2;   tris[1].nz = DN2;
      num_tris = 2; break;
    case  191:	
      tris[0].a = 1;     tris[0].b = 7;      tris[0].c = 5;
      tris[0].nx = DN2;  tris[0].ny = DN2;  tris[0].nz = DN2; 
      num_tris = 1; break;
    case  192:
      num_tris = 0; break;
    case  193:
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  194:
      tris[0].a = 1;     tris[0].b = 6;      tris[0].c = 7;
      tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  195:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 1;
      tris[0].nx = 0;    tris[0].ny = -DN1;  tris[0].nz = -DN1; 
      
      tris[1].a = 0;     tris[1].b = 6;      tris[1].c = 7;
      tris[1].nx = -DN1; tris[1].ny = DN1;   tris[1].nz = 0.0; 
      /*
	tris[3].a = 0;     tris[3].b = 1;      tris[3].c = 6;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
	
	tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 6;
	tris[1].nx = 1.0;  tris[1].ny = 0.0;   tris[1].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  196:
      tris[0].a = 2;     tris[0].b = 6;      tris[0].c = 7;
      tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0;
      num_tris = 1; break;
    case  197:
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 2;      tris[1].c = 6;
      tris[1].nx = DN2;  tris[1].ny = -DN2;  tris[1].nz = DN2; 

      tris[2].a = 0;     tris[2].b = 7;      tris[2].c = 2;
      tris[2].nx = -DN1; tris[2].ny = 0.0;   tris[2].nz = -DN1; 
      /*
	tris[0].a = 2;     tris[0].b = 7;      tris[0].c = 6;
	tris[0].nx = 1.0;  tris[0].ny = 0.0;   tris[0].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  198:
      tris[0].a = 1;     tris[0].b = 6;      tris[0].c = 7;
      tris[0].nx = -1.0;  tris[0].ny = 0.0;   tris[0].nz = 0.0; 

      tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 2;
      tris[1].nx = -1.0;  tris[1].ny = 0.0;   tris[1].nz = 0.0; 
      num_tris = 2; break;
    case  199:	
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 2;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = -DN1; 
      /*
	tris[2].a = 0;     tris[2].b = 1;      tris[2].c = 6;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = 1.0; 
	
	tris[3].a = 0;     tris[3].b = 2;      tris[3].c = 1;
	tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0; 
	
	tris[4].a = 1;     tris[4].b = 2;      tris[4].c = 6;
	tris[4].nx = 1.0;  tris[4].ny = 0.0;   tris[4].nz = 0.0; 
	
	tris[5].a = 2;     tris[5].b = 7;      tris[5].c = 6;
	tris[5].nx = 1.0;  tris[5].ny = 0.0;   tris[5].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  200:
      tris[0].a = 3;     tris[0].b = 6;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  201:
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 3;
      tris[1].nx = -DN1; tris[1].ny = DN1;   tris[1].nz = 0.0; 
      num_tris = 2; break;
    case  202:
      tris[0].a = 1;     tris[0].b = 6;      tris[0].c = 3;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 7;
      tris[1].nx = -DN1; tris[1].ny = DN1;   tris[1].nz = 0.0; 

      tris[2].a = 1;     tris[2].b = 3;      tris[2].c = 7;
      tris[2].nx = DN2;  tris[2].ny = -DN2;  tris[2].nz = -DN2; 
      /*
	tris[3].a = 1;     tris[3].b = 7;      tris[3].c = 6;
	tris[3].nx = 1.0;  tris[3].ny = 0.0;   tris[3].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  203:
      tris[0].a = 1;     tris[0].b = 3;      tris[0].c = 7;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = -DN2; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 7;
      tris[1].nx = -DN1; tris[1].ny = DN1;   tris[1].nz = 0.0; 

      tris[2].a = 0;     tris[2].b = 6;      tris[2].c = 3;
      tris[2].nx = -DN1; tris[2].ny = DN1;   tris[2].nz = 0.0; 
      /*
	tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 6;
	tris[1].nx = 1.0;  tris[1].ny = 0.0;   tris[1].nz = 0.0; 
	
	tris[2].a = 0;     tris[2].b = 1;      tris[2].c = 6;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = 1.0; 
	
	tris[3].a = 0;     tris[3].b = 3;      tris[3].c = 1;
	tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  204:
      tris[0].a = 3;     tris[0].b = 2;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 7;
      tris[1].nx = -DN1; tris[1].ny = DN1;   tris[1].nz = 0.0; 
      /*
	tris[1].a = 2;     tris[1].b = 7;      tris[1].c = 6;
	tris[1].nx = 1.0;  tris[1].ny = 0.0;   tris[1].nz = 0.0; 
	
	tris[2].a = 2;     tris[2].b = 3;      tris[2].c = 7;
	tris[2].nx = 0.0; tris[2].ny = 0.0;   tris[2].nz = -1.0; 
      */
      num_tris = 2;	break;
    case  205:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 7;
      tris[1].nx = -DN1; tris[1].ny = DN1;   tris[1].nz = 0.0; 

      tris[2].a = 0;     tris[2].b = 6;      tris[2].c = 3;
      tris[2].nx = -DN1; tris[2].ny = DN1;   tris[2].nz = 0.0; 

      /*
	tris[1].a = 2;     tris[1].b = 7;      tris[1].c = 6;
	tris[1].nx = 1.0;  tris[1].ny = 0.0;   tris[1].nz = 0.0; 
	
	tris[2].a = 2;     tris[2].b = 3;      tris[2].c = 7;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = -1.0; 
	
	tris[3].a = 0;     tris[3].b = 3;      tris[3].c = 2;
	tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  206:
      tris[0].a = 3;     tris[0].b = 6;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 3;     tris[1].b = 1;      tris[1].c = 6;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = DN1; 
      /*
	tris[4].a = 1;     tris[4].b = 7;      tris[4].c = 6;
	tris[4].nx = 1.0;  tris[4].ny = 0.0;   tris[4].nz = 0.0; 
	
	tris[5].a = 1;     tris[5].b = 2;      tris[5].c = 7;
	tris[5].nx = 1.0;  tris[5].ny = 0.0;   tris[5].nz = 0.0; 
	
	tris[1].a = 3;     tris[1].b = 7;      tris[1].c = 2;
	tris[1].nx = 0.0;  tris[1].ny = 0.0;   tris[1].nz = -1.0; 
	
	tris[2].a = 1;     tris[2].b = 3;      tris[2].c = 2;
	tris[2].nx = 0.0;  tris[2].ny = -1.0;  tris[2].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  207:
      tris[0].a = 3;     tris[0].b = 0;      tris[0].c = 6;
      tris[0].nx = -DN1; tris[0].ny = DN1;   tris[0].nz = 0.0; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 7;
      tris[1].nx = -DN1; tris[1].ny = DN1;   tris[1].nz = 0.0; 
      num_tris = 2; break;	
    case  208:
      tris[0].a = 4;     tris[0].b = 7;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  209:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 4;     tris[1].b = 0;      tris[1].c = 6;
      tris[1].nx = -DN2; tris[1].ny = DN2;   tris[1].nz = DN2; 

      tris[2].a = 0;     tris[2].b = 4;      tris[2].c = 7;
      tris[2].nx = 0.0;  tris[2].ny = -DN1;  tris[2].nz = -DN1; 
      num_tris = 3;	break;
    case  210:
      tris[0].a = 1;     tris[0].b = 4;      tris[0].c = 7;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = -DN1; 

      tris[1].a = 4;     tris[1].b = 1;      tris[1].c = 6;
      tris[1].nx = -DN1;  tris[1].ny = 0.0;   tris[1].nz = DN1; 
      num_tris = 2;	break;
    case  211:
      tris[0].a = 1;     tris[0].b = 4;      tris[0].c = 7;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = -DN1; 

      tris[1].a = 4;     tris[1].b = 0;      tris[1].c = 6;
      tris[1].nx = -DN2; tris[1].ny = DN2;   tris[1].nz = DN2; 

      tris[2].a = 4;     tris[2].b = 1;      tris[2].c = 0;
      tris[2].nx = 0.0;  tris[2].ny = -DN1;  tris[2].nz = -DN1; 

      /*
	tris[2].a = 1;     tris[2].b = 7;      tris[2].c = 6;
	tris[2].nx = 1.0;  tris[2].ny = 0.0;   tris[2].nz = 0.0; 

	tris[1].a = 4;     tris[1].b = 6;      tris[1].c = 7;
	tris[1].nx = 0.0;  tris[1].ny = 1.0;   tris[1].nz = 0.0; 
	
	tris[2].a = 6;     tris[2].b = 0;      tris[2].c = 1;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = 1.0; 
      */
      num_tris = 3;	break;
    case  212:
      tris[0].a = 2;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = DN2; 
      num_tris = 1;	break;
    case  213:
      tris[0].a = 4;     tris[0].b = 0;      tris[0].c = 6;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[1].a = 6;     tris[1].b = 0;      tris[1].c = 2;
      tris[1].nx = DN2;  tris[1].ny =-DN2;   tris[1].nz = DN2; 

      tris[2].a = 4;     tris[2].b = 2;      tris[2].c = 0;
      tris[2].nx = -DN2; tris[2].ny = -DN2;  tris[2].nz = -DN2; 
      num_tris = 3;	break;
    case  214:
      tris[0].a = 1;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = -DN2;   tris[0].nz = DN2; 

      tris[1].a = 1;     tris[1].b = 4;      tris[1].c = 2;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      num_tris = 2; break;
    case  215:
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[1].a = 0;     tris[1].b = 4;      tris[1].c = 2;
      tris[1].nx = -DN2; tris[1].ny = -DN2;  tris[1].nz = -DN2; 
      num_tris = 2; break;
    case  216:
      tris[0].a = 3;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 7;      tris[1].c = 6;
      tris[1].nx = DN1;  tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      /*
	tris[2].a = 6;     tris[2].b = 7;      tris[2].c = 4;
	tris[2].nx = 0.0;  tris[2].ny = 1.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 3;     tris[3].b = 4;      tris[3].c = 7;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = -1.0; 
      */ 
     num_tris = 2;	break;
    case  217:
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 6;
      tris[1].nx = DN1;  tris[1].ny = -DN1;  tris[1].nz = 0.0; 

      tris[2].a = 0;     tris[2].b = 3;      tris[2].c = 7;
      tris[2].nx = DN1;  tris[2].ny = -DN1;  tris[2].nz = 0.0; 
      /*
	tris[0].a = 0;     tris[0].b = 4;      tris[0].c = 3;
	tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
	
	tris[2].a = 6;     tris[2].b = 7;      tris[2].c = 4;
	tris[2].nx = 0.0;  tris[2].ny = 1.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 3;     tris[3].b = 4;      tris[3].c = 7;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = -1.0; 
      */

      num_tris = 3;	break;
    case  218:

      tris[0].a = 1;     tris[0].b = 3;      tris[0].c = 7;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = -DN2; 

      tris[1].a = 3;     tris[1].b = 1;      tris[1].c = 6;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = DN1; 

      tris[2].a = 3;     tris[2].b = 6;      tris[2].c = 4;
      tris[2].nx = -DN1; tris[2].ny = 0.0;   tris[2].nz = DN1; 
      /*
	tris[0].a = 6;     tris[0].b = 7;      tris[0].c = 4;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
	
	tris[1].a = 1;     tris[1].b = 7;      tris[1].c = 6;
	tris[1].nx = 1.0;  tris[1].ny = 0.0;   tris[1].nz = 0.0; 
	
	tris[3].a = 3;     tris[3].b = 4;      tris[3].c = 7;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = -1.0; 
      */
      num_tris = 3;	break;
    case  219:

      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[1].a = 1;     tris[1].b = 3;      tris[1].c = 7;
      tris[1].nx = DN2;  tris[1].ny = -DN2;  tris[1].nz = -DN2; 
      /*
	tris[0].a = 6;     tris[0].b = 7;      tris[0].c = 4;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
	
	tris[1].a = 3;     tris[1].b = 4;      tris[1].c = 7;
	tris[1].nx = 0.0;  tris[1].ny = 0.0;   tris[1].nz = -1.0; 
	
	tris[2].a = 0;     tris[2].b = 4;      tris[2].c = 3;
	tris[2].nx = -1.0; tris[2].ny = 0.0;   tris[2].nz = 0.0; 
	
	tris[4].a = 0;     tris[4].b = 1;      tris[4].c = 6;
	tris[4].nx = 0.0;  tris[4].ny = 0.0;  tris[4].nz = 1.0; 
	
	tris[5].a = 0;     tris[5].b = 3;      tris[5].c = 1;
	tris[5].nx = 0.0;  tris[5].ny = -1.0;  tris[5].nz = 0.0; 

	tris[6].a = 1;     tris[6].b = 7;      tris[6].c = 6;
	tris[6].nx = 1.0;  tris[6].ny = 0.0;   tris[6].nz = 0.0; 
      */

      num_tris = 2;	break;
    case  220:
      tris[0].a = 4;     tris[0].b = 3;      tris[0].c = 6;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 2;      tris[1].c = 6;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = DN2; 
      /*
	tris[0].a = 2;     tris[0].b = 3;      tris[0].c = 4;
	tris[0].nx = 0.0;  tris[0].ny = 0.0;   tris[0].nz = -1.0; 
      */

      num_tris = 2;	break;
    case  221:
      tris[0].a = 0;     tris[0].b = 6;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 

      tris[1].a = 6;     tris[1].b = 0;      tris[1].c = 2;
      tris[1].nx = DN2;  tris[1].ny =-DN2;   tris[1].nz = DN2; 

      /*tris[2].a = 4;     tris[2].b = 6;      tris[2].c = 2;
	tris[2].nx = DN2;  tris[2].ny = DN2;   tris[2].nz = -DN2; */
      num_tris = 2;	break;
    case  222:
      tris[0].a = 4;     tris[0].b = 3;      tris[0].c = 6;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 1;      tris[1].c = 6;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = DN1; 
      num_tris = 2;	break;
    case  223:	
      tris[0].a = 4;     tris[0].b = 0;      tris[0].c = 6;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = DN2; 
      num_tris = 1; break;
    case  224:
      tris[0].a = 7;     tris[0].b = 6;      tris[0].c = 5;
      tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0; 
      num_tris = 1; break;
    case  225:
      tris[0].a = 0;     tris[0].b = 7;     tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = -DN1; tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 5;      tris[1].c = 7;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = -DN1; 
      /*
	tris[1].a = 7;     tris[1].b = 5;      tris[1].c = 6;
	tris[1].nx = 0.0;  tris[1].ny = 1.0;   tris[1].nz = 0.0; 
	
	tris[2].a = 0;     tris[2].b = 6;      tris[2].c = 5;
	tris[2].nx = 0.0;  tris[2].ny = 0.0;   tris[2].nz = 1.0; 
      */
      
      num_tris = 2;	break;
     case  226:
      tris[0].a = 1;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = -DN2; 
      num_tris = 1; break;
    case  227:

      tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 1;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = -DN1; 
      /*
	tris[4].a = 0;     tris[4].b = 6;      tris[4].c = 5;
	tris[4].nx = 0.0;  tris[4].ny = 0.0;   tris[4].nz = 1.0; 
	
	tris[5].a = 0;     tris[5].b = 1;      tris[5].c = 6;
	tris[5].nx = 0.0;  tris[5].ny = 0.0;   tris[5].nz = 1.0; 
	
	tris[0].a = 5;     tris[0].b = 6;      tris[0].c = 7;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
	
	tris[2].a = 1;     tris[2].b = 7;      tris[2].c = 6;
	tris[2].nx = 1.0;  tris[2].ny = 0.0;   tris[2].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  228:
      tris[0].a = 2;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 2;     tris[1].b = 6;      tris[1].c = 5;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = DN1; 
      /*
	tris[3].a = 2;     tris[3].b = 7;      tris[3].c = 6;
	tris[3].nx = 1.0;  tris[3].ny = 0.0;   tris[3].nz = 0.0; 

	tris[1].a = 5;     tris[1].b = 6;      tris[1].c = 7;
	tris[1].nx = 0.0;  tris[1].ny = 1.0;   tris[1].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  229:
      tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 2;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 2;     tris[1].b = 5;      tris[1].c = 7;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = -DN1; 

      tris[2].a = 6;     tris[2].b = 0;      tris[2].c = 2;
      tris[2].nx = DN2;  tris[2].ny = -DN2;  tris[2].nz = DN2; 
      /*
	tris[5].a = 6;     tris[5].b = 2;      tris[5].c = 7;
	tris[5].nx = 1.0;  tris[5].ny = 0.0;   tris[5].nz = 0.0; 
	
	tris[2].a = 6;     tris[2].b = 7;      tris[2].c = 5;
	tris[2].nx = 0.0;  tris[2].ny = 1.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 6;     tris[3].b = 5;      tris[3].c = 0;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
      */
      num_tris = 3;	break;
    case  230:
      tris[0].a = 2;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = -DN2; 

      tris[1].a = 2;     tris[1].b = 1;      tris[1].c = 5;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      num_tris = 2; break;
    case  231:	
      tris[0].a = 0;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN1; tris[0].ny = 0.0;   tris[0].nz = -DN1; 

      tris[1].a = 0;     tris[1].b = 7;      tris[1].c = 2;
      tris[1].nx = -DN1; tris[1].ny = 0.0;   tris[1].nz = -DN1; 
      num_tris = 2; break;
    case  232:
      tris[0].a = 3;     tris[0].b = 6;      tris[0].c = 5;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 7;      tris[1].c = 6;
      tris[1].nx = DN1;  tris[1].ny = -DN1;  tris[1].nz = 0.0; 

      tris[2].a = 3;     tris[2].b = 5;      tris[2].c = 7;
      tris[2].nx = -DN2; tris[2].ny = DN2;   tris[2].nz = -DN2; 
      /*
	tris[0].a = 5;     tris[0].b = 6;      tris[0].c = 7;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
      */
      num_tris = 3;	break;
    case  233:
      tris[0].a = 5;     tris[0].b = 7;      tris[0].c = 3;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 

      tris[1].a = 0;     tris[1].b = 3;      tris[1].c = 6;
      tris[1].nx = DN1;  tris[1].ny = -DN1;  tris[1].nz = 0.0; 

      tris[2].a = 3;     tris[2].b = 7;      tris[2].c = 6;
      tris[2].nx = DN1;  tris[2].ny = -DN1;  tris[2].nz = 0.0; 
      /*
	tris[0].a = 5;     tris[0].b = 6;      tris[0].c = 7;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
	
	tris[2].a = 5;     tris[2].b = 3;      tris[2].c = 0;
	tris[2].nx = -1.0; tris[2].ny = 0.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 5;     tris[3].b = 0;      tris[3].c = 6;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
      */
      
      num_tris = 3;	break;
    case  234:
      tris[0].a = 3;     tris[0].b = 1;      tris[0].c = 5;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 3;     tris[1].b = 5;      tris[1].c = 7;
      tris[1].nx = -DN2; tris[1].ny = DN2;   tris[1].nz = -DN2; 

      tris[2].a = 3;     tris[2].b = 7;      tris[2].c = 1;
      tris[2].nx = DN2;  tris[2].ny = -DN2;  tris[2].nz = -DN2; 
      num_tris = 3; break;
    case  235:
      tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 7;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 

      tris[1].a = 1;     tris[1].b = 3;      tris[1].c = 7;
      tris[1].nx = DN2;  tris[1].ny = -DN2;  tris[1].nz = -DN2; 
      num_tris = 2; break;
    case  236:
      tris[0].a = 5;     tris[0].b = 2;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 5;      tris[1].c = 7;
      tris[1].nx = -DN2; tris[1].ny = DN2;   tris[1].nz = -DN2; 

      tris[2].a = 5;     tris[2].b = 3;      tris[2].c = 2;
      tris[2].nx = 0.0;  tris[2].ny = -DN1;  tris[2].nz = DN1; 
      /*
	tris[4].a = 2;     tris[4].b = 3;      tris[4].c = 7;
	tris[4].nx = 1.0;  tris[4].ny = 0.0;   tris[4].nz = 0.0; 
	
	tris[5].a = 7;     tris[5].b = 6;      tris[5].c = 2;
	tris[5].nx = 1.0;  tris[5].ny = 0.0;   tris[5].nz = 0.0; 
	
	tris[0].a = 5;     tris[0].b = 6;      tris[0].c = 7;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
      */

      num_tris = 3;	break;
    case  237:
      tris[0].a = 7;     tris[0].b = 3;      tris[0].c = 5;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 

      tris[1].a = 0;     tris[1].b = 2;      tris[1].c = 6;
      tris[1].nx = DN2;  tris[1].ny = -DN2;  tris[1].nz = DN2; 
      /*
	tris[7].a = 0;     tris[7].b = 6;      tris[7].c = 5;
	tris[7].nx = 0.0;  tris[7].ny = 0.0;   tris[7].nz = 1.0; 
	
	tris[0].a = 7;     tris[0].b = 5;      tris[0].c = 6;
	tris[0].nx = 0.0;  tris[0].ny = 1.0;   tris[0].nz = 0.0; 
	
	tris[2].a = 3;     tris[2].b = 0;      tris[2].c = 5;
	tris[2].nx = -1.0; tris[2].ny = 0.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 3;     tris[3].b = 2;      tris[3].c = 0;
	tris[3].nx = 0.0;  tris[3].ny = -1.0;  tris[3].nz = 0.0; 
	
	tris[4].a = 3;     tris[4].b = 7;      tris[4].c = 2;
	tris[4].nx = 0.0;  tris[4].ny = 0.0;   tris[4].nz = -1.0; 
	
	tris[5].a = 2;     tris[5].b = 7;      tris[5].c = 6;
	tris[5].nx = 1.0;  tris[5].ny = 0.0;   tris[5].nz = 0.0; 
      */
      num_tris = 2;	break;
    case  238:
      tris[0].a = 3;     tris[0].b = 1;      tris[0].c = 5;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 3;     tris[1].b = 5;      tris[1].c = 7;
      tris[1].nx = -DN2; tris[1].ny = DN2;   tris[1].nz = -DN2; 
      num_tris = 2; break;
    case  239:	
      tris[0].a = 7;     tris[0].b = 3;      tris[0].c = 5;
      tris[0].nx = -DN2; tris[0].ny = DN2;   tris[0].nz = -DN2; 
      num_tris = 1; break;
    case  240:
      tris[0].a = 5;     tris[0].b = 7;      tris[0].c = 6;
      tris[0].nx = 0.0;  tris[0].ny = -1.0;  tris[0].nz = 0.0; 

      tris[1].a = 5;     tris[1].b = 4;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = -1.0;  tris[1].nz = 0.0; 
      num_tris = 2; break;
    case  241:
      tris[0].a = 0;     tris[0].b = 7;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 0;     tris[1].b = 4;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = -DN1; 
      num_tris = 2; break;
    case  242:
      tris[0].a = 1;     tris[0].b = 5;      tris[0].c = 4;
      tris[0].nx = -DN1; tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 1;     tris[1].b = 4;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = -DN1; 
      /*
	tris[2].a = 1;     tris[2].b = 7;      tris[2].c = 6;
	tris[2].nx = 1.0;  tris[2].ny = 0.0;   tris[2].nz = 0.0; 
	
	tris[3].a = 1;     tris[3].b = 6;      tris[3].c = 5;
	tris[3].nx = 0.0;  tris[3].ny = 0.0;   tris[3].nz = 1.0; 
	
	tris[4].a = 5;     tris[4].b = 6;      tris[4].c = 7;
	tris[4].nx = 0.0;  tris[4].ny = 1.0;   tris[4].nz = 0.0; 
	
	tris[5].a = 5;     tris[5].b = 7;      tris[5].c = 4;
	tris[5].nx = 0.0;  tris[5].ny = 1.0;   tris[5].nz = 0.0; 
      */ 
      num_tris = 2;	break;    
    case  243:
      tris[0].a = 1;     tris[0].b = 0;      tris[0].c = 4;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = -DN1; 

      tris[1].a = 1;     tris[1].b = 4;      tris[1].c = 7;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = -DN1; 
      num_tris = 2; break;
    case  244:
      tris[0].a = 5;     tris[0].b = 4;      tris[0].c = 2;
      tris[0].nx = -DN1; tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 5;     tris[1].b = 2;      tris[1].c = 6;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = DN1; 
      /*
	tris[1].a = 5;     tris[1].b = 6;      tris[1].c = 4;
	tris[1].nx = 0.0;  tris[1].ny = 1.0;   tris[1].nz = 0.0; 
      */
      num_tris = 2; break;
    case  245:
      tris[0].a = 2;     tris[0].b = 0;      tris[0].c = 4;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = -DN2; 

      tris[1].a = 0;     tris[1].b = 2;      tris[1].c = 6;
      tris[1].nx = DN2;  tris[1].ny = -DN2;  tris[1].nz = DN2; 
      num_tris = 2; break;
    case  246:
      tris[0].a = 5;     tris[0].b = 4;      tris[0].c = 2;
      tris[0].nx = -DN1; tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 5;     tris[1].b = 2;      tris[1].c = 1;
      tris[1].nx = -DN1; tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      num_tris = 2; break;
    case  247:
      tris[0].a = 0;     tris[0].b = 4;      tris[0].c = 2;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = -DN2; 
      num_tris = 1; break;	
    case  248:
      tris[0].a = 3;     tris[0].b = 7;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 5;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = DN1; 
      /*
	tris[4].a = 5;     tris[4].b = 7;      tris[4].c = 4;
	tris[4].nx = 0.0;  tris[4].ny = 1.0;   tris[4].nz = 0.0; 
	
	tris[5].a = 5;     tris[5].b = 6;      tris[5].c = 7;
	tris[5].nx = 0.0;  tris[5].ny = 1.0;   tris[5].nz = 0.0; 
	
	tris[0].a = 3;     tris[0].b = 5;      tris[0].c = 4;
	tris[0].nx = -1.0; tris[0].ny = 0.0;   tris[0].nz = 0.0; 
	
	tris[1].a = 3;     tris[1].b = 4;      tris[1].c = 7;
	tris[1].nx = 0.0;  tris[1].ny = 0.0;   tris[1].nz = -1.0; 
      */
      num_tris = 2;	break;    
    case  249:
      tris[0].a = 3;     tris[0].b = 7;      tris[0].c = 6;
      tris[0].nx = DN1;  tris[0].ny = -DN1;  tris[0].nz = 0.0; 

      tris[1].a = 3;     tris[1].b = 6;      tris[1].c = 0;
      tris[1].nx = DN1;  tris[1].ny = -DN1;  tris[1].nz = 0.0; 
      num_tris = 2; break;
    case  250:
      tris[0].a = 3;     tris[0].b = 1;      tris[0].c = 5;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = DN2; 

      tris[1].a = 3;     tris[1].b = 7;      tris[1].c = 1;
      tris[1].nx = DN2;  tris[1].ny = -DN2;  tris[1].nz = -DN2; 
      num_tris = 2; break;
    case  251:
      tris[0].a = 3;     tris[0].b = 7;      tris[0].c = 1;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = -DN2; 
      num_tris = 1; break;
    case  252:
      tris[0].a = 3;     tris[0].b = 6;      tris[0].c = 5;
      tris[0].nx = 0.0;  tris[0].ny = -DN1;  tris[0].nz = DN1; 

      tris[1].a = 3;     tris[1].b = 2;      tris[1].c = 6;
      tris[1].nx = 0.0;  tris[1].ny = -DN1;  tris[1].nz = DN1; 
      num_tris = 2; break;
    case  253:
      tris[0].a = 0;     tris[0].b = 2;      tris[0].c = 6;
      tris[0].nx = DN2;  tris[0].ny = -DN2;  tris[0].nz = DN2; 
      num_tris = 1; break;
    case  254:
      tris[0].a = 3;     tris[0].b = 1;      tris[0].c = 5;
      tris[0].nx = -DN2; tris[0].ny = -DN2;  tris[0].nz = DN2; 
      num_tris = 1; break;
    case  255:	
      num_tris = 0; break;
    }
  
  *num_triangles = num_tris;
}

