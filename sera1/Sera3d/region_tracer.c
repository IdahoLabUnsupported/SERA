#include "sera3d.h"


int Push_Position(main_gui_t *gui,Point_t pt,int num_so_far);
int Pop_Position(main_gui_t *gui,Point_t *pt,int *num_so_far);
void Clear_Stacks(main_gui_t *gui);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Clear_Stacks(main_gui_t *gui)
{
  /*DEBUG_TRACE_IN printf("Entered Clear_Stacks\n");*/

  gui->region_tracer.stack_position = -1;
  gui->region_tracer.points_so_far_stack_position = -1;

  /*DEBUG_TRACE_OUT printf("Done with Clear_Stacks\n");*/
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int Push_Position(main_gui_t *gui,Point_t pt,int num_so_far)
{
  gui->region_tracer.stack_position++;
  gui->region_tracer.stack[gui->region_tracer.stack_position].x = pt.x;
  gui->region_tracer.stack[gui->region_tracer.stack_position].y = pt.y;

  /*DEBUG_TRACE_IN printf("Entered Push_Position\n");*/

  DEBUG_DATA {
     printf("Pushing Position : %d, point # %d,  %d,%d\n",
	    gui->region_tracer.stack_position,num_so_far,
	    gui->region_tracer.stack[gui->region_tracer.stack_position].x,
	    gui->region_tracer.stack[gui->region_tracer.stack_position].y);
     printf("stack position is now : %d\n",gui->region_tracer.stack_position);
  }

  if (gui->region_tracer.stack_position == 500){
    printf("\nImage Tracer Point Stack Overflowed\n\n");
    return 0;
    /*exit(0);*/
  }

  gui->region_tracer.points_so_far_stack_position++;
  gui->region_tracer.points_so_far_stack[gui->region_tracer.points_so_far_stack_position] = num_so_far;

  /*DEBUG_TRACE_OUT printf("Done with  Push_Position\n");*/
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int Pop_Position(main_gui_t *gui,Point_t *pt, int *num_so_far)
{
  /*DEBUG_TRACE_IN printf("Entered Pop_Position\n");*/

  if (gui->region_tracer.stack_position == -1){
    printf("\nWARNING, Trying to Pop an empty stack\n\n");
    return 0;
  }
  pt->x = gui->region_tracer.stack[gui->region_tracer.stack_position].x;
  pt->y = gui->region_tracer.stack[gui->region_tracer.stack_position].y;
 
  DEBUG_LOADING   printf("Popping Position : %d\n",gui->region_tracer.stack_position);  

  gui->region_tracer.stack_position--; 

  DEBUG_LOADING   printf("stack position is now : %d\n",gui->region_tracer.stack_position);

  *num_so_far =   gui->region_tracer.points_so_far_stack[gui->region_tracer.points_so_far_stack_position];
  gui->region_tracer.points_so_far_stack_position--;
  

  /*DEBUG_TRACE_OUT printf("Done with Pop_Position\n");*/
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int  trace_region(main_gui_t *gui,unsigned char *array,int ncols, int nrows,Point_t start,
		  unsigned char region_val,
		  int body_num,Point_t *points)
{
   unsigned char *temp;
   int i,j;

   Point_t next;
   Point_t cur;
   
   int done_tracing = 0;
   int num_points_so_far = 0;
   int num_paths = 0;
   int num;

   DEBUG_TRACE_IN   printf("Entered trace_region\n");

   temp = build_hierarchical_region_slice(gui,array,region_val, body_num,ncols,nrows);

   Clear_Stacks(gui);
   
   cur.x = start.x; 
   cur.y = start.y;

   points[num_points_so_far].x = start.x;
   points[num_points_so_far].y = start.y;
   num_points_so_far ++;

   temp[start.y*ncols+start.x] = 3;

   while(1){
     /*printf("passing : lx:%d, ly:%d, sx:%d,sy:%d,cx:%d,cy:%d\n",
       last_x,last_y,start_x,start_y,cur_x,cur_y);*/
  /*printf("the current pt is : %d,%d\n",cur.x,cur.y);*/
     done_tracing = get_next_pixel_in_outline(temp,
					      start,
					      cur,
					      &next,
					      ncols,nrows,
					      &num_paths);
    
     /*printf("got the next pt it is : %d,%d\n",next.x,next.y);*/
     switch(done_tracing){
     case 0:
              points[num_points_so_far].x = next.x;
	      points[num_points_so_far].y = next.y;
	      num_points_so_far ++;
 
	      cur.x = next.x; cur.y = next.y;

	      /** mark the first 5 value of 3, so we can hit the start 
		easier **/
	      if (num_points_so_far > 5)
		temp[cur.y*ncols+cur.x] = 2;
	      else
		temp[cur.y*ncols+cur.x] = 3;
	      
	      if (num_points_so_far > 2000) {
		printf("\nWARNING, trace had > 1000 pts,stopping\n\n"); 
		MT_free( (void *) temp);
		num_points_so_far --;
		/*printf("\nreturning %d points\n",num_points_so_far);*/
		DEBUG_TRACE_OUT printf("Done with trace_region, returning %d pts\n",num_points_so_far);
		return num_points_so_far;
	      };
	      break;
     case 2:              
	      /**the tracer had more than one direction to go 
		better back up these settings in case it hits a
		dead end**/
            
       /*printf("PUSHING POINT, %d times\n",num_paths);*/
             for (num=0;num<num_paths-1;num++)
	       if (!Push_Position(gui,cur,num_points_so_far)) return 0;
	/*printf("DONE PUSHING THE POINT\n");*/
	     /*num_points_so_far--;*/

              points[num_points_so_far].x = next.x;
	      points[num_points_so_far].y = next.y;
	      num_points_so_far ++;
	      
	      cur.x = next.x; cur.y = next.y;

	      /** mark the first 5 value of 3, so we can hit the start 
		easier **/
	      if (num_points_so_far > 5)
		temp[cur.y*ncols+cur.x] = 2;
	      else
		temp[cur.y*ncols+cur.x] = 3;

	      break;
     case 3:
              /** the tracer hit a dead end, backup its steps and erase
	      ** that part of the region **/
              DEBUG_LOADING{
		printf("HIT a dead end, now backing up\n");
		printf("point # %d,   %d,%d\n",
		       num_points_so_far,
		       points[num_points_so_far].x,points[num_points_so_far].y);
	      }
	      
	      if (!Pop_Position(gui,&cur,&num_points_so_far)) return 0;

	      DEBUG_LOADING printf("backed up to : %d\n",num_points_so_far);

	      temp[cur.y*ncols+cur.x] = 1;
	      
	      break;
     case 5:
              /** we are done **/
              MT_free( (void *)temp);
	      /*num_points_so_far --;*/
	      DEBUG_LOADING   printf("returning %d points\n\n",num_points_so_far);

	      DEBUG_TRACE_OUT   printf("Done with trace_region, returning %d pts\n",num_points_so_far);
	      return num_points_so_far;
	      break;
     }
   }   

}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_next_pixel_in_outline(unsigned char *temp,
			      Point_t start,
			      Point_t cur,
			      Point_t *next,
			      int ncols, int nrows,
			      int *num_paths)
{
  int neighbor;
  int x,y;
  Point_t chosen;
  int num_choices=0;
  int found_next = 0;
  static int numpts = 0;

  /*DEBUG_TRACE_IN   printf("Entered get_next_pixel_in_outline\n");*/

  /** look at 8 neighbors **
           5 6 7  
           4 P 0
           3 2 1
  **************************/

  x = cur.x; y = cur.y;

  if (start.x == cur.x && start.y == cur.y) numpts = 0;

  /*printf("received : lx:%d, ly:%d, sx:%d,sy:%d,cx:%d,cy:%d\n",
       last_x,last_y,start_x,start_y,cur_x,cur_y);*/
  /*
  printf("Current  x : %d, y : %d Start  %d,%d\n",x,y,start.x,start.y);
  printf("temp[y*ncols+x] == %d\n",temp[y*ncols+x]);
  */
  for (neighbor=0;neighbor<8;neighbor++){
    /*printf("checking neighbor : %d\n",neighbor);*/
    switch(neighbor){
      /*      
    case 0: x = cur.x +1; y = cur.y;    break;
    case 1: x = cur.x +1; y = cur.y +1; break;
    case 2: x = cur.x;    y = cur.y +1; break;
    case 3: x = cur.x -1; y = cur.y +1; break;
    case 4: x = cur.x -1; y = cur.y;    break;
    case 5: x = cur.x -1; y = cur.y -1; break;
    case 6: x = cur.x;    y = cur.y -1; break;
    case 7: x = cur.x +1; y = cur.y -1; break;
    */

  /** look at 8 neighbors **
           7 0 1 
           6 P 2
           5 4 3
  **************************/
      /* 
   case 0: x = cur.x;    y = cur.y -1; break;
    case 1: x = cur.x +1; y = cur.y -1; break;
    case 2: x = cur.x +1; y = cur.y;    break;
    case 3: x = cur.x +1; y = cur.y +1; break;
    case 4: x = cur.x;    y = cur.y +1; break;
    case 5: x = cur.x -1; y = cur.y +1; break;
    case 6: x = cur.x -1; y = cur.y;    break;
    case 7: x = cur.x -1; y = cur.y -1; break;
    */


  /** look at 8 neighbors **
           5 4 3 
           6 P 2
           7 0 1
	   NOTE: so far this is the best,
	   because the regions are outline counterclockwise,
	   and this is a natural 8-neighbor counterclockwise
	   search.
  **************************/
       
    case 0: x = cur.x;    y = cur.y +1; break;
    case 1: x = cur.x +1; y = cur.y +1; break;
    case 2: x = cur.x +1; y = cur.y;    break;
    case 3: x = cur.x +1; y = cur.y -1; break;
    case 4: x = cur.x;    y = cur.y -1; break;
    case 5: x = cur.x -1; y = cur.y -1; break;
    case 6: x = cur.x -1; y = cur.y;    break;
    case 7: x = cur.x -1; y = cur.y +1; break;
    
    }
    /*
    printf("testing x : %d, y : %d\n",x,y);
    printf("temp[y*ncols+x] == %d\n",temp[y*ncols+x]);
    */
    if ((x >= 0) && (x <= ncols-1) && 
	(y >= 0) && (y <= nrows-1) && 
	((temp[y*ncols+x] == 1) || (temp[y*ncols+x] == 3)) &&
	check_4_neighbors(temp,x,y,ncols,nrows,0)){

      if ( ((x == start.x && y == start.y) || (temp[y*ncols+x] == 3)) && 
	  numpts > 10){
	  DEBUG_LOADING   printf("back to the start, returning\n");
	return 5;
      }else{
	num_choices++;
	if (x == 0 && y == 0) printf("WARNING:adding a 0,0 point\n");
	if (!found_next){
	  chosen.x = x;
	  chosen.y = y;
	  found_next = 1;
	}
      }
    }
  }    

  /*printf("number of choices is : %d\n",num_choices);*/
  if (num_choices == 1 || (cur.x == start.x && cur.y == start.y)){
    next->x = chosen.x;
    next->y = chosen.y;
    numpts++;
    return 0;
  }else if (num_choices > 1){ 
    next->x = chosen.x;
    next->y = chosen.y;
    numpts++;
    *num_paths = num_choices;
    return 2;
  }else{
    /*printf("\nUH Oh, hit a DEAD END\n"); */
    return 3;
  }
  /*  DEBUG_TRACE_OUT   printf("Done with get_next_pixel_in_outline\n");*/
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
unsigned char *build_hierarchical_region_slice(main_gui_t *gui,unsigned char *orig, unsigned char region_val,int body_num, int ncols, int nrows)
{
  int body_map[256],i;
  unsigned char *new;
  int current_bod;

  /*DEBUG_TRACE_IN   printf("Entered build_hierarchical_region_slice\n");*/

  for (i=0;i<256;i++) body_map[i] = 0;

  /** fill the current region and its contained regions with 1 **/
  body_map[gui->bod[body_num].region_num] = 1;

  for (i=0;i<gui->bod[body_num].num_contained_bods;i++){
    current_bod = gui->bod[body_num].contained_bods[i];
    body_map[ gui->bod[current_bod].region_num] = 1;
  }


  new = (unsigned char *)MT_malloc(sizeof(unsigned char)*ncols*nrows);
  
  for (i=0;i<ncols*nrows;i++) 
    new[i] = body_map[orig[i]];


  /*DEBUG_TRACE_OUT printf("Done with build_hierarchical_region_slice\n");*/
  return new;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int check_4_neighbors(unsigned char *array, 
		      int x, int y,
		      int ncols,int nrows,
		      unsigned char neighbor_color)
{
  /*** 
      3
    2 P 0
      1
      ***/
  /*printf("checking 4 neighbors for neighbor of : %d\n",neighbor_color);*/    
/*
  if(array[x*ncols + y] != base_color){ 
   printf("it doesn't equal the base color returning\n");
    return 0;
  }
  */

  if ( (x+1 <= ncols-1) && 
       (array[(y)*ncols + x+1] == neighbor_color)){
    /*    printf("neighbor 0 works\n");
     */
    return 1;
  } 
  else if ( (y+1 <= nrows-1) && 
	    (array[(y+1)*ncols + x] == neighbor_color)){
    /*    printf("neighbor 1 works\n");
     */
    return 1;
  }
  else if ( (x-1 >= 0) && 
	    (array[(y)*ncols + x-1] == neighbor_color)){
    /*    printf("neighbor 2 works\n");
     */
    return 1;
  }
  else if ( (y-1 >= 0) && 
	    (array[(y-1)*ncols + x] == neighbor_color)){
    /*    printf("neighbor 3 works\n");
     */
    return 1;
  }

  /** no neighbors match the specifications*/
  return 0;
}
