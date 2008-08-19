#include "toqsh.h"

void bubble_sort(int *vals, int num);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure : 
%%
%%  Written By:
%%
%%  Parameters:
%%
%%  Purpose:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Sharpen_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  
  DEBUG_TRACE_IN printf("Entered Sharpen_ImagesCB\n");

  generic_redraw_manip_image(gui);

  refresh_manip_image(gui);
  DEBUG_TRACE_OUT printf("Done with Sharpen_ImagesCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure : 
%%
%%  Written By:
%%
%%  Parameters:
%%
%%  Purpose:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Blur_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  
  DEBUG_TRACE_IN printf("Entered Blur_ImagesCB\n");

  generic_redraw_manip_image(gui);

  refresh_manip_image(gui);
  DEBUG_TRACE_OUT printf("Done with Blur_ImagesCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure : 
%%
%%  Written By:
%%
%%  Parameters:
%%
%%  Purpose:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Median_Filter_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;

  generic_redraw_manip_image(gui);
  
  refresh_manip_image(gui);

}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure : 
%%
%%  Written By:
%%
%%  Parameters:
%%
%%  Purpose:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void mean_or_median_filter(unsigned char *new, unsigned char *old, int width, int height,int mask_size, int mean)
{
  int i,j,k,side, x,y;
  int sum,full_size,temp=0,must_divide=0;
  int *vals;

  side = (mask_size-1)/2;
  full_size = mask_size*mask_size;

  if (!mean) 
    vals = (int *) MT_malloc(sizeof(int)*full_size);
  else 
    vals = NULL;

  for (j=0;j<width;j++){
    for (i=0;i<height;i++){
      if (j<side || height-j<side || i<side || width-i<side){
	new[j*height+i] = old[j*height+i];
      }else{
        sum = 0;
	x = i-side; y = j-side;
	for (k=0;k<full_size;k++){
	  if (!mean) vals[k] = ((int)(unsigned char)old[y*width+x]);
	  sum += ((int)(unsigned char)old[y*width+x]);
	  x++;
	  if ((k+1)%mask_size == 0){
	    x = i-side; y++;
	  }
	}
	
	if (mean){
	  sum = (int)((float)sum/(float)full_size);
	  new[j*height+i] = (unsigned char)sum;
	  if (sum > 255) printf("sum is : %d\n",sum);
        }else{
	  bubble_sort(vals,full_size);
	  new[j*height+i] = vals[full_size/2-1];
        }	
      }
    }
  }
  if(vals != NULL)
    MT_free(vals);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure : 
%%
%%  Written By:
%%
%%  Parameters:
%%
%%  Purpose:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void bubble_sort(int *vals, int num)
{
  int i,j,temp;

  for (i=0;i<num;i++){
    for (j=i+1;j<num;j++){
      if (vals[i] > vals[j]){
	temp = vals[j]; vals[j] = vals[i];
	vals[i] = temp;
      }
    }
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure : 
%%
%%  Written By:
%%
%%  Parameters:
%%
%%  Purpose:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void apply_mask(unsigned char *ptr, int width, int height, 
		int mask_type)
{
  int MAX = -1000, MIN = 1000;
  int i,j,k,side, x,y,min_sum=100000,max_sum=-100000, sum_range;
  int sum,full_size,temp=0,must_divide=0;
  float /**fnew,*/ scale_factor, gamma_exp = 1.0;
  int *fnew;
  int mask_size;
  int *mask_vals;

  DEBUG_TRACE_IN printf("Entered apply_mask\n");

  mask_size = 3;

  mask_vals = (int *)MT_malloc(sizeof(int)*mask_size*mask_size);

  switch (mask_type){
  case SHARPEN:
    mask_vals[0] = -1;  mask_vals[1] = -1; mask_vals[2] = -1;
    mask_vals[3] = -1;  mask_vals[4] =  24; mask_vals[5] = -1;
    mask_vals[6] = -1;  mask_vals[7] = -1; mask_vals[8] = -1;
    gamma_exp = 1.5;
    break;
  case BLUR:
    mask_vals[0] = 1;  mask_vals[1] = 1; mask_vals[2] = 1;
    mask_vals[3] = 1;  mask_vals[4] = 1; mask_vals[5] = 1;
    mask_vals[6] = 1;  mask_vals[7] = 1; mask_vals[8] = 1;
    break;
  }

  side = (mask_size-1)/2;
  full_size = mask_size * mask_size;

  fnew = (int *)MT_malloc(sizeof(int)*width*height);
  
  /*for (i=0;i<full_size;i++) temp += mask_vals[i]; */
  
  for (j=0;j<height;j++){
    for (i=0;i<width;i++){
      if(ptr[j*width+i] > MAX) MAX=ptr[j*width+i];
      if(ptr[j*width+i] < MIN) MIN=ptr[j*width+i];
      
      if (j<side || width-j-1<side || i<side || height-i-1<side){
	fnew[j*width+i] = (int)ptr[j*width+i];
      }else{
        sum = 0;
	x = i-side; y = j-side;
	for (k=0;k<full_size;k++){
	  sum += ((int)ptr[y*width+x]) * mask_vals[k];
	  x++;
	  if ((k+1)%mask_size == 0){
	    x = i-side; y++;
	  }	  
	}
	if (sum > max_sum) max_sum = sum;
	if (sum < min_sum) min_sum = sum;
	
	fnew[j*width+i] = sum;
      }
    }
  }

  sum_range = max_sum-min_sum;
  scale_factor = (float)(MAX-MIN)/((float)sum_range);
  for(j=0;j<height;j++){
    for (i=0;i<width;i++){  
      if(j-side>=0 && j+side<height && i-side>=0 && i+side<width) {
	ptr[j*width+i] = (unsigned char)(pow(((fnew[j*width+i]-(float)min_sum)/sum_range),gamma_exp)*sum_range*scale_factor) + MIN;
      }
    }
  }

  MT_free((void *)fnew);
  MT_free(mask_vals);

  DEBUG_TRACE_OUT printf("Done with apply_mask\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure : 
%%
%%  Written By:
%%
%%  Parameters:
%%
%%  Purpose:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void equalize(main_gui_t *gui,int image_num)
{
  int min = 255,max=0,i,j,shift;
  int n[NUM_GRAYS];
  float r[NUM_GRAYS],pr[NUM_GRAYS],s[NUM_GRAYS],map[NUM_GRAYS];
  int num_pixels;
  unsigned char pixel;

  num_pixels = gui->qsh_gui->qsh_info->size_of_dimension[1] * gui->qsh_gui->qsh_info->size_of_dimension[2];
  /*  printf("Entered equalize, num_pixels is : %d\n",num_pixels);*/

  for (i=0;i<NUM_GRAYS;i++){ 
    r[i] = (float)i/(float)MAX_GRAY;
    n[i] = 0;
  }
  /*
  for (i=0;i<num_pixels;i++){
      n[(gui->manip_gui->image->data[i]-MIN_GRAY)]++;
  }
  */
  for (i=0;i<NUM_GRAYS;i++){
    pr[i] = (float)n[i]/(float)(num_pixels);
    
    if (i==0) s[i] = pr[i];
    else s[i] = s[i-1]+pr[i];
    
    map[i] = (int)(s[i] * (float)(NUM_GRAYS)/(float)(NUM_GRAYS));
    /*
      printf("group %d \thas %d pixels, \tpr is : %f, s[%d] : %f\n",
      i,n[i],pr[i],i,s[i]);*/
  }
  /*
    for (i=0;i<num_pixels;i++){
    gui->manip_gui->image->data[i] = (map[(unsigned char)gui->manip_gui->ximage->data[i]-MIN_GRAYS] * (float)NUM_GRAYS) + MIN_GRAY;    
    }
  */
}


