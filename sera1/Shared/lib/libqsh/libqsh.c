/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   The QSH Library.
%%
%%
%%
%%
%%
%%
%%
%%   General Debugging prints: compile with -DQSH_DEBUGGING_PRINTS
%%   Key matching Debugging prints compile with -DQSH_KEY_DEBUGGING_PRINTS
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#include <stdio.h>
#include <stdlib.h>
#include "libqsh.h"
#include "dialog_tools.h"

#define MAX_QSH_LINE_SIZE 512
#define QSH_KEY_VALUE_SPLIT_STRING ":="

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Local Prototypes
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_user_filled_values(qsh_gui_t *q);
void Apply_qhdCB(Widget w,XtPointer clientdata,XtPointer calldata);
void Cancel_qhdCB(Widget w,XtPointer clientdata,XtPointer calldata);
void load_key_mapping_file(qsh_gui_t *q);
void fill_qhd_popup_from_qhd_info(qsh_gui_t *q);
void print_validity(qsh_info_t *info);
void print_qsh_structure(qsh_info_t *info);
void copy_qsh_structure(qsh_info_t *dest,qsh_info_t *source);
char *string_to_lower(char *string);
char * string_to_upper(char *string);
void Bytes_Per_Pixel_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Show_Image_LocationsCB(Widget w, XtPointer clientdata, XtPointer calldata);
void build_image_locations_dialog(qsh_gui_t *q);
void Apply_Image_Locations_Dialog(Widget w, XtPointer clientdata, XtPointer calldata);
void Dismiss_Image_Locations_Dialog(Widget w, XtPointer clientdata, XtPointer calldata);
void Revert_keysCB(Widget w, XtPointer calldata, XtPointer clientdata);
int fill_image_location_popup_with_locations(qsh_gui_t *q);
void Add_Image_LocationCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Remove_Image_LocationCB(Widget w, XtPointer clientdata, XtPointer calldata);

void reverse_image_location_keys(qsh_gui_t *q);
void Reverse_keysCB(Widget w, XtPointer clientdata, XtPointer calldata);
int verify_ils_with_reference_and_spacing(float *ils,int num_ils,
				      float ref_loc, float uniform_spacing);
void Image_Referencing_ToggledCB(Widget w, XtPointer clientdata, XtPointer calldata);
void Revert_QhdCB(Widget w, XtPointer clientdata, XtPointer calldata);
int fill_qsh_image_locations_from_backup(qsh_gui_t *q);
int fill_qsh_image_locations_with_ref_and_spacing(qsh_gui_t *q);
void Apply_ref_and_spacingCB(Widget w, XtPointer clientdata,XtPointer calldata);
void Output_Current_qhdCB(Widget w, XtPointer clientdata, XtPointer calldata);
int  get_qhd_filename(Widget toplevel,XtAppContext app,char * title_string,char *filename, char *initial_string);
void qhd_saveCB(Widget w, XtPointer clientdata, XtPointer calldata);
void qhd_cancelCB(Widget w,XtPointer clientdata, XtPointer calldata);

int qsh_confirm(Widget toplevel,XtAppContext app,char *message);
void qsh_ConfirmedCB(Widget w, XtPointer clientdata, XtPointer calldata);
int qsh_error_popup(Widget toplevel,XtAppContext app,int options,char *message);
void qsh_errorpopup_fixCB (Widget w, XtPointer clientdata, XtPointer calldata);
void qsh_errorpopup_ignoreCB (Widget w, XtPointer clientdata, XtPointer calldata);

int process_qsh_key_and_value(qsh_gui_t *q, char *key, char *value);
int get_number_in_brackets_from_string(char *string);

int QSH_readln(FILE *fptr, char *s, int maxsize);
int QSH_readln_skip_comments_and_trim(FILE *fptr, char *s, int maxsize);
static void QSH_make_lower_string(char *s);
void QSH_break_into_lower_key_and_value(char * br, char * s,
				       char ** key, char ** value);
int QSH_read_next_key_and_value(FILE * fptr, char * str, int maxl, 
				char ** key, char ** value);
int QSH_trim_string(char * s);


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: read_qsh_pair
%%
%%  Written By: Cory Albright
%%
%%  Parameters: 
%%
%%  Purpose: taking either a .qhd or .qim file, will read in both the qhd and
%%           qim file into the qsh_info_t structure.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int read_qsh_pair(qsh_info_t *qsh_info, char *filename, 
		   Widget toplevel,XtAppContext app)
{
  static qsh_gui_t q;  /** this is the main gui structure **/
  char c;

  DEBUG_LIBQSH printf("Entered read_qsh_pair\n");
  
  q.qsh_toplevel = toplevel;
  q.qsh_info = qsh_info;
  q.qsh_app = app;

  q.mode = QSH_LOADING;

  if (strstr(filename,".gz")){
    printf("libqsh cannot read gzip files yet sorry\n");
    return 0;
  }

  if (!fill_correct_qhd_qim_filenames(&q,filename)) return 0;

  /******************************************************************/
  /** read the qhd   **/
  /******************************************************************/
  if (!read_qhd(&q)){
    printf("Could not read the qhd file, aborting\n");return 0;
  }

  /******************************************************************/
  /** after the qhd is successful get the images   **/
  /******************************************************************/
  if (!read_qim(&q)){
    printf("Could not read the qim file, aborting\n");return 0;
  }

  DEBUG_LIBQSH printf("Done with read_qsh_pair, leaving libqsh\n");


  return 1;
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    readQSHPairNoGui
%%%
%%%  Purpose:     Basically performs the same tasks as read_qsh_pair, but
%%%               the standard QSH header widget is never displayed. As a
%%%               result, this should be used when a qsh pair needs to be
%%%               reloaded, but it was previously loaded with read_qsh_pair.
%%%
%%%  Parameters:  qsh_info -> A ptr to a dynamically allocated qsh_info_t.
%%%               filename -> Either a .qhd or .qim file.
%%%
%%%  Returns:     1 on success, 0 on error
%%%
%%%  Notes:
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int readQSHPairNoGui( qsh_info_t * qsh_info, char * filename )
{
    static qsh_gui_t q;  /* the main gui structure */
    FILE * qsh_in;

    char buffer[MAX_QSH_LINE_SIZE];
    char * key;
    char * value;

    int returnValue = 0;
    
    DEBUG_LIBQSH printf("Entering readQSHPairNoQui\n");

    q.qsh_info = qsh_info;
    q.mode = QSH_LOADING;

    if( strstr( filename, ".gz" ) != NULL )
        printf("Libqsh cannot read gzip files yet. Sorry!\n");
    else
    {
        if( fill_correct_qhd_qim_filenames( &q, filename ) )
        {
            /* got the filenames, now read the files */
            qsh_in = fopen( q.qhd_filename, "r" );
            if( qsh_in != NULL )
            {
                /* Initialize, and load key mapping file */
                init_qsh_info_validity( q.qsh_info );
                load_key_mapping_file( &q );

                /* Loop through and process each key */
                while( QSH_read_next_key_and_value( qsh_in, buffer, 100, &key, &value ) )
                    process_qsh_key_and_value( &q, key, value );

                /* Done reading the file, close it */
                fclose( qsh_in );

                /* now read the qim file */
                if( read_qim( &q ) )
                    returnValue = 1;
            }
            else
                printf("Erroring opening the qhd file: %s\n", q.qhd_filename);
        }
    }

    DEBUG_LIBQSH printf("Leaving readQSHPairNoGui\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: read_qhd
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: reads the given .qhd file into the qsh_info_t structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int read_qhd(qsh_gui_t *q)
{
  FILE *qhd_in;
  char buffer[MAX_QSH_LINE_SIZE], *key,*value;

  DEBUG_LIBQSH printf("Entered read_qhd\n");


  if (!(qhd_in = fopen(q->qhd_filename,"r") )){
    printf("couldn't open the qhd file : %s\n",q->qhd_filename);
    return 0;
  }
  
  /******************************************************************/
  /** set all the validity keys in the structure to 0, since **/
  /**  none are valid yet **/
  /******************************************************************/
  init_qsh_info_validity(q->qsh_info); 

  /******************************************************************/
  /** load the key maps, these are for key 'aliases'    **/
  /******************************************************************/
  load_key_mapping_file(q); 
  
  /******************************************************************/
  /** now loop through each key in the file and process it   **/
  /******************************************************************/
  while (QSH_read_next_key_and_value(qhd_in,buffer,100,&key,&value)){
    process_qsh_key_and_value(q,key,value);
  }
  
  /******************************************************************/
  /** ok, now we have all the info extracted from the qhd   **/
  /**  we need to show the user what we found, what is missing, **/
  /**  and what they need to supply, this brings up the main **/
  /**  qsh dialog **/
  /******************************************************************/
  /*printf("calling check_and_report_qhd_values\n");*/
  if (!check_and_report_qhd_values(q)) return 0;

  /*printf("done with check_and_report_qhd_values\n");*/

  /******************************************************************/
  /** since we are now here, it means that all the info has been  **/
  /** supplied, and it is time to clean up and tell read_qsh_pair **/
  /** that we were successful in getting the qhd info **/
  /******************************************************************/
  /*free(q->keymap); removed it, it is being freed in Apply_qhdCB*/
  fclose(qhd_in);
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: read_qim
%%
%%  Written By: Cory Albright
%%
%%  Parameters: the main_qui_structure
%%
%%  Purpose: reads the qim (images) into the qsh_info_t structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int read_qim(qsh_gui_t *q)
{
  FILE *in;
  int full_size;
  int read_size;
  unsigned char *high,*low;

  DEBUG_LIBQSH  printf("Entered read_qim\n");
		   
  /******************************************************************/
  /** if the dimensions aren't valid we don't know how many  **/
  /**  images to read out of the file, so we must fail **/
  /******************************************************************/
  if (!q->qsh_info->valid.size_of_dimension[0] ||
      !q->qsh_info->valid.size_of_dimension[1] ||
      !q->qsh_info->valid.size_of_dimension[2]){
    printf("the dimension sizes are not available in the qsh_info structure,cannot read qim\n");
    return 0;
  }
  
  /******************************************************************/
  /** make sure we can open and read from the file  **/
  /******************************************************************/
  if (!(in = fopen(q->qim_filename,"r"))){
    printf("could not  open the QIM file : %s\n",q->qim_filename);
    return 0;
  }

  /******************************************************************/
  /** check to see if we about to read 1 or 2 byte images**/
  /******************************************************************/
  if (q->qsh_info->bytes_per_pixel == 2){
    /******************************************************************/
    /** if this is a 2 byte image set, we need to check to see  **/
    /** how the user wanted them handled **/
    /******************************************************************/
    if (q->qsh_info->byte_referencing == 1){
      /******************************************************************/
      /** ok, now we need to normalize the 2 bytes as they come in**/
      /******************************************************************/
      return read_qim_normalizing_2_bytes(q,in);
    }else if (q->qsh_info->byte_referencing == 2){
      return read_qim_splitting_2_bytes(q,in);
    }
  }else{
    /******************************************************************/
    /** OK, We are working with 1 byte images **/
    /** according to the qhd file, calculate what the full size of  **/
    /** the qim file will be, so we know how much to read **/
    /******************************************************************/
    full_size = q->qsh_info->size_of_dimension[0]*
      q->qsh_info->size_of_dimension[1]*
      q->qsh_info->size_of_dimension[2];
    
    /******************************************************************/
    /** make room for the images  **/
    /******************************************************************/
    if (!(q->qsh_info->images = (unsigned char *)MT_malloc(sizeof(unsigned char)*full_size))){
      printf("Couldn't allocate enough memory to load the qim\n");
      fclose(in);
      return 0;
    }			     

    DEBUG_LIBQSH printf("\tallocated the room for %d bytes (%d)images\n",full_size,q->qsh_info->size_of_dimension[0]);
    
    /******************************************************************/
    /** use fread to just read in the whole chunk,  **/
    /** watch to see if fread reads the expected amount **/
    /******************************************************************/
    read_size  = (int)fread(q->qsh_info->images,1,full_size,in);
    if (read_size < full_size){
      printf("\nWARNING, the size of the qim file did not match the qhd info\n");
      printf("\tread %d bytes, expecting %d bytes\n\n",read_size,full_size);
    }

    DEBUG_LIBQSH  printf("\tread in %d bytes (%d)images\n",read_size,
			 read_size/(q->qsh_info->size_of_dimension[1]*q->qsh_info->size_of_dimension[2]));

    /******************************************************************/
    /** we're done, clean up and tell read_qsh_pair we were successful**/
    /******************************************************************/
    fclose(in);
    q->qsh_info->valid.images = 1;
  }
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: read_qim_normalizing_2_bytes
%%
%%  Written By: Cory Albright
%%
%%  Parameters: the main_qui_structure
%%
%%  Purpose: reads the 2 byte qim (images),
%%           and normalizes the 2 bytes per pixel into 1
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int read_qim_normalizing_2_bytes(qsh_gui_t *q,FILE *in)
{
  int full_size;
  int i;
  int max,min,range;
/*
 *  CEW modification - 1/14/2003 - holding_value should be short, not unsigned int
 */
  short holding_value;
  unsigned char byte1,byte2;
  int big_endian = 0;
  Widget tempwidget;

  DEBUG_LIBQSH printf("Entered read_qim_normalizing_2_bytes\n");

  full_size = q->qsh_info->size_of_dimension[0]*
    q->qsh_info->size_of_dimension[1]*
    q->qsh_info->size_of_dimension[2];
  
  if (!(q->qsh_info->images = (unsigned char *)MT_malloc(sizeof(unsigned char)*full_size))){
    printf("Couldn't allocate enough memory to load the qim\n");
    fclose(in);
    return 0;
  }			
     

  /*  printf("checking to see if big endian or little\n");
      printf("mode is : %d\n",q->mode);
      printf("q->byte_order_menu is : %d\n",q->byte_order_menu);*/
      /*XtVaGetValues(q->byte_order_menu,XmNmenuHistory, &tempwidget,NULL);*/
  /*printf("the q->qsh_info_byte_order is : %s\n",q->qsh_info->byte_order);*/
  if (strcmp(q->qsh_info->byte_order,"Big Endian") == 0){
    /*printf("Using Big Endian\n");*/
    big_endian = 1;
  }else{
    /*printf("Using Little Endian\n");*/
    big_endian = 0;
  }
  /******************************************************************/
  /** first scan through and find the max value so we know  **/
  /** what to normalize then from **/
  /******************************************************************/
  max = 0;
  min = 100000;
  for (i=0;i<full_size;i++){
    if (!fread(&byte1,1,1,in)){
      printf("\nWARNING, the size of the qim file did not match the qhd info\n");
      printf("   there were not enough bytes in the file, have to abort\n");	  
      MT_free( (void*) q->qsh_info->high_byte_images);
      MT_free( (void*) q->qsh_info->low_byte_images);
      fclose(in);
      return 0;
    }
    if (!fread(&byte2,1,1,in)){
      printf("\nWARNING, the size of the qim file did not match the qhd info\n");
      printf("   there were not enough bytes in the file, have to abort\n");	  
      MT_free( (void*) q->qsh_info->high_byte_images);
      MT_free( (void*) q->qsh_info->low_byte_images);
      fclose(in);
      return 0;
    }
    
    if (big_endian){
      holding_value = (int)byte1*256;
      holding_value += (int)byte2;
    }else{
      holding_value = (int)byte2*256;
      holding_value += (int)byte1;
    }

/*
 *  CEW modification - 1/14/2003 - this change accomodates renormalization of
 *       CT images, where large negative values really messed up the contrast
 */
    if (holding_value < -1000) holding_value = -1000;

    if (holding_value > max) max = holding_value;
    if (holding_value < min) min = holding_value;
  }

  DEBUG_LIBQSH printf("found the max value it is : %d\n",max);
  DEBUG_LIBQSH printf("found the min value it is : %d\n",min);  
  range = max - min;

  rewind(in);
  
  for (i=0;i<full_size;i++){
    if (!fread(&byte1,1,1,in)){
      printf("\nWARNING, the size of the qim file did not match the qhd info\n");
      printf("   there were not enough bytes in the file, have to abort\n");	  
      MT_free( (void*) q->qsh_info->high_byte_images);
      MT_free( (void*) q->qsh_info->low_byte_images);
      fclose(in);
      return 0;
    }
    if (!fread(&byte2,1,1,in)){
      printf("\nWARNING, the size of the qim file did not match the qhd info\n");
      printf("   there were not enough bytes in the file, have to abort\n");	  
      MT_free( (void*) q->qsh_info->high_byte_images);
      MT_free( (void*) q->qsh_info->low_byte_images);
    }

    if (big_endian){
      holding_value = (int)byte1*256;
      holding_value += (int)byte2;
    }else{
      holding_value = (int)byte2*256;
      holding_value += (int)byte1;
    }

/*
 *  CEW modification - 1/14/2003 - this change accomodates renormalization of
 *       CT images, where large negative values really messed up the contrast
 */
    if (holding_value < -1000) holding_value = -1000;

    q->qsh_info->images[i] = (unsigned char)((float)(holding_value-min) * 255.0/(float)range);
    /*if (q->qsh_info->images[i] != 0) printf("found a value of %d\n",q->qsh_info->images[i]); */
  }
  q->qsh_info->valid.images = 1;
  q->qsh_info->bytes_per_pixel = 1;

  DEBUG_LIBQSH printf("Successful normalizing\n");
  fclose(in);
  return 1;
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: read_qim_splitting_2_bytes
%%
%%  Written By: Cory Albright
%%
%%  Parameters: the main_qui_structure
%%
%%  Purpose: reads the qim (images) into the qsh_info_t structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int read_qim_splitting_2_bytes(qsh_gui_t *q,FILE *in)
{
  int full_size;
  int i;
  unsigned char *high,*low;
  Widget tempwidget;
  int big_endian;

  DEBUG_LIBQSH printf("Entered read_qim_splitting_2_bytes\n");

  /******************************************************************/
  /** ok now we need to split the bytes as they come in **/
  /******************************************************************/  
  full_size = q->qsh_info->size_of_dimension[0]*
    q->qsh_info->size_of_dimension[1]*
    q->qsh_info->size_of_dimension[2];
  
  /*XtVaGetValues(q->byte_order_menu,XmNmenuHistory, &tempwidget,NULL);*/
  if (strcmp(q->qsh_info->byte_order,"Big Endian") == 0){
    DEBUG_LIBQSH printf("Using Big Endian\n");
    big_endian = 1;
  }else{
    DEBUG_LIBQSH printf("Using Little Endian\n");
    big_endian = 0;
  }

  /******************************************************************/
  /** make room for both the high and low byte images  **/
  /******************************************************************/
  if (!(q->qsh_info->high_byte_images = (unsigned char *)MT_malloc(sizeof(unsigned char)*full_size))){
    printf("Couldn't allocate enough memory to load the qim\n");
    fclose(in);
    return 0;
  }			     
  if (!(q->qsh_info->low_byte_images = (unsigned char *)MT_malloc(sizeof(unsigned char)*full_size))){
    printf("Couldn't allocate enough memory to load the qim\n");
    fclose(in);
    return 0;
  }			     
  
  for (i=0;i<full_size;i++){
    /******************************************************************/
    /** set the pointers to the correct byte in the arrays **/
    /******************************************************************/

    if (big_endian){
      high = &q->qsh_info->high_byte_images[i];
      low  = &q->qsh_info->low_byte_images[i];
    }else{
      high = &q->qsh_info->low_byte_images[i];
      low  = &q->qsh_info->high_byte_images[i];
    }
    /******************************************************************/
    /** read the next byte into high  **/
    /******************************************************************/
    if (!fread(high,1,1,in)){
      printf("\nWARNING, the size of the qim file did not match the qhd info\n");
      printf("   there were not enough bytes in the file, have to abort\n");	  
      MT_free( (void*) q->qsh_info->high_byte_images);
      MT_free( (void*) q->qsh_info->low_byte_images);
      fclose(in);
      return 0;
    }
    /******************************************************************/
    /** make room for both the high and low byte images  **/
    /******************************************************************/
    if (!fread(low,1,1,in)){
      printf("\nWARNING, the size of the qim file did not match the qhd info\n");
      printf("    there were not enough bytes in the file, have to abort");	  
      MT_free( (void*) q->qsh_info->high_byte_images);
      MT_free( (void*) q->qsh_info->low_byte_images);
      fclose(in);
      return 0;
    } 
    DEBUG_LIBQSH printf("split image #%d\n",i);       
  }
  
  /*q->qsh_info->valid.high_byte_images = 1;
  q->qsh_info->valid.low_byte_images = 1;
  */  

  /** TEMPORARY just set the qsh_info images to one of the split sets */
  q->qsh_info->valid.images = 1;

  if (big_endian){
    q->qsh_info->images = q->qsh_info->high_byte_images;
    MT_free( (void*) q->qsh_info->low_byte_images);
  }else{
    q->qsh_info->images = q->qsh_info->low_byte_images;
    MT_free( (void*) q->qsh_info->high_byte_images);
  }
  fclose(in);
  q->qsh_info->bytes_per_pixel = 1;

  DEBUG_LIBQSH printf("Successful splitting\n");
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: write_qhd
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: writes out the keys from the qsh_info_t structure to the file
%%           "filename"
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int write_qhd(qsh_info_t *qsh_info, char *filename)
{
  FILE *out;
  int i;

   DEBUG_LIBQSH printf("Entered write_qhd\n");

  /******************************************************************/
  /** make sure we can open the file and write to it  **/
  /******************************************************************/
  if (!(out = fopen(filename,"w"))){
    printf("Couldn't open the output file : %s\n",filename);
    return 0;
  }

  /******************************************************************/
  /** now write out the keys that we have loaded  **/
  /** NOTE:  the first fprintf prints a small header so **/
  /**        that we know the file was generated by libqsh **/
  /**        don't know if the header will cause a problem **/
  /**        with other code that would read the qhd file **/
  /**        libqsh doesn't have a problem reading it back in **/
  /******************************************************************/
  fprintf(out,"##############################################\n#  QHD file output by libqsh\n#############################################\n");

  if (qsh_info->valid.patient_name)
    fprintf(out,"Patient Name := %s\n",qsh_info->patient_name);

  if (qsh_info->valid.modality)
    fprintf(out,"Modality := %s\n",qsh_info->modality);

  if (qsh_info->valid.slice_orientation)
    fprintf(out,"Slice Orientation := %s\n",qsh_info->slice_orientation);

  if (qsh_info->valid.dimensionality)
    fprintf(out,"Dimensionality := %s\n",qsh_info->dimensionality);

  if (qsh_info->valid.bytes_per_pixel)
    fprintf(out,"Bytes Per Pixel := %d\n",qsh_info->bytes_per_pixel);
  
  if (qsh_info->valid.byte_order)
    fprintf(out,"Byte Order := %s\n",qsh_info->byte_order);
  /*
  if (qsh_info->valid.pixel_format)
    fprintf(out,"Pixel Format := %d\n",qsh_info->pixel_format);
  */
  if (qsh_info->valid.number_of_dimensions)
    fprintf(out,"Number of Dimensions := %d\n",qsh_info->number_of_dimensions);
  
  if (qsh_info->valid.number_of_dimensions)
    for (i=0;i<qsh_info->number_of_dimensions;i++){
      if (qsh_info->valid.size_of_dimension[i])
	fprintf(out,"Size of Dimension[%d] := %d\n",i,qsh_info->size_of_dimension[i]);
    }

  if (qsh_info->valid.reference_location)
    fprintf(out,"Reference Location := %f\n",qsh_info->reference_location);
  if (qsh_info->valid.uniform_spacing)
    fprintf(out,"Uniform Spacing := %f\n",qsh_info->uniform_spacing);

  if (qsh_info->valid.x_pixel_size)
    fprintf(out,"X Pixel Size := %f\n",qsh_info->x_pixel_size);
  
  if (qsh_info->valid.y_pixel_size)
    fprintf(out,"Y Pixel Size := %f\n",qsh_info->y_pixel_size);
  
  /******************************************************************/
  /** now output all of the image location keys (if we have them)  **/
  /******************************************************************/
  for (i=0;i<MAX_QSH_SLICES;i++){
    if (qsh_info->valid.image_location[i])
      fprintf(out,"Image Location[%d] := %f\n",i,qsh_info->image_location[i]);
  }
  fclose(out);
  
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: write_qim
%%
%%  Written By: Cory Albright
%%
%%  Parameters: qsh_info_t structure which has images, the filename 
%%
%%  Purpose: writes out the images into the file "filename" from the 
%%           qsh_info_t structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int write_qim(qsh_info_t *qsh_info, char *filename)
{
  FILE *out;
  int full_size;

  DEBUG_LIBQSH printf("Entered write_qim\n");

  /******************************************************************/
  /** make sure we have the dimension keys, so we know how much,  **/
  /** to write **/
  /******************************************************************/
  if (!qsh_info->valid.size_of_dimension[0] ||
      !qsh_info->valid.size_of_dimension[1] ||
      !qsh_info->valid.size_of_dimension[2]){
    printf("the dimension sizes are not available in the qsh_info structure,cannot write qim\n");
    return 0;
  }
  
  /******************************************************************/
  /** make sure we can open and write to the file  **/
  /******************************************************************/
  if (!(out = fopen(filename,"w"))){
    printf("couldn't open the file : %s\n",filename);
    return 0;
  }

  /******************************************************************/
  /** determine how many bytes we are going to write, this  **/
  /** is according to what the qhd says **/
  /******************************************************************/
  full_size = qsh_info->size_of_dimension[0]*
              qsh_info->size_of_dimension[1]*
              qsh_info->size_of_dimension[2]*
              qsh_info->bytes_per_pixel;

  printf("the fullsize of the qim file is : %d\n",full_size);

printf("dimen[0] : %d\n",qsh_info->size_of_dimension[0]);
printf("dimen[1] : %d\n",qsh_info->size_of_dimension[1]);
printf("dimen[2] : %d\n",qsh_info->size_of_dimension[2]);
printf("dimen[3] : %d\n",qsh_info->bytes_per_pixel);


  /******************************************************************/
  /** now use fwrite and dump the bytes to the file, keep watch  **/
  /** to see if we write the correct number of bytes **/
  /******************************************************************/
  if (fwrite(qsh_info->images,1,full_size,out) < full_size){
    printf("WARNING, did not fully write the qim file\n");
  }

  /******************************************************************/
  /** clean up and return success  **/
  /******************************************************************/
  fclose(out);
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: write_qim_single_image
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: will write out the image_numth image to the file "filename", from the 
%%           qsh_info_t structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int write_all_qim_single_images(qsh_info_t *qsh_info, char *initial_string,
				XtAppContext app, Widget toplevel)
{
    FILE *out;
    int image_size;
    unsigned char *ptr;
    int i, num_written, length;
    char filename[256],name_start[256];
    char temp[50];

    DEBUG_LIBQSH printf("Entered read_qsh_pair\n");

    /******************************************************************/
    /** if we don't have the dimension keys we don't know how **/
    /** many images are present, can't procede **/
    /******************************************************************/
    if (!qsh_info->valid.size_of_dimension[0] ||
        !qsh_info->valid.size_of_dimension[1] ||
        !qsh_info->valid.size_of_dimension[2]){
        printf("the dimension sizes are not available in the qsh_info structure,cannot write qim\n");
        return 0;
    }
  
    /******************************************************************/
    /** if the images are not present in the structure  **/
    /** we can output them **/
    /******************************************************************/
    if (!qsh_info->valid.images){
        printf("the images are not loaded in the qsh_info_t structure\n");
        return 0;
    }

    /******************************************************************/
    /** prompt the user for the filename to use  **/
    /******************************************************************/
    if (!get_qhd_filename(toplevel,app,"Enter a prefix for the raw images",name_start,initial_string)) return 0;

    /*
     * name_start should include a full path and a prefix.
     * Make sure that the user has supplied a prefix.
     */

    length = QSH_trim_string( name_start );
    if( length > 0 && name_start[ length - 1 ] != '/' )
    {
        /******************************************************************/
        /**   for each of the images :                                   **/
        /**   append the correct extension                               **/
        /**   example:   basename image_number . z_value , raw           **/
        /******************************************************************/

        for (i=0;i<qsh_info->size_of_dimension[0];i++)
        {
            sprintf( filename, "%s%03d.", name_start, i );
            
            sprintf(temp, "%.2f", qsh_info->image_location[i]);

            strcat(filename,temp);
            strcat(filename,",raw");

            /******************************************************************/
            /** make sure we can open the file and write to it **/
            /******************************************************************/
            if (!(out = fopen(filename,"w")))
            {
                printf("couldn't open the file : %s\n",filename);
                return 0;
            }

            /******************************************************************/
            /** determine the size of an image based on the qhd info  **/
            /******************************************************************/
            image_size = qsh_info->size_of_dimension[1] *
                qsh_info->size_of_dimension[2] *
                qsh_info->bytes_per_pixel;
            
            /******************************************************************/
            /** set the pointer to the right image in the array **/
            /******************************************************************/
            ptr = &qsh_info->images[ image_size * i ];
          
            /******************************************************************/
            /** dump out the size of one image to the file, keeping **/
            /** watch to make sure that fwrite dumps the right number **/
            /**  of bytes **/
            /******************************************************************/
            num_written = fwrite(ptr,1,image_size,out);
            if (num_written < image_size)
            {
                printf("WARNING, did not fully write the image file\n");
                printf("wrote %d, expected to write : %d\n",num_written,image_size);
            }
          
            /******************************************************************/
            /** now close that file and prepare to dump the next image **/
            /******************************************************************/
            fclose(out);
        }
        
        /******************************************************************/
        /** the image dumping was successful **/
        /******************************************************************/
        printf("DONE WRITING THE IMAGES\n");
        return 1;
    }
    else /* didn't have a valid prefix to start with */
        return( 0 );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: get_user_filled_values
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: gets the values filled into the qhd popup menu and fills them
%%           into the qsh_info_t structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_user_filled_values(qsh_gui_t *q)
{
  char temp[256];
  Widget tempwidget;
  int all_filled = 1;
  Boolean set1,set2;
  char missing_string[1500];

  DEBUG_LIBQSH printf("Entered get_user_filled_values, values are: \n");

  sprintf(missing_string,"Please fill in the needed values:\n\n");

  /**********************************************************************/
  /**  Patient Name **/
  /*********************************************************************/
  strcpy(q->qsh_info->patient_name,(char *)XmTextGetString(q->patient_name_tb));
  q->qsh_info->valid.patient_name = 1;

  DEBUG_LIBQSH printf("\tpatient_name is : %s\n",q->qsh_info->patient_name);

  /**********************************************************************/
  /**  Modality **/
  /*********************************************************************/
  XtVaGetValues(q->modality_menu,XmNmenuHistory, &tempwidget,NULL);
  if (strcmp(XtName(tempwidget),"Unknown") == 0){
     strcat(missing_string,"     Modality\n");
    all_filled = 0;
  }else{
    q->qsh_info->valid.modality = 1;
    strcpy(q->qsh_info->modality,XtName(tempwidget));

    DEBUG_LIBQSH printf("\tmodality is : %s\n",q->qsh_info->modality);
  }


  /**********************************************************************/
  /**  Slice Orientation **/
  /*********************************************************************/
  XtVaGetValues(q->slice_o_menu,XmNmenuHistory, &tempwidget,NULL);
  if (strcmp(XtName(tempwidget),"Unknown") == 0){
     strcat(missing_string,"     Slice Orientation\n");
    all_filled = 0;
  }else{
    q->qsh_info->valid.slice_orientation = 1;
    strcpy(q->qsh_info->slice_orientation,XtName(tempwidget));

    DEBUG_LIBQSH printf("\tslice_orientation is : %s\n",q->qsh_info->slice_orientation);
  }


  /**********************************************************************/
  /**  Dimensionality **/
  /*********************************************************************/
  XtVaGetValues(q->dimensionality_menu,XmNmenuHistory, &tempwidget,NULL);
  if (strcmp(XtName(tempwidget),"Unknown") == 0){
     strcat(missing_string,"     Dimensionality\n");
    all_filled = 0;
  }else{
    q->qsh_info->valid.dimensionality = 1;
    strcpy(q->qsh_info->dimensionality,XtName(tempwidget));

    DEBUG_LIBQSH printf("\tdimensionality is : %s\n",q->qsh_info->dimensionality);
  }


  /**********************************************************************/
  /**  Bytes Per Pixel **/
  /*********************************************************************/
  if (q->mode == QSH_LOADING){
    XtVaGetValues(q->bytes_per_pixel_menu,XmNmenuHistory, &tempwidget,NULL);
    if (strcmp(XtName(tempwidget),"Unknown") == 0){
      strcat(missing_string,"     Bytes Per Pixel\n");
      all_filled = 0;
    }else{
      q->qsh_info->valid.bytes_per_pixel = 1;
      q->qsh_info->bytes_per_pixel = atoi(XtName(tempwidget));
      
      DEBUG_LIBQSH printf("\tbytes_per_pixel is : %d\n",q->qsh_info->bytes_per_pixel);
    }
  }

  /**********************************************************************/
  /**  Byte Order **/
  /*********************************************************************/
  if (q->mode == QSH_LOADING){

    if (q->qsh_info->valid.bytes_per_pixel && q->qsh_info->bytes_per_pixel == 2){
      XtVaGetValues(q->byte_order_menu,XmNmenuHistory, &tempwidget,NULL);
      if (strcmp(XtName(tempwidget),"Unknown") == 0){
	strcat(missing_string,"     Bytes Order\n");
	all_filled = 0;
      }else{
	q->qsh_info->valid.byte_order = 1;
	strcpy(q->qsh_info->byte_order,XtName(tempwidget));
	
	DEBUG_LIBQSH printf("\tbyte_order is : %s\n",q->qsh_info->byte_order);
      }
      
      XtVaGetValues(q->byte_normalize_toggle,XmNset,&set1,NULL);
      XtVaGetValues(q->byte_split_toggle,XmNset,&set2,NULL);
      
      if (!set1 && !set2){
	strcat(missing_string,"     Byte Handling Option\n");
	all_filled = 0;
      }else if (set1){
	q->qsh_info->byte_referencing = 1;
      }else if (set2){
	q->qsh_info->byte_referencing = 2;
      }
    }

  }
  /**********************************************************************/
  /**  Size of Dimension X **/
  /*********************************************************************/
  strcpy(temp,(char *)XmTextGetString(q->size_of_dimensionX_tb));
  if (strlen(temp) < 1){
   strcat(missing_string,"     Size of Dimension X\n"); 
   all_filled = 0;
  }else{
    q->qsh_info->valid.size_of_dimension[1] = 1;
    q->qsh_info->size_of_dimension[1] = atoi(temp);

   DEBUG_LIBQSH printf("\tsize_of_dimensionX is : %d\n",q->qsh_info->size_of_dimension[1]);
  }


  /**********************************************************************/
  /**  Size of Dimension Y **/
  /*********************************************************************/
  strcpy(temp,(char *)XmTextGetString(q->size_of_dimensionY_tb));
  if (strlen(temp) < 1){
   strcat(missing_string,"     Size of Dimension Y\n"); 
    all_filled = 0;
  }else{
    q->qsh_info->valid.size_of_dimension[2] = 1;
    q->qsh_info->size_of_dimension[2] = atoi(temp);

    DEBUG_LIBQSH printf("\tsize_of_dimensionY is : %d\n",q->qsh_info->size_of_dimension[2]);
  }


  /**********************************************************************/
  /**  X Pixel Size **/
  /*********************************************************************/
  strcpy(temp,(char *)XmTextGetString(q->x_pixel_size_tb));
  if (strlen(temp) < 1){
    strcat(missing_string,"     X Pixel Size\n"); 
    all_filled = 0;
  }else{
    q->qsh_info->valid.x_pixel_size = 1;
    q->qsh_info->x_pixel_size = atof(temp);

    DEBUG_LIBQSH printf("\tx_pixel_size is : %f\n",q->qsh_info->x_pixel_size);
  }


  /**********************************************************************/
  /**  Y Pixel Size **/
  /**********************************************************************/
  strcpy(temp,(char *)XmTextGetString(q->y_pixel_size_tb));
  if (strlen(temp) < 1){
    strcat(missing_string,"     Y Pixel Size\n"); 
    all_filled = 0;
  }else{
    q->qsh_info->valid.y_pixel_size = 1;
    q->qsh_info->y_pixel_size = atof(temp);

    DEBUG_LIBQSH printf("\ty_pixel_size is : %f\n",q->qsh_info->y_pixel_size);
  }

  /**********************************************************************/
  /**  if one of the needed keys was not filled, all_filled **/
  /**  is set to 0, and the key name is added to missing_string **/
  /**  so if all_filled is 0, just bring up the error_popup with the **/
  /**  missing string **/
  /**********************************************************************/
  if (all_filled) return 1;
  else{
    qsh_error_popup(q->qsh_toplevel,q->qsh_app,1,missing_string);
    return 0;
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Apply_qhdCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: gets the user_filled values and closes the window
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Apply_qhdCB(Widget w,XtPointer clientdata,XtPointer calldata)
{
  qsh_gui_t *q;
  int i;
  q = (qsh_gui_t *)clientdata;

  DEBUG_LIBQSH printf("Entered Apply_qhdCB\n");
  
  /**********************************************************************/
  /** before we can apply the qhd values we have to check **/
  /** to make sure that the needed keys are filled and **/
  /** that there are no errors in the key values **/
  /**********************************************************************/
  if (get_user_filled_values(q) && error_check_qhd(q,QSH_ERROR_CHECK_FULL)){ 
    /**********************************************************************/
    /** passed the tests, the qhd is now ok **/
    /** now we need to do some cleaning up, and destroy **/
    /** the main qhd gui, if we need it again we will rebuild it **/
    /** this will prevent the qhd gui from holding memory when it **/
    /** is not being used **/
    /**********************************************************************/
    /*    printf("Entered the if after ignoring\n");*/

    q->values_set = 1;

    q->read_qhd_cancelled = 0;    

    /*printf("freeing %d mappings\n",q->num_mappings);*/
    for (i=0;i<q->num_mappings;i++){
      MT_free( (void*) q->keymap[i].key);
      MT_free( (void*) q->keymap[i].mapped_key);
    }
    if (q->num_mappings != 0){
      /*printf("freeing the q->keymap\n");*/
      MT_free( (void*) q->keymap);
    }

    /* Get rid of the image location popup if up */
    if( XtIsManaged( q->il.form ) )
    {
        XtUnmanageChild( q->il.form );
        XtDestroyWidget( q->il.shell );
    }
    
    XtDestroyWidget(q->qsh_dialog);
    /*printf("Destroyed the Widget\n");*/
  
    DEBUG_LIBQSH{
      printf("Leaving LIBQSH\n");
      /*print_qsh_structure(q->qsh_info);*/
    }
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Cancel_qhdCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Cancel_qhdCB(Widget w,XtPointer clientdata,XtPointer calldata)
{
  qsh_gui_t *q;
  q = (qsh_gui_t *)clientdata;

  DEBUG_LIBQSH printf("Entered Cancel_qhdCB\n");
  
  /*q->read_qhd_cancelled = 1;*/

  /* Get rid of the image location popup if up */
  if( XtIsManaged( q->il.form ) )
  {
      XtUnmanageChild( q->il.form );
      XtDestroyWidget( q->il.shell );
  }
  
  XtDestroyWidget(q->qsh_dialog);

  DEBUG_LIBQSH printf("Leaving LIBQSH\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: load_key_mapping_file
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: loads the alias file and puts the key maps into the key_map 
%%           structure, to be used when processing the keys in the qhd
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void load_key_mapping_file(qsh_gui_t *q)
{
  FILE *key_map_file;
  int i;
  char buffer[MAX_QSH_LINE_SIZE];
  char *r_path = getenv("SERA_RESOURCES");
  char path[256];
  char *key,*value;

  DEBUG_LIBQSH printf("Entered load_key_mapping_file\n");

  strcpy(path,r_path);
  strcat(path,"/LibQsh/qsh_key_mapping.info");

   DEBUG_LIBQSH printf("Loading the key mapping file : %s\n",path);

  /**********************************************************************/
  /** make sure that we can open the file and read from it **/
  /**********************************************************************/
  if (!(key_map_file = fopen(path,"r"))){
    printf("WARNING: could not open the qsh_key_mapping.info file, keys may not be found\n");
    return;
  }
  
  /**********************************************************************/
  /** make sure that we reset the number of mappings to 0 (in case **/
  /** it is not the first time we need to check qhd keys **/
  /**********************************************************************/
  q->num_mappings = 0;

  /**********************************************************************/
  /** scan through the key_mapping file to determine the number **/
  /** of mappings so we can allocate enough room for them **/
  /**********************************************************************/
  while (QSH_read_next_key_and_value(key_map_file,buffer,100,&key,&value)){
    q->num_mappings++;
  }

  /**********************************************************************/
  /** rewind the so we can get the key maps in the second pass **/
  /**********************************************************************/
  rewind(key_map_file);

  /**********************************************************************/
  /** make room for the keys **/
  /**********************************************************************/
  if (!(q->keymap = (qsh_key_map_t *) MT_malloc(sizeof(qsh_key_map_t)*q->num_mappings))){
    printf("ERROR:  Couldn't allocate enough memory for a qsh_key_map_t\n");
    return;
  }

  /**********************************************************************/
  /** now read through and load the key maps (aliases) into the structure**/
  /**********************************************************************/
  i=0;
  while (QSH_read_next_key_and_value(key_map_file,buffer,100,&key,&value)){
    /**********************************************************************/
    /** make room for the alias name**/
    /**********************************************************************/
    if (!(q->keymap[i].key = (char *) MT_malloc( sizeof(char)*(strlen(key)+1)))){
      printf("ERROR: Couldn't allocate enough memory for the string\n");
      return;;
    }
    /**********************************************************************/
    /** make room for the key name**/
    /**********************************************************************/
    if(!(q->keymap[i].mapped_key = (char *) MT_malloc( sizeof(char)*(strlen(value)+1)))){
      printf("ERROR:Couldn't allocate enough memory for the string\n");
      return;
    }

    /**********************************************************************/
    /** copy the strings into their spaces in the structure**/
    /**********************************************************************/
    strcpy(q->keymap[i].key,key);
    strcpy(q->keymap[i].mapped_key,value);
    i++;
  }  


  /**********************************************************************/
  /** we're done, close the file and leave**/
  /**********************************************************************/
  fclose(key_map_file);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: check_and_report_qhd_values
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: builds and brings up the qhd_popup, with the loaded qhd keys
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int check_and_report_qhd_values(qsh_gui_t *q){
  XmString xmstr;
  int i;

  DEBUG_LIBQSH printf("Entered check_and_report_qhd_values\n");

  q->qsh_dialog = (Widget)XmCreateMessageDialog(q->qsh_toplevel, "QHD Values",NULL,0);

  xmstr = XmStringCreateLocalized("QHD Keys");
  XtVaSetValues(q->qsh_dialog,
		XmNdialogTitle, xmstr,
		XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
                XmNdeleteResponse, XmUNMAP,
		NULL);
  XtAddCallback( XtParent(q->qsh_dialog), XmNpopdownCallback, Cancel_qhdCB, (XtPointer)q );

  XtVaSetValues(XtParent(q->qsh_dialog),
		XmNmwmDecorations, MWM_DECOR_ALL | MWM_DECOR_MENU | MWM_DECOR_RESIZEH | MWM_DECOR_MINIMIZE,
		NULL);

  XmStringFree(xmstr);

  /**********************************************************************/
  /** get rid of unwanted decorations **/
  /**********************************************************************/
  XtUnmanageChild((Widget)XmMessageBoxGetChild(q->qsh_dialog,XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(q->qsh_dialog,XmDIALOG_MESSAGE_LABEL));
  /*XtUnmanageChild((Widget)XmMessageBoxGetChild(q->qsh_dialog,XmDIALOG_CANCEL_BUTTON));*/
  XtUnmanageChild((Widget)XmMessageBoxGetChild(q->qsh_dialog,XmDIALOG_OK_BUTTON));
  /*XtUnmanageChild((Widget)XmMessageBoxGetChild(dialog,XmDIALOG_HELP_BUTTON));*/

  /**********************************************************************/
  /** NOTE:  we are using the help button on the messagedialog **/
  /**        because if we use the OK or CANCEL button, it automatically **/
  /**        closes the dialog whether we register a callback or not **/
  /**        and since we only want to close the dialog when everything **/
  /**        is correct, we need to use the help button which does not **/
  /**        automatically close the dialog when pressed **/
  /**  This could probably be revised, especially if contexthelp is used **/
  /**  in the dialog, but for now it works fine. **/
  /**********************************************************************/
  XtAddCallback(q->qsh_dialog,XmNhelpCallback,Apply_qhdCB,(XtPointer)q);
  

  XtAddCallback(q->qsh_dialog,XmNcancelCallback,Cancel_qhdCB,(XtPointer)q);

  /**********************************************************************/
  /** change the label on the button to "Apply"**/
  /**********************************************************************/
  xmstr = XmStringCreateLocalized("Apply");
  XtVaSetValues((Widget)XmMessageBoxGetChild(q->qsh_dialog,XmDIALOG_HELP_BUTTON),
		XmNlabelString, xmstr, NULL);
  XmStringFree(xmstr);

  /**********************************************************************/
  /** build the main forms**/
  /**********************************************************************/
  q->main_form = XtVaCreateManagedWidget("main_form",
			       xmFormWidgetClass, q->qsh_dialog,
			       NULL);
  /**********************************************************************/
  /** this is the row column widget for the left side of the dialog**/
  /**********************************************************************/
  q->rc1 = XtVaCreateManagedWidget("mainrc1",
				   xmRowColumnWidgetClass, q->main_form,
				   XmNorientation, XmVERTICAL,
				   XmNnumColumns, 2,
				   XmNpacking, XmPACK_COLUMN,
				   XmNrightAttachment, XmATTACH_NONE,
				   NULL);
  q->divider = XtVaCreateManagedWidget("divider",
				    xmSeparatorWidgetClass, q->main_form,
				    XmNleftAttachment,XmATTACH_WIDGET,
				    XmNleftWidget, q->rc1,
				    XmNorientation,XmVERTICAL,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment,XmATTACH_FORM,
				    XmNrightAttachment,XmATTACH_NONE,
				    XmNleftOffset,10,
				    XmNtopOffset,10,
				    XmNbottomOffset,10,
				    NULL);
  
  /**********************************************************************/
  /** these are the forms for the image location keys in the **/
  /** upper right of the dialog**/
  /**********************************************************************/
  q->image_ref_outer_form = XtVaCreateManagedWidget("out ref form",
						 xmFormWidgetClass, q->main_form,
						 XmNleftAttachment, XmATTACH_WIDGET,
						 XmNleftWidget, q->divider,
						 XmNleftOffset, 10,
						 XmNbottomAttachment, XmATTACH_NONE,
						 NULL);
  q->image_ref_frame = XtVaCreateManagedWidget("image ref frame",
					    xmFrameWidgetClass,q->image_ref_outer_form,
					    XmNshadowType, XmSHADOW_ETCHED_IN,
					    NULL);
  q->image_ref_form = XtVaCreateManagedWidget("out ref form",
					   xmFormWidgetClass, q->image_ref_frame,
					   NULL);


  /**********************************************************************/
  /** fill in the widgets on the left side of the box**/
  /**********************************************************************/ 
  XtVaCreateManagedWidget("Patient Name",
			  xmLabelWidgetClass,q->rc1,
			  NULL);
  XtVaCreateManagedWidget("Modality",
			  xmLabelWidgetClass,q->rc1,
			  NULL);
  XtVaCreateManagedWidget("Slice Orientation",
			  xmLabelWidgetClass,q->rc1,
			  NULL);  
  XtVaCreateManagedWidget("Dimensionality",
			  xmLabelWidgetClass,q->rc1,
			  NULL);
  XtVaCreateManagedWidget("Bytes Per Pixel",
			  xmLabelWidgetClass,q->rc1,
			  NULL);
  /*  XtVaCreateManagedWidget("Pixel Format",
			  xmLabelWidgetClass,q->rc1,
			  NULL);*/
  XtVaCreateManagedWidget("Number of Slices",
			  xmLabelWidgetClass,q->rc1,
			  NULL);


  XtVaCreateManagedWidget("Size of Dimension X (pixels)",
			  xmLabelWidgetClass,q->rc1,
			  NULL);
  XtVaCreateManagedWidget("Size of Dimension Y (pixels)",
			  xmLabelWidgetClass,q->rc1,
			  NULL);
  XtVaCreateManagedWidget("X Pixel Size",
			  xmLabelWidgetClass,q->rc1,
			  NULL);  
  XtVaCreateManagedWidget("Y Pixel Size",
			  xmLabelWidgetClass,q->rc1,
			  NULL);

  q->patient_name_tb = XtVaCreateManagedWidget("patientnametb",
					    xmTextFieldWidgetClass,q->rc1,
					    NULL);

  q->modality_pane = (Widget)XmCreatePulldownMenu(q->rc1,"Modality_pane",NULL, 0);
  q->modality_menu = (Widget)XtVaCreateManagedWidget("Modality_menu",xmRowColumnWidgetClass,q->rc1,
                                                XmNmarginHeight,       0,
                                                XmNmarginWidth,        0,
                                                XmNpacking,            XmPACK_TIGHT,
                                                XmNpopupEnabled,       TRUE,
                                                XmNrowColumnType,      XmMENU_OPTION,
                                                XmNspacing,            0,
                                                XmNsubMenuId, q->modality_pane,
                                                NULL);
  q->modality[0] = XtVaCreateManagedWidget("Unknown",xmPushButtonWidgetClass,q->modality_pane,NULL);
  q->modality[1] = XtVaCreateManagedWidget("MR",xmPushButtonWidgetClass,q->modality_pane,NULL);
  q->modality[2] = XtVaCreateManagedWidget("CT",xmPushButtonWidgetClass,q->modality_pane,NULL);
  q->modality[3] = XtVaCreateManagedWidget("PET",xmPushButtonWidgetClass,q->modality_pane,NULL);
  
  q->slice_o_pane = (Widget)XmCreatePulldownMenu(q->rc1,"slice_o_pane",NULL, 0);
  q->slice_o_menu = (Widget)XtVaCreateManagedWidget("slice_o_menu",xmRowColumnWidgetClass,q->rc1,
                                                    XmNmarginHeight,       0,
                                                    XmNmarginWidth,        0,
                                                    XmNpacking,            XmPACK_TIGHT,
                                                    XmNpopupEnabled,       TRUE,
                                                    XmNrowColumnType,      XmMENU_OPTION,
                                                    XmNspacing,            0,
                                                    XmNsubMenuId, q->slice_o_pane,
                                                    NULL);
  q->slice_o[0] = XtVaCreateManagedWidget("Unknown",xmPushButtonWidgetClass,q->slice_o_pane,NULL);
  q->slice_o[1] = XtVaCreateManagedWidget("Axial",xmPushButtonWidgetClass,q->slice_o_pane,NULL);
  q->slice_o[2] = XtVaCreateManagedWidget("Coronal",xmPushButtonWidgetClass,q->slice_o_pane,NULL);
  q->slice_o[3] = XtVaCreateManagedWidget("Sagittal",xmPushButtonWidgetClass,q->slice_o_pane,NULL);
  q->slice_o[4] = XtVaCreateManagedWidget("Oblique",xmPushButtonWidgetClass,q->slice_o_pane,NULL);

  q->dimensionality_pane = (Widget)XmCreatePulldownMenu(q->rc1,"dimensionality_pane",NULL, 0);
  q->dimensionality_menu = (Widget)XtVaCreateManagedWidget("dimensionality_menu",xmRowColumnWidgetClass,q->rc1,
                                                           XmNmarginHeight,       0,
                                                           XmNmarginWidth,        0,
                                                           XmNpacking,            XmPACK_TIGHT,
                                                           XmNpopupEnabled,       TRUE,
                                                           XmNrowColumnType,      XmMENU_OPTION,
                                                           XmNspacing,            0,
                                                           XmNsubMenuId, q->dimensionality_pane,
                                                           NULL);
  q->dimensionality[0] = XtVaCreateManagedWidget("Unknown",xmPushButtonWidgetClass,q->dimensionality_pane,NULL);
  q->dimensionality[1] = XtVaCreateManagedWidget("cm",xmPushButtonWidgetClass,q->dimensionality_pane,NULL);
  q->dimensionality[2] = XtVaCreateManagedWidget("mm",xmPushButtonWidgetClass,q->dimensionality_pane,NULL);


  if (q->mode == QSH_LOADING){
    q->bytes_per_pixel_pane = (Widget)XmCreatePulldownMenu(q->rc1,"bytes_per_pixel_pane",NULL, 0);
    q->bytes_per_pixel_menu = (Widget)XtVaCreateManagedWidget("bytes_per_pixel_menu",xmRowColumnWidgetClass,q->rc1,
                                                              XmNmarginHeight,       0,
                                                              XmNmarginWidth,        0,
                                                              XmNpacking,            XmPACK_TIGHT,
                                                              XmNpopupEnabled,       TRUE,
                                                              XmNrowColumnType,      XmMENU_OPTION,
                                                              XmNspacing,            0,
                                                              XmNsubMenuId,  q->bytes_per_pixel_pane,
                                                              NULL);
    q->bytes_per_pixel[0] = XtVaCreateManagedWidget("Unknown",xmPushButtonWidgetClass,q->bytes_per_pixel_pane,NULL);
    q->bytes_per_pixel[1] = XtVaCreateManagedWidget("1",xmPushButtonWidgetClass,q->bytes_per_pixel_pane,NULL);
    q->bytes_per_pixel[2] = XtVaCreateManagedWidget("2",xmPushButtonWidgetClass,q->bytes_per_pixel_pane,NULL);
    XtAddCallback(q->bytes_per_pixel[0],XmNactivateCallback,Bytes_Per_Pixel_ChangedCB,(XtPointer)q);
    XtAddCallback(q->bytes_per_pixel[1],XmNactivateCallback,Bytes_Per_Pixel_ChangedCB,(XtPointer)q);
    XtAddCallback(q->bytes_per_pixel[2],XmNactivateCallback,Bytes_Per_Pixel_ChangedCB,(XtPointer)q);
  }else{
    /** if we are in Edit mode, we don't want the bytes per pixel to be changeable **/
    q->bytes_per_pixel_menu = XtVaCreateManagedWidget("bytes_per_pixel_label",
						      xmLabelWidgetClass,q->rc1,
						      NULL);
  }
  /*
  pixel_format_tb = XtVaCreateManagedWidget("pixel format text box",
					    xmTextFieldWidgetClass,rc1,
					    NULL);
  */
  q->number_of_slices_label = XtVaCreateManagedWidget("num slices label",
						xmLabelWidgetClass,q->rc1,
						NULL);  

  q->size_of_dimensionX_tb = XtVaCreateManagedWidget("sod0 text box",
						  xmTextFieldWidgetClass,q->rc1,
						  NULL);
  q->size_of_dimensionY_tb = XtVaCreateManagedWidget("sod1 text box",
						  xmTextFieldWidgetClass,q->rc1,
						  NULL);
  q->x_pixel_size_tb = XtVaCreateManagedWidget("x_pixel_size_tb",
					    xmTextFieldWidgetClass,q->rc1,
					    NULL);  
  q->y_pixel_size_tb = XtVaCreateManagedWidget("y_pixel_size_tb",
					    xmTextFieldWidgetClass,q->rc1,
					    NULL);  

  /**********************************************************************/
  /** fill in the widgets on the right side of the dialog**/
  /**********************************************************************/
  q->image_locations_button = XtVaCreateManagedWidget("Show/Edit\nImage\nLocations",
						      xmPushButtonWidgetClass,q->image_ref_form,
						      XmNleftAttachment, XmATTACH_FORM,
						      XmNleftOffset, 375,
						      XmNrightAttachment, XmATTACH_FORM,
						      XmNrightOffset, 10,
						      XmNbottomAttachment, XmATTACH_FORM,
						      XmNbottomOffset, 10,
						      XmNtopAttachment, XmATTACH_FORM,
						      XmNtopOffset, 10,
						      XtVaTypedArg, XmNlabelString,XmRString,"Show/Edit\nImage\nLocations",strlen("Show/Edit\nImage\nLocations")+1,
						      NULL);
  XtAddCallback(q->image_locations_button,XmNactivateCallback,
		Show_Image_LocationsCB,(XtPointer)q);


  q->image_loc_label = XtVaCreateManagedWidget("Use Image Location Keys from File",
					    xmLabelWidgetClass,q->image_ref_form,
					    XmNleftAttachment, XmATTACH_FORM,
					    XmNleftOffset, 45,
					    XmNtopAttachment, XmATTACH_FORM,
					    XmNtopOffset, 10,
					    NULL);

  q->il_toggle = XtVaCreateManagedWidget(" ",
				      xmToggleButtonWidgetClass,q->image_ref_form,
				      XmNleftAttachment, XmATTACH_FORM,
				      XmNleftOffset, 10,
				      XmNtopAttachment, XmATTACH_FORM,
				      XmNtopOffset, 10,
                                      XmNhighlightThickness, 0,
				      NULL);
  XtAddCallback(q->il_toggle,XmNvalueChangedCallback,
		Image_Referencing_ToggledCB,(XtPointer)q);
  
  q->image_ref_divider = XtVaCreateManagedWidget("il divider",
					      xmSeparatorWidgetClass, q->image_ref_form,
					      XmNtopAttachment, XmATTACH_WIDGET,
					      XmNtopWidget, q->il_toggle,
					      XmNtopOffset, 10,
					      XmNorientation, XmHORIZONTAL,
					      XmNrightAttachment, XmATTACH_WIDGET,
					      XmNrightWidget, q->image_locations_button,
					      XmNleftAttachment, XmATTACH_FORM,
					      XmNshadowType, XmSHADOW_ETCHED_IN,
					      XmNleftOffset, 10,
					      XmNrightOffset,10,
					      NULL);
  

  q->ref_loc_label = XtVaCreateManagedWidget("Use Reference Location",
					  xmLabelWidgetClass,q->image_ref_form,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNleftOffset, 45,
					  XmNtopAttachment, XmATTACH_WIDGET,
					  XmNtopWidget, q->image_ref_divider,
					  XmNtopOffset, 15,
					  NULL);
  q->reference_location_tb = XtVaCreateManagedWidget("reference location text box",
						  xmTextFieldWidgetClass,q->image_ref_form,
						  XmNleftAttachment, XmATTACH_FORM,
						  XmNleftOffset, 250,
						  XmNwidth, 115,
						  XmNtopAttachment, XmATTACH_WIDGET,
						  XmNtopWidget, q->image_ref_divider,
						  XmNtopOffset, 15,
						  NULL);

  q->ref_toggle = XtVaCreateManagedWidget(" ",
				       xmToggleButtonWidgetClass,q->image_ref_form,
				       XmNleftAttachment, XmATTACH_FORM,
				       XmNleftOffset, 10,
				       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget, q->image_ref_divider,
				       XmNtopOffset, 45,
                                       XmNhighlightThickness, 0,   
				       NULL);
  XtAddCallback(q->ref_toggle,XmNvalueChangedCallback,
		Image_Referencing_ToggledCB,(XtPointer)q);

  q->ref_and_spacing_apply_button = 
    XtVaCreateManagedWidget("Apply",
			    xmPushButtonWidgetClass, q->image_ref_form,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNleftOffset, 185,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, q->image_ref_divider,
			    XmNtopOffset, 45,
			    NULL);
  XtAddCallback(q->ref_and_spacing_apply_button,
		XmNactivateCallback,Apply_ref_and_spacingCB,
		(XtPointer)q);

  q->uniform_spacing_label = XtVaCreateManagedWidget("and Uniform Thickness",
						  xmLabelWidgetClass,q->image_ref_form,
						  XmNleftAttachment, XmATTACH_FORM,
						  XmNleftOffset, 45,
						  XmNtopAttachment, XmATTACH_WIDGET,
						  XmNtopWidget, q->ref_loc_label,
						  XmNtopOffset, 35,
						  NULL);
  q->uniform_spacing_tb = XtVaCreateManagedWidget("uniform thickness text box",
					       xmTextFieldWidgetClass,q->image_ref_form,
					       XmNleftAttachment, XmATTACH_FORM,
					       XmNleftOffset, 250,
					       XmNtopAttachment, XmATTACH_WIDGET,
					       XmNtopWidget, q->reference_location_tb,
					       XmNtopOffset, 30,
					       XmNwidth, 115,
					       XmNbottomAttachment, XmATTACH_FORM,
					       XmNbottomOffset, 10,
					       NULL);

  q->tolerance_label = XtVaCreateManagedWidget("Image Spacing Tolerance (%)",
					    xmLabelWidgetClass, q->main_form,
					    XmNleftAttachment, XmATTACH_WIDGET,
					    XmNleftWidget, q->divider,
					    XmNtopAttachment, XmATTACH_WIDGET,
					    XmNtopWidget, q->image_ref_outer_form,
					    XmNleftOffset,10,
					    XmNtopOffset, 10,
					    NULL);
  q->tolerance_tb = XtVaCreateManagedWidget("uniform thickness text box",
					 xmTextFieldWidgetClass,q->main_form,
					 XmNleftAttachment, XmATTACH_WIDGET,
					 XmNleftWidget, q->tolerance_label,
					 XmNleftOffset, 5,
					 XmNwidth, 50,
					 XmNtopAttachment, XmATTACH_WIDGET,
					 XmNtopWidget, q->image_ref_outer_form,
					 XmNtopOffset, 10,
					 NULL);
  XmTextSetString(q->tolerance_tb,"1.0");


  q->output_qhd_button =XtVaCreateManagedWidget("Save qhd",
					     xmPushButtonWidgetClass, q->main_form,
					     XmNrightAttachment, XmATTACH_FORM,
					     XmNrightOffset, 1,
					     XmNbottomAttachment, XmATTACH_FORM,
					     XmNbottomOffset, 1,
					     NULL);

  XtAddCallback(q->output_qhd_button,XmNactivateCallback,
		Output_Current_qhdCB,(XtPointer)q);

  q->revert_qhd_button =XtVaCreateManagedWidget("Reset Qhd values",
					     xmPushButtonWidgetClass,q->main_form,
					     XmNrightAttachment, XmATTACH_WIDGET,
					     XmNrightWidget, q->output_qhd_button,
					     XmNrightOffset, 1,
					     XmNbottomAttachment, XmATTACH_FORM,
					     XmNbottomOffset, 1,
					     NULL);
  XtAddCallback(q->revert_qhd_button,XmNactivateCallback,
		Revert_QhdCB,(XtPointer)q);

  if (q->mode == QSH_LOADING){

    /**********************************************************************/
    /** build the byte options widgets**/
    /**********************************************************************/
    q->byte_options_outer_frame = XtVaCreateManagedWidget("Frame",
							  xmFrameWidgetClass, q->main_form,
							  XmNshadowType, XmSHADOW_ETCHED_IN,
							  XmNtopAttachment, XmATTACH_WIDGET,
							  XmNtopWidget, q->tolerance_tb,
							  XmNleftAttachment, XmATTACH_WIDGET,
							  XmNleftWidget, q->divider,
							  XmNrightAttachment, XmATTACH_FORM,
							  XmNbottomAttachment, XmATTACH_WIDGET,
							  XmNbottomWidget, q->revert_qhd_button,
							  XmNleftOffset, 5,
							  XmNrightOffset, 5,
							  XmNtopOffset, 5,
							  XmNbottomOffset, 5,
							  NULL);
    XtUnmanageChild(q->byte_options_outer_frame);
    q->byte_options_form = XtVaCreateManagedWidget("byte_options_form",
						   xmFormWidgetClass, q->byte_options_outer_frame,
						   NULL);
    
    q->byte_options_label = XtVaCreateManagedWidget("Handling of 2 byte images:",
						    xmLabelWidgetClass,q->byte_options_form,
						    XmNleftAttachment, XmATTACH_FORM,
						    XmNleftOffset, 25,
						    XmNtopAttachment, XmATTACH_FORM,
						    XmNtopOffset, 10,
						    NULL);
    
    q->byte_options_frame = XtVaCreateManagedWidget("Frame",
						    xmFrameWidgetClass, q->byte_options_form,
						    XmNshadowType, XmSHADOW_ETCHED_IN,
						    XmNleftAttachment, XmATTACH_FORM,
						    XmNrightAttachment, XmATTACH_FORM,
						    XmNtopAttachment, XmATTACH_WIDGET,
						    XmNtopWidget, q->byte_options_label,
						    XmNleftOffset, 25,
						    XmNrightOffset, 5,
						    NULL);
    
    q->byte_options_rc = XmCreateRadioBox(q->byte_options_frame, "byte_options_rc",NULL, 0);
    XtVaSetValues(q->byte_options_rc,XmNnumColumns, 1,NULL);
    XtManageChild(q->byte_options_rc);
    q->byte_normalize_toggle = (Widget)XtCreateManagedWidget("Normalize to 1 byte",
							     xmToggleButtonWidgetClass, q->byte_options_rc,
							     NULL, 0);
    q->byte_split_toggle = (Widget)XtCreateManagedWidget("Split the bytes",
							 xmToggleButtonWidgetClass, q->byte_options_rc,
							 NULL, 0);
    
    q->byte_order_label = XtVaCreateManagedWidget("Byte Order",
						  xmLabelWidgetClass,q->byte_options_form,
						  XmNtopAttachment, XmATTACH_WIDGET,
						  XmNtopWidget, q->byte_options_frame,
						  XmNleftAttachment , XmATTACH_FORM,
						  XmNleftOffset, 10,
						  NULL);
    q->byte_order_pane = (Widget)XmCreatePulldownMenu(q->byte_options_form,"byte_order_pane",NULL, 0);
    q->byte_order_menu = (Widget)XtVaCreateManagedWidget("byte_order_menu",xmRowColumnWidgetClass,q->byte_options_form,
                                                         XmNmarginHeight,       0,
                                                         XmNmarginWidth,        0,
                                                         XmNpacking,            XmPACK_TIGHT,
                                                         XmNpopupEnabled,       TRUE,
                                                         XmNrowColumnType,      XmMENU_OPTION,
                                                         XmNspacing,            0,
		                                         XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		                                         XmNtopWidget, q->byte_order_label,
		                                         XmNleftAttachment, XmATTACH_WIDGET,
		                                         XmNleftWidget, q->byte_order_label,
		                                         XmNleftOffset, 20,
		                                         XmNsubMenuId, q->byte_order_pane, 
		                                         NULL);
    q->byte_order[0] = XtVaCreateManagedWidget("Unknown",xmPushButtonWidgetClass,q->byte_order_pane,NULL);
    q->byte_order[1] = XtVaCreateManagedWidget("Little Endian",xmPushButtonWidgetClass,q->byte_order_pane,NULL);
    q->byte_order[2] = XtVaCreateManagedWidget("Big Endian",xmPushButtonWidgetClass,q->byte_order_pane,NULL);
      
  }


  if (q->mode == QSH_LOADING){
    /**********************************************************************/
    /** we need to backup the qhd info we pulled out of the file **/
    /** in case the user really messes things up and wants to revert**/
    /**********************************************************************/
    copy_qsh_structure(&q->backup_qsh, q->qsh_info);
  }

  /*printf("done with copy_qsh_structure\n");*/
  XtManageChild(q->qsh_dialog);  
  
  /**********************************************************************/
  /** fill in the qhd popup with the values pulled out of the file**/
  /**********************************************************************/
  fill_qhd_popup_from_qhd_info(q);
  
  /**********************************************************************/
  /** construct the image_locations dialog in so it is ready when **/
  /** the user wants to view/edit the keys**/
  /**********************************************************************/  
  build_image_locations_dialog(q);

  
  /**********************************************************************/
  /** error check the qhd and warn the user of any problems**/
  /**********************************************************************/
  /*error_check_qhd(q,QSH_ERROR_CHECK_JUST_WARN);*/


  q->read_qhd_cancelled = 1;
  q->user_done = 0;
  /**********************************************************************/
  /** hold in this procedure until the user done working with **/
  /** the qhd popup **/
  /**********************************************************************/
  /*printf("entering the AppProcess event loop in check_and_report_qhd\n");*/
  while (XtIsManaged(q->qsh_dialog)){
    XtAppProcessEvent(q->qsh_app, XtIMAll);
  }
  
  /*printf("past the appprocessing loop in check_and_report_qhd \n");*/

  if (q->read_qhd_cancelled) return 0;
  
  return 1;
}




/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: fill_qhd_popup_from_qhd_info
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: fills in the qhd_popup from the qsh_info_t structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void fill_qhd_popup_from_qhd_info(qsh_gui_t *q)
{
    char temp[256];
    int i,key_status = 0;
    XmString xmstr;

    DEBUG_LIBQSH printf("Entered fill_qhd_popup_from_qhd_info\n");

    /**********************************************************************/
    /** If valid, fill in the patient name**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the patient_name\n");
    if (q->qsh_info->valid.patient_name){
        /*printf("the patient name was valid\n");*/
        XmTextSetString(q->patient_name_tb, q->qsh_info->patient_name);
    }

    /**********************************************************************/
    /** If valid, fill in the modality**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the modality\n");
    if (q->qsh_info->valid.modality){
        string_to_lower(q->qsh_info->modality);
        if (strcmp(q->qsh_info->modality,"mr") == 0){
            XtVaSetValues(q->modality_menu,XmNmenuHistory,q->modality[1],NULL);
        }else if (strcmp(q->qsh_info->modality,"mri") == 0){
            XtVaSetValues(q->modality_menu,XmNmenuHistory,q->modality[1],NULL);
        }else if (strcmp(q->qsh_info->modality,"ct") == 0){
            XtVaSetValues(q->modality_menu,XmNmenuHistory,q->modality[2],NULL);
        }else if( strcmp( q->qsh_info->modality, "pet" ) == 0 ) {
            XtVaSetValues( q->modality_menu, XmNmenuHistory, q->modality[3], NULL );
        }else{
            XtVaSetValues(q->modality_menu,XmNmenuHistory,q->modality[0],NULL);
        }
    }else{
        XtVaSetValues(q->modality_menu,XmNmenuHistory,q->modality[0],NULL);
    }

    /**********************************************************************/
    /** If valid, fill in the slice orientation**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the slice_orientation\n");
    if (q->qsh_info->valid.slice_orientation){
        string_to_lower(q->qsh_info->slice_orientation);
        if (strcmp(q->qsh_info->slice_orientation,"axial") == 0){
            XtVaSetValues(q->slice_o_menu,XmNmenuHistory,q->slice_o[1],NULL);
        }else if (strcmp(q->qsh_info->slice_orientation,"transverse") == 0){
            XtVaSetValues(q->slice_o_menu,XmNmenuHistory,q->slice_o[1],NULL);
        }else if (strcmp(q->qsh_info->slice_orientation,"coronal") == 0){
            XtVaSetValues(q->slice_o_menu,XmNmenuHistory,q->slice_o[2],NULL);
        }else if (strcmp(q->qsh_info->slice_orientation,"sagittal") == 0){
            XtVaSetValues(q->slice_o_menu,XmNmenuHistory,q->slice_o[3],NULL);
        }else if (strcmp(q->qsh_info->slice_orientation,"oblique") == 0){
            XtVaSetValues(q->slice_o_menu,XmNmenuHistory,q->slice_o[4],NULL);
        }else{
            XtVaSetValues(q->slice_o_menu,XmNmenuHistory,q->slice_o[0],NULL);
        }
    }else{
        XtVaSetValues(q->slice_o_menu,XmNmenuHistory,q->slice_o[0],NULL);
    }

    /**********************************************************************/
    /** If valid, fill in the dimensionality**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the dimensionality\n");
    if (q->qsh_info->valid.dimensionality){
        string_to_lower(q->qsh_info->dimensionality);
        if (strcmp(q->qsh_info->dimensionality,"cm") == 0){
            XtVaSetValues(q->dimensionality_menu,XmNmenuHistory,q->dimensionality[1],NULL);
        }else if (strcmp(q->qsh_info->dimensionality,"mm") == 0){
            XtVaSetValues(q->dimensionality_menu,XmNmenuHistory,q->dimensionality[2],NULL);
        }else{
            XtVaSetValues(q->dimensionality_menu,XmNmenuHistory,q->dimensionality[0],NULL);
        }
    }else{
        XtVaSetValues(q->dimensionality_menu,XmNmenuHistory,q->dimensionality[0],NULL);
    }

    /**********************************************************************/
    /** If valid, fill in the bytes per pixel**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the bytes per pixel\n");
    if (q->mode == QSH_LOADING){
        if (q->qsh_info->valid.bytes_per_pixel){
            if (q->qsh_info->bytes_per_pixel == 1){
                XtVaSetValues(q->bytes_per_pixel_menu,XmNmenuHistory,q->bytes_per_pixel[1],NULL);
                /*
                  XtVaSetValues(byte_order[0],XmNsensitive,FALSE,NULL);
                  XtVaSetValues(byte_order[1],XmNsensitive,FALSE,NULL);
                  XtVaSetValues(byte_order[2],XmNsensitive,FALSE,NULL);*/
                  /*XtVaSetValues(byte_order_menu,XmNsensitive,FALSE,NULL);*/
                XtUnmanageChild(q->byte_order_label);
                XtUnmanageChild(q->byte_order_menu);
            }else if (q->qsh_info->bytes_per_pixel == 2){
                XtVaSetValues(q->bytes_per_pixel_menu,XmNmenuHistory,q->bytes_per_pixel[2],NULL);
                XtManageChild(q->byte_options_outer_frame);
            }else{
                XtVaSetValues(q->bytes_per_pixel_menu,XmNmenuHistory,q->bytes_per_pixel[0],NULL);
            }
        }else{
            XtVaSetValues(q->bytes_per_pixel_menu,XmNmenuHistory,q->bytes_per_pixel[0],NULL);
        }
    }else{
        if (q->qsh_info->valid.bytes_per_pixel){
            /*printf("the bytes per pixel is : %d\n",q->qsh_info->bytes_per_pixel);*/
            sprintf(temp,"%d",q->qsh_info->bytes_per_pixel);
            xmstr = XmStringCreateLocalized(temp);
            XtVaSetValues(q->bytes_per_pixel_menu,XmNlabelString,xmstr,NULL);
            XmStringFree(xmstr);
        }
    }
    /**********************************************************************/
    /** If valid, fill in the byte order**/
    /**********************************************************************/
    if (q->mode == QSH_LOADING){
        DEBUG_LIBQSH printf("going to fill the byte_order\n");
        if (q->qsh_info->valid.byte_order){
            string_to_lower(q->qsh_info->byte_order);
            if (strcmp(q->qsh_info->byte_order,"little endian") == 0){
                XtVaSetValues(q->byte_order_menu,XmNmenuHistory,q->byte_order[1],NULL);
            }else if (strcmp(q->qsh_info->byte_order,"big endian") == 0){
                XtVaSetValues(q->byte_order_menu,XmNmenuHistory,q->byte_order[2],NULL);
            }else{
                XtVaSetValues(q->byte_order_menu,XmNmenuHistory,q->byte_order[0],NULL);
            }
        }else{
            XtVaSetValues(q->byte_order_menu,XmNmenuHistory,q->byte_order[0],NULL);
        }
    }
    /*
      if (qsh_info->valid.pixel_format){
      sprintf(temp,"%d",qsh_info->pixel_format);
      XmTextSetString(pixel_format_tb,temp);
      }
    */

    /**********************************************************************/
    /** If valid, fill in the size of dimension[0]**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the size_of_dimension[0]\n");
    if (q->qsh_info->valid.size_of_dimension[0]){
        sprintf(temp,"%d",q->qsh_info->size_of_dimension[0]);
        xmstr = XmStringCreateLocalized(temp);
        XtVaSetValues(q->number_of_slices_label, XmNlabelString,xmstr,NULL);
        XmStringFree(xmstr);
    }

    /**********************************************************************/
    /** If valid, fill in the size of dimension [1]**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the size_of_dimension[1]\n");
    if (q->qsh_info->valid.size_of_dimension[1]){
        sprintf(temp,"%d",q->qsh_info->size_of_dimension[1]);
        XmTextSetString(q->size_of_dimensionX_tb,temp);
    }

    /**********************************************************************/
    /** If valid, fill in the size of dimension[2]**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the size_of_dimension[2]\n");
    if (q->qsh_info->valid.size_of_dimension[2]){
        sprintf(temp,"%d",q->qsh_info->size_of_dimension[2]);
        XmTextSetString(q->size_of_dimensionY_tb,temp);
    }

    /**********************************************************************/
    /** If valid, fill in the reference location**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the reference_location\n");
    if (q->qsh_info->valid.reference_location){
        sprintf(temp,"%f",q->qsh_info->reference_location);
        XmTextSetString(q->reference_location_tb,temp);
    }

    /**********************************************************************/
    /** If valid, fill in the uniform spacing**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the uniform_spacing\n");
    if (q->qsh_info->valid.uniform_spacing){
        sprintf(temp,"%f",q->qsh_info->uniform_spacing);
        XmTextSetString(q->uniform_spacing_tb,temp);
    }

    /**********************************************************************/
    /** If valid, fill in the x pixel size**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the x_pixel_size\n");
    if (q->qsh_info->valid.x_pixel_size){
        sprintf(temp,"%f",q->qsh_info->x_pixel_size);
        XmTextSetString(q->x_pixel_size_tb,temp);
    }

    /**********************************************************************/
    /** If valid, fill in the y pixel size**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the y_pixel_size\n");
    if (q->qsh_info->valid.y_pixel_size){
        sprintf(temp,"%f",q->qsh_info->y_pixel_size);
        XmTextSetString(q->y_pixel_size_tb,temp);
    }

    /**********************************************************************/
    /** If valid, fill in the image_location[0]**/
    /**********************************************************************/
    DEBUG_LIBQSH printf("going to fill the image_location\n");
    if (q->qsh_info->valid.image_location[0]){
        XtVaSetValues(q->il_toggle, XmNset, TRUE, NULL);
        XtVaSetValues(q->ref_toggle,XmNset, FALSE, NULL);
        q->current_qsh_image_referencing = 1;
        /*printf("found the image locations keys\n");*/
        key_status = 1;
    }else{
        XtVaSetValues(q->ref_toggle, XmNset, TRUE, NULL);
        XtVaSetValues(q->il_toggle,XmNset,FALSE,NULL);
        q->current_qsh_image_referencing = 2;

        if (q->qsh_info->valid.reference_location && q->qsh_info->valid.uniform_spacing)
            key_status = 2;
        else
            key_status = 3;
    }

    /**********************************************************************/
    /** the key status is being used to determine what error message **/
    /** to pop up **/
    /** possible key_status values: **/
    /**   1 ->  the image location keys were found in the file **/
    /**   2 ->  the image location keys were not found in the file, but **/
    /**             the reference location and uniform spacing keys were **/
    /**             found so the image location key were calculated from **/
    /**             them. **/
    /**   3 ->  None of the image location keys, the reference location, **/
    /**             nor the uniform spacing keys were found and there  **/
    /**             is no way to calculate the image location keys     */
                    /**********************************************************************/
    if (key_status == 2){
        fill_qsh_image_locations_with_ref_and_spacing(q);
        qsh_error_popup(q->qsh_toplevel,q->qsh_app,1,
                        "Didn't find the Image Location keys,\n   -> Calculated them from the reference location\n     and uniform spacing");
    }else if (key_status == 3){
        qsh_error_popup(q->qsh_toplevel,q->qsh_app,1,
                        "Didn't find the Image Location keys,\n   -> or the reference_location and uniform spacing,\n    you must fill them in");
    }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: print_validity
%%
%%  Written By: Cory Albright 
%%
%%  Parameters: 
%%
%%  Purpose: prints out the values of the validity keys in the
%%           qsh_info_t structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void print_validity(qsh_info_t *info)
{
  int i;

  printf("\n\nbytes_per_pixel valid : %d\n",info->valid.bytes_per_pixel);
  printf("byte_order valid : %d\n",info->valid.byte_order);
  printf("pixel_format valid : %d\n",info->valid.pixel_format);
  printf("dimensionality valid : %d\n",info->valid.dimensionality);
  printf("number_of_dimensions valid : %d\n",info->valid.number_of_dimensions);
  printf("size_of_dimension[0] valid : %d\n",info->valid.size_of_dimension[0]);
  printf("size_of_dimension[1] valid : %d\n",info->valid.size_of_dimension[1]);
  printf("size_of_dimension[2] valid : %d\n",info->valid.size_of_dimension[2]);

  printf("x_pixel_size valid : %d\n",info->valid.x_pixel_size);
  printf("y_pixel_size valid : %d\n",info->valid.y_pixel_size);

  printf("modality valid : %d\n",info->valid.modality);
  printf("slice_orientation valid : %d\n",info->valid.slice_orientation);

  if (info->valid.size_of_dimension[0]){
    for (i=0;i<info->size_of_dimension[0];i++){
      printf("image_location[%d] : %d\n",i,info->valid.image_location[i]);

    }
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: print_qsh_structure
%%
%%  Written By: 
%%
%%  Parameters:
%%
%%  Purpose: prints out the values of all the keys in the qsh_info_t
%%           structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void print_qsh_structure(qsh_info_t *qsh_info)
{
  int i;

  DEBUG_LIBQSH printf("Entered print_qsh_structure\n");

  printf("=============================================================\n");
  printf("        The QSH Structure\n");
  printf("=============================================================\n");

  print_validity(qsh_info);
  printf("--------------------\n");

  if (qsh_info->valid.patient_name){
    printf("qsh_info->patient_name  : %s\n",qsh_info->patient_name);
  }
  if (qsh_info->valid.modality){
    printf("qsh_info->modality  : %s\n",qsh_info->modality);
  }
  if (qsh_info->valid.slice_orientation){
    printf("qsh_info->slice_orientation  : %s\n",qsh_info->slice_orientation);
  }
  if (qsh_info->valid.dimensionality){
    printf("qsh_info->dimensionality  : %s\n",qsh_info->dimensionality);
  }
  if (qsh_info->valid.bytes_per_pixel){
    printf("qsh_info->bytes_per_pixel  : %d\n",qsh_info->bytes_per_pixel);
  }
  if (qsh_info->valid.byte_order){
    printf("qsh_info->byte_order  : %s\n",qsh_info->byte_order);
  }
  if (qsh_info->valid.pixel_format){
    printf("qsh_info->pixel_format  : %d\n",qsh_info->pixel_format);
  }
  if (qsh_info->valid.size_of_dimension[0]){
    printf("qsh_info->size_of_dimension[0]  : %d\n",qsh_info->size_of_dimension[0]);
  }
  if (qsh_info->valid.size_of_dimension[1]){
    printf("qsh_info->size_of_dimension[1]  : %d\n",qsh_info->size_of_dimension[1]);
  }
  if (qsh_info->valid.size_of_dimension[2]){
    printf("qsh_info->size_of_dimension[2]  : %d\n",qsh_info->size_of_dimension[2]);
  }
  if (qsh_info->valid.x_pixel_size){
    printf("qsh_info->x_pixel_size  : %f\n",qsh_info->x_pixel_size);
  }
  if (qsh_info->valid.y_pixel_size){
    printf("qsh_info->y_pixel_size  : %f\n",qsh_info->y_pixel_size);
  }
  
  if (qsh_info->valid.size_of_dimension[0]){
    for (i=0;i<qsh_info->size_of_dimension[0];i++){
      if (qsh_info->valid.image_location[i])
	printf("image_location[%d] : %f\n",i,qsh_info->image_location[i]);
    }
  }

  printf("=============================================================\n\n\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure:  copy_qsh_structure
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: copys the values in the qsh_info_t structure source into the
%%           qsh_info_t structure dest
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void copy_qsh_structure(qsh_info_t *dest,qsh_info_t *source)
{
  int i;

   DEBUG_LIBQSH printf("Entered copy_qsh_structure\n");

  /**********************************************************************/
  /** initialize the validity keys of the destination structure**/
  /**********************************************************************/
  init_qsh_info_validity(dest);
  /*printf("done with init_qsh_info_validity\n");*/
  /**********************************************************************/
  /** copy over the patient name**/
  /**********************************************************************/
  if (source->valid.patient_name){
    dest->valid.patient_name = 1;
    strcpy(dest->patient_name,source->patient_name);
  }

  /**********************************************************************/
  /** copy over the modality**/
  /**********************************************************************/
  if (source->valid.modality){
    dest->valid.modality = 1;
    strcpy(dest->modality,source->modality);
  }
  /*printf("copied the modality\n");*/
  /**********************************************************************/
  /** copy over the slice orientation**/
  /**********************************************************************/
  if (source->valid.slice_orientation){
    dest->valid.slice_orientation = 1;
    strcpy(dest->slice_orientation,source->slice_orientation);
  }

  /**********************************************************************/
  /** copy over the dimensionality**/
  /**********************************************************************/
  if (source->valid.dimensionality){
    dest->valid.dimensionality = 1;
    strcpy(dest->dimensionality,source->dimensionality);
  }

  /**********************************************************************/
  /** copy over the bytes per pixel**/
  /**********************************************************************/
  if (source->valid.bytes_per_pixel){
    dest->valid.bytes_per_pixel = 1;
    dest->bytes_per_pixel = source->bytes_per_pixel;
  }

  /**********************************************************************/
  /** copy over the byte order**/
  /**********************************************************************/
  if (source->valid.byte_order){
    dest->valid.byte_order = 1;
    strcpy(dest->byte_order,source->byte_order);
  }

  /**********************************************************************/
  /** copy over the pixel format**/
  /**********************************************************************/
  if (source->valid.pixel_format){
    dest->valid.pixel_format = 1;
    dest->pixel_format = source->pixel_format;
  }

  /**********************************************************************/
  /** copy over the number of dimensions**/
  /**********************************************************************/
  if (source->valid.number_of_dimensions){
    dest->valid.number_of_dimensions = 1;
    dest->number_of_dimensions = source->number_of_dimensions;
  }

  /**********************************************************************/
  /** copy over the size of dimension[0]**/
  /**********************************************************************/
  if (source->valid.size_of_dimension[0]){
    dest->valid.size_of_dimension[0] = 1;
    dest->size_of_dimension[0] = source->size_of_dimension[0];
  }

  /**********************************************************************/
  /** copy over the size of dimension[1]**/
  /**********************************************************************/
  if (source->valid.size_of_dimension[1]){
    dest->valid.size_of_dimension[1] = 1;
    dest->size_of_dimension[1] = source->size_of_dimension[1];
  }

  /**********************************************************************/
  /** copy over the size of dimension[2]**/
  /**********************************************************************/
  if (source->valid.size_of_dimension[2]){
    dest->valid.size_of_dimension[2] = 1;
    dest->size_of_dimension[2] = source->size_of_dimension[2];
  }

  /**********************************************************************/
  /** copy over the reference location**/
  /**********************************************************************/
  if (source->valid.reference_location){
    dest->valid.reference_location = 1;
    dest->reference_location = source->reference_location;
  }

  /**********************************************************************/
  /** copy over the uniform spacing**/
  /**********************************************************************/
  if (source->valid.uniform_spacing){
    dest->valid.uniform_spacing = 1;
    dest->uniform_spacing = source->uniform_spacing;
  }

  /**********************************************************************/
  /** copy over the x pixel size**/
  /**********************************************************************/
  if (source->valid.x_pixel_size){
    dest->valid.x_pixel_size = 1;
    dest->x_pixel_size = source->x_pixel_size;
  }

  /**********************************************************************/
  /** copy over the y pixel size**/
  /**********************************************************************/
  if (source->valid.y_pixel_size){
    dest->valid.y_pixel_size = 1;
    dest->y_pixel_size = source->y_pixel_size;
  }
  /*printf("done copying a bunch\n");*/
  /**********************************************************************/
  /** make room, then copy over the images**/
  /**********************************************************************/
  if (source->valid.images){
    dest->valid.images = 1;
    dest->images = (unsigned char *)MT_malloc(dest->size_of_dimension[0] *
					   dest->size_of_dimension[1] *
					   dest->size_of_dimension[2]);
    memcpy(dest->images,source->images,
      dest->size_of_dimension[0] * dest->size_of_dimension[1] *dest->size_of_dimension[2]);
  }
  
  /**********************************************************************/
  /** copy over the size of image location keys (if valid)**/
  /**********************************************************************/
  if (source->valid.size_of_dimension[0]){
    for (i = 0;i<dest->size_of_dimension[0];i++){
      if (source->valid.image_location[i]){
	dest->valid.image_location[i] = 1;
	dest->image_location[i] = source->image_location[i];
      }
    }
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: string_to_lower
%%
%%  Written By: Cory Albright
%%
%%  Parameters: 
%%
%%  Purpose: converts all characters in a string to lower case
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
char * string_to_lower(char *string)
{
  int i;

  for (i=0;i<strlen(string);i++){
    if (string[i]>64 && string[i]<91)
      string[i] += 32;
  }
  return string;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: string_to_upper
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: converts all characters in a string to upper case
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
char * string_to_upper(char *string)
{
  int i;

  for (i=0;i<strlen(string);i++){
    if (string[i]>96 && string[i]<123)
      string[i] -= 32;
  }
  return string;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Bytes_Per_Pixel_ChangedCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: if the Bytes Per Pixel Menu gets set to something other than 1,
%%           we need to know the byte order (big endian, little endian), thus,
%%           the byte_order menu gets managed.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Bytes_Per_Pixel_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  qsh_gui_t *q = (qsh_gui_t *)clientdata;

  DEBUG_LIBQSH printf("Entered Bytes_Per_Pixel_Changed\n");

  /**********************************************************************/
  /** if the bytes per pixel menu is set to 2, we need to know**/
  /** what the byte order is so we need to manage the byte_oder_menu **/
  /** Also, if it was set to 2 and is now set to something else, we **/ 
  /** need to remove the byte_oder_menu **/
  /**********************************************************************/
  if ( (strcmp(XtName(w),"2") == 0) && !(XtIsManaged(q->byte_options_outer_frame))){
    /*XtManageChild(q->byte_order_label);
      XtManageChild(q->byte_order_menu);*/
    XtManageChild(q->byte_options_outer_frame);
  }else if (XtIsManaged(q->byte_options_outer_frame)){
    /*XtUnmanageChild(q->byte_order_label);
      XtUnmanageChild(q->byte_order_menu);*/
    XtUnmanageChild(q->byte_options_outer_frame);
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Show_Image_LocationsCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: brings up the Image_Locations Dialog (builds it first time),
%%           and fills in the image locations from the qsh_info_t structure
%%           passed through clientdata
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Show_Image_LocationsCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
    qsh_gui_t *q;

    DEBUG_LIBQSH printf("Entered Show_Image_LocationsCB\n");

    q = (qsh_gui_t *)clientdata;

    /**********************************************************************/
    /** Bring up the Image Locations dialog    **/
    /** fill the key locations into the dialog **/
    /**********************************************************************/

    fill_image_location_popup_with_locations(q);
    XtManageChild(q->il.form /*,XtGrabNone*/);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: build_image_locations_dialog
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: builds and returns the Image Locations Dialog
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_image_locations_dialog(qsh_gui_t *q)
{
  char temp_string[256];
  char dim[5];
  int i;
  XmString xmstr;

  DEBUG_LIBQSH printf("Entered build_image_locations_dialog\n");
    
  /**********************************************************************/
  /** build the popup shell for the image location popup **/
  /**********************************************************************/
  q->il.shell= XmCreateDialogShell(q->qsh_toplevel, "Image Locations",NULL,0);
    /*XtCreatePopupShell("Image Locations",
      topLevelShellWidgetClass, q->qsh_toplevel, 
      NULL,0);*/

  XtVaSetValues(q->il.shell,
		XmNwidth, 500,
		XmNheight,300,
		XmNtitle, "Image Locations",
                XmNdeleteResponse, XmUNMAP, /* If close button is used, just unmanage */
		NULL);

  q->il.form = XtVaCreateWidget("il_form",
				xmFormWidgetClass,q->il.shell,
				XmNwidth, 500,
				XmNheight, 300,
				NULL);
  q->il.buttonrc = XtVaCreateManagedWidget("il_buttonrc",
					  xmRowColumnWidgetClass,q->il.form,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNrightAttachment, XmATTACH_FORM,
					  XmNbottomAttachment, XmATTACH_FORM,
					  XmNtopAttachment, XmATTACH_NONE,
					  XmNheight,50,
					  XmNorientation, XmHORIZONTAL,
					  NULL);

  /**********************************************************************/
  /** add the key manipulation buttons**/
  /**********************************************************************/
  q->il.add_button = XtVaCreateManagedWidget("Add",
				       xmPushButtonWidgetClass, q->il.buttonrc,
				       NULL);

  q->il.delete_button = XtVaCreateManagedWidget("Delete",
					  xmPushButtonWidgetClass, q->il.buttonrc,
					  NULL);

  q->il.fill_keys_button = XtVaCreateManagedWidget("Revert from file",
					  xmPushButtonWidgetClass, q->il.buttonrc,
					  NULL);
  XtAddCallback(q->il.fill_keys_button,XmNactivateCallback,
		Revert_keysCB,(XtPointer)q);
  q->il.reverse_button = XtVaCreateManagedWidget("Reverse",
					  xmPushButtonWidgetClass, q->il.buttonrc,
					  NULL);
  XtAddCallback(q->il.reverse_button,XmNactivateCallback,
		Reverse_keysCB,(XtPointer)q);

  
  q->il.apply_button = XtVaCreateManagedWidget("Apply",
					   xmPushButtonWidgetClass, q->il.buttonrc,
					   NULL);

  XtAddCallback(q->il.apply_button,XmNactivateCallback,
		Apply_Image_Locations_Dialog,(XtPointer)q);
  
  q->il.dismiss_button = XtVaCreateManagedWidget("Dismiss",
					   xmPushButtonWidgetClass, q->il.buttonrc,
					   NULL);
  
  XtAddCallback(q->il.dismiss_button,XmNactivateCallback,
		Dismiss_Image_Locations_Dialog,(XtPointer)q);
  
  
  q->il.sw = XtVaCreateManagedWidget("il_sw",
				  xmScrolledWindowWidgetClass,q->il.form,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNrightAttachment, XmATTACH_FORM,
				  XmNleftAttachment, XmATTACH_FORM,
				  XmNbottomAttachment, XmATTACH_WIDGET,
				  XmNbottomWidget, q->il.buttonrc,
				  XmNheight, 100,
				  XmNscrollingPolicy, XmAUTOMATIC,
				  XmNvisualPolicy, XmVARIABLE,
				  NULL);
  
  q->il.sw_rc = XtVaCreateManagedWidget("il_sw_rc",
				     xmRowColumnWidgetClass,q->il.sw,
				     XmNorientation, XmHORIZONTAL,
				     XmNnumColumns, q->qsh_info->size_of_dimension[0],
				     XmNpacking, XmPACK_COLUMN,
				     NULL);

  XtAddCallback(q->il.add_button,XmNactivateCallback,
		Add_Image_LocationCB,(XtPointer)q);
  XtAddCallback(q->il.delete_button,XmNactivateCallback,
		Remove_Image_LocationCB,(XtPointer)q);

  /**********************************************************************/
  /** build the labels for each of the image location keys **/
  /**  include the dimensionality in the label **/
  /**********************************************************************/
  for (i=0;i<q->qsh_info->size_of_dimension[0];i++){
    /*
    if (q->qsh_info->valid.dimensionality){
      if (strstr(q->qsh_info->dimensionality,"cm"))
	sprintf(dim,"cm");
      else if (strstr(q->qsh_info->dimensionality,"mm"))
	sprintf(dim,"mm");
 
      sprintf(temp_string,"Image_Location[%d] (%s)",i,dim);
    }
    else
    */
    sprintf(temp_string,"Image_Location[%d]",i);
    
    q->il.label[i] = XtVaCreateManagedWidget(temp_string,
					     xmLabelWidgetClass, q->il.sw_rc,
					     NULL);
    q->il.il[i] = XtVaCreateManagedWidget("tb",
					  xmTextFieldWidgetClass, q->il.sw_rc,
					  NULL);
    }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Dismiss_Image_Locations_Dialog
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: gets the keys out of the dialog, error checks them, then 
%%           pops down (unmanages) the Image Locations Dialog
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Apply_Image_Locations_Dialog(Widget w, XtPointer clientdata, XtPointer calldata)
{
    int values_ok,i;
    int num_errors;
    qsh_gui_t *q;
    q = (qsh_gui_t *)clientdata;

    DEBUG_LIBQSH printf("Entered Apply_Image_Locations_Dialog\n");
  
    /**********************************************************************/
    /** before we close the image location popup copy the keys **/
    /** out of the popup into the qsh structure **/ 
    /**********************************************************************/
    for (i=0;i<q->qsh_info->size_of_dimension[0];i++){
        q->qsh_info->image_location[i] = atof((char *)XmTextGetString(q->il.il[i]));
    }
  
    /**********************************************************************/
    /** now check the user editted values for errors and report them **/
    /**********************************************************************/  
    if( error_check_qhd(q,QSH_ERROR_CHECK_JUST_WARN) )
    {
        /**********************************************************************/
        /** we're done, popdown the image locations dialog**/
        /**********************************************************************/
        XtUnmanageChild(q->il.form);
    }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Dismiss_Image_Locations_Dialog
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: gets the keys out of the dialog, error checks them, then 
%%           pops down (unmanages) the Image Locations Dialog
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Dismiss_Image_Locations_Dialog(Widget w, XtPointer clientdata, XtPointer calldata)
{
    int values_ok,i;
    int num_errors;
    qsh_gui_t *q;
    q = (qsh_gui_t *)clientdata;

    DEBUG_LIBQSH printf("Entered Dismiss_Image_Locations_Dialog\n");
  
    /**********************************************************************/
    /** we're done, popdown the image locations dialog**/
    /**********************************************************************/
    XtUnmanageChild(q->il.form);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Revert_keysCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: calls fill_image_locations_popup_with_locations
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Revert_keysCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  char temp_string[25];
  XmString xmstr;
  int i;
  int current_size,backup_size;

  qsh_gui_t *q = (qsh_gui_t *)clientdata;

  DEBUG_LIBQSH printf("Entered Revert_keysCB\n");

  current_size = q->qsh_info->size_of_dimension[0];
  backup_size = q->backup_qsh.size_of_dimension[0];

  if (current_size < backup_size){
    for (i=current_size; i<backup_size ;i++)
      Add_Image_LocationCB(w,clientdata,calldata);
  }else if (current_size > backup_size){
    for (i=backup_size;i<current_size;i++)
      Remove_Image_LocationCB(w,clientdata,calldata);
  }
  
  if(q->qsh_info->size_of_dimension[0] != q->backup_qsh.size_of_dimension[0])
    printf("WARNING, For some reason the reverting from file did not reset the number of keys correctly\n");
  /*q->qsh_info->size_of_dimension[0] = q->backup_qsh->size_of_dimension[0]);*/

  sprintf(temp_string,"%d",q->qsh_info->size_of_dimension[0]);
  xmstr = XmStringCreateLocalized(temp_string);
  XtVaSetValues(q->number_of_slices_label,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);

  /**********************************************************************/
  /** reverting the keys simply copies them out of the structure and**/
  /** fills them into the image location popup **/
  /**********************************************************************/
  fill_qsh_image_locations_from_backup(q);
  fill_image_location_popup_with_locations(q);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: fill_image_location_popup_with_locations
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: fills in the image_locations with the image locations
%%           in the qsh_info_t structure;
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int fill_image_location_popup_with_locations(qsh_gui_t *q)
{
  int i;
  char temp_string[256];
  int num_errors = 0;
  char error_string[256*256];
  float ref_value,spacing;

  DEBUG_LIBQSH printf("Entered fill_image_location_popup_with_locations\n");

  /**********************************************************************/
  /** if the current qsh image referencing is 2 (ref and spacing)**/
  /** we simply fill the image location keys & popup with the **/
  /** calculated keys **/
  /**********************************************************************/
  if (q->current_qsh_image_referencing == 2){
    if (!fill_qsh_image_locations_with_ref_and_spacing(q)){
      qsh_error_popup(q->qsh_toplevel,q->qsh_app,1,
		      "There was a Problem filling the\nimage locations with the \nreference and uniform spacing\n");
      return 0;
    }
  }

  /**********************************************************************/
  /** check to make sure we know how many images there are**/
  /**********************************************************************/
  if (!q->qsh_info->valid.size_of_dimension[0]){
    qsh_error_popup(q->qsh_toplevel,q->qsh_app,1,"Sorry, don't  know how many slices there are\n");
    return 0;
  }
  
  sprintf(error_string,"Problems:\n");
  
  /**********************************************************************/
  /** loop through the keys and fill its value into the popup , if **/
  /** the key is not valid, add it to the warning string which **/
  /** will get displayed at the end of the procedure **/
  /**********************************************************************/
  for (i=0;i<q->qsh_info->size_of_dimension[0];i++){
    if (!q->qsh_info->valid.image_location[i]){
      sprintf(temp_string,"WARNING:  an image location[%d] appears to me missing\n",i);
      strcat(error_string,temp_string);
      num_errors++;
    }else{
      sprintf(temp_string,"%f",q->qsh_info->image_location[i]);
      XmTextSetString(q->il.il[i],temp_string);
    }
  }
  /**********************************************************************/
  /** if there were errors, report them**/
  /**********************************************************************/
  if (num_errors >0)
    qsh_error_popup(q->qsh_toplevel,q->qsh_app,1,error_string);
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Add_Image_LocationsCB
%%
%%  Written By: Cory Albright 
%%
%%  Parameters:
%%
%%  Purpose: Adds another image location box to the dialog and 
%%           increases the size of dimension [0] by 1
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Add_Image_LocationCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  char temp_string[256];
  XmString xmstr;
  qsh_gui_t *q;
  q = (qsh_gui_t *)clientdata;

  DEBUG_LIBQSH printf("Entered Add_Image_LocationCB\n");

  /**********************************************************************/
  /** check to make sure that we will not exceed the maximum allowable**/
  /** slices **/
  /**********************************************************************/
  if (q->qsh_info->size_of_dimension[0]+1 >= MAX_QSH_SLICES){
    printf("MAX_QSH_SLICES reached, cannot add another slice\n");
    return;
  }

  /**********************************************************************/
  /** make the two new widgets: the label and textbox for the **/
  /** additional image location key **/
  /**********************************************************************/
  sprintf(temp_string,"Image Location[%d]",q->qsh_info->size_of_dimension[0]);
  XtVaSetValues(q->il.sw_rc,XmNnumColumns,q->qsh_info->size_of_dimension[0]+1,NULL);
  q->il.label[q->qsh_info->size_of_dimension[0]] = XtVaCreateManagedWidget(temp_string,
			  xmLabelWidgetClass, q->il.sw_rc,
			  NULL);  
  q->il.il[q->qsh_info->size_of_dimension[0]] = XtVaCreateManagedWidget(
						"tb",
						xmTextFieldWidgetClass, q->il.sw_rc,
						NULL);

  /**********************************************************************/
  /** increment the number of slices **/
  /**********************************************************************/
  q->qsh_info->size_of_dimension[0]++;
  
  sprintf(temp_string,"%d",q->qsh_info->size_of_dimension[0]);
  xmstr = XmStringCreateLocalized(temp_string);
  XtVaSetValues(q->number_of_slices_label,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Remove_Image_LocationCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: removes an image_location box from the image location dialog
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Remove_Image_LocationCB(Widget w, XtPointer clientdata, XtPointer calldata)
{  
  char temp_string[25];
  XmString xmstr;
  qsh_gui_t *q;
  q = (qsh_gui_t *)clientdata;

  DEBUG_LIBQSH printf("Entered Remove_Image_LocationCB\n");

  /**********************************************************************/
  /** check to make sure that we are not going negative **/
  /** with the number of image locations **/
  /**********************************************************************/
  if (q->qsh_info->size_of_dimension[0] - 1 < 0){
    printf("None left to delete!\n");
    return;
  }

  /**********************************************************************/
  /** decrement the number of slices and remove the widgets **/
  /**********************************************************************/  
  q->qsh_info->size_of_dimension[0]--;
  XtVaSetValues(q->il.sw_rc,XmNnumColumns,q->qsh_info->size_of_dimension[0],NULL);
  XtDestroyWidget(q->il.label[q->qsh_info->size_of_dimension[0]]);
  XtDestroyWidget(q->il.il[q->qsh_info->size_of_dimension[0]]);

  sprintf(temp_string,"%d",q->qsh_info->size_of_dimension[0]);
  xmstr = XmStringCreateLocalized(temp_string);
  XtVaSetValues(q->number_of_slices_label,XmNlabelString,xmstr,NULL);
  XmStringFree(xmstr);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: error_check_qhd
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%     NOTE:  the paramater error_checking_option tells the popup 
%%            whether it should just warn the user or whether it 
%%            should give the user the option to fix or ignore the 
%%            errors.  
%%            The current options (sent into this procedure) are 
%%            QSH_ERROR_CHECK_JUST_WARN -          
%%                    will just give the user an OK button on 
%%                    the error_popup 
%%            QSH_ERROR_CHECK_FULL -              
%%                    will give the user both a FIX button and 
%%                    an IGNORE button. 
%%
%%  Purpose: checks throught the values in the qsh_info_t structure
%%           and reports any errors or warnings.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int error_check_qhd(qsh_gui_t *q, int error_checking_option)
{
  int i;
  int error_num;
  int num_errors = 0;
  char error_string[65536];
  float internal_spacing;
  float uniform_spacing,ref_location;
  int want_reversal = 0;
  char temp_string[256];
  float tolerance;
  int keys_are_not_reversed = 1;
  char current_dimensionality[10];
  int num_keys_less_than_1 = 0;
  float range_of_keys = 0.0;
  float x_pixel_size,y_pixel_size;
  Widget temp_widget;
  int return_status;
  int num_images;
  char * location_ptr;
  char * spacing_ptr;

  DEBUG_LIBQSH printf("Entered error_check_qhd\n");

  sprintf(error_string,"QHD Errors:\n");

  /**********************************************************************/
  /** make sure we know how many slices there are**/
  /**********************************************************************/
  if (!q->qsh_info->size_of_dimension[0]){
    qsh_error_popup(q->qsh_toplevel,q->qsh_app,error_checking_option,
		    (char *)"Number of Slices unkown, cannot procede without it\n");
    return 0;
  }

  /*printf("past the first\n");*/
  /**********************************************************************/
  /** Do some 'smart' error checking, look at the values intered **/
  /** and see if they 'seem' to match the dimensionality key **/
  /**********************************************************************/
  XtVaGetValues(q->dimensionality_menu,XmNmenuHistory,&temp_widget,NULL);
  strcpy(current_dimensionality,XtName(temp_widget));

  /*printf("got the dimensionality\n"); */
 
  x_pixel_size = atof((char *)XmTextGetString(q->x_pixel_size_tb));
  y_pixel_size = atof((char *)XmTextGetString(q->y_pixel_size_tb));

  spacing_ptr  = XmTextGetString(q->uniform_spacing_tb);
  location_ptr = XmTextGetString(q->reference_location_tb);
  uniform_spacing = atof( spacing_ptr ); 
  ref_location    = atof( location_ptr );
  
  range_of_keys = fabs((float)q->qsh_info->image_location[0] - 
		       (float)q->qsh_info->image_location[q->qsh_info->size_of_dimension[0]-1]);
  num_images = q->qsh_info->size_of_dimension[0];
  /*printf("past the second\n");*/






  /**********************************************************************/
  /** if the dimensionality key is in mm, look for any keys that **/
  /** seem to be in cm **/
  /**********************************************************************/

  /*** NOTE: all mm/cm checks have been removed by CLA 2-25-99, they were annoying everyone ***/

  if (strcmp(current_dimensionality,"mm") == 0){
    /**********************************************************************/
    /** check the x pixel size **/
    /**********************************************************************/
    /*
    if (x_pixel_size < .5 && x_pixel_size != 0.0){
      sprintf(temp_string,"\tWarning: the x_pixel_size appears to be in cm, not mm\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    */

    /**********************************************************************/
    /** check the y pixel size **/
    /**********************************************************************/
    /*
    if (y_pixel_size < .5 && y_pixel_size != 0.0){
      sprintf(temp_string,"\tWarning: the y_pixel_size appears to be in cm, not mm\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    */

    /**********************************************************************/
    /** check the uniform_spacing **/
    /**********************************************************************/
    /*
    if (uniform_spacing < .7 && uniform_spacing != 0.0){
      sprintf(temp_string,"\tWarning: the uniform thickness appears to be in cm, not mm\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    */
    
    /**********************************************************************/
    /** check the range of the keys **/
    /**********************************************************************/
    /*
    if (range_of_keys < (float)num_images && range_of_keys != 0.0){
      sprintf(temp_string,"\tWarning: the image location keys appears to be in cm, not mm\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    */

    /*
    for (i=0;i<q->qsh_info->size_of_dimension[0];i++){
      if (q->qsh_info->valid.image_location[i]){
	if ((q->qsh_info->image_location[i] < 1.0 && q->qsh_info->image_location[i] > -1.0) &&
	    q->qsh_info->image_location[i] != 0.0){
	  num_keys_less_than_1++;
	}
      }    
    }	
    if (num_keys_less_than_1 > 1){
      sprintf(temp_string,"\tWarning: the image_locations appear to be in cm, not mm\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    */
    
  }else if (strcmp(current_dimensionality,"cm") == 0){
    /**********************************************************************/
    /** check the x pixel size **/
    /**********************************************************************/
    /*
    if (x_pixel_size > .5 && x_pixel_size != 0.0){
      sprintf(temp_string,"\tWarning: the x_pixel_size appears to be in mm, not cm\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    */

    /**********************************************************************/
    /** check the y pixel size **/
    /**********************************************************************/
    /*
    if (y_pixel_size > .5 && y_pixel_size != 0.0){
      sprintf(temp_string,"\tWarning: the y_pixel_size appears to be in mm, not cm\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    */

    /**********************************************************************/
    /** check the uniform_spacing **/
    /**********************************************************************/
    /*
    if (uniform_spacing > .7 && uniform_spacing != 0.0){
      sprintf(temp_string,"\tWarning: the uniform spacing appears to be in mm, not cm\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    */

    /**********************************************************************/
    /** check the uniform_spacing **/
    /**********************************************************************/
    /*
    if ((ref_location < -50.0  || ref_location > 50.0) && ref_location != 0.0){
      sprintf(temp_string,"\tWarning: the x_pixel_size appears to be in mm, not cm\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    */
    
    /**********************************************************************/
    /** check the range of the keys **/
    /**********************************************************************/
    /*
    if (range_of_keys > (float)num_images){
      sprintf(temp_string,"\tWarning: the image location keys appears to be in mm, not cm\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    */


    /*
    for (i=0;i<q->qsh_info->size_of_dimension[0];i++){
      if (q->qsh_info->valid.image_location[i]){
	if (q->qsh_info->image_location[i] > 50.0 || q->qsh_info->image_location[i] < -50.0){
	  sprintf(temp_string,"\tWarning: the image_locations appear to be in mm, not cm\n");
	  strcat(error_string,temp_string);
	  num_errors++;
	  break;
	}
      }    
    }
    */
  }
  
  /**********************************************************************/
  /** if the current_qsh_image_referencing is 2 (using ref and spacing),**/
  /** we need to check to see that the reference location and **/
  /** uniform spacing keys are filled in **/
  /**********************************************************************/
  if (q->current_qsh_image_referencing == 2){
    if ( strlen( location_ptr ) == 0 ){
      sprintf(temp_string,"\tERROR: the reference_location key must be filled\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
    if ( strlen( spacing_ptr ) == 0 || fabs( uniform_spacing - 0.0 ) < 0.001 ){
      sprintf(temp_string,"\tERROR: the uniform spacing key must be filled\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
  }

  /**********************************************************************/
  /** check to see if there are any missing image locaiton keys**/
  /**********************************************************************/
  for (i=0;i<q->qsh_info->size_of_dimension[0];i++){
    if (!q->qsh_info->valid.image_location[i]){
      sprintf(temp_string,"\tWarning: Image Location[%d] missing\n",i);
      strcat(error_string,temp_string);
	num_errors++;    	
    }
  }

  /**********************************************************************/
  /** check to make sure the ref_location is the same as the first **/
  /** image location key.  **/
  /** if its not, check to see if it is the same as the last image **/
  /** location key, which would could mean the keys are reversed **/
  /**********************************************************************/
  if (ref_location != q->qsh_info->image_location[0] && ref_location != 0.0){
    if (ref_location == q->qsh_info->image_location[q->qsh_info->size_of_dimension[0]-1]){
      
      sprintf(temp_string,"\tWarning: It looks as if the image locations are reversed,\n    -> the reference_location equals the last image_location \n");      
      strcat(error_string,temp_string);
      num_errors++;
      
      /*
      want_reversal = qsh_confirm(q->qsh_toplevel,q->qsh_app,
   "It appears that the image locations are reversed\n\nWould you like me to reverse them?");      

      if (want_reversal){ 
	reverse_image_location_keys(q);
      }
      */
    }else{
      /**********************************************************************/
      /** this means that the reference location key does not match either**/
      /** the first or last image location key **/
      /**********************************************************************/
      sprintf(temp_string,"\tWarning: The first image location does not\nmatch the reference location\n");
      strcat(error_string,temp_string);
      num_errors++;
    }
  }

  /**********************************************************************/
  /** get the tolerance value out of the textbox and **/
  /** verify that the spacing of the image location keys **/
  /** is within the tolerance level **/
  /**********************************************************************/
  strcpy(temp_string,(char *)XmTextGetString(q->tolerance_tb));
  sscanf(temp_string,"%f",&tolerance);
  error_num = verify_ils_internally(q->qsh_info->image_location,
				    q->qsh_info->size_of_dimension[0],
				    &internal_spacing,
				    tolerance);

  /*printf("verify_ils_internally just returned : %d\n",error_num);*/
  /*printf("checked the internal spacing, found : %f\n",internal_spacing);*/

  /**********************************************************************/
  /** make sure that the internal spacing of the image location keys**/
  /** matches the uniform spacing key **/
  /**********************************************************************/
  if (uniform_spacing != internal_spacing && uniform_spacing != 0){
    sprintf(temp_string,
     "\tWarning: the Internal Image Spacing (%f) does not seem to match \n\t\tthe uniform spacing (%f)\n",
	    internal_spacing, uniform_spacing);
    strcat(error_string,temp_string);
  }

  /**********************************************************************/
  /** error_num is returned by verify_ils_internally, if **/
  /** it is not -1, the value it returns is the slice # that is **/
  /** not spaced correctly, so report that here **/
  /**********************************************************************/
  if (error_num != -1){
    sprintf(temp_string,"\tWarning: Image Location[%d] not spaced correctly\n",error_num);
    strcat(error_string,temp_string);
    num_errors++;   
  }

  /**********************************************************************/
  /** if there are errors, popup them up now **/
  /**********************************************************************/
  /*printf("near the end of error_check_qhd, calling qsh_error_popup\n");*/
  if (num_errors > 0){
    return_status = qsh_error_popup(q->qsh_toplevel,q->qsh_app,error_checking_option,error_string);
    /*printf("done with qsh_error_popup\n");*/
    return return_status;
  }

  return 1;
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: reverse_image_location_keys
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: reverses the keys in the structure and the popup
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void reverse_image_location_keys(qsh_gui_t *q)
{
  float temp[MAX_QSH_SLICES];
  int i,num_keys;
  char temp_string[256];

  DEBUG_LIBQSH printf("Entered reverse_image_location_keys\n");

  num_keys = q->qsh_info->size_of_dimension[0];

  /**********************************************************************/
  /** copy the keys backwards into the temp array**/
  /**********************************************************************/
  for (i=0;i<num_keys;i++){
    temp[i] = q->qsh_info->image_location[num_keys - i -1];
  }
  /**********************************************************************/
  /** now copy them back into the qsh structure and fill them into**/
  /** the image location popup **/
  /**********************************************************************/
  for (i=0;i<num_keys;i++){
    q->qsh_info->image_location[i] = temp[i];
    sprintf(temp_string,"%f",temp[i]);
    XmTextSetString(q->il.il[i],temp_string);
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Reverse_keysCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: reverses the keys in the structure and the popup
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Reverse_keysCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  qsh_gui_t *q = (qsh_gui_t *)clientdata;

  DEBUG_LIBQSH printf("Entered Reverse_keysCB\n");

  /**********************************************************************/
  /** just call reverse_image_location_keys**/
  /**********************************************************************/
  reverse_image_location_keys(q);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: verify_ils_with_reference_and_spacing
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: looks at the image locations in the qsh_info_t structure
%%           and sees if they match up with the reference_location and
%%           uniform spacing keys
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
int verify_ils_with_reference_and_spacing(float *ils,int num_ils,
				      float ref_loc, float uniform_spacing)
{
  int i;
  float ep,expected;
  float tolerance;


  sscanf((char *)XmTextGetString(tolerance_tb),"%f",&tolerance);
  tolerance /= 100.0;

  ep = tolerance * uniform_spacing;

  for (i=0;i<num_ils;i++){
    expected = ref_loc + i*uniform_spacing;

    if (ils[i] < expected-ep) return i;
    if (ils[i] > expected+ep) return i;
  }

  if (ils[0] != ref_loc) return -2;
  
  return -1;
} 
*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: verify_ils_internally
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: looks at the image location keys in the qsh_info_t structure
%%           and makes sure that they are spaced correctly and consistently
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int verify_ils_internally(float *ils,int num_ils, float *internal_spacing,float tolerance)
{
  int i;
  float ep,expected;
  float spacing;

  DEBUG_LIBQSH printf("Entered verify_ils_internally\n");

  /**********************************************************************/
  /** get the spacing from the first two image locations **/
  /** NOTE: this is probably not the best way to get the spacing **/
  /**       because it will be wrong if one of the first to keys **/
  /**       is not spaced correctly **/
  /**********************************************************************/
  spacing = ils[1] - ils[0];
  
  /**********************************************************************/
  /** return the internal spacing**/
  /**********************************************************************/
  *internal_spacing = spacing;

  /**********************************************************************/
  /** check the image locations for tolerance% of the spacing **/
  /**********************************************************************/
  tolerance /= 100.0;

  /**********************************************************************/
  /** determine the epsilon (deviation value) **/
  /**********************************************************************/
  ep = tolerance * spacing;

  /**********************************************************************/
  /** now loop through the keys and compare to what they would **/
  /**  be if they were calculated with the spacing **/
  /**********************************************************************/
  for (i=0;i<num_ils;i++){
    expected = ils[0] + i*spacing;
    
    /**********************************************************************/
    /** check to see if the key is within epsilon from the expected**/
    /**********************************************************************/
    if (ep > 0.0){ 
      if (ils[i] < expected-ep) return i;
      if (ils[i] > expected+ep) return i;
    }else{
      if (ils[i] > expected-ep) return i;
      if (ils[i] < expected+ep) return i;
    }
  }
  
  /**********************************************************************/
  /** all the keys were ok return -1**/
  /**********************************************************************/
  return -1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Image_Referencing_ToggledCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: toggles between using the image_location keys and using the 
%%           reference location and spacing.
%%
%%           NOTE:  when toggled either way, the keys are computed and filled
%%                  into the qsh_info_t structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Image_Referencing_ToggledCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)calldata;
  Boolean il,ref;
  qsh_gui_t *q = (qsh_gui_t *)clientdata;
  int i;

  DEBUG_LIBQSH printf("Entered Image_Referencing_ToggledCB\n");

  XtVaGetValues(q->il_toggle,XmNset,&il,NULL);
  XtVaGetValues(q->ref_toggle,XmNset,&ref,NULL);
  


  if (w == q->il_toggle && cbs->set){
    /**********************************************************************/
    /** the user just tried to toggle on the "use image location **/
    /** keys from the file" button, but before we allow them **/
    /** we have to make sure there were image location keys in the file **/
    /**********************************************************************/
    if (!fill_qsh_image_locations_from_backup(q)){
      /**********************************************************************/
      /** there were no image location keys in the file**/
      /** we cannot allow the user to set the current_image_referencing **/
      /** to use the keys from the file so set the toggle back to **/
      /** the ref_toggle **/
      /**********************************************************************/
      XtVaSetValues(q->il_toggle,XmNset,FALSE,NULL);
    }else{
      /**********************************************************************/
      /** there were image location keys in the file**/
      /** so we can set the current_image_referencing to **/
      /** use the keys from the file, but we need to remember **/
      /** to untoggle the ref_toggle button **/
      /**********************************************************************/
      q->current_qsh_image_referencing = 1;
      XtVaSetValues(q->ref_toggle, XmNset, FALSE, NULL);
    }
  }else if (w == q->il_toggle && !ref){
    /**********************************************************************/
    /** the user just toggled the il_toggle button, but we don't **/
    /** want them to be able to toggle it off so turn it back on **/
    /**********************************************************************/
    XtVaSetValues(q->il_toggle, XmNset, TRUE, NULL);
  }else if (w == q->ref_toggle && cbs->set){
    /**********************************************************************/
    /** the user just toggled  on the "use ref and spacing" button,**/
    /** we need to untoggle the il_toggle button (radio behavior) **/
    /**  and also calculate the image location keys from the ***/
    /** reference and spacing **/
    /**********************************************************************/
    q->current_qsh_image_referencing = 2;
    XtVaSetValues(q->il_toggle, XmNset, FALSE, NULL);
    
    fill_qsh_image_locations_with_ref_and_spacing(q);
  }else if (w == q->ref_toggle && !il){
    /**********************************************************************/
    /** the user tried to toggle off the ref & spacing toggle button **/
    /** but since one of the two has to be toggle, we can't allow them **/
    /** to turn this one off so toggle it on **/
    /**********************************************************************/
    XtVaSetValues(q->ref_toggle, XmNset, TRUE, NULL);
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Revert_QhdCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: fills in the qsh_info_t structure from backup, and 
%%           fills them into the qhd_popup
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Revert_QhdCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  qsh_gui_t *q = (qsh_gui_t *)clientdata;

  DEBUG_LIBQSH printf("Entered Revert_QhdCB\n");

  /**********************************************************************/
  /** to revert the whole qhd structure simply fill it in from **/
  /** the backed up copy **/
  /**********************************************************************/
  copy_qsh_structure(q->qsh_info, &q->backup_qsh);  

  /**********************************************************************/
  /** then refill the qhd_popup with the new values**/
  /**********************************************************************/
  fill_qhd_popup_from_qhd_info(q);  
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: fill_qsh_image_locations_from_backup
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: fills in the qsh_info_t structures image location keys from
%%           backup
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int fill_qsh_image_locations_from_backup(qsh_gui_t *q)
{
  int i;

  DEBUG_LIBQSH printf("Entered fill_qsh_image_locations_from_backup\n");

  /**********************************************************************/
  /** make sure we know how many images are there **/
  /**********************************************************************/
  if (!q->backup_qsh.valid.image_location[0]){
    qsh_error_popup(q->qsh_toplevel, q->qsh_app,1,"Sorry, no image location keys were found \n\tin the qhd file\n");    
    return 0;
  }

  /**********************************************************************/
  /** revert the number of images from the backup **/
  /**********************************************************************/
  q->qsh_info->size_of_dimension[0] = q->backup_qsh.size_of_dimension[0];
  
  /**********************************************************************/
  /** then simply copy the image location keys from the backup**/
  /**********************************************************************/
  for (i=0;i<q->qsh_info->size_of_dimension[0];i++){
    q->qsh_info->valid.image_location[i] = q->backup_qsh.valid.image_location[i];
    if (q->qsh_info->valid.image_location[i])
      q->qsh_info->image_location[i] = q->backup_qsh.image_location[i];
  }

  /**********************************************************************/
  /** fill these into the image location popup**/
  /**********************************************************************/
  fill_image_location_popup_with_locations(q);

  /**********************************************************************/
  /** all done success**/
  /**********************************************************************/
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: fill_qsh_image_locations_with_ref_and_spacing
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: calculates the image keys from the reference_location
%%           and uniform spacing and fills them into the image location
%%           keys in the qsh_info_t
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int fill_qsh_image_locations_with_ref_and_spacing(qsh_gui_t *q)
{
  float ref_value,spacing;
  int i;

  DEBUG_LIBQSH printf("Entered fill_qsh_image_locations_with_ref_and_spacing\n");

  /**********************************************************************/
  /** get the reference and spacing keys out of the popup**/
  /**********************************************************************/
  ref_value = atof((char *)XmTextGetString(q->reference_location_tb));
  spacing = atof((char *)XmTextGetString(q->uniform_spacing_tb));
  
  /**********************************************************************/
  /** calculate the image locations keys and save them in the structure**/
  /**********************************************************************/
  for (i=0;i<q->qsh_info->size_of_dimension[0];i++){
    q->qsh_info->valid.image_location[i] = 1;
    q->qsh_info->image_location[i] = ref_value + spacing*(float)i;
  }  

  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Apply_ref_and_spacingCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Apply_ref_and_spacingCB(Widget w, XtPointer clientdata,XtPointer calldata)
{
  qsh_gui_t *q = (qsh_gui_t *)clientdata;
  float uniform_spacing,ref_location;
  char * spacing_ptr;
  char * location_ptr;
  
  DEBUG_LIBQSH printf("Entered Apply_ref_and_spacing\n");

  /**********************************************************************/
  /** make sure that they are in "ref and spacing" mode **/
  /** (current_qhs_image_referencing == 2)**/
  /**********************************************************************/
  if (q->current_qsh_image_referencing == 1){
    qsh_error_popup(q->qsh_toplevel,q->qsh_app,1,"You must have Ref & Location checked first");
    return;
  }

  /**********************************************************************/
  /** get the uniform spacing and reference location keys from the popup**/
  /**********************************************************************/
  spacing_ptr  = XmTextGetString(q->uniform_spacing_tb);
  location_ptr = XmTextGetString(q->reference_location_tb);

  uniform_spacing = atof( spacing_ptr );
  ref_location = atof( location_ptr );

  /**********************************************************************/
  /** make sure that the uniform spacing and reference location keys  **/
  /** were filled in **/
  /**********************************************************************/
  if ( strlen( location_ptr ) == 0 )
  {
    qsh_error_popup(q->qsh_toplevel,q->qsh_app,1,"ERROR: the reference_location key must be filled\n");
    return;
  }
  XtFree( location_ptr );
  
  if ( strlen( spacing_ptr ) == 0 || fabs( uniform_spacing - 0.0 ) < 0.001 )
  {
      qsh_error_popup(q->qsh_toplevel,q->qsh_app,1,"ERROR: the uniform_spacing key must be filled\n");
      return;
  }
  XtFree( spacing_ptr );

  /*
   * Mark reference location and spacing as valid.
   */
  q->qsh_info->valid.uniform_spacing = 1;
  q->qsh_info->valid.reference_location = 1;
  q->qsh_info->uniform_spacing = uniform_spacing;
  q->qsh_info->reference_location = ref_location;
  
  /**********************************************************************/
  /** calculate the image location keys from the ref and spacing**/
  /**********************************************************************/
  fill_qsh_image_locations_with_ref_and_spacing(q);

  /**********************************************************************/
  /** now put them into the image location popup **/
  /**********************************************************************/
  fill_image_location_popup_with_locations(q); 
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Output_Current_qhdCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: gets the filename, and outputs the current qhd_popup settings
%%           to the file
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Output_Current_qhdCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  qsh_gui_t *q;
  char filename[256];
  char * ptr;
  int length;
  
  q = (qsh_gui_t *)clientdata;

  DEBUG_LIBQSH printf("Entered Ouput_Current_qhdCB\n");

  /**********************************************************************/
  /** get the values out of the qhd popup, this call also**/
  /** will error_check the qhd values **/
  /**********************************************************************/
  if (!get_user_filled_values(q)) return;
      
  /**********************************************************************/
  /** get the output filename **/
  /**********************************************************************/
  if (!get_qhd_filename(q->qsh_toplevel,q->qsh_app,"Enter QHD Filename",filename,q->qhd_filename)) return;

  /*
   * Make sure filename ends in .qhd
   */
  ptr = strrchr( filename, '.' );
  if( ptr == NULL || strcmp( ptr, ".qhd" ) != 0 )
      strcat( filename, ".qhd" );
  
  /**********************************************************************/
  /** now send the structure off to get written**/
  /**********************************************************************/
  write_qhd(q->qsh_info,filename);

}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: get_qhd_filename
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: brings up a small shell with a text box and save and close
%%           buttons, and waits for the user to chose
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_qhd_filename(Widget toplevel,XtAppContext app, char * title_string,
		     char *filename, char *initial_string)
{
    int successful = 0;
    char localFilename[256];

    DEBUG_LIBQSH printf("Entered get_qhd_filename\n");

    /* Get the filename from a file selection box */
    successful = DT_select_file( toplevel, app, localFilename, title_string );

    if( successful )
    {
        strcpy( filename, localFilename );
    }

    DEBUG_LIBQSH printf("Leaving get_qhd_filename\n");
    return( successful );
} 

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: qhd_saveCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: callback for the "save" button on the get_filename popup
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void qhd_saveCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  qhd_out_t *qhd_out = (qhd_out_t *)clientdata;
  FILE *temp_file;

  DEBUG_LIBQSH printf("Entered qhd_saveCB\n");
  
  /**********************************************************************/
  /** get the name out of the text box**/
  /**********************************************************************/
  strcpy(qhd_out->temp_filename,(char *)XmTextGetString(qhd_out->filebox));

  /**********************************************************************/
  /** check to see if the file already exists, and if so  **/
  /** make the user confirm writing over it **/
  /**********************************************************************/
  if ((temp_file = fopen(qhd_out->temp_filename,"r"))){
    qhd_out->confirmed = qsh_confirm(qhd_out->toplevel,qhd_out->app,
				    "File already exists, overwrite?");
    fclose(temp_file);
  }else{
    /**********************************************************************/
    /** the file doesn't exist go ahead and make it **/
    /**********************************************************************/
    qhd_out->confirmed = 1;
  }
  
  /**********************************************************************/
  /** if the user did confirm, then popdown the shell, otherwise**/
  /** they may want to change the filename and try again **/
  /**********************************************************************/
  if (qhd_out->confirmed) XtUnmanageChild(qhd_out->shell);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure:qhd_cancelCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: unmanages the get_filename shell
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void qhd_cancelCB(Widget w,XtPointer clientdata, XtPointer calldata)
{
  qhd_out_t *qhd_out = (qhd_out_t *)clientdata;
 
  DEBUG_LIBQSH printf("Entered qhd_cancelCB\n");

  XtUnmanageChild(qhd_out->shell);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: qsh_confirm
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: brings up a confirm shell and returns 1 or 0 depending 
%%           on whether the user confirms the message
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int qsh_confirm(Widget toplevel,XtAppContext app,char *message)
{
  XmString xmstr;
  qsh_confirm_t c;

  DEBUG_LIBQSH printf("Entered qsh_confirm\n");

  /**********************************************************************/
  /** first build the confirm message dialog  **/
  /**********************************************************************/
  c.shell = (Widget)XmCreateMessageDialog(toplevel, "QHD OUT",NULL,0);
  
  xmstr = XmStringCreateLocalized("Confirm");
  XtVaSetValues(c.shell,
		XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
		XmNdialogTitle,xmstr,
		NULL);    
  XmStringFree(xmstr);
  XtUnmanageChild((Widget)XmMessageBoxGetChild(c.shell,XmDIALOG_HELP_BUTTON));
  
  XtAddCallback(c.shell,
		XmNokCallback,qsh_ConfirmedCB,(XtPointer)&c);
  
  xmstr = XmStringCreateLocalized(message);
  XtVaSetValues((Widget)XmMessageBoxGetChild(c.shell,XmDIALOG_MESSAGE_LABEL),
		XmNlabelString,xmstr,
		NULL);
  XmStringFree(xmstr);


  c.confirmed = 0;
  XtManageChild(c.shell);
  
  /**********************************************************************/
  /** wait here until the user has decided **/
  /**********************************************************************/
  while (XtIsManaged(c.shell))
    XtAppProcessEvent(app, XtIMAll);
  
  /**********************************************************************/
  /** get rid of the shell **/
  /**********************************************************************/
  XtDestroyWidget(c.shell);

  /**********************************************************************/
  /** return the user's choice**/
  /**********************************************************************/
  return c.confirmed;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: qsh_ConfirmedCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: sets the confirmation to 1
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void qsh_ConfirmedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  qsh_confirm_t *c = (qsh_confirm_t *)clientdata;

  DEBUG_LIBQSH printf("Entered qsh_ConfirmedCB\n");

  /**********************************************************************/
  /** they press the ok button, confirming **/
  /**********************************************************************/
  c->confirmed = 1;
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: qsh_error_popup
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: displays a message concerning errors, or warnings in a 
%%           scrolled window
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int qsh_error_popup(Widget toplevel,XtAppContext app,int options, char *message)
{
   int i;
   XmString xmstr;
   qsh_error_t err;

   /******************************************************************
    ** NOTE:  the options paramter will either be 1 or 2,
    **
    **        if it is 1, this means that the user
    **           will only be given the 1 button to acknowledge the
    **           error
    **        if it is 2, both a fix button and ignore button
    **           will be available
    **
    ******************************************************************/

   DEBUG_LIBQSH printf("Entered qsh_error_popup\n");

  /**********************************************************************/
  /** get the user's attention**/
  /**********************************************************************/
   XBell (XtDisplay (toplevel), 100);

  /**********************************************************************/
  /** create the error_popup**/
  /**********************************************************************/
   err.shell =(Widget)XmCreateMessageDialog(toplevel,"error",NULL,0);
   
   xmstr = XmStringCreateLocalized("QHD Error");
   XtVaSetValues(err.shell,
		 XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL,
		 XmNdialogTitle,xmstr,
		 NULL);
   XmStringFree(xmstr);

  /**********************************************************************/
  /** get rid of unwanted decorations**/
  /**********************************************************************/   
   XtUnmanageChild((Widget)XmMessageBoxGetChild(err.shell,XmDIALOG_SYMBOL_LABEL));
   XtUnmanageChild((Widget)XmMessageBoxGetChild(err.shell,XmDIALOG_MESSAGE_LABEL));
   XtUnmanageChild((Widget)XmMessageBoxGetChild(err.shell,XmDIALOG_HELP_BUTTON));

  /**********************************************************************/
  /** if the option is JUST_WARN, then only give the user the ok button **/
  /**********************************************************************/
   if (options == 1) XtUnmanageChild((Widget)XmMessageBoxGetChild(err.shell,XmDIALOG_CANCEL_BUTTON)); 

  /**********************************************************************/
  /** set the button labels according to the error_option**/
  /**********************************************************************/
   if (options == 1)xmstr = XmStringCreateLocalized("OK");
   if (options == 2)xmstr = XmStringCreateLocalized("I'll Fix");
   XtVaSetValues((Widget)XmMessageBoxGetChild(err.shell,XmDIALOG_OK_BUTTON),
		 XmNlabelString, xmstr,
		 NULL);
   XmStringFree(xmstr);

   XtAddCallback(err.shell, XmNokCallback, 
		 (XtCallbackProc)qsh_errorpopup_fixCB, (XtPointer)&err);
   if(options == 2){
     xmstr = XmStringCreateLocalized("I'll Ignore");
     XtVaSetValues((Widget)XmMessageBoxGetChild(err.shell,XmDIALOG_CANCEL_BUTTON),
		   XmNlabelString, xmstr,
		   NULL);
     XmStringFree(xmstr);
     XtAddCallback(err.shell, XmNcancelCallback, 
		   (XtCallbackProc)qsh_errorpopup_ignoreCB, (XtPointer)&err);
   }

   err.sw = XtVaCreateManagedWidget("error sw",
				     xmScrolledWindowWidgetClass,err.shell,
				     XmNheight, 500,
				     XmNwidth, 500,
				     XmNscrollingPolicy, XmAUTOMATIC,
				     XmNvisualPolicy, XmVARIABLE,
				     NULL);

  /**********************************************************************/
  /** if the error message is not very long, shorten the message box size **/
  /**********************************************************************/   
   if (strlen(message) < 200) 
     XtVaSetValues(err.sw,XmNheight,150,NULL);

   err.main_rc = XtVaCreateManagedWidget("error main_rc",
					  xmRowColumnWidgetClass, err.sw,
					  XmNpacking, XmPACK_COLUMN,
					  XmNorientation, XmVERTICAL,
					  NULL);

  /**********************************************************************/
  /** put the message on a label widget **/
  /**********************************************************************/
   XtVaCreateManagedWidget(message, xmLabelWidgetClass,err.main_rc,
			   XtVaTypedArg, XmNlabelString, XmRString,
			   message, strlen(message)+1,
			   NULL);
   
  /**********************************************************************/
  /** manage the shell **/
  /**********************************************************************/
   XtManageChild (err.shell);

  /**********************************************************************/
  /** initialize the ignor_errors to 0**/
  /**********************************************************************/
   err.ignore_errors = 0;

  /**********************************************************************/
  /** wait here until the user decides**/
  /**********************************************************************/
  while (XtIsManaged(err.shell))
    XtAppProcessEvent(app, XtIMAll);

  /**********************************************************************/
  /** ok they decided now get rid of the shell**/
  /**********************************************************************/
  XtDestroyWidget(err.shell);

  /**********************************************************************/
  /** return the user's decision, if they hit the cancel button **/
  /** the ignore_errors will still be 0 **/
  /**********************************************************************/
  return err.ignore_errors;
} 

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: qsh_errorpopupokCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: gets rid of the error_popup
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void qsh_errorpopup_fixCB (Widget w, XtPointer clientdata, XtPointer calldata)
{
  qsh_error_t *err = (qsh_error_t *)clientdata;

  DEBUG_LIBQSH printf("Entered qsh_errorpopup_fixCB\n");
    
  /**********************************************************************/
  /** the user decided to fix the errors**/
  /**********************************************************************/
  err->ignore_errors = 0;
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: qsh_errorpopup_ignoreCB
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: gets rid of the error_popup
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void qsh_errorpopup_ignoreCB (Widget w, XtPointer clientdata, XtPointer calldata)
{
  qsh_error_t *err = (qsh_error_t *)clientdata;

  DEBUG_LIBQSH printf("Entered qsh_errorpopupokCB\n");

  /**********************************************************************/
  /** the user decided to ignor the errors**/
  /**********************************************************************/
  err->ignore_errors = 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: process_qsh_key_and_value
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: takes the given key and value and decides whether they 
%%           are needed and if so, fills them into the structure
%%
%%           NOTE:  also looks into the alias file to see if a key
%%                  has an alias
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int process_qsh_key_and_value(qsh_gui_t *q, char *key, char *value)
{
  int i,num;
  qsh_info_t *qsh_info = q->qsh_info;
  int numkeywords = 14;
  int caseval = -1,found_a_mapping=0;
  char mapped_key[256];
  enum keyword_labels{
    e_patient_name,
    e_modality,
    e_slice_orientation,
    e_dimensionality,
    e_bytes_per_pixel,
    e_byte_order,
    e_pixel_format,
    e_number_of_dimensions,
    e_size_of_dimension,
    e_reference_location,
    e_uniform_spacing,
    e_x_pixel_size,
    e_y_pixel_size,
    e_image_location
  } e_keyval;
  char *ptr, *keywords[]={
    "patient name",
    "modality",
    "slice orientation",
    "dimensionality",
    "bytes per pixel",
    "byte order",
    "pixel format",
    "number of dimensions",
    "size of dimension",
    "reference location",
    "uniform spacing",
    "x pixel size",
    "y pixel size",
    "image location"
  };

  DEBUG_LIBQSH_KEYS printf("Inside process qsh_key_and_value, processing  %s:%s\n",key,value);


  /**********************************************************************/
  /** Ok, run through the keywords and see if any match the key sent in**/
  /** if so, we have the match so just set caseval and get out **/
  /** so we can decide what to do with the key **/
  /**********************************************************************/
  for (i=0;i<numkeywords;i++){
    if (strstr(key,keywords[i])){
      DEBUG_LIBQSH_KEYS printf("Found the Key Match:   %s <==> %s\n", key,keywords[i]);
      caseval = i;
      break;
    }
  }
  

  /**********************************************************************/
  /** if we didn't find a match above, caseval will still be -1 **/
  /**********************************************************************/
  if (caseval==-1){ 
    /**********************************************************************/
    /** Now since we didn't find the match we have to check and see **/
    /** if there is an 'alias' (keymapping) in the already loaded **/
    /** keymap structure **/
    /**********************************************************************/  
    for (i=0;i<q->num_mappings;i++){
      if (strstr(key,q->keymap[i].key)){
	/**********************************************************************/
	/** we found a match in the keymap structure **/
	/** copy it out and lets see if we can use that instead **/
	/**********************************************************************/
       DEBUG_LIBQSH_KEYS printf("Didn't find the key, but found a mapping for it, trying the mapping:   %s => %s\n", key,keywords[i]);
       strcpy(mapped_key,q->keymap[i].mapped_key);
       found_a_mapping = 1;
       break;
      }
    }
    
    /**********************************************************************/
    /** if we didn't find a mapping, this key and value pair are **/
    /** goners **/
    /**********************************************************************/
    if (found_a_mapping){
      /**********************************************************************/
      /** we found a keymapping so lets see its mapping matches a keyword**/
      /**********************************************************************/
      for (i=0;i<numkeywords;i++){
	if (strstr(mapped_key,keywords[i])){
	  /**********************************************************************/
	  /** we found a match for the mapped key, lets get out and process it**/
	  /**********************************************************************/
	  DEBUG_LIBQSH_KEYS printf("Found the Key Match for the mapping:   %s <==> %s\n",mapped_key,keywords[i]);
	  caseval = i;
	  break;
	}
      }
    }

  }
    
  /**********************************************************************/
  /** if we didn't find a match, throw out the key and value and leave**/
  /**********************************************************************/
  if (caseval==-1 && found_a_mapping == 0 ) return(0);
  
  e_keyval = caseval;

  /**********************************************************************/
  /** switch the caseval we recieved to decide what to do with the key**/
  /** and value **/
  /**********************************************************************/
  switch(e_keyval){

    /**********************************************************************/
    /** we found the patient name key**/
    /**********************************************************************/
  case e_patient_name:
    if (strlen(value)<255){
      strcpy(qsh_info->patient_name,value);
    }else{
      qsh_info->patient_name[255] = '\0';
      strncpy(qsh_info->patient_name,value,255);
    }
    qsh_info->valid.patient_name = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   patient_name is : %s\n\n",qsh_info->patient_name);
    break;

    /**********************************************************************/
    /** we found the modality key**/
    /**********************************************************************/
  case e_modality:
    if (strlen(value)<255){
      strcpy(qsh_info->modality,value);
    }else{
      qsh_info->modality[255] = '\0';
      strncpy(qsh_info->modality,value,255);
    }
    qsh_info->valid.modality = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   modality is : %s\n\n",qsh_info->modality);
    break;

    /**********************************************************************/
    /** we found the slice orientation key**/
    /**********************************************************************/
  case e_slice_orientation:
    if (strlen(value)<255){
      strcpy(qsh_info->slice_orientation,value);
    }else{
      qsh_info->slice_orientation[255] = '\0';
      strncpy(qsh_info->slice_orientation,value,255);
    }
    qsh_info->valid.slice_orientation = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   slice_orientation is : %s\n\n",qsh_info->slice_orientation);
    break;

    /**********************************************************************/
    /** we found the dimensionality key**/
    /**********************************************************************/
  case e_dimensionality:
    if (strlen(value)<255){
      strcpy(qsh_info->dimensionality,value);
    }else{
      qsh_info->dimensionality[255] = '\0';
      strncpy(qsh_info->dimensionality,value,255);
    }
    qsh_info->valid.dimensionality = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   dimensionality is : %s\n\n",qsh_info->dimensionality);
    break;

    /**********************************************************************/
    /** we found the bytes per pixel key**/
    /**********************************************************************/
  case e_bytes_per_pixel:
    qsh_info->bytes_per_pixel = atoi(value);
    qsh_info->valid.bytes_per_pixel = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   bytes_per_pixel is : %d\n\n",qsh_info->bytes_per_pixel);
    break;

    /**********************************************************************/
    /** we found the byte order key**/
    /**********************************************************************/
  case e_byte_order:
    if (strlen(value)<255){
      strcpy(qsh_info->byte_order,value);
    }else{
      qsh_info->byte_order[255] = '\0';
      strncpy(qsh_info->byte_order,value,255);
    }
    qsh_info->valid.byte_order = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   byte_ordery is : %s\n\n",qsh_info->byte_order);
    break;

    /**********************************************************************/
    /** we found the pixel format key**/
    /**********************************************************************/
  case e_pixel_format:
    qsh_info->pixel_format = atoi(value);
    qsh_info->valid.pixel_format = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   pixel_format is : %d\n\n",qsh_info->pixel_format);
    break;

    /**********************************************************************/
    /** we found the number of dimensions key**/
    /**********************************************************************/
  case e_number_of_dimensions:
    qsh_info->number_of_dimensions = atoi(value);
    qsh_info->valid.number_of_dimensions = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   number_of_dimensions is : %d\n\n",qsh_info->number_of_dimensions);
    break;

    /**********************************************************************/
    /** we found the size of dimension [?] key**/
    /**********************************************************************/
  case e_size_of_dimension:
    num = get_number_in_brackets_from_string(key);
    if (num != -1){ 
      qsh_info->size_of_dimension[num] = atoi(value);
      qsh_info->valid.size_of_dimension[num] = 1;
        DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   size_of_dimension[%d] is : %d\n\n",num,qsh_info->size_of_dimension[num]);
    }
    break;

    /**********************************************************************/
    /** we found the reference location key**/
    /**********************************************************************/
  case e_reference_location:
    qsh_info->reference_location = atof(value);
    qsh_info->valid.reference_location = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   reference_location is : %f\n\n",qsh_info->reference_location);
    break;

    /**********************************************************************/
    /** we found the uniform spacing key**/
    /**********************************************************************/
  case e_uniform_spacing:
    qsh_info->uniform_spacing = atof(value);
    qsh_info->valid.uniform_spacing = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   uniform_spacing is : %f\n\n",qsh_info->uniform_spacing);
    break;

    /**********************************************************************/
    /** we found the x pixel size key**/
    /**********************************************************************/
  case e_x_pixel_size:
    qsh_info->x_pixel_size = atof(value);
    qsh_info->valid.x_pixel_size = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   x_pixel_size is : %f\n\n",qsh_info->x_pixel_size);
    break;

    /**********************************************************************/
    /** we found the y pixel size key**/
    /**********************************************************************/
  case e_y_pixel_size:
    qsh_info->y_pixel_size = atof(value);
    qsh_info->valid.y_pixel_size = 1;
    DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   y_pixel_size is : %f\n\n",qsh_info->y_pixel_size);
    break;

    /**********************************************************************/
    /** we found the image location [?] key**/
    /**********************************************************************/
  case e_image_location:  
    num = get_number_in_brackets_from_string(key);
    if (num != -1){
      qsh_info->image_location[num] = atof(value);
      qsh_info->valid.image_location[num] = 1;
      DEBUG_LIBQSH_KEYS printf("----->>> GOT IT:   image_location[%d] is : %f\n\n",num,qsh_info->image_location[num]);
    }
    break;
  }
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: init_qsh_info_validity
%%
%%  Written By: Cory Albright
%%
%%  Parameters:
%%
%%  Purpose: initiallizes all the keys validations to 0 for a qsh_info_t 
%%           structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_qsh_info_validity(qsh_info_t *qsh_info)
{
  int i;

   DEBUG_LIBQSH printf("Entered init_qsh_info_validity\n");
  
  /**********************************************************************/
  /** set all the valid keys to 0 **/
  /**********************************************************************/
  qsh_info->valid.patient_name = 0;
  qsh_info->valid.modality = 0;;           /*   "CT" | "MRI" | "NMR" | "PET"            */
  qsh_info->valid.slice_orientation = 0;;  /* "Transverse"|"Saggital"|"Coronal" */
  qsh_info->valid.dimensionality = 0;        /*    "mm"|"cm"|"in"                 */
  qsh_info->valid.bytes_per_pixel = 0;
  qsh_info->valid.byte_order = 0;            /*    "little endian" | "big endian" */
  qsh_info->valid.pixel_format = 0;
  qsh_info->valid.number_of_dimensions = 0;
  for (i=0;i<3;i++) qsh_info->valid.size_of_dimension[i] = 0;
  qsh_info->valid.reference_location = 0;
  qsh_info->valid.uniform_spacing = 0;
  qsh_info->valid.x_pixel_size = 0;
  qsh_info->valid.y_pixel_size = 0;
  qsh_info->valid.images = 0;
  qsh_info->valid.high_byte_images = 0;
  qsh_info->valid.low_byte_images = 0;

  for (i=0;i<MAX_QSH_SLICES;i++) qsh_info->valid.image_location[i] = 0;

}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: get_number_in_brackets_from_string
%%
%%  Written By: Cory Albright
%%
%%  Parameters: the string
%%
%%  Purpose: returns the number in brackets from a string
%%           ex.     image_location[50] , returns 50
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_number_in_brackets_from_string(char *string)
{
  int num,i=0;
  char *temp,c;

  DEBUG_LIBQSH printf("Entered get_number_in_brackets_from_string\n");

  /**********************************************************************/
  /** first make sure that there is a set of brackets in the string**/
  /**********************************************************************/
  if (!strstr(string,"[")){
    printf("WARNING:  expecting a number in brackets for the string : %s, ignoring key\n",string);
    return -1;
  }

  /**********************************************************************/
  /** scan over to the brackets**/
  /**********************************************************************/
  temp = string;
  while(temp[0] != '['){
    i++;
    temp = &string[i];
  }
  i++;
  temp = &string[i];

  /**********************************************************************/
  /** read out the integer within the brackets (pointed to by temp)**/
  /**********************************************************************/
  sscanf(temp,"%d",&num);
 
  /**********************************************************************/
  /** send back the number **/
  /**********************************************************************/ 
  return num;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: fill_correct_qhd_qim_filenames
%%
%%  Written By: Cory Albright
%%
%%  Parameters: the structure
%%
%%  Purpose: 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int fill_correct_qhd_qim_filenames(qsh_gui_t *q,char *filename)
{
  
  /******************************************************************/
  /** get the correct file extension in the correct string    **/
  /******************************************************************/
  if (strstr(filename,".qhd")){
    strcpy(q->qhd_filename,filename);
    strcpy(q->qim_filename,filename);
    q->qim_filename[strlen(q->qim_filename)-2] = 'i';
    q->qim_filename[strlen(q->qim_filename)-1] = 'm';
  }else if (strstr(filename,".qim")){
    strcpy(q->qhd_filename,filename);
    strcpy(q->qim_filename,filename);
    q->qhd_filename[strlen(q->qhd_filename)-2] = 'h';
    q->qhd_filename[strlen(q->qhd_filename)-1] = 'd';
  }else{
    printf("not a valid qsh file\n");
    return 0;
  }
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: Free_Qsh
%%
%%  Written By: Cory Albright
%%
%%  Parameters: the structure
%%
%%  Purpose: 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Free_Qsh(qsh_info_t *qsh)
{
  if (qsh->valid.images) MT_free( (void*) qsh->images);
  if (qsh->valid.high_byte_images) MT_free( (void*) qsh->high_byte_images);
  if (qsh->valid.low_byte_images) MT_free( (void*) qsh->low_byte_images);
  MT_free( (void*) qsh);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: QSH_readln
%%
%%  Written By: Mike Frandsen (taken from KeyVal Tools, keyval_tools.c)
%%
%%  Parameters:
%%
%%  Purpose: Reads a line from a file up to (maxsize-1) characters.  Because
%%           it is intended to read a line, will stop after reading a '\n'.
%%           The line is then terminated with a '\0' which uses a byte in
%%           the array and is why at most (maxsize-1) characters can be read.
%%           The LENGTH returned should be the same as that that would be
%%           reported by STRLEN.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int QSH_readln(FILE *fptr, char *s, int maxsize) {
  int i=0, ch;

  while (i<(maxsize-1)) {
    ch=fgetc(fptr);
    if (ch<0) {
      /* don't increment i because we didn't really read a character,
       * we hit the end of the file or had an error -- so just stop
       */
      s[i]='\0';
      return(i);
    } else {
      s[i] = (char)ch;
      i++;
      if ((char)ch=='\n') {
	s[i] = '\0';
	return(i);
      }
    }
  }
  /* If here, filled all the characters before encountering a \n */
  /* --Better terminate the string */
  s[maxsize-1] = '\0';
  return(i);
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: QSH_readln_skip_comments_and_trim
%%
%%  Written By: Mike Frandsen (taken from KeyVal Tools, keyval_tools.c)
%%
%%  Parameters:
%%
%%  Purpose: Acts a lot like UV_readln with these additions:
%%           Skips comments
%%           Trims the string when done (removes trailing and leading
%%           white space)
%%           If what remains is of length 0, will look for the next line
%%           SO when 0 returns that means no more meaningful lines
%%           in file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int QSH_readln_skip_comments_and_trim(FILE *fptr, char *s, int maxsize) {
  int length;
  char * loc;

  do {
    length = QSH_readln(fptr, s, maxsize);
    if (length==0) return(0); /* hit end of file */
    
    /* In this case, remove comments and then trim */
    if ((loc = (char *)strchr(s, '#'))) {
      loc[0] = '\0';
      length = strlen(s);
    }

    if (length>0) {
      length = QSH_trim_string(s);
    }
  } while (length==0);
  
  return(length);
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: QSH_break_into_lower_key_and_value
%%
%%  Written By: Mike Frandsen (taken from KeyVal Tools, keyval_tools.c)
%%
%%  Parameters:
%%
%%  Purpose:  Looks for first br to break the string in two (if none, all
%%            of string is key)
%%            throws out the br portion
%%            'trims' each remaining string
%%            converts the key to lowercase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void QSH_break_into_lower_key_and_value(char * br, char * s,
				       char ** key, char ** value) {
  char * wherebreak;
  
  if ((wherebreak=(char *)strstr(s, br))) {
    *value = wherebreak + strlen(br);
    QSH_trim_string(*value);
    wherebreak[0] = '\0';
  } else {
    *value = s+strlen(s); /* The '\0' at the end */
  }
  *key = s;
  QSH_trim_string(*key);
  QSH_make_lower_string(*key);
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: QSH_make_lower_string
%%
%%  Written By: Mike Frandsen (taken from KeyVal Tools, keyval_tools.c)
%%
%%  Parameters:
%%
%%  Purpose:Given a string, converts all uppercase characters found to
%%           corresponding lowercase characters
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void QSH_make_lower_string(char *s) {
  int i;
  char ch;
  
  i=0;
  while ((ch=s[i])) {
    s[i]=(char)tolower((int)ch);
    i++;
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: QSH_read_next_key_and_value
%%
%%  Written By: Mike Frandsen (taken from KeyVal Tools, keyval_tools.c)
%%
%%  Parameters:
%%
%%  Purpose: returns true whenever it can read another non-null line from
%%           the file.  It's not necessarily a key-value pair.  (For example,
%%           'end' will say 'end' is the key and the value is N/A.)  It then
%%           fills in each passed sub-part.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int QSH_read_next_key_and_value(FILE * fptr, char * str, int maxl, 
			       char ** key, char ** value) {
  int len;

  len = QSH_readln_skip_comments_and_trim(fptr, str, maxl);
  if (len==0) return(0);
  QSH_break_into_lower_key_and_value(QSH_KEY_VALUE_SPLIT_STRING, 
				     str, key, value);
  return(1);
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: QSH_read_string_value_for_key
%%
%%  Written By: Mike Frandsen (taken from KeyVal Tools, keyval_tools.c)
%%
%%  Parameters:
%%
%%  Purpose: rewinds the file, searches for first occurance of key,
%%           returns value of the key
%%           Returns 1 if it's found, 0 if it's not
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int QSH_read_string_value_for_key(FILE * fptr, char * key, 
				 char * value, int maxl) {
  char local_str[MAX_QSH_LINE_SIZE], *local_key, *local_value;

  rewind(fptr);
  while (QSH_read_next_key_and_value(fptr, local_str, MAX_QSH_LINE_SIZE,
				 &local_key, &local_value)) {
    if (!strcmp(local_key, key)) {
      if (strlen(local_value)<maxl) {
	strcpy(value, local_value);
      } else {
	strncpy(value, local_value, maxl-1);
	value[maxl-1] = '\0';
      }
      return(1); /* terminiate when first finds the key */
    }
  }

  return(0);
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: QSH_trim_string
%%
%%  Written By: Mike Frandsen (taken from KeyVal Tools, keyval_tools.c)
%%
%%  Parameters:
%%
%%  Purpose:  Based on length of passed string, removes all 'isspace'
%%            characters at end of string and then those at the beginning.
%%            If it removes characters at the beginning, it must translate
%%            the string backwards.  Returns the length of the 'trimmed'
%%            string.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int QSH_trim_string(char * s) {
  int i, length, first_non_space;

  length = strlen(s);

  /* Trim the end of the string */
  while((length>0)&&(isspace(s[length-1]))) {
    length--;
    s[length] = '\0';
  }

  /* Now, trim the beginning of the string -- this will consist of
   * translating the string backwards a few bytes, as necessary
   */
  if (length>0) {
    first_non_space = 0;

    while((s[first_non_space]!='\0')&&(isspace(s[first_non_space])))
      first_non_space++;

    if (first_non_space>0) {
      for (i=first_non_space; i<=length; i++) {
	s[i-first_non_space] = s[i];
      }
      length -= first_non_space;
    }
  }

  return(length);
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     is_a_valid_qsh_file
%% 
%% Purpose:      Determine if a given string ends in either .qhd or .qim
%% 
%% Parameters:   filename -> A char *, the string to test.
%% 
%% Return Value: 1 if the string ends in .qim or .qhd
%%               0 if the string does not end in .qim or .qhd
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int is_a_valid_qsh_file( char * filename )
{
  int valid = 0;
  int length;
  char * ptr;

  /* 
   * Look for the '.' starting at the end 
   * of the string.
   */
  ptr = strrchr( filename, '.' );

  if( ptr != NULL )
  {
    length = strlen( ptr );  /* length must be 4 to be valid */
    if( length == 4 )       
    {
      if( strcmp( ptr, ".qim" ) == 0 ||
	  strcmp( ptr, ".qhd" ) == 0 )
	valid = 1;
    }
  }
  return( valid );
}
