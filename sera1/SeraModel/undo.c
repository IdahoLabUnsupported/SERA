#include "include.h"
#include "functions.h"
#include "segment.h"
#include "image_matrix.h"
#include "undo.h"

/* Here are the "private" functions */
void save_for_restore(int, int, int, unsigned char *);
/************************************/

/* Roughly, max amt of memory devoted to undos -- in bytes */
/* Technically, removes undos until amt of memory used falls below
 * this limit and then adds the new item for undo -- possibly going a little
 * over the limit (but not by much)
 * NOTE ALSO:  If you rotate the images, the amount of undo memory may also
 * go up.  If it goes over the limit, it will remain over the limit until
 * another undo is saved and the computer realizes the limit has been exceeded.
 */
#ifndef UNDO_MEM_MAX
#define UNDO_MEM_MAX 4000000
#endif

#ifndef UNDO_MAX
#define UNDO_MAX 1024
#endif

/* For restoring the last undo */
int can_restore = 0;
int restore_index;
int restore_width;
int restore_height;
unsigned char *restore_data;
int undo_mem_used=0; /* total amt of undo memory used */
/* treat the undo's as a circular array keeping track of last UNDO_MAX */
int undo_circ_size=0, undo_saved=-1, undo_to_save=0, undo_count=0;
undo_type undo_array[UNDO_MAX]; /* needs to be size UNDO_MAX */
int current_key = 0;            /* will increment with undos */
int using_undo_set = 0;         /* generally, increment key with
				 * each saved undo.  However, for a whole
				 * undo set, keep the key the same
				 */
/*******************************/

void save_for_undo_no_restores(int z) {
  if (can_restore) {
    can_restore=0;
    MT_free((void*)restore_data);
  }
  save_for_undo(z);
}

void save_for_undo(int z) {
  int size;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  /* Don't add beyond allowed amount of memory */
  if (undo_mem_used>UNDO_MEM_MAX) {
    while (undo_mem_used>UNDO_MEM_MAX) {
      undo_circ_size--;
      set_undo_invalid(&(undo_array[(undo_saved+UNDO_MAX-undo_circ_size)%UNDO_MAX]));
    }
  }

  /* Don't want to free these until entire array is filled
   * and wrapping around
   */
  if (undo_circ_size==UNDO_MAX) {
    set_undo_invalid(&(undo_array[undo_to_save]));
  } else {
    undo_circ_size++;
  }
  undo_array[undo_to_save].valid = 1;
  undo_array[undo_to_save].index = z;
  undo_array[undo_to_save].width = image_matrix_ptr->img_arr[z].data_w;
  undo_array[undo_to_save].height = image_matrix_ptr->img_arr[z].data_h;
  undo_array[undo_to_save].undo_set_key = current_key;
  if (!using_undo_set) {
    current_key++;
  }

  size = undo_array[undo_to_save].width*undo_array[undo_to_save].height;
  undo_array[undo_to_save].data =
    (unsigned char *)MT_malloc(size*sizeof(unsigned char ));
  /* keep track of data so we can 'undo' */
  memcpy(undo_array[undo_to_save].data,
	 image_matrix_ptr->img_arr[z].region_data,
	 size*sizeof(unsigned char));
  undo_array[undo_to_save].compressed = 0;
  undo_mem_used+=size;
  undo_count++;
  compress_undo_data(&(undo_array[undo_to_save]));
  debug("Undo count:  %d                  kbytes used:  %f/%f\n", undo_count, ((float)undo_mem_used)/1000.0, ((float)UNDO_MEM_MAX)/1000.0);
  add_to_image_undo_list(&(undo_array[undo_to_save]), z);

  /* Save index of where to save next time */
  undo_saved = undo_to_save;
  undo_to_save = (undo_to_save+1)%UNDO_MAX;
}

void undo(void) {
  int still_working = 1;
  int undo_key;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  /* Restore image -- provided there has been at least 1 'apply' */
  while ((undo_circ_size>0)&&(still_working)) {
    undo_circ_size--;
    /* Make sure the next undo is valid before using it
     */
    if (undo_array[undo_saved].valid) {
      undo_key = undo_array[undo_saved].undo_set_key;
      restore_undo(&(undo_array[undo_saved]));
      still_working = 0; /* Here, actually did an undo */
    }
    undo_to_save = undo_saved;
    undo_saved = (undo_saved+UNDO_MAX-1)%UNDO_MAX;
    /* This part checks to see if the undo is really a part of an undo set */
    if ((!still_working)&&(undo_circ_size>0)) {
      if (undo_array[undo_saved].undo_set_key==undo_key) {
	still_working = 1;
      }
    }
  }
}

void restore_undo (undo_type * undo_ptr) {
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  if (!undo_ptr) return;
  if (!(undo_ptr->valid)) return;
  
  /* Uncompress if needed */
  uncompress_undo_data(undo_ptr);
  save_for_restore(undo_ptr->index, undo_ptr->width, undo_ptr->height,
		   image_matrix_ptr->img_arr[undo_ptr->index].region_data);
  memcpy(image_matrix_ptr->img_arr[undo_ptr->index].region_data,
	 undo_ptr->data,
	 (undo_ptr->width)*(undo_ptr->height)*sizeof(unsigned char));
  draw_image(image_matrix_ptr, undo_ptr->index);
  set_undo_invalid(undo_ptr);
}

void undo_on_imageCB(Widget w, XtPointer ClientData, XtPointer CallData) {
  static int odd_call = 1;
  static MOUSE_MODE old_mode;
  XmString xmstr;
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_UNDO_MODE)) return;

  image_matrix_ptr = get_image_matrix();

  if (odd_call) {
    old_mode=get_mouse_function();
    set_mouse_function(MM_UNDO);
    xmstr = XmStringCreateLtoR("Undo Off", image_matrix_ptr->char_set);
  } else {
    set_mouse_function(old_mode);
    xmstr = XmStringCreateLtoR("Undo Mode", image_matrix_ptr->char_set);
  }
  XtVaSetValues(w,
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);
  odd_call = 1-odd_call;
}

/* Restore the last applied undo */
void restoreCB(Widget w, XtPointer ClientData, XtPointer CallData) {
  image_matrix_type * image_matrix_ptr;

  if (!is_allowed_callback(CB_UNDO)) return;

  image_matrix_ptr = get_image_matrix();

  if (can_restore) {
    if (restore_index<image_matrix_ptr->num_pics) {
      if ((restore_width==image_matrix_ptr->img_arr[restore_index].data_w)&&
	  (restore_height==image_matrix_ptr->img_arr[restore_index].data_h)) {
	save_for_undo(restore_index);
	memcpy(image_matrix_ptr->img_arr[restore_index].region_data,
	       restore_data,
	       restore_width*restore_height*sizeof(unsigned char));
	draw_image(image_matrix_ptr, restore_index);
      }
    }
    can_restore = 0;
    MT_free((void*)restore_data);
  }
}

void compress_undo_data(undo_type * undo_ptr) {
  int max_size, cur_size, num_els, i;
  unsigned char * big_compressed_data, * orig_data;
  unsigned char cur_character, next_character, cur_character_count;

  if (!undo_ptr) return;
  if (undo_ptr->compressed) return; /* already compressed */
  if (!(orig_data=undo_ptr->data)) return;    /* no data present */

  num_els = (undo_ptr->width)*(undo_ptr->height);
  max_size = 2*num_els;
  big_compressed_data = (unsigned char *)MT_malloc(max_size*sizeof(unsigned char));

  cur_size = 0;
  cur_character = orig_data[0];
  cur_character_count = 0;
  for (i=0; i<=num_els; i++) {
    if (i<num_els)
      next_character = orig_data[i];
    else /* force the last character to be different */
      next_character = cur_character+1;
    if ((next_character==cur_character)&&(cur_character_count<255)) {
      cur_character_count++;
    } else {
      big_compressed_data[cur_size++] = cur_character_count;
      big_compressed_data[cur_size++] = cur_character;
      cur_character = next_character;
      cur_character_count=1; /* the new next character */
    }
  }

  /* only keep compressed version if it's smaller */
  if (cur_size<num_els) {
    undo_ptr->compressed_data = (unsigned char *)MT_malloc(cur_size*sizeof(unsigned char));
    memcpy(undo_ptr->compressed_data, big_compressed_data, cur_size*sizeof(unsigned char));
    MT_free((void*)undo_ptr->data);
    undo_ptr->data = NULL;
    undo_ptr->compressed_size = cur_size;
    undo_ptr->compressed = 1;
    undo_mem_used-=num_els;
    undo_mem_used+=cur_size;
  } else {
    undo_ptr->compressed_data = NULL;
    undo_ptr->compressed_size = 0;
  }
  MT_free((void*)big_compressed_data);
  debug("compression ratio:  %d to %d = %2.4f:1\n", num_els, cur_size,
	 ((float)num_els)/(float)cur_size);
}


void uncompress_undo_data(undo_type * undo_ptr) {
  unsigned char * data, * compressed_data, cur_char, count;
  int newsize, i, ci, ci_top, num_repeated;

  if (!undo_ptr) return;
  if (!(undo_ptr->compressed)) return; /* not compressed */
  if (!(compressed_data=undo_ptr->compressed_data)) return; /* no data present */

  newsize = undo_ptr->width*undo_ptr->height;
  data = (unsigned char *)MT_malloc(newsize*sizeof(unsigned char));
  undo_ptr->data = data;

  ci_top = undo_ptr->compressed_size;
  i = 0;
  for (ci=0; ci<ci_top; ci+=2) {
    num_repeated = (int)compressed_data[ci];
    if (i+num_repeated<=newsize)
      memset(data+i, (int)compressed_data[ci+1], num_repeated);
    i+=num_repeated;
  }

  undo_mem_used-=ci_top;
  undo_mem_used+=newsize;
  MT_free((void*)undo_ptr->compressed_data);
  undo_ptr->compressed_data = NULL;
  undo_ptr->compressed_size = 0;
  undo_ptr->compressed = 0;
}

unsigned char * get_undo_data(undo_type * undo_ptr) {
  if (!undo_ptr) return(NULL);
  if (!(undo_ptr->valid)) return(NULL);
  uncompress_undo_data(undo_ptr);
  return(undo_ptr->data);
}

void set_undo_invalid(undo_type * undo_ptr) {
  if (!undo_ptr) return;
  if (!(undo_ptr->valid)) return;

  remove_from_image_undo_list(undo_ptr);
  if (undo_ptr->compressed) {
    MT_free((void*)undo_ptr->compressed_data);
    undo_mem_used-=undo_ptr->compressed_size;
  } else {
    MT_free((void*)undo_ptr->data);
    undo_mem_used-=(undo_ptr->width)*(undo_ptr->height);
  }
  undo_ptr->data = NULL;
  undo_ptr->compressed_data = NULL;
  undo_ptr->compressed_size = 0;
  undo_ptr->compressed = 0;
  undo_ptr->valid = 0;
  undo_count--;
  debug("Undo count:  %d                  kbytes used:  %f/%f\n", undo_count, ((float)undo_mem_used)/1000.0, ((float)UNDO_MEM_MAX)/1000.0);
}

void save_for_restore(int index, int width, int height, unsigned char * data) {
  if (can_restore) {
    can_restore = 0;
    MT_free((void*)restore_data);
  }
  restore_data = (unsigned char *)MT_malloc(width*height*sizeof(unsigned char));
  memcpy(restore_data, data, width*height*sizeof(unsigned char));
  restore_width = width;
  restore_height = height;
  restore_index = index;
  can_restore = 1;
}

/* undo_set_key will remain the same while in an undo_set */
void start_undo_set(void) {
  using_undo_set = 1;
}

/* stop the undo_set -- will let the current_key change again */
void end_undo_set(void) {
  using_undo_set = 0;
  current_key++;
}
