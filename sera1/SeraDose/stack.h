/*
 * stack.h
 * 
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 * David Helzer 7/97
 *
 * Taken from L.S. Foster's "C By Discover, 2nd ed."
 *
 * Contains the declaration of the data type STACK and the
 * declarations of the stack utility functions. This header 
 * file should be included in every C source file that 
 * referncess a STACK or any of its utility functions.
 */
 

/* Constant Declarations */
#define MAX 262144          /* From 512 x 512 */


/* Type Descriptions */
typedef struct {
   int elts[MAX];
   int top;
} STACK;


/* Stack Utility Function Prototypes */
int push (int item, STACK *s_ptr);
int pop (STACK *s_ptr);
void init_stack (STACK *s_ptr);
int is_empty (STACK *s_ptr);
int is_full (STACK *s_ptr);
