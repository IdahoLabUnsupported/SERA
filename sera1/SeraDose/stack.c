/*
 * stack.c
 *
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 * David Helzer  7/97
 *
 * Taken from L.S. Foster's "C by Discovery, 2nd Edition"
 *
 * This file contains the stack utility functions push(), pop(),
 * init_stack(), is_empty(), and is_full().
 *
 */


/* Include Files */
#include "stack.h"


/* 
 * init_stack() - initializes a stack to an empty state by
 * setting top beyond the end of the array
 */
 
void init_stack (STACK *s_ptr)
{
   s_ptr->top = MAX;
}


/* 
 * is_empty() - returns 1 if the STACK is empty and 0 if not
 */

int is_empty (STACK *s_ptr)
{
   return (s_ptr->top >= MAX);
}


/* 
 * is_full() - returns 1 if the STACK is full and 0 if not
 */
 
int is_full (STACK *s_ptr)
{
   return (s_ptr->top <= 0);
}


/*
 * push() - puts a new item on the stack and adjusts the top
 */
 
int push (int item, STACK *s_ptr)
{
   if (is_full (s_ptr))
      return (-1);
   else 
   {
      s_ptr->top--;
      s_ptr->elts[s_ptr->top] = item;
      return (0);
   }
}


/*
 * pop() - removes an item from teh top of the STACK, adjusts
 * the top, and returns the removed item, or -1 if the STACK
 * was empty
 */
 
int pop (STACK *s_ptr)
{
   if (is_empty (s_ptr))
      return (-1);
   else
      return (s_ptr->elts[s_ptr->top++]);
}
