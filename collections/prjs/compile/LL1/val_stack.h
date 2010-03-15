/**
 * value stack
 * kevin lynx
 */
#ifndef ___VAL_STACK_H_
#define ___VAL_STACK_H_

typedef double Number;
#define STACK_SIZE (256)

void val_push( Number v );
Number val_pop();
/* calculate the value on top of the stack, push the result on top the stack */
void val_calc(); 

#endif

