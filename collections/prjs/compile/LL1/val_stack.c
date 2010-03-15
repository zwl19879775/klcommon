/**
 * value stack
 * kevin lynx
 * 3.15.2010
 */
#include "val_stack.h"

static Number s[STACK_SIZE];
static int pos = 0;

void val_push( Number v )
{
	s[pos++] = v;
}

Number val_pop()
{
	return s[--pos];
}

void val_calc()
{
	Number op1 = val_pop();
	int op = (int) val_pop();
	Number op2 = val_pop();
	Number ret;
	switch( op )
	{
	case '+':
		ret = op2 + op1;
		break;
	case '-':
		ret = op2 - op1;
		break;
	case '*':
		ret = op2 * op1;
		break;
	case '/':
		if( op1 != 0 )
			ret = op2 / op1;
		break;
	default:
		ret = -1; /* error */
	}

	val_push( ret );
}

