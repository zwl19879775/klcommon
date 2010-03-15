/**
 * test LL(1) practice
 * kevin lynx
 * 3.15.2010
 */
#include <stdio.h>
#include "val_stack.h"

extern void init_grammer();
extern int LL_analyse();
extern void release();

int main()
{
	init_grammer();
	if( !LL_analyse() )
	{
		printf( "parse ok\n" );
	}
	release();

	{
		Number ret = val_pop();
		printf( "ret=%lf\n", ret );
	}
	return 0;
}

