/*
 *
 */
#include "async_oper.h"
#include <stdio.h>
#include <stdlib.h>

int func( void *p )
{
	int *a = (int*) p;
	(*a)++;
	printf( "%d\n", *a );
	return *a == 10 ? 1 : 0;
}

int main()
{
	struct async_oper op;
	int a = 0;
	async_init( &op, func, &a );
	getchar();		
	async_start( &op );
	getchar();
	async_release( &op );
	return 0;
}

