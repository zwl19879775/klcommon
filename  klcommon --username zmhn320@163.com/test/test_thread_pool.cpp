///
/// test thread pool
///
#include "../src/kl_threadpool.h"
#include <stdio.h>

void func( void *p )
{
	int count = (int)p;
	printf( "%d : ", count );
	for( int i = 0; i < count; ++ i )
	{
		printf( "." );
	}
	printf( "\n" );
}

int main()
{
	kl_common::thread_pool tp;
	tp.startup();
	
	tp.dispatch( kl_common::thread_pool::thread_func( func ), (void*)4 );
	tp.dispatch( kl_common::thread_pool::thread_func( func ), (void*)10 );
	tp.dispatch( kl_common::thread_pool::thread_func( func ), (void*)5 );
	tp.dispatch( kl_common::thread_pool::thread_func( func ), (void*)3 );

	tp.cleanup();
	printf( "exit all threads ok\n" );
	return 0;
}