///
///
///
#include "../src/kl_async_operator.h"

using namespace kl_common;

struct node
{
	int _count;

	node( int i ) : _count( i )
	{
	}
};

async_operator<node> AsyncOper;

void node_op( const node &n )
{
	printf( "node_op : %d\n", n._count );

	Sleep( 100 );
}

void node_init()
{
	//::CoInitialize( 0 );
}

void node_release()
{
	//::CoUninitialize();
}

int main()
{
	AsyncOper.execute( async_operator<node>::operator_type( node_op ), async_operator<node>::init_type( node_init ),
		async_operator<node>::release_type( node_release ) );

	for( int i = 0; i < 100; ++ i )
	{
		AsyncOper.list().push_back( node( i ) );
		Sleep( 10 );
	}

	getchar();
	AsyncOper.exit();
	return 0;
}