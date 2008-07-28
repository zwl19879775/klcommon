/**
  @file mem_test.c
  @brief to test the memcached client.
*/
#include <winsock2.h>
#include "../kl_memcached/kl_memcached.h"

void error_handler( int err_code, const char *desc )
{
	fprintf( stderr, "ERROR : \nCode : %d\nDesc : %s\n", err_code, desc );
}

void test_set( const struct mem_server *server )
{
	int ret = 0;
	char data[] = "some string data";	
	struct store_item item;
	item._exp_time = 0;
	NEW_ITEM_DATA( item._item, "TestSet", 0 );
	
	evbuffer_add( STORE_ITEM_BUF( item ), data, strlen( data ) );
	
	ret = mem_set( server, &item, 1 );
	if( ret == MEM_STORED )
	{
		printf( "test_set : stored.\n" );
	}

	DEL_ITEM_DATA( item._item );
}

void test_add( const struct mem_server *server )
{
	int ret = 0;
	char data[] = "some string data";	
	struct store_item item;
	item._exp_time = 0;
	NEW_ITEM_DATA( item._item, "TestAdd", 0 );
	
	evbuffer_add( STORE_ITEM_BUF( item ), data, strlen( data ) );
	
	ret = mem_add( server, &item, 1 );
	if( ret == MEM_STORED )
	{
		printf( "test_add : stored.\n" );
	}
	if( ret == MEM_NOT_STORED )
	{
		/* delete the item */
		mem_delete( server, item._item._key, 1 );
	}

	DEL_ITEM_DATA( item._item );
}

void test_replace( const struct mem_server *server )
{
	int ret = 0;
	char data[] = "some string data";	
	struct store_item item;
	item._exp_time = 0;
	NEW_ITEM_DATA( item._item, "TestReplace", 0 );
	
	evbuffer_add( STORE_ITEM_BUF( item ), data, strlen( data ) );
	
	ret = mem_replace( server, &item, 1 );
	if( ret == MEM_STORED )
	{
		printf( "test_replace : stored.\n" );
	}
	if( ret == MEM_NOT_STORED )
	{
		/* add this */
		mem_add( server, &item, 1 );
	}

	DEL_ITEM_DATA( item._item );
}

void test_stats( const struct mem_server *server )
{
	char stat_info[512];
	mem_stats( server, stat_info, sizeof( stat_info ) );
	printf( "stats : \n%s", stat_info );
}

void test_delete( const struct mem_server *server, const char key[256] )
{
	int ret = mem_delete( server, key, 1 );
	if( ret == MEM_DELETED )
	{
		printf( "deleted key [%s] success.\n", key );
	}
	if( ret == MEM_NOT_FOUND )
	{
		printf( "the key [%s] is not found\n", key );
	}
}

void test_get( const struct mem_server *server )
{
	int c;
	struct item_data item;
	NEW_ITEM_DATA( item, "TestSet", 0 );
	
	c = mem_get( server, &item );
	if( c < 1 )
	{
		printf( "did not get the item [TestSet]\n" );
	}
	else
	{
		char data[256];
		c = evbuffer_remove( item._buffer, data, sizeof( data ) );
		data[c] = 0;

		printf( "get [TestSet] : %s\n", data );
	}

	DEL_ITEM_DATA( item );
}

void test_inc_dec( const struct mem_server *server )
{
	unsigned int new_value;
	int ret = mem_inc( server, "TestInc", 10, &new_value );
	if( ret == MEM_OK )
	{
		printf( "inc result : %u\n", new_value );
	}
	else if( ret == MEM_NOT_FOUND )
	{
		/* add */
		struct store_item item;
		item._exp_time = 0;
		NEW_ITEM_DATA( item._item, "TestInc", 0 );
		evbuffer_add( STORE_ITEM_BUF( item ), "102", 3 );
		mem_add( server, &item, 1 );
	}

	ret = mem_dec( server, "TestDec", 20, &new_value );
	if( ret == MEM_OK )
	{
		printf( "dec result : %u\n", new_value );
	}
	else if( ret == MEM_NOT_FOUND )
	{
		/* add */
		struct store_item item;
		item._exp_time = 0;
		NEW_ITEM_DATA( item._item, "TestDec", 0 );
		evbuffer_add( STORE_ITEM_BUF( item ), "201", 3 );
		mem_add( server, &item, 1 );
	}
}

int main( int argc, char *argv[] )
{
	struct mem_server server;
	int ret;

	if( argc < 3 )
	{
		fprintf( stderr, "Usage : %s -server_ip -server_port\n", argv[0] );
		return -1;
	}

	mem_set_error_handler( error_handler );

	{
		/* startup the winsock */
		WSADATA wd;
		WSAStartup( MAKEWORD( 2, 0 ), &wd );
	}

	ret = mem_connect( &server, argv[1], atoi( argv[2] ) );
	
	/* test */
	{
		test_stats( &server );
		test_set( &server );
		test_add( &server );
		test_replace( &server );
		/*test_delete( &server, "TestSet" );*/
		test_get( &server );

		test_inc_dec( &server );
	}

	mem_disconnect( &server );
	/* shutdown the winsock */
	WSACleanup();
	return 0;
}
