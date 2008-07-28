///
/// test kl_net client
///
#include "klnet_system.h"
#include "klnet_client.h"
#include "klnet_session.h"

#ifdef _DEBUG
#pragma comment( lib, "klnet_d.lib" )
#else
#pragma comment( lib, "klnet.lib" )
#endif

#pragma comment( lib, "ws2_32.lib" )


int main( int argc, char **argv )
{
	if( argc != 4 )
	{
		printf( "usage : %s server_ip server_port packet_count.\n", argv[0] );
		exit( -1 );
	}

	kl_net::Client client;
	kl_net::system::startup();

	printf( "connecting to the server %s-%d...\n", argv[1], atoi( argv[2] ) );
	client.connect( kl_net::Address( argv[1], atoi( argv[2] ) ) ) ;

	int pack_count = atoi( argv[3] );
	for( int i = 0; i < pack_count; ++ i )
	{
		char pak[256];
		sprintf( pak, "packet %d", i + 1 );
		client.session()->send( pak, strlen( pak ) + 1 );

		timeval timeout = { 1, 0 };
		client.query( timeout );

		Sleep( 100 );
		
		client.query( timeout );
		int bytes = client.session()->recv( pak, sizeof( pak ));
		if( bytes > 0 )
		{
			printf( "recv : %s[%d bytes]\n", pak, bytes );
		}
	}

	client.disconnect();
	kl_net::system::shutdown();
	return 0;
}