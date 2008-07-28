///
///
///
#include "klnet_system.h"
#include "klnet_server.h"
#include "klnet_session.h"

#pragma comment( lib, "ws2_32.lib" )
#ifdef _DEBUG
#pragma comment( lib, "klnet_d.lib" )
#else
#pragma comment( lib, "klnet.lib" )
#endif


class Listener : public kl_net::ServerListener 
{
public:
	void onConnect( kl_net::Session *session )
	{
		const kl_net::Address &addr = session->address();
		printf( "client %s-%d connected!\n", addr.get_ip().c_str(), addr.get_port() );
	}

	void onDisconnect( kl_net::Session *session )
	{
		const kl_net::Address &addr = session->address();
		printf( "client %s-%d disconnected!\n", addr.get_ip().c_str(), addr.get_port() );
	}

	void onRead( int bytes, kl_net::Session *session )
	{
		char pak[256];

		const kl_net::Address &addr = session->address();
		printf( "recv %d bytes from client %s-%d.\n", bytes, addr.get_ip().c_str(), addr.get_port() );

		// get data from the queue but not the remote machine.
		session->recv( pak, sizeof( pak ) );
		printf( "#### data : %s ####\n", pak );

		// send the data back.
		session->send( pak, bytes );
	}
	
	void onWrite( int bytes, kl_net::Session *session )
	{
		const kl_net::Address &addr = session->address();
		printf( "send %d bytes to client %s-%d.\n", bytes, addr.get_ip().c_str(), addr.get_port() );
	}
};

///
/// exit flag
///
volatile static bool is_exit = false;

///
/// ctrl+c routine
///
BOOL WINAPI Handler( DWORD ctrl_event )
{
	switch( ctrl_event )
	{
	case CTRL_CLOSE_EVENT:
	case CTRL_C_EVENT:
		is_exit = true;
		return TRUE;
	}

	return FALSE;
}


int main( int argc, char **argv )
{
	if( argc != 3 )
	{
		fprintf( stderr, "usage : %s local_ip listen_port.\n", argv[0] );
		return -1;
	}

	if( !SetConsoleCtrlHandler( Handler, TRUE ) )
	{
		fprintf( stderr, "error setting event handler.\n" );
		return -1;
	}

	kl_net::system::startup();

	kl_net::Server server;
	Listener listener;

	printf( "server startup.\n" );
	server.startup( 500, kl_net::Address( argv[1], atoi( argv[2] ) ), 10 );
	server.set_listener( &listener );

	while( !is_exit )
	{
		timeval timeout = { 1, 0 };
		server.poll( timeout );
	}
	
	printf( "server shutdown.\n" );
	server.shutdown();
	kl_net::system::shutdown();
	return 0;
}