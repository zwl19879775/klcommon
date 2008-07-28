///
/// chatserver based on kl_net
///
#include <stdio.h>
#include <string>
#include <map>
#include "klnet_server.h"
#include "klnet_session.h"

#pragma comment( lib, "ws2_32.lib" )
#ifdef _DEBUG
#pragma comment( lib, "klnet_d.lib" )
#else
#pragma comment( lib, "klnet.lib" )
#endif

const char *WEL_MSG = "Welcome to kl_net chat server['quit' to quit]!\r\n";


class Listener : public kl_net::ServerListener 
{
public:
	///
	typedef std::map<kl_net::system::SystemIDType, kl_net::Buffer*> SessionList;
public:
	Listener( kl_net::Server *server ) : _server( server )
	{
	}

	~Listener()
	{
		for( SessionList::iterator it = _sessions.begin(); it != _sessions.end(); ++ it )
		{
			delete it->second ;
		}
	}

	void onConnect( kl_net::Session *session )
	{
		const kl_net::Address &addr = session->address();
		printf( "%s-%d connected.\n", addr.get_ip().c_str(), addr.get_port() );

		session->send( WEL_MSG, (int)strlen( WEL_MSG ) );

		//
		_sessions[session->id()] = new kl_net::Buffer();;

		// notify message
		char msg[1024];
		sprintf( msg, "%s-%d get online!\r\n", addr.get_ip().c_str(), addr.get_port() );
		_server->broadcast( msg, strlen( msg ) );
	}

	void onDisconnect( kl_net::Session *session )
	{
		const kl_net::Address &addr = session->address();
		printf( "%s-%d disconnected.\n", addr.get_ip().c_str(), addr.get_port() );

		char msg[1024];
		sprintf( msg, "%s-%d offline.\r\n", addr.get_ip().c_str(), addr.get_port() );
		_server->broadcast( msg, strlen( msg ) );
	}

	void onWrite( int bytes, kl_net::Session *session )
	{
	}

	void onRead( int bytes, kl_net::Session *session )
	{
		SessionList::iterator it = _sessions.find( session->id() );
		if( it == _sessions.end() ) return ;

		kl_net::Buffer *buf = it->second ;
		
		char pak[1024];
		session->recv( pak, sizeof( pak ) );

		buf->put( pak, bytes );
		
		// find the end line character
		std::string s = buf->raw();
		std::string::size_type pos = s.find_first_of( "\r\n" );
		
		if( pos != std::string::npos )
		{
			// find it.
			int cpbytes = buf->get( pak, sizeof( pak ) );
			buf->clear();
			pak[cpbytes] = 0;
			char msg[1024];
			const kl_net::Address &addr = session->address();

			// check command
			if( stricmp( pak, "quit\r\n" ) == 0 )
			{
				// disconnect
				_server->close_session( session->id() );
				return ;
			}

			sprintf( msg, "%s-%d said : %s", addr.get_ip().c_str(), addr.get_port(), pak );

			// broadcast the message
			_server->broadcast( msg, strlen( msg ) );
		}
	}

	void onError( int error, kl_net::Session *session, const char *desc )
	{
		printf( "socket error : %d [%s]\n", error, desc );
	}

private:
	SessionList _sessions;
	kl_net::Server *_server;
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
	Listener listener( &server );

	printf( "server startup.\n" );
	server.startup( 500, kl_net::Address( argv[1], atoi( argv[2] ) ), 10 );
	server.set_listener( &listener );

	while( !is_exit )
	{
		timeval timeout = { 1, 0 };
		server.poll( timeout );

		Sleep( 10 );
	}
	
	printf( "server shutdown.\n" );
	server.shutdown();
	kl_net::system::shutdown();
	return 0;
}
