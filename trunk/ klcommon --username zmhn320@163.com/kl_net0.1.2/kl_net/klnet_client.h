///
/// @file klnet_client.h
/// @author Kevin Lynx
/// @date 5.22.2008
///
#ifndef ___KLNET_CLIENT_H_
#define ___KLNET_CLIENT_H_

#include "klnet_config.h"
#include "klnet_socket.h"
#include "klnet_address.h"
#include "klnet_fdset.h"
#include "klnet_system.h"

namespace kl_net
{
	class Session;
	class ClientListener;

	///
	/// You can use Client to connect some servers.
	///
	class Client : public Private::Socket
	{
	public:
		/// constructor
		Client() : _session( 0 )
		{
			_listener = 0;
		}

		/// destructor
		~Client()
		{
			if( is_connect() )
			{
				disconnect();
			}
		}

		///
		/// set the event listener
		///
		void set_listener( ClientListener *listener )
		{
			_listener = listener;
		}

		/// 
		/// connect to the server, if connectes ok, it will create a session.
		///
		int connect( const Address &remote_addr, const Address &local_addr = Address( "0.0.0.0", 0 ) );

		///
		/// disconnect to the server.
		///
		void disconnect();	

		/// 
		/// whether connected to the server
		///
		bool is_connect() const
		{
			return _session != 0;
		}

		///
		/// query the session, sometimes you should call this function in your main loop.
		/// @return the query result.( -1 : SOCKET_ERROR, 0 : time expired, -2 : the peer disconnect)
		int query( const timeval &timeout );

		///
		/// retrieve the session, so that you can read/write data from the server.
		///
		Session *session() 
		{
			return _session;
		}

	private:
		/// the session
		Session *_session;
		/// read fdset
		Fdset _read_set;
		/// write fdset
		Fdset _write_set;
		/// event listener
		ClientListener *_listener;
	};

	///
	/// ClientListener, will be notified something by the client.
	///
	class ClientListener
	{
	public:
		///
		/// called after disconnect from the server.
		///
		virtual void onDisconnect( Session *session ) {}

		///
		/// called after read some data.
		///
		virtual void onRead( int bytes, Session *session ) {}

		///
		/// called after write some data.
		///
		virtual void onWrite( int bytes, Session *session ) {}

		///
		/// called when some error occured.
		///
		virtual void onError( int error, Session *session, const char *desc = "no description" ) {}
	};
}

#endif // end ___KLNET_CLIENT_H_