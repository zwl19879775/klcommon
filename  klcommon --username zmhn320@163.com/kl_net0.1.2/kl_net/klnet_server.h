///
/// @file klnet_server.h
/// @author Kevin Lynx
/// @date 5.23.2008
///
#ifndef ___KLNET_SERVER_H_
#define ___KLNET_SERVER_H_

#include "klnet_config.h"
#include "klnet_socket.h"
#include "klnet_fdset.h"
#include "klnet_system.h"
#include <map>

namespace kl_net
{
	class Session;
	class ServerListener;

	///
	/// You can use this class to set up a TCP server.
	///
	class Server : public Private::Socket
	{
	public:
		/// session container
		typedef std::map<system::SystemIDType, Session*> SessionList;

	public:
		/// constructor
		Server();

		/// destructor
		~Server();
		
		///
		/// startup as a server.start to listen.
		/// @param max_fd the number of sessions this server can hold.
		bool startup( int max_fd, const Address &local_addr, int backlog );
		
		/// shutdown, disconnect all the sessions and free socket resources
		void shutdown();

		///
		/// poll the socket status, this function will accept new connection and poll every
		/// session's status.
		///
		int poll( const timeval &timeout );

		///
		/// broadcast
		///
		void broadcast( const char *buf, int len );

		///
		/// get a session so that you can use this session to send/recv data.
		///
		Session *get_session( system::SystemIDType id );
		Session *get_session( const Address &addr );

		/// get the number of sessions.
		unsigned int session_count() const 
		{
			return (unsigned int )_sessions.size();
		}

		///
		/// disconnect one session.
		///
		void close_session( system::SystemIDType id );
		
		///
		/// close all the sessions.
		///
		void close_all();

		///
		/// set the event listener
		///
		void set_listener( ServerListener *listener )
		{
			_listener = listener;
		}

	private:
		/// create a new session and save the session in the list.
		void new_session();

		/// remove a session, close it and remove it from the list.
		void remove_session( system::SystemIDType id );

		/// setup fd set
		void setup_fdset();

	private:
		/// session list
		SessionList _sessions;
		/// read fd set
		Fdset _read_set;
		/// write fd set
		Fdset _write_set;
		/// max fd count
		int _max_fd;
		/// the event listener
		ServerListener *_listener;
	};

	///
	/// ServerListener, listen the server events like some clients connected or disconnected.
	///
	class ServerListener
	{
	public:
		///
		/// a new client connected. 
		/// @return false to disconnect the session.
		virtual void onConnect( Session *session ) { }

		///
		/// a client disconnected
		///
		virtual void onDisconnect( Session *session ) { }

		///
		/// readed some data
		///
		/// @return false to disconnect the session.
		virtual void onRead( int bytes, Session *session ) {  }

		///
		/// sended some data
		///
		virtual void onWrite( int bytes, Session *session ) { }

		///
		/// called when some error occurs.
		///
		virtual void onError( int error, Session *session, const char *desc = "no desc" ) {}
	};

}

#endif // end ___KLNET_SERVER_H_