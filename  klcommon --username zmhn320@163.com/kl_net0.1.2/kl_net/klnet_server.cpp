///
/// @file klnet_server.cpp
/// @author Kevin Lynx
/// @date 5.23.2008
///
#include "klnet_server.h"
#include "klnet_session.h"
#include <algorithm>

namespace kl_net
{
	Server::Server()
	{
		_max_fd = 0;
		_listener = 0;
	}

	Server::~Server()
	{
	}

	bool Server::startup( int max_fd, const Address &local_addr, int backlog )
	{
		// create the socket
		if( !Socket::create() )
		{
			if( _listener != 0 )
			{
				_listener->onError( WSAGetLastError(), 0, "create socket failed" );
			}

			return false;
		}

		// bind address
		if( !Socket::bind( local_addr ) )
		{
			if( _listener != 0 )
			{
				_listener->onError( WSAGetLastError(), 0, "bind address failed" );
			}
			return false;
		}
		
		_max_fd = max_fd;

		// set the reuse address option.
		char reuseaddr = 1;
		::setsockopt( _socket, SOL_SOCKET, SO_REUSEADDR, &reuseaddr, sizeof( reuseaddr ) ); 

		// listen
		listen( _socket, backlog );

		return true;
	}

	void Server::shutdown()
	{
		close_all();
		Socket::release();
	}

	int Server::poll( const timeval &timeout )
	{
		setup_fdset();
		int ret = ::select( 0, _read_set, _write_set, 0, &timeout );

		if( ret > 0 )
		{
			// new connection comes ?
			if( _read_set.is_set( _socket ) )
			{
				new_session();
			}

			// poll every session
			for( SessionList::iterator it = _sessions.begin(); it != _sessions.end(); ++ it )
			{
				Session *session = it->second;
				SOCKET s = session->query_socket();

				if( _read_set.is_set( s ) )
				{
					// can read
					int read_bytes = session->do_recv();
					if( read_bytes <= 0 )
					{
						// set the close flag
						session->_to_close = true;
					}
					else 
					// notify the listener
					if( _listener != 0 )
					{
						_listener->onRead( read_bytes, session );
					}
				}

				if( _write_set.is_set( s ) )
				{
					// can write
					int send_bytes = session->do_send();

					// notify the listener
					if( send_bytes > 0 && _listener != 0 )
					{
						_listener->onWrite( send_bytes, session );
					}
				}
			} // for

			// close these sessions
			for( SessionList::iterator it = _sessions.begin(); it != _sessions.end();  )
			{
				Session *session = it->second;
				if( session->_to_close )
				{
					// notify the listener
					if( _listener != 0 )
					{
						_listener->onDisconnect( session );
					}

					session->close();
					delete session;
					it = _sessions.erase( it );
				}
				else
				{
					++ it;
				}
			}

		} // if

		return ret;
	}

	void Server::broadcast( const char *buf, int len )
	{
		struct Pred
		{
			Pred( const char *buf, int len ) : _buf( buf ), _len( len ) {}
			void operator() ( const std::pair<system::SystemIDType, Session*> &session_pair )
			{
				session_pair.second->send( _buf, _len );
			}
			const char *_buf;
			int _len;
		};

		std::for_each( _sessions.begin(), _sessions.end(), Pred( buf, len ) );
	}

	Session *Server::get_session( system::SystemIDType id )
	{
		SessionList::iterator it = _sessions.find( id );
		
		return it == _sessions.end() ? 0 : it->second ;
	}

	Session *Server::get_session( const Address &addr )
	{
		// preidcator used for std::find
		struct Pred
		{
			Pred( const Address &addr ) : _addr( addr ) { }
			bool operator() ( const std::pair<system::SystemIDType, Session*> &session_pair )
			{
				return session_pair.second->address() == _addr;
			}

			const Address &_addr;
		};

		SessionList::iterator it = std::find_if( _sessions.begin(), _sessions.end(), Pred( addr ) );

		return it == _sessions.end() ? 0 : it->second;
	}
	
	void Server::close_session( system::SystemIDType id )
	{
		Session *session = get_session( id );
		assert( session != 0 );
		session->_to_close = true;;
	}

	void Server::close_all()
	{
		for( SessionList::iterator it = _sessions.begin(); it != _sessions.end(); ++ it )
		{
			it->second->close();
			delete it->second ;
		}

		_sessions.clear();
	}

	void Server::new_session()
	{
		sockaddr_in addr;
		int len = sizeof( addr );

		SOCKET s = ::accept( _socket, (sockaddr*) &addr, &len );
		if( s == INVALID_SOCKET )
		{
			// todo : error checking
		}
		
		// whether should accept ?
		if( (int)_sessions.size() >= _max_fd )
		{
			closesocket( s );
			return ;
		}

		Session *session = new Session( system::create_id(), s, addr );
		_sessions[session->id()] = session;

		// notify the listener
		if( _listener != 0 )
		{
			_listener->onConnect( session );
		}
	}

	void Server::remove_session( system::SystemIDType id )
	{
		Session *session = get_session( id );
		assert( session != 0 );

		session->close();
		_sessions.erase( id );
	}

	void Server::setup_fdset()
	{
		_read_set.clear();
		_write_set.clear();

		_read_set.add( _socket );
		_write_set.add( _socket );

		for( SessionList::iterator it = _sessions.begin(); it != _sessions.end(); ++ it )
		{
			Session *session = it->second;
			_read_set.add( session->query_socket() );
			_write_set.add( session->query_socket() );
		}
	}
}