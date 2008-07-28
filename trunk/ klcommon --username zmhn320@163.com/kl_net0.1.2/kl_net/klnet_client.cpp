///
/// @file klnet_client.cpp
/// @author Kevin Lynx
/// @date 5.22.2008
///
#include "klnet_client.h"
#include "klnet_session.h"

namespace kl_net
{
	int Client::connect( const Address &remote_addr, const Address &local_addr )
	{
		assert( !is_connect() );
		
		// create the socket
		if( !Socket::create() )
		{
			if( _listener != 0 )
			{
				// notify the error
				_listener->onError( WSAGetLastError(), 0, "create socket failed" );
			}

			return SOCKET_ERROR;
		}
		
		// should bind address ?
		if( local_addr.get_ip() != "0.0.0.0" ||
			local_addr.get_port() != 0 )
		{
			if( !Socket::bind( local_addr ) )
			{
				if( _listener != 0 )
				{
					// notify the error
					_listener->onError( WSAGetLastError(), 0, "bind address failed" );
				}

				return SOCKET_ERROR;
			}
		}

		sockaddr_in address = remote_addr;
		int ret = ::connect( _socket, (sockaddr*) &address, sizeof( address ) );

		if( ret == 0 )
		{
			// success, create the session
			_session = new Session( system::create_id(), _socket, remote_addr );
		}
		else if( ret == SOCKET_ERROR )
		{
			if( _listener != 0 )
			{
				// notify the error
				_listener->onError( WSAGetLastError(), 0, "connect failed" );
			}
		}

		return ret;
	}
	
	void Client::disconnect()
	{
		assert( is_connect() );
		_session->close();
		delete _session;
		_session = 0;
	}

	int Client::query( const timeval &timeout )
	{
		assert( is_connect() );	

		_read_set.clear();
		_write_set.clear();

		_read_set.add( _socket );
		_write_set.add( _socket );
		int ret = ::select( 0, _read_set, _write_set, 0, &timeout );

		if( ret > 0 )
		{
			// query the socket status.
			if( _read_set.is_set( _socket ) )
			{
				// can read data
				int recv_bytes = _session->do_recv();

				if( recv_bytes <= 0 )
				{
					if( _listener != 0 )
					{
						_listener->onDisconnect( _session );
					}

					// disconnect
					return -2;
				}

				if( _listener != 0 )
				{
					_listener->onRead( recv_bytes, _session );
				}
			}
			if( _write_set.is_set( _socket ) )
			{
				// can write data
				int send_bytes = _session->do_send();

				if( send_bytes > 0 && _listener != 0 )
				{
					_listener->onWrite( send_bytes, _session );
				}
			}
		}

		return ret;
	}
}