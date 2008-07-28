///
/// @file klnet_session.h
/// @author Kevin Lynx
/// @date 5.23.2008
///
#ifndef ___KLNET_SESSION_H_
#define ___KLNET_SESSION_H_

#include "klnet_config.h"
#include "klnet_buffer.h"
#include "klnet_address.h"
#include "klnet_system.h"

namespace kl_net
{
	///
	/// Session class represents a connection between server and client.
	/// You cannot create this class yourself.The server/client will create it when
	/// some connection is established..
	///
	class Session
	{
	public:
		/// 
		/// receive data, this function only return the queued data to you.
		/// @return the number of bytes copyed
		///
		int recv( char *buf, int len );

		///
		/// send data, this function only copy the data you provide to the queue,
		/// and send it later.
		/// @return the number of bytes it copyed
		///
		int send( const char *buf, int len );

		/// get the peer(remote) address.
		const Address &address() const
		{
			return _addr;
		}

		/// query the socket handle
		SOCKET query_socket() const
		{
			return _socket;
		}

		/// query the id
		system::SystemIDType id() const 
		{
			return _id;
		}

	private:
		/// so that only server can operate the Session.
		friend class Server;
		friend class Client;

		///
		/// hide constructor
		/// @param addr the remote address
		Session( system::SystemIDType id, SOCKET s, const Address &addr, int buf_size = Buffer::SIZE ) : 
			_id( id ), _addr( addr ), _sendbuf( buf_size ), _recvbuf( buf_size )
		{
			assert( s != INVALID_SOCKET );
			_socket = s;
			_to_close = false;
		}

		/// hide the destructor so that only the friend class can delete session object.
		~Session()
		{
		}

		/// 
		/// do_recv, recv from TCP stack.This function will recv data and queue the data.
		/// @return the number bytes it received.
		int do_recv();

		///
		/// do_send, send the queued data in the buffer to the peer.
		/// @return the number bytes it send.
		int do_send();

		///
		/// close the session, it means disconnect from the peer.
		///
		void close()
		{
			assert( _socket != INVALID_SOCKET );
			closesocket( _socket );
			_socket = INVALID_SOCKET;
		}

	private:
		/// socket
		SOCKET _socket;
		/// buffer to queue the send data.
		Buffer _sendbuf;
		/// buffer to queue the recv data
		Buffer _recvbuf;
		/// the peer(remote) address
		Address _addr;
		/// the session id
		system::SystemIDType _id;
		/// the close flag.
		bool _to_close;
	};
}

#endif // end ___KLNET_SESSION_H_