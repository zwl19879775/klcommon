///
/// @file klnet_socket.h
/// @author Kevin Lynx
/// @date 5.22.2008
///
#ifndef ___KLNET_SOCKET_H_
#define ___KLNET_SOCKET_H_

#include "klnet_config.h"
#include "klnet_address.h"

namespace kl_net
{
	/// 
	/// everything in the private namespace is only for the library, not for you.
	///
	namespace Private
	{
		///
		/// socket, wrap the basic socket behavior.
		///
		class Socket
		{
		public:
			/// constructor
			Socket()
			{
				_socket = INVALID_SOCKET;
			}

			/// destructor
			~Socket()
			{
			}

			/// 
			/// update local address
			///
			bool update_addr()
			{
				assert( is_valid() );

				sockaddr_in addr ;
				int len = sizeof( addr );

				int ret = ::getsockname( _socket, (sockaddr*) &addr, &len );
				if( ret != SOCKET_ERROR )
				{
					_local_addr = addr;
					return true;
				}

				return false;
			}

			///
			/// get local address
			///
			Address get_local_addr()
			{
				return _local_addr;
			}

		protected:
			///
			/// create the socket
			///
			bool create()
			{
				_socket = ::socket( AF_INET, SOCK_STREAM, 0 );
				return _socket != INVALID_SOCKET;
			}

			///
			/// release the socket, must be called manually.
			///
			void release()
			{
				assert( is_valid() );
				closesocket( _socket );
				_socket = INVALID_SOCKET;
			}

			/// 
			/// check whether the socket is valid
			///
			bool is_valid()
			{
				return _socket != INVALID_SOCKET;
			}
		
			///
			/// bind local address ( ip and port )
			///
			bool bind( const Address &address )
			{
				assert( is_valid() );

				sockaddr_in addr = address;
				int len = sizeof( addr );
				return ::bind( _socket, (sockaddr*) &addr, len ) != SOCKET_ERROR;				
			}

			bool bind( const std::string &ip, unsigned short port )
			{
				return bind( Address( ip, port ) );
			}

		protected:
			/// the kernel socket handle
			SOCKET _socket;
			/// the local address
			Address _local_addr;
		};
	}
}

#endif // end ___KLNET_SOCKET_H_