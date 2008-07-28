///
/// @file klnet_address.h
/// @author Kevin Lynx
/// @date 5.22.2008
///
#ifndef ___KLNET_ADDRESS_H_
#define ___KLNET_ADDRESS_H_

#include "klnet_config.h"
#include <string>

namespace kl_net
{
	///
	/// wrap the ipv4 address
	///
	class Address
	{
	public:
		/// constructor
		Address()
		{
			reset();
		}

		/// constructor
		Address( const std::string &ip, unsigned short port )
		{
			reset();
			set_ip( ip );
			set_port( port );
		}

		/// construct from sockaddr_in structure
		Address( const sockaddr_in &addr )
		{
			_addr = addr;
		}

		/// destructor
		~Address()
		{
		}

		/// convert to sockaddr_in
		operator sockaddr_in() const
		{
			return _addr;
		}

		/// = sockaddr_in
		const Address &operator = ( const sockaddr_in &addr )
		{
			_addr = addr;
			return *this;
		}

		/// reset the address to invalid.
		void reset()
		{
			memset( &_addr, 0, sizeof( _addr ) );
			_addr.sin_family = AF_INET;
			_addr.sin_addr.s_addr = INADDR_NONE;
		}

		/// get the ip
		std::string get_ip() const
		{
			char *ip_desc = inet_ntoa( _addr.sin_addr );
			return ip_desc != 0 ? ip_desc : "0.0.0.0";
		}

		/// get the port
		unsigned short get_port() const
		{
			return ntohs( _addr.sin_port );
		}

		/// set the ip
		void set_ip( const std::string &ip )
		{
			_addr.sin_addr.s_addr = inet_addr( ip.c_str() );
		}
	
		/// set the port
		void set_port( unsigned short port )
		{
			_addr.sin_port = htons( port );
		}

		/// is the address is valid
		bool is_valid() const
		{
			return _addr.sin_addr.s_addr != INADDR_NONE;
		}

	private:
		/// sockaddr_in address
		sockaddr_in _addr;
	};

	/// whether the address is equal.
	inline bool operator == ( const Address &addr1, const Address &addr2 )
	{
		sockaddr_in _addr1 = addr1;
		sockaddr_in _addr2 = addr2;

		return _addr1.sin_family == _addr2.sin_family &&
			_addr1.sin_addr.s_addr == _addr2.sin_addr.s_addr &&
			_addr1.sin_port == _addr2.sin_port;
	}
}

#endif // end ___KLNET_ADDRESS_H_