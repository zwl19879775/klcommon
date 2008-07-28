///
/// @file klnet_fdset.h
/// @author Kevin Lynx
/// @date 5.23.2008
///
#ifndef ___KLNET_FDSET_H_
#define ___KLNET_FDSET_H_

#include "klnet_config.h"
#include <stdlib.h>

namespace kl_net
{
	///
	/// wrap winsock.h::fd_set so that it's dependent on the value of FD_SETSIZE.
	///
	class Fdset
	{
	public:
		/// constructor
		Fdset( unsigned int max_fd = FD_SETSIZE )
		{
			_max_count = max_fd > FD_SETSIZE ? max_fd : FD_SETSIZE;
			_set = (fd_set*) malloc( sizeof( fd_set ) + sizeof( SOCKET ) * ( _max_count - FD_SETSIZE ) );
		}

		/// destructor
		~Fdset()
		{
			free( _set );
		}

		/// convert a fd_set type object.
		operator fd_set* ()
		{
			return _set;
		}
		
		/// 
		/// set the fd count
		///
		void set_size( unsigned int max_fd )
		{
			if( max_fd <= _max_count )
			{
				return ;
			}

			_max_count = max_fd;
			_set = (fd_set*) realloc( _set, sizeof( fd_set ) + sizeof( SOCKET ) * ( _max_count - FD_SETSIZE ) );
		}

		///
		/// add a fd into the set
		///
		bool add( SOCKET s )
		{
			// no space to add.
			if( _set->fd_count >= _max_count )
			{
				return false;
			}

			unsigned int i;
			for( i = 0; i < _set->fd_count; ++ i )
			{
				if( _set->fd_array[i] == s )
				{
					// already exist.
					return true;
				}
			}

			// otherwise, i==fd_count
			_set->fd_array[_set->fd_count++] = s;
			return true;
		}

		///
		/// remove a fd from the set
		///
		void remove( SOCKET s )
		{
			FD_CLR( s, _set );
		}

		///
		/// clear the set
		///
		void clear()
		{
			FD_ZERO( _set );
		}

		/// 
		/// check one fd is in this set?
		///
		bool is_set( SOCKET s ) const
		{
			return FD_ISSET( s, _set ) != 0;
		}

		/// return the max fd count
		int max_count() const
		{
			return _max_count;
		}

	private:
		/// fd_set
		::fd_set *_set;
		/// max fd count
		unsigned int _max_count;
	};
}

#endif // end ___KLNET_FDSET_H_