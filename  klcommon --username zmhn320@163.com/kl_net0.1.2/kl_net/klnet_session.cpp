///
/// @file klnet_session.cpp
/// @author Kevin Lynx
/// @date 5.23.2008
///
#include "klnet_session.h"

namespace kl_net
{
	int Session::recv( char *buf, int len )
	{
		return _recvbuf.get( buf, len );
	}

	int Session::send( const char *buf, int len )
	{
		return _sendbuf.put( buf, len );
	}

	int Session::do_recv()
	{
		// note : here i use a trick about my buffer class.
		// it's not safe, but i think it's effective.
		char *buf = _recvbuf.free_space();
		int len = _recvbuf.free_size(); 

		assert( buf != 0 );
		
		// if the free memory is 0, realloc the memory
		if( len == 0 )
		{
			_recvbuf.realloc( Buffer::SIZE );
			buf = _recvbuf.free_space();
			len = _recvbuf.free_size();
		}

		// do recv
		int ret = ::recv( _socket, buf, len, 0 );

		// cool, if ::recv received some data, the data has already been in the Buffer object.:D
		// only change the buffer pointer
		if( ret > 0 )
		{
			_recvbuf.move_pos( ret );
		}

		return ret;
	}

	int Session::do_send()
	{
		int len = _sendbuf.size();
		// is there any data to send ?
		if( len <= 0 )
		{
			return 0;
		}

		const char *buf = _sendbuf.raw();
		int ret = ::send( _socket, buf, len, 0 );

		// remove the data which has been sent.
		if( ret > 0 )
		{
			_sendbuf.move_buf( ret );
		}

		return ret;
	}
}