///
/// @file klnet_buffer.h
/// @author Kevin Lynx
/// @date 5.23.2008
///
#ifndef ___KLNET_BUFFER_H_
#define ___KLNET_BUFFER_H_

#include "klnet_config.h"
#include <stdlib.h>

namespace kl_net
{
	///
	/// Buffer class allocate memory and restore data.It's a queue-like class.
	/// I donot use some STL stuff for efficiency reason.:D
	/// @todo i suppose i should implement a memory pool to make the buffer
	/// more effective.
	///
	class Buffer
	{
	public:
		/// the default initial buffer max size.
		enum
		{
			SIZE = 1024
		};

	public:
		/// constructor
		Buffer() 
		{
			_cur_pos = 0;
			allocate( SIZE );
		}

		/// specify the initial buffer size.
		Buffer( int init_size )
		{
			_cur_pos = 0;
			allocate( init_size );
		}

		/// copy constructor will copy the data this buffer holds.
		Buffer( const Buffer &buf )
		{
			_cur_pos = buf._cur_pos ;
			allocate( buf._max_size );
			memcpy( _buf, buf.raw(), buf.size() );
		}

		/// destructor, will free memory it allocated.
		~Buffer()
		{
			free();
		}

		/// assignment operator will copy the data the buffer holds.
		const Buffer &operator = ( const Buffer &buf )
		{
			_cur_pos = buf._cur_pos;
			allocate( buf._max_size );
			memcpy( _buf, buf.raw(), buf.size() );

			return *this;
		}

		/// 
		/// realloc the buffer
		///
		int realloc( int add_len )
		{
			int new_size = add_len >= SIZE ? add_len : SIZE;
			_buf = (char*) ::realloc( _buf, new_size );
			_max_size += new_size;

			return _max_size;
		}

		///
		/// put data into the buffer
		/// @return the number bytes it copyed
		///
		int put( const char *buf, int len )
		{
			// the memory is enough ?
			if( _cur_pos + len > _max_size )
			{
				// not enough, realloc
				realloc( len );
			}

			// so the memory is enough, copy the data
			memcpy( _buf + _cur_pos, buf, len );
			_cur_pos += len;
			return len;
		}

		///
		/// get data from the buffer, and it will remove the data you got.
		/// @return the number bytes it copyed.
		int get( char *buf, int len )
		{
			// calculate the number of bytes to copyed
			int copy_bytes = _cur_pos < len ? _cur_pos : len;
			// no data
			if( copy_bytes == 0 )
			{
				return 0;
			}

			memcpy( buf, _buf, copy_bytes );
			// all the data is copyed ?
			if( copy_bytes != _cur_pos )
			{
				// move the data this buffer holds to the front of the memory
				memmove( _buf, _buf + copy_bytes, _cur_pos - copy_bytes );
			}

			_cur_pos -= copy_bytes;

			return copy_bytes;
		}

		///
		/// clear all the data, but not free the memory.
		///
		void clear()
		{
			_cur_pos = 0;
			memset( _buf, 0, _max_size );
		}

		///
		/// return the data size of this buffer
		///
		int size() const
		{
			return _cur_pos;
		}

		///
		/// return the free space size in bytes
		int free_size() const
		{
			return _max_size - _cur_pos;
		}

		///
		/// get the raw pointer.
		/// this function return the raw data pointer to you to make everything more
		/// effective.I suggest you to be more smart to manage the memory.:D
		///
		const char *raw() const
		{
			return _buf;
		}

		///
		/// get the free space pointer so that you can write the memory directly.
		/// pay attention to the free_size .
		///
		char *free_space() 
		{
			return _buf + _cur_pos;
		}

		///
		/// move the write position. 
		///
		void move_pos( int add_bytes )
		{
			_cur_pos += add_bytes;
		}

		///
		/// move the raw buffer.
		///
		void move_buf( int move_bytes )
		{
			assert( move_bytes <= _cur_pos );
			
			if( move_bytes < _cur_pos )
			{
				memmove( _buf, _buf + move_bytes, _cur_pos - move_bytes );
			}
			_cur_pos -= move_bytes;
		}

	private:
		/// helper function to allocate memory
		void allocate( int size )
		{
			assert( size > 0 );
			_max_size = size;
			_buf = (char*) malloc( _max_size );
		}
	
		/// helper function to free memory
		void free()
		{
			assert( _buf != 0 );
			::free( _buf );
			_max_size = 0;
		}

	private:
		/// the buffer pointer
		char *_buf;
		/// the buffer max size
		int _max_size;
		/// the current write position on the buffer.
		int _cur_pos;
	};
}

#endif // end ___KLNET_BUFFER_H_