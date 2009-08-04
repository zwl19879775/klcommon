///
/// @file kl_bytecoder.h
/// @author Kevin Lynx
/// @date 6.4.2008
///
#ifndef ___KL_BYTECODER_H_
#define ___KL_BYTECODER_H_

#include <assert.h>
#include "kl_compiler_cfg.h"
#include <string>

KL_COMMON_NAMESPACE_BEGIN

///
/// byte_coder is tool to code any kinds of c++ types into bytes.
///
template <typename _Container>
class byte_coder
{
public:
	/// buffer type
	typedef _Container buffer_type;
public:
	/// constructor
	byte_coder()
	{
	}

	/// destructor, i write it virtual so that you can inherite this class to add more 
	/// functions if you need.
	virtual ~byte_coder()
	{
	}

	/// construct from byte buffer
	byte_coder( const buffer_type &buf ) : _buf( buf )
	{
	}

	/// copy constructor
	byte_coder( const byte_coder &bc ) : _buf( bc._buf )
	{
	}

	/// operator =
	const byte_coder &operator = ( const byte_coder &bc )
	{
		_buf = bc._buf;
		return *this;
	}

	/// get the buffer size in bytes.
	std::size_t size() const
	{
		return _buf.size();
	}

	/// add data in the buffer
	template <typename _Tp>
	void add( const _Tp &data )
	{
		for( int i = 0; i < sizeof( _Tp ); ++ i )
		{
			_buf.push_back( ( (unsigned char*)(&data) )[i] );
		}
	}

	/// add a string in the buffer.
	void add( const std::string &str )
	{
		for( std::string::const_iterator it = str.begin(); it != str.end(); ++ it )
		{
			add( *it );
		}
		// append '\0'
		add( (char) 0 );
	}

	/// add a string in the buffer.
	void add( const char *str )
	{
		add( std::string( str ) );
	}

	/// add raw buffer .
	void add( const void *buf, std::size_t size )
	{
		assert( buf != 0 );
		const char *cbuf = (const char*) buf;
		for( std::size_t i = 0; i < size; ++ i )
		{
			add( cbuf[i] );
		}
	}

	/// add a byte_coder, it will copy the source byte_coder content.
	template <typename _Tp>
	void add( byte_coder<_Tp> &other )
	{
		_buf.insert( _buf.end(), other._buf.begin(), other._buf.end() );
	}

	/// get data, if no data to return, it will return Tp().
	template <typename _Tp>
	_Tp get( _Tp *buf = 0 )
	{
		if( size() < sizeof( _Tp ) )
		{
			return _Tp();
		}
		
		_Tp t( *(_Tp*)( &_buf[0] ) );
		_buf.erase( _buf.begin(), _buf.begin() + sizeof( _Tp ) );

		if( buf != 0 )
		{
			*buf = t;
		}

		return t;
	}

	/// get string in the buffer.
	std::string &get( std::string &str )
	{
		str.clear();
		for( buffer_type::iterator it = _buf.begin(); (char)(*it) != '\0' && it != _buf.end(); ++ it )
		{
			str.push_back( *it );
		}

		_buf.erase( _buf.begin(), _buf.begin() + str.length() + 1 );
		return str;
	}

	/// get raw data.
	void *get( void *buf, std::size_t size )
	{
		std::size_t buf_size = this->size();
		std::size_t copy_size = buf_size < size ? buf_size : size;
		if( copy_size == 0 )
		{
			return buf;
		}
		memcpy( buf, &_buf[0], copy_size );
		_buf.erase( _buf.begin(), _buf.begin() + copy_size );
		return buf;
	}

protected:
	/// data buffer
	buffer_type _buf;
};


KL_COMMON_NAMESPACE_END

#endif // end ___KL_BYTECODER_H_