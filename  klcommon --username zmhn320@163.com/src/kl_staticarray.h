///
/// @file kl_staticarray.h
/// @author Kevin Lynx
/// @date 6.5.2008
///
#ifndef ___KL_STATICARRAY_H_
#define ___KL_STATICARRAY_H_

#include "kl_compiler_cfg.h"
#include <exception>

KL_COMMON_NAMESPACE_BEGIN


///
/// static array iterator
///
template <typename _Tp>
class sa_iterator : public std::iterator<std::random_access_iterator_tag, _Tp>
{
public:
	/// self type
	typedef sa_iterator<_Tp> self_type;
public:
	/// constructor
	sa_iterator()
	{
		_ptr = 0;
	}

	/// construct from a pointer.
	sa_iterator( pointer ptr ) : _ptr( ptr )
	{
	}

	/// return the reference 
	reference operator*() const
	{
		return *_ptr;
	}

	/// return the pointer
	pointer operator->() const
	{
		return _ptr;
	}

	/// operator ++ preincrement
	self_type &operator++()
	{
		++_ptr;
		return *this;
	}

	/// operator ++ postincrement
	self_type &operator++(int)
	{
		pointer ptr = _ptr;
		++_ptr;
		return *this;
	}

	/// operator -- preincrement
	self_type &operator--()
	{
		--_ptr;
		return *this;
	}

	/// operator -- postincrement
	self_type &operator--(int)
	{
		pointer ptr = _ptr;
		--_ptr;
		return *this;
	}

	/// operator +
	self_type operator+( difference_type off ) const
	{
		return self_type( _ptr + off );
	}

	/// operator +=
	self_type &operator+=( difference_type off ) 
	{
		_ptr += off;
		return *this;
	}

	/// operator -
	self_type operator-( difference_type off ) const
	{
		return self_type( _ptr - off );
	}

	/// operator -=
	self_type &operator-=( difference_type off )
	{
		_ptr -= off;
		return *this;
	}

	/// operator ==
	bool operator==( const sa_iterator &right ) const
	{
		return this->_ptr == right._ptr ;
	}

	/// operator !=
	bool operator!=( const sa_iterator &right ) const
	{
		return !operator==( right );
	}

	/// operator -
	difference_type operator-( const self_type &right ) const
	{
		return _ptr - right._ptr ;
	}

	/// add more operator...

public:
	/// raw pointer
	pointer _ptr;
};

///
/// static array uses a unchangeable space to store data.
/// TODO : make this container more compatible to STL.
///
/// @param _Tp the object type.
/// @param size the object number this array can holds.
///
template <typename _Tp, std::size_t size = 1024>
class static_array
{
public:
	/// the data type
	typedef _Tp value_type;
	/// iterator type
	typedef sa_iterator<_Tp> iterator;

	enum
	{
		/// the container size
		max_size = size,
	};

public:
	/// constructor
	static_array() 
	{
		clear();
	}

	/// destructor
	~static_array()
	{
	}

	/// return the data size.
	std::size_t size() const
	{
		return _epos - _spos;
	}

	/// clear all data. 
	void clear()
	{
		_spos = _epos = 0;
	}

	/// check whether the container is empty.
	bool empty() const
	{
		return size() == 0 ;
	}

	/// add data to the end of the array.
	/// @note if no free space in the array, this function will throw an exception.
	void push_back( const value_type &d )
	{
		if( _epos < max_size )
		{
			_con[_epos++] = d;
		}
		else if( _spos > 0 )
		{
			// more effective, en ?
			// move the data.
			std::size_t count = _epos - _spos;
			memmove( _con, &_con[_spos], count * sizeof( value_type ) );
			// change the pointer
			_epos = count;
			_spos = 0;
			// push the data
			_con[_epos++] = d;
		}
		else 
		{
			// no free space.
			throw std::exception( "no free space in the static_array" );
		}
	}

	/// pop the back data out.If no data, then nothing will happen.
	void pop_back()
	{
		if( size() > 0 )
		{
			--_epos;
		}
	}

	/// push data in the front of the array.
	/// @note if no free space in the array, this function will throw an exception.
	void push_front( const value_type &d )
	{
		if( _spos > 0 )
		{
			_con[--_spos] = d;
		}
		else if( _epos < max_size )
		{
			// move the data.
			std::size_t count = _epos;
			if( count != 0 )
			{
				memmove( &_con[1], _con, count * sizeof( value_type ) );
			}
			// change the pointer
			++_epos;
			// push the data.
			_con[0] = d;
		}
		else
		{
			// no free space
			throw std::exception( "no free space in the static_array" );
		}
	}

	/// pop data at the front. if no data nothing will hanppen.
	void pop_front()
	{
		if( size() > 0 )
		{
			++_spos;
		}
	}

	/// return the front iterator.(read/write)
	iterator begin()
	{
		return iterator( &_con[_spos] );
	}

	/// return the end iterator.
	iterator end()
	{
		return iterator( &_con[_epos] );
	}

	/// erase
	iterator erase( iterator first, iterator last )
	{
		if( first != last )
		{
			iterator::difference_type count = end() - last;
			if( count != 0 )
			{
				memmove( first._ptr, last._ptr, count * sizeof( value_type ) );
			}
			_epos = count + ( first - begin() );
		}
		return first;
	}

	/// erase
	iterator erase( iterator Where )
	{
		return erase( Where, Where + 1 );
	}

	/// operator []
	value_type &operator[]( std::size_t off ) 
	{
		return *(begin() + off );
	}

	/// operator [] read only.
	const value_type &operator[] ( std::size_t off ) const
	{
		return *(begin() + off );
	}

private:
	/// container
	_Tp _con[max_size+1];
	/// start pos
	std::size_t _spos;
	/// end post
	std::size_t _epos;
};

KL_COMMON_NAMESPACE_END

#endif // end ___KL_STATICARRAY_H_