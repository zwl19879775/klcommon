///
/// @file kl_multi_list.h
/// @author Kevin Lynx
/// @date 3.5.2008
///
#ifndef ___KL_MULTI_LIST_H_
#define ___KL_MULTI_LIST_H_

#include <list>
#include "kl_condition.h"

KL_COMMON_NAMESPACE_BEGIN

///
/// multi_list, wrap std::list to support multi-thread.
///
/// @param _ConditionType condition type
/// @param _DataType 
/// @param _ContainerType used to contain data.
///
template <typename _DataType, typename _MutexType, typename _MultipleEventType,
	template <typename _Tp, typename _Alloc = std::allocator<_Tp> > class _ContainerType = std::list, 
	template <typename _Tp, typename _Tp2> class _ConditionType = condition>
class multi_list
{
public:
	/// value type
	typedef _DataType value_type;
	/// mutex type
	typedef _MutexType mutex_type;
	/// multiple events type
	typedef _MultipleEventType multiple_event_type;
	/// container type
	typedef _ContainerType<_DataType, std::allocator<_DataType> > container_type;
	/// condition type
	typedef _ConditionType<_MutexType, _MultipleEventType> condition_type;
public:
	/// constructor
	multi_list() :
	  _condition( _mutex )
	{
		_condition.reset();
	}

	/// destructor
	~multi_list()
	{
	}

	/// add data to the end of the list
	void push_back( const value_type &data )
	{
		guard<mutex_type> g( _mutex );

		if( _container.size() == 0 )
		{
			_condition.signal();
		}

		_container.push_back( data );
	}

	/// add data to the front of the list
	void push_front( const value_type &data )
	{
		guard<mutex_type> g( _mutex );

		if( _container.size() == 0 )
		{
			_condition.signal();
		}

		_container.push_front( data );
	}

	/// remove the last element and returns it
	/// @param block if true the function will block when there is not data in the container.
	value_type pop_back( bool block = true )
	{
		guard<mutex_type> g( _mutex );

		if( block )
		{
			while( _container.size() == 0 )
				_condition.wait();
		}

		assert( _container.size() > 0 && "multi_list::pop_back : no element in the list!" );
		value_type data = _container.back();
		_container.pop_back();
		
		return data;
	}

	/// remove the first element and returns it
	/// @param block if true the function will block when there is not data in the container.
	value_type pop_front( bool block = true )
	{
		guard<mutex_type> g( _mutex );

		if( block )
		{
			while( _container.size() == 0 )
				_condition.wait();
		}

		assert( _container.size() > 0 && "multi_list::pop_back : no element in the list!" );
		value_type data = _container.front();
		_container.pop_front();
		
		return data;
	}

	/// if you want to pop all the data, alougth you can pop_front everytime, but that's 
	/// not the good way because the lock/unlock will spend more time.
	/// @param container output container contains all the data.
	void pop_all( container_type &container, bool block = true )
	{
		guard<mutex_type> g( _mutex );
	
		if( block )
		{
			while( _container.size() == 0 )
				_condition.wait();
		}

		while( _container.size() > 0 )
		{
			container.push_back( _container.front() );
			_container.pop_front();
		}
	}

	/// remove all the elements
	void clear()
	{
		guard<mutex_type> g( _mutex );
		_container.clear();
	}

	/// get the size of the list
	std::size_t size()
	{
		guard<mutex_type> g( _mutex );
		return _container.size();
	}

private:
	/// container
	container_type _container;
	/// condition
	condition_type _condition;
	/// mutex
	mutex_type _mutex;
};

KL_COMMON_NAMESPACE_END

#endif // end ___KL_MULTI_LIST_H_