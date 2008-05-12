///
/// @file condition.h
/// @author Kevin Lynx
/// @date 3.5.2008
///
#ifndef ___KL_CONDITION_H_
#define ___KL_CONDITION_H_

#include "kl_guard.h"

KL_COMMON_NAMESPACE_BEGIN

///
/// condition class
///
///
template <typename _MutexType, typename _MultipleEventType>
class condition
{
public:
	/// mutex type
	typedef _MutexType mutex_type;
	/// multiple event type
	typedef _MultipleEventType multiple_event_type;
public:
	/// constructor
	condition( mutex_type &_mutex ) :
	  _external_mutex( _mutex )
	{
	}

	/// set the condition state to the signaled state
	void signal()
	{
		guard<mutex_type> g( _internal_mutex );
		if( _wait_count != 0 )
		{
			_events.signal( 1 );
			-- _wait_count;
		}
	}

	/// wait the condition until it 's signaled state
	void wait()
	{
		{
			guard<mutex_type> g( _internal_mutex );
			++ _wait_count ;
		}

		_external_mutex.release();
		_events.wait();
		_external_mutex.acquire();
	}

	/// set all the events in the _events to the signaled state
	/// so that all the 'wait' operation will return.
	void broadcast()
	{
		guard<mutex_type> g( _internal_mutex );
		if( _wait_count != 0 )
		{
			_events.signal( _wait_count );
			_wait_count = 0;
		}
	}

	/// reset the condition to the nonsignaled state
	/// that means, no event is signaled in the event array.
	void reset()
	{
		_events.reset();
	}

protected:
	/// external mutex
	mutex_type &_external_mutex;
	/// internal mutex
	mutex_type _internal_mutex;
	/// events
	multiple_event_type _events;
	/// wait count
	int _wait_count;
};

KL_COMMON_NAMESPACE_END

#endif // end ___KL_CONDITION_H_