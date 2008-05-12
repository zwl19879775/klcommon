///
/// @file my_thread_wrapper.h
/// @author Kevin Lynx
/// @date 3.6.2008
///
#ifndef __MY_THREAD_WRAPPER_H_
#define __MY_THREAD_WRAPPER_H_

#include <windows.h>

/// a semaphore object usually can be considered as an event set.
class Semaphore
{
public:
	/// 
	/// Constructor
	/// @param init_count 
	/// @param max_count
	Semaphore( long init_count = 0, long max_count = 2147483647 )
	{
		_semaphore = ::CreateSemaphore( NULL, init_count, max_count, NULL );
		_sig_count = init_count;
	}

	/// Destructor
	~Semaphore()
	{
		CloseHandle( _semaphore );
	}

	/// set the signaled state
	void signal( long count )
	{
		if( count <= 0 ) return ;
		::ReleaseSemaphore( _semaphore, count, 0 );
		_sig_count += count;
	}

	/// wait the semphore to be signaled state
	/// you can call signale function to set the signaled state.
	void wait()
	{
		::WaitForSingleObject( _semaphore, INFINITE );
		-- _sig_count;
	}

	/// reset the semphore to the nonsignaled state
	void reset()
	{
		for( long i = 0; i < _sig_count; ++ i )
		{
			::WaitForSingleObject( _semaphore, INFINITE );
		}

		_sig_count = 0;
	}

private:
	/// windows semaphore handle
	HANDLE _semaphore;
	/// the count specifies the signaled state
	long _sig_count;
};

/// multiple events
template <long size = 100>
class MultipleEvent
{
public:
	/// constructor
	MultipleEvent()
	{
		for( long i = 0; i < size; ++ i )
		{
			_events[i] = ::CreateEvent( NULL, TRUE, FALSE, NULL );
		}
		_pos = -1;
	}

	/// destructor
	~MultipleEvent()
	{
		for( long i = 0; i < size; ++ i )
		{
			::CloseHandle( _events[i] );
		}
	}

	/// set 'count' events to the signaled state
	void signal( long count )
	{
		if( count <= 0 ) return ;
		
		long max = _pos + count >= size ? size - 1 : _pos + count;
		for( long i = _pos + 1; i <= max; ++ i )
		{
			::SetEvent( _events[i] );
		}

		_pos += ( max - _pos );
	}

	/// wait an event
	void wait()
	{
		int wait_pos = _pos < 0 ? 0 : _pos;
		::WaitForSingleObject(_events[wait_pos], INFINITE );
		::ResetEvent( _events[wait_pos] );
		_pos = wait_pos == 0 ? _pos : _pos - 1;
	}

	/// reset all the events to the nonsignaled state
	void reset()
	{
		for( long i = 0; i <= _pos; ++ i )
		{
			::ResetEvent( _events[i] );
		}

		_pos = -1;
	}

private:
	/// event array
	HANDLE _events[size];
	/// state pointer
	long _pos;
};

/// mutex 
class Mutex
{
public:
	/// constructor
	Mutex()
	{
		::InitializeCriticalSection( &_cs );
	}

	/// destructor
	~Mutex()
	{
		::DeleteCriticalSection( &_cs );
	}

	/// acquire the mutex
	/// it will block if the mutex is acquired by other thread
	void acquire()
	{
		::EnterCriticalSection( &_cs );
	}

	/// release the mutex
	void release()
	{
		::LeaveCriticalSection( &_cs );
	}

private:
	/// critical section 
	CRITICAL_SECTION _cs;
};


#endif // end __MY_THREAD_WRAPPER_H_