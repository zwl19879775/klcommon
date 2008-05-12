///
/// @file kl_thread.h
/// @author Kevin Lynx
/// @date 3.7.2008
///
#ifndef ___KL_THREAD_H_
#define ___KL_THREAD_H_

#include <windows.h>
#include <process.h>
#include <cassert>
#include "kl_compiler_cfg.h"
#include "kl_functor.h"

KL_COMMON_NAMESPACE_BEGIN

#ifdef WIN32 

/// 
/// Wrap the windows thread details
///
class thread
{
public:
	/// thread parameter
	struct param
	{
		thread *_this;
		void *_other;
	};

public:
	/// thread function signature
	typedef kl_common::functor<void, TYPE_LIST1( void* )> thread_func_type;

	/// thread messages
	enum
	{
		WM_EXIT_THREAD = WM_USER + 100,
	};

	/// thread state
	enum
	{
		TS_INVALID = -1,
		TS_ACTIVE,
		TS_SUSPEND,
		TS_EXIT
	};

public:
	/// constructor
	thread()
	{
		_handle = 0;
		_id = 0;
		_param._other = 0;
		_param._this = this;
		_state = TS_INVALID;
	}

	/// destructor
	~thread()
	{
		// make sure the thread has already been terminated
		DWORD exit_code = 0;
		if( ::GetExitCodeThread( _handle, &exit_code ) && exit_code == STILL_ACTIVE )
		{
			// the thread is still running, terminate it directly.
			terminate();
		}
		::CloseHandle( _handle );
	}

	/// start the thread, the thread function will run immediately
	bool start( thread_func_type &thread_func, void *p = NULL )
	{
		assert( _state == TS_INVALID && "thread::start : invalid call!" );
		_param._this = this;
		_param._other = p;
		_func = thread_func;
		_handle = (HANDLE) ::_beginthreadex( NULL, 0, ThreadFunc, &_param, 0, (unsigned int*)&_id );
		_state = TS_ACTIVE;
		return _handle != NULL;
	}

	/// terminate the thread, it will terminate the thread forcely.
	void terminate()
	{
		assert( _handle != NULL && "thread::terminate : the thread is not exist!" );
		::TerminateThread( _handle, -1 ); 
		_state = TS_EXIT;
	}

	/// exit the thread, this function is safer than 'terminate' because it
	/// will post a WM_EXIT_THREAD message to the thread, and the thread will
	/// exit normally.
	/// @param wait if true, this function will NOT return until the thread is exit.
	void exit( bool should_wait = true )
	{
		assert( _handle != NULL && "thread::exit : the thread is invalid!" );
		if( post_message( WM_EXIT_THREAD, 0, 0 ) && should_wait )
		{
			wait();
		}
		_state = TS_EXIT;
	}

	/// wait for the thread until it's quit
	DWORD wait( DWORD millseconds = INFINITE )
	{
		assert( _handle != NULL && "thread::wait : the thread is invalid!" );
		return ::WaitForSingleObject( _handle, millseconds );
	}

	/// post a message to the thread.
	/// @see PostThreadMessage
	bool post_message( UINT msg, WPARAM wparam, LPARAM lparam )
	{
		assert( _handle != NULL && "thread::post_message : the thread is invalid!" );
		return ::PostThreadMessage( _id, msg, wparam, lparam ) != 0 ? true : false;
	}

	/// handle the message
	/// @return return the message value.return -1 indicate error.
	UINT handle_message()
	{
		MSG msg;
		memset( &msg, 0, sizeof( MSG ) );
		if( ::PeekMessage( &msg, (HWND) -1, 0, 0, PM_REMOVE ) == 0 )
		{
			return -1;
		}

		return msg.message ;
	}

	/// handle default message, if the message is WM_EXIT_THREAD, return false.
	bool handle_def_message()
	{
		UINT msg = handle_message();
		return msg != WM_EXIT_THREAD;
	}

	/// get the handle of the thread
	HANDLE handle() const { return _handle; }

	/// get the id of the thread
	DWORD id() const { return _id; }

	/// get the thread state
	int state() const { return _state; }

private:
	/// thread main function
	friend static unsigned int __stdcall ThreadFunc( void *p )
	{
		param *_param = (param*) p;
		assert( _param != NULL && "ThreadFunc : invalid thread parameter!" );
		thread *_thread = _param->_this ;
		assert( _thread != NULL && "ThreadFunc : invalid thread parameter!" );

		assert( _thread->_func && "thread : no thread function has been set!" );

		_thread->_func( p );

		_endthreadex( 0 );
		return 0;
	}

private:
	/// the thread handle
	HANDLE _handle;
	/// the thread id
	DWORD _id;
	/// thread parameter
	param _param;
	/// thread callback function
	thread_func_type _func;
	/// thread state
	int _state;
};

#endif

KL_COMMON_NAMESPACE_END

#endif // end ___KL_THREAD_H_