///
/// @file kl_threadpool.h
/// @author Kevin Lynx
/// @date 9.2.2008
///
#ifndef ___KL_THREAD_POOL_H_
#define ___KL_THREAD_POOL_H_

#include "kl_functor.h"
#include "my_thread_wrapper.h"
#include "kl_guard.h"
#include <list>
#include <algorithm>

#ifdef WIN32
#include <windows.h>
#include <process.h>

typedef HANDLE thread_h;
#define invalid_thread NULL
#define suspend_state CREATE_SUSPENDED
#define thread_create _beginthreadex
#define thread_terminate TerminateThread
#define thread_exit _endthreadex
#define thread_suspend SuspendThread
#define thread_resume ResumeThread
#define thread_close CloseHandle
#define thread_wait WaitForSingleObject
#define wait_infinite INFINITE
#endif

namespace kl_common
{
	///
	/// ThreadPool provide your ability to dispathc a job in a separate thread.
	///
	class thread_pool
	{
	public:
		/// callback function type
		typedef functor<void, TYPE_LIST1( void* )> thread_func;
		
		/// represent a thread.
		struct thread_t
		{
			thread_pool *_parent;
			thread_h _self;
			thread_func _func;
			void *_arg;
		};
		typedef std::list<thread_t*> thread_list;
	public:
		/// ctor
		thread_pool() : _shutdown( false )
		{
		}

		/// dtor
		~thread_pool()
		{
		}

		///
		/// pre-create some threads in the pool.
		///
		bool startup( int t_count = 5 )
		{
			bool ret = true;
			for( int i = 0; i < t_count; ++ i )
			{
				ret = add_thread() && ret;
			}
			return ret;
		}

		///
		/// cleanup the pool, kill all threads, you must ensure all your job function
		/// should return before this.
		///
		void cleanup()
		{
			guard<Mutex> g( _mutex );
			_shutdown = true;
			for( thread_list::iterator it = _free_threads.begin(); it != _free_threads.end(); ++ it )
			{
				// safe to terminate because they're suspended.
				thread_terminate( (*it)->_self, 0 );
				thread_close( (*it)->_self );
				delete *it;
			}
			_free_threads.clear();

			for( thread_list::iterator it = _busy_threads.begin(); it != _busy_threads.end(); ++ it )
			{
				thread_wait( (*it)->_self, wait_infinite );
				thread_close( (*it)->_self );
				delete *it;
			}
			_busy_threads.clear();
		}

		///
		/// Add a free thread in the pool.
		///
		bool add_thread()
		{
			guard<Mutex> g( _mutex );
			thread_t *t = new thread_t;
			t->_parent = this;
			t->_arg = 0;
			t->_func.reset();
			t->_self = (thread_h)thread_create( 0, 0, wrap_thread, t, suspend_state, 0 );
			if( t->_self == invalid_thread )
			{
				delete t;
				return false;
			}
			_free_threads.push_back( t );			
			return true;
		}

		///
		/// Dispatch a job in a separate thread, if there is no free thread, it will
		/// create a new thread.
		///
		void dispatch( thread_func &func, void *arg = 0 )
		{
			if( _free_threads.empty() )
			{
				add_thread();
			}
			guard<Mutex> g( _mutex );
			thread_t *t = _free_threads.front();
			t->_func = func;
			t->_arg = arg;
			_free_threads.pop_front();
			_busy_threads.push_back( t );
			thread_resume( t->_self );
		}

	private:

		///
		/// Put the thread into the free list.
		///
		void save( thread_t *t )
		{
			{
				guard<Mutex> g( _mutex );
				t->_func.reset();
				_free_threads.push_back( t );
				thread_list::iterator it = std::find( _busy_threads.begin(), _busy_threads.end(), t );
				_busy_threads.erase( it );
			}
			// and now, safe to suspend
			thread_suspend( t->_self );
		}

	private:
		friend static unsigned int __stdcall wrap_thread( void *p )
		{
			thread_t *t = (thread_t*) p;

			for( ; !t->_parent->_shutdown; )
			{
				if( t->_func )
				{
					t->_func( t->_arg );
					if( t->_parent->_shutdown )
					{
						break;
					}
					// suspend itself
					t->_parent->save( t );
				}
			}
			thread_exit( 0 );
			return 0;
		}
	private:
		thread_list _busy_threads;
		thread_list _free_threads;
		bool _shutdown;
		Mutex _mutex;
	};
}

#endif // ___KL_THREAD_POOL_H_