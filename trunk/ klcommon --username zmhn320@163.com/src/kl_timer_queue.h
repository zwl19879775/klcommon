///
/// @file kl_timer_queue.h
/// @author Kevin Lynx
/// @brief a simple timer queue.
///
#ifndef ___KL_TIMER_QUEUE_H_
#define ___KL_TIMER_QUEUE_H_

#include <queue>

namespace kl_common
{
	namespace Private
	{
		struct timer
		{
			unsigned long timeout;
			typedef void (*callback_fn_T)( void * );
			callback_fn_T callback;
			void *arg;
		};
	
		bool operator < ( const timer &t1, const timer &t2 )
		{
			return t1.timeout > t2.timeout;
		}
	}

	///
	/// usage:schedule a timer by 'schedule' function, and should be checked
	/// every program loop by called 'run'.
	///
	class timer_queue
	{
	private:
		typedef std::priority_queue<timer> TimerQueueT;
	public:
		void schedule( unsigned long timeout, timer::callback_fn_T fn, void *arg )
		{
			timer t = { timeout, fn, arg };
			_timer_queue.push( t );
		}

		void run( unsigned long cur_time )
		{
			while( !_timer_queue.empty() )
			{
				timer &t = _timer_queue.top();
				if( t.timeout <= cur_time )
				{
					t.callback( t.arg );
					_timer_queue.pop();
				}
				else
				{
					break;
				}
			}
		}

		size_t size() const
		{
			return _timer_queue.size() ;
		}
	private:
		TimerQueueT _timer_queue;
	};
}

#endif
