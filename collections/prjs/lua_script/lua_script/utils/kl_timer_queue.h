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
			unsigned long interval;
			unsigned long id;
			typedef void (*callback_fn_T)( void * );
			callback_fn_T callback;
			void *arg;
		};

		inline bool operator < ( const timer &t1, const timer &t2 )
		{
			return t1.timeout > t2.timeout;
		}

		struct executor
		{
			 Private::timer::callback_fn_T fn;
			 void *arg;
		};
	}

	///
	/// usage:schedule a timer by 'schedule' function, and should be checked
	/// every program loop by called 'run'.
	///
	class timer_queue : protected std::priority_queue<Private::timer>
	{
	private:
		typedef std::priority_queue<Private::timer> BaseT;
	public:
		timer_queue()
		{
			_id_index = 1;
		}

		///
		/// schedule a timer.
		/// @param timeout the first timer timeout time.
		/// @param interval 0 to schedule a one-time timer.
		/// @return timer id
		///
		unsigned long schedule( unsigned long timeout, unsigned long interval,
			Private::timer::callback_fn_T fn, void *arg )
		{
			unsigned long id = get_id();
			Private::timer t = { timeout, interval, id, fn, arg };
			push( t );
			return id;
		}

		/// cancel a timer
		bool cancel( unsigned long id )
		{
			for( BaseT::container_type::iterator it = BaseT::c.begin();
				it != BaseT::c.end(); ++ it )
			{
				if( it->id == id )
				{
					BaseT::c.erase( it );
					std::make_heap( BaseT::c.begin(), BaseT::c.end(), BaseT::comp );
					return true;
				}
			}
			return false;
		}

		void run( unsigned long cur_time )
		{

			std::queue<Private::executor> exe_queue;

			while( !empty() )
			{
				Private::timer &t = top();
				if( t.timeout <= cur_time )
				{
					Private::executor exe = { t.callback, t.arg };
					exe_queue.push( exe );
					if( t.interval != 0 )
					{
						t.timeout = cur_time + t.interval;
						std::make_heap( BaseT::c.begin(), BaseT::c.end(), BaseT::comp );
					}
					else
					{
						pop();
					}
				}
				else
				{
					break;
				}
			}

			while( !exe_queue.empty() )
			{
				Private::executor &exe = exe_queue.front();
				exe.fn( exe.arg );
				exe_queue.pop();
			}
		}

		size_t size() const
		{
			return BaseT::size();
		}
	private:
		unsigned long get_id()
		{
			return _id_index ++;
		}
	private:
		unsigned long _id_index;
	};
}

#endif
