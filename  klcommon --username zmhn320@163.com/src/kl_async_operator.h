///
///
///
#ifndef ___KL_ASYNC_OPERATOR_H_
#define ___KL_ASYNC_OPERATOR_H_

#include "kl_compiler_cfg.h"
#include "kl_multi_list.h"
#include "kl_thread.h"
#include "my_thread_wrapper.h"

KL_COMMON_NAMESPACE_BEGIN

///
/// async_operator wraps a thread-safe list( multi_list ) and create a thread to execute async operation.
/// Anytime you can push some list nodes into the list, and the thread will give it to the callback function.
///
template <typename _NodeType>
class async_operator
{
public:
	/// the list node type.
	typedef _NodeType node_type;
	/// the list.
	typedef multi_list<node_type, Mutex, Semaphore> list_type;
	/// operator signature
	typedef functor<void, TYPE_LIST1( list_type& )> operator_type;
	/// init function signature, called when the thread starts.
	typedef functor<void> init_type;
	/// called before the thread exits.
	typedef functor<void> release_type;

public:
	///
	/// start the thread and execute the operation.
	/// @param op the callback function operate the list node.
	/// 
	void execute( operator_type &op, init_type &init = functor(), release_type &release = functor() )
	{
		_operator = op;
		_init = init;
		_release = release;
		_list.clear();
		_thread.start( thread::thread_func_type( *this, &async_operator::thread_func ) );
	}

	///
	/// exit the thread.It will block until the thread exited.
	///
	void exit()
	{
		_thread.exit();
	}
	
	///
	/// get the list so that you can pust list nodes.
	///
	list_type &list()
	{
		return _list;
	}

private:
	///
	/// thread function.
	///
	void thread_func( void *p )
	{
		thread::param *param = (thread::param*) p;
		thread *t = param->_this;
		
		if( _init )
		{
			_init();
		}

		while( true )
		{
			unsigned int msg = t->handle_message();
			if( msg == thread::WM_EXIT_THREAD )
			{
				break;
			}

			_operator( _list );
		}

		if( _release )
		{
			_release();
		}
	}

private:
	list_type _list;
	operator_type _operator;
	init_type _init;
	release_type _release;
	thread _thread;
};

KL_COMMON_NAMESPACE_END

#endif // end ___KL_ASYNC_OPERATOR_H_