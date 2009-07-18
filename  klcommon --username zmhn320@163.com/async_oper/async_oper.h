/**
 @file async_oper.h
 @author Kevin Lynx
 @brief A pure c version async-operator. It only use a os thread to 
 provide the async ability. Based on its implemention, it can be used
 on Windows, Unix-like OS, etc.
*/
#ifndef ___ASYNC_OPER_H_
#define ___ASYNC_OPER_H_

#ifdef __cplusplus
extern "C"
{
#endif

struct async_oper
{
	void *h;
	void *arg;
	volatile int stop;
	int (*routine)( void* );
};

/**
  Init the async_oper, it will create a suspend thread.

  @param routine the async executed function, return non-zero to pause the execution.
  @param arg argument passed to the routine.
*/
int async_init( struct async_oper *op, int (*routine)( void* ), void *arg );

/**
  Resume the suspend thread, and start the async operator.
*/
int async_start( struct async_oper *op );

/**
  Release the async operator, destroy the thread resource.
  This function will wait the thread until it's terminated, and you must be sure
  the routine of async_oper returns 1.
*/
int async_release( struct async_oper *op );

#ifdef __cplusplus
}
#endif
#endif

