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
	void (*routine)( void* );
};

int async_init( struct async_oper *op, void (*routine)( void* ), void *arg );

int async_start( struct async_oper *op );

int async_release( struct async_oper *op );

#ifdef __cplusplus
}
#endif
#endif

