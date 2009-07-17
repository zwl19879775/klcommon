/**
 @file async_oper.c
 @author Kevin Lynx
 @brief A pure c version async-operator. It only use a os thread to 
 provide the async ability. Based on its implemention, it can be used
 on Windows, Unix-like OS, etc.
*/
#include "async_oper.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifdef WIN32
#include <process.h>
#include <windows.h>

static void *thread_create( unsigned (*routine)( void * ), void *arg )
{
	uintptr_t t = _beginthreadex( 0, 0, routine, arg, 0, 0 );
	return (void*)t;
}	

static void thread_destroy( void *t )
{
	CloseHandle( (HANDLE) t );
}

static void thread_suspend( void *t )
{
	SuspendThread( (HANDLE) t );
}

static void thread_resume( void *t )
{
	ResumeThread( (HANDLE) t );
}

#else
#include <pthread.h>
#include <stdlib.h>

struct thread_h
{
	pthread_t t;
	pthread_cond_t cond;
	pthread_mutex_t mutex;
};

static void thread_add_init( struct thread_h *t )
{
	pthread_mutex_init( &t->mutex, 0 );
	pthread_cond_init( &t->cond, 0 );
}

static void thread_add_release( struct thread_h *t )
{
	pthread_mutex_destroy( &t->mutex );
	pthread_cond_destroy( &t->cond );
}

static void *thread_create( void* (*routine)( void * ), void *arg )
{
	struct thread_h *h = (struct thread_h*) malloc( sizeof( struct thread_h ) );
	if( pthread_create( &h->t, 0, routine, arg ) )
	{
		free( h );
		return 0;
	}
	thread_add_init( h );
	return h;
}

static void thread_destroy( void *t )
{
	struct thread_h *h = (struct thread_h*) t;
	thread_add_release( h );
	free( h );
}

static void thread_suspend( void *t )
{
	struct thread_h *h = (struct thread_h*) t;
	pthread_mutex_lock( &h->mutex );
	pthread_cond_wait( &h->cond, &h->mutex );
	pthread_mutex_unlock( &h->mutex );
}

static void thread_resume( void *t )
{
	struct thread_h *h = (struct thread_h*) t;
	pthread_mutex_lock( &h->mutex );
	pthread_cond_signal( &h->cond );
	pthread_mutex_unlock( &h->mutex );
}

#endif

static 
#ifdef WIN32 
__stdcall unsigned 
#else
void *
#endif
thread_func( void *arg )
{
	struct async_oper *op = (struct async_oper*) arg;
	if( op == 0 )
	{
		return 0;
	}
	thread_suspend( op->h );
	while( !op->stop )	
	{
		op->routine( op->arg );	
		if( op->stop ) break;
		thread_suspend( op->h );
	}
	return 0;
}

int async_init( struct async_oper *op, void (*routine)( void* ), void *arg )
{
	op->arg = arg;
	op->stop = 0;
	op->routine = routine;
	op->h = thread_create( thread_func, op->arg );
	return op->h == 0;
}

int async_start( struct async_oper *op )
{
	thread_resume( op->h );
	return 0;
}

int async_release( struct async_oper *op )
{
	thread_destroy( op->h );
	return 0;
}

#ifdef __cplusplus
}
#endif

