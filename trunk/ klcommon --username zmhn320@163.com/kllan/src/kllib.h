/**
 * @file kllib.h
 * @author Kevin Lynx
 * @brief handle kl-addons etc
 */
#ifndef ___KL_LIB_H_
#define ___KL_LIB_H_

#include <stddef.h>

struct symTable;
struct klState;

/**
 * value passing between kl and c
 */
struct TValue
{
	union
	{
		double dval;
		char *sval;
	};
	enum
	{
		NUMBER, STRING
	} type;
	struct TValue *next;
};

/**
 * the plugin function arguments type
 * i.e:
 * struct TValue my_plugin_func( ArgType arg )
 */
typedef struct TValue* ArgType;

#define kl_next_arg( arg ) ( arg = arg->next )

/**
 * kl plugin function prototype
 */
typedef struct TValue (*kl_func)( struct TValue *arg_list );

/**
 * kl log function prototype
 */
typedef void (*kl_log)( size_t lineno, const char *fmt, ... );

/**
 * kl state
 */
struct klState
{
	struct symTable *st;
	kl_log log;
};


/**
 * create a new klState object
 */
struct klState *kl_new( kl_log l );

/**
 * delete a klState
 */
void kl_free( struct klState *kl );

/**
 * register a c-function in kl
 */
int kl_register( struct klState *kl, kl_func f, const char *name );

/**
 * run a chunk of kl code
 */
int kl_run( struct klState *kl, char *source );


#endif 
