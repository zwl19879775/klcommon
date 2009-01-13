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
struct interEnv;
struct treeNode;

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
#define kl_new_arg( arg ) arg = 0
#define kl_new_arg_malloc( arg ) \
	{ \
		arg = (struct TValue*) malloc( sizeof( struct TValue ) ) ; \
		arg->next = 0; arg->type = NUMBER; arg->dval = 0; \
	}

#define kl_add_number( arg_head, num ) \
	{ \
		ArgType arg, tail ; \
		kl_new_arg_malloc( arg ); \
		arg->type = NUMBER; arg->dval = (double)num; \
		if( arg_head == 0 ) arg_head = arg; \
		else \
		{ \
			for( tail = arg_head; tail->next != 0; tail = tail->next ) ; \
			tail->next = arg; \
		} \
	}
#define kl_add_string( arg_head, str ) \
	{ \
		ArgType arg, tail ; \
		kl_new_arg_malloc( arg ); \
		arg->type = STRING; arg->sval = (char*)str; \
		if( arg_head == 0 ) arg_head = arg; \
		else \
		{ \
			for( tail = arg_head; tail->next != 0; tail = tail->next ) ; \
			tail->next = arg; \
		} \
	}
#define kl_free_arg( arg_head ) \
	{ \
		ArgType arg; \
		for( arg = arg_head; arg != 0;  ) \
		{ \
			ArgType tmp = arg; \
			arg = arg->next; \
			free( tmp ); \
		} \
		arg_head = 0; \
	}	

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
	struct interEnv *env;
	struct treeNode *root;
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
 * parse the source, build the global symbol table, get ready to execute.
 */
int kl_prepare( struct klState *kl, char *source );

/**
 * call a kl function
 */
struct TValue kl_call( struct klState *kl, const char *name, ArgType args );

/**
 * run a chunk of kl code
 */
int kl_run( struct klState *kl, char *source );


#endif 
