/**
 * @file klinterpret.h
 * @author Kevin Lynx
 * @brief to interpret syntax tree
 */
#ifndef ___KL_INTERPRET_H_
#define ___KL_INTERPRET_H_

#include <stddef.h>
#include "klsymtab.h"

struct treeNode;

typedef void (*inter_log_func)( size_t lineno, const char *fmt, ... );

/** 
 * interpret environment
 */
struct interEnv
{
	struct symTable *global_st;
	struct symTable *local_st;
	struct symTable *plugin_st;
	inter_log_func inter_log; 
};

/**
 * create a new interpret environment
 */
struct interEnv *inter_env_new( inter_log_func log_func, struct symTable *plugin_st );

/**
 * free an interpret environment
 */
void inter_env_free( struct interEnv *env );

/**
 * execute a function. 
 */
struct Value inter_call_func( struct interEnv *env, struct treeNode *root );

/**
 * a helper function to call 'main' function.
 */
int inter_execute( struct interEnv *env );

/**
 * build the global symbol table.
 */
void inter_build_global_st( struct interEnv *env, struct treeNode *root );

#endif 
