/**
 * @file klinterpret.h
 * @author Kevin Lynx
 * @brief to interpret syntax tree
 */
#ifndef ___KL_INTERPRET_H_
#define ___KL_INTERPRET_H_

#include <stddef.h>

struct treeNode;
struct symTable;

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
 * execute a syntax tree. 
 */
int inter_execute( struct treeNode *root, inter_log_func log_func, struct symTable *plugin_st );

#endif 
