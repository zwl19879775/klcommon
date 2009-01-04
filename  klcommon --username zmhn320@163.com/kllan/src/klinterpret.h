/**
 * @file klinterpret.h
 * @author Kevin Lynx
 * @brief to interpret syntax tree
 */
#ifndef ___KL_INTERPRET_H_
#define ___KL_INTERPRET_H_

struct treeNode;
struct symTable;

typedef void (*inter_log_func)( const char *fmt, ... );

/** 
 * interpret environment
 */
struct interEnv
{
	struct symTable *global_st;
	struct symTable *local_st;
	inter_log_func inter_log; 
};

/**
 * execute a syntax tree. 
 */
int inter_execute( struct treeNode *root, inter_log_func log_func );

#endif 
