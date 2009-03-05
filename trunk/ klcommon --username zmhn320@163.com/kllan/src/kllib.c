/**
 * @file kllib.h
 * @author Kevin Lynx
 * @brief handle kl-addons etc
 */
#include "kllib.h"
#include "klsymtab.h"
#include "klinterpret.h"
#include "klparser.h"
#include "kllex.h"
#include <stdio.h>
#include <stdarg.h>
#include <malloc.h>
#include <string.h>
#include "klmemcheck.h"

/**
 * register some build-in symbol
 *
 */
static void kl_register_buildin_symbol( struct klState *kl )
{
	struct Value val;
	val.type = SB_VAR_NUM;
	val.dval = (long) kl;
	sym_insert( kl->env->global_st, "KLSTATE", val );
}

double kl_check_number( ArgType *arg )
{
	if( *arg != 0 && (*arg)->type == NUMBER )
	{
		double t = (*arg)->dval;
		kl_next_arg( *arg );
		return t;
	}
	return 0;	
}

const char *kl_check_string( ArgType *arg )
{
	if( *arg != 0 && (*arg)->type == STRING )
	{
		const char *s = (*arg)->sval;
		kl_next_arg( *arg );
		return s;
	}
	return "";
}

struct klState *kl_new( kl_log l )
{
	struct klState *kl = (struct klState*) malloc( sizeof( struct klState ) );
	kl->log = l;
	kl->root = 0;
	kl->st = sym_new();
	kl->env = inter_env_new( l, kl->st );
	return kl;
}

void kl_free( struct klState *kl )
{
	inter_env_free( kl->env );
	sym_free( kl->st );
	syn_free_tree( kl->root );
	free( kl );
}

int kl_register( struct klState *kl, kl_func f, const char *name )
{
	struct Value val ;
	val.address = f;
	val.type = SB_FUNC;
	sym_insert( kl->st, name, val );
	return 0;
}

int kl_prepare( struct klState *kl, char *source )
{
	struct lexState ls;
	lex_setinput( &ls, source, kl->log );
	kl->root = syn_parse( &ls );
	inter_build_global_st( kl->env, kl->root );
	/* to build some build-in symbol */
	kl_register_buildin_symbol( kl );
	return 0;
}

struct TValue kl_call( struct klState *kl, const char *name, ArgType args )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	struct Value vret;
	struct symTable *st = 0, *tmp_st = 0;
	struct Symbol *func = sym_lookup( kl->env->global_st, name );
	struct treeNode *func_node = 0, *param_node = 0;
	if( func == 0 )
	{
		kl->log( 0, ">>lib error->dost not find the function->%s", name );
		return ret;
	}
	func_node = (struct treeNode*) func->val.address;
	st = sym_new();
	for( param_node = func_node->child[0]; args != 0 && param_node != 0;
			param_node = param_node->sibling, args = args->next )
	{
		struct Value arg ;
		if( args->type == NUMBER )
		{
			arg.dval = args->dval;
			arg.type = SB_VAR_NUM;
		}
		else if( args->type == STRING )
		{
			arg.sval = (char*) malloc( strlen( args->sval ) + 1 );
			strcpy( arg.sval, args->sval );
			arg.type = SB_VAR_STRING;
		}
		sym_insert( st, param_node->attr.val.sval, arg );
	}
	
	tmp_st = kl->env->local_st;
	kl->env->local_st = st;
	vret = inter_call_func( kl->env, func_node );
	sym_free( st );
	kl->env->local_st = tmp_st;
	if( vret.type == SB_VAR_NUM )
	{
		ret.type = NUMBER;
		ret.dval = vret.dval;
	}
	else if( vret.type == SB_VAR_STRING )
	{
		ret.type = STRING;
		ret.sval = vret.sval;
	}

	return ret;
}

int kl_run( struct klState *kl, char *source )
{
	kl_prepare( kl, source );
	kl_call( kl, "main", 0 );
	return 0;
}
