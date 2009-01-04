/**
 * @file klinterpret.c
 * @author Kevin Lynx
 * @brief to interpret syntax tree
 */
#include "klinterpret.h"
#include "klparser.h"
#include "kllex.h"
#include "klsymtab.h"
#include <stdio.h>

/**
 * build the global symbol table
 */
static void inter_build_global_st( struct interEnv *env, struct treeNode *root );
static struct Symbol *inter_lookup_symbol( struct interEnv *env, const char *name );
static union Value inter_op_exp( struct interEnv *env, struct treeNode *node );
static union Value inter_expression( struct interEnv *env, struct treeNode *node );
static void inter_exp_stmt( struct interEnv *env, struct treeNode *tree );
static union Value inter_function( struct interEnv *env, struct treeNode *tree );

static void inter_build_global_st( struct interEnv *env, struct treeNode *root )
{
	struct treeNode *node;
	for( node = root; node != 0; node = node->sibling )
	{
		if( node->type == NT_STMT )
		{
			if( node->subtype.stmt == ST_VAR_DEF )
			{
				union Value val;
			    val	= inter_expression( env, node->child[0] );
				sym_insert( env->global_st, node->attr.val.sval, val, SB_VAR );				
			}
			else if( node->subtype.stmt == ST_FUNC_DEF )
			{
				union Value val;
				val.address = node;
				sym_insert( env->global_st, node->attr.val.sval, val, SB_FUNC );
			}
			else
			{
				env->inter_log( "runtime error->error statement in global scope" );
			}
		}
		else
		{
			env->inter_log( "runtime error->global scope must be var/func def" );
		}
	}
}

static struct Symbol *inter_lookup_symbol( struct interEnv *env, const char *name )
{
	struct Symbol *sb;
	sb = sym_lookup( env->global_st, name );
	if( sb == 0 )
	{
		sb = sym_lookup( env->local_st, name );
	}
	return sb;
}

static union Value inter_op_exp( struct interEnv *env, struct treeNode *node )
{
	union Value ret, left, right;
	ret.address = 0;
	/* two operator operation */
	if( node->child[0] == 0 || node->child[1] == 0 )
	{
		env->inter_log( "runtime error->expect an expression" );
	}

	if( node->attr.op != '=' )
	{
		left = inter_expression( env, node->child[0] );
	}
	right = inter_expression( env, node->child[1] );
	switch( node->attr.op )
	{
		case TK_LE:
			{
			}
			break;

		case TK_GE:
			{
			}
			break;

		case TK_EQ:
			{
			}
			break;

		case TK_NE:
			{
			}
			break;

		case '<':
			{
			}
			break;

		case '>':
			{

			}
			break;

		case '+':
			{

			}
			break;

		case '-':
			{
			}
			break;

		case '*':
			{
			}
			break;

		case '/':
			{
			}
			break;

		case '%':
			{

			}
			break;

		case '=':
			{
				struct treeNode *l_node = node->child[0];
				struct Symbol *sb = sym_lookup( env->global_st, l_node->attr.val.sval );
				if( sb == 0 )
				{
					sym_insert( env->local_st, l_node->attr.val.sval, right, SB_VAR );
				}	
				else
				{
					sym_insert( env->global_st, l_node->attr.val.sval, right, SB_VAR );
				}
			}
			break;

		default:
			env->inter_log( "runtime error->unsupport operation" );
	}
}

static union Value inter_expression( struct interEnv *env, struct treeNode *node )
{
	union Value ret ;

	switch( node->subtype.exp )
	{
		case ET_OP:
			return inter_op_exp( env, node );

		case ET_CONST:
			ret.dval = node->attr.val.dval;
		    return ret;

		case ET_STRING:
			ret.sval = node->attr.val.sval;
			return ret;

		case ET_ID:
			/* lookup in the symbol table */
			{
				struct Symbol *sb = inter_lookup_symbol( env, node->attr.val.sval );
				if( sb == 0 )
				{
					/* not exist, create a new symbol */
					ret.address = 0;
					sym_insert( env->local_st, node->attr.val.sval, ret, SB_VAR ); 
					return ret;
				}
				return sb->val;
			}

		case ET_FUNC_CALL:
			{
			}
			break;	

		default:
			env->inter_log( "" );	
	}
	ret.address = 0;
	return ret;
}

static void inter_exp_stmt( struct interEnv *env, struct treeNode *tree )
{
	inter_expression( env, tree );
}

static union Value inter_function( struct interEnv *env, struct treeNode *tree )
{
	struct treeNode *node = tree;
	union Value ret;
	env->local_st = sym_new();
	for( node = tree; node != 0; node = node->sibling )
	{
		if( node->type == NT_STMT )
		{
			switch( node->subtype.stmt )
			{
				case ST_IF:
					break;

				case ST_WHILE:
					break;

				case ST_RETURN:
					break;

				default:
					inter_exp_stmt( env, node );
			}
		}	
	}
	sym_free( env->local_st );
	ret.address = 0;
	return ret;	
}

int inter_execute( struct treeNode *root, inter_log_func log_func )
{
	struct Symbol *main;
	struct interEnv *env = (struct interEnv*) malloc( sizeof( struct interEnv ) );
	env->inter_log = log_func;
	env->global_st = sym_new();
	inter_build_global_st( env, root );
	main = sym_lookup( env->global_st, "main" );
	if( main != 0 )
	{
		inter_function( env, (struct treeNode*) main->val.address );
	}
	sym_free( env->global_st );
	free( env );
	return 0;
}


