/**
 * @file klinterpret.c
 * @author Kevin Lynx
 * @brief to interpret syntax tree
 */
#include "klinterpret.h"
#include "klparser.h"
#include "kllex.h"
#include "klsymtab.h"
#include "kllib.h"
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "klmemcheck.h"

/**
 * function executed result type
 */
enum
{
	ER_NORMAL,
	ER_RETURN,
	ER_BREAK
};

/**
 * function executed result
 */
struct FuncRet
{
	struct Value val;
	int type;
};

/**
 * build the global symbol table
 */
static struct Symbol *inter_lookup_symbol( struct interEnv *env, const char *name );
static struct Value inter_op_exp( struct interEnv *env, struct treeNode *node );
static struct Value inter_func_call_exp( struct interEnv *env, struct treeNode *node );
static struct Value inter_expression( struct interEnv *env, struct treeNode *node );
static void inter_exp_stmt( struct interEnv *env, struct treeNode *tree );
static struct Value inter_function( struct interEnv *env, struct treeNode *tree );
static struct FuncRet inter_statements( struct interEnv *env, struct treeNode *node );
static struct FuncRet inter_if_stmt( struct interEnv *env, struct treeNode *node );
static struct FuncRet inter_while_stmt( struct interEnv *env, struct treeNode *node );
static struct FuncRet inter_for_stmt( struct interEnv *env, struct treeNode *node );
static int _kl_run_plugin( struct interEnv *env, const char *name, struct treeNode *arg_node, struct Value *ret );


static struct Value inter_copy_val_str( struct Value *val )
{
	if( val->type == SB_VAR_STRING )
	{
		struct Value tmp;
		tmp.type = SB_VAR_STRING;
		tmp.sval = (char*) malloc( strlen( val->sval ) + 1 );
		strcpy( tmp.sval, val->sval );
		return tmp;
	}
	return *val;
}

static void inter_free_val_str( struct Value *val )
{
	if( val->type == SB_VAR_STRING )
	{
		free( val->sval );
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

static struct Value inter_op_exp( struct interEnv *env, struct treeNode *node )
{
	struct Value ret, left, right = { { 0 }, SB_VAR_NUM };
	ret = left = right;
	/* two operator operation */
	if( ( node->child[0] == 0 || node->child[1] == 0 ) && node->attr.op != '-' && 
			node->attr.op != '!' )
	{
		env->inter_log( node->lineno, ">>runtime error->expect an expression" );
		return ret;
	}

	if( node->attr.op != '=' && node->child[0] != 0 )
	{
		left = inter_expression( env, node->child[0] );
	}
	right = inter_expression( env, node->child[1] );
	switch( node->attr.op )
	{
		/* these operations below do not handle the type of the two opertions */
		case TK_OR:
			{
				ret.type = SB_VAR_NUM;
				ret.dval = ( left.dval || right.dval );
			}
			break;

		case TK_AND:
			{
				ret.type = SB_VAR_NUM;
				ret.dval = ( left.dval && right.dval );
			}
			break;

		case '!':
			{
				ret.type = SB_VAR_NUM;
				ret.dval = !(right.dval);
			}
			break;	

		case TK_LE:
			{
				ret.type = SB_VAR_NUM;
				ret.dval = ( left.dval <= right.dval ? 1 : 0 );	
			}
			break;

		case TK_GE:
			{
				ret.type = SB_VAR_NUM;
				ret.dval = ( left.dval >= right.dval ? 1 : 0 );
			}
			break;

		case TK_EQ:
			{
				ret.type = SB_VAR_NUM;
				ret.dval = ( left.dval == right.dval ? 1 : 0 );
			}
			break;

		case TK_NE:
			{
				ret.type = SB_VAR_NUM;
				ret.dval = ( left.dval != right.dval ? 1 : 0 );
			}
			break;

		case '<':
			{
				ret.type = SB_VAR_NUM;
				ret.dval = ( left.dval < right.dval ? 1 : 0 );
			}
			break;

		case '>':
			{
				ret.type = SB_VAR_NUM;
				ret.dval = ( left.dval > right.dval ? 1 : 0 );
			}
			break;

		case '+':
			{
				if( left.type == SB_VAR_NUM && right.type == SB_VAR_NUM )
				{
					ret.type = SB_VAR_NUM;
					ret.dval = left.dval + right.dval;
				}
				else if( left.type == SB_VAR_STRING && right.type == SB_VAR_STRING )
				{
					ret.type = SB_VAR_STRING;
					ret.sval = (char*) malloc( strlen( left.sval ) + strlen( right.sval ) + 1 );
					strcpy( ret.sval, left.sval );
					strcat( ret.sval, right.sval );
				}
				else if( left.type == SB_VAR_STRING || right.type == SB_VAR_STRING )
				{
					ret.type = SB_VAR_STRING;
					if( left.type == SB_VAR_STRING )
					{
						char tmp[STRING_MAX_LEN];
						sprintf( tmp, "%s%lf", left.sval, right.dval );
						ret.sval = (char*)malloc( strlen( tmp ) + 1 );
						strcpy( ret.sval, tmp );
					}
					else if( right.type == SB_VAR_STRING )
					{
						char tmp[STRING_MAX_LEN];
						sprintf( tmp, "%lf%s", left.dval, right.sval );
						ret.sval = (char*)malloc( strlen( tmp ) + 1 );
						strcpy( ret.sval, tmp );
					}	
				}

				inter_free_val_str( &left );
				inter_free_val_str( &right );
			}
			break;

		case '-':
			{
				ret.type = SB_VAR_NUM;
				ret.dval = left.dval - right.dval;
			}
			break;

		case '*':
			{
				ret.type = SB_VAR_NUM;
				ret.dval = left.dval * right.dval;
			}
			break;

		case '/':
			{
				if( right.dval == 0 )
				{
					env->inter_log( node->lineno, ">>runtime error->divied by 0" );
				}
				else
				{
					ret.type = SB_VAR_NUM;
					ret.dval = left.dval / right.dval;
				}
			}
			break;

		case '%':
			{
				if( right.dval == 0 )
				{
					env->inter_log( node->lineno, ">>runtime error->divied by 0" );
				}
				else
				{
					ret.type = SB_VAR_NUM;
					ret.dval = (int)left.dval % (int)right.dval;
				}
			}
			break;

		case '=':
			{
				struct treeNode *l_node = node->child[0];
				if( l_node->subtype.exp == ET_ID )
				{
					struct Symbol *sb = sym_lookup( env->global_st, l_node->attr.val.sval );
					if( sb == 0 )
					{
						sb = sym_lookup( env->local_st, l_node->attr.val.sval );
						if( sb != 0 && sb->val.type == SB_VAR_ARRAY )
						{
							env->inter_log( l_node->lineno, ">>runtime error->cannot write array address" );						
						}
						else
						{
							/* create new or update the symbol in local scope */
							sym_insert( env->local_st, l_node->attr.val.sval, right );
						}
					}	
					else if( sb->val.type == SB_VAR_ARRAY )
					{
						env->inter_log( l_node->lineno, ">>runtime error->cannot write array address" );						
					}
					else
					{
						/* update the symbol in global scope */
						sym_insert( env->global_st, l_node->attr.val.sval, right );
					}
				}
				else if( l_node->subtype.exp == ET_ARRAY )
				{
					struct Symbol *sb = inter_lookup_symbol( env, l_node->attr.val.sval );
					if( sb == 0 )
					{
						env->inter_log( l_node->lineno, ">>runtime error->undefined array" );
					}
					else
					{
						struct Value index = inter_expression( env, l_node->child[0] );
						if( index.type != SB_VAR_NUM || index.dval < 0 || index.dval >= sb->val.size )
						{
							env->inter_log( l_node->child[0]->lineno, ">>runtime error->invalid array index" );
						}
						else
						{
							sb->val.aval[(size_t)index.dval] = right;
						}
					}
				}
				ret = inter_copy_val_str( &right );
			}
			break;

		default:
			env->inter_log( node->lineno, ">>runtime error->unsupport operation" );
	}
	return ret;
}

static struct Value inter_func_call_exp( struct interEnv *env, struct treeNode *node )
{
	struct Value ret = { { 0 }, SB_VAR_NUM }, arg;
	struct symTable *st = 0, *tmp_st = 0;
	struct treeNode *func_node = 0, *param_node = 0, *arg_node = 0;
	struct Symbol *func = 0;
	if( _kl_run_plugin( env, node->attr.val.sval, node->child[1], &ret ) == 0 )
	{
		return ret;
	}

	func = sym_lookup( env->global_st, node->attr.val.sval );
	if( func == 0 || func->val.type != SB_FUNC )
	{
		env->inter_log( node->lineno, ">>runtime error->the function [%s] does not exist", node->attr.val.sval );
		return ret;
	}
	
	func_node = (struct treeNode*) func->val.address;
	st = sym_new();
	/* compute the arguments value and insert them into the symbol table */
	for( arg_node = node->child[1], param_node = func_node->child[0]; 
			arg_node != 0 && param_node != 0;
			arg_node = arg_node->sibling, param_node = param_node->sibling )
	{
		arg = inter_expression( env, arg_node );
		sym_insert( st, param_node->attr.val.sval, arg );
	}

	tmp_st = env->local_st;
	env->local_st = st;
	ret = inter_function( env, func_node );
	sym_free( st );
	env->local_st = tmp_st;
	return ret;
}

static int _kl_run_plugin( struct interEnv *env, const char *name, struct treeNode *arg_node, struct Value *ret )
{
	kl_func func;
	struct Symbol *func_s = 0;
	struct Value arg;
	struct TValue *tval = 0, *tprev = 0, tret;
	if( env->plugin_st == 0 )
	{
		return -1;
	}
	func_s = sym_lookup( env->plugin_st, name );
	if( func_s == 0 )
	{
		return -1;
	}
	func = (kl_func)func_s->val.address;
	for( ; arg_node != 0; arg_node = arg_node->sibling )
	{
		struct TValue *t = (struct TValue*) malloc( sizeof( struct TValue ) );
		t->next = 0;
		arg = inter_expression( env, arg_node );
		if( arg.type == SB_VAR_NUM )
		{
			t->dval = arg.dval;
			t->type = NUMBER;
		}
		else if( arg.type == SB_VAR_STRING )
		{
			/*t->sval = (char*) malloc( strlen( arg.sval ) + 1 );
			strcpy( t->sval, arg.sval );*/
			t->sval = arg.sval;
			t->type = STRING;
		}
		if( tval == 0 )
		{
			tval = tprev = t;
		}
		else
		{
			tprev->next = t;
			tprev = t;
		}
	}
	tret = func( tval );
	for( tprev = tval; tprev != 0; )
	{
		struct TValue *tmp = tprev;
		if( tprev->type == STRING )
		{
			free( tprev->sval );
		}
		tprev = tprev->next;
		free( tmp );	
	}
	
	if( tret.type == NUMBER )
	{
		(*ret).type = SB_VAR_NUM;
		(*ret).dval = tret.dval;
	}
	else if( tret.type == STRING )
	{
		(*ret).type = SB_VAR_STRING;
		(*ret).sval = tret.sval;
	}
	return 0;
}
		
static struct Value inter_expression( struct interEnv *env, struct treeNode *node )
{
	struct Value ret ;

	switch( node->subtype.exp )
	{
		case ET_OP:
			return inter_op_exp( env, node );

		case ET_CONST:
			ret.dval = node->attr.val.dval;
			ret.type = SB_VAR_NUM;
		    return ret;

		case ET_STRING:
			ret.sval = (char*) malloc( strlen( node->attr.val.sval ) + 1 );
			strcpy( ret.sval, node->attr.val.sval );
			ret.type = SB_VAR_STRING;
			return ret;

		case ET_ID:
			/* lookup in the symbol table */
			{
				struct Symbol *sb = inter_lookup_symbol( env, node->attr.val.sval );
				if( sb == 0 )
				{
					/* not exist, create a new symbol */
					ret.dval = 0;
					ret.type = SB_VAR_NUM;
					sym_insert( env->local_st, node->attr.val.sval, ret ); 
					return ret;
				}
				if( sb->val.type == SB_VAR_ARRAY )
				{
					ret.type = SB_VAR_NUM;
					ret.dval = (size_t)sb->val.aval;
					return ret;
				}
				return inter_copy_val_str( &sb->val );
			}

		case ET_ARRAY:
			/* array access */
			{
				struct Symbol *sb = inter_lookup_symbol( env, node->attr.val.sval );
				if( sb == 0 )
				{
					env->inter_log( node->lineno, ">>runtime error->undefine array" );
					return ret;
				}
				else
				{
					struct Value index = inter_expression( env, node->child[0] );
					if( index.type != SB_VAR_NUM || index.dval < 0 || index.dval >= sb->val.size )
					{
						env->inter_log( node->child[0]->lineno, ">>runtime error->invalid array index" );
					}
					return inter_copy_val_str( &sb->val.aval[(size_t)index.dval] );
				}
			}

		case ET_FUNC_CALL:
			{
				return inter_func_call_exp( env, node );
			}
			break;	

		default:
			env->inter_log( node->lineno, ">>runtime error->unsupport exp type" );	
	}
	ret.address = 0;
	ret.type = SB_VAR_NUM;
	return ret;
}

static void inter_exp_stmt( struct interEnv *env, struct treeNode *tree )
{
	struct Value val = inter_expression( env, tree );
	inter_free_val_str( &val );
}

static struct FuncRet inter_if_stmt( struct interEnv *env, struct treeNode *tree )
{
	struct FuncRet ret = { { { 0 }, SB_VAR_NUM }, ER_NORMAL };
	struct Value exp = inter_expression( env, tree->child[0] );
	if( exp.type != SB_VAR_NUM )
	{
		inter_free_val_str( &exp );
		env->inter_log( tree->child[0]->lineno, ">>runtime error->if expression must be a number result" );
		return ret;
	}
	if( (int)exp.dval )
	{
		ret = inter_statements( env, tree->child[1] );	
	}
	else if( tree->child[2] != 0 )
	{
		ret = inter_statements( env, tree->child[2] );
	}
	return ret;
}

static struct FuncRet inter_while_stmt( struct interEnv *env, struct treeNode *tree )
{
	struct FuncRet ret = { { { 0 }, SB_VAR_NUM }, ER_NORMAL };
	struct Value exp = inter_expression( env, tree->child[0] );
	if( exp.type != SB_VAR_NUM )
	{
		inter_free_val_str( &exp );
		env->inter_log( tree->child[0]->lineno, ">>runtime error->while expression must be a number result" );
		return ret;
	}
	while( (int)exp.dval )
	{
		ret = inter_statements( env, tree->child[1] );
		if( ret.type == ER_RETURN || ret.type == ER_BREAK )
		{
			break;
		}
		exp = inter_expression( env, tree->child[0] );
	}
	return ret;	
}

static struct FuncRet inter_for_stmt( struct interEnv *env, struct treeNode *node )
{
	struct FuncRet ret = { { { 0 }, SB_VAR_NUM }, ER_NORMAL };
	struct Value exp;
	/* initial expression */
	inter_expression( env, node->child[0] );
	exp = inter_expression( env, node->child[1] );
	if( exp.type != SB_VAR_NUM )
	{
		inter_free_val_str( &exp );
		env->inter_log( node->child[1]->lineno, ">>runtime error->for expression must be a number result" );
		return ret;
	}
	while( (int)exp.dval )
	{
		ret = inter_statements( env, node->child[3] );
		if( ret.type == ER_RETURN || ret.type == ER_BREAK )
		{
			break;
		}
		/* the third expression of 'for' */
		inter_expression( env, node->child[2] );
		/* the second expression: bool expression */
		exp = inter_expression( env, node->child[1] );
	}
	return ret;

}

static struct FuncRet inter_return_stmt( struct interEnv *env, struct treeNode *node )
{
	struct FuncRet ret = { { { 0 }, SB_VAR_NUM }, ER_RETURN };
	ret.val = inter_expression( env, node->child[0] );
	return ret;
}

static void inter_array_def_stmt( struct interEnv *env, struct treeNode *node )
{
	struct Value val = inter_expression( env, node->child[0] );
	if( val.type != SB_VAR_NUM || val.dval <= 0 )
	{
		env->inter_log( node->child[0]->lineno, ">>runtime error->invalid array size" );
	}
	else
	{
		sym_insert_array( env->local_st, node->attr.val.sval, (size_t)val.dval );
	}
}

static struct FuncRet inter_statements( struct interEnv *env, struct treeNode *node )
{
	struct FuncRet ret = { { { 0 }, SB_VAR_NUM }, ER_NORMAL };
	for( ; node != 0; node = node->sibling )
	{
		if( node->type == NT_STMT )
		{
			switch( node->subtype.stmt )
			{
				case ST_IF:
					ret = inter_if_stmt( env, node );
					break;

				case ST_WHILE:
					ret = inter_while_stmt( env, node );
					break;

				case ST_FOR:
					ret = inter_for_stmt( env, node );
					break;

				case ST_RETURN:
					ret = inter_return_stmt( env, node );
					break;

				case ST_BREAK:
					ret.type = ER_BREAK;
					break;

				case ST_ARRAY_DEF:
					inter_array_def_stmt( env, node );
					break;

				default:
					break;	
			}

			if( ret.type == ER_RETURN || ret.type == ER_BREAK )
			{
				break;
			}
		}	
		else if( node->type == NT_EXP )
		{
			/* expression statement*/
			inter_exp_stmt( env, node );
		}
	}
	
	return ret;
}

static void inter_build_string_st( struct interEnv *env, struct treeNode *root )
{
	int i;
	if( root == 0 )
	{
		return;
	}
	if( root->type == NT_EXP && root->subtype.exp == ET_STRING )
	{
		char name[8];
		struct Value val;
	   	val.sval = root->attr.val.sval;
		val.type = SB_VAR_STRING;	
		/* make the address as the symbol name */
		itoa( (int) root->attr.val.sval, name, 10 );
		sym_insert( env->global_st, name, val );
	}

	for( i = 0; i < MAXCHILDREN; ++ i )
	{
		inter_build_string_st( env, root->child[i] );
	}	
	inter_build_string_st( env, root->sibling );
}
		
void inter_build_global_st( struct interEnv *env, struct treeNode *root )
{
	struct treeNode *node;
	for( node = root; node != 0; node = node->sibling )
	{
		if( node->type == NT_STMT )
		{
			if( node->subtype.stmt == ST_VAR_DEF )
			{
				struct Value val = { { 0 }, SB_VAR_NUM };
				if( node->child[0] != 0 )
				{
			    	val	= inter_expression( env, node->child[0] ); 
				}
				sym_insert( env->global_st, node->attr.val.sval, val );				
			}
			else if( node->subtype.stmt == ST_ARRAY_DEF )
			{
				struct Value val;
				val = inter_expression( env, node->child[0] );
				if( val.type != SB_VAR_NUM || val.dval <= 0 )
				{
					env->inter_log( node->child[0]->lineno, ">>runtime error->invalid array size" );
				}
				else
				{
					sym_insert_array( env->global_st, node->attr.val.sval, (size_t)val.dval );
				}
			}
			else if( node->subtype.stmt == ST_FUNC_DEF )
			{
				struct Value val;
				val.address = node;
				val.type = SB_FUNC;
				sym_insert( env->global_st, node->attr.val.sval, val );
			}
			else
			{
				env->inter_log( node->lineno, ">>runtime error->error statement in global scope" );
			}
		}
		else
		{
			env->inter_log( node->lineno, ">>runtime error->global scope must be var/func def" );
		}
	}

	/* build the string symbol table */
	inter_build_string_st( env, root );
}

struct Value inter_function( struct interEnv *env, struct treeNode *tree )
{
	struct FuncRet ret = { { { 0 }, SB_VAR_NUM }, ER_NORMAL };
	ret = inter_statements( env, tree->child[1] );
	return ret.val;	
}

struct Value inter_call_func( struct interEnv *env, struct treeNode *root )
{
	return inter_function( env, root );
}

int inter_execute( struct interEnv *env )
{
	struct Symbol *main_func;
	main_func = sym_lookup( env->global_st, "main" );
	if( main_func != 0 )
	{
		env->local_st = sym_new();
		inter_function( env, (struct treeNode*) main_func->val.address );
		sym_free( env->local_st );
	}
	return 0;
}

struct interEnv *inter_env_new( inter_log_func log_func, struct symTable *plugin_st )
{
	struct interEnv *env = (struct interEnv*) malloc( sizeof( struct interEnv ) );
	env->inter_log = log_func;
	env->global_st = sym_new();
	env->plugin_st = plugin_st;
	env->local_st = 0;
	return env;
}

void inter_env_free( struct interEnv *env )
{
	sym_free( env->global_st );
	free( env );
}


