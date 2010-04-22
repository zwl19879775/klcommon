/** 
 * @file klparser.h
 * @author Kevin Lynx
 * @brief
 *
 */
#include "klparser.h"
#include "kllex.h"
#include <stdio.h>
#include <malloc.h>
#include <memory.h>

/**
 * create a new expression node 
 */
static struct treeNode *syn_new_exp_node( ExpType t, size_t lineno );

/**
 * create a new statement node 
 */
static struct treeNode *syn_new_stmt_node( StmtType t, size_t lineno );

/**
 * match the current token to the expected token, if matched ok, get the next token.
 */
static void syn_match( struct lexState *ls, int token );

/**
 * these functions below obey the BNF rules.
 */
static struct treeNode *syn_definition( struct lexState *ls );
static struct treeNode *syn_var_def( struct lexState *ls );
static struct treeNode *syn_array_def( struct lexState *ls );
static struct treeNode *syn_func_def( struct lexState *ls );
static struct treeNode *syn_param_list( struct lexState *ls );
static struct treeNode *syn_param( struct lexState *ls );
static struct treeNode *syn_factor( struct lexState *ls );
static struct treeNode *syn_term( struct lexState *ls );
static struct treeNode *syn_add_exp( struct lexState *ls );
static struct treeNode *syn_relop_exp( struct lexState *ls );
static struct treeNode *syn_simple_exp( struct lexState *ls );
static struct treeNode *syn_var( struct lexState *ls );
static struct treeNode *syn_logic_exp( struct lexState *ls );
static struct treeNode *syn_expression( struct lexState *ls );
static struct treeNode *syn_exp_stmt( struct lexState *ls );
static struct treeNode *syn_statement( struct lexState *ls );
static struct treeNode *syn_statement_list( struct lexState *ls );
static struct treeNode *syn_compound_stmt( struct lexState *ls );
static struct treeNode *syn_if_stmt( struct lexState *ls );
static struct treeNode *syn_while_stmt( struct lexState *ls );
static struct treeNode *syn_for_stmt( struct lexState *ls );
static struct treeNode *syn_return_stmt( struct lexState *ls );
static struct treeNode *syn_break_stmt( struct lexState *ls );
static struct treeNode *syn_args( struct lexState *ls );

/*************************************************************************************/
static struct treeNode *syn_new_exp_node( ExpType t, size_t lineno )
{
	struct treeNode *node = (struct treeNode*) malloc( sizeof( struct treeNode ) );
	node->type = NT_EXP;
	node->subtype.exp = t;
	node->sibling = 0;
	node->lineno = lineno;
	memset( node->child, 0, sizeof( node->child[0] ) * MAXCHILDREN );
	return node;
}

static struct treeNode *syn_new_stmt_node( StmtType t, size_t lineno )
{
	struct treeNode *node = (struct treeNode*) malloc( sizeof( struct treeNode ) );
	node->type = NT_STMT;
	node->subtype.stmt = t;
	node->sibling = 0;
	node->lineno = lineno;
	memset( node->child, 0, sizeof( node->child[0] ) * MAXCHILDREN );
	return node;
}

static void syn_match( struct lexState *ls, int token )
{
	char *s = lex_current_str( ls );
	if( lex_current( ls ) == token )
	{
		lex_token( ls );
	}
	else if( lex_current( ls ) != TK_ERROR )
	{
		ls->lex_error( ls->lineno, ">>syntax error->unexpcted token-> %s (expect->%s)", lex_current_str( ls ),
			   lex_token_desc( token ) );
		lex_token( ls );
	}
	else
	{
		ls->lex_error( ls->lineno, ">>syntax error->invalid token : %s (expect->%s)", lex_current_str( ls ),
			   lex_token_desc( token ) );
		lex_token( ls );
	}

	if( lex_current( ls ) != TK_EOF && token != TK_ID && token != TK_STRING && s != 0 )
	{
		free( s );
	}
}

static void syn_to_const( struct treeNode *node, struct lexState *ls )
{
	char *str = lex_current_str( ls );
	switch( lex_current( ls ) )
	{
		case TK_NUM:
		case TK_FLOAT:
			sscanf( str, "%lf", (double*)&node->attr.val.dval ); 
			break;

		case TK_CHAR:
			{
				char c;
				sscanf( str, "%c", &c );
				node->attr.val.dval = c;
			}
			break;

		case TK_STRING:
			node->attr.val.sval = str;
		   break;
	}		   
}

static struct treeNode *syn_definition( struct lexState *ls )
{
	int token = lex_current( ls );
	struct treeNode *node = 0, *root = 0, *prev = 0;
	while( token != TK_EOF && token != TK_ERROR )
	{
		if( token == TK_ID )
		{
			node = syn_var_def( ls );
		}
		else if( token == TK_ARRAY )
		{
			node = syn_array_def( ls );
		}
		else if( token == TK_FUNCTION )
		{
			node = syn_func_def( ls );
		}
		else
		{
			ls->lex_error( ls->lineno, ">>syntax error->unexpected token-> %s (expect->array/var/func def", 
					lex_current_str( ls ) );
			syn_match( ls, lex_current( ls ) );
			node = 0;
		}

		if( node != 0 )
		{
			if( root == 0 )
			{
				root = prev = node;
			}
			else
			{
				prev->sibling = node;
				prev = node;
			}
		}
		token = lex_current( ls );
	}
	return root;
}

static struct treeNode *syn_var_def( struct lexState *ls )
{
	struct treeNode *node = syn_new_stmt_node( ST_VAR_DEF, ls->lineno );
	node->attr.val.sval = lex_current_str( ls );
	syn_match( ls, TK_ID );
	if( lex_current( ls ) == '=' )
	{
		syn_match( ls, '=' );
		node->child[0] = syn_expression( ls );
	}
	syn_match( ls, ';' );

	return node;
}

static struct treeNode *syn_array_def( struct lexState *ls )
{
	struct treeNode *node = syn_new_stmt_node( ST_ARRAY_DEF, ls->lineno );
	syn_match( ls, TK_ARRAY );
	node->attr.val.sval = lex_current_str( ls );
	syn_match( ls, TK_ID );
	syn_match( ls, '[' );
	node->child[0] = syn_expression( ls );
	syn_match( ls, ']' );
	syn_match( ls, ';' );
	return node;
}

static struct treeNode *syn_func_def( struct lexState *ls )
{
	struct treeNode *node = syn_new_stmt_node( ST_FUNC_DEF, ls->lineno );
	syn_match( ls, TK_FUNCTION );
	node->attr.val.sval = lex_current_str( ls );
	syn_match( ls, TK_ID );
	syn_match( ls, '(' );
	node->child[0] = syn_param_list( ls );
	syn_match( ls, ')' );
	node->child[1] = syn_compound_stmt( ls );	
	return node;
}

static struct treeNode *syn_param_list( struct lexState *ls )
{
	struct treeNode *node = 0, *first = 0, *prev = 0;
	while( lex_current( ls ) != ')' )
	{
		node = syn_param( ls );
		if( first == 0 )
		{
			first = prev = node;
		}
		else
		{
			prev->sibling = node;
			prev = node;
		}
		if( lex_current( ls ) != ')' )
		{
			syn_match( ls, ',' );
		}
	}

	return first;
}

static struct treeNode *syn_param( struct lexState *ls )
{
	struct treeNode *node = syn_new_stmt_node( ST_PARAM_DEF, ls->lineno );
	node->attr.val.sval = lex_current_str( ls );
	syn_match( ls, TK_ID );
	if( lex_current( ls ) == '[' )
	{
		syn_match( ls, '[' );
		node->child[0] = syn_expression( ls );
		syn_match( ls, ']' );
	}
	return node;
}

static struct treeNode *syn_compound_stmt( struct lexState *ls )
{
	struct treeNode *node = 0;
	syn_match( ls, '{' );
	node = syn_statement_list( ls );
	syn_match( ls, '}' );
	return node;
}

static struct treeNode *syn_statement_list( struct lexState *ls )
{
	struct treeNode *prev = 0, *ret_node = 0, *node = 0;
	int token = lex_current( ls );
	while( token != '}' && token != TK_EOF && token != TK_ERROR )
	{
		node = syn_statement( ls );	
		if( node != 0 ) /* the node maybe nil because of syntax error */
		{
			if( prev == 0 )
			{
				prev = ret_node = node;
			}
			else
			{
				prev->sibling = node;
				prev = node;
			}
			/* because 'syn_statement' may create a list of statement, so here
			 * should adjust the 'prev' pointer */
			while( prev->sibling != 0 )
			{
				prev = prev->sibling;
			}
		}
		token = lex_current( ls );
	}

	return ret_node;
}

static struct treeNode *syn_statement( struct lexState *ls )
{
	int token = lex_current( ls );
	struct treeNode *node = 0;

	switch( token )
	{
		case '{':
			node = syn_compound_stmt( ls );
			break;

		case TK_IF:
			node = syn_if_stmt( ls );
			break;

		case TK_WHILE:
			node = syn_while_stmt( ls );
			break;

		case TK_FOR:
			node = syn_for_stmt( ls );
			break;

		case TK_RETURN:
			node = syn_return_stmt( ls );
			break;

		case TK_BREAK:
			node = syn_break_stmt( ls );
			break;

		case TK_ARRAY:
			node = syn_array_def( ls );
			break;

		default:
			node = syn_exp_stmt( ls );
	}

	return node;
}

static struct treeNode *syn_if_stmt( struct lexState *ls )
{
	struct treeNode *if_node = syn_new_stmt_node( ST_IF, ls->lineno );
	syn_match( ls, TK_IF );
	syn_match( ls, '(' );
	if_node->child[0] = syn_expression( ls );
	syn_match( ls, ')' );
	if( lex_current( ls ) == '{' )
	{
		if_node->child[1] = syn_compound_stmt( ls );
	}
	else
	{
		if_node->child[1] = syn_statement( ls );
	}
	if( lex_current( ls ) == TK_ELSE )
	{
		syn_match( ls, TK_ELSE );
		if( lex_current( ls ) == '{' )
		{
			if_node->child[2] = syn_compound_stmt( ls );
		}
		else
		{
			if_node->child[2] = syn_statement( ls );
		}
	}

	return if_node;
}

static struct treeNode *syn_while_stmt( struct lexState *ls )
{
	struct treeNode *it_node = syn_new_stmt_node( ST_WHILE, ls->lineno );
	syn_match( ls, TK_WHILE );
	syn_match( ls, '(' );
	it_node->child[0] = syn_expression( ls );
	syn_match( ls, ')' );
	if( lex_current( ls ) == '{' )
	{
		it_node->child[1] = syn_compound_stmt( ls );
	}
	else
	{
		it_node->child[1] = syn_exp_stmt( ls );
	}

	return it_node;
}

static struct treeNode *syn_for_stmt( struct lexState *ls )
{
	struct treeNode *it_node = syn_new_stmt_node( ST_FOR, ls->lineno );
	syn_match( ls, TK_FOR );
	syn_match( ls, '(' );
	it_node->child[0] = syn_expression( ls );
	syn_match( ls, ';' );
	it_node->child[1] = syn_expression( ls );
	syn_match( ls, ';' );
	it_node->child[2] = syn_expression( ls );
	syn_match( ls, ')' );
	if( lex_current( ls ) == '{' )
	{
		it_node->child[3] = syn_compound_stmt( ls );
	}
	else
	{
		it_node->child[3] = syn_exp_stmt( ls );
	}
	return it_node;
}

static struct treeNode *syn_return_stmt( struct lexState *ls )
{
	struct treeNode *rnode = syn_new_stmt_node( ST_RETURN, ls->lineno );
	syn_match( ls, TK_RETURN );
	if( lex_current( ls ) == '(' )
	{
		syn_match( ls, '(' );
		rnode->child[0] = syn_expression( ls );
		syn_match( ls, ')' );
	}
	else
	{
		rnode->child[0] = syn_expression( ls );
	}
	syn_match( ls, ";" );
	return rnode;
}

static struct treeNode *syn_break_stmt( struct lexState *ls )
{
	struct treeNode *node = syn_new_stmt_node( ST_BREAK, ls->lineno );
	syn_match( ls, TK_BREAK );
	syn_match( ls, ';' );
	return node;
}

static struct treeNode *syn_exp_stmt( struct lexState *ls )
{
	struct treeNode *node = syn_expression( ls );
	syn_match( ls, ';' );
	return node;
}

static struct treeNode *syn_expression( struct lexState *ls )
{
	return syn_simple_exp( ls );
}

static struct treeNode *syn_var( struct lexState *ls )
{
	struct treeNode *node = syn_new_exp_node( ET_ID, ls->lineno );
	node->attr.val.sval = lex_current_str( ls );
	syn_match( ls, TK_ID );
	return node;
}

static struct treeNode *syn_simple_exp( struct lexState *ls )
{
	struct treeNode *node = syn_logic_exp( ls );
	int token = lex_current( ls );
	while( token == '=' )
	{
		struct treeNode *op_node = syn_new_exp_node( ET_OP, ls->lineno );
		op_node->attr.op = token;
		op_node->child[0] = node;
		syn_match( ls, token );
		op_node->child[1] = syn_logic_exp( ls );
		node = op_node;
		token = lex_current( ls );
	}
	return node;
}

static struct treeNode *syn_logic_exp( struct lexState *ls )
{
	struct treeNode *node = syn_relop_exp( ls );
	int token = lex_current( ls );
	while( token == TK_OR || token == TK_AND )
	{
		struct treeNode *op_node = syn_new_exp_node( ET_OP, ls->lineno );
		op_node->attr.op = token;
		op_node->child[0] = node;
		syn_match( ls, token );
		op_node->child[1] = syn_relop_exp( ls );
		node = op_node;
		token = lex_current( ls );
	}
	return node;
}

static struct treeNode *syn_relop_exp( struct lexState *ls )
{
	struct treeNode *node = syn_add_exp( ls );
	int token = lex_current( ls );
	while( token == '<' || token == '>' || token == TK_LE || token == TK_GE || 
			token == TK_NE || token == TK_EQ )
	{
		struct treeNode *op_node = syn_new_exp_node( ET_OP, ls->lineno );
		op_node->attr.op = token;
		op_node->child[0] = node;
		syn_match( ls, token );
		op_node->child[1] = syn_add_exp( ls );
		node = op_node;
		token = lex_current( ls );
	}
	return node;
}

static struct treeNode *syn_add_exp( struct lexState *ls )
{
	struct treeNode *node = syn_term( ls );
	int token = lex_current( ls );
	while( token == '+' || token == '-' )
	{
		struct treeNode *op_node = syn_new_exp_node( ET_OP, ls->lineno );
		op_node->attr.op = token;
		op_node->child[0] = node;
		syn_match( ls, token );
		op_node->child[1] = syn_term( ls );
		node = op_node;
		token = lex_current( ls );
	}
	return node;
}

static struct treeNode *syn_term( struct lexState *ls )
{
	struct treeNode *node = syn_factor( ls );
	int token = lex_current( ls );
	while( token == '*' || token == '/' || token == '%' )
	{	
		struct treeNode *op_node = syn_new_exp_node( ET_OP, ls->lineno );
		op_node->attr.op = token;
		op_node->child[0] = node;
		syn_match( ls, token );
		op_node->child[1] = syn_factor( ls );
		node = op_node;
		token = lex_current( ls );
	}
	return node;
}

static struct treeNode *syn_factor( struct lexState *ls )
{
	struct treeNode *node = 0;
	switch( lex_current( ls ) )
	{
		case '(':
			syn_match( ls, '(' );
			node = syn_expression( ls );
			syn_match( ls, ')' );
			break;

		case TK_ID:
			{
				node = syn_var( ls );
				if( lex_current( ls ) == '(' )
				{
					node->subtype.exp = ET_FUNC_CALL;
					syn_match( ls, '(' );
					/* function call, child[0] is the index of the id if it's a syntax error */
					node->child[1] = syn_args( ls );
					syn_match( ls, ')' );
				}
				else if( lex_current( ls ) == '[' )
				{
					node->subtype.exp = ET_ARRAY;
					syn_match( ls, '[' );
					node->child[0] = syn_expression( ls );
					syn_match( ls, ']' );
				}
			}
			break;

		case TK_FLOAT:
		case TK_NUM:
		case TK_CHAR:
			{
				
				node = syn_new_exp_node( ET_CONST, ls->lineno );
				syn_to_const( node, ls );	
				syn_match( ls, lex_current( ls ) );
			}
			break;

		case TK_STRING:
			{
				node = syn_new_exp_node( ET_STRING, ls->lineno );
				syn_to_const( node, ls );
				syn_match( ls, lex_current( ls ) );
			}
			break;
	
		case '-':
		case '!':
			{
				node = syn_new_exp_node( ET_OP, ls->lineno );
				node->attr.op = lex_current( ls ) ;
				syn_match( ls, lex_current( ls ) );
				node->child[1] = syn_factor( ls );
			}
			break;	
	
		default:
			ls->lex_error( ls->lineno, ">>syntax error->unexpected token-> %s", lex_current_str( ls ) );
			syn_match( ls, lex_current( ls ) );
	}

	return node;
}

static struct treeNode *syn_args( struct lexState *ls )
{
	struct treeNode *node = 0, *first = 0, *prev = 0;
	while( lex_current( ls ) != TK_ERROR && lex_current( ls ) != TK_EOF && lex_current( ls ) != ')' )
	{
		node = syn_expression( ls );
		if( node != 0 )
		{
			if( first == 0 )
			{
				first = prev = node;
			}
			else
			{
				prev->sibling = node;
				prev = node;
			}
		}
		if( lex_current( ls ) != ')' )
		{
			syn_match( ls, ',' );
		}
	}

	return first;	
}
		
struct treeNode *syn_parse( struct lexState *ls )
{
	lex_token( ls );
	return syn_definition( ls );
}

void syn_free_tree( struct treeNode *tree )
{
	int i;
	if( tree == 0 )
	{
		return ;
	}
	if( tree->type == NT_STMT )
	{
		int stmt = tree->subtype.stmt;
		if( stmt == ST_VAR_DEF || stmt == ST_FUNC_DEF || stmt == ST_PARAM_DEF || stmt == ST_ARRAY_DEF )
		{
			free( tree->attr.val.sval );
		}
	}
	else if( tree->type == NT_EXP )
	{
		int exp = tree->subtype.exp;
		if( exp == ET_ID || exp == ET_FUNC_CALL || exp == ET_ARRAY )
		{
			free( tree->attr.val.sval );
		}	
	}
	
	for( i = 0; i < MAXCHILDREN; ++ i )
	{
		syn_free_tree( tree->child[i] );
	}
	syn_free_tree( tree->sibling );
	free( tree );
}

