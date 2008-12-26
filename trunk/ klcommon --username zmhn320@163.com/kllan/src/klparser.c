/**
 *
 *
 */
#include "klparser.h"
#include "kllex.h"
#include <stdio.h>

/**
 * create a new expression node 
 */
static struct treeNode *syn_new_exp_node( ExpType t );

/**
 * create a new statement node 
 */
static struct treeNode *syn_new_stmt_node( StmtType t );

/**
 * match the current token to the expected token, if matched ok, get the next token.
 */
static void syn_match( struct lexState *ls, int token );

static struct treeNode *syn_definition( struct lexState *ls );
static struct treeNode *syn_var_def( struct lexState *ls );
static struct treeNode *syn_func_def( struct lexState *ls );
static struct treeNode *syn_param_list( struct lexState *ls );
static struct treeNode *syn_param( struct lexState *ls );

/*************************************************************************************/
static struct treeNode *syn_new_exp_node( ExpType t )
{
	struct treeNode *node = (struct treeNode*) malloc( sizeof( struct treeNode ) );
	node->type = NT_EXP;
	node->subtype.exp = t;
	node->sibling = 0;
	memset( node->child, 0, sizeof( node->child[0] ) * MAXCHILDREN );
	return node;
}

static struct treeNode *syn_new_stmt_node( StmtType t )
{
	struct treeNode *node = (struct treeNode*) malloc( sizeof( struct treeNode ) );
	node->type = NT_STMT;
	node->subtype.stmt = t;
	node->sibling = 0;
	memset( node->child, 0, sizeof( node->child[0] ) * MAXCHILDREN );
	return node;
}

static void syn_match( struct lexState *ls, int token )
{
	if( lex_current( ls ) == token )
	{
		lex_token( ls );
	}
	else if( lex_current( ls ) != TK_ERROR )
	{
		ls->lex_error( ls, "syntax error : unexpcted token -> %s", lex_current_str( ls ) );
	}
	else
	{
		ls->lex_error( ls, "lex error" );
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
		else if( token == TK_FUNCTION )
		{
			node = syn_func_def( ls )i;
		}

		if( root == 0 )
		{
			root = prev = node;
		}
		else
		{
			prev->sibling = node;
			prev = node;
		}
		token = lex_current( ls );
	}
	return root;
}

static struct treeNode *syn_var_def( struct lexState *ls )
{
	struct treeNode *node = syn_new_stmt_node( ST_VAR_DEF );
	/* not necessary to copy the string, free the token string later */
	node->attr.val.sval = lex_current_str( ls );
	syn_match( TK_ID );
	if( lex_current( ls ) == '[' )
	{
		syn_match( '[' );
		node->child[0] = syn_expression( ls );
		syn_match( ']' );
	}
	syn_match( ';' );

	return node;
}

static struct treeNode *syn_func_def( struct lexState *ls )
{
	struct treeNode *node = syn_new_stmt_node( ST_FUNC_DEF );
	syn_match( TK_FUNCTION );
	node->attr.val.sval = lex_current_str( ls );
	syn_match( TK_ID );
	syn_match( '(' );
	node->child[0] = syn_param_list( ls );
	/* TODO: to parse the statement list for this function */

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
	}

	return first;
}

static struct treeNode *syn_param( struct lexState *ls )
{
	struct treeNode *node = syn_new_stmt_node( ST_PARAM_DEF );
	node->attr.val.sval = lex_current_str( ls );
	syn_match( TK_ID );
	if( lex_current( ls ) == '[' )
	{
		syn_match( '[' );
		node->child[0] = syn_expression( ls );
		syn_match( ']' );
	}
	return node;
}

struct treeNode *syn_parse( struct lexState *ls )
{
	lex_token( ls );
	return syn_definition( ls );
}


