/**
 * @file klparser.h
 * @author Kevin Lynx
 * @brief
 */
#ifndef ___KL_PARSER_H_
#define ___KL_PARSER_H_

struct lexState ;

/**
 * syntax tree node type
 */
typedef enum
{
	NT_STMT, /* statement */
	NT_EXP, /* expression */
} NodeType;

/**
 * statement type
 */
typedef enum
{
	ST_VAR_DEF,
	ST_FUNC_DEF, ST_PARAM_DEF,
	ST_IF, ST_WHILE, /*ST_ASSIGN*/
	ST_RETURN,
} StmtType;

/**
 * expression type
 */
typedef enum
{
	ET_OP,
	ET_CONST,
	ET_STRING, /* const string */
	ET_ID,
	ET_FUNC_CALL
} ExpType;

/**
 * max children of a node
 */
#define MAXCHILDREN 3

/**
 * syntax tree node
 */
struct treeNode
{
	NodeType type;
	union 
	{
		StmtType stmt;
		ExpType exp;
	} subtype;
	union 
	{
		/* operation type */
		int op;
		union
		{
			double dval;
			char *sval;
		} val;
	} attr;

	struct treeNode *child[MAXCHILDREN];
	struct treeNode *sibling;
};

/**
 * parse a lexState and returns the syntax tree
 *
 */
struct treeNode *syn_parse( struct lexState *ls );

/**
 * free a syntax tree
 */
void syn_free_tree( struct treeNode *tree );

#endif
