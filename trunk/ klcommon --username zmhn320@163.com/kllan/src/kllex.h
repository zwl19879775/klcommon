/**
 * @file kllex.h
 * @brief to process lex for kl source
 * @author Kevin Lynx
 */
#ifndef ___KL_LEX_H_
#define ___KL_LEX_H_

#include <stddef.h>

#define STRING_MAX_LEN 2056
#define NUMBER_MAX_LEN 256
#define ID_MAX_LEN 256

/**
 * token values
 */
enum
{
	TK_ERROR = -1,
	TK_EOF,
	TK_ID, TK_NUM, TK_FLOAT, TK_CHAR, TK_STRING,
	TK_IF, TK_ELSE, TK_WHILE, TK_DO, TK_RETURN, TK_BREAK, TK_FUNCTION,
    TK_NE, TK_EQ, TK_LE, TK_GE, TK_OR, TK_AND,  
};

struct lexState;
/**
 * lex error function prototype
 */
typedef void (*lex_errorfn)( struct lexState *ls, const char *msg );

/**
 * token type
 */
struct Token
{
	int type;
	char *string;
};

/**
 * lexState
 */
struct lexState
{
	size_t lineno;
	char *source;
	size_t current;
	struct Token token;
  	lex_errorfn lex_error;
};


/**
 * return the current token
 */
#define lex_current( ls ) ( (struct lexState*)(ls)->token.type )

/**
 * return the current token string 
 */
#define lex_current_str( ls ) ( (struct lexState*)(ls)->token.string )

/**
 * scan the next token and save it to the lexState.
 * everytime you get the token and you must free the token.string yourself.
 */
int lex_token( struct lexState *ls );

/**
 * set input source to a lex state, and initialize the state.
 * keep the source memory valid before the token scanning finished. 
 */
void lex_setinput( struct lexState *ls, char *source, lex_errorfn lex_error );
#endif 

