/**
 * parse.c
 * this module can construct a NFA graph from a simple regular expression.
 * the expression only support : *, ., |, () 
 * and the NFA has only one final state, one start state of cource.
 * kevin lynx
 * 2.15.2010
 */
#include <stdio.h>
#include "scan.h"
#include "state.h"

struct state_pair expr();

static int match( int cur_tok, int tok )
{
	if( cur_tok != tok )
	{
		fprintf( stderr, "unexprected token : %c\n", (char) cur_tok );
		return -1;
	}
	else
	{
		return scan_next();
	}
}

/* closure */
struct state_pair factor()
{
	struct state_pair sp = { 0 };
	if( lex_type != CONCAT && lex_type != '(' )
	{
		return sp;
	}
	if( lex_type == '(' )
	{
		sp = expr();
		match( lex_type, ')' );
	}
	else
	{
		struct state *start = state_new();
		struct state *final = state_new();
		state_link( start, final, (char) lex_value );
		sp.start = start;
		sp.final = final;

		printf( "%c", (char) lex_value );	
		match( lex_type, CONCAT );
	}
	if( lex_type != CLOSURE )
	{
		/*scan_reverse();*/
	}
	else
	{
		struct state *start = state_new();
		struct state *final = state_new();
		state_link( start, sp.start, E_TOK );
		state_link( sp.final, final, E_TOK );
		state_link( sp.final, sp.start, E_TOK );
		state_link( start, final, E_TOK );	
		sp.start = start;
		sp.final = final;
		match( lex_type, CLOSURE );
		printf( "*" );
	}

	return sp;
}

/* concat */
struct state_pair term()
{
	struct state_pair sp = factor();
	while( lex_type == CONCAT ||
		   lex_type == '(' )
	{
		/*match( lex_type, CONCAT );*/
		printf( "." );
		struct state_pair loc_sp = factor();
		state_link( sp.final, loc_sp.start, E_TOK );
		sp.final = loc_sp.final;
	}
	return sp;
}

/* union */
struct state_pair expr()
{
	struct state_pair sp = { 0 };
	scan_next();
	struct state_pair loc_sp = term();
	if( lex_type == UNION )
	{
		sp.start = state_new();
		sp.final = state_new();
		state_link( sp.start, loc_sp.start, E_TOK );
		state_link( loc_sp.final, sp.final, E_TOK );
	}
	else
	{
		sp = loc_sp;
	}
	while( lex_type == UNION )
	{
		match( lex_type, UNION );
		printf( "|" );
		loc_sp = term();
		state_link( sp.start, loc_sp.start, E_TOK );
		state_link( loc_sp.final, sp.final, E_TOK );
	}
	return sp;
}

struct state_pair parse( const char *str )
{
	struct state_pair sp = { 0 };
	scan_init( str );
	sp = expr();
	return sp;
}

