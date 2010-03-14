/**
 * test string match
 * kevin lynx
 * 2.15.2010
 */
#include <stdio.h>

#ifdef NFA_MATCH
extern int nfa_match( const char *reg_expr, const char *match_str );
#else
extern int dfa_match( const char *reg_expr, const char *match_str );
#endif

int main( int argc, char **argv )
{
	if( argc < 3 )
	{
		fprintf( stderr, "Usage: %s reg_expr string.\n", argv[0] );
		return -1;
	}
#ifdef NFA_MATCH
	if( nfa_match( argv[1], argv[2] ) )
#else
	if( dfa_match( argv[1], argv[2] ) )
#endif
	{
		printf( "\n%s match %s\n", argv[1], argv[2] );
	}
	else
	{
		printf( "\n%s does not match %s\n", argv[1], argv[2] );
	}
	return 0;
}

