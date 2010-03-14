/**
 * test parse module
 */
#include <stdio.h>
#include "state.h"

extern struct state *parse( const char * );

int main( int argc, char **argv )
{
	struct state *s = parse( argv[1] );
#ifdef DEBUG
	state_print( s );
#endif
	state_free( s );
	return 0;
}

