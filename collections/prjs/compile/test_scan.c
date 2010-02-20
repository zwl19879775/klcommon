/**
 * test scan module.
 */
#include <stdio.h>
#include "scan.h"

int main( int argc, char **argv )
{
	int ret;
	scan_init( argv[1] );

	do
	{
		ret = scan_next();
		switch( ret )
		{
			case CLOSURE:
				printf( "*" );
				break;

			case UNION:
				printf( "|" );
				break;

			case CONCAT:
				printf( "%c.", (char) lex_value );
				break;

			case DONE:
				break;

			default:
				printf( "%c", (char) ret );
		}
	} while( ret != DONE );	

	return 0;
}

