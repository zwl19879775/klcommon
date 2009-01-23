/**
 * test cmlc
 */
#include <malloc.h>
#include <stdio.h>
#include "cmlc_malloc.h"

int main()
{
	cmlc_init();
	char *str = (char*) malloc( 100 );
	str = (char*) malloc( 111 );
	return 0;
}
