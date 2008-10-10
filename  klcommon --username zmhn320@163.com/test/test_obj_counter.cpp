///
/// @file test_obj_counter.cpp
///
#include <stdio.h>
#include "kl_obj_counter.h"

class Hunter : public kl_common::obj_counter<Hunter>
{
};

int main()
{
	for( int i = 0; i < 10; ++ i )
	{
		new Hunter();
	}
	
	printf( "count = %u\n", Hunter::count() ); 
	return 0;
}