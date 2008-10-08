///
/// @file test_ref.cpp
/// @brief to test kl_ref
///
#include "kl_ref.h"
#include <stdio.h>

using namespace kl_common;

int main()
{
	int a = 2;
	ref_wrapper<int> refw = ref( a );
	if( is_ref_wrapper( refw ) )
	{
		printf( "is a ref_wrapper\n" );
	}
	
	if( is_ref_wrapper( a ) )
	{
		printf( "is a ref_wrapper\n" );
	}
	
	unwrap_ref<int>::type b = 2;
	unwrap_ref<ref_wrapper<int> >::type c = 2;
	
	return 0;
}