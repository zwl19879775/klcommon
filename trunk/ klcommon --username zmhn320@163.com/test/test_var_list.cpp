///
/// @file test_var_list.cpp
/// 
#include "kl_var_list.h"
#include <stdio.h>

using namespace kl_common;

struct Obj
{
	int _value;
	Obj( int v ) : _value( v ) 
	{
	}
};

int main()
{
	var_list *vl = new var_list();
	vl->add( 2 );
	vl->add( Obj( 2 ) );
	
	int a = vl->get<int>( 0 );
	printf( "%d\n", a );
	Obj &o = vl->get<Obj>( 1 );
	o._value = 10;
	printf( "%d\n", o._value );
	
	const var_list *cvl = vl;
	const Obj &o2 = cvl->get<Obj>( 1 );
	
	delete vl;
	return 0;
}