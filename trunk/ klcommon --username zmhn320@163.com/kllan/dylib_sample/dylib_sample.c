/**
 * dylib_sample.h
 * an example to make a dynamical library importing in script
 * Kevin Lynx
 */
#include "kllib.h"

#ifdef _WIN32
#define EXPORT __declspec( dllexport )
#else
#define EXPORT
#endif

#ifdef __cplusplus
extern "C" {
#endif 

static struct TValue sample_fn( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	if( arg->type == STRING )
	{
		printf( "Sample dynamical library : %s\n", arg->sval );
	}
	return ret;
}

EXPORT void lib_open( struct klState *kl )
{
	kl_register( kl, sample_fn, "sample_fn" );
}

#ifdef __cplusplus
}
#endif

