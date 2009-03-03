/**
 * @file kllibbase.c
 * @author Kevin Lynx
 * @brief to provide some basic functions in the script
 */
#include "kllib.h"
#include "kllibloader.h"
#include <stdio.h>

#ifdef _WIN32
#include <windows.h>

static void *loader_load( struct klState *kl, const char *path )
{
	HINSTANCE lib = LoadLibraryA( path );
	if( lib == 0 )
	{
		kl->log( 0, ">>lib error->load library file [%s] failed.", path );
	}
	return lib;
}

static void loader_unload( void *lib )
{
	FreeLibrary( (HINSTANCE) lib );
}

static void *loader_getsym( struct klState *kl, void *lib, const char *sym_name )
{
	void *sym = GetProcAddress( (HINSTANCE) lib, sym_name );
	if( sym == 0 )
	{
		kl->log( 0, ">>lib error->Symbol [%s] not found.", sym_name );
	}
	return sym;
}
#endif

static struct TValue loader_import( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	struct klState *kl;
	if( arg->type == NUMBER )
	{
		kl = (struct klState*) (long)arg->dval;
	}
	kl_next_arg( arg );
	if( arg->type == STRING )
	{
		void *lib = loader_load( kl, arg->sval );
		void *sym;
		if( lib == 0 )
		{
			return ret;
		}
		ret.dval = (long) lib;
		sym = loader_getsym( kl, lib, "lib_open" );
		if( sym != 0 )
		{
			typedef void (*lib_open)( struct klState *kl );
			lib_open fn = (lib_open) sym;
			fn( kl );
		}	
	}

	return ret;
}

static struct TValue loader_unimport( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	if( arg->type == NUMBER )
	{
		void *lib = (void*) (long) ret.dval;
		loader_unload( lib );
	}
	return ret;
}

int kllib_open_loader( struct klState *kl )
{
	kl_register( kl, loader_import, "import" );
	kl_register( kl, loader_unimport, "unimport" );
	return 0;
}

