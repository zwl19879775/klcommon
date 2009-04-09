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
static void *dl_load( const char *file )
{
	return LoadLibraryA( file );
}

static void dl_unload( void *lib )
{
	FreeLibrary( (HINSTANCE) lib );
}

static void *dl_sym( void *lib, const char *sym_name )
{
	return GetProcAddress( (HINSTANCE) lib, sym_name );
}

#elif defined _UNIX
#include <dlfcn.h>
#define dl_load( file ) dlopen( (const char*) file, RTLD_NOW )
#define dl_unload( lib ) dlclose( lib )
#define dl_sym( lib, sym_name ) dlsym( lib, sym_name )
#endif

static void *loader_load( struct klState *kl, const char *path )
{
	void *lib = dl_load( path );
	if( lib == 0 )
	{
		kl->log( 0, ">>lib error->load library file [%s] failed.", path );
	}
	return lib;
}

static void loader_unload( void *lib )
{
	dl_unload( lib );
}

static void *loader_getsym( struct klState *kl, void *lib, const char *sym_name )
{
	void *sym = dl_sym( lib, sym_name ); 
	if( sym == 0 )
	{
		kl->log( 0, ">>lib error->Symbol [%s] not found.", sym_name );
	}
	return sym;
}

static struct TValue loader_import( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	struct klState *kl = (struct klState*)(long) kl_check_number( &arg );
	if( kl != 0 )
	{
		void *lib = loader_load( kl, kl_check_string( &arg ) );
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
	void *lib = (void*) (long) kl_check_number( &arg );
	if( lib != 0 )
	{
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

