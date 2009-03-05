/**
 * @file hge_plugin_font.cpp
 * @author Kevin Lynx
 * @brief adapt hgeFont in kl script
 *
 */
extern "C"
{
#include "kllib.h"
}
#include "hgefont.h"

#define CAST_PTR( dval, type ) (type)(long)(dval)

static struct TValue khgeFontCreate( ArgType arg )
{
	DEF_DEFAULT_VAL( ret );
	const char *file = kl_check_string( &arg );
	if( file != 0 )
	{
		hgeFont *fnt = new hgeFont( file );
		ret.dval = (long) fnt;
	}
	return ret;
}

static struct TValue khgeFontRelease( ArgType arg )
{
	DEF_DEFAULT_VAL( ret );
	hgeFont *fnt = CAST_PTR( kl_check_number( &arg ), hgeFont* );
	if( fnt != 0 )
	{
		delete fnt;
	}
	return ret;
}

static struct TValue khgeFontRender( ArgType arg )
{
	DEF_DEFAULT_VAL( ret );
	hgeFont *fnt = CAST_PTR( kl_check_number( &arg ), hgeFont* );
	if( fnt != 0 )
	{
		float x = (float) kl_check_number( &arg );
		float y = (float) kl_check_number( &arg );
		int align = (int) kl_check_number( &arg );
		const char *s = kl_check_string( &arg );
		fnt->Render( x, y, align, s );
	}
	return ret;
}

static struct TValue khgeFontSetColor( ArgType arg )
{
	DEF_DEFAULT_VAL( ret );
	hgeFont *fnt = CAST_PTR( kl_check_number( &arg ), hgeFont* );
	if( fnt != 0 )
	{
		unsigned long c = (unsigned long) kl_check_number( &arg );
		fnt->SetColor( c );
	}
	return ret;
}

static struct TValue khgeFontGetColor( ArgType arg )
{
	DEF_DEFAULT_VAL( ret );
	hgeFont *fnt = CAST_PTR( kl_check_number( &arg ), hgeFont* );
	if( fnt != 0 )
	{
		ret.dval = fnt->GetColor();
	}
	return ret;
}

extern "C"
{
void kl_hge_open_font( struct klState *kl )
{
	kl_register( kl, khgeFontCreate, "hgeFont_Create" );
	kl_register( kl, khgeFontRelease, "hgeFont_Release" );
	kl_register( kl, khgeFontRender, "hgeFont_Render" );
	kl_register( kl, khgeFontSetColor, "hgeFont_SetColor" );
	kl_register( kl, khgeFontGetColor, "hgeFont_GetColor" );
}

}
