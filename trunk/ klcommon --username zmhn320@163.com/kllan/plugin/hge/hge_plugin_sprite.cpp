/**
 * @file hge_plugin_sprite.cpp
 * @author Kevin Lynx
 * @brief adapt hgeSprite in kl script
 *
 */
extern "C"
{
#include "kllib.h"
}
#include "hgeSprite.h"

#define DEFAULT_RET( ret ) struct TValue ret = { {0}, TValue::NUMBER,0}
#define CAST_PTR( dval, type ) (type)(long)(dval)

static struct TValue khgeSpriteCreate( ArgType arg )
{
	DEFAULT_RET( ret );
	HTEXTURE tex = (HTEXTURE) kl_check_number( &arg );
	float x = (float) kl_check_number( &arg );
	float y = (float) kl_check_number( &arg );
	float w = (float) kl_check_number( &arg );
	float h = (float) kl_check_number( &arg );

	hgeSprite *sp = new hgeSprite( tex, x, y, w, h );
	ret.dval = (long) sp;
	return ret;
}	

static struct TValue khgeSpriteRelease( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	delete sp;
	return ret;
}

static struct TValue khgeSpriteRender( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	if( sp != 0 )
	{
		float x = (float) kl_check_number( &arg );
		float y = (float) kl_check_number( &arg );
		sp->Render( x, y );
	}
	return ret;
}

static struct TValue khgeSpriteRenderEx( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	if( sp != 0 )
	{
		float x = (float) kl_check_number( &arg );
		float y = (float) kl_check_number( &arg );
		float rot = (float) kl_check_number( &arg );
		float hscale = (float) kl_check_number( &arg );
		float vscale = (float) kl_check_number( &arg );
		sp->RenderEx( x, y, rot, hscale, vscale );
	}
	return ret;
}

static struct TValue khgeSpriteRenderStretch( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	if( sp != 0 )
	{
		float x1 = (float) kl_check_number( &arg );
		float y1 = (float) kl_check_number( &arg );
		float x2 = (float) kl_check_number( &arg );
		float y2 = (float) kl_check_number( &arg );
		sp->RenderStretch( x1, y1, x2, y2 );
	}
	return ret;
}

static struct TValue khgeSpriteSetTexture( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	if( sp != 0 )
	{
		HTEXTURE tex = (HTEXTURE) kl_check_number( &arg );
		sp->SetTexture( tex );
	}
	return ret;
}

static struct TValue khgeSpriteGetTexture( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	if( sp != 0 )
	{
		HTEXTURE tex = sp->GetTexture();
		ret.dval = tex;
	}
	return ret;
}

static struct TValue khgeSpriteSetColor( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	if( sp != 0 )
	{
		unsigned long c = (unsigned long) kl_check_number( &arg );
		int index = (int) kl_check_number( &arg );
		sp->SetColor( c, index );
	}
	return ret;
}

static struct TValue khgeSpriteGetColor( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	if( sp != 0 )
	{
		int index = (int) kl_check_number( &arg );
		ret.dval = sp->GetColor( index );
	}
	return ret;
}

static struct TValue khgeSpriteSetHotSpot( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	if( sp != 0 )
	{
		float x = (float) kl_check_number( &arg );
		float y = (float) kl_check_number( &arg );
		sp->SetHotSpot( x, y );
	}
	return ret;
}

static struct TValue khgeSpriteGetHotSpotX( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	if( sp != 0 )
	{
		float x, y;
		sp->GetHotSpot( &x, &y );
		ret.dval = x;
	}
	return ret;
}

static struct TValue khgeSpriteGetHotSpotY( ArgType arg )
{
	DEFAULT_RET( ret );
	hgeSprite *sp = CAST_PTR( kl_check_number( &arg ), hgeSprite* );
	if( sp != 0 )
	{
		float x, y;
		sp->GetHotSpot( &x, &y );
		ret.dval = y;
	}
	return ret;
}

extern "C"
{

void kl_hge_open_sprite( struct klState *kl )
{
	kl_register( kl, khgeSpriteCreate, "hgeSprite_Create" );
	kl_register( kl, khgeSpriteRelease, "hgeSprite_Release" );
	kl_register( kl, khgeSpriteRender, "hgeSprite_Render" );
	kl_register( kl, khgeSpriteRenderEx, "hgeSprite_RenderEx" );
	kl_register( kl, khgeSpriteRenderStretch, "hgeSprite_RenderStretch" );
	kl_register( kl, khgeSpriteSetTexture, "hgeSprite_SetTexture" );
	kl_register( kl, khgeSpriteGetTexture, "hgeSprite_GetTexture" );
	kl_register( kl, khgeSpriteSetColor, "hgeSprite_SetColor" );
	kl_register( kl, khgeSpriteGetColor, "hgeSprite_GetColor" );
	kl_register( kl, khgeSpriteSetHotSpot, "hgeSprite_SetHotSpot" );
	kl_register( kl, khgeSpriteGetHotSpotX, "hgeSprite_GetHotSpotX" );
	kl_register( kl, khgeSpriteGetHotSpotY, "hgeSprite_GetHotSpotY" );
}

}
