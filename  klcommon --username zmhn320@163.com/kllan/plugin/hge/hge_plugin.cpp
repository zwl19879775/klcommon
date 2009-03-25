/**
 * @file hge_plugin.cpp
 * @author Kevin Lynx
 * @brief a kl wrapper for kl script
 *
 */
#include "hge.h"
extern "C"
{
#include "kllib.h"
}

#define DEFAULT_RET( ret ) struct TValue ret = { {0}, TValue::NUMBER,0}
#define CAST_PTR( dval, type ) (type)(long)(dval)

/**
 * be careful this variable, so that this plugin cannot be used in 
 * multi-thread enviroment.
 */
static struct klState *kl;

/**
 * because kl does not support function callback, this function
 * is just an function adapter, adapted to HGE frame function
 */
static bool FrameFunc()
{
	ArgType arg_list;
	kl_new_arg( arg_list );
	struct TValue ret;
	ret = kl_call( kl, "FrameFunc", arg_list );
	kl_free_arg( arg_list );

	return ret.dval != 0;
}

static bool RenderFunc()
{
	ArgType arg_list;
	kl_new_arg( arg_list );
	struct TValue ret;
	ret = kl_call( kl, "RenderFunc", arg_list );
	kl_free_arg( arg_list );

	return ret.dval != 0;
}

static struct TValue khgeCreate( ArgType arg )
{
	DEFAULT_RET( ret );
	kl = CAST_PTR( kl_check_number( &arg ), struct klState* );
	if( kl != 0 )
	{
		HGE *hge = hgeCreate( HGE_VERSION );	
		ret.dval = (long) hge;
	}
	return ret;
}

static struct TValue khgeRelease( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		hge->Release();
	}
	return ret;
}

static struct TValue khgeSysInitiate( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		ret.dval = hge->System_Initiate() ? 1 : 0;
	}
	return ret;
}

static struct TValue khgeSysStart( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		hge->System_SetState( HGE_FRAMEFUNC, FrameFunc );
		hge->System_SetState( HGE_RENDERFUNC, RenderFunc );
		hge->System_Start();
	}
	return ret;
}

static struct TValue khgeSysShutdown( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		hge->System_Shutdown();
	}
	return ret;
}

static struct TValue khgeSysSetState( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	int state_type = (int) kl_check_number( &arg );

	if( state_type >= HGE_WINDOWED && state_type <= HGE_SHOWSPLASH )
	{
		/* bool state */
		hge->System_SetState( (hgeBoolState)state_type, kl_check_number( &arg ) != 0 );	
	}
	else if( state_type >= HGE_SCREENWIDTH && state_type <= HGE_FPS )
	{
		/* int state */
		hge->System_SetState( (hgeIntState)state_type, (int)kl_check_number( &arg ) );
	}
	else if( state_type >= HGE_TITLE && state_type <= HGE_LOGFILE )
	{
		/* string state */
		hge->System_SetState( (hgeStringState)state_type, kl_check_string( &arg ) );	
	}

	return ret;
}

static struct TValue khgeSysLog( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		hge->System_Log( kl_check_string( &arg ) );
	}
	return ret;
}

static struct TValue khgeSysSnapshot( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		hge->System_Snapshot( kl_check_string( &arg ) );
	}
	return ret;
}

static struct TValue khgeInputGetKeyState( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		ret.dval = hge->Input_GetKeyState( (int)kl_check_number( &arg ) ) ? 1 : 0;
	}
	return ret;
}

static struct TValue khgeInputGetMousePosX( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		float x, y;
		hge->Input_GetMousePos( &x, &y );
		ret.dval = x;
	}
	return ret;
}

static struct TValue khgeInputGetMousePosY( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		float x, y;
		hge->Input_GetMousePos( &x, &y );
		ret.dval = y;
	}
	return ret;
}

static struct TValue khgeGfxBeginScene( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		ret.dval = hge->Gfx_BeginScene() ? 1 : 0;
	}
	return ret;
}

static struct TValue khgeGfxEndScene( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		hge->Gfx_EndScene();
	}
	return ret;
}

static struct TValue khgeGfxClear( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		hge->Gfx_Clear( (unsigned long)kl_check_number( &arg ) );
	}
	return ret;
}

static struct TValue khgeTimerGetDelta( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		ret.dval = hge->Timer_GetDelta();
	}
	return ret;
}

static struct TValue khgeTimerGetFPS( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		ret.dval = hge->Timer_GetFPS();
	}
	return ret;
}

static struct TValue khgeTimerGetTime( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		ret.dval = hge->Timer_GetTime();
	}
	return ret;
}

static struct TValue khgeARGB( ArgType arg )
{
	DEFAULT_RET( ret );
	long a = (long) kl_check_number( &arg );
	long r = (long) kl_check_number( &arg );
	long g = (long) kl_check_number( &arg );
	long b = (long) kl_check_number( &arg );

	ret.dval = ARGB( a, r, g, b );
	return ret;
}

static struct TValue khgeTextureLoad( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		HTEXTURE tex = hge->Texture_Load( kl_check_string( &arg ) );
		ret.dval = (double) tex;
	}
	return ret;
}

static struct TValue khgeTextureFree( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		HTEXTURE tex = (HTEXTURE) kl_check_number( &arg );
		hge->Texture_Free( tex );
	}
	return ret;
}

static struct TValue khgeRandomSeed( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		int seed = (int) kl_check_number( &arg );
		hge->Random_Seed( seed );
	}
	return ret;
}

static struct TValue khgeRandomFloat( ArgType arg )
{
	DEFAULT_RET( ret );
	HGE *hge = CAST_PTR( kl_check_number( &arg ), HGE* );
	if( hge != 0 )
	{
		float min = (float) kl_check_number( &arg );
		float max = (float) kl_check_number( &arg );
		ret.dval = hge->Random_Float( min, max );
	}
	return ret;
}

#ifdef __cplusplus
extern "C"{
#endif

extern void kl_hge_open_sprite( struct klState *kl );
extern void kl_hge_open_font( struct klState *kl );

void kllib_open_hge( struct klState *kl )
{
	kl_register( kl, khgeCreate, "hgeCreate" );
	kl_register( kl, khgeRelease, "hgeRelease" );
	kl_register( kl, khgeSysInitiate, "hgeSys_Init" );
	kl_register( kl, khgeSysStart, "hgeSys_Start" );
	kl_register( kl, khgeSysShutdown, "hgeSys_Shutdown" );
	kl_register( kl, khgeSysSetState, "hgeSys_SetState" );
	kl_register( kl, khgeSysLog, "hgeSys_Log" );
	kl_register( kl, khgeSysSnapshot, "hgeSys_Snapshot" );
	kl_register( kl, khgeInputGetKeyState, "hgeInput_GetKeyState" );
	kl_register( kl, khgeInputGetMousePosX, "hgeInput_GetMousePosX" );
	kl_register( kl, khgeInputGetMousePosY, "hgeInput_GetMousePosY" );
	kl_register( kl, khgeARGB, "ARGB" );
	kl_register( kl, khgeGfxBeginScene, "hgeGfx_BeginScene" );
	kl_register( kl, khgeGfxEndScene, "hgeGfx_EndScene" );
	kl_register( kl, khgeGfxClear, "hgeGfx_Clear" );
	kl_register( kl, khgeTextureLoad, "hgeTexture_Load" );
	kl_register( kl, khgeTextureFree, "hgeTexture_Free" );
	kl_register( kl, khgeTimerGetDelta, "hgeTimer_GetDelta" );
	kl_register( kl, khgeTimerGetTime, "hgeTimer_GetTime" );
	kl_register( kl, khgeTimerGetFPS, "hgeTimer_GetFPS" );
	kl_register( kl, khgeRandomSeed, "hgeRandom_Seed" );
	kl_register( kl, khgeRandomFloat, "hgeRandom_Float" );

	/* sprite helper */
	kl_hge_open_sprite( kl );
	/* font helper */
	kl_hge_open_font( kl );
}

/**
 * the entry function for a shared library
 */
void lib_open( struct klState *kl )
{
	kllib_open_hge( kl );
}
#ifdef __cplusplus
}
#endif

