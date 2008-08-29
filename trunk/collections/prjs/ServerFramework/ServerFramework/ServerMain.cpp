/**
 * 包含程序入口点
 *
 */
#include "App.h"

#ifdef _MY_DEBUG
#include "vld.h" // for memory leak checking.
#endif

int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE hPrecInstance, LPTSTR lpCmdLine, int nCmdShow )
{
	///// debug 
	//EnableDebugNew();
	///// log
	//InitialDebugFileName();

	gApp.init();
	gApp.mainLoop();

	return 0;
}