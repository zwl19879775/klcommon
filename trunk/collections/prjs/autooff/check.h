///
///
///
#ifndef ___CHECK_H_
#define ___CHECK_H_

struct CheckConfig
{
	bool checkProcess;
	bool checkNet;
	bool safeShutdown;
};

bool CheckInit();
void CheckRelease();
bool CheckStart( HWND hWnd, const CheckConfig *cfg );
void CheckStop( HWND hWnd );
bool GetInitAddr( WORD *port, char *ip );

#ifdef COMPILE_DIST
#define TARGET_NAME "dnf.exe"
#else
#define TARGET_NAME "calc.exe"
#endif

#endif

