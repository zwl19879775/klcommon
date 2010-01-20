///
///
///
#ifndef ___CRASH_DUMPER_H_
#define ___CRASH_DUMPER_H_

#include <windows.h>

namespace CrashHelper
{
	int CrashFilter( LPEXCEPTION_POINTERS lpEP, DWORD dwExceptionCode, 
		char* dumpfilename  = 0 );
}

#endif
