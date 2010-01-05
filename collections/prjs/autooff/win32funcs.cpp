///
/// 1.5.2010
///
#include <algorithm>
#include "win32funcs.h"
#include <tlhelp32.h>
#include "utils.h"

namespace Win32
{
	size_t GetProcessList( ProcessListType *pl, bool ignore_same )
	{
		HANDLE handle = CreateToolhelp32Snapshot( TH32CS_SNAPPROCESS, 0 );
		if( handle == NULL )
		{
			return 0;
		}
		PROCESSENTRY32 process;
		process.dwSize = sizeof( process );
		Process32First( handle, &process );
		while( Process32Next( handle, &process ) != FALSE )
		{
			bool add = true;
			if( ignore_same && std::find_if( pl->begin(), pl->end(), Utils::StringInSensCmp( process.szExeFile ) )
					!= pl->end() )
			{
				add = false;
			}
			if( add )
			{
				ProcessInfo pi = { process.th32ProcessID, process.szExeFile };
				pl->push_back( pi );	
			}
		}
		CloseHandle( handle );
		return pl->size();
	}

	bool ShutdownSystem( bool safe )
	{
		HANDLE hToken; 
		TOKEN_PRIVILEGES tkp; 

		if( !OpenProcessToken( GetCurrentProcess(), 
					TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken ) ) 
		{
			return false;
		}

		LookupPrivilegeValue( NULL, SE_SHUTDOWN_NAME, &tkp.Privileges[0].Luid ); 

		tkp.PrivilegeCount = 1;  
		tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED; 

		AdjustTokenPrivileges( hToken, FALSE, &tkp, 0, (PTOKEN_PRIVILEGES)NULL, 0 ); 

		if( GetLastError() != ERROR_SUCCESS ) 
		{
			return false;
		}

		UINT nFlags = safe ? EWX_SHUTDOWN : EWX_SHUTDOWN | EWX_FORCE;

		if( !ExitWindowsEx( nFlags, 0 ) )
		{
			return false;
		}

		return true;
	}
}

