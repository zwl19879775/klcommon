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

    bool TerminateProcess( unsigned long process_id )
    {
        HANDLE h = OpenProcess( PROCESS_TERMINATE, FALSE, process_id );
        if( h == NULL )
        {
            return false;
        }
		return ::TerminateProcess( h, 0 ) == TRUE;
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

    void SetAutoRun( const char *key, const char *cmd )
    {
        HKEY regkey;
        const char *auto_key = "SOFTWARE\\Microsoft\\windows\\currentversion\\run";
        RegOpenKey( HKEY_LOCAL_MACHINE,
            auto_key, &regkey );
        RegSetValueEx( regkey, key, 0, REG_SZ, 
            (const BYTE*)cmd, (DWORD) strlen( cmd ) );
        RegCloseKey( regkey );	
    }

    static int SplitFileName( const char *s )
    {
        const char *p = strlen(s) + s - 1;
        while( *p != '\\' && *p != '/' ) --p;
        return p - s;
    }

    size_t GetFileList( const char *_path, std::vector<std::string> &list, bool recur )
    {
        size_t s = list.size();
        char path[MAX_PATH], n[128];
        strcpy( path, _path );
        int pos = SplitFileName( path );
        path[pos] = '\0';
        strcpy( n, &path[pos+1] );
        WIN32_FIND_DATA fdata;
        HANDLE h = FindFirstFile( _path, &fdata );
        if( h == INVALID_HANDLE_VALUE ) return list.size() - s;
        do
        {
            char p[MAX_PATH];
            sprintf( p, "%s\\%s", path, fdata.cFileName );
            list.push_back( p );
            if( recur && 
                fdata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY &&
                strcmp( "..", fdata.cFileName ) &&
                strcmp( ".", fdata.cFileName ) )
            {
                char p[MAX_PATH];
                sprintf( p, "%s\\%s\\%s", path, fdata.cFileName, n );
                GetFileList( p, list, recur );
            }
        } while( FindNextFile( h, &fdata ) );
        FindClose( h );
        return list.size() - s;
    }
}

