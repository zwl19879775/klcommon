///
///
///
#include "tcptable.h"
#include <tlhelp32.h>

#define ANY_SIZE (512)
#include <iphlpapi.h>

namespace Win32
{
	static const char *ProcessPidToName( HANDLE hProcessSnap, DWORD ProcessId )
	{
		static char Name[256];
		PROCESSENTRY32 processEntry;
		strcpy( Name, "??" );
		if( !Process32First( hProcessSnap, &processEntry )) 
		{
			return Name;
		}
		do 
		{
			if( processEntry.th32ProcessID == ProcessId ) 
			{
				strcpy( Name, processEntry.szExeFile );
				return Name;
			}
		} while( Process32Next( hProcessSnap, &processEntry ));
		return Name;
	}

	TcpTable::TcpTable()
	{
		hLib = NULL;
		GetExtendedTcpTablePtr = NULL;
	}	

	bool TcpTable::Init()
	{
		hLib = LoadLibrary( "Iphlpapi.dll" );
		if( hLib == NULL )
		{
			return false;
		}
		GetExtendedTcpTablePtr = (GetExtendedTcpTableFunc)
			GetProcAddress( hLib, "GetExtendedTcpTable" );
		if( GetExtendedTcpTablePtr == NULL )
		{
			return false;
		}
		return true;
	}

	void TcpTable::Release()
	{
		GetExtendedTcpTablePtr = NULL;
		FreeLibrary( hLib );
		hLib = NULL;
	}

	size_t TcpTable::Get( TcpInfoList &tcps )
	{
		MIB_TCPTABLE_OWNER_PID tcpTable;
		DWORD dwSize = sizeof( tcpTable );
		
		DWORD dwRet = GetExtendedTcpTablePtr( &tcpTable, &dwSize, TRUE, AF_INET, 
				TCP_TABLE_OWNER_PID_ALL, 0 );
		if( dwRet != NO_ERROR )
		{
			return FAILED;
		}
		HANDLE hProcessSnap = CreateToolhelp32Snapshot( TH32CS_SNAPPROCESS, 0 );
		for( DWORD i = 0; i < tcpTable.dwNumEntries; ++ i )
		{
			TcpInfo tcp;
			tcp.localAddr = tcpTable.table[i].dwLocalAddr;
			tcp.localPort = (WORD)tcpTable.table[i].dwLocalPort;
			tcp.remoteAddr = tcpTable.table[i].dwRemoteAddr;
			tcp.remotePort = (WORD)tcpTable.table[i].dwRemotePort;
			tcp.status = (StatusType)tcpTable.table[i].dwState;
			tcp.Pid = tcpTable.table[i].dwOwningPid;
			strcpy( tcp.PName, ProcessPidToName( hProcessSnap, tcp.Pid ) );

			tcps.push_back( tcp );
		}
		CloseHandle( hProcessSnap );
		return tcps.size();
	}

	bool TcpTable::ProcessHasTcp( const char *PName )
	{
		TcpInfoList tcps;
		if( Get( tcps ) == FAILED )
		{
			return false;
		}
		for( size_t i = 0; i < tcps.size(); ++ i )
		{
			if( stricmp( PName, tcps[i].PName ) == 0 )
			{
				return true;
			}
		}
		return false;
	}

	static char TcpState[][32] = {
		"???",
		"CLOSED",
		"LISTENING",
		"SYN_SENT",
		"SYN_RCVD",
		"ESTABLISHED",
		"FIN_WAIT1",
		"FIN_WAIT2",
		"CLOSE_WAIT",
		"CLOSING",
		"LAST_ACK",
		"TIME_WAIT",
		"DELETE_TCB"
	};

	const char *StatusDesc( StatusType s )
	{
		return TcpState[s];
	}

	const char *IpDesc( DWORD ip )
	{
		in_addr addr;
		addr.S_un.S_addr = ip;
		return inet_ntoa( addr );
	}

	const char *PortDesc( WORD port )
	{
		static char desc[10];
		sprintf( desc, "%u", htons( port ) );
		return desc;
	}
}

