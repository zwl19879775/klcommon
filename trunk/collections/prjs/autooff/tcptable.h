///
/// Check current tcp connections in system.
/// Kevin Lynx
/// 1.8.2010
/// GetExtendedTcpTable, to query all tcp connections in the system.
///
#ifndef ___WIN32_TCPTABLE_H_
#define ___WIN32_TCPTABLE_H_

#include <windows.h>
#include <vector>

namespace Win32
{
	enum StatusType
	{
		STATUS_UNKNOWN = 0,
		STATUS_CLOSED,
		STATUS_LISTENING,
		STATUS_SYN_SENT,
		STATUS_SYN_RCVD,
		STATUS_ESTABLISHED,
		STATUS_FIN_WAIT1,
		STATUS_FIN_WAIT2,
		STATUS_CLOSE_WAIT,
		STATUS_CLOSING,
		STATUS_LAST_ACK,
		STATUS_TIME_WAIT,
		STATUS_DELETE_TCB,
	};

	typedef enum   
	{ 
		TCP_TABLE_BASIC_LISTENER, 
		TCP_TABLE_BASIC_CONNECTIONS, 
		TCP_TABLE_BASIC_ALL, 
		TCP_TABLE_OWNER_PID_LISTENER, 
		TCP_TABLE_OWNER_PID_CONNECTIONS, 
		TCP_TABLE_OWNER_PID_ALL, 
		TCP_TABLE_OWNER_MODULE_LISTENER, 
		TCP_TABLE_OWNER_MODULE_CONNECTIONS, 
		TCP_TABLE_OWNER_MODULE_ALL 
	} TCP_TABLE_CLASS;

	typedef DWORD (_stdcall *GetExtendedTcpTableFunc)(
			PVOID, PDWORD, BOOL, ULONG, TCP_TABLE_CLASS, ULONG );
	
	struct TcpInfo
	{
		DWORD localAddr;
		DWORD remoteAddr;
		WORD localPort;
		WORD remotePort;
		StatusType status;
		DWORD Pid;
		char PName[64];
	};

	typedef std::vector<TcpInfo> TcpInfoList;

	class TcpTable
	{
	public:
		enum { FAILED = (size_t)(-1) };
	public:
		TcpTable();

		bool Init();

		void Release();

		bool ProcessHasTcp( const char *PName );

		bool ProcessHasTcp( const char *PName, WORD remotePort, WORD localPort = 0 );

		size_t Get( TcpInfoList &tcps );

		size_t Get( const char *PName, TcpInfoList &tcps );
	private:
		GetExtendedTcpTableFunc GetExtendedTcpTablePtr;
		HINSTANCE hLib;
	};

	const char *StatusDesc( StatusType s );

	const char *IpDesc( DWORD ip );

	const char *PortDesc( WORD port );
}

#endif

