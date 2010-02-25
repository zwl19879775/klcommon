///
///
///
#include <windows.h>
#include <tlhelp32.h>
#include <string>
#include <stdlib.h>
#include <vector>
#include <algorithm>
#include "kl_logger.h"

struct ProcessInfo
{
	unsigned long id;
	std::string name;	
};
typedef std::vector<ProcessInfo> ProcessListType;

std::string tolower( const std::string &str )
{
	std::string ret = str;
	for( size_t i = 0; i < ret.size(); ++ i )
	{
		ret[i] = tolower( ret[i] );
	}
	return ret;
}

struct StringInSensCmp
{
	StringInSensCmp( const std::string &l ) : _l( tolower( l ) )
	{
	}

	bool operator() ( const std::string &r )
	{
		//return ::stricmp( _l.c_str(), r.c_str() ) == 0;
		std::string s = tolower( r );
		return s.find( _l ) != std::string::npos;
	}

	template <typename _Tp>
	bool operator() ( const _Tp &r )
	{
		return operator() ( r.name );
	}

	const std::string &_l;
};

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
		if( ignore_same && std::find_if( pl->begin(), pl->end(), StringInSensCmp( process.szExeFile ) )
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

BOOL TerminateProcessByID( unsigned long process_id )
{
	HANDLE h = OpenProcess( PROCESS_TERMINATE, FALSE, process_id );
	if( h == NULL )
	{
		return FALSE;
	}
	return TerminateProcess( h, 0 );
}

#ifdef DUMPPROCESS
void DumpProcess( const ProcessListType &pl )
{
	static int s_index = 1;
	char file[512];
	sprintf( file, "process_list%d.txt", s_index++ );
	FILE *fp = fopen( file, "w" );
	fprintf( fp, "ID  \t\tName\n" );
	for( ProcessListType::const_iterator it = pl.begin();
			it != pl.end(); ++ it )
	{
		fprintf( fp, "%4u\t\t%s\n", 
				it->id, it->name.c_str() );
	}
	fclose( fp );
}
#else
#define DumpProcess( t )
#endif


void OnProcessCheck( const std::vector<std::string> &tl, 	
		kl_common::logger<kl_common::file_output> *logger )
{
	if( tl.size() == 0 )
	{
		return ;
	}
	ProcessListType pl;
	GetProcessList( &pl, false );
	DumpProcess( pl );
	for( std::vector<std::string>::const_iterator it = tl.begin();
			it != tl.end(); ++ it )
	{
		ProcessListType::const_iterator pit = std::find_if( pl.begin(), pl.end(),
				StringInSensCmp( *it ) );
		if( pit != pl.end() )
		{
			logger->write( kl_common::LL_INFO, 
					"Find process [%s], terminate it.\n", (*pit).name.c_str() );
			TerminateProcessByID( pit->id );
		}
	}
}

void OnProcessCheckInvalid( const std::vector<std::string> &validp,
		kl_common::logger<kl_common::file_output> *logger )
{
	if( validp.size() == 0 )
	{
		return ;
	}
	ProcessListType pl;
	GetProcessList( &pl, false );
	for( ProcessListType::iterator it = pl.begin(); it != pl.end(); ++ it )
	{
		std::vector<std::string>::const_iterator tit = std::find_if(
				validp.begin(), validp.end(), StringInSensCmp( it->name ) );
		if( tit == validp.end() )
		{
			logger->write( kl_common::LL_INFO, 
					"Find invalid process [%s], terminate it.\n", it->name.c_str() );
			if( !TerminateProcessByID( it->id ) )
			{
				logger->write( kl_common::LL_ERROR,
						"Terminate process [%s] failed : %d.\n", it->name.c_str(),
						GetLastError() );
			}
		}
	}	
}

extern const char *GetSelfPath();
#define VALIDPROCESS "validprocess.cfg"
void DumpValidProcess()
{
	ProcessListType pl;
	GetProcessList( &pl, true );
	char file[512];
	sprintf( file, "%s\\%s", GetSelfPath(), VALIDPROCESS );
	FILE *fp = fopen( file, "w" );
	if( fp == NULL )
	{
		return ;
	}
	for( ProcessListType::iterator it = pl.begin();
			it != pl.end(); ++ it )
	{
		fprintf( fp, "%s\n", it->name.c_str() );
	}
	fclose( fp );
}

size_t LoadValidProcess( std::vector<std::string> &tl )
{
	char file[512];
	sprintf( file, "%s\\%s", GetSelfPath(), VALIDPROCESS );
	FILE *fp = fopen( file, "r" );
	if( fp == NULL )
	{
		return 0;
	}
	char name[512];
	while( fscanf( fp, "%s", name ) != EOF )
	{
		tl.push_back( name );
	}
	fclose( fp );
	return tl.size();
}

#define INVALIDPROCESS "invalidprocess.cfg"
size_t LoadInvalidProcess( std::vector<std::string> &tl )
{
	char file[512];
	sprintf( file, "%s\\%s", GetSelfPath(), INVALIDPROCESS );
	FILE *fp = fopen( file, "r" );
	if( fp == NULL )
	{
		return 0;
	}
	char name[512];
	while( fscanf( fp, "%s", name ) != EOF )
	{
		tl.push_back( name );
	}
	fclose( fp );
	return tl.size();
}

