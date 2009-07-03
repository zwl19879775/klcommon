///
/// @file kl_logger.h
/// @author Kevin Lynx
/// @date 4.21.2008
///
#ifndef ___KL_LOGGER_H_
#define ___KL_LOGGER_H_

#include <assert.h>
#include <stdarg.h>
#include <time.h>
#include <fstream>
#include "kl_compiler_cfg.h"

KL_COMMON_NAMESPACE_BEGIN

/// log level
enum log_level
{
	LL_MIN = -1,
	LL_ERROR,
	LL_WARNING,
	LL_INFO,
	LL_DEBUG,
	LL_MAX
};

///
/// default log pre-string, add system time and log level.
///
struct default_log_prestr
{
	/// output pre-string in the buffer
	/// @return the offset of the buf.
	std::size_t operator() ( char *buf, int level )
	{
		char time[9];
		char date[9];
		_strtime( time );
		_strdate( date );

		const char *ll_desc = 0;
		switch( level )
		{
		case LL_ERROR:
			ll_desc = "ERROR";
			break;
		case LL_WARNING:
			ll_desc = "WARNING";
			break;
		case LL_INFO:
			ll_desc = "INFO";
			break;
		case LL_DEBUG:
			ll_desc = "DEBUG";
			break;

		default:
			ll_desc = "UNKNOWN";
		}

		// combine
		sprintf( buf, "%s %s %s : ", date, time, ll_desc );
		return strlen( buf ) ;
	}
};

///
/// 
/// A simple logger class to write log information.
///
/// @param _Output where the log information to put, it must implement 'log( const char*)' function.
/// @param _PreStr used to write the pre-string of the log text like : 4.21.2008 : something.
/// @param string_size used when format string.(static buffer is more fast than dynamic buffer)
template <typename _Output, typename _PreStr = default_log_prestr, std::size_t _string_size = 1024>
class logger
{
public:
	/// output object type
	typedef _Output output_type;
	
	/// pre-string type
	typedef _PreStr prestr_type;

	/// string size used when formatting strings.
	enum
	{
		string_size = _string_size
	};

public:
	/// constructor
	logger() :
	  _output( 0 ), _level( LL_DEBUG )
	{
	}

	/// destructor
	~logger()
	{
	}

	/// set the output manager, you must call this function before you 
	/// log anything.
	void set_output( output_type *ot )
	{
		assert( ot != 0 && "logger::set_output : invalid arguments." );
		_output = ot;
	}

	/// write log text
	void write_only( int level, const char *format, ... )
	{
		assert( _output != 0 && "logger::write_only : You must set up the output manager before you log anything." );
		static char buf[string_size];

		// checck the level
		if( level > _level ) return ;

		// format the string
		va_list list;
		va_start( list, format );
		vsprintf( buf, format, list );
		va_end( list );

		// output the log text
		_output->log( buf );
	}

	/// write log text and append prestring
	void write( int level, const char *format, ... )
	{
		assert( _output != 0 && "logger::write : You must set up the output manager before you log anything." );
		static char buf[string_size];

		// checck the level
		if( level > _level ) return ;

		// append pre-string
		std::size_t pos = _prestr( buf, level );

		// format the string
		va_list list;
		va_start( list, format );
		vsprintf( &buf[pos], format, list );
		va_end( list );

		// output the log text
		_output->log( buf );
	}

	/// set log level
	void set_level( int level )
	{
		assert( level > LL_MIN && level < LL_MAX && "logger::set_level : invalid arguments." );
		_level = level;
	}

	/// get the log level
	int get_level()
	{
		return _level;
	}

private:
	/// output manager to collect log text.
	output_type *_output;
	/// pre-str to append some text before the log text
	prestr_type _prestr;
	/// log level
	int _level;
};

///
/// The file log output manager, write log text to the file.
///
class file_output
{
public:
	/// constructor
	file_output()
	{
	}

	/// this constructor will open the file
	file_output( const std::string filename, std::ios_base::openmode _Mode = std::ios_base::out ) :
		_file_handle( filename.c_str(), _Mode )
	{
	}

	/// destructor
	~file_output()
	{
	}

	/// open the file if it's not open
	bool open( const std::string filename, std::ios_base::openmode _Mode = std::ios_base::out )
	{
		if( _file_handle.is_open() )
		{
			return false;
		}

		_file_handle.open( filename.c_str(), _Mode );
		return _file_handle.is_open();
	}

	/// log
	void log( const char *str )
	{
		assert( _file_handle.is_open() && "file_output::log : you cannot write anything before you open the file!" );
		_file_handle << str ;
		_file_handle.flush();
	}

private:
	/// output file
	std::ofstream _file_handle;
};

KL_COMMON_NAMESPACE_END

#endif // end ___KL_LOGGER_H_