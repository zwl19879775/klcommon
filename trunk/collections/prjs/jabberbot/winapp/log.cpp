/**
  log.cpp
  Kevin Lynx
  6.7.2010
*/
#include "log.h"
#include "kl_logger.h"
#include <windows.h>

static kl_common::logger<kl_common::file_output> _logger;
static kl_common::file_output _output;
static xmpp_log_level_t _loglvl;

const int kllogger_lvl[] = { kl_common::LL_DEBUG, 
kl_common::LL_INFO, kl_common::LL_WARNING, kl_common::LL_ERROR
};

void logger (void * const userdata, const xmpp_log_level_t level,
			 const char * const area, const char * const msg);

xmpp_log_t xmpplogger = { logger, 0 };

static const char *GetLogFileName( const char *logPath )
{
	static char s_file[512];
	SYSTEMTIME time;
	GetLocalTime( &time );
	sprintf( s_file, "%s\\bot_%02d-%02d-%2d-%2d-%2d.log",
			logPath,
			time.wMonth, time.wDay, time.wHour, time.wMinute, time.wSecond );
	return s_file;
}

const char *GetSelfPath()
{
	static char s_path[512];
	::GetModuleFileName( NULL, s_path, sizeof( s_path ) );
	size_t len = ::strlen( s_path );
	for( size_t i = len - 1; i >= 0; -- i )
	{
		if( s_path[i] == '\\' || s_path[i] == '/' )
		{
			s_path[i] = '\0';
			break;
		}
	}
	return s_path;
}

static const char *GetLogPath( const char *logdir )
{
	static char s_path[512];
	sprintf( s_path, "%s\\%s", GetSelfPath(), logdir );
	return s_path;
}

void logger (void * const userdata,
			 const xmpp_log_level_t level,
			 const char * const area,
			 const char * const msg) {
    xmpp_log_level_t filter_level = * (xmpp_log_level_t*)userdata;
    if (level >= filter_level) {
        _logger.write( kllogger_lvl[level], "%s %s\n", area, msg );
    }
}

xmpp_log_t *get_logger() {
    return &xmpplogger;
}

void set_loglvl (int lvl) {
    _loglvl = (xmpp_log_level_t) lvl;
    get_logger()->userdata = (void*) &_loglvl;
}

void open_logfile() {
    const char *logpath = GetLogPath("log");
    ::CreateDirectory(logpath, NULL);
    _output.open(GetLogFileName(logpath));
    _logger.set_output(&_output);
}
