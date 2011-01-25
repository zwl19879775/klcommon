
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "Utils.h"

void Log( const char *fmt, ... )
{
    char buf[1024];
    va_list va;
    va_start( va, fmt );
    vsprintf( buf, fmt, va );
    va_end( va );
    FILE *fp = fopen( "FeiqDll_log.txt", "a+" );
    if( fp == NULL ) return;
    fseek( fp, 0, SEEK_END );
    fwrite( buf, strlen( buf ), 1, fp );
    fclose( fp );
}
