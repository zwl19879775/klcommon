/**
 * @file cmlc_malloc.h
 * @author Kevin Lynx
 * @brief the core of cmlc library, a tiny c memory leak checking library
 * @data 2.23.2008
 */
#ifndef ___CMLC_MALLOC_H_
#define ___CMLC_MALLOC_H_

#include <stddef.h>

#if !defined _MSC_VER
#define __FUNCTION __func__
#endif

#define MAX_FILE_NAME 256
#define MAX_FUNC_NAME 256
#define MAX_RECORD 256

void cmlc_init();

void *cmlc_malloc( size_t size, const char *file, const char *func, size_t lineno );

void cmlc_free( void *address );

#define malloc( size ) cmlc_malloc( size, __FILE__, __FUNCTION__, __LINE__ )
#define free( address ) cmlc_free( address )

#endif 
