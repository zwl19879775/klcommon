///
/// @file EntityBase.h
/// @author Kevin Lynx
/// @brief The whole Entity-Component system basic definitions.
///
#ifndef ___ENTITY_BASE_H_
#define ___ENTITY_BASE_H_

#include "gvalue.h"
#include <stdlib.h>
#include <string.h>

#ifndef _USE_LOG4C_LOG
#define ELogDebug(fmt, ...)
#define ELogInfo(fmt, ...)
#define ELogWarn(fmt, ...)
#define ELogError(fmt, ...)
#else
#define EntityModule "Entity-Component"
#define EntityFmt(fmt) EntityModule, __FUNCTION__, __LINE__, fmt, __VA_ARGS__
#define ELogDebug(fmt, ...) Log4c::Debug (EntityFmt (fmt))
#define ELogInfo(fmt, ...) Log4c::Debug (EntityFmt (fmt))
#define ELogWarn(fmt, ...) Log4c::Debug (EntityFmt (fmt))
#define ELogError(fmt, ...) Log4c::Debug (EntityFmt (fmt))
#endif

/// internal use only, used as the map iterator name
#define IT_N __it

#define TRAVERSE_MAP(type, map, exp) \
    for (type::iterator IT_N = map.begin(); IT_N != map.end(); ++IT_N) { \
        exp; \
    }

#define TRAVERSE_CONST_MAP(type, map, exp) \
    for (type::const_iterator IT_N = map.begin(); IT_N != map.end(); ++IT_N) { \
        exp; \
    }

#define I_KEY (IT_N->first)
#define I_VALUE (IT_N->second)

#define TRAVERSE_LIST(type, list, exp) \
    for (type::iterator IT_N = list.begin(); IT_N != list.end(); ++IT_N) { \
        exp; \
    }

#define TRAVERSE_CONST_LIST(type, list, exp) \
    for (type::const_iterator IT_N = list.begin(); IT_N != list.end(); ++IT_N) { \
        exp; \
    }

#define L_VALUE (*IT_N)

inline void *CreateStringArg (const std::string &s)
{
    void *arg = malloc (s.size ()+1);
    memcpy (arg, s.c_str (), s.size ()+1);
    return arg;
}

inline void DestroyStringArg (void *arg)
{
    free (arg);
}

#define StringArg(arg) ((char*) arg)

#define VALUE_NIL(val) (val.Type () == GValue::NIL)
#endif

