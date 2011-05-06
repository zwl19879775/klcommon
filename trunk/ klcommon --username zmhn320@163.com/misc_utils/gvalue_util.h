///
/// @file gvalue_util.h
/// @brief Some helper functions based on GValue.
/// @author Kevin Lynx
/// @date 5.6.2011
///
#ifndef ___gvalue_util_h_
#define ___gvalue_util_h_

#include "gvalue.h"
#include <string>
#include <string.h>

typedef long CGUID; // temp code

enum ExtendType {
    EXT_STRING = GValue::RAW,
    EXT_GUID,
};

#define IS_STRING(val) (val.Type() == EXT_STRING)
#define IS_GUID(val) (val.Type() == EXT_GUID)

/// If you pass an unsupported value type, you'll get a compile error.
template <typename Tp>
GValue CreateGValue (Tp t) {
    return GValue (t);
}

/// String support.
inline GValue CreateGValue (const std::string &s) {
    return GValue (s.c_str(), s.size() + 1, EXT_STRING);
}

inline GValue CreateGValue (const char *s) {
    return GValue (s, strlen(s) + 1, EXT_STRING);
}

/// GUID support.
inline GValue CreateGValue (const CGUID &s) {
    return GValue (&s, sizeof(s), EXT_GUID);
}

/// Cast operations.
inline std::string CastString (const GValue &val) {
    if (!IS_STRING(val)) return std::string();
    return std::string ((const char*)val.GetRaw()->p);
}

inline CGUID CastGUID (const GValue &val) {
    if (!IS_GUID(val)) return CGUID();
    return *(CGUID*) val.GetRaw()->p;
}

#endif

