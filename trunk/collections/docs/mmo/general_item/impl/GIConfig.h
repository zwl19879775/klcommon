///
/// @file GIConfig.h
/// @author Kevin Lynx
/// @brief Config GI module.
///
#ifndef ___GI_CONFIG_H_
#define ___GI_CONFIG_H_

#ifndef GI_DEPTYPES
#error "GI dependent config file does NOT exist."
#else
// include depentent file which must defines some types.
#include GI_DEPTYPES
#endif

namespace GI
{
    /// the position an object in a container.
    typedef int PosType;
}

#endif

