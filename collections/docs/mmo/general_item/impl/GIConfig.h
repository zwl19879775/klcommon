///
/// @file GIConfig.h
/// @author Kevin Lynx
/// @brief Config GI module.
///
#ifndef ___GI_CONFIG_H_
#define ___GI_CONFIG_H_

#ifndef GI_DEPTYPES
//#error "GI dependent config file does NOT exist."
#include "GITypesSample.h"
#else
// include depentent file which must defines some types.
#include GI_DEPTYPES
#endif

/// Config singleton class getting.
#define SINGLETON( c ) c::getSingleton()
#define SINGLETON_PTR( c ) c::getSingletonPtr()

/// Config property value (un)serialize no type.
#define VALUE_NO_TYPE

#endif

