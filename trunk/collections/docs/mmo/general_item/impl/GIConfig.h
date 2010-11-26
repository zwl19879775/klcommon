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

#define _USE_KL_SINGLETON
/// Config singleton class getting.
#ifdef _USE_KL_SINGLETON
#include "kl_singleton.h"
#define DEF_SINGLETON(c) : public kl_common::singleton<c>
#define MULTI_DEF_SINGLETON(c) ,public kl_common::singleton<c>
#define SINGLETON( c ) c::getSingleton()
#define SINGLETON_PTR( c ) c::getSingletonPtr()
#endif

#endif

