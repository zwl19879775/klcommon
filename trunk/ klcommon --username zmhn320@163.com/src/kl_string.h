///
/// @file kl_string.h
/// @author Kevin Lynx
/// @date 3.10.2008
///
#ifndef ___KL_STRING_H_
#define ___KL_STRING_H_

#include "kl_compiler_cfg.h"
#include <string>
#include <sstream>

KL_COMMON_NAMESPACE_BEGIN

/// 
/// convert a type which support operator << sstream to a std::string
///
template <typename _Tp>
std::string to_string( const _Tp &t )
{
	std::stringstream str_stream;
	str_stream << t;
	str_stream.flush();

	return str_stream.str();
}

KL_COMMON_NAMESPACE_END

#endif // end ___KL_STRING_H_