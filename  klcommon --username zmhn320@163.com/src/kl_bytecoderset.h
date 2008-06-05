///
/// @file kl_bytecoderset.h
/// @author Kevin Lynx
/// @date 6.5.2008
///
#ifndef __KL_BYTECODERSET_H_
#define __KL_BYTECODERSET_H_

#include "kl_compiler_cfg.h"
#include <vector>
#include "kl_staticarray.h"

KL_COMMON_NAMESPACE_BEGIN

///
/// byte_coder set
///
template <std::size_t size = 1024>
class byte_coder_set
{
public:
	/// byte type
	typedef unsigned char byte;
	/// vector byte coder
	typedef byte_coder<std::vector<byte> > vector_byte_coder;
	/// static array byte coder
	typedef byte_coder<static_array<byte, size> > sa_byte_coder;

private:
	/// you cannot create this object.
	byte_coder_set() ;
};

KL_COMMON_NAMESPACE_END

#endif // end __KL_BYTECODERSET_H_