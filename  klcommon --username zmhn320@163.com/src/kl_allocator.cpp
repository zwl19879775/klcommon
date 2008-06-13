///
/// @file kl_allocator.cpp
/// @author Kevin Lynx
/// @date 6.12.2008
///
#include "kl_allocator.h"

KL_COMMON_NAMESPACE_BEGIN

static alloc_adapter __alloc_adapter;
static cmMemoryPool __mp;

alloc_adapter::alloc_adapter()
{
	__mp = cmCreate( align, max_bytes );
}

alloc_adapter::~alloc_adapter()
{
	cmRelease( &__mp );
}

void *alloc_adapter::allocate( std::size_t size_bytes )
{
	return size_bytes == 0 ? 0 : cmAllocate( &__mp, (cm_size_t)size_bytes );
}

void alloc_adapter::deallocate( void *p, std::size_t size_bytes )
{
	cmDeallocate( &__mp, p, (cm_size_t)size_bytes );
}

KL_COMMON_NAMESPACE_END
