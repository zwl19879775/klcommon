///
/// @file kl_cmempool.h
/// @author Kevin Lynx, i suppose you should check the SGI stl alloc source code because i copyed them.:D
/// @date 6.11.2008
///
#ifndef ___KL_C_MEM_POOL_H_
#define ___KL_C_MEM_POOL_H_

#ifdef __cplusplus
extern "C"
{
#endif

/// cm_size_t
typedef unsigned int cm_size_t;

///
/// memory linked list node.
///
typedef union _cmObj
{
	union _cmObj *_next;
	char _p[1];
}cmObj;

///
/// chunk memory linked list node, used to free chunk memory.
///
typedef struct _cmChunkNode
{
	char *_pointer;
	struct _cmChunkNode *_next;
}cmChunkNode;

///
/// a chunk of memory.
///
typedef struct _cmChunk
{
	char *_start_free;
	char *_end_free;
	/// records how many bytes the memory pool holds.
	cm_size_t _heap_size;
	/// chunk memory list, used to free memory.
	cmChunkNode *_header;
}cmChunk;

///
/// memory pool.
///
typedef struct _cmMP
{
	/// free memory list.
	cmObj **_list;
	cm_size_t _align;
	cm_size_t _max_bytes;
	cm_size_t _list_size;
	/// chunk memory holding the chunk memory.the free list will build in the pool.
	cmChunk _pool;
}cmMemoryPool;

///
/// create a memory pool.This function will create a null free memory list arranged like :
/// align, 2*align,3*align,4*align,...
///
/// @param align it will be adjusted to 8times.
/// @param max_bytes when you allocate > max_bytes memory, this function will use ::malloc to allocate.
///
cmMemoryPool cmCreate( cm_size_t align, cm_size_t max_bytes );

///
/// release a memory pool.
///
/// This function also will free the memory the memory pool holds.
void cmRelease( cmMemoryPool *mp );

///
/// allocate memory from the memory pool.
///
/// @param mp the memory pool which will provide the memory.
/// @param size number of bytes to allocate.
/// @return the memory pointer, null if failed.
///
void *cmAllocate( cmMemoryPool *mp, cm_size_t size );

///
/// deallocate memory, return the memory to the memory pool.
///
/// @param mp the memory pool.
/// @param the memory pointer which will be returned.
/// @param size number of bytes to return.
///
void cmDeallocate( cmMemoryPool *mp, void *p, cm_size_t size );

///
/// reallocate memory from the memory pool.
///
/// @param mp the memory pool which will provide the memory.
///
void *cmReallocate( cmMemoryPool *mp, void *p, cm_size_t old_size, cm_size_t new_size );


#ifdef __cplusplus
}
#endif

#endif // end ___KL_C_MEM_POOL_H_