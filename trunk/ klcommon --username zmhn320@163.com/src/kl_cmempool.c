///
/// @file kl_cmempool.h
/// @author Kevin Lynx
/// @date 6.11.2008
///
#include "kl_cmempool.h"
#include <stdlib.h>
#include <string.h>

#define ROUND_UP( bytes, align ) (( (bytes) + (cm_size_t)(align) - 1 ) &~ ((cm_size_t)(align) - 1 ) ) 
#define LIST_INDEX( bytes, align ) (((bytes) + (cm_size_t)(align) - 1 ) / (cm_size_t)(align) - 1 ) 
#define RECORD_CHUNK( chunk, p, node_addr ) add_chunk( chunk, p, node_addr )
#define FREE_CHUNK( chunk ) free_chunk( chunk )

#ifdef __cplusplus
extern "C"
{
#endif

///
/// add a chunk of memory to the linked list.
///
static void add_chunk( cmChunk *chunk, char *p, void *node_addr )
{
	// put the ChunkNode in the node_addr can make things better.:D
	cmChunkNode *node = (cmChunkNode*) node_addr;
	node->_next = chunk->_header ;
	node->_pointer = p;
	chunk->_header = node;
}

///
/// free memory recored by a chunk.
///
static void free_chunk( cmChunk *chunk )
{
	cmChunkNode *node = chunk->_header ;
	cmChunkNode *tmp;
	while( node != 0 )
	{
		tmp = node;
		node = node->_next;
		free( tmp->_pointer );
	}

	chunk->_header = 0;
}

///
/// chunk_alloc, allocate a chunk of memory.
///
/// When alloc failed, return 0.
/// @param mp operated memory pool.
/// @param size number of bytes one object.
/// @param objs number of objects to allocate, it's an input/output parameter.
///
static char *chunk_alloc( cmMemoryPool *mp, cm_size_t size, int *objs )
{
	char *result;
	cmChunk *chunk = &mp->_pool;
	cm_size_t total_bytes = size * (*objs);
	cm_size_t bytes_left = chunk->_end_free - chunk->_start_free ;

	if( bytes_left >= total_bytes )
	{
		// enough memory to allocate total_bytes.
		result = chunk->_start_free ;
		chunk->_start_free += total_bytes;
		return result;
	}
	else if( bytes_left >= size )
	{
		// can allocate more than one object.
		// how many objs it can allocate.
		*objs = (int)( bytes_left / size );
		total_bytes = size * (*objs );
		result = chunk->_start_free ;
		chunk->_start_free += total_bytes;
		return result;
	}
	else
	{
		// god damm it. not enough memory in the pool, increase the pool size. 
		cm_size_t bytes_to_get = 2 * total_bytes + ROUND_UP( ( chunk->_heap_size >> 4 ), mp->_align );
		// Try to make use of the left-over piece.I mean, add the left memory to the memory list.
		if( bytes_left > 0 )
		{
			// in fact, i think here maybe a bug because bytes_left may < sizeof( list_node ), if this node
			// is used by the client, what will happen ? whatever, pay attention to these codes below!
			cmObj **free_list = mp->_list+ LIST_INDEX( bytes_left, mp->_align );
			((cmObj*)chunk->_start_free )->_next = *free_list;
			*free_list = (cmObj*)chunk->_start_free ;
		}

		// and now, i think the memory pool is empty, so malloc again.
		// the extra memory is used to put cmChunkNode.
		chunk->_start_free = (char*)malloc( bytes_to_get + sizeof( cmChunkNode ) );
		if( chunk->_start_free == 0 )
		{
			// malloc failed. that means the os cannot provide our more memory resource, and now,
			// we have to find memory in the memory list.
			cm_size_t i;
			cmObj **free_list;
			cmObj *p;
			for( i = size; i <= mp->_max_bytes; i += mp->_align )
			{
				free_list = mp->_list + LIST_INDEX( i, mp->_align );
				p = *free_list;

				if( p != 0 )
				{
					// ok, we found a memory list node that can server us.
					*free_list = p->_next;
					chunk->_start_free = (char*)p;
					chunk->_end_free = chunk->_start_free + i;
					// and now, the meory pool has resource
					return chunk_alloc( mp, size, objs );
				}
			}

			// god, we did not find memory in the memory list! That means we cannot find 
			// memory either from OS or the memory list.So just return NULL to the client.
			chunk->_end_free = 0;
			chunk->_start_free = 0;
			return 0;
		}
		
		// we got memory from OS
		chunk->_heap_size += bytes_to_get;
		chunk->_end_free = chunk->_start_free + bytes_to_get;
		// record the chunk to free.
		RECORD_CHUNK( chunk, chunk->_start_free, chunk->_end_free );

		return chunk_alloc( mp, size, objs );
	}
}

///
/// refill, returns an object of size n , and optionally adds to size [n] free list.
///
/// @param n object size in bytes.
/// @return If this function fails, return 0.
///
static void *refill( cmMemoryPool *mp, cm_size_t n )
{
	int objs = 20;
	cmObj **free_list;
	cmObj *result;
	cmObj *current_obj;
	cmObj *next_obj;
	cm_size_t i;

	// try to get more memory.
	char *chunk = chunk_alloc( mp, n, &objs );
	if( chunk == 0 )
	{
		// failed.
		return 0;
	}

	// if we only got one object memory, return directly.
	if( objs == 1 )
	{
		return chunk;
	}

	// otherwise, add other memory to size [n] free list.
	free_list = mp->_list + LIST_INDEX( n, mp->_align );
	result = (cmObj*) chunk;
	
	*free_list = next_obj = (cmObj*) ( chunk + n );
	for( i = 1; ; ++ i )
	{
		// build the new memory list.
		current_obj = next_obj;
		// point to the next node.
		next_obj = (cmObj*) ( (char*)next_obj + n );

		if( objs - 1 == i )
		{
			// build finished.
			current_obj->_next = 0;
			break;
		}
		else
		{
			// link them
			current_obj->_next = next_obj;
		}
	}

	return result;
}

cmMemoryPool cmCreate( cm_size_t align, cm_size_t max_bytes )
{
	cmMemoryPool mp;
	cm_size_t i ;

	// adjust align and max_bytes.
	align = ROUND_UP( align, 8 );
	max_bytes = ROUND_UP( max_bytes, 8 );

	// the list properties.
	mp._align = align;
	mp._max_bytes = max_bytes;
	// the pool
	mp._pool._start_free = mp._pool._end_free = 0;
	mp._pool._heap_size = 0;
	mp._pool._header = 0;
	// the list
	mp._list_size = mp._max_bytes / mp._align ;
	mp._list = (cmObj**) malloc( sizeof( cmObj* ) * mp._list_size );
	
	for( i = 0; i < mp._list_size; ++ i )
	{
		mp._list[i] = 0; 
	}

	return mp;
}

void cmRelease( cmMemoryPool *mp )
{
	// i did not find the release behavior in SGI stl.
	// free the chunk memory.
	FREE_CHUNK( &mp->_pool );
	// free the list array.
	free( mp->_list );
	mp->_list = 0;
}

void *cmAllocate( cmMemoryPool *mp, cm_size_t size )
{
	void *ret;

	if( size > mp->_max_bytes )
	{
		// big enough, we use system malloc.
		ret = malloc( size );
	}
	else
	{
		// otherwise, we get the memory from the free list.
		cmObj **free_list = mp->_list + LIST_INDEX( size, mp->_align );
		cmObj *result = *free_list;

		if( result == 0 )
		{
			// no memory node. build new free_list of size [size]
			ret = refill( mp, ROUND_UP( size, mp->_align ) );
		}
		else
		{
			*free_list = result->_next;
			ret = result;
		}
	}

	return ret;
}

void cmDeallocate( cmMemoryPool *mp, void *p, cm_size_t size )
{
	if( size > mp->_max_bytes )
	{
		// we use system free.
		free( p );
	}
	else
	{
		// return the memory to the free list( memory list)
		cmObj **free_list = mp->_list + LIST_INDEX( size, mp->_align );
		cmObj *node = (cmObj*) p;
		node->_next = *free_list;
		*free_list = node;
	}
}

void *cmReallocate( cmMemoryPool *mp, void *p, cm_size_t old_size, cm_size_t new_size )
{
	void *result;
	cm_size_t copy_bytes;

	if( old_size > mp->_max_bytes && new_size > mp->_max_bytes )
	{
		// big enough, we use system realloc
		return realloc( p, new_size );
	}

	if( ROUND_UP( old_size, mp->_align ) == ROUND_UP( new_size, mp->_align ) )
	{
		// in fact, the size is equal.
		return p;
	}

	// get new memory.
	result = cmAllocate( mp, new_size );
	if( result == 0 )
	{
		// failed.
		return result;
	}

	copy_bytes = new_size > old_size ? old_size : new_size;
	// copy the data.
	memcpy( result, p, copy_bytes );
	// deallocate the old memory.
	cmDeallocate( mp, p, old_size );
	return result;
}

#ifdef __cplusplus
}
#endif
