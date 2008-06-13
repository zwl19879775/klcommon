///
/// @file kl_allocator.h
/// @author Kevin Lynx
/// @date 6.12.2008
///
#ifndef ___KL_ALLOCATOR_H_
#define ___KL_ALLOCATOR_H_

#include "kl_compiler_cfg.h"
#include "kl_cmempool.h"

KL_COMMON_NAMESPACE_BEGIN

///
/// allocator adapter, adapted to the cmAllocate etc.
/// This class must be global object so that the memory pool can be created only one time.
///
/// This class only is used internally by the allocator.
///
class alloc_adapter
{
private:
	enum
	{
		align = 8,
		max_bytes = 128
	};
public:
	/// to create the memory pool automatically.
	alloc_adapter();
	/// release the memory pool.
	~alloc_adapter();
	/// allocate
	static void *allocate( std::size_t size_bytes );
	/// deallocate
	static void deallocate( void *p, std::size_t size_bytes );
};

///
/// This allocator can be used in STL, it's a standard stl allocator. It's underlying allocator
/// behaviors like SGI::allocator. Yes, it uses a linked list to manage the memory.
///
template <typename _Tp>
class allocator
{
public:
	typedef std::size_t size_type;
	typedef ptrdiff_t difference_type;
	typedef _Tp	*pointer;
	typedef const _Tp *const_pointer;
	typedef _Tp	&reference;
	typedef const _Tp &const_reference;
	typedef _Tp value_type;

	/// allows any allocator might allocate memory of another type indirectly.
	template <class U> 
	struct rebind 
	{
		typedef allocator<U> other;
	};

public:
	/// default constructor
	allocator() 
	{
	}

	/// copy constructor, it will be called.
	allocator( const allocator &alloc )
	{
	}

	/// copy constructor from another allocator. it also will be called.
	template <typename _Tp2>
	allocator( const allocator<_Tp2> &alloc ) 
	{
	}

	/// destructor, will free all the memory.
	~allocator()
	{
	}

	/// Returns a pointer to the value x.
	pointer address( reference x ) const 
	{ 
		return &x; 
	}
	
	/// Returns a constant pointer to the constant value x.
	const_pointer address(const_reference x ) const 
	{ 
		return &x; 
	}

	/// Returns the largest value which can be passed to the 'allocate()' function.
	size_type max_size() const  
    { 
		return size_t(-1) / sizeof(value_type); 
	}
	
	/// Returns storage for n elements of the element type being used in the memory model.
	/// if failed, return 0.
	pointer allocate( size_type n, const void *p = 0 )
	{
		return n != 0 ? static_cast<pointer>( alloc_adapter::allocate( sizeof( value_type ) * n )  ) : 0 ;	
	}
	
	/// Deallocates the storage for n elements of the element type being used in the memory model,beginning at location p.
	/// @param n must be the same value as it was while calling allocate(). 
	/// @param p returned by allocate.
	void deallocate( pointer p, size_type n )
	{
		alloc_adapter::deallocate( p, sizeof( value_type ) * n );
	}
	
	///	Initializes the storage of the element, pointed to by p, with the value v.
	void construct( pointer p, const _Tp& val ) 
	{ 
		new(p) _Tp( val ); 
	}
	
	/// Destroys the element, pointed to by p, without deallocating the storage.
	void destroy( pointer p ) 
	{ 
		p->~_Tp(); 
	}
};

/// test for allocator equality (always true)
template <class _T1, class _T2>
inline bool operator== ( const allocator<_T1>&, const allocator<_T2>& ) 
{
	return true;
}

/// test for allocator inequality (always false)
template <class _T1, class _T2>
inline bool operator!= (const allocator<_T1>&, const allocator<_T2>& )
{
	return false;
}

/// generic allocator for type void
template <>
class allocator<void> 
{
public:
	typedef size_t size_type;
	typedef ptrdiff_t difference_type;
	typedef void *pointer;
	typedef const void *const_pointer;
	typedef void value_type;

	template <class _Tp1> 
	struct rebind 
	{
		typedef allocator<_Tp1> other;
	};
};

KL_COMMON_NAMESPACE_END

#endif // end ___KL_ALLOCATOR_H_